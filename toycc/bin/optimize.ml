open Ast

(* 强化版常量折叠优化 *)
let rec fold_constants_expr expr =
  match expr with
  | Literal _ -> expr
  | Var _ -> expr
  | BinOp (e1, op, e2) ->
      let e1' = fold_constants_expr e1 in
      let e2' = fold_constants_expr e2 in
      begin match e1', e2' with
      | Literal (IntLit n1), Literal (IntLit n2) ->
          let result = match op with
            | "+" -> n1 + n2
            | "-" -> n1 - n2
            | "*" -> n1 * n2
            | "/" -> n1 / n2
            | "%" -> n1 mod n2
            | "<" -> if n1 < n2 then 1 else 0
            | ">" -> if n1 > n2 then 1 else 0
            | "<=" -> if n1 <= n2 then 1 else 0
            | ">=" -> if n1 >= n2 then 1 else 0
            | "==" -> if n1 = n2 then 1 else 0
            | "!=" -> if n1 != n2 then 1 else 0
            | _ -> failwith ("Unsupported operator for constant folding: " ^ op)
          in
          Literal (IntLit result)
      | e, Literal (IntLit 0) when op = "+" -> e  (* x + 0 → x *)
      | Literal (IntLit 0), e when op = "+" -> e  (* 0 + x → x *)
      | e, Literal (IntLit 0) when op = "-" -> e  (* x - 0 → x *)
      | Literal (IntLit 0), e when op = "-" -> UnOp ("-", e)  (* 0 - x → -x *)
      | Literal (IntLit 1), e when op = "*" -> e  (* 1 * x → x *)
      | e, Literal (IntLit 1) when op = "*" -> e  (* x * 1 → x *)
      | _, Literal (IntLit 0) when op = "*" -> Literal (IntLit 0)  (* x * 0 → 0 *)
      | Literal (IntLit 0), _ when op = "*" -> Literal (IntLit 0)  (* 0 * x → 0 *)
      | e1, e2 when e1 = e2 && op = "-" -> Literal (IntLit 0)  (* x - x → 0 *)
      | e1, e2 when e1 = e2 && op = "==" -> Literal (IntLit 1)  (* x == x → 1 *)
      | e1, e2 when e1 = e2 && op = "!=" -> Literal (IntLit 0)  (* x != x → 0 *)
      | _ -> BinOp (e1', op, e2')
      end
  | UnOp (op, e) ->
      let e' = fold_constants_expr e in
      begin match e' with
      | Literal (IntLit n) ->
          let result = match op with
            | "-" -> -n
            | "!" -> if n = 0 then 1 else 0
            | _ -> failwith ("Unsupported operator for constant folding: " ^ op)
          in
          Literal (IntLit result)
      | UnOp ("!", e'') -> e''  (* !!x → x *)
      | UnOp ("-", UnOp ("-", e'')) -> e''  (* -(-x) → x *)
      | _ -> UnOp (op, e')
      end
  | Call (fname, args) ->
      let args' = List.map fold_constants_expr args in
      Call (fname, args')
  | Paren e ->
      let e' = fold_constants_expr e in
      begin match e' with
      | Paren e'' -> e''  (* 移除嵌套括号 ((x)) → x *)
      | _ -> Paren e'
      end

let rec fold_constants_stmt stmt =
  match stmt with
  | Block stmts ->
      Block (List.map fold_constants_stmt stmts)
  | Empty -> Empty
  | ExprStmt expr ->
      ExprStmt (fold_constants_expr expr)
  | Assign (id, expr) ->
      Assign (id, fold_constants_expr expr)
  | Decl (id, expr) ->
      Decl (id, fold_constants_expr expr)
  | If (cond, then_stmt, else_stmt_opt) ->
      let cond' = fold_constants_expr cond in
      let then_stmt' = fold_constants_stmt then_stmt in
      let else_stmt_opt' = Option.map fold_constants_stmt else_stmt_opt in
      If (cond', then_stmt', else_stmt_opt')
  | While (cond, body) ->
      let cond' = fold_constants_expr cond in
      let body' = fold_constants_stmt body in
      While (cond', body')
  | Break -> Break
  | Continue -> Continue
  | Return expr_opt ->
      Return (Option.map fold_constants_expr expr_opt)

let fold_constants program =
  List.map (fun func ->
    { func with body = List.map fold_constants_stmt func.body }
  ) program

(* 增强版死代码消除 *)
module VarSet = Set.Make(String)
module VarMap = Map.Make(String)

(* 判断表达式是否为常量真 *)
let is_const_true expr =
  match expr with
  | Literal (IntLit n) -> n != 0
  | _ -> false

(* 判断表达式是否为常量假 *)
let is_const_false expr =
  match expr with
  | Literal (IntLit 0) -> true
  | _ -> false

(* 移除不可达语句 *)
let rec eliminate_dead_stmt reachable stmt =
  if not reachable then (None, false)
  else
    match stmt with
    | Block stmts ->
        let stmts', last_reachable = eliminate_dead_stmts reachable stmts in
        (Some (Block stmts'), last_reachable)
    | Empty -> (Some Empty, true)
    | ExprStmt expr -> (Some (ExprStmt expr), true)
    | Assign (id, expr) -> (Some (Assign (id, expr)), true)
    | Decl (id, expr) -> (Some (Decl (id, expr)), true)
    | If (cond, then_stmt, else_stmt_opt) ->
        if is_const_true cond then
          (* 条件为真，只保留then分支 *)
          let then_res, then_reachable = eliminate_dead_stmt true then_stmt in
          let then_stmt' = Option.value then_res ~default:Empty in
          (Some then_stmt', then_reachable)
        else if is_const_false cond then
          (* 条件为假，只保留else分支 *)
          begin match else_stmt_opt with
          | Some else_stmt ->
              let else_res, else_reachable = eliminate_dead_stmt true else_stmt in
              let else_stmt' = Option.value else_res ~default:Empty in
              (Some else_stmt', else_reachable)
          | None -> (Some Empty, true)
          end
        else
          (* 条件为变量，两个分支都可能执行 *)
          let then_res, then_reachable = eliminate_dead_stmt true then_stmt in
          let else_res, else_reachable = 
            match else_stmt_opt with
            | Some else_stmt -> eliminate_dead_stmt true else_stmt
            | None -> (None, true)
          in
          let then_stmt' = Option.value then_res ~default:Empty in
          let else_stmt_opt' = Option.map (fun _ -> Option.value else_res ~default:Empty) else_stmt_opt in
          let new_reachable = then_reachable || else_reachable in
          (Some (If (cond, then_stmt', else_stmt_opt')), new_reachable)
    | While (cond, body) ->
        if is_const_false cond then
          (* 条件恒为假，整个循环都不会执行 *)
          (Some Empty, true)
        else if is_const_true cond then
          (* 条件恒为真，这是一个无限循环，循环后的代码不可达 *)
          let body_res, _ = eliminate_dead_stmt true body in
          let body' = Option.value body_res ~default:Empty in
          (Some (While (cond, body')), false)
        else
          (* 正常循环 *)
          let body_res, _ = eliminate_dead_stmt true body in
          let body' = Option.value body_res ~default:Empty in
          (Some (While (cond, body')), true)
    | Break -> (Some Break, false)  (* Break后循环内后续代码不可达 *)
    | Continue -> (Some Continue, false)  (* Continue后循环内后续代码不可达 *)
    | Return expr_opt -> (Some (Return expr_opt), false)  (* Return后函数内后续代码不可达 *)

and eliminate_dead_stmts reachable stmts =
  match stmts with
  | [] -> ([], reachable)
  | stmt :: rest ->
      let stmt_res, stmt_reachable = eliminate_dead_stmt reachable stmt in
      let rest_stmts, rest_reachable = eliminate_dead_stmts stmt_reachable rest in
      let stmts' = 
        match stmt_res with
        | Some s -> s :: rest_stmts
        | None -> rest_stmts
      in
      (stmts', rest_reachable)

(* 收集所有被使用的变量 *)
let rec collect_vars_expr vars expr =
  match expr with
  | Literal _ -> vars
  | Var id -> VarSet.add id vars
  | BinOp (e1, _, e2) ->
      let vars = collect_vars_expr vars e1 in
      collect_vars_expr vars e2
  | UnOp (_, e) -> collect_vars_expr vars e
  | Call (_, args) ->
      List.fold_left collect_vars_expr vars args
  | Paren e -> collect_vars_expr vars e

let rec collect_vars_stmt vars stmt =
  match stmt with
  | Block stmts ->
      List.fold_left collect_vars_stmt vars stmts
  | Empty -> vars
  | ExprStmt expr -> collect_vars_expr vars expr
  | Assign (_, expr) -> collect_vars_expr vars expr
  | Decl (_, expr) -> collect_vars_expr vars expr
  | If (cond, then_stmt, else_stmt_opt) ->
      let vars = collect_vars_expr vars cond in
      let vars = collect_vars_stmt vars then_stmt in
      begin match else_stmt_opt with
      | Some else_stmt -> collect_vars_stmt vars else_stmt
      | None -> vars
      end
  | While (cond, body) ->
      let vars = collect_vars_expr vars cond in
      collect_vars_stmt vars body
  | Break | Continue -> vars
  | Return expr_opt ->
      begin match expr_opt with
      | Some expr -> collect_vars_expr vars expr
      | None -> vars
      end

(* 跟踪变量定义和使用的数据流分析 *)
let rec analyze_data_flow_stmt (defs, uses) stmt =
  match stmt with
  | Block stmts ->
      List.fold_left analyze_data_flow_stmt (defs, uses) stmts
  | Empty -> (defs, uses)
  | ExprStmt expr ->
      (defs, collect_vars_expr uses expr)
  | Assign (id, expr) ->
      let new_uses = collect_vars_expr uses expr in
      let new_defs = VarSet.add id defs in
      (new_defs, new_uses)
  | Decl (id, expr) ->
      let new_uses = collect_vars_expr uses expr in
      let new_defs = VarSet.add id defs in
      (new_defs, new_uses)
  | If (cond, then_stmt, else_stmt_opt) ->
      let cond_uses = collect_vars_expr uses cond in
      let then_defs, then_uses = analyze_data_flow_stmt (defs, cond_uses) then_stmt in
      let else_defs, else_uses = 
        match else_stmt_opt with
        | Some else_stmt -> analyze_data_flow_stmt (defs, cond_uses) else_stmt
        | None -> (defs, cond_uses)
      in
      (VarSet.union then_defs else_defs, VarSet.union then_uses else_uses)
  | While (cond, body) ->
      let cond_uses = collect_vars_expr uses cond in
      analyze_data_flow_stmt (defs, cond_uses) body
  | Break | Continue -> (defs, uses)
  | Return expr_opt ->
      let return_uses = 
        match expr_opt with
        | Some expr -> collect_vars_expr uses expr
        | None -> uses
      in
      (defs, return_uses)

(* 移除未使用的变量和表达式 *)
let rec remove_unused_stmt used_vars stmt =
  match stmt with
  | Block stmts ->
      let filtered = List.filter_map (fun s ->
        let s' = remove_unused_stmt used_vars s in
        match s' with
        | Empty -> None
        | _ -> Some s'
      ) stmts in
      if filtered = [] then Empty else Block filtered
  | Empty -> Empty
  | ExprStmt expr -> 
      let expr' = remove_unused_expr used_vars expr in
      ExprStmt expr'
  | Assign (id, expr) ->
      if VarSet.mem id used_vars then
        Assign (id, remove_unused_expr used_vars expr)
      else
        Empty  (* 未使用的变量赋值可以安全移除 *)
  | Decl (id, expr) ->
      if VarSet.mem id used_vars then
        Decl (id, remove_unused_expr used_vars expr)
      else
        Empty  (* 未使用的变量声明可以安全移除 *)
  | If (cond, then_stmt, else_stmt_opt) ->
      let cond' = remove_unused_expr used_vars cond in
      let then_stmt' = remove_unused_stmt used_vars then_stmt in
      let else_stmt_opt' = Option.map (remove_unused_stmt used_vars) else_stmt_opt in
      If (cond', then_stmt', else_stmt_opt')
  | While (cond, body) ->
      let cond' = remove_unused_expr used_vars cond in
      let body' = remove_unused_stmt used_vars body in
      While (cond', body')
  | Break -> Break
  | Continue -> Continue
  | Return expr_opt ->
      Return (Option.map (remove_unused_expr used_vars) expr_opt)

and remove_unused_expr used_vars expr =
  match expr with
  | Literal _ -> expr
  | Var id -> Var id
  | BinOp (e1, op, e2) ->
      BinOp (remove_unused_expr used_vars e1, op, remove_unused_expr used_vars e2)
  | UnOp (op, e) ->
      UnOp (op, remove_unused_expr used_vars e)
  | Call (fname, args) ->
      Call (fname, List.map (remove_unused_expr used_vars) args)
  | Paren e ->
      Paren (remove_unused_expr used_vars e)

(* 简化空语句和空块 *)
let rec simplify_empty_stmt stmt =
  match stmt with
  | Block stmts ->
      let stmts' = List.filter_map (fun s ->
        let s' = simplify_empty_stmt s in
        match s' with
        | Empty -> None
        | _ -> Some s'
      ) stmts in
      if stmts' = [] then Empty else Block stmts'
  | If (cond, then_stmt, else_stmt_opt) ->
      let then_stmt' = simplify_empty_stmt then_stmt in
      let else_stmt_opt' = Option.map simplify_empty_stmt else_stmt_opt in
      (match then_stmt', else_stmt_opt' with
       | Empty, None -> Empty
       | Empty, Some Empty -> Empty
       | _ -> If (cond, then_stmt', else_stmt_opt'))
  | While (cond, body) ->
      let body' = simplify_empty_stmt body in
      While (cond, body')
  | _ -> stmt

(* 循环优化：移除循环中的不变量 *)
let rec find_loop_invariants uses stmt =
  match stmt with
  | Assign (_, expr) ->
      (* 如果表达式中没有使用循环中定义的变量，则是不变量 *)
      let expr_vars = collect_vars_expr VarSet.empty expr in
      if VarSet.disjoint expr_vars uses then (true, stmt) else (false, stmt)
  | Decl (_, expr) ->
      let expr_vars = collect_vars_expr VarSet.empty expr in
      if VarSet.disjoint expr_vars uses then (true, stmt) else (false, stmt)
  | Block stmts ->
      let invs_and_body = List.map (find_loop_invariants uses) stmts in
      let invariants = List.filter fst invs_and_body |> List.map snd in
      let body = List.filter (fun (is_inv, _) -> not is_inv) invs_and_body |> List.map snd in
      (invariants <> [], Block (invariants @ body))
  | _ -> (false, stmt)

let rec get_loop_variables stmt =
  match stmt with
  | Assign (id, _) -> VarSet.singleton id
  | Decl (id, _) -> VarSet.singleton id
  | Block stmts ->
      List.fold_left (fun vars s -> VarSet.union vars (get_loop_variables s)) VarSet.empty stmts
  | If (_, then_stmt, else_stmt_opt) ->
      let then_vars = get_loop_variables then_stmt in
      let else_vars = match else_stmt_opt with
        | Some s -> get_loop_variables s
        | None -> VarSet.empty
      in
      VarSet.union then_vars else_vars
  | _ -> VarSet.empty

let optimize_loop loop_body =
  let loop_vars = get_loop_variables loop_body in
  let rec split_invariants stmts =
    match stmts with
    | [] -> ([], [])
    | stmt :: rest ->
        let is_inv, stmt' = find_loop_invariants loop_vars stmt in
        let inv_rest, body_rest = split_invariants rest in
        if is_inv then (stmt' :: inv_rest, body_rest)
        else (inv_rest, stmt' :: body_rest)
  in
  match loop_body with
  | Block stmts ->
      let invariants, body = split_invariants stmts in
      (invariants, Block body)
  | _ -> ([], loop_body)

let rec apply_loop_optimizations stmt =
  match stmt with
  | While (cond, body) ->
      let invariants, optimized_body = optimize_loop body in
      let optimized_body' = apply_loop_optimizations optimized_body in
      if invariants = [] then
        While (cond, optimized_body')
      else
        Block (invariants @ [While (cond, optimized_body')])
  | Block stmts ->
      Block (List.map apply_loop_optimizations stmts)
  | If (cond, then_stmt, else_stmt_opt) ->
      let then_stmt' = apply_loop_optimizations then_stmt in
      let else_stmt_opt' = Option.map apply_loop_optimizations else_stmt_opt in
      If (cond, then_stmt', else_stmt_opt')
  | _ -> stmt

(* 复制传播：用变量的值替换变量引用 *)
let rec copy_propagate_expr var_map expr =
  match expr with
  | Var id ->
      (match VarMap.find_opt id var_map with
       | Some value -> value
       | None -> expr)
  | BinOp (e1, op, e2) ->
      BinOp (copy_propagate_expr var_map e1, op, copy_propagate_expr var_map e2)
  | UnOp (op, e) ->
      UnOp (op, copy_propagate_expr var_map e)
  | Call (fname, args) ->
      Call (fname, List.map (copy_propagate_expr var_map) args)
  | Paren e ->
      Paren (copy_propagate_expr var_map e)
  | _ -> expr

let rec copy_propagate_stmt var_map stmt =
  match stmt with
  | Block stmts ->
      let rec process_stmts vars stmts =
        match stmts with
        | [] -> ([], vars)
        | s :: rest ->
            let s', vars' = copy_propagate_stmt vars s in
            let rest', vars'' = process_stmts vars' rest in
            (s' :: rest', vars'')
      in
      let stmts', vars' = process_stmts var_map stmts in
      (Block stmts', vars')
  | Assign (id, expr) ->
      let expr' = copy_propagate_expr var_map expr in
      (* 如果赋值的是一个简单变量，可以记录用于后续传播 *)
      let new_var_map = 
        match expr' with
        | Var _ | Literal _ -> VarMap.add id expr' var_map
        | _ -> var_map  (* 复杂表达式不传播 *)
      in
      (Assign (id, expr'), new_var_map)
  | Decl (id, expr) ->
      let expr' = copy_propagate_expr var_map expr in
      let new_var_map = 
        match expr' with
        | Var _ | Literal _ -> VarMap.add id expr' var_map
        | _ -> var_map
      in
      (Decl (id, expr'), new_var_map)
  | If (cond, then_stmt, else_stmt_opt) ->
      let cond' = copy_propagate_expr var_map cond in
      let then_stmt', then_vars = copy_propagate_stmt var_map then_stmt in
      let else_stmt_opt', else_vars = 
        match else_stmt_opt with
        | Some else_stmt -> 
            let s, v = copy_propagate_stmt var_map else_stmt in
            (Some s, v)
        | None -> (None, var_map)
      in
      (* 只保留两个分支都有的映射 *)
      let common_vars = 
        VarMap.fold (fun k v acc ->
          match VarMap.find_opt k else_vars with
          | Some v' when v = v' -> VarMap.add k v acc
          | _ -> acc
        ) then_vars VarMap.empty
      in
      (If (cond', then_stmt', else_stmt_opt'), common_vars)
  | While (cond, body) ->
      let cond' = copy_propagate_expr var_map cond in
      (* 循环可能执行多次，保守处理变量映射 *)
      let body', _ = copy_propagate_stmt var_map body in
      (While (cond', body'), var_map)
  | ExprStmt expr ->
      let expr' = copy_propagate_expr var_map expr in
      (ExprStmt expr', var_map)
  | Return expr_opt ->
      let expr_opt' = Option.map (copy_propagate_expr var_map) expr_opt in
      (Return expr_opt', var_map)
  | Break | Continue | Empty -> (stmt, var_map)

let copy_propagate program =
  List.map (fun func ->
    let body', _ = copy_propagate_stmt VarMap.empty (Block func.body) in
    match body' with
    | Block body -> { func with body }
    | _ -> func
  ) program

(* 增强版死代码消除入口 *)
let eliminate_dead_code program =
  List.map (fun func ->
    (* 1. 第一次消除不可达语句 *)
    let body_reachable, _ = eliminate_dead_stmts true func.body in
    
    (* 2. 第二次深度消除（处理嵌套块） *)
    let body_deep_reachable = 
      List.map (fun s -> 
        let s', _ = eliminate_dead_stmt true s in
        Option.value s' ~default:Empty
      ) body_reachable
    in
    
    (* 3. 收集使用的变量 *)
    let used_vars = List.fold_left collect_vars_stmt VarSet.empty body_deep_reachable in
    
    (* 4. 移除未使用的变量 *)
    let body_unused_removed = List.map (remove_unused_stmt used_vars) body_deep_reachable in
    
    (* 5. 简化空语句 *)
    let body_simplified = List.map simplify_empty_stmt body_unused_removed in
    
    { func with body = body_simplified }
  ) program

(* 迭代优化直到没有变化 *)
let rec optimize_iteratively program =
  let optimized = 
    program
    |> fold_constants
    |> copy_propagate
    |> eliminate_dead_code
    |> (fun p -> List.map (fun f -> { f with body = List.map apply_loop_optimizations f.body }) p)
  in
  if program = optimized then
    program
  else
    optimize_iteratively optimized

(* 完整的优化流程 *)
let optimize program =
  optimize_iteratively program
