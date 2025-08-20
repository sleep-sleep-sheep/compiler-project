open Ast

(* 强化版表达式简化与常量折叠 *)
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
            | _ -> failwith ("Unsupported operator: " ^ op)
          in
          Literal (IntLit result)
      (* 代数简化规则 *)
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
            | _ -> failwith ("Unsupported unary operator: " ^ op)
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

(* 检查表达式是否为常量真/假 *)
let is_const_true expr =
  match expr with
  | Literal (IntLit n) -> n != 0
  | _ -> false

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
        if is_const_true cond then (
          (* 条件为真，只保留then分支 *)
          let then_res, then_reachable = eliminate_dead_stmt true then_stmt in
          let then_stmt' = Option.value then_res ~default:Empty in
          (Some then_stmt', then_reachable)
        )
        else if is_const_false cond then (
          (* 条件为假，只保留else分支 *)
          match else_stmt_opt with
          | Some else_stmt ->
              let else_res, else_reachable = eliminate_dead_stmt true else_stmt in
              let else_stmt' = Option.value else_res ~default:Empty in
              (Some else_stmt', else_reachable)
          | None -> (Some Empty, true)
        )
        else (
          (* 条件未知，都保留但分别处理可达性 *)
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
        )
    | While (cond, body) ->
        if is_const_false cond then (
          (* 条件恒为假，循环永不执行 *)
          (Some Empty, true)
        )
        else if is_const_true cond then (
          (* 条件恒为真，这是一个无限循环 *)
          let body_res, _ = eliminate_dead_stmt true body in
          let body' = Option.value body_res ~default:Empty in
          (Some (While (cond, body')), false)  (* 循环后语句不可达 *)
        )
        else (
          let body_res, _ = eliminate_dead_stmt true body in
          let body' = Option.value body_res ~default:Empty in
          (Some (While (cond, body')), true)
        )
    | Break -> (Some Break, false)  (* Break后语句不可达 *)
    | Continue -> (Some Continue, false)  (* Continue后语句不可达 *)
    | Return expr_opt -> (Some (Return expr_opt), false)  (* Return后语句不可达 *)

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

(* 移除未使用的变量和表达式 *)
let rec remove_unused_stmt used_vars stmt =
  match stmt with
  | Block stmts ->
      Block (List.filter_map (fun s ->
        let s' = remove_unused_stmt used_vars s in
        match s' with
        | Empty -> None
        | _ -> Some s'
      ) stmts)
  | Empty -> Empty
  | ExprStmt expr -> 
      let expr' = remove_unused_expr used_vars expr in
      ExprStmt expr'
  | Assign (id, expr) ->
      if VarSet.mem id used_vars then
        Assign (id, remove_unused_expr used_vars expr)
      else
        Empty  (* 未使用的变量赋值 *)
  | Decl (id, expr) ->
      if VarSet.mem id used_vars then
        Decl (id, remove_unused_expr used_vars expr)
      else
        Empty  (* 未使用的变量声明 *)
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

(* 循环优化 *)
(* 检测循环不变量：在循环内不被修改的变量和表达式 *)
let rec is_modified_in_stmt var stmt =
  match stmt with
  | Block stmts -> List.exists (is_modified_in_stmt var) stmts
  | Assign (id, _) -> id = var
  | Decl (id, _) -> id = var  (* 假设声明时的初始化不算修改 *)
  | If (_, then_stmt, else_stmt_opt) ->
      is_modified_in_stmt var then_stmt ||
      (match else_stmt_opt with Some s -> is_modified_in_stmt var s | None -> false)
  | While (_, body) -> is_modified_in_stmt var body
  | _ -> false  (* 其他语句不修改变量 *)

let is_invariant_expr var_set expr body =
  let rec check expr =
    match expr with
    | Var id -> not (VarSet.mem id var_set || is_modified_in_stmt id body)
    | Literal _ -> true
    | BinOp (e1, _, e2) -> check e1 && check e2
    | UnOp (_, e) -> check e
    | Call _ -> false  (* 函数调用可能有副作用，不视为不变量 *)
    | Paren e -> check e
  in
  check expr

let rec collect_invariants var_set body stmt =
  match stmt with
  | Block stmts ->
      let rec split stmts =
        match stmts with
        | [] -> ([], [])
        | s :: rest ->
            let invs, non_invs = split rest in
            let is_inv = is_invariant_stmt var_set body s in
            if is_inv then (s :: invs, non_invs) else (invs, s :: non_invs)
      in
      let invs, non_invs = split stmts in
      (invs, Block non_invs)
  | _ ->
      if is_invariant_stmt var_set body stmt then ([stmt], Empty)
      else ([], stmt)

and is_invariant_stmt var_set body stmt =
  match stmt with
  | Decl (id, expr) ->
      is_invariant_expr var_set expr body &&
      not (is_modified_in_stmt id body)
  | Assign (id, expr) ->
      is_invariant_expr var_set expr body &&
      not (is_modified_in_stmt id body)
  | ExprStmt expr -> is_invariant_expr var_set expr body
  | _ -> false

(* 提取循环中的变量集合 *)
let rec collect_loop_vars stmt =
  match stmt with
  | Block stmts -> List.fold_left (fun s t -> VarSet.union s (collect_loop_vars t)) VarSet.empty stmts
  | Assign (id, _) -> VarSet.singleton id
  | Decl (id, _) -> VarSet.singleton id
  | If (_, then_stmt, else_stmt_opt) ->
      let then_vars = collect_loop_vars then_stmt in
      let else_vars = match else_stmt_opt with
        | Some s -> collect_loop_vars s
        | None -> VarSet.empty
      in
      VarSet.union then_vars else_vars
  | While (_, body) -> collect_loop_vars body
  | _ -> VarSet.empty

(* 循环不变量外提优化 *)
let optimize_loop loop_body =
  let loop_vars = collect_loop_vars loop_body in
  collect_invariants loop_vars loop_body loop_body

let rec apply_loop_optimizations stmt =
  match stmt with
  | While (cond, body) ->
      let invariants, optimized_body = optimize_loop body in
      let optimized_body' = apply_loop_optimizations optimized_body in
      let cond' = fold_constants_expr cond in
      if invariants = [] then
        While (cond', optimized_body')
      else
        Block (invariants @ [While (cond', optimized_body')])
  | Block stmts ->
      Block (List.map apply_loop_optimizations stmts)
  | If (cond, then_stmt, else_stmt_opt) ->
      let then_stmt' = apply_loop_optimizations then_stmt in
      let else_stmt_opt' = Option.map apply_loop_optimizations else_stmt_opt in
      If (cond, then_stmt', else_stmt_opt')
  | _ -> stmt

(* 死代码消除入口 *)
let eliminate_dead_code program =
  List.map (fun func ->
    (* 1. 消除不可达语句 *)
    let body_reachable, _ = eliminate_dead_stmts true func.body in
    
    (* 2. 深度处理嵌套块中的不可达语句 *)
    let body_deep_reachable = 
      List.map (fun s -> 
        let s', _ = eliminate_dead_stmt true s in
        Option.value s' ~default:Empty
      ) body_reachable
    in
    
    (* 3. 收集使用的变量 *)
    let used_vars = List.fold_left collect_vars_stmt VarSet.empty body_deep_reachable in
    
    (* 4. 移除未使用的变量和表达式 *)
    let body_unused_removed = List.map (remove_unused_stmt used_vars) body_deep_reachable in
    
    (* 5. 简化空语句和空块 *)
    let body_simplified = List.map simplify_empty_stmt body_unused_removed in
    
    { func with body = body_simplified }
  ) program

(* 控制流优化：合并连续的块 *)
let rec merge_blocks stmt =
  match stmt with
  | Block stmts ->
      let merged = List.flatten (List.map (fun s ->
        match merge_blocks s with
        | Block bs -> bs
        | s' -> [s']
      ) stmts) in
      Block merged
  | If (cond, then_stmt, else_stmt_opt) ->
      let then_merged = merge_blocks then_stmt in
      let else_merged = Option.map merge_blocks else_stmt_opt in
      If (cond, then_merged, else_merged)
  | While (cond, body) ->
      While (cond, merge_blocks body)
  | _ -> stmt

(* 迭代优化直到没有变化 *)
let rec optimize_iteratively program =
  let optimized = 
    program
    |> fold_constants
    |> eliminate_dead_code
    |> (fun p -> List.map (fun f -> { f with body = List.map apply_loop_optimizations f.body }) p)
    |> (fun p -> List.map (fun f -> { f with body = List.map merge_blocks f.body }) p)
  in
  if program = optimized then
    program
  else
    optimize_iteratively optimized

(* 完整的优化流程 *)
let optimize program =
  optimize_iteratively program
