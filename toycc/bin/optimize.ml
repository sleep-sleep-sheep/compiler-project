open Ast

(* 常量值表示 *)
type const_value =
  | IntVal of int
  | Unknown

(* 环境：变量到常量值的映射 *)
type env = (id * const_value) list

(* 查找变量的常量值 *)
let rec lookup env id =
  match env with
  | [] -> Unknown
  | (var, value) :: rest ->
      if var = id then value else lookup rest id

(* 更新环境中的变量值 *)
let update env id value =
  (id, value) :: env

(* 常量传播表达式优化 *)
let rec propagate_constants_expr env expr =
  match expr with
  | Literal (IntLit n) -> (expr, IntVal n)
  | Var id ->
      begin match lookup env id with
      | IntVal n -> (Literal (IntLit n), IntVal n)
      | Unknown -> (Var id, Unknown)
      end
  | BinOp (e1, op, e2) ->
      let e1', v1 = propagate_constants_expr env e1 in
      let e2', v2 = propagate_constants_expr env e2 in
      begin match v1, v2 with
      | IntVal n1, IntVal n2 ->
          (* 可以进行常量折叠 *)
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
          (Literal (IntLit result), IntVal result)
      | _ ->
          (* 不能完全折叠，但可能部分优化 *)
          let opt_e1 = if v1 != Unknown then e1' else e1 in
          let opt_e2 = if v2 != Unknown then e2' else e2 in
          (BinOp (opt_e1, op, opt_e2), Unknown)
      end
  | UnOp (op, e) ->
      let e', v = propagate_constants_expr env e in
      begin match v with
      | IntVal n ->
          let result = match op with
            | "-" -> -n
            | "!" -> if n = 0 then 1 else 0
            | _ -> failwith ("Unsupported unary operator: " ^ op)
          in
          (Literal (IntLit result), IntVal result)
      | Unknown ->
          (UnOp (op, e'), Unknown)
      end
  | Call (fname, args) ->
      let args' = List.map (fun arg -> fst (propagate_constants_expr env arg)) args in
      (Call (fname, args'), Unknown)  (* 函数调用结果视为未知 *)
  | Paren e ->
      let e', v = propagate_constants_expr env e in
      (Paren e', v)

(* 常量传播语句优化 *)
let rec propagate_constants_stmt env stmt =
  match stmt with
  | Block stmts ->
      let env', stmts' = propagate_constants_stmts env stmts in
      (env', Block stmts')
  | Empty -> (env, Empty)
  | ExprStmt expr ->
      let expr', _ = propagate_constants_expr env expr in
      (env, ExprStmt expr')
  | Assign (id, expr) ->
      let expr', value = propagate_constants_expr env expr in
      let new_env = update env id value in
      (new_env, Assign (id, expr'))
  | Decl (id, expr) ->
      let expr', value = propagate_constants_expr env expr in
      let new_env = update env id value in
      (new_env, Decl (id, expr'))
  | If (cond, then_stmt, else_stmt_opt) ->
      let cond', _ = propagate_constants_expr env cond in
      
      (* 对then分支和else分支分别进行常量传播 *)
      let then_env, then_stmt' = propagate_constants_stmt env then_stmt in
      let else_env, else_stmt_opt' = 
        match else_stmt_opt with
        | Some else_stmt ->
            let env', stmt' = propagate_constants_stmt env else_stmt in
            (env', Some stmt')
        | None -> (env, None)
      in
      
      (* 合并两个分支的环境 - 只保留在两个分支中值相同的变量 *)
      let merged_env = 
        List.fold_left (fun acc (id, val1) ->
          match lookup else_env id with
          | IntVal val2 when val1 = IntVal val2 -> (id, val1) :: acc
          | _ -> acc
        ) [] then_env
      in
      
      (merged_env, If (cond', then_stmt', else_stmt_opt'))
  | While (cond, body) ->
      let cond', _ = propagate_constants_expr env cond in
      (* 循环体可能执行多次，保守处理环境 *)
      let _, body' = propagate_constants_stmt env body in
      (env, While (cond', body'))  (* 不更新环境，因为循环可能执行0次或多次 *)
  | Break -> (env, Break)
  | Continue -> (env, Continue)
  | Return expr_opt ->
      let expr_opt' = Option.map (fun e -> fst (propagate_constants_expr env e)) expr_opt in
      (env, Return expr_opt')  (* return后不更新环境 *)

and propagate_constants_stmts env stmts =
  match stmts with
  | [] -> (env, [])
  | stmt :: rest ->
      let env', stmt' = propagate_constants_stmt env stmt in
      let env'', rest' = propagate_constants_stmts env' rest in
      (env'', stmt' :: rest')

(* 常量传播入口 *)
let propagate_constants program =
  List.map (fun func ->
    let _, body' = propagate_constants_stmts [] func.body in
    { func with body = body' }
  ) program

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
        if is_const_true cond then
          let then_res, then_reachable = eliminate_dead_stmt true then_stmt in
          let then_stmt' = Option.value then_res ~default:Empty in
          (Some then_stmt', then_reachable)
        else if is_const_false cond then
          begin match else_stmt_opt with
          | Some else_stmt ->
              let else_res, else_reachable = eliminate_dead_stmt true else_stmt in
              let else_stmt' = Option.value else_res ~default:Empty in
              (Some else_stmt', else_reachable)
          | None -> (Some Empty, true)
          end
        else
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
          (Some Empty, true)
        else
          let body_res, _ = eliminate_dead_stmt true body in
          let body' = Option.value body_res ~default:Empty in
          (Some (While (cond, body')), true)
    | Break -> (Some Break, false)
    | Continue -> (Some Continue, false)
    | Return expr_opt -> (Some (Return expr_opt), false)

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
        Empty
  | Decl (id, expr) ->
      if VarSet.mem id used_vars then
        Decl (id, remove_unused_expr used_vars expr)
      else
        Empty
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
let  find_loop_invariants stmt =
  match stmt with
  | Block stmts ->
      (* 简单实现：假设块中的第一个非声明语句前的都是不变量 *)
      let rec split_invariants stmts =
        match stmts with
        | [] -> ([], [])
        | (Decl _ as d) :: rest ->
            let inv, body = split_invariants rest in
            (d :: inv, body)
        | stmt :: rest -> ([], stmt :: rest)
      in
      let invariants, body = split_invariants stmts in
      (invariants, Block body)
  | _ -> ([], stmt)

let optimize_loop loop_body =
  let invariants, body = find_loop_invariants loop_body in
  (invariants, body)

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
    |> propagate_constants
    |> fold_constants
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
    
