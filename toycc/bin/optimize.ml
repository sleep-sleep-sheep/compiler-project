open Ast

(* 常量折叠优化 - 增强版 *)
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
            | "/" -> if n2 = 0 then 0 else n1 / n2  (* 避免除零错误 *)
            | "%" -> if n2 = 0 then 0 else n1 mod n2
            | "<" -> if n1 < n2 then 1 else 0
            | ">" -> if n1 > n2 then 1 else 0
            | "<=" -> if n1 <= n2 then 1 else 0
            | ">=" -> if n1 >= n2 then 1 else 0
            | "==" -> if n1 = n2 then 1 else 0
            | "!=" -> if n1 != n2 then 1 else 0
            | "&&" -> if n1 != 0 && n2 != 0 then 1 else 0
            | "||" -> if n1 != 0 || n2 != 0 then 1 else 0
            | _ -> 0  (* 不支持的运算符不折叠 *)
          in
          Literal (IntLit result)
      | _ -> BinOp (e1', op, e2')
      end
  | UnOp (op, e) ->
      let e' = fold_constants_expr e in
      begin match e' with
      | Literal (IntLit n) ->
          let result = match op with
            | "-" -> -n
            | "!" -> if n = 0 then 1 else 0
            | "+" -> n
            | _ -> 0
          in
          Literal (IntLit result)
      | _ -> UnOp (op, e')
      end
  | Call (fname, args) ->
      Call (fname, List.map fold_constants_expr args)
  | Paren e ->
      let e' = fold_constants_expr e in
      (match e' with  (* 移除冗余括号 *)
       | Paren e'' -> e''
       | _ -> Paren e')

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


(* 常量传播优化 - 修正版 *)
type var_status =
  | Const of literal
  | Top

module VarEnv = Map.Make(String)
type env = var_status VarEnv.t

let init_env : env = VarEnv.empty

let rec expr_to_const (env : env) expr : var_status option =
  match expr with
  | Literal lit -> Some (Const lit)
  | Var id -> 
      (match VarEnv.find_opt id env with
       | Some (Const lit) -> Some (Const lit)
       | _ -> None)
  | BinOp (e1, op, e2) ->
      (match expr_to_const env e1, expr_to_const env e2 with
       | Some (Const (IntLit n1)), Some (Const (IntLit n2)) ->
           (try 
             let result = match op with
               | "+" -> n1 + n2
               | "-" -> n1 - n2
               | "*" -> n1 * n2
               | "/" -> if n2 = 0 then raise Exit else n1 / n2
               | "%" -> if n2 = 0 then raise Exit else n1 mod n2
               | _ -> raise Exit
             in
             Some (Const (IntLit result))
            with Exit -> None)
       | _ -> None)
  | _ -> None

let rec analyze_stmt (env : env) stmt : env =
  match stmt with
  | Empty | Break | Continue | Return _ -> env
  | ExprStmt _ -> env
  | Assign (id, expr) ->
      (match expr_to_const env expr with
       | Some const_val -> VarEnv.add id const_val env
       | None -> VarEnv.add id Top env)
  | Decl (id, expr) ->
      (match expr_to_const env expr with
       | Some const_val -> 
           if VarEnv.mem id env then env
           else VarEnv.add id const_val env
       | None -> 
           if VarEnv.mem id env then env
           else VarEnv.add id Top env)
  | If (_, then_stmt, else_stmt_opt) ->
      let then_env = analyze_stmt env then_stmt in
      let else_env = 
        match else_stmt_opt with
        | Some else_stmt -> analyze_stmt env else_stmt
        | None -> env
      in
      VarEnv.merge (fun _ v1 v2 ->
        match v1, v2 with
        | Some (Const l1), Some (Const l2) when l1 = l2 -> Some (Const l1)
        | Some (Const _), Some (Const _) -> Some Top
        | Some Top, Some Top -> Some Top
        | Some Top, Some (Const _) | Some (Const _), Some Top -> Some Top
        | Some v, None | None, Some v -> Some v
        | None, None -> None
      ) then_env else_env
  | While (_, body) ->
      let body_env = analyze_stmt env body in
      VarEnv.merge (fun _ env_val body_val ->
        match env_val, body_val with
        | Some (Const l), Some (Const l') when l = l' -> Some (Const l)
        | Some v, _ -> Some v
        | None, _ -> body_val
      ) env body_env
  | Block stmts ->
      List.fold_left analyze_stmt env stmts

let rec replace_vars_in_expr (env : env) expr =
  match expr with
  | Literal _ -> expr
  | Var id ->
      (match VarEnv.find_opt id env with
       | Some (Const lit) -> Literal lit
       | _ -> expr)
  | BinOp (e1, op, e2) ->
      BinOp (replace_vars_in_expr env e1, op, replace_vars_in_expr env e2)
  | UnOp (op, e) ->
      UnOp (op, replace_vars_in_expr env e)
  | Call (fname, args) ->
      Call (fname, List.map (replace_vars_in_expr env) args)
  | Paren e ->
      Paren (replace_vars_in_expr env e)

let rec propagate_in_stmt (env : env) stmt =
  match stmt with
  | Block stmts ->
      let stmts', _ = propagate_in_stmts env stmts in
      Block stmts'
  | Empty -> Empty
  | ExprStmt expr ->
      ExprStmt (replace_vars_in_expr env expr)
  | Assign (id, expr) ->
      Assign (id, replace_vars_in_expr env expr)
  | Decl (id, expr) ->
      Decl (id, replace_vars_in_expr env expr)
  | If (cond, then_stmt, else_stmt_opt) ->
      let cond' = replace_vars_in_expr env cond in
      let then_env = analyze_stmt env then_stmt in
      let then_stmt' = propagate_in_stmt then_env then_stmt in
      let else_stmt_opt' = 
        match else_stmt_opt with
        | Some else_stmt ->
            let else_env = analyze_stmt env else_stmt in
            Some (propagate_in_stmt else_env else_stmt)
        | None -> None
      in
      If (cond', then_stmt', else_stmt_opt')
  | While (cond, body) ->
      let cond' = replace_vars_in_expr env cond in
      let body_env = analyze_stmt env body in
      let merged_env = VarEnv.merge (fun _ e1 e2 ->
        match e1, e2 with Some v, _ -> Some v | None, e -> e) env body_env in
      let body' = propagate_in_stmt merged_env body in
      While (cond', body')
  | Break -> Break
  | Continue -> Continue
  | Return expr_opt ->
      Return (Option.map (replace_vars_in_expr env) expr_opt)

and propagate_in_stmts env stmts =
  List.fold_left (fun (stmts_acc, env_acc) stmt ->
    let stmt' = propagate_in_stmt env_acc stmt in
    let new_env = analyze_stmt env_acc stmt' in
    (stmt' :: stmts_acc, new_env)
  ) ([], env) stmts
  |> fun (stmts', env') -> (List.rev stmts', env')

let propagate_constants program =
  List.map (fun func ->
    let param_env = List.fold_left (fun env param ->
      VarEnv.add param.pname Top env
    ) init_env func.params in
    let body', _ = propagate_in_stmts param_env func.body in
    { func with body = body' }
  ) program


(* 死代码消除 - 增强版 *)
module VarSet = Set.Make(String)

let is_const_true expr =
  match fold_constants_expr expr with
  | Literal (IntLit n) -> n != 0
  | _ -> false

let is_const_false expr =
  match fold_constants_expr expr with
  | Literal (IntLit 0) -> true
  | _ -> false

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
  | Assign (id, expr) -> 
      let vars = collect_vars_expr vars expr in
      VarSet.add id vars
  | Decl (id, expr) -> 
      let vars = collect_vars_expr vars expr in
      VarSet.add id vars
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

let rec remove_unused_stmt used_vars stmt =
  match stmt with
  | Block stmts ->
      Block (List.map (remove_unused_stmt used_vars) stmts)
  | Empty -> Empty
  | ExprStmt expr -> ExprStmt (remove_unused_expr used_vars expr)
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

let eliminate_dead_code program =
  List.map (fun func ->
    let param_vars = List.fold_left (fun vars param ->
      VarSet.add param.pname vars
    ) VarSet.empty func.params in
    
    let body_reachable, _ = eliminate_dead_stmts true func.body in
    let used_vars = List.fold_left collect_vars_stmt param_vars body_reachable in
    let body_unused_removed = List.map (remove_unused_stmt used_vars) body_reachable in
    let body_simplified = List.map simplify_empty_stmt body_unused_removed in
    
    { func with body = body_simplified }
  ) program


(* 语义等价判断函数 - 用于优化终止条件 *)
let rec expr_equal e1 e2 =
  match e1, e2 with
  | Paren e1', Paren e2' -> expr_equal e1' e2'
  | Paren e1', e2' -> expr_equal e1' e2'  (* 忽略括号差异 *)
  | e1', Paren e2' -> expr_equal e1' e2'
  | Literal l1, Literal l2 -> l1 = l2
  | Var id1, Var id2 -> id1 = id2
  | BinOp (a1, op1, b1), BinOp (a2, op2, b2) ->
      op1 = op2 && expr_equal a1 a2 && expr_equal b1 b2
  | UnOp (op1, a1), UnOp (op2, a2) ->
      op1 = op2 && expr_equal a1 a2
  | Call (f1, args1), Call (f2, args2) ->
      f1 = f2 && List.for_all2 expr_equal args1 args2
  | _ -> false

let rec stmt_equal s1 s2 =
  match s1, s2 with
  | Block b1, Block b2 -> List.for_all2 stmt_equal b1 b2
  | Empty, Empty -> true
  | ExprStmt e1, ExprStmt e2 -> expr_equal e1 e2
  | Assign (id1, e1), Assign (id2, e2) -> id1 = id2 && expr_equal e1 e2
  | Decl (id1, e1), Decl (id2, e2) -> id1 = id2 && expr_equal e1 e2
  | If (c1, t1, e1), If (c2, t2, e2) ->
      expr_equal c1 c2 && stmt_equal t1 t2 && 
      Option.equal stmt_equal e1 e2
  | While (c1, b1), While (c2, b2) ->
      expr_equal c1 c2 && stmt_equal b1 b2
  | Break, Break -> true
  | Continue, Continue -> true
  | Return e1, Return e2 -> Option.equal expr_equal e1 e2
  | _ -> false

let func_equal f1 f2 =
  f1.fname = f2.fname &&
  List.for_all2 (fun p1 p2 -> p1.pname = p2.pname) f1.params f2.params &&
  List.for_all2 stmt_equal f1.body f2.body

let program_equal p1 p2 =
  List.for_all2 func_equal p1 p2


(* 优化流程 - 带最大迭代次数限制 *)
let rec optimize_iteration max_iter program =
  if max_iter <= 0 then program  (* 达到最大迭代次数，强制退出 *)
  else
    let program' = 
      program
      |> fold_constants
      |> propagate_constants
      |> eliminate_dead_code
    in
    if program_equal program program' then program'  (* 使用语义等价判断 *)
    else optimize_iteration (max_iter - 1) program'  (* 减少迭代次数 *)

(* 主优化函数 *)
let optimize program =
  let preserve_tail_recursion program =
    List.map (fun func ->
      let is_tail_recursive = ref false in
      let rec check_tail_recursion in_tail stmt =
        match stmt with
        | Return (Some (Call (fname, _))) when fname = func.fname ->
            is_tail_recursive := true
        | Return _ -> ()
        | Block stmts ->
            List.iter (check_tail_recursion true) stmts
        | If (_, then_stmt, Some else_stmt) ->
            check_tail_recursion in_tail then_stmt;
            check_tail_recursion in_tail else_stmt
        | If (_, then_stmt, None) ->
            check_tail_recursion in_tail then_stmt
        | While (_, body) ->
            check_tail_recursion false body
        | ExprStmt _ | Assign _ | Decl _ -> if in_tail then ()
        | Break | Continue -> ()
        | Empty -> ()
      in
      List.iter (check_tail_recursion false) func.body;
      if !is_tail_recursive then func else func
    ) program
  in
  
  program
  |> preserve_tail_recursion
  |> optimize_iteration 3  (* 限制最大迭代次数为5次 *)
    
