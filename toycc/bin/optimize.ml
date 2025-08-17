open Ast

(* 常量折叠优化 *)
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
            | _ -> failwith ("Unsupported operator for constant folding: " ^ op)
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
            | _ -> failwith ("Unsupported operator for constant folding: " ^ op)
          in
          Literal (IntLit result)
      | _ -> UnOp (op, e')
      end
  | Call (fname, args) ->
      let args' = List.map fold_constants_expr args in
      Call (fname, args')
  | Paren e ->
      let e' = fold_constants_expr e in
      Paren e'

(* 常量折叠优化语句 *)
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

(* 常量折叠优化程序 *)
let fold_constants program =
  List.map (fun func ->
    { func with body = List.map fold_constants_stmt func.body }
  ) program


(* 常量传播优化 *)
type var_status =
  | Const of literal  (* 变量持有常量值 *)
  | Top               (* 变量值未知或不可预测 *)

(* 明确定义环境类型为从字符串到var_status的映射 *)
module VarEnv = Map.Make(String)
type env = var_status VarEnv.t

(* 初始化环境为明确的var_status映射类型 *)
let init_env : env = VarEnv.empty

(* 表达式转常量状态 - 明确返回var_status option类型 *)
let expr_to_const (env : env) expr : var_status option =
  match expr with
  | Literal lit -> Some (Const lit)  (* 返回包装在Const中的literal *)
  | Var id -> VarEnv.find_opt id env
  | _ -> None

(* 分析语句并更新环境 - 确保输入输出都是env类型 *)
let rec analyze_stmt (env : env) stmt : env =
  match stmt with
  | Empty | Break | Continue | Return _ -> env
  | ExprStmt _ -> env
  | Assign (id, expr) ->
      begin match expr_to_const env expr with
      | Some const_val -> VarEnv.add id const_val env  (* 添加var_status类型的值 *)
      | None -> VarEnv.add id Top env
      end
  | Decl (id, expr) ->
      begin match expr_to_const env expr with
      | Some const_val -> VarEnv.add id const_val env  (* 添加var_status类型的值 *)
      | None -> VarEnv.add id Top env
      end
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
        | _ -> Some Top
      ) then_env else_env
  | While (_, _) ->
      VarEnv.map (fun _ -> Top) env
  | Block stmts ->
      List.fold_left analyze_stmt env stmts

(* 在表达式中替换变量为常量 *)
let rec replace_vars_in_expr (env : env) expr =
  match expr with
  | Literal _ -> expr
  | Var id ->
      begin match VarEnv.find_opt id env with
      | Some (Const lit) -> Literal lit
      | _ -> expr
      end
  | BinOp (e1, op, e2) ->
      BinOp (replace_vars_in_expr env e1, op, replace_vars_in_expr env e2)
  | UnOp (op, e) ->
      UnOp (op, replace_vars_in_expr env e)
  | Call (fname, args) ->
      Call (fname, List.map (replace_vars_in_expr env) args)
  | Paren e ->
      Paren (replace_vars_in_expr env e)

(* 在语句中应用常量传播 *)
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
      let body' = propagate_in_stmt env body in
      While (cond', body')
  | Break -> Break
  | Continue -> Continue
  | Return expr_opt ->
      Return (Option.map (replace_vars_in_expr env) expr_opt)

(* 在语句列表中应用常量传播 *)
and propagate_in_stmts env stmts =
  List.fold_left (fun (stmts_acc, env_acc) stmt ->
    let stmt' = propagate_in_stmt env_acc stmt in
    let new_env = analyze_stmt env_acc stmt' in
    (stmt' :: stmts_acc, new_env)
  ) ([], env) stmts
  |> fun (stmts', _) -> (List.rev stmts', env)

(* 常量传播优化程序 *)
let propagate_constants program =
  List.map (fun func ->
    let env = init_env in
    let body', _ = propagate_in_stmts env func.body in
    { func with body = body' }
  ) program


(* 死代码消除 *)
module VarSet = Set.Make(String)

(* 判断表达式是否为常量真/假 *)
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

(* 移除未使用的变量 *)
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

(* 死代码消除主函数 *)
let eliminate_dead_code program =
  List.map (fun func ->
    (* 1. 移除不可达语句 *)
    let body_reachable, _ = eliminate_dead_stmts true func.body in
    
    (* 2. 收集使用的变量 *)
    let used_vars = List.fold_left collect_vars_stmt VarSet.empty body_reachable in
    
    (* 3. 移除未使用的变量声明和赋值 *)
    let body_unused_removed = List.map (remove_unused_stmt used_vars) body_reachable in
    
    (* 4. 简化空语句和空块 *)
    let body_simplified = List.map simplify_empty_stmt body_unused_removed in
    
    { func with body = body_simplified }
  ) program

(* 完整的优化流程 *)
let optimize program =
  program
  |> fold_constants
  |> propagate_constants


