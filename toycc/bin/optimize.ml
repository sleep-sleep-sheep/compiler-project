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
      | e, Literal (IntLit 0) when op = "+" -> e
      | Literal (IntLit 0), e when op = "+" -> e
      | e, Literal (IntLit 0) when op = "-" -> e
      | Literal (IntLit 0), e when op = "-" -> UnOp ("-", e)
      | Literal (IntLit 1), e when op = "*" -> e
      | e, Literal (IntLit 1) when op = "*" -> e
      | _, Literal (IntLit 0) when op = "*" -> Literal (IntLit 0)
      | Literal (IntLit 0), _ when op = "*" -> Literal (IntLit 0)
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
      | UnOp ("!", e'') -> e''
      | UnOp ("-", UnOp ("-", e'')) -> e''
      | _ -> UnOp (op, e')
      end
  | Call (fname, args) ->
      let args' = List.map fold_constants_expr args in
      Call (fname, args')
  | Paren e ->
      let e' = fold_constants_expr e in
      begin match e' with
      | Paren e'' -> e''
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
    let body_reachable, _ = eliminate_dead_stmts true func.body in
    let body_deep_reachable = 
      List.map (fun s -> 
        let s', _ = eliminate_dead_stmt true s in
        Option.value s' ~default:Empty
      ) body_reachable
    in
    let used_vars = List.fold_left collect_vars_stmt VarSet.empty body_deep_reachable in
    let body_unused_removed = List.map (remove_unused_stmt used_vars) body_deep_reachable in
    let body_simplified = List.map simplify_empty_stmt body_unused_removed in
    { func with body = body_simplified }
  ) program


(* 尾递归检测与转换为循环 *)
let is_tail_recursive_call func_name params expr =
  match expr with
  | Return (Some (Call (fname, args))) when fname = func_name ->
      (* 检查参数是否是简单表达式，不包含对自身的递归调用 *)
      let rec is_simple_expr e =
        match e with
        | Literal _ | Var _ -> true
        | BinOp (e1, _, e2) -> is_simple_expr e1 && is_simple_expr e2
        | UnOp (_, e) -> is_simple_expr e
        | Paren e -> is_simple_expr e
        | Call (n, _) -> n <> func_name  (* 不允许嵌套调用自身 *)
      in
      List.for_all is_simple_expr args && 
      List.length args = List.length params
  | _ -> false

let rec find_tail_recursive_return func_name params stmts =
  match stmts with
  | [] -> None
  | stmt :: rest ->
      match stmt with
      | Return e when is_tail_recursive_call func_name params (Return e) ->
          Some (Return e)
      | Block b ->
          begin match find_tail_recursive_return func_name params b with
          | Some r -> Some r
          | None -> find_tail_recursive_return func_name params rest
          end
      | If (_, then_stmt, Some else_stmt) ->
          begin match find_tail_recursive_return func_name params [then_stmt] with
          | Some r1 ->
              begin match find_tail_recursive_return func_name params [else_stmt] with
              | Some r2 -> Some (Block [r1; r2])  (* 两个分支都有尾递归 *)
              | None -> None
              end
          | None -> find_tail_recursive_return func_name params [else_stmt]
          end
      | If (_, then_stmt, None) ->
          find_tail_recursive_return func_name params [then_stmt]
      | While (_, body) ->
          (* 修复错误：将单个stmt包装成列表 [body] *)
          begin match find_tail_recursive_return func_name params [body] with
          | Some _ -> None  (* 循环内的尾递归不算 *)
          | None -> find_tail_recursive_return func_name params rest
          end
      | _ -> find_tail_recursive_return func_name params rest

(* 将尾递归转换为循环 *)
let transform_tail_recursion func =
  let func_name = func.fname in
  let params = func.params in
  
  (* 检查是否存在尾递归返回语句 *)
  match find_tail_recursive_return func_name params func.body with
  | None -> func  (* 不是尾递归，不转换 *)
  | Some tail_call ->
      (* 从尾调用中提取参数 *)
      let args = match tail_call with
        | Return (Some (Call (_, args))) -> args
        | Block [Return (Some (Call (_, args1))); Return (Some (Call (_, args2)))] ->
            if args1 = args2 then args1 else args1  (* 取相同的参数列表 *)
        | _ -> failwith "Invalid tail call structure"
      in
      
      (* 创建参数赋值语句：将新参数值赋给原参数 *)
      let param_assignments = 
        List.map2 (fun param arg ->
          Assign (param.pname, arg)
        ) params args
      in
      
      (* 创建循环条件：原函数的退出条件 *)
      let exit_cond = 
        If (
          BinOp (Literal (IntLit 0), "==", Literal (IntLit 0)),  (* 初始为false *)
          Return None,  (* 实际退出条件在原函数中 *)
          None
        )
      in
      
      (* 构建循环体：原函数体(移除尾递归调用) + 参数重新赋值 + continue *)
      let rec remove_tail_call stmt =
        match stmt with
        | Return e when is_tail_recursive_call func_name params (Return e) ->
            Block (param_assignments @ [Continue])
        | Block stmts ->
            Block (List.map remove_tail_call stmts)
        | If (cond, then_stmt, else_stmt_opt) ->
            If (
              cond,
              remove_tail_call then_stmt,
              Option.map remove_tail_call else_stmt_opt
            )
        | While (cond, body) ->
            While (cond, remove_tail_call body)
        | _ -> stmt
      in
      
      let transformed_body = List.map remove_tail_call func.body in
      
      (* 创建初始参数声明 *)
      let param_decls = 
        List.map (fun param ->
          Decl (param.pname, Var param.pname)  (* 保留原参数声明 *)
        ) params
      in
      
      (* 构建循环结构 *)
      let loop_body = Block (transformed_body @ [exit_cond]) in
      let while_loop = While (Literal (IntLit 1), loop_body) in  (* 始终为真的循环条件 *)
      
      (* 新函数体：参数声明 + 循环 *)
      let new_body = param_decls @ [while_loop] in
      
      { func with body = new_body }

let transform_tail_recursions program =
  List.map transform_tail_recursion program


(* 完整的优化流程 *)
let optimize program =
  program 
  |> fold_constants        (* 强化的常量折叠 *)
  |> eliminate_dead_code   (* 强化的死代码消除 *)
  |> transform_tail_recursions  (* 尾递归转循环 *)
    
