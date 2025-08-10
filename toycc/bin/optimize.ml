(* 代码优化器 *)

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
      let stmts' = List.map fold_constants_stmt stmts in
      Block stmts'
  | Empty -> Empty
  | ExprStmt expr ->
      let expr' = fold_constants_expr expr in
      ExprStmt expr'
  | Assign (id, expr) ->
      let expr' = fold_constants_expr expr in
      Assign (id, expr')
  | Decl (id, expr) ->
      let expr' = fold_constants_expr expr in
      Decl (id, expr')
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
      let expr_opt' = Option.map fold_constants_expr expr_opt in
      Return expr_opt'

(* 常量折叠优化程序 *)
let fold_constants program =
  List.map (fun func ->
    { func with 
      body = List.map fold_constants_stmt func.body }
  ) program

(* 死代码消除 *)
let eliminate_dead_code program =
  program  