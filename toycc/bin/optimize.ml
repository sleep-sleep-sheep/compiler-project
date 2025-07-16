open Ast

let rec fold_expr = function
  | EBin (op, e1, e2) ->
      let e1' = fold_expr e1 in
      let e2' = fold_expr e2 in
      begin match op, e1', e2' with
      | Add, EInt n1, EInt n2 -> EInt (n1 + n2)
      | Sub, EInt n1, EInt n2 -> EInt (n1 - n2)
      | Mul, EInt n1, EInt n2 -> EInt (n1 * n2)
      | Div, EInt n1, EInt n2 when n2 <> 0 -> EInt (n1 / n2)
      | _ -> EBin (op, e1', e2')
      end
  | EUn (`Neg, EInt n) -> EInt (-n)
  | EUn (`Not, EInt n) -> EInt (if n = 0 then 1 else 0)
  | EUn (op, e) -> EUn (op, fold_expr e)
  | ECall (f, args) -> ECall (f, List.map fold_expr args)
  | e -> e

let rec fold_stmt = function
  | SExpr e -> SExpr (fold_expr e)
  | SDecl (id, e) -> SDecl (id, fold_expr e)
  | SAssign (id, e) -> SAssign (id, fold_expr e)
  | SBlock ss -> SBlock (List.map fold_stmt ss)
  | SIf (cond, th, Some el) -> SIf (fold_expr cond, fold_stmt th, Some (fold_stmt el))
  | SIf (cond, th, None) -> SIf (fold_expr cond, fold_stmt th, None)
  | SWhile (cond, body) -> SWhile (fold_expr cond, fold_stmt body)
  | SReturn (Some e) -> SReturn (Some (fold_expr e))
  | s -> s

let optimize_program prog =
  List.map (fun f -> { f with body = fold_stmt f.body }) prog