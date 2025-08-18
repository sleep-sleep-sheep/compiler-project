open Ast

(* 优化表达式 *)
let rec optimize_expr e =
  match e with
  | Literal _ -> e
  | Var _ -> e
  | Paren e1 -> optimize_expr e1  (* 去掉无用括号 *)

  | UnOp (op, e1) ->
      let e1' = optimize_expr e1 in
      begin match op, e1' with
      | "-", Literal (IntLit n) -> Literal (IntLit (-n))
      | "!", Literal (IntLit n) -> Literal (IntLit (if n = 0 then 1 else 0))
      | _ -> UnOp (op, e1')
      end

  | BinOp (e1, op, e2) ->
      let e1' = optimize_expr e1 in
      let e2' = optimize_expr e2 in
      begin match e1', e2' with
      | Literal (IntLit n1), Literal (IntLit n2) ->
          (* 常量折叠 *)
          begin match op with
          | "+" -> Literal (IntLit (n1 + n2))
          | "-" -> Literal (IntLit (n1 - n2))
          | "*" -> Literal (IntLit (n1 * n2))
          | "/" when n2 <> 0 -> Literal (IntLit (n1 / n2))
          | "&&" -> Literal (IntLit (if (n1 <> 0) && (n2 <> 0) then 1 else 0))
          | "||" -> Literal (IntLit (if (n1 <> 0) || (n2 <> 0) then 1 else 0))
          | "==" -> Literal (IntLit (if n1 = n2 then 1 else 0))
          | "!=" -> Literal (IntLit (if n1 <> n2 then 1 else 0))
          | "<"  -> Literal (IntLit (if n1 < n2 then 1 else 0))
          | "<=" -> Literal (IntLit (if n1 <= n2 then 1 else 0))
          | ">"  -> Literal (IntLit (if n1 > n2 then 1 else 0))
          | ">=" -> Literal (IntLit (if n1 >= n2 then 1 else 0))
          | _ -> BinOp (e1', op, e2')
          end
      (* 短路优化：true || e => true，false && e => false *)
      | Literal (IntLit n1), _ when op = "||" && n1 <> 0 -> Literal (IntLit 1)
      | Literal (IntLit 0), e when op = "||" -> e
      | Literal (IntLit 0), _ when op = "&&" -> Literal (IntLit 0)
      | Literal (IntLit n1), e when op = "&&" && n1 <> 0 -> e
      | _ -> BinOp (e1', op, e2')
      end

  | Call (fname, args) ->
      Call (fname, List.map optimize_expr args)


(* 优化语句 *)
let rec optimize_stmt s =
  match s with
  | Empty -> Empty
  | ExprStmt e -> ExprStmt (optimize_expr e)
  | Assign (id, e) -> Assign (id, optimize_expr e)
  | Decl (id, e) -> Decl (id, optimize_expr e)

  | Block stmts ->
      let stmts' = List.map optimize_stmt stmts in
      Block (List.filter (fun s -> s <> Empty) stmts')

  | If (cond, then_s, else_s) ->
      let cond' = optimize_expr cond in
      let then_s' = optimize_stmt then_s in
      let else_s' = Option.map optimize_stmt else_s in
      begin match cond' with
      | Literal (IntLit 0) ->
          (* if (false) -> else 分支 *)
          (match else_s' with Some e -> e | None -> Empty)
      | Literal (IntLit _) ->
          (* if (true) -> then 分支 *)
          then_s'
      | _ -> If (cond', then_s', else_s')
      end

  | While (cond, body) ->
      let cond' = optimize_expr cond in
      let body' = optimize_stmt body in
      begin match cond' with
      | Literal (IntLit 0) -> Empty  (* while(false) -> 消除 *)
      | _ -> While (cond', body')
      end

  | Break -> Break
  | Continue -> Continue
  | Return eo ->
      Return (Option.map optimize_expr eo)


(* 优化函数定义 *)
let optimize_func f =
  { f with body = List.map optimize_stmt f.body }

(* 优化整个程序 *)
let optimize (prog : program) : program =
  List.map optimize_func prog
