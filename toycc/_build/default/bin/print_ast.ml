(* ast_printer.ml *)
open Ast

(* 辅助函数：生成缩进 *)
let indent_level n = String.make (n * 2) ' '

(* 打印类型 *)
let print_typ typ =
  match typ with
  | Int -> "int"
  | Void -> "void"

(* 打印字面量 *)
let print_literal lit =
  match lit with
  | IntLit n -> string_of_int n

(* 打印表达式，返回字符串表示 *)
let rec expr_to_string level expr =
  let indent = indent_level level in
  match expr with
  | Literal lit ->
      Printf.sprintf "%sLiteral(%s)" indent (print_literal lit)
  | Var id ->
      Printf.sprintf "%sVar(%s)" indent id
  | BinOp (e1, op, e2) ->
      let s = Printf.sprintf "%sBinOp(%s)" indent op in
      let s1 = expr_to_string (level + 1) e1 in
      let s2 = expr_to_string (level + 1) e2 in
      s ^ "\n" ^ s1 ^ "\n" ^ s2
  | UnOp (op, e) ->
      let s = Printf.sprintf "%sUnOp(%s)" indent op in
      let s1 = expr_to_string (level + 1) e in
      s ^ "\n" ^ s1
  | Call (id, args) ->
      let s = Printf.sprintf "%sCall(%s)" indent id in
      let args_str = List.map (expr_to_string (level + 1)) args in
      s ^ "\n" ^ String.concat "\n" args_str
  | Paren e ->
      let s = Printf.sprintf "%sParen" indent in
      let s1 = expr_to_string (level + 1) e in
      s ^ "\n" ^ s1

(* 打印语句，返回字符串表示 *)
let rec stmt_to_string level stmt =
  let indent = indent_level level in
  match stmt with
  | Block stmts ->
      let s = Printf.sprintf "%sBlock" indent in
      let stmts_str = List.map (stmt_to_string (level + 1)) stmts in
      s ^ "\n" ^ String.concat "\n" stmts_str
  | Empty ->
      Printf.sprintf "%sEmpty" indent
  | ExprStmt expr ->
      let s = Printf.sprintf "%sExprStmt" indent in
      let s1 = expr_to_string (level + 1) expr in
      s ^ "\n" ^ s1
  | Assign (id, expr) ->
      let s = Printf.sprintf "%sAssign(%s)" indent id in
      let s1 = expr_to_string (level + 1) expr in
      s ^ "\n" ^ s1
  | Decl (id, expr) ->
      let s = Printf.sprintf "%sDecl(%s)" indent id in
      let s1 = expr_to_string (level + 1) expr in
      s ^ "\n" ^ s1
  | If (cond, then_stmt, else_stmt) ->
      let s = Printf.sprintf "%sIf" indent in
      let cond_str = Printf.sprintf "%sCondition" (indent_level (level + 1)) ^ "\n" ^ expr_to_string (level + 2) cond in
      let then_str = Printf.sprintf "%sThen" (indent_level (level + 1)) ^ "\n" ^ stmt_to_string (level + 2) then_stmt in
      let else_str = match else_stmt with
        | Some s -> Printf.sprintf "%sElse" (indent_level (level + 1)) ^ "\n" ^ stmt_to_string (level + 2) s
        | None -> ""
      in
      s ^ "\n" ^ cond_str ^ "\n" ^ then_str ^ (if else_str = "" then "" else "\n" ^ else_str)
  | While (cond, body) ->
      let s = Printf.sprintf "%sWhile" indent in
      let cond_str = Printf.sprintf "%sCondition" (indent_level (level + 1)) ^ "\n" ^ expr_to_string (level + 2) cond in
      let body_str = Printf.sprintf "%sBody" (indent_level (level + 1)) ^ "\n" ^ stmt_to_string (level + 2) body in
      s ^ "\n" ^ cond_str ^ "\n" ^ body_str
  | Break ->
      Printf.sprintf "%sBreak" indent
  | Continue ->
      Printf.sprintf "%sContinue" indent
  | Return expr_opt ->
      let s = Printf.sprintf "%sReturn" indent in
      match expr_opt with
      | Some expr -> s ^ "\n" ^ expr_to_string (level + 1) expr
      | None -> s

(* 打印参数，返回字符串 *)
let param_to_string param =
  Printf.sprintf "%s %s" (print_typ param.ptype) param.pname

(* 打印函数定义 *)
let func_def_to_string func_def =
  let params_str = 
    func_def.params
    |> List.map param_to_string
    |> String.concat ", "
  in
  let body_str = 
    func_def.body
    |> List.map (stmt_to_string 1)
    |> String.concat "\n"
  in
  Printf.sprintf "Function %s %s(%s) {\n%s\n}"
    (print_typ func_def.ftype)
    func_def.fname
    params_str
    body_str

(* 打印整个程序 *)
let print_program program =
  Printf.printf "===== AST Structure =====\n";
  program
  |> List.map func_def_to_string
  |> String.concat "\n\n"
  |> Printf.printf "%s\n";
  Printf.printf "=========================\n"
