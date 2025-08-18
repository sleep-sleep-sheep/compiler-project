(* AST Optimizer *)
open Ast
let rec optimize_expr = function
  (* Constant folding *)
  | BinOp (Literal (IntLit a), op, Literal (IntLit b)) ->
    let result = match op with
      | "+" -> a + b
      | "-" -> a - b
      | "*" -> a * b
      | "/" -> a / b
      | "%" -> a mod b
      | "==" -> if a = b then 1 else 0
      | "!=" -> if a <> b then 1 else 0
      | "<" -> if a < b then 1 else 0
      | "<=" -> if a <= b then 1 else 0
      | ">" -> if a > b then 1 else 0
      | ">=" -> if a >= b then 1 else 0
      | "&&" -> if a <> 0 && b <> 0 then 1 else 0
      | "||" -> if a <> 0 || b <> 0 then 1 else 0
      | _ -> raise (Invalid_argument ("Unknown operator: " ^ op))
    in
    Literal (IntLit result)
  
  (* Boolean short-circuiting *)
  | BinOp (a, "||", b) ->
    let opt_a = optimize_expr a in
    (match opt_a with
     | Literal (IntLit 1) -> Literal (IntLit 1)  (* true || x -> true *)
     | Literal (IntLit 0) -> optimize_expr b     (* false || x -> x *)
     | _ -> BinOp (opt_a, "||", optimize_expr b))
  
  | BinOp (a, "&&", b) ->
    let opt_a = optimize_expr a in
    (match opt_a with
     | Literal (IntLit 0) -> Literal (IntLit 0)  (* false && x -> false *)
     | Literal (IntLit 1) -> optimize_expr b     (* true && x -> x *)
     | _ -> BinOp (opt_a, "&&", optimize_expr b))
  
  (* Identity operations *)
  | BinOp (e, "+", Literal (IntLit 0)) -> optimize_expr e
  | BinOp (Literal (IntLit 0), "+", e) -> optimize_expr e
  | BinOp (e, "*", Literal (IntLit 1)) -> optimize_expr e
  | BinOp (Literal (IntLit 1), "*", e) -> optimize_expr e
  | BinOp (e, "-", Literal (IntLit 0)) -> optimize_expr e
  | BinOp (e, "/", Literal (IntLit 1)) -> optimize_expr e
  
  (* Zero operations *)
  | BinOp (_, "*", Literal (IntLit 0)) -> Literal (IntLit 0)
  | BinOp (Literal (IntLit 0), "*", _) -> Literal (IntLit 0)
  
  (* Unary operations *)
  | UnOp ("-", Literal (IntLit n)) -> Literal (IntLit (-n))
  | UnOp ("!", Literal (IntLit 0)) -> Literal (IntLit 1)
  | UnOp ("!", Literal (IntLit _)) -> Literal (IntLit 0)
  
  (* Nested optimizations *)
  | BinOp (a, op, b) -> BinOp (optimize_expr a, op, optimize_expr b)
  | UnOp (op, e) -> UnOp (op, optimize_expr e)
  | Call (id, args) -> Call (id, List.map optimize_expr args)
  | Paren e -> Paren (optimize_expr e)
  
  (* Base cases *)
  | Literal _ as l -> l
  | Var _ as v -> v

let rec optimize_stmt = function
  (* Empty statement elimination *)
  | Block stmts ->
    let optimized = List.map optimize_stmt stmts |> List.filter (function Empty -> false | _ -> true) in
    if optimized = [] then Empty else Block optimized
  
  (* If statement optimizations *)
  | If (cond, then_stmt, else_stmt) ->
    let opt_cond = optimize_expr cond in
    (match opt_cond with
     | Literal (IntLit 0) -> 
       (match else_stmt with
        | None -> Empty
        | Some s -> optimize_stmt s)
     | Literal (IntLit _) -> optimize_stmt then_stmt
     | _ ->
       let opt_then = optimize_stmt then_stmt in
       let opt_else = match else_stmt with
         | None -> None
         | Some s -> Some (optimize_stmt s) in
       (match opt_then, opt_else with
        | Empty, None -> ExprStmt opt_cond (* if condition with no side effects *)
        | Empty, Some Empty -> ExprStmt opt_cond
        | _ -> If (opt_cond, opt_then, opt_else)))
  
  (* While loop optimizations *)
  | While (cond, body) ->
    let opt_cond = optimize_expr cond in
    let opt_body = optimize_stmt body in
    (match opt_cond with
     | Literal (IntLit 0) -> Empty (* while (false) -> nothing *)
     | _ ->
       (match opt_body with
        | Empty -> ExprStmt opt_cond (* while condition with no body *)
        | _ -> While (opt_cond, opt_body)))
  
  (* Expression statement optimizations *)
  | ExprStmt expr -> 
    let opt_expr = optimize_expr expr in
    (match opt_expr with
     | Literal _ -> Empty (* Discard literals with no side effects *)
     | _ -> ExprStmt opt_expr)
  
  (* Other statements *)
  | Assign (id, expr) -> Assign (id, optimize_expr expr)
  | Decl (id, expr) -> Decl (id, optimize_expr expr)
  | Return expr_opt -> 
    (match expr_opt with
     | None -> Return None
     | Some expr -> Return (Some (optimize_expr expr)))
  
  (* Base cases *)
  | Empty -> Empty
  | Break -> Break
  | Continue -> Continue

let optimize_param param = { param with pname = param.pname } (* No optimization needed *)

let optimize_func_def func = {
  func with
  params = List.map optimize_param func.params;
  body = List.map optimize_stmt func.body |> List.filter (function Empty -> false | _ -> true);
}

let optimize (program: program) : program =
  List.map optimize_func_def program
