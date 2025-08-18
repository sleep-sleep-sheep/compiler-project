open Ast

(* 内部辅助类型：变量取值信息 *)
type value =
  | Unknown                 (* 未知 *)
  | Const of int            (* 常量 *)
  | Alias of id             (* 复制传播：等价于另一个变量 *)
  | NonConst                (* 确定不是常量 *)

module SMap = Map.Make (String)
module SSet = Set.Make (String)

type env = value SMap.t
type cse_tbl = id SMap.t   (* 表达式键 -> 产生它的变量 *)

(* ===================== 工具函数 ===================== *)

let int_of_bool b = if b then 1 else 0
let is_true n = n <> 0
let is_false n = n = 0

let rec expr_has_side_effect = function
  | Call _ -> true
  | UnOp (_, e) -> expr_has_side_effect e
  | BinOp (a, _, b) -> expr_has_side_effect a || expr_has_side_effect b
  | Paren e -> expr_has_side_effect e
  | _ -> false

(* ===================== 环境管理 ===================== *)

let env_find x env = try SMap.find x env with Not_found -> Unknown
let env_set x v env = SMap.add x v env
let env_clear = SMap.empty

(* ===================== 表达式优化 ===================== *)

(* 表达式键，用于 CSE *)
let rec key_of_expr = function
  | Literal (IntLit n) -> Some ("#I" ^ string_of_int n)
  | Var x -> Some ("#V" ^ x)
  | Paren e -> key_of_expr e
  | UnOp (op, e) -> Option.map (fun k -> "U(" ^ op ^ "," ^ k ^ ")") (key_of_expr e)
  | BinOp (a, op, b) ->
      (match key_of_expr a, key_of_expr b with
       | Some ka, Some kb -> Some ("B(" ^ op ^ "," ^ ka ^ "," ^ kb ^ ")")
       | _ -> None)
  | Call _ -> None

(* 常量折叠 *)
let fold_bin op n1 n2 =
  match op with
  | "+" -> Some (n1 + n2)
  | "-" -> Some (n1 - n2)
  | "*" -> Some (n1 * n2)
  | "/" when n2 <> 0 -> Some (n1 / n2)
  | "%" when n2 <> 0 -> Some (n1 mod n2)
  | "==" -> Some (int_of_bool (n1 = n2))
  | "!=" -> Some (int_of_bool (n1 <> n2))
  | "<"  -> Some (int_of_bool (n1 < n2))
  | "<=" -> Some (int_of_bool (n1 <= n2))
  | ">"  -> Some (int_of_bool (n1 > n2))
  | ">=" -> Some (int_of_bool (n1 >= n2))
  | "&&" -> Some (int_of_bool (is_true n1 && is_true n2))
  | "||" -> Some (int_of_bool (is_true n1 || is_true n2))
  | _ -> None

(* 代数化简 *)
let algebraic_simplify a op b =
  match op, a, b with
  | "+", _, Literal (IntLit 0) -> Some a
  | "+", Literal (IntLit 0), _ -> Some b
  | "-", _, Literal (IntLit 0) -> Some a
  | "*", _, Literal (IntLit 1) -> Some a
  | "*", Literal (IntLit 1), _ -> Some b
  | "*", _, Literal (IntLit 0) -> Some (Literal (IntLit 0))
  | "*", Literal (IntLit 0), _ -> Some (Literal (IntLit 0))
  | "/", _, Literal (IntLit 1) -> Some a
  | "&&", _, Literal (IntLit 0) -> Some (Literal (IntLit 0))
  | "&&", Literal (IntLit 0), _ -> Some (Literal (IntLit 0))
  | "||", _, Literal (IntLit n) when is_true n -> Some (Literal (IntLit 1))
  | "||", Literal (IntLit n), _ when is_true n -> Some (Literal (IntLit 1))
  | "==", x, y when x = y -> Some (Literal (IntLit 1))
  | "!=", x, y when x = y -> Some (Literal (IntLit 0))
  | _ -> None

(* 环境代入 *)
let rec substitute env = function
  | Var x ->
      (match env_find x env with
       | Const n -> Literal (IntLit n)
       | Alias y -> Var y
       | _ -> Var x)
  | Paren e -> substitute env e
  | UnOp (op, e) -> UnOp (op, substitute env e)
  | BinOp (a, op, b) -> BinOp (substitute env a, op, substitute env b)
  | Call (f, args) -> Call (f, List.map (substitute env) args)
  | e -> e

(* 表达式简化 *)
let rec simplify_expr env e =
  let e = substitute env e in
  match e with
  | Paren e1 -> simplify_expr env e1
  | UnOp ("-", Literal (IntLit n)) -> Literal (IntLit (-n))
  | UnOp ("!", Literal (IntLit n)) -> Literal (IntLit (if n = 0 then 1 else 0))
  | UnOp (op, e1) -> UnOp (op, simplify_expr env e1)
  | BinOp (a, op, b) ->
      let a' = simplify_expr env a in
      (match op, a' with
       | "||", Literal (IntLit n) when is_true n -> Literal (IntLit 1)
       | "&&", Literal (IntLit 0) -> Literal (IntLit 0)
       | _ ->
           let b' = simplify_expr env b in
           (match a', b' with
            | Literal (IntLit n1), Literal (IntLit n2) ->
                (match fold_bin op n1 n2 with
                 | Some r -> Literal (IntLit r)
                 | None -> BinOp (a', op, b'))
            | _ ->
                (match algebraic_simplify a' op b' with
                 | Some e' -> e'
                 | None -> BinOp (a', op, b'))))
  | Call (f, args) -> Call (f, List.map (simplify_expr env) args)
  | _ -> e

(* ===================== 语句优化 ===================== *)

let rec optimize_stmt env = function
  | Empty -> (env, Empty)

  | ExprStmt e ->
      let e' = simplify_expr env e in
      if expr_has_side_effect e' then (env, ExprStmt e') else (env, Empty)

  | Assign (x, rhs) ->
      let rhs' = simplify_expr env rhs in
      let env' =
        match rhs' with
        | Literal (IntLit n) -> env_set x (Const n) env
        | Var y -> env_set x (Alias y) env
        | _ -> env_set x NonConst env
      in
      (env', Assign (x, rhs'))

  | Decl (x, rhs) ->
      let rhs' = simplify_expr env rhs in
      let env' =
        match rhs' with
        | Literal (IntLit n) -> env_set x (Const n) env
        | Var y -> env_set x (Alias y) env
        | _ -> env_set x NonConst env
      in
      (env', Decl (x, rhs'))

  | If (cond, thn, els) ->
      let cond' = simplify_expr env cond in
      (match cond' with
       | Literal (IntLit n) when is_false n ->
           (match els with
            | Some e -> optimize_stmt env e
            | None -> (env, Empty))
       | Literal (IntLit _) ->
           optimize_stmt env thn
       | _ ->
           let _, thn' = optimize_stmt env thn in
           let els' = Option.map (fun e -> snd (optimize_stmt env e)) els in
           (env, If (cond', thn', els')))

  | While (cond, body) ->
      let cond' = simplify_expr env cond in
      (match cond' with
       | Literal (IntLit 0) -> (env, Empty)
       | _ ->
           let _, body' = optimize_stmt env body in
           (env, While (cond', body')))

  | Block stmts ->
      let _, stmts' =
        List.fold_left
          (fun (env, acc) s ->
             match acc with
             | Return _ :: _ -> (env, acc)  (* return 后的语句全部丢弃 *)
             | _ ->
                 let env', s' = optimize_stmt env s in
                 (env', if s' = Empty then acc else acc @ [s']))
          (env, []) stmts
      in
      (env, Block stmts')

  | Return eo ->
      let eo' = Option.map (simplify_expr env) eo in
      (env, Return eo')

  | Break -> (env, Break)
  | Continue -> (env, Continue)

(* ===================== 函数/程序优化 ===================== *)

let optimize_func f =
  let _, body' = List.fold_left
      (fun (env, acc) s ->
         let env', s' = optimize_stmt env s in
         (env', if s' = Empty then acc else acc @ [s']))
      (env_clear, []) f.body
  in
  { f with body = body' }

let optimize (p : program) : program =
  List.map optimize_func p
