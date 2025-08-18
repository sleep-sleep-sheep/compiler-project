open Ast

(* ========= 内部数据结构 ========= *)

type value =
  | Unknown
  | Const of int
  | Alias of id
  | NonConst

module SMap = Map.Make (String)

type env = value SMap.t
type cse_tbl = id SMap.t   (* 纯表达式键 -> 产生它的变量 *)

type fstate = {
  env : env;
  cse : cse_tbl;
}

let empty_state = { env = SMap.empty; cse = SMap.empty }

(* ========= 小工具 ========= *)

let int_of_bool b = if b then 1 else 0
let is_true n = n <> 0
let is_false n = n = 0

let env_find x env = try SMap.find x env with Not_found -> Unknown
let env_set x v env = SMap.add x v env

(* 防止别名环：有限步解析 + visited *)
let resolve_alias ?(limit=2) (env:env) (x:id) : value =
  let rec go seen k v =
    if k = 0 then v
    else match v with
    | Alias y ->
        if SMap.mem y seen then Unknown
        else go (SMap.add y Unknown seen) (k-1) (env_find y env)
    | _ -> v
  in
  go (SMap.singleton x Unknown) limit (env_find x env)

let rec expr_pure = function
  | Literal _ | Var _ -> true
  | Paren e -> expr_pure e
  | UnOp (_, e) -> expr_pure e
  | BinOp (a,_,b) -> expr_pure a && expr_pure b
  | Call _ -> false

let rec expr_has_side_effect = function
  | Call _ -> true
  | UnOp (_, e) -> expr_has_side_effect e
  | BinOp (a,_,b) -> expr_has_side_effect a || expr_has_side_effect b
  | Paren e -> expr_has_side_effect e
  | _ -> false

(* ========= 键生成（CSE） ========= *)

let rec key_of_expr = function
  | Literal (IntLit n) -> Some ("#I" ^ string_of_int n)
  | Var x -> Some ("#V" ^ x)
  | Paren e -> key_of_expr e
  | UnOp (op,e) ->
      Option.map (fun k -> "U(" ^ op ^ "," ^ k ^ ")") (key_of_expr e)
  | BinOp (a,op,b) ->
      begin match key_of_expr a, key_of_expr b with
      | Some ka, Some kb -> Some ("B(" ^ op ^ "," ^ ka ^ "," ^ kb ^ ")")
      | _ -> None
      end
  | Call _ -> None

(* ========= 折叠/代数化简 ========= *)

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

(* ========= 表达式简化（按需代入，单遍） ========= *)

let rec simplify_expr (st:fstate) (e:expr) : expr =
  match e with
  | Literal _ -> e
  | Var x ->
      begin match resolve_alias st.env x with
      | Const n -> Literal (IntLit n)
      | Alias y -> if y = x then Var x else Var y
      | _ -> Var x
      end
  | Paren e1 -> simplify_expr st e1
  | UnOp ("-", Literal (IntLit n)) -> Literal (IntLit (-n))
  | UnOp ("!", Literal (IntLit n)) -> Literal (IntLit (if n=0 then 1 else 0))
  | UnOp (op, e1) -> UnOp (op, simplify_expr st e1)
  | Call (f, args) ->
      Call (f, List.map (simplify_expr st) args)
  | BinOp (a, op, b) ->
      let a' = simplify_expr st a in
      (* 短路先看左 *)
      begin match op, a' with
      | "||", Literal (IntLit n) when is_true n -> Literal (IntLit 1)
      | "&&", Literal (IntLit 0) -> Literal (IntLit 0)
      | _ ->
          let b' = simplify_expr st b in
          match a', b' with
          | Literal (IntLit n1), Literal (IntLit n2) ->
              (match fold_bin op n1 n2 with
               | Some r -> Literal (IntLit r)
               | None -> BinOp (a', op, b'))
          | _ ->
              (match algebraic_simplify a' op b' with
               | Some e' -> e'
               | None -> BinOp (a', op, b'))
      end

(* ========= CSE/传播：对赋值右侧进行一次学习 ========= *)

let kill_on_call (st:fstate) : fstate =
  (* 保守处理：清空 CSE；别名信息降级为 NonConst，避免错误传播 *)
  let env' =
    SMap.map (function Const n -> Const n | _ -> NonConst) st.env
  in
  { env = env'; cse = SMap.empty }

let learn_assignment (st:fstate) (x:id) (rhs:expr)
  : fstate * expr =
  (* 先简化 rhs *)
  let rhs1 = simplify_expr st rhs in
  (* 若纯表达式且已出现过等价键，直接替换为那个变量（复制传播+CSE） *)
  let rhs2 =
    match key_of_expr rhs1, expr_pure rhs1 with
    | Some k, true ->
        (match SMap.find_opt k st.cse with
         | Some y -> Var y
         | None -> rhs1)
    | _ -> rhs1
  in
  (* 更新 env *)
  let env' =
    match rhs2 with
    | Literal (IntLit n) -> env_set x (Const n) st.env
    | Var y when y <> x -> env_set x (Alias y) st.env
    | _ -> env_set x NonConst st.env
  in
  (* 更新 CSE 记录 *)
  let cse' =
    match key_of_expr rhs2, expr_pure rhs2 with
    | Some k, true -> SMap.add k x st.cse
    | _ -> st.cse
  in
  ({ env = env'; cse = cse' }, rhs2)

(* ========= 语句优化（线性、尾递归构建） ========= *)

let rec optimize_stmt (st:fstate) (s:stmt) : fstate * stmt =
  match s with
  | Empty -> (st, Empty)

  | ExprStmt e ->
      let e' = simplify_expr st e in
      let st' = if expr_has_side_effect e' then kill_on_call st else st in
      (st', if expr_has_side_effect e' then ExprStmt e' else Empty)

  | Assign (x, rhs) ->
      let st', rhs' = learn_assignment st x rhs in
      (st', Assign (x, rhs'))

  | Decl (x, rhs) ->
      let st', rhs' = learn_assignment st x rhs in
      (st', Decl (x, rhs'))

  | Return eo ->
      let eo' = Option.map (simplify_expr st) eo in
      (st, Return eo')

  | Break | Continue -> (st, s)

  | If (cond, thn, els) ->
      let cond' = simplify_expr st cond in
      begin match cond' with
      | Literal (IntLit n) when is_false n ->
          (match els with
           | None -> (st, Empty)
           | Some e -> optimize_stmt st e)
      | Literal (IntLit _) ->
          optimize_stmt st thn
      | _ ->
          let _, thn' = optimize_stmt st thn in
          let els' = Option.map (fun e -> snd (optimize_stmt st e)) els in
          (st, If (cond', thn', els'))
      end

  | While (cond, body) ->
      let cond' = simplify_expr st cond in
      begin match cond' with
      | Literal (IntLit 0) -> (st, Empty)
      | _ ->
          let _, body' = optimize_stmt st body in
          (st, While (cond', body'))
      end

  | Block ss ->
      let rec loop st acc = function
        | [] -> (st, List.rev acc)
        | _ when (match acc with Return _ :: _ -> true | _ -> false) ->
            (* return 之后的语句全部丢弃 *)
            (st, List.rev acc)
        | s :: tl ->
            let st', s' = optimize_stmt st s in
            let acc' = if s' = Empty then acc else s' :: acc in
            loop st' acc' tl
      in
      let st', ss' = loop st [] ss in
      (st', Block ss')

(* ========= 顶层：函数 / 程序 ========= *)

let optimize_func (f:func_def) : func_def =
  let _, body' =
    let rec loop st acc = function
      | [] -> (st, List.rev acc)
      | s :: tl ->
          let st', s' = optimize_stmt st s in
          let acc' = if s' = Empty then acc else s' :: acc in
          loop st' acc' tl
    in
    loop empty_state [] f.body
  in
  { f with body = body' }

let optimize (p:program) : program =
  List.map optimize_func p
