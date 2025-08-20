open Ast

(*****************************************************************************)
(* 尾递归优化 (TCO) - 新增部分                                               *)
(*****************************************************************************)

(* Helper to get the last element and the preceding elements of a list. *)
let rec last_and_init = function
  | [] -> failwith "Internal error: last_and_init called on an empty list"
  | [x] -> (x, [])
  | h :: t -> let (last, init) = last_and_init t in (last, h :: init)

(* Scans a statement to see if it contains a tail-recursive call. *)
let rec contains_tco_candidate (func: func_def) (is_tail_pos: bool) (stmt: stmt) : bool =
  match stmt with
  | Return (Some (Call(callee, args))) ->
      is_tail_pos && callee = func.fname && List.length args = List.length func.params
  
  | If (_, then_s, else_s_opt) ->
      let then_has = contains_tco_candidate func is_tail_pos then_s in
      let else_has = match else_s_opt with
        | Some else_s -> contains_tco_candidate func is_tail_pos else_s
        | None -> false
      in
      then_has || else_has

  | Block (stmts) ->
      if not is_tail_pos || stmts = [] then
        List.exists (contains_tco_candidate func false) stmts
      else
        let (last, init) = last_and_init stmts in
        List.exists (contains_tco_candidate func false) init || contains_tco_candidate func true last

  | While (_, body) -> 
      contains_tco_candidate func false body
  
  | _ -> false

(* Transforms a statement, converting tail-recursive calls into assignments and a continue. *)
let rec transform_stmt_for_tco (func: func_def) (is_tail_pos: bool) (fresh_var_gen: unit -> id) (stmt: stmt) : stmt =
  match stmt with
  | Return (Some (Call(callee, args))) 
    when is_tail_pos && callee = func.fname && List.length args = List.length func.params ->
      let params = func.params in
      let temp_decls_and_names = List.map (fun arg_expr ->
        let temp_name = fresh_var_gen () in
        (temp_name, Decl(temp_name, arg_expr))
      ) args in
      let temp_decls = List.map snd temp_decls_and_names in
      let temp_names = List.map fst temp_decls_and_names in
      let assignments = List.map2 (fun param temp_name ->
        Assign(param.pname, Var temp_name)
      ) params temp_names in
      Block(temp_decls @ assignments @ [Continue])

  | If (cond, then_s, else_s_opt) ->
      let new_then = transform_stmt_for_tco func is_tail_pos fresh_var_gen then_s in
      let new_else_opt = Option.map (transform_stmt_for_tco func is_tail_pos fresh_var_gen) else_s_opt in
      If (cond, new_then, new_else_opt)

  | Block (stmts) ->
      if not is_tail_pos || stmts = [] then
        Block (List.map (transform_stmt_for_tco func false fresh_var_gen) stmts)
      else
        let (last, init) = last_and_init stmts in
        let transformed_init = List.map (transform_stmt_for_tco func false fresh_var_gen) init in
        let transformed_last = transform_stmt_for_tco func true fresh_var_gen last in
        Block(transformed_init @ [transformed_last])
        
  | While (cond, body) ->
      While(cond, transform_stmt_for_tco func false fresh_var_gen body)

  | _ -> stmt

(* Main optimization function for a single function definition. *)
let optimize_func_for_tco (func: func_def) : func_def =
  let has_tco_candidate = List.exists (contains_tco_candidate func true) func.body in
  
  if not has_tco_candidate then
    func
  else
    let counter = ref 0 in
    let fresh_var_gen () =
      counter := !counter + 1;
      "__tco_" ^ func.fname ^ "_" ^ (string_of_int !counter)
    in
    let transformed_body_stmts = List.map (transform_stmt_for_tco func true fresh_var_gen) func.body in
    let true_expr = Literal (IntLit 1) in
    let loop_body = Block transformed_body_stmts in
    let new_body = [While (true_expr, loop_body)] in
    { func with body = new_body }

(* Top-level entry point for the TCO pass. *)
let optimize_tail_recursion (prog: program) : program =
  List.map optimize_func_for_tco prog


(*****************************************************************************)
(* 您原有的优化代码                                                          *)
(*****************************************************************************)

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

(* 增强版死代码消除入口 *)
let eliminate_dead_code program =
  List.map (fun func ->
    (* 1. 第一次消除不可达语句 *)
    let body_reachable, _ = eliminate_dead_stmts true func.body in
    (* 2. 第二次深度消除（处理嵌套块） *)
    let body_deep_reachable = 
      List.map (fun s -> 
        let s', _ = eliminate_dead_stmt true s in
        Option.value s' ~default:Empty
      ) body_reachable
    in
    (* 3. 收集使用的变量 *)
    let used_vars = List.fold_left collect_vars_stmt VarSet.empty body_deep_reachable in
    (* 4. 移除未使用的变量 *)
    let body_unused_removed = List.map (remove_unused_stmt used_vars) body_deep_reachable in
    (* 5. 简化空语句 *)
    let body_simplified = List.map simplify_empty_stmt body_unused_removed in
    { func with body = body_simplified }
  ) program

(*****************************************************************************)
(* 最终的优化流水线                                                          *)
(*****************************************************************************)

(*****************************************************************************)
(* 公共子表达式消除 (CSE)                                                    *)
(*****************************************************************************)

module ExprMap = Map.Make(struct
  type t = expr
  let compare = compare
end)

module ExprSet = Set.Make(struct
  type t = expr
  let compare = compare
end)

(* 收集表达式中所有的子表达式 *)
let rec collect_subexpressions expr =
  let subexprs = ExprSet.singleton expr in
  match expr with
  | Literal _ | Var _ -> subexprs
  | BinOp (e1, _, e2) ->
      let s1 = collect_subexpressions e1 in
      let s2 = collect_subexpressions e2 in
      ExprSet.union (ExprSet.union s1 s2) subexprs
  | UnOp (_, e) ->
      let s = collect_subexpressions e in
      ExprSet.union s subexprs
  | Call (_, args) ->
      let s = List.fold_left (fun acc e ->
        ExprSet.union acc (collect_subexpressions e)
      ) ExprSet.empty args in
      ExprSet.union s subexprs
  | Paren e ->
      let s = collect_subexpressions e in
      ExprSet.union s subexprs

(* 计算表达式出现的次数 *)
let count_subexpression_occurrences expr =
  let subexprs = collect_subexpressions expr in
  ExprSet.fold (fun e acc ->
    let count = ref 0 in
    let rec count_occurrences e target =
      if e = target then incr count;
      match e with
      | Literal _ | Var _ -> ()
      | BinOp (e1, _, e2) ->
          count_occurrences e1 target;
          count_occurrences e2 target
      | UnOp (_, e) -> count_occurrences e target
      | Call (_, args) -> List.iter (fun a -> count_occurrences a target) args
      | Paren e -> count_occurrences e target
    in
    count_occurrences expr e;
    ExprMap.add e !count acc
  ) subexprs ExprMap.empty

(* 替换表达式中的子表达式 *)
let rec replace_subexpr expr target replacement =
  if expr = target then replacement
  else
    match expr with
    | Literal _ | Var _ -> expr
    | BinOp (e1, op, e2) ->
        BinOp (replace_subexpr e1 target replacement, op, replace_subexpr e2 target replacement)
    | UnOp (op, e) ->
        UnOp (op, replace_subexpr e target replacement)
    | Call (fname, args) ->
        Call (fname, List.map (fun a -> replace_subexpr a target replacement) args)
    | Paren e ->
        Paren (replace_subexpr e target replacement)

(* 对表达式应用CSE *)
let rec cse_expr (counter: int ref) (expr: expr) : expr * stmt list =
  let occurrences = count_subexpression_occurrences expr in
  
  (* 找到出现次数超过1次的复杂子表达式（不是简单变量或字面量） *)
  let candidates = ExprMap.fold (fun e count acc ->
    match e with
    | Literal _ | Var _ -> acc  (* 跳过简单表达式 *)
    | _ when count > 1 -> e :: acc
    | _ -> acc
  ) occurrences [] in
  
  (* 按表达式复杂度排序，先处理更复杂的表达式 *)
  let sorted_candidates = List.sort (fun e1 e2 ->
    let size1 = ExprSet.cardinal (collect_subexpressions e1) in
    let size2 = ExprSet.cardinal (collect_subexpressions e2) in
    compare size2 size1  (* 降序排序 *)
  ) candidates in
  
  match sorted_candidates with
  | [] -> (expr, [])
  | candidate :: _ ->
      (* 创建新的临时变量 *)
      counter := !counter + 1;
      let temp_var = "__cse_" ^ string_of_int !counter in
      
      (* 递归处理候选表达式本身 *)
      let simplified_candidate, decls1 = cse_expr counter candidate in
      
      (* 用临时变量替换原表达式中的候选子表达式 *)
      let new_expr = replace_subexpr expr candidate (Var temp_var) in
      
      (* 递归处理新表达式 *)
      let simplified_expr, decls2 = cse_expr counter new_expr in
      
      (* 创建临时变量声明 *)
      let temp_decl = Decl (temp_var, simplified_candidate) in
      
      (simplified_expr, decls1 @ [temp_decl] @ decls2)

(* 对语句应用CSE *)
let rec cse_stmt (counter: int ref) (stmt: stmt) : stmt =
  match stmt with
  | Block stmts ->
      (* 使用List.map和List.flatten替代List.flat_map以兼容旧版本OCaml *)
      let new_stmts = List.flatten (List.map (fun s ->
        match cse_stmt counter s with
        | Block bs -> bs
        | s' -> [s']
      ) stmts) in
      Block new_stmts
  | Empty -> Empty
  | ExprStmt expr ->
      let simplified_expr, decls = cse_expr counter expr in
      Block (decls @ [ExprStmt simplified_expr])
  | Assign (id, expr) ->
      let simplified_expr, decls = cse_expr counter expr in
      Block (decls @ [Assign (id, simplified_expr)])
  | Decl (id, expr) ->
      let simplified_expr, decls = cse_expr counter expr in
      Block (decls @ [Decl (id, simplified_expr)])
  | If (cond, then_stmt, else_stmt_opt) ->
      let simplified_cond, decls = cse_expr counter cond in
      let new_then = cse_stmt counter then_stmt in
      let new_else_opt = Option.map (cse_stmt counter) else_stmt_opt in
      Block (decls @ [If (simplified_cond, new_then, new_else_opt)])
  | While (cond, body) ->
      let simplified_cond, decls = cse_expr counter cond in
      let new_body = cse_stmt counter body in
      Block (decls @ [While (simplified_cond, new_body)])
  | Break -> Break
  | Continue -> Continue
  | Return expr_opt ->
      let new_expr_opt, decls = match expr_opt with
        | Some expr ->
            let e, d = cse_expr counter expr in
            (Some e, d)
        | None -> (None, [])
      in
      Block (decls @ [Return new_expr_opt])

(* 对函数应用CSE *)
let cse_func (func: func_def) : func_def =
  let counter = ref 0 in
  { func with body = List.map (cse_stmt counter) func.body }

(* CSE优化入口 *)
let eliminate_common_subexpressions (program: program) : program =
  List.map cse_func program


(*****************************************************************************)
(* 循环不变量外提 (LICM)                                                     *)
(*****************************************************************************)

(* 收集表达式中使用的变量 *)
let rec get_vars expr =
  match expr with
  | Literal _ -> VarSet.empty
  | Var id -> VarSet.singleton id
  | BinOp (e1, _, e2) -> VarSet.union (get_vars e1) (get_vars e2)
  | UnOp (_, e) -> get_vars e
  | Call (_, args) ->
      List.fold_left (fun acc e -> VarSet.union acc (get_vars e)) VarSet.empty args
  | Paren e -> get_vars e

(* 收集语句中赋值或声明的变量 *)
let rec get_assigned_vars stmt =
  match stmt with
  | Block stmts ->
      List.fold_left (fun acc s -> VarSet.union acc (get_assigned_vars s)) VarSet.empty stmts
  | Assign (id, _) -> VarSet.singleton id
  | Decl (id, _) -> VarSet.singleton id
  | If (_, then_stmt, else_stmt_opt) ->
      let then_vars = get_assigned_vars then_stmt in
      let else_vars = match else_stmt_opt with
        | Some s -> get_assigned_vars s
        | None -> VarSet.empty
      in
      VarSet.union then_vars else_vars
  | While (_, body) -> get_assigned_vars body
  | _ -> VarSet.empty  (* 其他语句不改变变量集 *)

(* 检查表达式是否是循环不变量 *)
let is_invariant expr loop_vars =
  let expr_vars = get_vars expr in
  VarSet.disjoint expr_vars loop_vars

(* 重写表达式中的变量，替换为外提的临时变量 *)
let rec rewrite_expr expr var_map =
  match expr with
  | Var id ->
      (match VarSet.find_opt id var_map with
       | Some temp -> Var temp
       | None -> expr)
  | BinOp (e1, op, e2) ->
      BinOp (rewrite_expr e1 var_map, op, rewrite_expr e2 var_map)
  | UnOp (op, e) ->
      UnOp (op, rewrite_expr e var_map)
  | Call (fname, args) ->
      Call (fname, List.map (fun e -> rewrite_expr e var_map) args)
  | Paren e ->
      Paren (rewrite_expr e var_map)
  | _ -> expr  (* 字面量不变 *)

(* 重写语句中的变量，替换为外提的临时变量 *)
let rec rewrite_stmt stmt var_map =
  match stmt with
  | Block stmts ->
      Block (List.map (fun s -> rewrite_stmt s var_map) stmts)
  | ExprStmt expr ->
      ExprStmt (rewrite_expr expr var_map)
  | Assign (id, expr) ->
      Assign (id, rewrite_expr expr var_map)
  | Decl (id, expr) ->
      Decl (id, rewrite_expr expr var_map)
  | If (cond, then_stmt, else_stmt_opt) ->
      If (rewrite_expr cond var_map,
          rewrite_stmt then_stmt var_map,
          Option.map (fun s -> rewrite_stmt s var_map) else_stmt_opt)
  | While (cond, body) ->
      While (rewrite_expr cond var_map, rewrite_stmt body var_map)
  | Return expr_opt ->
      Return (Option.map (fun e -> rewrite_expr e var_map) expr_opt)
  | _ -> stmt  (* Break, Continue, Empty不变 *)

(* 提取循环不变量表达式 *)
let rec extract_invariants (counter: int ref) loop_vars stmt =
  match stmt with
  | Block stmts ->
      let invariant_decls = ref [] in
      let var_map = ref VarSet.empty in
      let new_stmts = List.map (fun s ->
        let invars, vars, s' = extract_invariants counter loop_vars s in
        invariant_decls := !invariant_decls @ invars;
        var_map := VarSet.union !var_map vars;
        rewrite_stmt s' !var_map
      ) stmts in
      (!invariant_decls, !var_map, Block new_stmts)
  
  | ExprStmt expr when is_invariant expr loop_vars ->
      (* 提取表达式不变量 *)
      counter := !counter + 1;
      let temp_var = "__licm_" ^ string_of_int !counter in
      let decl = Decl (temp_var, expr) in
      ([decl], VarSet.singleton temp_var, ExprStmt (Var temp_var))
  
  | Assign (id, expr) when is_invariant expr loop_vars && not (VarSet.mem id loop_vars) ->
      (* 提取赋值不变量 *)
      counter := !counter + 1;
      let temp_var = "__licm_" ^ string_of_int !counter in
      let decl = Decl (temp_var, expr) in
      ([decl], VarSet.singleton temp_var, Assign (id, Var temp_var))
  
  | Decl (id, expr) when is_invariant expr loop_vars && not (VarSet.mem id loop_vars) ->
      (* 提取声明不变量 *)
      counter := !counter + 1;
      let temp_var = "__licm_" ^ string_of_int !counter in
      let decl = Decl (temp_var, expr) in
      ([decl], VarSet.singleton temp_var, Decl (id, Var temp_var))
  
  | If (cond, then_stmt, else_stmt_opt) ->
      let cond_invariant = is_invariant cond loop_vars in
      let then_invars, then_vars, then_stmt' = extract_invariants counter loop_vars then_stmt in
      let else_invars, else_vars, else_stmt_opt' = 
        match else_stmt_opt with
        | Some s ->
            let i, v, s' = extract_invariants counter loop_vars s in
            (i, v, Some s')
        | None -> ([], VarSet.empty, None)
      in
      
      (* 公共不变量可以外提到if语句外 *)
      let common_invars = List.filter (fun inv1 ->
        List.exists (fun inv2 -> inv1 = inv2) else_invars
      ) then_invars in
      
      let then_only_invars = List.filter (fun inv ->
        not (List.exists (fun inv' -> inv = inv') common_invars)
      ) then_invars in
      
      let else_only_invars = List.filter (fun inv ->
        not (List.exists (fun inv' -> inv = inv') common_invars)
      ) else_invars in
      
      let var_map = VarSet.union then_vars else_vars in
      
      let new_then = Block (then_only_invars @ [then_stmt']) in
      let new_else_opt = Option.map (fun s -> Block (else_only_invars @ [s])) else_stmt_opt' in
      
      let cond_decl, cond_var, new_cond = 
        if cond_invariant then (
          counter := !counter + 1;
          let temp_var = "__licm_cond_" ^ string_of_int !counter in
          ([Decl (temp_var, cond)], VarSet.singleton temp_var, Var temp_var)
        ) else (
          ([], VarSet.empty, cond)
        )
      in
      
      let all_invars = cond_decl @ common_invars in
      let all_vars = VarSet.union cond_var var_map in
      
      (all_invars, all_vars, If (new_cond, new_then, new_else_opt))
  
  | While (cond, body) ->
      (* 处理嵌套循环 - 先处理内层循环 *)
      let loop_vars = get_assigned_vars body in
      let body_invars, body_var_map, new_body = extract_invariants counter loop_vars body in
      (body_invars, body_var_map, While (cond, new_body))
  
  | _ -> ([], VarSet.empty, stmt)  (* 不是不变量，返回原语句 *)

(* 对语句应用LICM *)
let rec licm_stmt (counter: int ref) stmt =
  match stmt with
  | While (cond, body) ->
      (* 计算循环中被修改的变量 *)
      let loop_vars = get_assigned_vars body in
      
      (* 检查循环条件是否是不变量 *)
      let cond_invariant = is_invariant cond loop_vars in
      let cond_decl, new_cond =
        if cond_invariant then (
          counter := !counter + 1;
          let temp_var = "__licm_cond_" ^ string_of_int !counter in
          ([Decl (temp_var, cond)], Var temp_var)
        ) else (
          ([], cond)
        )
      in
      
      (* 从循环体中提取不变量 *)
      let invariants, var_map, new_body = extract_invariants counter loop_vars body in
      
      (* 重写循环体，使用外提的变量 *)
      let rewritten_body = rewrite_stmt new_body var_map in
      
      (* 创建新的循环结构：外提的不变量 + 新循环 *)
      Block (cond_decl @ invariants @ [While (new_cond, rewritten_body)])
  
  | Block stmts ->
      Block (List.map (licm_stmt counter) stmts)
  
  | If (cond, then_stmt, else_stmt_opt) ->
      let new_then = licm_stmt counter then_stmt in
      let new_else_opt = Option.map (licm_stmt counter) else_stmt_opt in
      If (cond, new_then, new_else_opt)
  
  | _ -> stmt  (* 其他语句不变 *)

(* 对函数应用LICM *)
let licm_func (func: func_def) : func_def =
  let counter = ref 0 in
  { func with body = List.map (licm_stmt counter) func.body }

(* LICM优化入口 *)
let lift_loop_invariants (program: program) : program =
  List.map licm_func program


(*****************************************************************************)
(* 更新后的优化流水线                                                        *)
(*****************************************************************************)

let optimize program =
  program 
  |> fold_constants                 (* 1. 常量折叠 *)
  |> eliminate_common_subexpressions (* 2. 公共子表达式消除 (新增) *)
  |> lift_loop_invariants            (* 3. 循环不变量外提 (新增) *)
  |> eliminate_dead_code             (* 4. 死代码消除 *)
  |> optimize_tail_recursion         (* 5. 尾递归优化 *)
    
