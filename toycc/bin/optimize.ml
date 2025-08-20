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
(* 循环优化：循环不变量外提 (LICM) 和公共子表达式消除 (CSE)                  *)
(*****************************************************************************)

open Ast

(* 变量集合操作 *)
module VarMap = Map.Make(String)

(* 辅助函数：判断表达式是否是变量 *)
let is_var = function
  | Var _ -> true
  | _ -> false

(* 辅助函数：获取表达式中所有变量 *)
let rec get_vars_expr expr =
  match expr with
  | Literal _ -> VarSet.empty
  | Var id -> VarSet.singleton id
  | BinOp (e1, _, e2) ->
      VarSet.union (get_vars_expr e1) (get_vars_expr e2)
  | UnOp (_, e) -> get_vars_expr e
  | Call (_, args) ->
      List.fold_left (fun acc e -> VarSet.union acc (get_vars_expr e)) VarSet.empty args
  | Paren e -> get_vars_expr e

(* 辅助函数：获取语句中所有被修改的变量 *)
let rec get_modified_vars_stmt stmt =
  match stmt with
  | Block stmts ->
      List.fold_left (fun acc s -> VarSet.union acc (get_modified_vars_stmt s)) VarSet.empty stmts
  | Assign (id, _) -> VarSet.singleton id
  | Decl (id, _) -> VarSet.singleton id
  | If (_, then_s, else_s_opt) ->
      let then_mod = get_modified_vars_stmt then_s in
      let else_mod = match else_s_opt with
        | Some s -> get_modified_vars_stmt s
        | None -> VarSet.empty
      in
      VarSet.union then_mod else_mod
  | While (_, body) -> get_modified_vars_stmt body
  | ExprStmt (Call (_, args)) ->
      List.fold_left (fun acc e -> VarSet.union acc (get_vars_expr e)) VarSet.empty args
  | _ -> VarSet.empty  (* 其他语句不修改变量 *)

(* 循环不变量外提 (LICM) 实现 *)

(* 判断表达式是否为循环不变量 *)
let is_invariant_expr modified_in_loop vars_in_cond expr =
  let expr_vars = get_vars_expr expr in
  (* 表达式变量不在循环修改的变量集中，且不在条件变量集中 *)
  VarSet.is_empty (VarSet.inter expr_vars modified_in_loop) &&
  VarSet.is_empty (VarSet.inter expr_vars vars_in_cond)

(* 递归处理语句，提取循环不变量 *)
let rec extract_invariants modified_in_loop vars_in_cond stmt =
  match stmt with
  | Block stmts ->
      let (new_stmts, invariants) = 
        List.fold_left (fun (stmts_acc, invs_acc) s ->
          let (s', invs) = extract_invariants modified_in_loop vars_in_cond s in
          (stmts_acc @ [s'], invs_acc @ invs)
        ) ([], []) stmts
      in
      (Block new_stmts, invariants)
      
  | Decl (_, expr) when is_invariant_expr modified_in_loop vars_in_cond expr ->
      (* 声明是不变量，提取到循环外 *)
      (Empty, [stmt])
      
  | Assign (id, expr) when is_invariant_expr modified_in_loop vars_in_cond expr &&
                          not (VarSet.mem id vars_in_cond) ->
      (* 赋值是不变量且不影响循环条件，提取到循环外 *)
      (Empty, [stmt])
      
  | If (cond, then_s, else_s_opt) ->
      let (then_s', then_invs) = extract_invariants modified_in_loop vars_in_cond then_s in
      let (else_s', else_invs) = match else_s_opt with
        | Some s -> extract_invariants modified_in_loop vars_in_cond s
        | None -> (Empty, [])
      in
      let combined_invs = 
        (* 只有在两个分支都出现的不变量才能外提 *)
        List.filter (fun inv -> List.mem inv else_invs) then_invs
      in
      let then_s'' = List.fold_left (fun s inv -> remove_stmt inv s) then_s' combined_invs in
      let else_s'' = List.fold_left (fun s inv -> remove_stmt inv s) else_s' combined_invs in
      (If (cond, then_s'', Some else_s''), combined_invs)
      
  | While _ -> 
      (* 嵌套循环，不处理其内部不变量 *)
      (stmt, [])
      
  | _ -> (stmt, [])

(* 从语句中移除特定子语句 *)
and remove_stmt target stmt =
  match stmt with
  | Block stmts ->
      Block (List.filter (fun s -> s <> target) stmts)
  | If (cond, then_s, else_s_opt) ->
      let then_s' = remove_stmt target then_s in
      let else_s_opt' = Option.map (remove_stmt target) else_s_opt in
      If (cond, then_s', else_s_opt')
  | _ -> stmt

(* 应用LICM到单个语句 *)
let rec apply_licm stmt =
  match stmt with
  | While (cond, body) ->
      (* 1. 收集循环条件中的变量 *)
      let vars_in_cond = get_vars_expr cond in
      
      (* 2. 收集循环体中被修改的变量 *)
      let modified_in_loop = get_modified_vars_stmt body in
      
      (* 3. 提取循环不变量 *)
      let (new_body, invariants) = extract_invariants modified_in_loop vars_in_cond body in
      
      (* 4. 递归处理新循环体 *)
      let optimized_body = apply_licm new_body in
      
      (* 5. 创建新的循环结构：先执行不变量，再进入循环 *)
      if invariants = [] then
        While (cond, optimized_body)
      else
        Block (invariants @ [While (cond, optimized_body)])
        
  | Block stmts ->
      Block (List.map apply_licm stmts)
      
  | If (cond, then_s, else_s_opt) ->
      let then_s' = apply_licm then_s in
      let else_s_opt' = Option.map apply_licm else_s_opt in
      If (cond, then_s', else_s_opt')
      
  | _ -> stmt


(* 公共子表达式消除 (CSE) 实现 *)

(* 表达式哈希，用于比较表达式是否相同 *)
module ExprHash = struct
  type t = expr
  let rec equal e1 e2 =
    match e1, e2 with
    | Literal l1, Literal l2 -> l1 = l2
    | Var v1, Var v2 -> v1 = v2
    | BinOp (a1, op1, b1), BinOp (a2, op2, b2) ->
        op1 = op2 && equal a1 a2 && equal b1 b2
    | UnOp (op1, a1), UnOp (op2, a2) ->
        op1 = op2 && equal a1 a2
    | Call (f1, args1), Call (f2, args2) ->
        f1 = f2 && List.for_all2 equal args1 args2
    | Paren e1, Paren e2 -> equal e1 e2
    | _ -> false

  let hash = Hashtbl.hash
end

module ExprMap = Hashtbl.Make(ExprHash)

(* 为表达式生成临时变量名 *)
let fresh_cse_var counter =
  let id = !counter in
  counter := id + 1;
  "__cse_" ^ string_of_int id

(* 替换表达式为临时变量 *)
let rec replace_expr expr_map expr =
  match expr with
  | BinOp (e1, op, e2) ->
      let e1' = replace_expr expr_map e1 in
      let e2' = replace_expr expr_map e2 in
      let e = BinOp (e1', op, e2') in
      begin match ExprMap.find_opt expr_map e with
      | Some var -> Var var
      | None -> e
      end
  | UnOp (op, e) ->
      let e' = replace_expr expr_map e in
      let e = UnOp (op, e') in
      begin match ExprMap.find_opt expr_map e with
      | Some var -> Var var
      | None -> e
      end
  | Call (f, args) ->
      let args' = List.map (replace_expr expr_map) args in
      Call (f, args')
  | Paren e ->
      let e' = replace_expr expr_map e in
      Paren e'
  | _ -> expr  (* 变量和常量不替换 *)

(* 收集公共子表达式 *)
let rec collect_common_subexprs expr_map counter expr =
  match expr with
  | BinOp (e1, op, e2) ->  (* 修复：捕获操作符op而不是使用_ *)
      collect_common_subexprs expr_map counter e1;
      collect_common_subexprs expr_map counter e2;
      let e1' = replace_expr expr_map e1 in
      let e2' = replace_expr expr_map e2 in
      let e = BinOp (e1', op, e2') in  (* 使用捕获的op *)
      if not (is_var e) && not (ExprMap.mem expr_map e) then
        let var = fresh_cse_var counter in
        ExprMap.add expr_map e var
  | UnOp (op, e) ->  (* 修复：捕获操作符op而不是使用_ *)
      collect_common_subexprs expr_map counter e;
      let e' = replace_expr expr_map e in
      let e = UnOp (op, e') in  (* 使用捕获的op *)
      if not (is_var e) && not (ExprMap.mem expr_map e) then
        let var = fresh_cse_var counter in
        ExprMap.add expr_map e var
  | Call (_, args) ->
      List.iter (collect_common_subexprs expr_map counter) args
  | Paren e -> collect_common_subexprs expr_map counter e
  | _ -> ()  (* 变量和常量不处理 *)

(* 替换语句中的公共子表达式 *)
let rec replace_common_subexprs expr_map stmt =
  match stmt with
  | Block stmts ->
      Block (List.map (replace_common_subexprs expr_map) stmts)
  | ExprStmt expr ->
      ExprStmt (replace_expr expr_map expr)
  | Assign (id, expr) ->
      Assign (id, replace_expr expr_map expr)
  | Decl (id, expr) ->
      Decl (id, replace_expr expr_map expr)
  | If (cond, then_s, else_s_opt) ->
      let cond' = replace_expr expr_map cond in
      let then_s' = replace_common_subexprs expr_map then_s in
      let else_s' = Option.map (replace_common_subexprs expr_map) else_s_opt in
      If (cond', then_s', else_s')
  | While (cond, body) ->
      let cond' = replace_expr expr_map cond in
      let body' = replace_common_subexprs expr_map body in
      While (cond', body')
  | Return expr_opt ->
      Return (Option.map (replace_expr expr_map) expr_opt)
  | _ -> stmt  (* Break, Continue, Empty 不需要处理 *)

(* 应用CSE到单个语句 *)
let rec apply_cse stmt =
  match stmt with
  | Block stmts ->
      (* 1. 收集整个块中的公共子表达式 *)
      let expr_map = ExprMap.create 100 in
      let counter = ref 0 in
      
      (* 先收集所有表达式 *)
      let rec collect_all_exprs stmt =
        match stmt with
        | Block stmts -> List.iter collect_all_exprs stmts
        | ExprStmt e | Assign (_, e) | Decl (_, e) -> collect_common_subexprs expr_map counter e
        | If (e, s1, s2_opt) ->
            collect_common_subexprs expr_map counter e;
            collect_all_exprs s1;
            Option.iter collect_all_exprs s2_opt
        | While (e, s) ->
            collect_common_subexprs expr_map counter e;
            collect_all_exprs s
        | Return (Some e) -> collect_common_subexprs expr_map counter e
        | _ -> ()
      in
      List.iter collect_all_exprs stmts;
      
      (* 2. 生成临时变量声明 *)
      let temp_decls = ExprMap.fold (fun e var acc ->
        Decl (var, e) :: acc
      ) expr_map [] in
      
      (* 3. 替换公共子表达式 *)
      let replaced_stmts = List.map (replace_common_subexprs expr_map) stmts in
      
      (* 4. 递归处理替换后的语句 *)
      let optimized_stmts = List.map apply_cse replaced_stmts in
      
      Block (temp_decls @ optimized_stmts)
      
  | While (cond, body) ->
      (* 对循环体单独应用CSE *)
      While (cond, apply_cse body)
      
  | If (cond, then_s, else_s_opt) ->
      If (cond, apply_cse then_s, Option.map apply_cse else_s_opt)
      
  | _ -> stmt


(* 循环优化入口函数 *)
let optimize_loops program =
  List.map (fun func ->
    { func with 
      body = List.map (fun stmt ->
        stmt
        |> apply_licm    (* 先应用循环不变量外提 *)
        |> apply_cse     (* 再应用公共子表达式消除 *)
      ) func.body
    }
  ) program


(*****************************************************************************)
(* 更新优化流水线                                                           *)
(*****************************************************************************)

let optimize program =
  program 
  |> fold_constants        (* 1. 常量折叠 *)
  |> eliminate_dead_code   (* 2. 死代码消除 *)
  |> optimize_loops        (* 3. 循环优化 (新增) *)
  |> optimize_tail_recursion (* 4. 尾递归优化 *)
    
