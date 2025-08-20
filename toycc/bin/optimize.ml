open Ast

(*****************************************************************************)
(* 尾递归优化 (TCO)                                                         *)
(*****************************************************************************)

(* 获取列表的最后一个元素和前面的元素 *)
let rec last_and_init = function
  | [] -> failwith "Internal error: last_and_init called on an empty list"
  | [x] -> (x, [])
  | h :: t -> let (last, init) = last_and_init t in (last, h :: init)

(* 扫描语句，检查是否包含尾递归调用 *)
let rec contains_tco_candidate (func: func_def) (is_tail_pos: bool) (stmt: stmt) : bool =
  match stmt with
  | Return (Some (Call(callee, args))) ->
      (* 如果在尾位置，且调用自身，参数数量匹配，则是尾递归候选 *)
      is_tail_pos && callee = func.fname && List.length args = List.length func.params
  
  | If (_, then_s, else_s_opt) ->
      (* 检查if语句的两个分支 *)
      let then_has = contains_tco_candidate func is_tail_pos then_s in
      let else_has = match else_s_opt with
        | Some else_s -> contains_tco_candidate func is_tail_pos else_s
        | None -> false
      in
      then_has || else_has

  | Block (stmts) ->
      if not is_tail_pos || stmts = [] then
        (* 非尾位置，检查任何语句是否有尾递归 *)
        List.exists (contains_tco_candidate func false) stmts
      else
        (* 尾位置，只有最后一个语句可能是尾递归 *)
        let (last, init) = last_and_init stmts in
        List.exists (contains_tco_candidate func false) init || 
        contains_tco_candidate func true last

  | While (_, body) -> 
      (* while循环体中没有尾位置 *)
      contains_tco_candidate func false body
  
  | _ -> false

(* 转换语句，将尾递归调用转换为赋值和continue *)
let rec transform_stmt_for_tco (func: func_def) (is_tail_pos: bool) (fresh_var_gen: unit -> id) (stmt: stmt) : stmt =
  match stmt with
  | Return (Some (Call(callee, args))) 
    when is_tail_pos && callee = func.fname && List.length args = List.length func.params ->
      (* 处理尾递归调用：将参数存储到临时变量，赋值给函数参数，然后continue *)
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
      (* 转换if语句的两个分支 *)
      let new_then = transform_stmt_for_tco func is_tail_pos fresh_var_gen then_s in
      let new_else_opt = Option.map (transform_stmt_for_tco func is_tail_pos fresh_var_gen) else_s_opt in
      If (cond, new_then, new_else_opt)

  | Block (stmts) ->
      if not is_tail_pos || stmts = [] then
        (* 非尾位置，转换所有语句 *)
        Block (List.map (transform_stmt_for_tco func false fresh_var_gen) stmts)
      else
        (* 尾位置，只将最后一个语句视为尾位置 *)
        let (last, init) = last_and_init stmts in
        let transformed_init = List.map (transform_stmt_for_tco func false fresh_var_gen) init in
        let transformed_last = transform_stmt_for_tco func true fresh_var_gen last in
        Block(transformed_init @ [transformed_last])
        
  | While (cond, body) ->
      (* 转换while循环体 *)
      While(cond, transform_stmt_for_tco func false fresh_var_gen body)

  | _ -> stmt

(* 单个函数的尾递归优化 *)
let optimize_func_for_tco (func: func_def) : func_def =
  (* 检查函数是否有尾递归调用 *)
  let has_tco_candidate = List.exists (contains_tco_candidate func true) func.body in
  
  if not has_tco_candidate then
    func  (* 没有尾递归，直接返回 *)
  else
    (* 生成新的变量名 *)
    let counter = ref 0 in
    let fresh_var_gen () =
      counter := !counter + 1;
      "__tco_" ^ func.fname ^ "_" ^ (string_of_int !counter)
    in
    (* 转换函数体中的语句 *)
    let transformed_body_stmts = List.map (transform_stmt_for_tco func true fresh_var_gen) func.body in
    (* 创建一个无限循环包裹转换后的代码 *)
    let true_expr = Literal (IntLit 1) in  (* 非0值表示真 *)
    let loop_body = Block transformed_body_stmts in
    let new_body = [While (true_expr, loop_body)] in
    { func with body = new_body }

(*****************************************************************************)
(* 强化版常量折叠优化                                                       *)
(*****************************************************************************)

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
            (* 比较运算符支持 *)
            | "<" -> if n1 < n2 then 1 else 0
            | ">" -> if n1 > n2 then 1 else 0
            | "<=" -> if n1 <= n2 then 1 else 0
            | ">=" -> if n1 >= n2 then 1 else 0
            | "==" -> if n1 = n2 then 1 else 0
            | "!=" -> if n1 != n2 then 1 else 0
            | _ -> failwith ("Unsupported operator for constant folding: " ^ op)
          in
          Literal (IntLit result)
      (* 扩展部分折叠规则集 *)
      | e, Literal (IntLit 0) when op = "+" -> e  (* x + 0 → x *)
      | Literal (IntLit 0), e when op = "+" -> e  (* 0 + x → x *)
      | e, Literal (IntLit 0) when op = "-" -> e  (* x - 0 → x *)
      | Literal (IntLit 0), e when op = "-" -> UnOp ("-", e)  (* 0 - x → -x *)
      | Literal (IntLit 1), e when op = "*" -> e  (* 1 * x → x *)
      | e, Literal (IntLit 1) when op = "*" -> e  (* x * 1 → x *)
      | _, Literal (IntLit 0) when op = "*" -> Literal (IntLit 0)  (* x * 0 → 0 *)
      | Literal (IntLit 0), _ when op = "*" -> Literal (IntLit 0)  (* 0 * x → 0 *)
      | e1, BinOp(e2, "+", _) when e1 = e2 -> BinOp(e1, "*", Literal (IntLit 2))  (* x + x → 2*x *)
      | BinOp(_, "+", e2), e3 when e2 = e3 -> BinOp(e2, "*", Literal (IntLit 2))  (* x + y + y → x + 2*y *)
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
      | UnOp ("!", e'') -> e''  (* !!x → x *)
      | UnOp ("-", UnOp ("-", e'')) -> e''  (* -(-x) → x *)
      | _ -> UnOp (op, e')
      end
  | Call (fname, args) ->
      let args' = List.map fold_constants_expr args in
      Call (fname, args')
  | Paren e ->
      let e' = fold_constants_expr e in
      begin match e' with
      | Paren e'' -> e''  (* 移除嵌套括号 ((x)) → x *)
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

(*****************************************************************************)
(* 循环优化                                                                 *)
(*****************************************************************************)

(* 检测并移除循环中的冗余计算 *)
let rec remove_loop_redundancies_stmt stmt =
  match stmt with
  | While (cond, body) ->
      (* 分析循环体，找出可以提升到循环外的计算 *)
      let loop_invariants = find_loop_invariants body in
      let body' = remove_invariants_from_body loop_invariants body in
      (* 创建一个块，先执行不变量计算，再执行循环 *)
      let invariant_decls = List.map (fun (id, expr) -> Decl(id, expr)) loop_invariants in
      Block (invariant_decls @ [While (cond, body')])
      
  | Block stmts ->
      Block (List.map remove_loop_redundancies_stmt stmts)
      
  | If (cond, then_stmt, else_stmt_opt) ->
      let then_stmt' = remove_loop_redundancies_stmt then_stmt in
      let else_stmt_opt' = Option.map remove_loop_redundancies_stmt else_stmt_opt in
      If (cond, then_stmt', else_stmt_opt')
      
  | _ -> stmt

(* 查找循环中的不变量表达式 *)
and find_loop_invariants stmt =
  (* 简单实现：查找不依赖循环变量的声明 *)
  match stmt with
  | Block stmts ->
      List.filter_map (fun s ->
        match s with
        | Decl (id, expr) when not (depends_on_loop_vars expr) ->
            Some (id, expr)
        | _ -> None
      ) stmts
  | _ -> []

(* 检查表达式是否依赖循环变量 *)
and depends_on_loop_vars expr =
  (* 简化实现：假设循环变量是常见的"i", "j", "k"等 *)
  let loop_vars = ["i"; "j"; "k"; "x"; "y"; "z"] in
  let rec check expr =
    match expr with
    | Var id -> List.mem id loop_vars
    | BinOp (e1, _, e2) -> check e1 || check e2
    | UnOp (_, e) -> check e
    | Call (_, args) -> List.exists check args
    | Paren e -> check e
    | _ -> false
  in
  check expr

(* 从循环体中移除已提升的不变量 *)
and remove_invariants_from_body invariants body =
  match body with
  | Block stmts ->
      let invariant_ids = List.map fst invariants in
      let filtered_stmts = List.filter (fun s ->
        match s with
        | Decl (id, _) -> not (List.mem id invariant_ids)
        | _ -> true
      ) stmts in
      Block (List.map (remove_loop_redundancies_stmt) filtered_stmts)
  | _ -> remove_loop_redundancies_stmt body

(* 循环优化入口 *)
let optimize_loops program =
  List.map (fun func ->
    { func with body = List.map remove_loop_redundancies_stmt func.body }
  ) program

(*****************************************************************************)
(* 增强版死代码消除                                                         *)
(*****************************************************************************)

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
(* 最终的优化流水线                                                         *)
(*****************************************************************************)

let optimize_tail_recursion (prog: program) : program =
  List.map optimize_func_for_tco prog



let optimize program =
  program 
  |> fold_constants        (* 1. 常量折叠 *)
  |> eliminate_dead_code   (* 2. 死代码消除 *)
  |> optimize_loops        (* 3. 循环优化 - 新增 *)
  |> fold_constants        (* 4. 再次常量折叠 - 新增 *)
  |> eliminate_dead_code   (* 5. 再次死代码消除 - 新增 *)
  |> optimize_tail_recursion (* 6. 尾递归优化 *)
