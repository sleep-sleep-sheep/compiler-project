open Ast

(*****************************************************************************)
(* 共享的工具函数                                                           *)
(*****************************************************************************)

module VarSet = Set.Make(String)

(* 获取列表的最后一个元素和前面的元素 *)
let rec last_and_init = function
  | [] -> failwith "Internal error: last_and_init called on an empty list"
  | [x] -> (x, [])
  | h :: t -> let (last, init) = last_and_init t in (last, h :: init)

(* 收集表达式中使用的变量 *)
let rec expr_vars expr =
  match expr with
  | Literal _ -> VarSet.empty
  | Var id -> VarSet.singleton id
  | BinOp (e1, _, e2) -> VarSet.union (expr_vars e1) (expr_vars e2)
  | UnOp (_, e) -> expr_vars e
  | Call (_, args) -> List.fold_left (fun s e -> VarSet.union s (expr_vars e)) VarSet.empty args
  | Paren e -> expr_vars e

(* 收集语句中赋值的变量 *)
let rec stmt_defs stmt =
  match stmt with
  | Block stmts -> List.fold_left (fun s stmt -> VarSet.union s (stmt_defs stmt)) VarSet.empty stmts
  | Assign (id, _) -> VarSet.singleton id
  | Decl (id, _) -> VarSet.singleton id
  | If (_, then_stmt, else_stmt_opt) ->
      let defs = stmt_defs then_stmt in
      begin match else_stmt_opt with
      | Some else_stmt -> VarSet.union defs (stmt_defs else_stmt)
      | None -> defs
      end
  | While (_, body) -> stmt_defs body
  | _ -> VarSet.empty  (* 其他语句不定义变量 *)

(*****************************************************************************)
(* 尾递归优化 (TCO)                                                         *)
(*****************************************************************************)

(* 扫描语句，检查是否包含尾递归调用 *)
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
        List.exists (contains_tco_candidate func false) init || 
        contains_tco_candidate func true last

  | While (_, body) -> 
      contains_tco_candidate func false body
  
  | _ -> false

(* 转换语句，将尾递归调用转换为赋值和continue *)
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

(* 单个函数的尾递归优化 *)
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

(* 尾递归优化入口函数 *)
let optimize_tail_recursion (prog: program) : program =
  List.map optimize_func_for_tco prog

(*****************************************************************************)
(* 常量折叠优化                                                            *)
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
      Call (fname, List.map fold_constants_expr args)
  | Paren e ->
      let e' = fold_constants_expr e in
      begin match e' with
      | Paren e'' -> e''
      | _ -> Paren e'
      end

let rec fold_constants_stmt stmt =
  match stmt with
  | Block stmts -> Block (List.map fold_constants_stmt stmts)
  | Empty -> Empty
  | ExprStmt expr -> ExprStmt (fold_constants_expr expr)
  | Assign (id, expr) -> Assign (id, fold_constants_expr expr)
  | Decl (id, expr) -> Decl (id, fold_constants_expr expr)
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
  | Return expr_opt -> Return (Option.map fold_constants_expr expr_opt)

let fold_constants program =
  List.map (fun func -> { func with body = List.map fold_constants_stmt func.body }) program

(*****************************************************************************)
(* 增强版循环优化                                                          *)
(*****************************************************************************)

(* 1. 循环不变量外提增强版 *)
let is_invariant expr loop_defs =
  let vars_used = expr_vars expr in
  VarSet.disjoint vars_used loop_defs

let is_invariant_stmt stmt loop_defs =
  match stmt with
  | Assign (id, expr) ->
      is_invariant expr loop_defs && not (VarSet.mem id loop_defs)
  | Decl (id, expr) ->
      is_invariant expr loop_defs && not (VarSet.mem id loop_defs)
  | ExprStmt expr -> is_invariant expr loop_defs
  | If (cond, then_stmt, else_stmt_opt) ->
      (* 增强：支持条件语句作为循环不变量 *)
      is_invariant cond loop_defs &&
      VarSet.disjoint (stmt_defs then_stmt) loop_defs &&
      (match else_stmt_opt with
       | Some else_stmt -> VarSet.disjoint (stmt_defs else_stmt) loop_defs
       | None -> true)
  | _ -> false

(* 2. 循环融合：合并相邻的相似循环 *)
let can_fuse_loops loop1 loop2 =
  match loop1, loop2 with
  | While (cond1, body1), While (cond2, body2) ->
      (* 条件相同且循环变量不冲突时可以融合 *)
      cond1 = cond2 && 
      VarSet.disjoint (stmt_defs body1) (stmt_defs body2)
  | _ -> false

let fuse_loops loop1 loop2 =
  match loop1, loop2 with
  | While (cond, body1), While (_, body2) ->
      let fused_body = Block [body1; body2] in
      While (cond, fused_body)
  | _ -> failwith "Cannot fuse non-while loops"

(* 3. 循环展开：适度展开小循环 *)
let unroll_loop count stmt =
  if count <= 1 then stmt
  else match stmt with
    | While (cond, body) ->
        let unrolled = ref [] in
        for _ = 1 to count - 1 do
          unrolled := body :: !unrolled
        done;
        let unrolled_body = Block (List.rev !unrolled @ [While (cond, body)]) in
        While (cond, unrolled_body)
    | _ -> stmt

(* 循环优化主函数 *)
let rec optimize_loop_stmt stmt =
  match stmt with
  | Block stmts ->
      (* 尝试循环融合 *)
      let rec fuse_adjacent_loops stmts acc =
        match stmts with
        | [] -> List.rev acc
        | [s] -> List.rev (s :: acc)
        | s1 :: s2 :: rest ->
            if can_fuse_loops s1 s2 then
              let fused = fuse_loops s1 s2 in
              fuse_adjacent_loops (fused :: rest) acc
            else
              fuse_adjacent_loops (s2 :: rest) (s1 :: acc)
      in
      let fused_stmts = fuse_adjacent_loops stmts [] in
      (* 对每个语句应用其他循环优化 *)
      let optimized_stmts = List.map optimize_loop_stmt fused_stmts in
      Block optimized_stmts
      
  | While (cond, body) ->
      (* 先优化循环体 *)
      let optimized_body = optimize_loop_stmt body in
      
      (* 计算循环体中定义的变量 *)
      let loop_defs = stmt_defs optimized_body in
      
      (* 分离循环不变量和变体 *)
      let rec separate_invariants stmts invariants variants =
        match stmts with
        | [] -> (List.rev invariants, List.rev variants)
        | stmt::rest ->
            if is_invariant_stmt stmt loop_defs then
              separate_invariants rest (stmt::invariants) variants
            else
              separate_invariants rest invariants (stmt::variants)
      in
      
      (* 处理循环体 *)
      let body' = optimize_loop_stmt optimized_body in
      let invariants, variants = 
        match body' with
        | Block stmts -> separate_invariants stmts [] []
        | _ -> separate_invariants [body'] [] []
      in
      
      (* 创建新的循环体 *)
      let new_body = if variants = [] then Empty else Block variants in
      let loop = While (cond, new_body) in
      
      (* 决定是否展开循环（小循环适度展开） *)
      let loop = 
        let body_size = List.length variants in
        if body_size > 0 && body_size <= 3 then  (* 小型循环体 *)
          unroll_loop 2 loop  (* 展开2次 *)
        else
          loop
      in
      
      (* 创建外提的不变量块和新循环 *)
      if invariants = [] then loop
      else Block (invariants @ [loop])
      
  | If (cond, then_stmt, else_stmt_opt) ->
      let then_stmt' = optimize_loop_stmt then_stmt in
      let else_stmt_opt' = Option.map optimize_loop_stmt else_stmt_opt in
      If (cond, then_stmt', else_stmt_opt')
      
  | _ -> stmt

let optimize_loops program =
  List.map (fun func ->
    { func with body = List.map optimize_loop_stmt func.body }
  ) program

(*****************************************************************************)
(* 强度削弱增强版                                                          *)
(*****************************************************************************)

let rec strength_reduction_expr expr =
  match expr with
  | BinOp (e, "*", Literal (IntLit 2)) ->
      BinOp (strength_reduction_expr e, "+", strength_reduction_expr e)
  | BinOp (Literal (IntLit 2), "*", e) ->
      BinOp (strength_reduction_expr e, "+", strength_reduction_expr e)
  | BinOp (e, "*", Literal (IntLit 4)) ->
      let e' = strength_reduction_expr e in
      BinOp (BinOp (e', "+", e'), "+", BinOp (e', "+", e'))
  | BinOp (e, "*", Literal (IntLit 8)) ->
      let e' = strength_reduction_expr e in
      let double = BinOp (e', "+", e') in
      let quadruple = BinOp (double, "+", double) in
      BinOp (quadruple, "+", quadruple)
  | BinOp (e, "*", Literal (IntLit n)) when n > 0 && (n land (n - 1)) = 0 ->
      (* 增强：处理任意2的幂次乘法 *)
      let rec power_of_two n =
        if n = 1 then 0
        else 1 + power_of_two (n / 2)
      in
      let exp = power_of_two n in
      let rec build_add e' exp =
        if exp = 1 then BinOp (e', "+", e')
        else BinOp (build_add e' (exp - 1), "+", build_add e' (exp - 1))
      in
      build_add (strength_reduction_expr e) exp
  | BinOp (e, "/", Literal (IntLit n)) when n > 0 && (n land (n - 1)) = 0 ->
      (* 增强：处理除以2的幂次，转换为右移 *)
      let rec power_of_two n =
        if n = 1 then 0
        else 1 + power_of_two (n / 2)
      in
      let exp = power_of_two n in
      let e' = strength_reduction_expr e in
      (* 假设>>运算符可用，表示右移 *)
      BinOp (e', ">>", Literal (IntLit exp))
  | BinOp (e1, op, e2) ->
      BinOp (strength_reduction_expr e1, op, strength_reduction_expr e2)
  | UnOp (op, e) ->
      UnOp (op, strength_reduction_expr e)
  | Call (fname, args) ->
      Call (fname, List.map strength_reduction_expr args)
  | Paren e ->
      Paren (strength_reduction_expr e)
  | _ -> expr

let rec strength_reduction_stmt stmt =
  match stmt with
  | Block stmts -> Block (List.map strength_reduction_stmt stmts)
  | ExprStmt expr -> ExprStmt (strength_reduction_expr expr)
  | Assign (id, expr) -> Assign (id, strength_reduction_expr expr)
  | Decl (id, expr) -> Decl (id, strength_reduction_expr expr)
  | If (cond, then_stmt, else_stmt_opt) ->
      let cond' = strength_reduction_expr cond in
      let then_stmt' = strength_reduction_stmt then_stmt in
      let else_stmt_opt' = Option.map strength_reduction_stmt else_stmt_opt in
      If (cond', then_stmt', else_stmt_opt')
  | While (cond, body) ->
      let cond' = strength_reduction_expr cond in
      let body' = strength_reduction_stmt body in
      While (cond', body')
  | _ -> stmt

let strength_reduction program =
  List.map (fun func -> { func with body = List.map strength_reduction_stmt func.body }) program

(*****************************************************************************)
(* 死代码消除                                                              *)
(*****************************************************************************)

let is_const_true expr =
  match expr with
  | Literal (IntLit n) -> n != 0
  | _ -> false

let is_const_false expr =
  match expr with
  | Literal (IntLit 0) -> true
  | _ -> false

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
  | Block stmts -> List.fold_left collect_vars_stmt vars stmts
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

let eliminate_dead_code program =
  List.map (fun func ->
    let body_reachable, _ = eliminate_dead_stmts true func.body in
    let body_deep_reachable = 
      List.map (fun s -> 
        let s', _ = eliminate_dead_stmt true s in
        Option.value s' ~default:Empty
      ) body_reachable
    in
    let used_vars = List.fold_left collect_vars_stmt VarSet.empty body_deep_reachable in
    let body_unused_removed = List.map (remove_unused_stmt used_vars) body_deep_reachable in
    let body_simplified = List.map simplify_empty_stmt body_unused_removed in
    { func with body = body_simplified }
  ) program

(*****************************************************************************)
(* 最终的优化流水线                                                         *)
(*****************************************************************************)

let optimize program =
  program 
  |> fold_constants                (* 1. 常量折叠 *)
  |> eliminate_dead_code           (* 2. 死代码消除 *)
  |> optimize_loops                (* 3. 增强版循环优化（不变量外提+融合+适度展开） *)
  |> strength_reduction            (* 4. 增强版强度削弱 *)
  |> eliminate_dead_code           (* 5. 再次死代码消除 *)
  |> optimize_tail_recursion       (* 6. 尾递归优化 *)
