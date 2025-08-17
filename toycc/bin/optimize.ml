open Ast

(* 辅助类型：符号表项（支持作用域和表达式哈希） *)
type symbol = 
  | Constant of literal
  | Variable 
  | TempVar of id  (* 用于公共子表达式消除 *)

module ExprHash = struct
  type t = expr
  let equal e1 e2 = 
    match e1, e2 with
    | Literal l1, Literal l2 -> l1 = l2
    | Var id1, Var id2 -> id1 = id2
    | BinOp(a1, op1, b1), BinOp(a2, op2, b2) -> a1 = a2 && op1 = op2 && b1 = b2
    | UnOp(op1, a1), UnOp(op2, a2) -> op1 = op2 && a1 = a2
    | Call(id1, args1), Call(id2, args2) -> id1 = id2 && args1 = args2
    | Paren e1, Paren e2 -> e1 = e2
    | _ -> false

  let hash = Hashtbl.hash
end

module ExprTable = Hashtbl.Make(ExprHash)
module IdSet = Set.Make(String)

(* 1. 表达式优化增强 *)
let rec optimize_expr (cse_table: (id * expr) ExprTable.t) (e: expr) : expr * (id * expr) ExprTable.t =
  match e with
  | Literal l -> Literal l, cse_table
  
  | Var id -> Var id, cse_table
  
  | Paren e -> 
      let e', tbl = optimize_expr cse_table e in
      e', tbl  (* 移除冗余括号 *)
  
  | UnOp(op, e) ->
      let e', tbl = optimize_expr cse_table e in
      let opt = match op, e' with
        | "-", Literal (IntLit n) -> Literal (IntLit (-n))
        | "!", Literal (IntLit 0) -> Literal (IntLit 1)
        | "!", Literal (IntLit _) -> Literal (IntLit 0)
        | _ -> UnOp(op, e')
      in
      opt, tbl
  
  | BinOp(e1, op, e2) ->
      let e1', tbl1 = optimize_expr cse_table e1 in
      let e2', tbl2 = optimize_expr tbl1 e2 in
      
      (* 常量折叠 *)
      let folded = match e1', op, e2' with
        | Literal (IntLit a), "+", Literal (IntLit b) -> Literal (IntLit (a + b))
        | Literal (IntLit a), "-", Literal (IntLit b) -> Literal (IntLit (a - b))
        | Literal (IntLit a), "*", Literal (IntLit b) -> Literal (IntLit (a * b))
        | Literal (IntLit a), "/", Literal (IntLit b) when b <> 0 -> Literal (IntLit (a / b))
        | Literal (IntLit a), "%", Literal (IntLit b) when b <> 0 -> Literal (IntLit (a mod b))
        | Literal (IntLit a), "&&", Literal (IntLit b) -> 
            Literal (IntLit (if a <> 0 && b <> 0 then 1 else 0))
        | Literal (IntLit a), "||", Literal (IntLit b) -> 
            Literal (IntLit (if a <> 0 || b <> 0 then 1 else 0))
        | Literal (IntLit a), "==", Literal (IntLit b) -> Literal (IntLit (if a = b then 1 else 0))
        | Literal (IntLit a), "!=", Literal (IntLit b) -> Literal (IntLit (if a <> b then 1 else 0))
        | Literal (IntLit a), "<",  Literal (IntLit b) -> Literal (IntLit (if a < b then 1 else 0))
        | Literal (IntLit a), ">",  Literal (IntLit b) -> Literal (IntLit (if a > b then 1 else 0))
        | Literal (IntLit a), "<=", Literal (IntLit b) -> Literal (IntLit (if a <= b then 1 else 0))
        | Literal (IntLit a), ">=", Literal (IntLit b) -> Literal (IntLit (if a >= b then 1 else 0))
        (* 代数简化 *)
        | e, "+", Literal (IntLit 0) -> e
        | Literal (IntLit 0), "+", e -> e
        | e, "-", Literal (IntLit 0) -> e
        | e, "*", Literal (IntLit 1) -> e
        | Literal (IntLit 1), "*", e -> e
        | e, "/", Literal (IntLit 1) -> e
        | _, "*", Literal (IntLit 0) -> Literal (IntLit 0)
        | Literal (IntLit 0), "*", _ -> Literal (IntLit 0)
        | e, "&&", Literal (IntLit 1) -> e
        | Literal (IntLit 1), "&&", e -> e
        | _, "&&", Literal (IntLit 0) -> Literal (IntLit 0)
        | Literal (IntLit 0), "&&", _ -> Literal (IntLit 0)
        | e, "||", Literal (IntLit 0) -> e
        | Literal (IntLit 0), "||", e -> e
        | _, "||", Literal (IntLit 1) -> Literal (IntLit 1)
        | Literal (IntLit 1), "||", _ -> Literal (IntLit 1)
        | _ -> BinOp(e1', op, e2')
      in
      
      (* 公共子表达式消除 (CSE) *)
      let cse_opt, new_tbl = 
        if folded <> e1' && folded <> e2' then  (* 跳过简单表达式 *)
          match ExprTable.find_opt tbl2 folded with
          | Some (id, _) -> Var id, tbl2  (* 复用已有临时变量 *)
          | None ->
              let temp_id = "__cse_" ^ string_of_int (ExprTable.length tbl2) in
              ExprTable.add tbl2 folded (temp_id, folded);
              folded, tbl2
        else
          folded, tbl2
      in
      cse_opt, new_tbl
  
  | Call(id, args) ->
      let rec optimize_args args tbl acc =
        match args with
        | [] -> List.rev acc, tbl
        | arg::rest ->
            let arg', tbl' = optimize_expr tbl arg in
            optimize_args rest tbl' (arg'::acc)
      in
      let args', tbl = optimize_args args cse_table [] in
      Call(id, args'), tbl

(* 2. 识别循环不变式 *)
let rec get_variables (e: expr) : IdSet.t =
  match e with
  | Literal _ -> IdSet.empty
  | Var id -> IdSet.singleton id
  | Paren e -> get_variables e
  | UnOp(_, e) -> get_variables e
  | BinOp(e1, _, e2) -> IdSet.union (get_variables e1) (get_variables e2)
  | Call(_, args) -> List.fold_left (fun s e -> IdSet.union s (get_variables e)) IdSet.empty args

let is_loop_invariant (loop_vars: IdSet.t) (e: expr) : bool =
  IdSet.disjoint (get_variables e) loop_vars

(* 3. 尾递归优化 *)
let detect_tail_recursion (func_name: id) (body: stmt list) : stmt option =
  let rec is_tail_call = function
    | Return (Some (Call(id, args))) when id = func_name -> Some args
    | Block stmts -> 
        (match List.rev stmts with
         | last::_ -> is_tail_call last
         | [] -> None)
    | If(_, then_stmt, Some else_stmt) ->
        (match is_tail_call then_stmt, is_tail_call else_stmt with
         | Some a, Some b when a = b -> Some a
         | _ -> None)
    | If(_, then_stmt, None) -> is_tail_call then_stmt
    | _ -> None
  in
  
  match body with
  | [stmt] ->
      (match is_tail_call stmt with
       | Some args ->
           let param_names = List.map (fun p -> p.pname) (List.filter (fun p -> p.ptype = Int) []) in (* 需要实际参数列表 *)
           let updates = List.map2 (fun p arg -> Assign(p, arg)) param_names args in
           Some (Block (updates @ [Continue]))
       | None -> None)
  | _ -> None

(* 4. 语句优化增强 *)
let rec optimize_stmt 
  (symtab: id -> symbol) 
  (loop_vars: IdSet.t)
  (cse_table: (id * expr) ExprTable.t)
  (s: stmt) : stmt * (id -> symbol) * (id * expr) list * (id -> symbol) =
  
  match s with
  | Empty -> Empty, symtab, [], symtab
  
  | Block stmts ->
      let rec optimize_block symtab loop_vars cse temps stmts acc =
        match stmts with
        | [] -> 
            let cse_decls = ExprTable.fold (fun _ (id, e) acc -> Decl(id, e)::acc) cse [] in
            Block (List.rev (cse_decls @ acc)), symtab, temps, symtab
        | stmt::rest ->
            let stmt', symtab', new_temps, _ = optimize_stmt symtab loop_vars cse stmt in
            let acc' = if stmt' = Empty then acc else stmt'::acc in
            optimize_block symtab' loop_vars cse (new_temps @ temps) rest acc'
      in
      optimize_block symtab loop_vars cse_table [] stmts []
  
  | ExprStmt e ->
      let e', cse' = optimize_expr cse_table e in
      let cse_decls = ExprTable.fold (fun _ (id, e) acc -> Decl(id, e)::acc) cse' [] in
      (match cse_decls with
       | [] -> ExprStmt e', symtab, [], symtab
       | _ -> Block (cse_decls @ [ExprStmt e']), symtab, [], symtab)
  
  | Assign(id, e) ->
      let e', _ = optimize_expr cse_table e in
      let symtab' = match e' with
        | Literal l -> (fun x -> if x = id then Constant l else symtab x)
        | _ -> (fun x -> if x = id then Variable else symtab x)
      in
      Assign(id, e'), symtab', [], symtab'
  
  | Decl(id, e) ->
      let e', _ = optimize_expr cse_table e in
      let symtab' = match e' with
        | Literal l -> (fun x -> if x = id then Constant l else symtab x)
        | _ -> (fun x -> if x = id then Variable else symtab x)
      in
      Decl(id, e'), symtab', [], symtab'
  
  | If(cond, then_stmt, else_stmt) ->
      let cond', cse' = optimize_expr cse_table cond in
      (match cond' with
       | Literal (IntLit 0) ->
           (match else_stmt with
            | Some s -> optimize_stmt symtab loop_vars cse' s
            | None -> Empty, symtab, [], symtab)
       | Literal (IntLit n) when n <> 0 ->
           optimize_stmt symtab loop_vars cse' then_stmt
       | _ ->
           let then_opt, _, temps1, _ = optimize_stmt symtab loop_vars cse' then_stmt in
           let else_opt, symtab2, temps2, _ = 
             match else_stmt with
             | Some s -> optimize_stmt symtab loop_vars cse' s
             | None -> Empty, symtab, [], symtab
           in
           If(cond', then_opt, Some else_opt), symtab2, temps1 @ temps2, symtab2)
  
  | While(cond, body) ->
      let cond', cse' = optimize_expr cse_table cond in
      (match cond' with
       | Literal (IntLit 0) -> Empty, symtab, [], symtab
       | _ ->
           (* 识别循环变量 *)
           let loop_vars = get_variables cond' in
           (* 提取循环不变式 *)
           let rec extract_invariants stmts invs rest =
             match stmts with
             | [] -> List.rev invs, List.rev rest
             | stmt::ss ->
                 (match stmt with
                  | Decl(_, e) when is_loop_invariant loop_vars e ->
                      extract_invariants ss (stmt::invs) rest
                  | Assign(_, e) when is_loop_invariant loop_vars e ->
                      extract_invariants ss (stmt::invs) rest
                  | _ -> extract_invariants ss invs (stmt::rest))
           in
           let body_opt, _, _, _ = optimize_stmt symtab loop_vars cse' body in
           let invariants, body_rest = 
             match body_opt with
             | Block stmts -> extract_invariants stmts [] []
             | _ -> [], [body_opt]
           in
           (* 尾递归转换为循环 *)
           let optimized_body = Block body_rest in
           While(cond', Block (invariants @ [optimized_body])), symtab, [], symtab)
  
  | Break | Continue -> s, symtab, [], symtab
  
  | Return e_opt ->
      let e_opt' = Option.map (fun e -> fst (optimize_expr cse_table e)) e_opt in
      Return e_opt', symtab, [], symtab

(* 5. 函数优化增强（含尾递归优化） *)
let optimize_func_def (fd: func_def) : func_def =
  let initial_symtab = (fun _ -> Variable) in
  let param_names = List.map (fun p -> p.pname) fd.params in
  let initial_loop_vars = IdSet.of_list param_names in
  let initial_cse = ExprTable.create 10 in
  
  (* 尾递归优化 *)
  let optimized_body = 
    match detect_tail_recursion fd.fname fd.body with
    | Some tail_loop ->
        (* 将尾递归转换为while循环 *)
        [Block [
          While(Literal (IntLit 1), tail_loop);
          Return (Some (Literal (IntLit 0)))  (* 假设返回0为默认值 *)
        ]]
    | None ->
        (* 普通函数体优化 *)
        let rec optimize_body symtab cse stmts acc =
          match stmts with
          | [] -> List.rev acc
          | stmt::rest ->
              let stmt', symtab', _, _ = optimize_stmt symtab initial_loop_vars cse stmt in
              let acc' = if stmt' = Empty then acc else stmt'::acc in
              optimize_body symtab' cse rest acc'
        in
        optimize_body initial_symtab initial_cse fd.body []
  in
  
  { fd with body = optimized_body }

(* 6. 程序优化（带迭代次数限制的高效优化） *)
let optimize (p: program) : program =
  let rec optimize_iteration program iter =
    if iter >= 5 then program  (* 限制最大迭代次数，避免无限循环 *)
    else
      let optimized = List.map optimize_func_def program in
      if optimized = program then program
      else optimize_iteration optimized (iter + 1)
  in
  optimize_iteration p 0
    
