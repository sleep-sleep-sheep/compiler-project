(* AST优化器 *)
open Ast  (* 假设你的AST定义在Ast模块中 *)

(* 辅助函数：判断表达式是否为常量并返回其值 *)
let is_constant_expr = function
  | Literal lit -> Some lit
  | _ -> None

(* 优化表达式：常量折叠、表达式简化等 *)
let rec optimize_expr e =
  match e with
  | Literal l -> Literal l  (* 常量本身无需优化 *)
  
  | Var id -> Var id  (* 变量引用，常量传播在语句处理中进行 *)
  
  | Paren e -> optimize_expr e  (* 移除多余括号 *)
  
  | UnOp (op, e) ->
      let e' = optimize_expr e in
      (* 一元运算常量折叠 *)
      (match op, e' with
       | "-", Literal (IntLit n) -> Literal (IntLit (-n))
       | "!", Literal (IntLit 0) -> Literal (IntLit 1)  (* !false → true (1) *)
       | "!", Literal (IntLit _) -> Literal (IntLit 0)  (* !true → false (0) *)
       | _ -> UnOp (op, e'))  (* 无法优化的情况 *)
  
  | BinOp (e1, op, e2) ->
      let e1' = optimize_expr e1 in
      let e2' = optimize_expr e2 in
      (* 二元运算常量折叠 *)
      (match e1', op, e2' with
       (* 算术运算 *)
       | Literal (IntLit a), "+", Literal (IntLit b) -> Literal (IntLit (a + b))
       | Literal (IntLit a), "-", Literal (IntLit b) -> Literal (IntLit (a - b))
       | Literal (IntLit a), "*", Literal (IntLit b) -> Literal (IntLit (a * b))
       | Literal (IntLit a), "/", Literal (IntLit b) when b <> 0 -> Literal (IntLit (a / b))
       | Literal (IntLit a), "%", Literal (IntLit b) when b <> 0 -> Literal (IntLit (a mod b))
       
       (* 逻辑运算 (假设0为false，非0为true) *)
       | Literal (IntLit a), "&&", Literal (IntLit b) ->
           Literal (IntLit (if a <> 0 && b <> 0 then 1 else 0))
       | Literal (IntLit a), "||", Literal (IntLit b) ->
           Literal (IntLit (if a <> 0 || b <> 0 then 1 else 0))
       
       (* 比较运算 *)
       | Literal (IntLit a), "==", Literal (IntLit b) -> Literal (IntLit (if a = b then 1 else 0))
       | Literal (IntLit a), "!=", Literal (IntLit b) -> Literal (IntLit (if a <> b then 1 else 0))
       | Literal (IntLit a), "<",  Literal (IntLit b) -> Literal (IntLit (if a < b then 1 else 0))
       | Literal (IntLit a), ">",  Literal (IntLit b) -> Literal (IntLit (if a > b then 1 else 0))
       | Literal (IntLit a), "<=", Literal (IntLit b) -> Literal (IntLit (if a <= b then 1 else 0))
       | Literal (IntLit a), ">=", Literal (IntLit b) -> Literal (IntLit (if a >= b then 1 else 0))
       
       (* 表达式简化规则 *)
       | e, "+", Literal (IntLit 0) -> e  (* x + 0 → x *)
       | Literal (IntLit 0), "+", e -> e  (* 0 + x → x *)
       | e, "-", Literal (IntLit 0) -> e  (* x - 0 → x *)
       | e, "*", Literal (IntLit 1) -> e  (* x * 1 → x *)
       | Literal (IntLit 1), "*", e -> e  (* 1 * x → x *)
       | e, "/", Literal (IntLit 1) -> e  (* x / 1 → x *)
       | _, "*", Literal (IntLit 0) -> Literal (IntLit 0)  (* x * 0 → 0 *)
       | Literal (IntLit 0), "*", _ -> Literal (IntLit 0)  (* 0 * x → 0 *)
       | e, "&&", Literal (IntLit 1) -> e  (* x && true → x *)
       | Literal (IntLit 1), "&&", e -> e  (* true && x → x *)
       | _, "&&", Literal (IntLit 0) -> Literal (IntLit 0)  (* x && false → false *)
       | Literal (IntLit 0), "&&", _ -> Literal (IntLit 0)  (* false && x → false *)
       | e, "||", Literal (IntLit 0) -> e  (* x || false → x *)
       | Literal (IntLit 0), "||", e -> e  (* false || x → x *)
       | _, "||", Literal (IntLit 1) -> Literal (IntLit 1)  (* x || true → true *)
       | Literal (IntLit 1), "||", _ -> Literal (IntLit 1)  (* true || x → true *)
       
       (* 无法进一步优化的情况 *)
       | _ -> BinOp (e1', op, e2'))
  
  | Call (id, args) ->
      let args' = List.map optimize_expr args in  (* 优化函数调用参数 *)
      Call (id, args')

(* 优化语句：使用符号表跟踪常量传播，返回优化后的语句和更新的符号表 *)
let rec optimize_stmt (symtab : id -> literal option) (s : stmt) : stmt * (id -> literal option) =
  match s with
  | Empty -> Empty, symtab  (* 空语句保持不变 *)
  
  | Block stmts ->
      (* 优化块内语句并移除空语句 *)
      let rec optimize_block symtab stmts acc =
        match stmts with
        | [] -> List.rev acc, symtab
        | stmt :: rest ->
            let stmt', symtab' = optimize_stmt symtab stmt in
            let acc' = if stmt' = Empty then acc else stmt' :: acc in
            optimize_block symtab' rest acc'
      in
      let optimized_stmts, symtab' = optimize_block symtab stmts [] in
      
      (* 合并嵌套块 *)
      let merged_stmts = 
        match optimized_stmts with
        | [Block b] -> b  (* 单块嵌套 → 展开 *)
        | _ -> optimized_stmts
      in
      Block merged_stmts, symtab'
  
  | ExprStmt e ->
      let e' = optimize_expr e in
      ExprStmt e', symtab  (* 优化表达式语句中的表达式 *)
  
  | Assign (id, e) ->
      let e' = optimize_expr e in
      (* 更新符号表：如果赋值为常量，则记录；否则清除记录 *)
      let symtab' = 
        match e' with
        | Literal l -> (fun x -> if x = id then Some l else symtab x)
        | _ -> (fun x -> if x = id then None else symtab x)
      in
      Assign (id, e'), symtab'
  
  | Decl (id, e) ->
      let e' = optimize_expr e in
      (* 变量声明时的常量传播 *)
      let symtab' = 
        match e' with
        | Literal l -> (fun x -> if x = id then Some l else symtab x)
        | _ -> (fun x -> if x = id then None else symtab x)
      in
      Decl (id, e'), symtab'
  
  | If (cond, then_stmt, else_stmt) ->
      let cond' = optimize_expr cond in
      (* 条件为常量时的死代码消除 *)
      (match cond' with
       | Literal (IntLit 0) ->  (* 条件恒为false → 只保留else分支 *)
           (match else_stmt with
            | Some s -> optimize_stmt symtab s
            | None -> Empty, symtab)
       
       | Literal (IntLit n) when n <> 0 ->  (* 条件恒为true → 只保留then分支 *)
           let then_opt, symtab' = optimize_stmt symtab then_stmt in
           then_opt, symtab'
       
       | _ ->  (* 条件为变量 → 优化两个分支 *)
           let then_opt, _ = optimize_stmt symtab then_stmt in
           let else_opt, symtab2 = 
             match else_stmt with
             | Some s -> let s', st = optimize_stmt symtab s in Some s', st
             | None -> None, symtab
           in
           If (cond', then_opt, else_opt), symtab2)  (* 简化处理：使用最后一个符号表 *)
  
  | While (cond, body) ->
      let cond' = optimize_expr cond in
      (match cond' with
       | Literal (IntLit 0) ->  (* 条件恒为false → 整个循环为死代码 *)
           Empty, symtab
       | _ ->  (* 优化循环体 *)
           let body_opt, _ = optimize_stmt symtab body in  (* 循环内符号表变化不传播到外部 *)
           While (cond', body_opt), symtab)
  
  | Break | Continue -> s, symtab  (* 跳转语句保持不变 *)
  
  | Return e_opt ->
      let e_opt' = Option.map optimize_expr e_opt in  (* 优化返回值表达式 *)
      Return e_opt', symtab

(* 优化函数定义：主要优化函数体 *)
let optimize_func_def (fd : func_def) : func_def =
  (* 初始化符号表：函数参数不是常量 *)
  let initial_symtab = (fun _ -> None) in
  
  (* 优化函数体语句列表 *)
  let rec optimize_body symtab stmts acc =
    match stmts with
    | [] -> List.rev acc, symtab
    | stmt :: rest ->
        let stmt', symtab' = optimize_stmt symtab stmt in
        let acc' = if stmt' = Empty then acc else stmt' :: acc in
        optimize_body symtab' rest acc'
  in
  
  let optimized_body, _ = optimize_body initial_symtab fd.body [] in
  { fd with body = optimized_body }

(* 优化程序：对所有函数定义进行优化 *)
let rec optimize (p : program) : program =
  let optimized = List.map optimize_func_def p in
  (* 多轮优化直到不再变化（处理优化后新出现的可优化项） *)
  if optimized = p then p else optimize optimized
