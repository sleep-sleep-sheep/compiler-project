(*二次修改*)
(* 语义分析器 *)

open Ast

(* 符号表 *)
module SymbolTable = struct
  type t = (string, typ * int) Hashtbl.t

  let create () = Hashtbl.create 10

  let add st name typ offset =
    if Hashtbl.mem st name then
      failwith ("Redeclaration of variable: " ^ name)
    else
      Hashtbl.add st name (typ, offset)

  let find st name =
    try Hashtbl.find st name
    with Not_found -> failwith ("Undeclared variable: " ^ name)

  let mem st name = Hashtbl.mem st name
end

(* 作用域栈 *)
module ScopeStack = struct
  type t = SymbolTable.t list

  let create () = []

  let push_scope st = 
    let new_scope = SymbolTable.create () in
    new_scope :: st

  let pop_scope = function
    | [] -> failwith "Empty scope stack"
    | _ :: rest -> rest

  let add st name typ offset =
    match st with
    | [] -> failwith "Empty scope stack"
    | top :: _ ->
        SymbolTable.add top name typ offset;
        st

  let find st name =
    let rec aux = function
      | [] -> failwith ("Undeclared variable: " ^ name)
      | top :: rest ->
          if SymbolTable.mem top name then
            SymbolTable.find top name
          else
            aux rest
    in
    aux st

  let mem st name =
    let rec aux = function
      | [] -> false
      | top :: rest ->
          if SymbolTable.mem top name then true else aux rest
    in
    aux st
end

(* 函数表 *)
module FunctionTable = struct
  type func_info = { return_type: typ; params: typ list }
  
  type t = (string, func_info) Hashtbl.t
  
  let create () = Hashtbl.create 10
  
  let add st name return_type params =
    if Hashtbl.mem st name then
      failwith ("Redeclaration of function: " ^ name)
    else
      Hashtbl.add st name { return_type; params }

  let find st name =
    try Hashtbl.find st name
    with Not_found -> failwith ("Undeclared function: " ^ name)
    
  let mem st name = Hashtbl.mem st name
end

(* 语义分析上下文 *)
type context = {
  functions: FunctionTable.t;
  mutable scopes: ScopeStack.t;
  current_function: string option;
  loop_depth: int;
  current_offset: int ref;
}

(* 创建新的上下文 *)
let create_context () = {
  functions = FunctionTable.create ();
  scopes = ScopeStack.create ();
  current_function = None;
  loop_depth = 0;
  current_offset = ref 0;
}

(* 类型检查表达式 *)
let rec check_expr ctx expr =
  match expr with
  | Literal (IntLit _) -> Int
  | Var id -> 
      let typ, _ = ScopeStack.find ctx.scopes id in
      typ
  | BinOp (e1, _, e2) ->
      let t1 = check_expr ctx e1 in
      let t2 = check_expr ctx e2 in
      if t1 <> Int || t2 <> Int then
        failwith "Type error in binary operation";
      Int (* 所有二元运算都返回int类型 *)
  | UnOp (_, e) ->
      let t = check_expr ctx e in
      if t <> Int then
        failwith "Type error in unary operation";
      Int
  | Call (fname, args) ->
      let func = FunctionTable.find ctx.functions fname in
      let arg_types = List.map (check_expr ctx) args in
      if List.length arg_types <> List.length func.params then
        failwith ("Wrong number of arguments in call to " ^ fname);
      List.iter2 (fun arg_typ param_typ ->
        if arg_typ <> param_typ then
          failwith ("Type mismatch in argument to " ^ fname)
      ) arg_types func.params;
      func.return_type
  | Paren e -> check_expr ctx e

(* 检查表达式是否为void类型的函数调用 *)
let is_void_call ctx expr =
  match expr with
  | Call (fname, _) ->
      let func = FunctionTable.find ctx.functions fname in
      func.return_type = Void
  | _ -> false

(* 类型检查语句 *)
let rec check_stmt ctx stmt =
  match stmt with
  | Block stmts ->
      (* 创建新的作用域 *)
      let new_scopes = ScopeStack.push_scope ctx.scopes in
      let new_ctx = { ctx with scopes = new_scopes } in
      List.iter (check_stmt new_ctx) stmts;
      (* 作用域在这里自动结束 *)
      ctx.scopes <- ScopeStack.pop_scope new_scopes
  | Empty -> ()
  | ExprStmt expr ->
      ignore (check_expr ctx expr)
  | Assign (id, expr) ->
      (* 检查赋值右值不能是void函数调用 *)
      if is_void_call ctx expr then
        failwith "Cannot assign void function result to variable";
      let typ, _ = ScopeStack.find ctx.scopes id in
      let expr_typ = check_expr ctx expr in
      if typ <> expr_typ then
        failwith ("Type mismatch in assignment to " ^ id)
  | Decl (id, expr) ->
      (* 检查初始化表达式不能是void函数调用 *)
      if is_void_call ctx expr then
        failwith "Cannot initialize variable with void function result";
      let expr_typ = check_expr ctx expr in
      if expr_typ <> Int then
        failwith ("Type mismatch in declaration of " ^ id);
      ctx.scopes <- ScopeStack.add ctx.scopes id Int !(ctx.current_offset);
      ctx.current_offset := !(ctx.current_offset) - 4
  | If (cond, then_stmt, else_stmt_opt) ->
      (* 检查条件不能是void函数调用 *)
      if is_void_call ctx cond then
        failwith "Cannot use void function result as condition";
      let cond_typ = check_expr ctx cond in
      if cond_typ <> Int then
        failwith "Condition in if statement must be an integer";
      check_stmt ctx then_stmt;
      Option.iter (check_stmt ctx) else_stmt_opt
  | While (cond, body) ->
      (* 检查条件不能是void函数调用 *)
      if is_void_call ctx cond then
        failwith "Cannot use void function result as condition";
      let cond_typ = check_expr ctx cond in
      if cond_typ <> Int then
        failwith "Condition in while statement must be an integer";
      let new_ctx = { ctx with loop_depth = ctx.loop_depth + 1 } in
      check_stmt new_ctx body
  | Break ->
      if ctx.loop_depth = 0 then
        failwith "Break statement not in loop"
  | Continue ->
      if ctx.loop_depth = 0 then
        failwith "Continue statement not in loop"
  | Return expr_opt ->
      begin match ctx.current_function with
      | None -> failwith "Return statement outside function"
      | Some fname ->
          let func = FunctionTable.find ctx.functions fname in
          begin match expr_opt with
          | None ->
              if func.return_type <> Void then
                failwith "Return without value in non-void function"
          | Some expr ->
              let expr_typ = check_expr ctx expr in
              if expr_typ <> func.return_type then
                failwith "Return type mismatch"
          end
      end

(* 检查函数是否在所有执行路径上都有返回值 *)
let rec check_return_paths return_type stmt =
  match stmt with
  | Block stmts ->
      List.exists (check_return_paths return_type) stmts
  | Empty -> false
  | ExprStmt _ -> false
  | Assign _ -> false
  | Decl _ -> false
  | If (_, then_stmt, Some else_stmt) ->
      (* if-else: 两个分支都必须有返回值 *)
      (check_return_paths return_type then_stmt) && 
      (check_return_paths return_type else_stmt)
  | If (_, _, None) ->
      (* 只有if没有else，不能保证所有路径都返回 *)
      false
  | While (_, _) ->
      (* while循环不能保证执行，所以不算有返回值 *)
      false
  | Break -> false
  | Continue -> false
  | Return None ->
      return_type = Void
  | Return (Some _) ->
      return_type = Int

(* 语义分析程序 *)
let check_program program =
  let ctx = create_context () in
  
  (* 第一遍：收集函数定义 *)
  List.iter (fun func ->
    let param_types = List.map (fun param -> param.ptype) func.params in
    FunctionTable.add ctx.functions func.fname func.ftype param_types
  ) program;
  
  (* 检查是否有main函数且符合要求 *)
  let main_func = FunctionTable.find ctx.functions "main" in
  if main_func.return_type <> Int then
    failwith "Main function must return int";
  if List.length main_func.params <> 0 then
    failwith "Main function must have no parameters";
  
  (* 第二遍：检查函数体 *)
  List.iter (fun func ->
    (* 创建一个新的局部作用域 *)
    let local_scope = SymbolTable.create () in
    let param_offset = ref 16 in  (* 参数从x8(sp+16)开始 *)
    
    (* 将函数参数添加到局部作用域 *)
    List.iter (fun param ->
      let offset = !param_offset in
      SymbolTable.add local_scope param.pname param.ptype offset;
      param_offset := !param_offset + 4
    ) func.params;
    
    (* 将局部作用域压入作用域栈 *)
    let new_scopes = local_scope :: ctx.scopes in
    
    let new_ctx = {
      ctx with 
      scopes = new_scopes;
      current_function = Some func.fname;
      current_offset = ref (-4)  (* 局部变量从sp-4开始 *)
    } in
    
    (* 检查函数体 *)
    List.iter (check_stmt new_ctx) func.body;
    
    (* 检查返回路径 *)
    if func.ftype = Int then
      let has_return = List.exists (check_return_paths func.ftype) func.body in
      if not has_return then
        failwith ("Function " ^ func.fname ^ " must return a value on all paths")
  ) program;
  
  program
