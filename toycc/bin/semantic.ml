open Ast

exception SemanticError of string

let rec check_program prog =
  let functions = Hashtbl.create 16 in
  List.iter (fun f ->
    if Hashtbl.mem functions f.name then
      raise (SemanticError ("Duplicate function: " ^ f.name));
    Hashtbl.add functions f.name f
  ) prog;
  if not (Hashtbl.mem functions "main") then
    raise (SemanticError "No main() defined");
  List.iter (check_function functions) prog

and check_function funcs f =
  let env = Hashtbl.create 16 in
  List.iter (fun p -> Hashtbl.add env p `Int) f.params;
  let rec check_stmt = function
    | SBlock ss -> List.iter check_stmt ss
    | SExpr e -> ignore (check_expr e env funcs)
    | SDecl(id,e) -> ignore (check_expr e env funcs); Hashtbl.add env id `Int
    | SAssign(id,e) ->
        if not (Hashtbl.mem env id) then
          raise (SemanticError ("Unbound variable: " ^ id));
        ignore (check_expr e env funcs)
    | SIf(cond,th,Some el) ->
        ignore (check_expr cond env funcs);
        check_stmt th; check_stmt el
    | SIf(cond,th,None) -> ignore (check_expr cond env funcs); check_stmt th
    | SWhile(cond,body) -> ignore (check_expr cond env funcs); check_stmt body
    | SReturn eo -> Option.iter (fun e -> ignore (check_expr e env funcs)) eo
    | SBreak | SContinue -> ()
  and check_expr e env funcs = match e with
    | EInt _ -> `Int
    | EVar id -> if Hashtbl.mem env id then `Int else raise (SemanticError ("Unbound var: " ^ id))
    | EBin(_,l,r) -> ignore (check_expr l env funcs); ignore (check_expr r env funcs); `Int
    | EUn(_,e)    -> ignore (check_expr e env funcs); `Int
    | ECall(f,args) ->
        if not (Hashtbl.mem funcs f) then
          raise (SemanticError ("Unknown function: " ^ f));
        let fd = Hashtbl.find funcs f in
        if List.length args <> List.length fd.params then
          raise (SemanticError ("Arg count mismatch: " ^ f));
        List.iter (fun arg -> ignore (check_expr arg env funcs)) args;
        fd.ret_type
  in check_stmt f.body