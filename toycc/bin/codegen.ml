open Ast

let rv_op = function
  | Add -> "add" | Sub -> "sub" | Mul -> "mul" | Div -> "div" | Mod -> "rem"
  | Lt  -> "slt" | Gt  -> "sgt" | Le  -> "sle" | Ge  -> "sge"
  | Eq  -> "seq" | Ne  -> "sne"
  | Land -> "and" | Lor -> "or"

let fresh_label =
  let c = ref 0 in fun () -> incr c; Printf.sprintf ".L%d" !c

let rec emit_program prog =
  let buf = Buffer.create 1024 in
  List.iter (fun f -> emit_function buf f) prog;
  Buffer.contents buf

and emit_function buf f =
  Buffer.add_string buf (Printf.sprintf ".globl %s
%s:
" f.name f.name);
  Buffer.add_string buf
    "  addi sp, sp, -16
  sw ra, 12(sp)
  sw fp, 8(sp)
  mv fp, sp
";
  emit_stmt buf f.body;
  Buffer.add_string buf
    "  mv sp, fp
  lw fp, 8(sp)
  lw ra, 12(sp)
  ret

"

and emit_stmt buf = function
  | SBlock ss -> List.iter (emit_stmt buf) ss
  | SExpr e -> emit_expr buf e; Buffer.add_string buf "  addi sp, sp, 4  # pop\n"
  | SDecl (_, e) -> emit_expr buf e; Buffer.add_string buf "  # decl reserve\n"
  | SAssign (_, e) -> emit_expr buf e; Buffer.add_string buf "  # assign var\n"
  | SReturn (Some e) ->  (* 处理 return 带表达式的情况（int 函数） *)
      emit_expr buf e; 
      Buffer.add_string buf "  mv a0, t0  # 将返回值存入 a0\n"  (* a0 是返回值寄存器 *)
  | SReturn None ->  (* 补充处理 return 不带表达式的情况（void 函数） *)
      Buffer.add_string buf "  # void return\n"  (* 无需移动返回值，直接返回 *)
  | SIf (cond, t, Some el) ->
      let l1 = fresh_label () and l2 = fresh_label () in
      emit_expr buf cond;
      Buffer.add_string buf (Printf.sprintf "  beqz t0, %s\n" l1);
      emit_stmt buf t;
      Buffer.add_string buf (Printf.sprintf "  j %s\n%s:\n" l2 l1);
      emit_stmt buf el;
      Buffer.add_string buf (Printf.sprintf "%s:\n" l2)
  | SIf (cond, t, None) ->
      let l = fresh_label () in
      emit_expr buf cond;
      Buffer.add_string buf (Printf.sprintf "  beqz t0, %s\n" l);
      emit_stmt buf t;
      Buffer.add_string buf (Printf.sprintf "%s:\n" l)
  | SWhile (cond, body) ->
      let l1 = fresh_label () and l2 = fresh_label () in
      Buffer.add_string buf (Printf.sprintf "%s:\n" l1);
      emit_expr buf cond;
      Buffer.add_string buf (Printf.sprintf "  beqz t0, %s\n" l2);
      emit_stmt buf body;
      Buffer.add_string buf (Printf.sprintf "  j %s\n%s:\n" l1 l2)
  | SBreak | SContinue -> ()
and emit_expr buf = function
  | EInt n -> Buffer.add_string buf (Printf.sprintf "  li t0, %d
" n)
  | EVar id -> Buffer.add_string buf (Printf.sprintf "  la t0, %s
" id)
  | EBin (op, e1, e2) ->
      emit_expr buf e1;
      Buffer.add_string buf "  push t0
";
      emit_expr buf e2;
      Buffer.add_string buf "  pop t1
";
      Buffer.add_string buf (Printf.sprintf "  %s t0, t1, t0
" (rv_op op))
  | EUn (`Neg, e) -> emit_expr buf e; Buffer.add_string buf "  neg t0, t0
"
  | EUn (`Not, e) -> emit_expr buf e; Buffer.add_string buf "  seqz t0, t0
"
  | ECall (f, args) ->
      List.iter (fun arg -> emit_expr buf arg; Buffer.add_string buf "  push t0
") args;
      List.iter (fun _ -> Buffer.add_string buf "  pop a0
") args;
      Buffer.add_string buf (Printf.sprintf "  call %s
" f);
      Buffer.add_string buf "  mv t0, a0
"