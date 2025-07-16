open Ast

(* RISC-V 操作符映射 *)
let rv_op = function
  | Add -> "add" | Sub -> "sub" | Mul -> "mul" | Div -> "div" | Mod -> "rem"
  | Lt  -> "slt" | Gt  -> "sgt" | Le  -> "sle" | Ge  -> "sge"
  | Eq  -> "seqz" | Ne  -> "snez"
  | Land -> "and" | Lor -> "or"

(* 生成新的标签 *)
let fresh_label =
  let c = ref 0 in fun () -> incr c; Printf.sprintf ".L%d" !c

(* 生成栈操作的辅助函数 *)
let emit_stack_push buf reg =
  Buffer.add_string buf (Printf.sprintf "  addi sp, sp, -4\n  sw %s, 0(sp)\n" reg)

let emit_stack_pop buf reg =
  Buffer.add_string buf (Printf.sprintf "  lw %s, 0(sp)\n  addi sp, sp, 4\n" reg)

(* 程序生成入口 *)
let rec emit_program prog =
  let buf = Buffer.create 1024 in
  (* 添加全局数据段 *)
  Buffer.add_string buf ".data\n";
  List.iter (fun f -> ()) prog;  (* 移除了对 emit_function_data 的调用 *)
  Buffer.add_string buf "\n.text\n.align 2\n";
  List.iter (fun f -> emit_function buf f) prog;
  Buffer.contents buf

and emit_function buf f =
  Buffer.add_string buf (Printf.sprintf ".globl %s\n%s:\n" f.name f.name);
  (* 保存调用者保存的寄存器 *)
  Buffer.add_string buf "  addi sp, sp, -32\n";
  Buffer.add_string buf "  sw ra, 28(sp)\n";
  Buffer.add_string buf "  sw s0, 24(sp)\n";  (* s0 用作 fp *)
  Buffer.add_string buf "  sw s1, 20(sp)\n";
  Buffer.add_string buf "  sw s2, 16(sp)\n";
  Buffer.add_string buf "  sw s3, 12(sp)\n";
  Buffer.add_string buf "  sw s4, 8(sp)\n";
  Buffer.add_string buf "  sw s5, 4(sp)\n";
  Buffer.add_string buf "  sw s6, 0(sp)\n";
  Buffer.add_string buf "  mv s0, sp\n";  (* 设置帧指针 *)
  
  (* 为局部变量分配空间 - 修改后的版本 *)
  (* 这里假设局部变量数量可以通过遍历函数体来确定 *)
  (* 实际实现可能需要更复杂的AST分析 *)
  let locals_size = 16 in  (* 假设局部变量需要16字节空间 *)
  if locals_size > 0 then begin
    Buffer.add_string buf (Printf.sprintf "  addi sp, sp, -%d\n" locals_size)
  end;
  
  emit_stmt buf f.body;
  
  (* 恢复栈和寄存器 *)
  if locals_size > 0 then begin
    Buffer.add_string buf (Printf.sprintf "  addi sp, s0, 0\n")
  end;
  Buffer.add_string buf "  lw ra, 28(sp)\n";
  Buffer.add_string buf "  lw s0, 24(sp)\n";
  Buffer.add_string buf "  lw s1, 20(sp)\n";
  Buffer.add_string buf "  lw s2, 16(sp)\n";
  Buffer.add_string buf "  lw s3, 12(sp)\n";
  Buffer.add_string buf "  lw s4, 8(sp)\n";
  Buffer.add_string buf "  lw s5, 4(sp)\n";
  Buffer.add_string buf "  lw s6, 0(sp)\n";
  Buffer.add_string buf "  addi sp, sp, 32\n";
  Buffer.add_string buf "  ret\n\n"

and emit_stmt buf = function
  | SBlock ss -> List.iter (emit_stmt buf) ss
  | SExpr e -> emit_expr buf e; Buffer.add_string buf "  addi sp, sp, 4  # pop\n"
  | SDecl (id, e) -> 
      emit_expr buf e;
      (* 假设变量存储在栈上 *)
      Buffer.add_string buf "  mv t1, s0\n";  (* 帧指针 *)
      Buffer.add_string buf "  addi t1, t1, -4\n";  (* 假设第一个局部变量位置 *)
      Buffer.add_string buf "  sw t0, 0(t1)\n";
      Buffer.add_string buf (Printf.sprintf "  # 变量 %s 存储在 t1\n" id)
  | SAssign (id, e) -> 
      emit_expr buf e;
      (* 假设变量存储在栈上 *)
      Buffer.add_string buf "  mv t1, s0\n";  (* 帧指针 *)
      Buffer.add_string buf "  addi t1, t1, -4\n";  (* 假设第一个局部变量位置 *)
      Buffer.add_string buf "  sw t0, 0(t1)\n";
      Buffer.add_string buf (Printf.sprintf "  # 变量 %s 赋值\n" id)
  | SReturn (Some e) ->  
      emit_expr buf e; 
      Buffer.add_string buf "  mv a0, t0  # 将返回值存入 a0\n"  
  | SReturn None ->  
      Buffer.add_string buf "  li a0, 0  # void 返回 0\n"  
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
  | SBreak ->
      let l = fresh_label () in
      Buffer.add_string buf (Printf.sprintf "  j %s\n" l);
      Buffer.add_string buf (Printf.sprintf "%s:\n" l)
  | SContinue ->
      let l = fresh_label () in
      Buffer.add_string buf (Printf.sprintf "  j %s\n" l);
      Buffer.add_string buf (Printf.sprintf "%s:\n" l)

and emit_expr buf = function
  | EInt n -> 
      if n >= -2048 && n <= 2047 then
        Buffer.add_string buf (Printf.sprintf "  li t0, %d\n" n)
      else
        Buffer.add_string buf (Printf.sprintf "  lui t0, %d\n  addi t0, t0, %d\n" 
                                 (n lsr 12) (n land 0xFFF))
  | EVar id -> 
      (* 假设变量存储在栈上 *)
      Buffer.add_string buf "  mv t0, s0\n";  (* 帧指针 *)
      Buffer.add_string buf "  addi t0, t0, -4\n";  (* 假设第一个局部变量位置 *)
      Buffer.add_string buf "  lw t0, 0(t0)\n";
      Buffer.add_string buf (Printf.sprintf "  # 加载变量 %s\n" id)
  | EBin (op, e1, e2) ->
      emit_expr buf e1;
      emit_stack_push buf "t0";
      emit_expr buf e2;
      emit_stack_pop buf "t1";
      Buffer.add_string buf (Printf.sprintf "  %s t0, t1, t0\n" (rv_op op))
  | EUn (`Neg, e) -> emit_expr buf e; Buffer.add_string buf "  neg t0, t0\n"
  | EUn (`Not, e) -> emit_expr buf e; Buffer.add_string buf "  seqz t0, t0\n"
  | ECall (f, args) ->
      (* 计算参数数量 *)
      let nargs = List.length args in
      
      (* 保存参数到栈 *)
      List.iter (fun arg -> emit_expr buf arg; emit_stack_push buf "t0") args;
      
      (* 将参数从栈移动到参数寄存器 *)
      let rec move_args i =
        if i < nargs && i < 8 then begin
          let reg = Printf.sprintf "a%d" i in
          Buffer.add_string buf (Printf.sprintf "  lw %s, %d(sp)\n" reg ((nargs - i - 1) * 4));
          move_args (i + 1)
        end
      in
      move_args 0;
      
      (* 对于超过8个的参数，需要调整栈指针 *)
      if nargs > 8 then begin
        Buffer.add_string buf (Printf.sprintf "  addi sp, sp, -%d\n" ((nargs - 8) * 4));
        let rec move_extra_args i =
          if i < nargs then begin
            let offset = (nargs - i - 1) * 4 in
            Buffer.add_string buf (Printf.sprintf "  lw t0, %d(sp)\n" (offset + ((nargs - 8) * 4)));
            Buffer.add_string buf (Printf.sprintf "  sw t0, %d(sp)\n" offset);
            move_extra_args (i + 1)
          end
        in
        move_extra_args 8
      end;
      
      (* 调用函数 *)
      Buffer.add_string buf (Printf.sprintf "  call %s\n" f);
      
      (* 恢复栈指针 *)
      if nargs > 8 then begin
        Buffer.add_string buf (Printf.sprintf "  addi sp, sp, %d\n" ((nargs - 8) * 4));
      end;
      
      (* 移除栈上的参数 *)
      if nargs > 0 then begin
        Buffer.add_string buf (Printf.sprintf "  addi sp, sp, %d\n" (nargs * 4));
      end;
      
      (* 获取返回值 *)
      Buffer.add_string buf "  mv t0, a0\n"
