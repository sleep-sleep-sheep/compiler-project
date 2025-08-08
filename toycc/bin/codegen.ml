(* 代码生成器 - 生成RISC-V汇编 *)

open Ast

(* 寄存器分配 *)
module Register = struct
  type t = 
    | Zero  (* x0 *)
    | RA    (* x1 *)
    | SP    (* x2 *)
    | GP    (* x3 *)
    | TP    (* x4 *)
    | T0    (* x5 *)
    | T1    (* x6 *)
    | T2    (* x7 *)
    | S0    (* x8 *)
    | S1    (* x9 *)
    | A0    (* x10 *)
    | A1    (* x11 *)
    | A2    (* x12 *)
    | A3    (* x13 *)
    | A4    (* x14 *)
    | A5    (* x15 *)
    | A6    (* x16 *)
    | A7    (* x17 *)
    | S2    (* x18 *)
    | S3    (* x19 *)
    | S4    (* x20 *)
    | S5    (* x21 *)
    | S6    (* x22 *)
    | S7    (* x23 *)
    | S8    (* x24 *)
    | S9    (* x25 *)
    | S10   (* x26 *)
    | S11   (* x27 *)
    | T3    (* x28 *)
    | T4    (* x29 *)
    | T5    (* x30 *)
    | T6    (* x31 *)

  let to_string = function
    | Zero -> "x0"
    | RA -> "x1"
    | SP -> "x2"
    | GP -> "x3"
    | TP -> "x4"
    | T0 -> "x5"
    | T1 -> "x6"
    | T2 -> "x7"
    | S0 -> "x8"
    | S1 -> "x9"
    | A0 -> "x10"
    | A1 -> "x11"
    | A2 -> "x12"
    | A3 -> "x13"
    | A4 -> "x14"
    | A5 -> "x15"
    | A6 -> "x16"
    | A7 -> "x17"
    | S2 -> "x18"
    | S3 -> "x19"
    | S4 -> "x20"
    | S5 -> "x21"
    | S6 -> "x22"
    | S7 -> "x23"
    | S8 -> "x24"
    | S9 -> "x25"
    | S10 -> "x26"
    | S11 -> "x27"
    | T3 -> "x28"
    | T4 -> "x29"
    | T5 -> "x30"
    | T6 -> "x31"
end

(* 生成器状态 *)
type state = {
  mutable label_count: int;
  output: Buffer.t;
  indent_level: int;
  local_vars: (string * int) list;  (* 变量名到栈偏移的映射 *)
  mutable next_offset: int;         (* 下一个可用的栈偏移量 *)
}

(* 创建新的生成器状态 *)
let create_state () = {
  label_count = 0;
  output = Buffer.create 1024;
  indent_level = 0;
  local_vars = [];
  next_offset = 0;
}

(* 复制状态并更新变量和偏移量 *)
let update_state state vars offset = {
  state with
  local_vars = vars;
  next_offset = offset;
}

(* 生成新的标签 *)
let new_label state prefix =
  let count = state.label_count in
  state.label_count <- count + 1;
  prefix ^ string_of_int count

(* 输出缩进 *)
let emit_indent state =
  for _ = 1 to state.indent_level do
    Buffer.add_string state.output "  "
  done

(* 输出一行代码 *)
let emit_line state line =
  emit_indent state;
  Buffer.add_string state.output line;
  Buffer.add_char state.output '\n'

(* 增加缩进 *)
let indent state =
  { state with indent_level = state.indent_level + 1 }

(* 减少缩进 *)
let unindent state =
  { state with indent_level = max 0 (state.indent_level - 1) }

(* 查找变量的栈偏移 *)
let find_var_offset state var =
  match List.assoc_opt var state.local_vars with
  | Some offset -> offset
  | None -> failwith ("Undefined variable: " ^ var)

(* 为新变量分配栈空间 *)
let allocate_var state var =
  let offset = state.next_offset in
  let new_vars = (var, offset) :: state.local_vars in
  let new_state = update_state state new_vars (offset + 4) in
  (new_state, offset)

(* 加载立即数到寄存器（不使用伪指令） *)
let load_imm state reg value =
  if value >= -2048 && value < 2048 then begin
    emit_line state (Printf.sprintf "addi %s, x0, %d" (Register.to_string reg) value)
  end else begin
    (* 使用 lui 和 addi 组合加载大立即数 *)
    let upper = (value lsr 12) land 0xFFFFF in
    let lower = value land 0xFFF in
    emit_line state (Printf.sprintf "lui %s, %d" (Register.to_string reg) upper);
    if lower <> 0 then begin
      emit_line state (Printf.sprintf "addi %s, %s, %d" 
        (Register.to_string reg) (Register.to_string reg) lower)
    end
  end

(* 生成表达式代码 *)
let rec gen_expr state expr =
  match expr with
  | Literal (IntLit n) ->
      let reg = Register.T0 in
      load_imm state reg n;
      reg
  | Var id ->
      let reg = Register.T0 in
      let offset = find_var_offset state id in
      emit_line state (Printf.sprintf "lw %s, %d(sp)" (Register.to_string reg) offset);
      reg
  | BinOp (e1, op, e2) ->
      let r1 = gen_expr state e1 in
      let r2 = gen_expr state e2 in
      begin match op with
        | "+" ->
            emit_line state (Printf.sprintf "add %s, %s, %s" 
              (Register.to_string r1) 
              (Register.to_string r1) 
              (Register.to_string r2))
        | "-" ->
            emit_line state (Printf.sprintf "sub %s, %s, %s" 
              (Register.to_string r1) 
              (Register.to_string r1) 
              (Register.to_string r2))
        | "*" ->
            emit_line state (Printf.sprintf "mul %s, %s, %s" 
              (Register.to_string r1) 
              (Register.to_string r1) 
              (Register.to_string r2))
        | "/" ->
            emit_line state (Printf.sprintf "div %s, %s, %s" 
              (Register.to_string r1) 
              (Register.to_string r1) 
              (Register.to_string r2))
        | "%" ->
            emit_line state (Printf.sprintf "rem %s, %s, %s" 
              (Register.to_string r1) 
              (Register.to_string r1) 
              (Register.to_string r2))
        | "&&" ->
            emit_line state (Printf.sprintf "and %s, %s, %s" 
              (Register.to_string r1) 
              (Register.to_string r1) 
              (Register.to_string r2))
        | "||" ->
            emit_line state (Printf.sprintf "or %s, %s, %s" 
              (Register.to_string r1) 
              (Register.to_string r1) 
              (Register.to_string r2))
        | "==" ->
            emit_line state (Printf.sprintf "xor %s, %s, %s" 
              (Register.to_string r1) 
              (Register.to_string r1) 
              (Register.to_string r2));
            emit_line state (Printf.sprintf "seqz %s, %s" 
              (Register.to_string r1) 
              (Register.to_string r1))
        | "!=" ->
            emit_line state (Printf.sprintf "xor %s, %s, %s" 
              (Register.to_string r1) 
              (Register.to_string r1) 
              (Register.to_string r2));
            emit_line state (Printf.sprintf "snez %s, %s" 
              (Register.to_string r1) 
              (Register.to_string r1))
        | "<" ->
            emit_line state (Printf.sprintf "slt %s, %s, %s" 
              (Register.to_string r1) 
              (Register.to_string r1) 
              (Register.to_string r2))
        | ">" ->
            (* a > b 等价于 b < a *)
            emit_line state (Printf.sprintf "slt %s, %s, %s" 
              (Register.to_string r1) 
              (Register.to_string r2) 
              (Register.to_string r1))
        | "<=" ->
            (* a <= b 等价于 !(a > b) *)
            emit_line state (Printf.sprintf "slt %s, %s, %s" 
              (Register.to_string r1) 
              (Register.to_string r2) 
              (Register.to_string r1));
            emit_line state (Printf.sprintf "xori %s, %s, 1" 
              (Register.to_string r1) 
              (Register.to_string r1))
        | ">=" ->
            (* a >= b 等价于 !(a < b) *)
            emit_line state (Printf.sprintf "slt %s, %s, %s" 
              (Register.to_string r1) 
              (Register.to_string r1) 
              (Register.to_string r2));
            emit_line state (Printf.sprintf "xori %s, %s, 1" 
              (Register.to_string r1) 
              (Register.to_string r1))
        | _ -> failwith ("Unsupported operator: " ^ op)
      end;
      r1
  | UnOp (op, e) ->
      let r = gen_expr state e in
      begin match op with
        | "-" ->
            emit_line state (Printf.sprintf "sub %s, x0, %s" 
              (Register.to_string r) 
              (Register.to_string r))
        | "!" ->
            emit_line state (Printf.sprintf "seqz %s, %s" 
              (Register.to_string r) 
              (Register.to_string r));
            emit_line state (Printf.sprintf "xori %s, %s, 1" 
              (Register.to_string r) 
              (Register.to_string r))
        | "+" ->
            (* 一元正号，无需操作 *)
            ()
        | _ -> failwith ("Unsupported unary operator: " ^ op)
      end;
      r
  | Call (fname, args) ->
      (* 检查参数数量 *)
      if List.length args > 8 then
        failwith "Too many arguments (maximum 8 allowed)";
      
      (* 将参数放入适当的寄存器 *)
      List.iteri (fun i arg ->
        let reg = match i with
          | 0 -> Register.A0
          | 1 -> Register.A1
          | 2 -> Register.A2
          | 3 -> Register.A3
          | 4 -> Register.A4
          | 5 -> Register.A5
          | 6 -> Register.A6
          | 7 -> Register.A7
          | _ -> failwith "Too many arguments"
        in
        let arg_reg = gen_expr state arg in
        emit_line state (Printf.sprintf "add %s, %s, x0" 
          (Register.to_string reg) 
          (Register.to_string arg_reg))
      ) args;
      
      (* 调用函数 *)
      emit_line state (Printf.sprintf "jal ra, %s" fname);
      
      (* 返回值在a0中 *)
      Register.A0
  | Paren e ->
      gen_expr state e

(* 生成语句代码 *)
let rec gen_stmt state stmt =
  match stmt with
  | Block stmts ->
      emit_line state "# block start";
      let new_state = indent state in
      let final_state = List.fold_left (fun s st -> gen_stmt s st) new_state stmts in
      emit_line (unindent final_state) "# block end";
      final_state
  | Empty ->
      emit_line state "nop";
      state
  | ExprStmt expr ->
      ignore (gen_expr state expr);
      state
  | Assign (id, expr) ->
      let reg = gen_expr state expr in
      let offset = find_var_offset state id in
      emit_line state (Printf.sprintf "sw %s, %d(sp)" 
        (Register.to_string reg) offset);
      state
  | Decl (id, expr) ->
      let (new_state, offset) = allocate_var state id in
      let reg = gen_expr new_state expr in
      emit_line new_state (Printf.sprintf "sw %s, %d(sp)" 
        (Register.to_string reg) offset);
      new_state
  | If (cond, then_stmt, else_stmt_opt) ->
      let else_label = new_label state "else" in
      let end_label = new_label state "endif" in
      
      (* 计算条件表达式 *)
      let cond_reg = gen_expr state cond in
      
      (* 如果条件为假，跳转到else *)
      emit_line state (Printf.sprintf "beq %s, x0, %s" 
        (Register.to_string cond_reg) else_label);
      
      (* 生成then部分 *)
      let state_after_then = gen_stmt state then_stmt in
      
      (* 跳转到结束 *)
      emit_line state_after_then (Printf.sprintf "j %s" end_label);
      
      (* else标签 *)
      emit_line state_after_then (Printf.sprintf "%s:" else_label);
      
      (* 生成else部分（如果有） *)
      let state_after_else = 
        match else_stmt_opt with
        | Some else_stmt -> gen_stmt state_after_then else_stmt
        | None -> state_after_then
      in
      
      (* 结束标签 *)
      emit_line state_after_else (Printf.sprintf "%s:" end_label);
      state_after_else
  | While (cond, body) ->
      let loop_label = new_label state "loop" in
      let cond_label = new_label state "cond" in
      let end_label = new_label state "endloop" in
      
      (* 跳转到条件检查 *)
      emit_line state (Printf.sprintf "j %s" cond_label);
      
      (* 循环体开始 *)
      emit_line state (Printf.sprintf "%s:" loop_label);
      let state_after_body = gen_stmt state body in
      
      (* 条件检查标签 *)
      emit_line state_after_body (Printf.sprintf "%s:" cond_label);
      let cond_reg = gen_expr state_after_body cond in
      
      (* 如果条件为真，继续循环 *)
      emit_line state_after_body (Printf.sprintf "bne %s, x0, %s" 
        (Register.to_string cond_reg) loop_label);
      
      (* 结束标签 *)
      emit_line state_after_body (Printf.sprintf "%s:" end_label);
      state_after_body
  | Break ->
      (* 实现break需要知道循环结束标签，这里简化处理 *)
      let break_label = new_label state "break" in
      emit_line state (Printf.sprintf "j %s" break_label);
      emit_line state (Printf.sprintf "%s:" break_label);
      state
  | Continue ->
      (* 实现continue需要知道循环条件标签，这里简化处理 *)
      let continue_label = new_label state "continue" in
      emit_line state (Printf.sprintf "j %s" continue_label);
      emit_line state (Printf.sprintf "%s:" continue_label);
      state
  | Return expr_opt ->
      begin match expr_opt with
      | None ->
          load_imm state Register.A0 0
      | Some expr ->
          let reg = gen_expr state expr in
          emit_line state (Printf.sprintf "add a0, %s, x0" (Register.to_string reg))
      end;
      emit_line state "jalr x0, 0(ra)";
      state

(* 生成函数代码 *)
let gen_function state func =
  (* 函数标签 *)
  emit_line state (Printf.sprintf "%s:" func.fname);
  
  (* 保存调用者保存的寄存器 *)
  emit_line state "addi sp, sp, -16";
  emit_line state "sw ra, 12(sp)";
  emit_line state "sw s0, 8(sp)";
  emit_line state "sw s1, 4(sp)";
  emit_line state "sw s2, 0(sp)";
  
  (* 设置帧指针 *)
  emit_line state "add s0, sp, x0";
  
  (* 初始化局部变量状态 *)
  let local_state = { state with local_vars = []; next_offset = 16 } in
  
  (* 计算所需的栈空间 *)
  let temp_state = List.fold_left (fun s st -> gen_stmt s st) local_state func.body in
  let local_size = temp_state.next_offset - 16 in
  
  (* 为局部变量分配空间 *)
  if local_size > 0 then begin
    emit_line state (Printf.sprintf "addi sp, sp, -%d" local_size);
  end;
  
  (* 生成函数体 *)
  let _ = List.fold_left (fun s st -> gen_stmt s st) local_state func.body in
  
  (* 恢复栈指针 *)
  if local_size > 0 then begin
    emit_line state (Printf.sprintf "addi sp, sp, %d" local_size);
  end;
  
  (* 恢复寄存器 *)
  emit_line state "lw ra, 12(sp)";
  emit_line state "lw s0, 8(sp)";
  emit_line state "lw s1, 4(sp)";
  emit_line state "lw s2, 0(sp)";
  emit_line state "addi sp, sp, 16";
  
  (* 返回指令 *)
  emit_line state "jalr x0, 0(ra)";
  state

(* 生成程序代码 *)
let gen_program program =
  let state = create_state () in
  
  (* 输出文件头 *)
  emit_line state ".text";
  emit_line state ".globl main";
  
  (* 生成每个函数 *)
  let _ = List.fold_left (fun s f -> gen_function s f) state program in
  
  (* 返回生成的汇编代码 *)
  Buffer.contents state.output
