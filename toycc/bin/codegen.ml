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
}

(* 创建新的生成器状态 *)
let create_state () = {
  label_count = 0;
  output = Buffer.create 1024;
  indent_level = 0;
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

(* 生成表达式代码 *)
let rec gen_expr state expr =
  match expr with
  | Literal (IntLit n) ->
      let reg = Register.T0 in
      emit_line state (Printf.sprintf "li %s, %d" (Register.to_string reg) n);
      reg
  | Var id ->
      let reg = Register.T0 in
      emit_line state (Printf.sprintf "lw %s, %s(sp)" (Register.to_string reg) id);
      reg
  | BinOp (e1, op, e2) ->
      let r1 = gen_expr state e1 in
      let r2 = gen_expr state e2 in
      let op_code = match op with
        | "+" -> "add"
        | "-" -> "sub"
        | "*" -> "mul"
        | "/" -> "div"
        | "%" -> "rem"
        | "&&" -> "and"
        | "||" -> "or"
        | "==" -> "seqz"  (* 需要调整 *)
        | "!=" -> "snez"  (* 需要调整 *)
        | "<" -> "slt"
        | ">" -> "sgt"    (* 不存在，需要调整 *)
        | "<=" -> "sle"   (* 不存在，需要调整 *)
        | ">=" -> "sge"   (* 不存在，需要调整 *)
        | _ -> failwith ("Unsupported operator: " ^ op)
      in
      emit_line state (Printf.sprintf "%s %s, %s, %s" 
        op_code 
        (Register.to_string r1) 
        (Register.to_string r1) 
        (Register.to_string r2));
      r1
  | UnOp (op, e) ->
      let r = gen_expr state e in
      let op_code = match op with
        | "-" -> "neg"
        | "!" -> "seqz"
        | _ -> failwith ("Unsupported unary operator: " ^ op)
      in
      emit_line state (Printf.sprintf "%s %s, %s" 
        op_code 
        (Register.to_string r) 
        (Register.to_string r));
      r
  | Call (fname, args) ->
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
        emit_line state (Printf.sprintf "mv %s, %s" 
          (Register.to_string reg) 
          (Register.to_string arg_reg))
      ) args;
      
      (* 调用函数 *)
      emit_line state (Printf.sprintf "call %s" fname);
      
      (* 返回值在a0中 *)
      Register.A0
  | Paren e ->
      gen_expr state e

(* 生成语句代码 *)
let rec gen_stmt state stmt =
  match stmt with
  | Block stmts ->
      emit_line state "{  # block start";
      let new_state = indent state in
      List.iter (gen_stmt new_state) stmts;
      emit_line (unindent new_state) "}  # block end"
  | Empty ->
      emit_line state "nop"
  | ExprStmt expr ->
      ignore (gen_expr state expr)
  | Assign (id, expr) ->
      let reg = gen_expr state expr in
      emit_line state (Printf.sprintf "sw %s, %s(sp)" 
        (Register.to_string reg) id)
  | Decl (id, expr) ->
      let reg = gen_expr state expr in
      emit_line state (Printf.sprintf "sw %s, %s(sp)" 
        (Register.to_string reg) id)
  | If (cond, then_stmt, else_stmt_opt) ->
      let else_label = new_label state "else" in
      let end_label = new_label state "endif" in
      
      (* 计算条件表达式 *)
      let cond_reg = gen_expr state cond in
      
      (* 如果条件为假，跳转到else *)
      emit_line state (Printf.sprintf "beqz %s, %s" 
        (Register.to_string cond_reg) else_label);
      
      (* 生成then部分 *)
      gen_stmt state then_stmt;
      
      (* 跳转到结束 *)
      emit_line state (Printf.sprintf "j %s" end_label);
      
      (* else标签 *)
      emit_line state (Printf.sprintf "%s:" else_label);
      
      (* 生成else部分（如果有） *)
      Option.iter (gen_stmt state) else_stmt_opt;
      
      (* 结束标签 *)
      emit_line state (Printf.sprintf "%s:" end_label)
  | While (cond, body) ->
      let loop_label = new_label state "loop" in
      let cond_label = new_label state "cond" in
      let end_label = new_label state "endloop" in
      
      (* 跳转到条件检查 *)
      emit_line state (Printf.sprintf "j %s" cond_label);
      
      (* 循环体开始 *)
      emit_line state (Printf.sprintf "%s:" loop_label);
      gen_stmt state body;
      
      (* 条件检查标签 *)
      emit_line state (Printf.sprintf "%s:" cond_label);
      let cond_reg = gen_expr state cond in
      
      (* 如果条件为真，继续循环 *)
      emit_line state (Printf.sprintf "bnez %s, %s" 
        (Register.to_string cond_reg) loop_label);
      
      (* 结束标签 *)
      emit_line state (Printf.sprintf "%s:" end_label)
  | Break ->
      failwith "Break not implemented"
  | Continue ->
      failwith "Continue not implemented"
  | Return expr_opt ->
      begin match expr_opt with
      | None ->
          emit_line state "li a0, 0"  (* void函数返回0 *)
      | Some expr ->
          let reg = gen_expr state expr in
          emit_line state (Printf.sprintf "mv a0, %s" (Register.to_string reg))
      end;
      emit_line state "ret"

(* 生成函数代码 *)
let gen_function state func =
  (* 函数标签 *)
  emit_line state (Printf.sprintf "%s:" func.fname);
  
  (* 保存调用者保存的寄存器 *)
  emit_line state "addi sp, sp, -16";
  emit_line state "sw ra, 12(sp)";
  emit_line state "sw s0, 8(sp)";
  emit_line state "sw s1, 4(sp)";
  
  (* 设置帧指针 *)
  emit_line state "mv s0, sp";
  
  (* 为局部变量分配空间 *)
  let local_size = 4 * List.length (List.filter (function 
    | Decl _ -> true 
    | _ -> false) func.body) 
  in
  if local_size > 0 then begin
    emit_line state (Printf.sprintf "addi sp, sp, -%d" local_size);
  end;
  
  (* 生成函数体 *)
  List.iter (gen_stmt state) func.body;
  
  (* 恢复寄存器和栈指针 *)
  if local_size > 0 then begin
    emit_line state (Printf.sprintf "addi sp, sp, %d" local_size);
  end;
  emit_line state "lw ra, 12(sp)";
  emit_line state "lw s0, 8(sp)";
  emit_line state "lw s1, 4(sp)";
  emit_line state "addi sp, sp, 16";
  
  (* 返回指令 *)
  emit_line state "ret"

(* 生成程序代码 *)
let gen_program program =
  let state = create_state () in
  
  (* 输出文件头 *)
  emit_line state ".text";
  emit_line state ".globl main";
  
  (* 生成每个函数 *)
  List.iter (gen_function state) program;
  
  (* 返回生成的汇编代码 *)
  Buffer.contents state.output  
