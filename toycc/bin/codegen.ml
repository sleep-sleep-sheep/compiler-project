(* 代码生成器 - 生成RISC-V汇编 *)

open Ast

(* 寄存器定义（内联模块，无需单独open） *)
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
    | S0    (* x8/fp *)
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

(* 循环标签信息（用于break和continue） *)
type loop_labels = {
  cond_label: string;  (* 循环条件检查标签 *)
  end_label: string;   (* 循环结束标签 *)
}

(* 生成器状态（使用可变缩进） *)
type state = {
  mutable label_count: int;
  output: Buffer.t;
  indent_level: int ref;  (* 可变缩进级别 *)
  var_offsets: (string, int) Hashtbl.t;  (* 变量名到栈偏移量的映射 *)
  mutable loop_stack: loop_labels list;  (* 循环标签栈（可变） *)
}

(* 创建新的生成器状态 *)
let create_state var_offsets = {
  label_count = 0;
  output = Buffer.create 1024;
  indent_level = ref 0;  (* 初始缩进0 *)
  var_offsets;
  loop_stack = [];
}

(* 生成新的标签 *)
let new_label state prefix =
  let count = state.label_count in
  state.label_count <- count + 1;
  prefix ^ "_" ^ string_of_int count

(* 输出缩进 *)
let emit_indent state =
  for _ = 1 to !(state.indent_level) do
    Buffer.add_string state.output "  "
  done

(* 输出一行代码 *)
let emit_line state line =
  emit_indent state;
  Buffer.add_string state.output line;
  Buffer.add_char state.output '\n'

(* 增加缩进（直接修改状态） *)
let indent state =
  state.indent_level := !(state.indent_level) + 1

(* 减少缩进（直接修改状态） *)
let unindent state =
  state.indent_level := max 0 (!(state.indent_level) - 1)

(* 生成表达式代码（返回寄存器） *)
let rec gen_expr state expr =
  match expr with
  | Literal (IntLit n) ->
      let reg = Register.T0 in
      emit_line state (Printf.sprintf "li %s, %d" (Register.to_string reg) n);
      reg
  | Var id ->
      let offset = Hashtbl.find state.var_offsets id in
      let reg = Register.T0 in
      emit_line state (Printf.sprintf "lw %s, %d(sp)" (Register.to_string reg) offset);
      reg
  | BinOp (e1, op, e2) ->
      let r1 = gen_expr state e1 in
      let r2 = gen_expr state e2 in
      let emit_binop op_code =
        emit_line state (Printf.sprintf "%s %s, %s, %s" 
          op_code 
          (Register.to_string r1) 
          (Register.to_string r1) 
          (Register.to_string r2))
      in
      (match op with
       | "+" -> emit_binop "add"
       | "-" -> emit_binop "sub"
       | "*" -> emit_binop "mul"
       | "/" -> emit_binop "div"
       | "%" -> emit_binop "rem"
       | "&&" -> emit_binop "and"
       | "||" -> emit_binop "or"
       | "==" ->
           emit_binop "sub";
           emit_line state (Printf.sprintf "seqz %s, %s" (Register.to_string r1) (Register.to_string r1))
       | "!=" ->
           emit_binop "sub";
           emit_line state (Printf.sprintf "snez %s, %s" (Register.to_string r1) (Register.to_string r1))
       | "<" -> emit_binop "slt"
       | ">" ->
           emit_line state (Printf.sprintf "slt %s, %s, %s" 
             (Register.to_string r1) 
             (Register.to_string r2) 
             (Register.to_string r1))
       | "<=" ->
           emit_line state (Printf.sprintf "slt %s, %s, %s" 
             (Register.to_string r1) 
             (Register.to_string r2) 
             (Register.to_string r1));
           emit_line state (Printf.sprintf "xori %s, %s, 1" 
             (Register.to_string r1) (Register.to_string r1))
       | ">=" ->
           emit_binop "slt";
           emit_line state (Printf.sprintf "xori %s, %s, 1" 
             (Register.to_string r1) (Register.to_string r1))
       | _ -> failwith ("Unsupported operator: " ^ op));
      r1
  | UnOp (op, e) ->
      let r = gen_expr state e in
      (match op with
       | "-" -> emit_line state (Printf.sprintf "neg %s, %s" (Register.to_string r) (Register.to_string r))
       | "!" -> 
           emit_line state (Printf.sprintf "seqz %s, %s" (Register.to_string r) (Register.to_string r));
           emit_line state (Printf.sprintf "xori %s, %s, 1" (Register.to_string r) (Register.to_string r))
       | "+" -> emit_line state (Printf.sprintf "mv %s, %s" (Register.to_string r) (Register.to_string r))
       | _ -> failwith ("Unsupported unary operator: " ^ op));
      r
  | Call (fname, args) ->
      let num_args = List.length args in
      let stack_args = if num_args > 8 then num_args - 8 else 0 in
      let stack_size = stack_args * 4 in
      if stack_size > 0 then
        emit_line state (Printf.sprintf "addi sp, sp, -%d" stack_size);
      List.iteri (fun i arg ->
        let arg_reg = gen_expr state arg in
        if i < 8 then
          let dest_reg = match i with
            | 0 -> Register.A0 | 1 -> Register.A1 | 2 -> Register.A2 | 3 -> Register.A3
            | 4 -> Register.A4 | 5 -> Register.A5 | 6 -> Register.A6 | 7 -> Register.A7
            | _ -> assert false
          in
          emit_line state (Printf.sprintf "mv %s, %s" 
            (Register.to_string dest_reg) 
            (Register.to_string arg_reg))
        else
          let offset = (i - 8) * 4 in
          emit_line state (Printf.sprintf "sw %s, %d(sp)" 
            (Register.to_string arg_reg) offset)
      ) args;
      emit_line state (Printf.sprintf "call %s" fname);
      if stack_size > 0 then
        emit_line state (Printf.sprintf "addi sp, sp, %d" stack_size);
      Register.A0
  | Paren e ->
      gen_expr state e

(* 生成语句代码（返回unit，直接修改状态） *)
let rec gen_stmt state stmt =
  match stmt with
  | Block stmts ->
      indent state;  (* 进入块增加缩进 *)
      List.iter (gen_stmt state) stmts;  (* 递归处理内部语句 *)
      unindent state  (* 离开块减少缩进 *)
  | Empty ->
      emit_line state "nop"
  | ExprStmt expr ->
      ignore (gen_expr state expr)
  | Assign (id, expr) ->
      let offset = Hashtbl.find state.var_offsets id in
      let reg = gen_expr state expr in
      emit_line state (Printf.sprintf "sw %s, %d(sp)" 
        (Register.to_string reg) offset)
  | Decl (id, expr) ->
      let offset = Hashtbl.find state.var_offsets id in
      let reg = gen_expr state expr in
      emit_line state (Printf.sprintf "sw %s, %d(sp)" 
        (Register.to_string reg) offset)
  | If (cond, then_stmt, else_stmt_opt) ->
      let else_label = new_label state "else" in
      let end_label = new_label state "endif" in
      let cond_reg = gen_expr state cond in
      emit_line state (Printf.sprintf "beqz %s, %s" 
        (Register.to_string cond_reg) else_label);
      gen_stmt state then_stmt;  (* 处理then分支 *)
      emit_line state (Printf.sprintf "j %s" end_label);
      emit_line state (Printf.sprintf "%s:" else_label);
      (match else_stmt_opt with  (* 处理else分支 *)
       | Some else_stmt -> gen_stmt state else_stmt
       | None -> ());
      emit_line state (Printf.sprintf "%s:" end_label)
  | While (cond, body) ->
      let loop_label = new_label state "loop" in
      let cond_label = new_label state "cond" in
      let end_label = new_label state "endloop" in
      let loop_info = { cond_label; end_label } in
      state.loop_stack <- loop_info :: state.loop_stack;  (* 入栈循环信息 *)
      emit_line state (Printf.sprintf "j %s" cond_label);
      emit_line state (Printf.sprintf "%s:" loop_label);
      indent state;
      gen_stmt state body;  (* 处理循环体 *)
      unindent state;
      emit_line state (Printf.sprintf "%s:" cond_label);
      let cond_reg = gen_expr state cond in
      emit_line state (Printf.sprintf "bnez %s, %s" 
        (Register.to_string cond_reg) loop_label);
      emit_line state (Printf.sprintf "%s:" end_label);
      state.loop_stack <- List.tl state.loop_stack  (* 出栈循环信息 *)
  | Break ->
      (match state.loop_stack with
       | [] -> failwith "Break outside loop"
       | { end_label; _ } :: _ ->
           emit_line state (Printf.sprintf "j %s" end_label))
  | Continue ->
      (match state.loop_stack with
       | [] -> failwith "Continue outside loop"
       | { cond_label; _ } :: _ ->
           emit_line state (Printf.sprintf "j %s" cond_label))
  | Return expr_opt ->
      (match expr_opt with
       | None -> emit_line state "li a0, 0"
       | Some expr -> 
           let reg = gen_expr state expr in
           emit_line state (Printf.sprintf "mv a0, %s" (Register.to_string reg)));
      emit_line state "ret"

(* 生成函数代码 *)
let gen_function func =
  let var_offsets = Hashtbl.create 16 in
  (* 这里需要从语义分析获取变量偏移量，示例中临时填充 *)
  (* 实际应替换为：Hashtbl.iter (fun k (_,o) -> Hashtbl.add var_offsets k o) func.var_offsets; *)
  let state = create_state var_offsets in
  emit_line state (Printf.sprintf "%s:" func.fname);
  emit_line state "addi sp, sp, -16";
  emit_line state "sw ra, 12(sp)";
  emit_line state "sw s0, 8(sp)";
  emit_line state "sw s1, 4(sp)";
  emit_line state "mv s0, sp";
  let local_size = 4 * Hashtbl.length var_offsets in
  if local_size > 0 then
    emit_line state (Printf.sprintf "addi sp, sp, -%d" local_size);
  List.iter (gen_stmt state) func.body;  (* 生成函数体 *)
  if local_size > 0 then
    emit_line state (Printf.sprintf "addi sp, sp, %d" local_size);
  emit_line state "lw ra, 12(sp)";
  emit_line state "lw s0, 8(sp)";
  emit_line state "lw s1, 4(sp)";
  emit_line state "addi sp, sp, 16";
  emit_line state "ret";
  Buffer.contents state.output

(* 生成程序代码 *)
let gen_program program =
  let buffer = Buffer.create 4096 in
  Buffer.add_string buffer ".text\n";
  Buffer.add_string buffer ".globl main\n";
  List.iter (fun func ->
    let func_asm = gen_function func in
    Buffer.add_string buffer func_asm;
    Buffer.add_char buffer '\n'
  ) program;
  Buffer.contents buffer
