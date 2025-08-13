open Ast

(* RISC-V 32位寄存器定义 - 包含s0-s11完整12个寄存器 *)
type reg =
  | Zero 
  | Ra 
  | Sp 
  | Gp 
  | Tp 
  | T0 
  | T1 
  | T2 
  | Fp 
  | S0  (* 新增S0寄存器 *)
  | S1 
  | A0 
  | A1 
  | A2 
  | A3 
  | A4 
  | A5 
  | A6 
  | A7 
  | S2 
  | S3 
  | S4 
  | S5 
  | S6 
  | S7 
  | S8 
  | S9 
  | S10 
  | S11 
  | T3 
  | T4 
  | T5 
  | T6 

(* 将寄存器转换为字符串 *)
let reg_to_string reg = 
  match reg with
  | Zero -> "zero"
  | Ra -> "ra"
  | Sp -> "sp"
  | Gp -> "gp"
  | Tp -> "tp"
  | T0 -> "t0"
  | T1 -> "t1"
  | T2 -> "t2"
  | Fp -> "fp"
  | S0 -> "s0"  (* S0寄存器映射 *)
  | S1 -> "s1"
  | A0 -> "a0"
  | A1 -> "a1"
  | A2 -> "a2"
  | A3 -> "a3"
  | A4 -> "a4"
  | A5 -> "a5"
  | A6 -> "a6"
  | A7 -> "a7"
  | S2 -> "s2"
  | S3 -> "s3"
  | S4 -> "s4"
  | S5 -> "s5"
  | S6 -> "s6"
  | S7 -> "s7"
  | S8 -> "s8"
  | S9 -> "s9"
  | S10 -> "s10"
  | S11 -> "s11"
  | T3 -> "t3"
  | T4 -> "t4"
  | T5 -> "t5"
  | T6 -> "t6"

(* 辅助函数：正确拆分立即数为高位和低位 *)
let split_imm imm =
  let imm32 = imm land 0xFFFFFFFF in
  let lower = imm32 land 0xFFF in
  let adjusted_lower =
    if (lower land 0x800) != 0 then lower - 4096 else lower
  in
  let upper = (imm32 - adjusted_lower) lsr 12 in
  (upper land 0xFFFFF, adjusted_lower)

(* RISC-V 指令类型 *)
type instruction =
  | Add of reg * reg * reg 
  | Addi of reg * reg * int 
  | Sub of reg * reg * reg 
  | Mul of reg * reg * reg 
  | Div of reg * reg * reg 
  | Rem of reg * reg * reg 
  | And of reg * reg * reg 
  | Or of reg * reg * reg 
  | Xor of reg * reg * reg 
  | Xori of reg * reg * int 
  | Slt of reg * reg * reg 
  | Slti of reg * reg * int 
  | Sltu of reg * reg * reg 
  | Sltiu of reg * reg * int 
  | Lw of reg * int * reg 
  | Sw of reg * int * reg 
  | Beq of reg * reg * string 
  | Bne of reg * reg * string 
  | Blt of reg * reg * string 
  | Bge of reg * reg * string 
  | J of string 
  | Jal of reg * string 
  | Jalr of reg * reg * int 
  | Ret 
  | Li of reg * int 
  | Lui of reg * int 
  | Mv of reg * reg 
  | Nop 

(* 将指令转换为汇编字符串 *)
let instr_to_string instr = match instr with
  | Add (rd, rs1, rs2) ->
    Printf.sprintf "add %s, %s, %s"
      (reg_to_string rd) (reg_to_string rs1) (reg_to_string rs2)
  | Addi (rd, rs1, imm) ->
    if imm < -2048 || imm > 2047 then
      failwith (Printf.sprintf "Addi immediate out of range: %d" imm)
    else
      Printf.sprintf "addi %s, %s, %d" (reg_to_string rd) (reg_to_string rs1) imm
  | Sub (rd, rs1, rs2) ->
    Printf.sprintf "sub %s, %s, %s"
      (reg_to_string rd) (reg_to_string rs1) (reg_to_string rs2)
  | Mul (rd, rs1, rs2) ->
    Printf.sprintf "mul %s, %s, %s"
      (reg_to_string rd) (reg_to_string rs1) (reg_to_string rs2)
  | Div (rd, rs1, rs2) ->
    Printf.sprintf "div %s, %s, %s"
      (reg_to_string rd) (reg_to_string rs1) (reg_to_string rs2)
  | Rem (rd, rs1, rs2) ->
    Printf.sprintf "rem %s, %s, %s"
      (reg_to_string rd) (reg_to_string rs1) (reg_to_string rs2)
  | And (rd, rs1, rs2) ->
    Printf.sprintf "and %s, %s, %s"
      (reg_to_string rd) (reg_to_string rs1) (reg_to_string rs2)
  | Or (rd, rs1, rs2) ->
    Printf.sprintf "or %s, %s, %s"
      (reg_to_string rd) (reg_to_string rs1) (reg_to_string rs2)
  | Xor (rd, rs1, rs2) ->
    Printf.sprintf "xor %s, %s, %s"
      (reg_to_string rd) (reg_to_string rs1) (reg_to_string rs2)
  | Xori (rd, rs1, imm) ->
    Printf.sprintf "xori %s, %s, %d" (reg_to_string rd) (reg_to_string rs1) imm
  | Slt (rd, rs1, rs2) ->
    Printf.sprintf "slt %s, %s, %s"
      (reg_to_string rd) (reg_to_string rs1) (reg_to_string rs2)
  | Slti (rd, rs1, imm) ->
    Printf.sprintf "slti %s, %s, %d" (reg_to_string rd) (reg_to_string rs1) imm
  | Sltu (rd, rs1, rs2) ->
    Printf.sprintf "sltu %s, %s, %s"
      (reg_to_string rd) (reg_to_string rs1) (reg_to_string rs2)
  | Sltiu (rd, rs1, imm) ->
    Printf.sprintf "sltiu %s, %s, %d" (reg_to_string rd) (reg_to_string rs1) imm
  | Lw (rd, offset, rs1) ->
    Printf.sprintf "lw %s, %d(%s)" (reg_to_string rd) offset (reg_to_string rs1)
  | Sw (rs2, offset, rs1) ->
    Printf.sprintf "sw %s, %d(%s)" (reg_to_string rs2) offset (reg_to_string rs1)
  | Beq (rs1, rs2, label) ->
    Printf.sprintf "beq %s, %s, %s" (reg_to_string rs1) (reg_to_string rs2) label
  | Bne (rs1, rs2, label) ->
    Printf.sprintf "bne %s, %s, %s" (reg_to_string rs1) (reg_to_string rs2) label
  | Blt (rs1, rs2, label) ->
    Printf.sprintf "blt %s, %s, %s" (reg_to_string rs1) (reg_to_string rs2) label
  | Bge (rs1, rs2, label) ->
    Printf.sprintf "bge %s, %s, %s" (reg_to_string rs1) (reg_to_string rs2) label
  | J label -> Printf.sprintf "j %s" label
  | Jal (rd, label) -> Printf.sprintf "jal %s, %s" (reg_to_string rd) label
  | Jalr (rd, rs1, offset) ->
    Printf.sprintf "jalr %s, %s, %d" (reg_to_string rd) (reg_to_string rs1) offset
  | Ret -> "ret"
  | Li (rd, imm) -> 
    if imm >= -2048 && imm <= 2047 then
      Printf.sprintf "li %s, %d" (reg_to_string rd) imm
    else
      let (upper, lower) = split_imm imm in
      if upper < 0 || upper > 1048575 then
        failwith (Printf.sprintf "LUI immediate out of range: %d" upper)
      else if lower < -2048 || lower > 2047 then
        failwith (Printf.sprintf "Addi immediate out of range: %d" lower)
      else
        Printf.sprintf "lui %s, %d\n    addi %s, %s, %d"
          (reg_to_string rd) upper
          (reg_to_string rd) (reg_to_string rd) lower
  | Lui (rd, imm) -> 
    if imm < 0 || imm > 1048575 then
      failwith (Printf.sprintf "LUI immediate out of range: %d" imm)
    else
      Printf.sprintf "lui %s, %d" (reg_to_string rd) imm
  | Mv (rd, rs) -> Printf.sprintf "mv %s, %s" (reg_to_string rd) (reg_to_string rs)
  | Nop -> "nop"

(* 标签和汇编代码项 *)
type label = string

type asm_item =
  | Label of label
  | Instruction of instruction
  | Comment of string
  | Directive of string

(* 将汇编项转换为字符串 *)
let asm_item_to_string item = match item with 
  | Instruction instr -> 
    let s = instr_to_string instr in
    if String.contains s '\n' then
      String.split_on_char '\n' s 
      |> List.map (fun line -> "    " ^ line)
      |> String.concat "\n"
    else
      "    " ^ s
  | Label l -> l ^ ":"
  | Directive d -> "    " ^ d
  | Comment c -> "    # " ^ c

(* 输出汇编代码 *)
let emit_asm_to_file filepath asm_items =
  let file = open_out filepath in
  List.iter
    (fun item ->
       output_string file (asm_item_to_string item);
       output_string file "\n")
    asm_items;
  close_out file

let emit_asm_to_stdout asm_items =
  List.iter (fun item -> print_endline (asm_item_to_string item)) asm_items

(* 代码生成上下文 - 严格遵循栈帧布局 *)
type codegen_context =
  { mutable label_counter : int
  ; mutable temp_counter : int
  ; mutable temp_regs : reg list  (* 可用临时寄存器栈 *)
  ; mutable stack_offset : int    (* 当前栈偏移，从fp开始计算（负值） *)
  ; mutable break_labels : string list
  ; mutable continue_labels : string list
  ; mutable local_vars : (string * int) list  (* 变量名到栈偏移的映射 *)
  ; func_name : string
  ; total_locals : int ref        (* 局部变量总数 *)
  ; frame_size : int              (* 当前函数的栈帧大小 *)
  }

(* 定义各区域大小常量（字节） *)
let ra_size = 4                     (* ra寄存器大小 *)
let fp_size = 4                     (* fp寄存器大小 *)
let s_regs_count = 12               (* s0-s11共12个寄存器 *)
let s_regs_save_area_size = s_regs_count * 4  (* 48字节 *)
let max_params = 64                 (* 最大参数数量 *)
let params_area_size = max_params * 4        (* 256字节 *)
let locals_start_offset = -320      (* 局部变量区起始偏移 *)

(* 创建上下文 - 初始化局部变量偏移为fp-320 *)
let create_context _symbol_table func_name frame_size =
  { label_counter = 0;
    temp_counter = 0;
    temp_regs = [];
    stack_offset = locals_start_offset;  (* 局部变量从fp-320开始 *)
    break_labels = [];
    continue_labels = [];
    local_vars = [];
    func_name = func_name;
    total_locals = ref 0;
    frame_size = frame_size
  }

(* 生成新标签 *)
let new_label ctx prefix =
  let label = Printf.sprintf "%s_%s%d" ctx.func_name prefix ctx.label_counter in
  ctx.label_counter <- ctx.label_counter + 1;
  label

(* 临时寄存器管理 *)
let get_temp_reg ctx =
  match ctx.temp_regs with
  | reg :: rest ->
      ctx.temp_regs <- rest;
      reg
  | [] ->
      let reg =
        match ctx.temp_counter mod 7 with
        | 0 -> T0
        | 1 -> T1
        | 2 -> T2
        | 3 -> T3
        | 4 -> T4
        | 5 -> T5
        | 6 -> T6
        | _ -> failwith "Invalid temp reg index"
      in
      ctx.temp_counter <- ctx.temp_counter + 1;
      reg

let release_temp_reg ctx reg =
  if not (List.mem reg ctx.temp_regs) then
    ctx.temp_regs <- reg :: ctx.temp_regs

(* 变量栈管理 - 局部变量从fp-320向下分配 *)
let add_local_var ctx name =
  ctx.stack_offset <- ctx.stack_offset - 4;  (* 每次分配4字节 *)
  ctx.local_vars <- (name, ctx.stack_offset) :: ctx.local_vars;
  ctx.total_locals := !(ctx.total_locals) + 1;
  ctx.stack_offset

(* 添加参数变量 - 强制分配到参数区 *)
let add_param_var ctx name index =
  let param_offset = -60 - (index * 4) in  (* 参数区从fp-60开始 *)
  ctx.local_vars <- (name, param_offset) :: ctx.local_vars;
  param_offset

let get_var_offset ctx name =
  match List.assoc_opt name ctx.local_vars with
  | Some offset -> offset
  | None -> 
      failwith (Printf.sprintf "Variable '%s' not found in scope" name)

(* 表达式生成逻辑 *)
let rec gen_expr ctx (expr : Ast.expr) : reg * instruction list =
  match expr with
  | Ast.Literal(IntLit n) ->
    let reg = get_temp_reg ctx in
    let instrs =
      if n >= -2048 && n <= 2047 then [ Li (reg, n) ]
      else
        let (upper, lower) = split_imm n in
        [ Lui (reg, upper); Addi (reg, reg, lower) ]
    in
    reg, instrs

  | Ast.Var id ->
    let reg = get_temp_reg ctx in
    let offset = get_var_offset ctx id in
    reg, [ Lw (reg, offset, Fp) ]  (* 从fp偏移加载变量 *)

  | Ast.Paren e -> gen_expr ctx e

  | Ast.UnOp (op, e) ->
    let e_reg, e_instrs = gen_expr ctx e in
    let result_reg = get_temp_reg ctx in
    let instrs =
      match op with
      | "+" -> e_instrs @ [ Mv (result_reg, e_reg) ]
      | "-" -> e_instrs @ [ Sub (result_reg, Zero, e_reg) ]
      | "!" -> e_instrs @ [ Sltiu (result_reg, e_reg, 1) ]
      | _ -> failwith (Printf.sprintf "Unknown unary operator: %s" op)
    in
    release_temp_reg ctx e_reg;
    result_reg, instrs

  | Ast.BinOp (e1, op, e2) ->
    let e1_reg, e1_instrs = gen_expr ctx e1 in
    let e2_reg, e2_instrs = gen_expr ctx e2 in
    let result_reg = get_temp_reg ctx in
    let op_instrs =
      match op with
      | "+" -> [ Add (result_reg, e1_reg, e2_reg) ]
      | "-" -> [ Sub (result_reg, e1_reg, e2_reg) ]
      | "*" -> [ Mul (result_reg, e1_reg, e2_reg) ]
      | "/" -> [ Div (result_reg, e1_reg, e2_reg) ]
      | "%" -> [ Rem (result_reg, e1_reg, e2_reg) ]
      | "==" ->  
          [ Sub (result_reg, e1_reg, e2_reg); 
            Sltiu (result_reg, result_reg, 1) ]
      | "!=" -> 
          [ Sub (result_reg, e1_reg, e2_reg); 
            Sltu (result_reg, Zero, result_reg) ]
      | "<" -> [ Slt (result_reg, e1_reg, e2_reg) ]
      | "<=" -> 
          [ Slt (T0, e2_reg, e1_reg); 
            Xori (result_reg, T0, 1) ]
      | ">" -> [ Slt (result_reg, e2_reg, e1_reg) ]
      | ">=" -> 
          [ Slt (T0, e1_reg, e2_reg); 
            Xori (result_reg, T0, 1) ]
      | "&&" ->
          [ Sltu (T0, Zero, e1_reg);
            Sltu (T1, Zero, e2_reg);
            And (result_reg, T0, T1) ]
      | "||" ->
          [ Or (T0, e1_reg, e2_reg);
            Sltu (result_reg, Zero, T0) ]
      | _ -> failwith (Printf.sprintf "Unknown binary operator: %s" op)
    in
    let instrs = e1_instrs @ e2_instrs @ op_instrs in
    release_temp_reg ctx e1_reg;
    release_temp_reg ctx e2_reg;
    result_reg, instrs

  | Ast.Call (fname, args) ->
    let result_reg = A0 in
    let num_args = List.length args in
    let num_stack_args = max 0 (num_args - 8) in
    let stack_args_space = num_stack_args * 4 in
    let saved_regs_space = 7 * 4 in  (* T0-T6保存区 *)
    
    (* 栈空间16字节对齐 *)
    let unaligned_space = stack_args_space + saved_regs_space in
    let stack_space = 
      if unaligned_space mod 16 = 0 then unaligned_space
      else (unaligned_space / 16 + 1) * 16
    in
    
    (* 保存临时寄存器 *)
    let save_instrs = 
      if stack_space > 0 then
        Addi (Sp, Sp, -stack_space) ::
        [ Sw (T0, saved_regs_space - 4, Sp);
          Sw (T1, saved_regs_space - 8, Sp);
          Sw (T2, saved_regs_space - 12, Sp);
          Sw (T3, saved_regs_space - 16, Sp);
          Sw (T4, saved_regs_space - 20, Sp);
          Sw (T5, saved_regs_space - 24, Sp);
          Sw (T6, saved_regs_space - 28, Sp)
        ]
      else []
    in
    
    (* 处理参数 *)
    let arg_instrs =
      List.mapi
        (fun i arg ->
           let arg_reg, arg_code = gen_expr ctx arg in
           let instrs =
             if i < 8 then
               let target_reg =
                 match i with
                 | 0 -> A0 | 1 -> A1 | 2 -> A2 | 3 -> A3
                 | 4 -> A4 | 5 -> A5 | 6 -> A6 | 7 -> A7
                 | _ -> failwith "Invalid register index"
               in
               arg_code @ [ Mv (target_reg, arg_reg) ]
             else
               let stack_pos = saved_regs_space + (i - 8) * 4 in
               arg_code @ [ Sw (arg_reg, stack_pos, Sp) ]
           in
           release_temp_reg ctx arg_reg;
           instrs)
        args
      |> List.flatten
    in
    
    (* 函数调用和恢复现场 *)
    let call_instr = [ Jal (Ra, fname) ] in
    let restore_instrs =
      if stack_space > 0 then
        [ Lw (T0, saved_regs_space - 4, Sp);
          Lw (T1, saved_regs_space - 8, Sp);
          Lw (T2, saved_regs_space - 12, Sp);
          Lw (T3, saved_regs_space - 16, Sp);
          Lw (T4, saved_regs_space - 20, Sp);
          Lw (T5, saved_regs_space - 24, Sp);
          Lw (T6, saved_regs_space - 28, Sp);
          Addi (Sp, Sp, stack_space)
        ]
      else []
    in
    
    result_reg, save_instrs @ arg_instrs @ call_instr @ restore_instrs   

(* 函数序言 - 分配栈帧并保存ra和fp *)
let gen_prologue_instrs frame_size =
  [ Instruction(Addi (Sp, Sp, -frame_size));  (* 分配栈帧 *)
    Instruction(Sw (Ra, frame_size - 4, Sp));  (* ra保存到fp-4 (sp+frame_size-4) *)
    Instruction(Sw (Fp, frame_size - 8, Sp));  (* fp保存到fp-8 (sp+frame_size-8) *)
    Instruction(Addi (Fp, Sp, frame_size))     (* fp = sp + frame_size (栈帧顶部) *)
  ]

(* 函数尾声 - 恢复ra和fp并释放栈帧 *)
let gen_epilogue_instrs frame_size =
  [ Lw (Ra, frame_size - 4, Sp);  (* 恢复ra *)
    Lw (Fp, frame_size - 8, Sp);  (* 恢复fp *)
    Addi (Sp, Sp, frame_size);    (* 释放栈帧 *)
    Ret ]                         (* 返回 *)

(* 保存s0-s11寄存器（fp-12到fp-56） *)
let gen_save_s_regs =
  [ Instruction (Sw (S0, -12, Fp));    (* s0: fp-12 *)
    Instruction (Sw (S1, -16, Fp));    (* s1: fp-16 *)
    Instruction (Sw (S2, -20, Fp));    (* s2: fp-20 *)
    Instruction (Sw (S3, -24, Fp));    (* s3: fp-24 *)
    Instruction (Sw (S4, -28, Fp));    (* s4: fp-28 *)
    Instruction (Sw (S5, -32, Fp));    (* s5: fp-32 *)
    Instruction (Sw (S6, -36, Fp));    (* s6: fp-36 *)
    Instruction (Sw (S7, -40, Fp));    (* s7: fp-40 *)
    Instruction (Sw (S8, -44, Fp));    (* s8: fp-44 *)
    Instruction (Sw (S9, -48, Fp));    (* s9: fp-48 *)
    Instruction (Sw (S10, -52, Fp));   (* s10: fp-52 *)
    Instruction (Sw (S11, -56, Fp)) ]  (* s11: fp-56 *)

(* 恢复s0-s11寄存器（逆序恢复） *)
let gen_restore_s_regs =
  [ Lw (S11, -56, Fp);   (* 恢复s11: fp-56 *)
    Lw (S10, -52, Fp);   (* 恢复s10: fp-52 *)
    Lw (S9, -48, Fp);    (* 恢复s9: fp-48 *)
    Lw (S8, -44, Fp);    (* 恢复s8: fp-44 *)
    Lw (S7, -40, Fp);    (* 恢复s7: fp-40 *)
    Lw (S6, -36, Fp);    (* 恢复s6: fp-36 *)
    Lw (S5, -32, Fp);    (* 恢复s5: fp-32 *)
    Lw (S4, -28, Fp);    (* 恢复s4: fp-28 *)
    Lw (S3, -24, Fp);    (* 恢复s3: fp-24 *)
    Lw (S2, -20, Fp);    (* 恢复s2: fp-20 *)
    Lw (S1, -16, Fp);    (* 恢复s1: fp-16 *)
    Lw (S0, -12, Fp) ]   (* 恢复s0: fp-12 *)

(* 语句生成逻辑 *)
let rec gen_stmt ctx (stmt : Ast.stmt) : asm_item list =
  match stmt with
  | Ast.Empty -> []
  
  | Ast.ExprStmt e ->
    let reg, instrs = gen_expr ctx e in
    release_temp_reg ctx reg;
    List.map (fun i -> Instruction i) instrs
  
  | Ast.Block stmts ->
    let old_vars = ctx.local_vars in
    let old_offset = ctx.stack_offset in
    let old_total_locals = !(ctx.total_locals) in
    let old_temp_regs = ctx.temp_regs in
    let items = List.map (gen_stmt ctx) stmts |> List.flatten in
    ctx.local_vars <- old_vars;
    ctx.stack_offset <- old_offset;
    ctx.total_locals := old_total_locals;
    ctx.temp_regs <- old_temp_regs;
    items
  
  | Ast.Return (Some e) ->
    let e_reg, e_instrs = gen_expr ctx e in
    let all_instrs = 
      e_instrs @ [ Mv (A0, e_reg) ] @ gen_restore_s_regs @ gen_epilogue_instrs ctx.frame_size 
    in
    release_temp_reg ctx e_reg;
    List.map (fun i -> Instruction i) all_instrs
  
  | Ast.Return None -> 
    List.map (fun i -> Instruction i) 
      ([ Li (A0, 0) ] @ gen_restore_s_regs @ gen_epilogue_instrs ctx.frame_size)
  
  | Ast.If (cond, then_stmt, else_stmt) ->
    let cond_reg, cond_instrs = gen_expr ctx cond in
    let else_label = new_label ctx "else" in
    let end_label = new_label ctx "endif" in
    let then_items = gen_stmt ctx then_stmt in
    let else_items = Option.value ~default:[] (Option.map (gen_stmt ctx) else_stmt) in
    release_temp_reg ctx cond_reg;
    List.map (fun i -> Instruction i) cond_instrs
    @ [ Instruction (Beq (cond_reg, Zero, else_label)) ]
    @ then_items
    @ [ Instruction (J end_label); Label else_label ]
    @ else_items
    @ [ Label end_label ]
  
  | Ast.While (cond, body) ->
    let loop_label = new_label ctx "loop" in
    let end_label = new_label ctx "endloop" in
    ctx.break_labels <- end_label :: ctx.break_labels;
    ctx.continue_labels <- loop_label :: ctx.continue_labels;
    let cond_reg, cond_instrs = gen_expr ctx cond in
    let body_items = gen_stmt ctx body in
    ctx.break_labels <- List.tl ctx.break_labels;
    ctx.continue_labels <- List.tl ctx.continue_labels;
    release_temp_reg ctx cond_reg;
    [ Label loop_label ]
    @ List.map (fun i -> Instruction i) cond_instrs
    @ [ Instruction (Beq (cond_reg, Zero, end_label)) ]
    @ body_items
    @ [ Instruction (J loop_label); Label end_label ]
  
  | Ast.Break ->
    (match ctx.break_labels with
     | label :: _ -> [ Instruction (J label) ]
     | [] -> failwith "Break outside loop")
  
  | Ast.Continue ->
    (match ctx.continue_labels with
     | label :: _ -> [ Instruction (J label) ]
     | [] -> failwith "Continue outside loop")
  
  | Ast.Decl (name, e) ->
    let offset = add_local_var ctx name in  (* 分配到局部变量区 *)
    let e_reg, e_instrs = gen_expr ctx e in
    let all_instrs = e_instrs @ [ Sw (e_reg, offset, Fp) ] in
    release_temp_reg ctx e_reg;
    List.map (fun i -> Instruction i) all_instrs
  
  | Ast.Assign (name, e) ->
    let offset = get_var_offset ctx name in
    let e_reg, e_instrs = gen_expr ctx e in
    let all_instrs = e_instrs @ [ Sw (e_reg, offset, Fp) ] in
    release_temp_reg ctx e_reg;
    List.map (fun i -> Instruction i) all_instrs

(* 栈帧大小计算 - 严格按照指定布局 *)
let calculate_frame_size (func_def : Ast.func_def) =
  (* 统计局部变量数量 *)
  let rec count_decls_in_stmt (stmt:Ast.stmt) =
    match stmt with
    | Decl _ -> 1
    | Block stmts -> List.fold_left (fun acc s -> acc + count_decls_in_stmt s) 0 stmts
    | If (_, s1, Some s2) -> count_decls_in_stmt s1 + count_decls_in_stmt s2
    | If (_, s1, None) -> count_decls_in_stmt s1
    | While (_, s) -> count_decls_in_stmt s
    | _ -> 0
  in
  let num_locals = List.fold_left (fun acc stmt -> acc + count_decls_in_stmt stmt) 0 func_def.body in
  let num_params = List.length func_def.params in
  
  (* 各区域大小总和（字节） *)
  let ra_fp_space = ra_size + fp_size in          (* 8字节：ra + fp *)
  let s_regs_space = s_regs_save_area_size in     (* 48字节：s0-s11 *)
  let params_space = max params_area_size (num_params * 4) in  (* 至少256字节 *)
  let locals_space = if num_locals = 0 then 0 else (locals_start_offset - (locals_start_offset - num_locals * 4)) in
  
  (* 总需求空间 *)
  let required_space = ra_fp_space + s_regs_space + params_space + locals_space in
  
  (* 16字节对齐 *)
  let aligned_space = 
    if required_space mod 16 = 0 then required_space
    else required_space + (16 - required_space mod 16)
  in
  
  (* 确保最小栈帧能容纳所有固定区域 *)
  max aligned_space (ra_fp_space + s_regs_space + params_space)

(* 函数生成 - 按指定栈帧布局处理参数和局部变量 *)
let gen_function symbol_table (func_def : Ast.func_def) : asm_item list =
  let frame_size = calculate_frame_size func_def in
  let ctx = create_context symbol_table func_def.fname frame_size in
  
  (* 函数序言：分配栈帧并设置fp *)
  let prologue = gen_prologue_instrs frame_size in
  
  (* 保存s0-s11寄存器到fp-12至fp-56 *)
  let save_s_regs = gen_save_s_regs in
  
  (* 处理参数：分配到fp-60至fp-316的参数区 *)
  let param_instrs =
    List.mapi
      (fun i { Ast.pname = name; _ } ->
       (* 为参数分配到参数区固定位置 *)
       let param_offset = add_param_var ctx name i in
       let instr =
         if i < 8 then
           (* 前8个参数从A0-A7寄存器保存 *)
           let arg_reg =
             match i with
             | 0 -> A0 | 1 -> A1 | 2 -> A2 | 3 -> A3
             | 4 -> A4 | 5 -> A5 | 6 -> A6 | 7 -> A7
             | _ -> failwith "Invalid register index"
           in
           [ Instruction (Sw (arg_reg, param_offset, Fp)) ]
         else
           (* 第9个及以后参数从调用者栈帧加载（sp+16开始） *)
           let stack_arg_offset = 16 + ((i - 8) * 4) in
           [ Instruction (Lw (T0, stack_arg_offset, Sp));  (* 从栈加载 *)
             Instruction (Sw (T0, param_offset, Fp)) ]     (* 保存到参数区 *)
       in
       instr)
      func_def.params
    |> List.flatten
  in
  
  (* 生成函数体 *)
  let body_items =
    func_def.body
    |> List.map (fun s -> gen_stmt ctx s)
    |> List.flatten
  in
  
  (* 检查是否有显式return *)
  let has_explicit_return =
    List.exists
      (function
        | Instruction Ret -> true
        | _ -> false)
      body_items
  in
  
  (* 隐式return处理 *)
  let epilogue =
    if has_explicit_return then []
    else List.map (fun i -> Instruction i) (gen_restore_s_regs @ gen_epilogue_instrs frame_size)
  in
  
  (* 组合所有部分 *)
  [ Label func_def.fname; Comment ("Function: " ^ func_def.fname) ]
  @ prologue 
  @ save_s_regs 
  @ param_instrs 
  @ body_items 
  @ epilogue

(* 程序生成 *)
let gen_program symbol_table (program : Ast.program) =
  let header =
    [ Directive ".text"; 
      Directive ".globl main"; 
      Directive ".align 2";
      Comment "Generated by RISC-V Code Generator (Strict Stack Layout)" ]
  in
  let func_asm_items =
    List.map (gen_function symbol_table) program
    |> List.flatten
  in
  header @ func_asm_items



let compile_to_riscv symbol_table program =
  let asm_items = gen_program symbol_table program in
  List.iter
    (fun item -> print_endline (asm_item_to_string item))
    asm_items
    


