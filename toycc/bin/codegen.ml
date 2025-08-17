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
  | S0  (* s0寄存器 *)
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
  | S0 -> "s0"
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

(* 代码生成上下文 - 增加函数调用结果保存区管理 *)
type codegen_context =
  { mutable label_counter : int
  ; mutable temp_counter : int
  ; mutable temp_regs : reg list  (* 可用临时寄存器栈 *)
  ; mutable stack_offset : int    (* 当前栈偏移，从fp开始计算（负值） *)
  ; mutable call_result_offset : int  (* 函数调用结果保存区当前偏移 *)
  ; mutable break_labels : string list
  ; mutable continue_labels : string list
  ; mutable local_vars : (string * int) list  (* 变量名到栈偏移的映射 *)
  ; func_name : string
  ; total_locals : int ref        (* 局部变量总数 *)
  ; frame_size : int              (* 当前函数的栈帧大小 *)
  ; call_results_area_size : int  (* 函数调用结果保存区大小 *)
  ; ret_val_offset : int          (* 函数返回值在栈帧中的偏移 (相对fp) *)
  ; stack_args_offset : int       (* 栈参数区起始偏移 (相对sp) *)
  }

(* 定义需要保存的临时寄存器列表 - 现在由被调用者保存 *)
let temp_regs_to_save = [T0; T1; T2; T3; T4; T5; T6]
(* 计算需要的保存空间（每个寄存器4字节） *)
let temp_regs_save_size = List.length temp_regs_to_save * 4

(* 定义各区域大小常量（字节） - 从高地址到低地址布局:
   sp+0 ~ sp+255: 栈参数区 (固定256字节，用于传递参数到其他函数)
   ...: 临时寄存器保存区 (28字节，用于保存T0-T6，现在由被调用者管理)
   ...: 函数调用结果保存区
   ...: 局部变量区
   ...: 返回值保存区 (4字节)
   ...: 参数区
   ...: ra和fp保存区
*)
let ra_offset = -4                  (* ra寄存器位置: fp-4 *)
let fp_offset = -8                  (* 旧fp寄存器位置: fp-8 *)
let params_area_size = 256          (* 参数区固定256字节 *)
let params_start_offset = -12       (* 参数区起始偏移: fp-12 *)
let params_end_offset = params_start_offset - params_area_size  (* fp-268 *)
let ret_val_area_size = 4           (* 返回值保存区4字节 *)
let stack_args_area_size = 256      (* 栈参数区固定256字节，从sp+0开始 *)
let call_results_area_size = 128   (* 函数调用结果保存区1024字节 *)

(* 创建上下文 - 初始化函数调用结果保存区 *)
let create_context _symbol_table func_name frame_size call_results_area_size 
    ret_val_offset stack_args_offset =
  { label_counter = 0;
    temp_counter = 0;
    temp_regs = [];
    (* 调整栈偏移，预留临时寄存器保存区（现在由被调用者管理） *)
    stack_offset = params_end_offset - call_results_area_size - temp_regs_save_size;
    (* 调整调用结果区偏移 *)
    call_result_offset = params_end_offset - 4 - temp_regs_save_size;
    break_labels = [];
    continue_labels = [];
    local_vars = [];
    func_name = func_name;
    total_locals = ref 0;
    frame_size = frame_size;
    call_results_area_size = call_results_area_size;
    ret_val_offset = ret_val_offset;   (* 返回值在栈帧中的偏移 *)
    stack_args_offset = stack_args_offset
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

(* 为函数调用结果分配栈空间 *)
let alloc_call_result ctx =
  let offset = ctx.call_result_offset in
  ctx.call_result_offset <- ctx.call_result_offset - 4;
  if ctx.call_result_offset < (params_end_offset - ctx.call_results_area_size - temp_regs_save_size) then
    failwith "Exceeded call results area size";
  offset

(* 变量栈管理 - 局部变量从函数调用结果区下方开始分配 *)
let add_local_var ctx name =
  ctx.local_vars <- (name, ctx.stack_offset) :: ctx.local_vars;
  ctx.stack_offset <- ctx.stack_offset - 4;  (* 每次分配4字节，向低地址增长 *)
  ctx.total_locals := !(ctx.total_locals) + 1;
  ctx.stack_offset + 4

(* 添加参数变量 - 强制分配到参数区(fp-12开始) *)
let add_param_var ctx name index =
  let param_offset = params_start_offset - (index * 4) in  (* 参数区从fp-12开始 *)
  if param_offset < params_end_offset then  (* 确保不超出参数区范围 *)
    failwith (Printf.sprintf "Too many parameters (max %d)" (params_area_size / 4));
  ctx.local_vars <- (name, param_offset) :: ctx.local_vars;
  param_offset

let get_var_offset ctx name =
  match List.assoc_opt name ctx.local_vars with
  | Some offset -> offset
  | None -> 
      failwith (Printf.sprintf "Variable '%s' not found in scope" name)

(* 生成保存临时寄存器的指令 - 现在在函数序言中调用 *)
let save_temp_regs _ =
  List.mapi (fun i reg ->
    (* 计算保存位置，从参数区下方开始分配空间 *)
    let offset = params_end_offset - (i * 4) - 4 in
    Sw (reg, offset, Fp)
  ) temp_regs_to_save

(* 生成恢复临时寄存器的指令 - 现在在函数尾声中调用 *)
let restore_temp_regs _ =
  List.mapi (fun i reg ->
    (* 使用与保存时相同的偏移位置 *)
    let offset = params_end_offset - (i * 4) - 4 in
    Lw (reg, offset, Fp)
  ) temp_regs_to_save

(* 表达式生成逻辑 - 修改为被调用者保存临时寄存器 *)
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
    (* 1. 处理参数：前8个用寄存器，其余用栈参数区(sp+0开始)
       不再需要保存临时寄存器，因为现在由被调用者负责 *)
    let arg_instrs =
      List.mapi
        (fun i arg ->
           let arg_reg, arg_code = gen_expr ctx arg in
           let instrs =
             if i < 8 then
               (* 前8个参数使用a0-a7寄存器 *)
               let target_reg =
                 match i with
                 | 0 -> A0 | 1 -> A1 | 2 -> A2 | 3 -> A3
                 | 4 -> A4 | 5 -> A5 | 6 -> A6 | 7 -> A7
                 | _ -> failwith "Invalid register index"
               in
               arg_code @ [ Mv (target_reg, arg_reg) ]
             else
               (* 第9个及以后参数放在栈参数区(sp+0开始) *)
               let stack_arg_offset = ctx.stack_args_offset + ((i - 8) * 4) in
               arg_code @ [ Sw (arg_reg, stack_arg_offset, Sp) ]  (* 使用Sp作为基地址 *)
           in
           release_temp_reg ctx arg_reg;
           instrs)
        args
      |> List.flatten
    in
  
    (* 2. 调用者分配栈空间保存函数调用结果 *)
    let result_offset = alloc_call_result ctx in
    let result_reg = get_temp_reg ctx in
    
    (* 3. 函数调用并由调用者将返回值(A0)保存到栈上 *)
    let call_instr = [ 
      Jal (Ra, fname); 
      Sw (A0, result_offset, Fp);  (* 调用者保存结果到栈 *)
      Lw (result_reg, result_offset, Fp)  (* 从栈加载到结果寄存器 *)
    ] in
    
    (* 4. 返回结果寄存器和完整指令序列 - 不再需要恢复临时寄存器 *)
    result_reg, arg_instrs @ call_instr   

(* 函数序言 - 分配栈帧并保存ra、fp和临时寄存器(T0-T6) *)
let gen_prologue_instrs ctx frame_size =
  (* 首先分配整个栈帧 *)
  let prologue = [ 
    Addi (Sp, Sp, -frame_size);  (* 一次性分配整个栈帧 *)
    Sw (Ra, frame_size-4, Sp);   (* ra保存到fp-4 *)
    Sw (Fp, frame_size-8, Sp);   (* fp保存到fp-8 *)  
    Addi (Fp, Sp, frame_size)    (* fp = sp + frame_size（指向调用前的sp） *)
  ] in
  
  (* 被调用者保存临时寄存器T0-T6 *)
  prologue @ save_temp_regs ctx

(* 函数尾声 - 恢复临时寄存器(T0-T6)、ra和fp并释放栈帧 *)
let gen_epilogue_instrs ctx frame_size =
  (* 被调用者恢复临时寄存器T0-T6，然后恢复ra和fp *)
  restore_temp_regs ctx @ [
    Lw (Ra, frame_size-4, Sp);   (* 恢复ra *)
    Lw (Fp, frame_size-8, Sp);   (* 恢复fp *)
    Addi (Sp, Sp, frame_size);   (* 释放整个栈帧 *)
    Ret ]                        (* 返回 *)

(* 语句生成逻辑 - 适应被调用者保存临时寄存器的策略 *)
let rec gen_stmt ctx (stmt : Ast.stmt) : asm_item list =
  match stmt with
  | Ast.Empty -> []
  
  | Ast.ExprStmt e ->
    let reg, instrs = gen_expr ctx e in
    (* 对于表达式语句，使用后释放寄存器但保留计算结果 *)
    let items = List.map (fun i -> Instruction i) instrs in
    release_temp_reg ctx reg;
    items
  
  | Ast.Block stmts ->
    let old_vars = ctx.local_vars in
    let old_offset = ctx.stack_offset in
    let old_call_offset = ctx.call_result_offset in
    let old_total_locals = !(ctx.total_locals) in
    let old_temp_regs = ctx.temp_regs in
    let items = List.map (gen_stmt ctx) stmts |> List.flatten in
    ctx.local_vars <- old_vars;
    ctx.stack_offset <- old_offset;
    ctx.call_result_offset <- old_call_offset;
    ctx.total_locals := old_total_locals;
    ctx.temp_regs <- old_temp_regs;
    items
  
  | Ast.Return (Some e) ->
    let e_reg, e_instrs = gen_expr ctx e in
    (* 函数返回值放入A0 *)
    let all_instrs = 
      e_instrs @ [ Mv (A0, e_reg) ] @ gen_epilogue_instrs ctx ctx.frame_size 
    in
    release_temp_reg ctx e_reg;
    List.map (fun i -> Instruction i) all_instrs
  
  | Ast.Return None -> 
    List.map (fun i -> Instruction i) 
      ([ Li (A0, 0) ] @ gen_epilogue_instrs ctx ctx.frame_size)
  
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
    (* 将函数调用结果存储到变量中 *)
    let all_instrs = e_instrs @ [ Sw (e_reg, offset, Fp) ] in
    release_temp_reg ctx e_reg;
    List.map (fun i -> Instruction i) all_instrs
  
  | Ast.Assign (name, e) ->
    let offset = get_var_offset ctx name in
    let e_reg, e_instrs = gen_expr ctx e in
    (* 将函数调用结果赋值给变量 *)
    let all_instrs = e_instrs @ [ Sw (e_reg, offset, Fp) ] in
    release_temp_reg ctx e_reg;
    List.map (fun i -> Instruction i) all_instrs

(* 栈帧大小计算 - 临时寄存器保存区仍需计算但由被调用者管理 *)
let calculate_frame_size_and_offsets (func_def : Ast.func_def) =
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
  let locals_area_size = num_locals * 4 in
  
  (* 栈帧布局从高地址到低地址:
     1. 栈参数区: sp+0 到 sp+255 (256字节) - 用于传递参数到其他函数
     2. 临时寄存器保存区: 28字节 (T0-T6共7个寄存器，由被调用者管理)
     3. 函数调用结果保存区: 1024字节
     4. 局部变量区: locals_area_size 字节
     5. 返回值保存区: 4字节
     6. 参数区: 256字节
     7. ra和fp保存区: 8字节
  *)
  
  (* 计算总栈帧大小，包含临时寄存器保存区 *)
  let frame_size = 
    stack_args_area_size +        (* 256字节 *)
    temp_regs_save_size +         (* 临时寄存器保存区28字节 *)
    call_results_area_size +      (* 1024字节 *)
    locals_area_size +            (* 局部变量大小 *)
    ret_val_area_size +           (* 返回值保存区4字节 *)
    params_area_size +            (* 256字节 *)
    8                             (* ra和fp (2个寄存器) *)
  in
  
  (* 计算各区域偏移 (相对fp) *)
  (* fp = sp + frame_size，所以sp = fp - frame_size *)
  let stack_args_offset = 0 in  (* 栈参数区从sp+0开始 *)
  
  (* 返回值保存区相对fp的偏移 *)
  let ret_val_offset = -(locals_area_size + params_area_size + 8) in
  
  (frame_size, call_results_area_size, ret_val_offset, stack_args_offset)

(* 函数生成 - 支持被调用者保存临时寄存器T0-T6 *)
let gen_function symbol_table (func_def : Ast.func_def) : asm_item list =
  let (frame_size, call_results_area_size, ret_val_offset, stack_args_offset) = 
    calculate_frame_size_and_offsets func_def in
  let ctx = create_context symbol_table func_def.fname frame_size 
      call_results_area_size ret_val_offset stack_args_offset in
  
  (* 函数序言：一次性分配整个栈帧、设置fp并保存临时寄存器 *)
  let prologue = List.map (fun i -> Instruction i) (gen_prologue_instrs ctx frame_size) in
  
  (* 处理参数：使用Fp访问栈参数 *)
  let param_instrs =
    List.mapi
      (fun i { Ast.pname = name; _ } ->
       let param_offset = add_param_var ctx name i in
       let instr =
         if i < 8 then
           (* 前8个参数从A0-A7寄存器保存到参数区 *)
           let arg_reg =
             match i with
             | 0 -> A0 | 1 -> A1 | 2 -> A2 | 3 -> A3
             | 4 -> A4 | 5 -> A5 | 6 -> A6 | 7 -> A7
             | _ -> failwith "Invalid register index"
           in
           [ Instruction (Sw (arg_reg, param_offset, Fp)) ]
         else
           (* 第9+个参数从调用者栈参数区加载 (sp+0开始) *)
           let stack_arg_offset = ((i - 8) * 4) in  (* sp+0开始的偏移 *)
           [ Instruction (Lw (T0, stack_arg_offset, Fp));  (* 使用Sp访问调用者栈参数 *)(*_______________________________________________________________________________________________-*)
             Instruction (Sw (T0, param_offset, Fp)) ]     (* 保存到当前函数参数区 *)
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
    List.exists (function Instruction Ret -> true | _ -> false) body_items
  in
  
  (* 隐式return处理 *)
  let epilogue =
    if has_explicit_return then []
    else List.map (fun i -> Instruction i) (gen_epilogue_instrs ctx frame_size)
  in
  
  (* 组合所有部分 *)
  [ Label func_def.fname; Comment ("Function: " ^ func_def.fname);
    Comment ("Frame size: " ^ string_of_int frame_size ^ " bytes");
    Comment ("Call results area size: " ^ string_of_int call_results_area_size ^ " bytes");
    Comment ("Temp registers save area: " ^ string_of_int temp_regs_save_size ^ " bytes (被调用者管理)") ]
  @ prologue 
  @ param_instrs 
  @ body_items 
  @ epilogue

(* 程序生成 *)
let gen_program symbol_table (program : Ast.program) =
  [ Directive ".text"; 
    Directive ".globl main"; 
    Directive ".align 2";
    Comment "Generated by RISC-V Code Generator (被调用者保存T0-T6寄存器)";
    Comment "Stack layout from high to low address:";
    Comment "1. 栈参数区: sp+0 ~ sp+255 (256字节，用于传递参数到其他函数)";
    Comment "2. 临时寄存器保存区: 28字节 (保存T0-T6，由被调用者管理)";
    Comment "3. 函数调用结果保存区: 1024字节";
    Comment "4. 局部变量区";
    Comment "5. 返回值保存区 (4字节)";
    Comment "6. 参数区 (256字节)";
    Comment "7. ra (fp-4) 和 旧fp (fp-8)";
    Comment "函数进入时自动保存T0-T6，返回前恢复" ]
  @ List.flatten (List.map (gen_function symbol_table) program)




let compile_to_riscv symbol_table program =
  let asm_items = gen_program symbol_table program in
  List.iter
    (fun item -> print_endline (asm_item_to_string item))
    asm_items
    









