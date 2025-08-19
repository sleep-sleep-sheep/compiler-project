open Ast

(* RISC-V 32位寄存器定义 *)
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
  | S0  
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
  | Lw of reg * reg * int  (* 修改为 reg * base_reg * offset 形式 *)
  | Sw of reg * reg * int  (* 修改为 reg * base_reg * offset 形式 *)
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
  | Lw (rd, rs1, offset) ->
    Printf.sprintf "lw %s, %d(%s)" (reg_to_string rd) offset (reg_to_string rs1)
  | Sw (rs2, rs1, offset) ->
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

(* 寄存器溢出栈帧项 - 记录被溢出到栈的寄存器及其位置 *)
type spilled_reg = {
  reg: reg;
  offset: int;  (* 相对于fp的偏移 *)
}

(* 代码生成上下文 - 增加寄存器溢出管理 *)
type codegen_context =
  { mutable label_counter : int
  ; mutable temp_counter : int
  ; used_regs : reg list ref  (* 当前正在使用的寄存器 *)
  ; free_regs : reg list ref  (* 可用的临时寄存器 *)
  ; spill_stack : spilled_reg list ref  (* 被溢出到栈的寄存器 *)
  ; mutable spill_offset : int  (* 溢出区域当前偏移，从fp开始计算（负值） *)
  ; initial_spill_offset : int  (* 溢出区域初始偏移，用于计算使用量 *)
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
  ; spill_area_size : int         (* 寄存器溢出区大小 *)
  }

(* 定义临时寄存器列表 - 被调用者保存 *)
let temp_regs = [T0; T1; T2; T3; T4; T5; T6]
let num_temp_regs = List.length temp_regs
(* 计算需要的保存空间（每个寄存器4字节） *)
let temp_regs_save_size = num_temp_regs * 4

(* 定义各区域大小常量（字节） - 从高地址到低地址布局 *)
let ra_offset = -4                  (* ra寄存器位置: fp-4 *)
let fp_offset = -8                  (* 旧fp寄存器位置: fp-8 *)
let params_area_size = 256          (* 参数区固定256字节 *)
let params_start_offset = -12       (* 参数区起始偏移: fp-12 *)
let params_end_offset = params_start_offset - params_area_size  (* fp-268 *)
let ret_val_area_size = 4           (* 返回值保存区4字节 *)
let stack_args_area_size = 256      (* 栈参数区固定256字节 *)
let call_results_area_size = 512     (* 函数调用结果保存区 *)
let spill_area_size = 256     (* 优化：16KB溢出区，足够大多数情况 *)

(* 创建上下文 - 初始化寄存器溢出管理 *)
let create_context _symbol_table func_name frame_size call_results_area_size 
    ret_val_offset stack_args_offset spill_area_size =
  let initial_spill_offset = params_end_offset - spill_area_size in
  { label_counter = 0;
    temp_counter = 0;
    used_regs = ref [];
    free_regs = ref temp_regs;  (* 初始时所有临时寄存器都可用 *)
    spill_stack = ref [];
    spill_offset = initial_spill_offset;  (* 溢出区从参数区下方开始 *)
    initial_spill_offset;  (* 保存初始偏移用于计算使用量 *)
    stack_offset = params_end_offset - spill_area_size - call_results_area_size - temp_regs_save_size;
    call_result_offset = params_end_offset - spill_area_size - 4 - temp_regs_save_size;
    break_labels = [];
    continue_labels = [];
    local_vars = [];
    func_name = func_name;
    total_locals = ref 0;
    frame_size = frame_size;
    call_results_area_size = call_results_area_size;
    ret_val_offset = ret_val_offset;
    stack_args_offset = stack_args_offset;
    spill_area_size = spill_area_size;
  }

(* 生成新标签 *)
let new_label ctx prefix =
  let label = Printf.sprintf "%s_%s%d" ctx.func_name prefix ctx.label_counter in
  ctx.label_counter <- ctx.label_counter + 1;
  label

(* 优化：优先选择最近最少使用的寄存器进行溢出 *)
let get_least_recently_used_reg used_regs =
  match !used_regs with
  | [] -> None
  | regs -> Some (List.hd (List.rev regs))  (* 假设最后添加的是最近使用的 *)

(* 临时寄存器管理 - 带溢出机制优化 *)
let spill_one_register ctx =
  (* 优化：选择最近最少使用的寄存器溢出 *)
  let reg_to_spill = 
    match get_least_recently_used_reg ctx.used_regs with
    | Some reg -> reg
    | None -> failwith "No registers to spill"
  in
  
  (* 计算已使用的溢出空间 *)
  let used_spill_space = ctx.initial_spill_offset - ctx.spill_offset in
  if used_spill_space + 4 > ctx.spill_area_size then
    failwith (Printf.sprintf "Exceeded spill area size (used: %d, total: %d)" 
                used_spill_space ctx.spill_area_size);
  
  (* 保存寄存器值到栈 *)
  let offset = ctx.spill_offset in
  ctx.spill_offset <- ctx.spill_offset - 4;
  let spill_instr = Sw (reg_to_spill, Fp, offset) in
  
  (* 更新上下文状态 *)
  ctx.used_regs := List.filter (fun r -> r <> reg_to_spill) !(ctx.used_regs);
  ctx.free_regs := reg_to_spill :: !(ctx.free_regs);
  ctx.spill_stack := {reg = reg_to_spill; offset} :: !(ctx.spill_stack);
  
  spill_instr

let get_temp_reg ctx =
  let rec get_reg () =
    match !(ctx.free_regs) with
    | reg :: rest ->
      (* 有可用寄存器，直接使用 *)
      ctx.free_regs := rest;
      (* 优化：将新使用的寄存器添加到列表末尾表示最近使用 *)
      ctx.used_regs := !(ctx.used_regs) @ [reg];
      reg, []
    | [] ->
      (* 没有可用寄存器，溢出一个到栈中 *)
      let spill_instr = spill_one_register ctx in
      let reg, instrs = get_reg ()  (* 递归获取现在可用的寄存器 *)
      in reg, spill_instr :: instrs
  in
  get_reg ()

let release_temp_reg ctx reg =
  if List.mem reg !(ctx.used_regs) then begin
    (* 从used_regs移除，添加到free_regs *)
    ctx.used_regs := List.filter (fun r -> r <> reg) !(ctx.used_regs);
    ctx.free_regs := reg :: !(ctx.free_regs);
    
    (* 尝试从溢出栈恢复寄存器 - 优化：只在需要时恢复 *)
    let try_restore () =
      match !(ctx.spill_stack) with
      | spill :: spills when List.length !(ctx.free_regs) > 2 ->
        (* 当有多个空闲寄存器时才恢复，减少频繁交换 *)
        let restore_instr = Lw (spill.reg, Fp, spill.offset) in
        ctx.spill_stack := spills;
        ctx.used_regs := !(ctx.used_regs) @ [spill.reg];
        ctx.free_regs := List.filter (fun r -> r <> spill.reg) !(ctx.free_regs);
        [restore_instr]
      | _ -> []
    in
    try_restore ()
  end else
    []

(* 为函数调用结果分配栈空间 *)
let alloc_call_result ctx =
  let offset = ctx.call_result_offset in
  ctx.call_result_offset <- ctx.call_result_offset - 4;
  if ctx.call_result_offset < (params_end_offset - ctx.spill_area_size - ctx.call_results_area_size - temp_regs_save_size) then
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

(* 生成保存临时寄存器的指令 *)
let save_temp_regs ctx =
  List.mapi (fun i reg ->
    (* 计算保存位置，从参数区下方开始分配空间 *)
    let offset = ctx.initial_spill_offset - (i * 4) - 4 in
    Sw (reg, Fp, offset)
  ) temp_regs

(* 生成恢复临时寄存器的指令 *)
let restore_temp_regs ctx =
  List.mapi (fun i reg ->
    (* 使用与保存时相同的偏移位置 *)
    let offset = ctx.initial_spill_offset - (i * 4) - 4 in
    Lw (reg, Fp, offset)
  ) temp_regs

(* 表达式生成逻辑 - 带寄存器溢出处理优化 *)
let rec gen_expr ctx (expr : Ast.expr) : reg * instruction list =
  match expr with
  | Ast.Literal(IntLit n) ->
    let reg, spill_instrs = get_temp_reg ctx in
    let instrs =
      if n >= -2048 && n <= 2047 then [ Li (reg, n) ]
      else
        let (upper, lower) = split_imm n in
        [ Lui (reg, upper); Addi (reg, reg, lower) ]
    in
    reg, spill_instrs @ instrs

  | Ast.Var id ->
    let reg, spill_instrs = get_temp_reg ctx in
    let offset = get_var_offset ctx id in
    reg, spill_instrs @ [ Lw (reg, Fp, offset) ]  (* 从fp偏移加载变量 *)

  | Ast.Paren e -> gen_expr ctx e

  | Ast.UnOp (op, e) ->
    let e_reg, e_instrs = gen_expr ctx e in
    let result_reg, spill_instrs = get_temp_reg ctx in
    let op_instrs =
      match op with
      | "+" -> [ Mv (result_reg, e_reg) ]
      | "-" -> [ Sub (result_reg, Zero, e_reg) ]
      | "!" -> [ Sltiu (result_reg, e_reg, 1) ]
      | _ -> failwith (Printf.sprintf "Unknown unary operator: %s" op)
    in
    let release_instrs = release_temp_reg ctx e_reg in
    result_reg, e_instrs @ spill_instrs @ op_instrs @ release_instrs

  | Ast.BinOp (e1, op, e2) ->
    (* 优化：对于简单运算，尝试复用寄存器减少溢出 *)
    let e1_reg, e1_instrs = gen_expr ctx e1 in
    let e2_reg, e2_instrs = gen_expr ctx e2 in
    
    (* 优化：对于 commutative 操作，选择占用寄存器更久的作为第一个操作数 *)
    let (rs1, rs2, instrs) = 
      match op with
      | "+" | "*" | "&&" | "||" | "==" | "!=" ->
        (* 交换操作数可能带来更好的寄存器利用率 *)
        (e2_reg, e1_reg, [])
      | _ -> (e1_reg, e2_reg, [])
    in
    
    let result_reg, spill_instrs = get_temp_reg ctx in
    let op_instrs, temp_regs_used =
      match op with
      | "+" -> ([ Add (result_reg, rs1, rs2) ], [])
      | "-" -> ([ Sub (result_reg, rs1, rs2) ], [])
      | "*" -> ([ Mul (result_reg, rs1, rs2) ], [])
      | "/" -> ([ Div (result_reg, rs1, rs2) ], [])
      | "%" -> ([ Rem (result_reg, rs1, rs2) ], [])
      | "==" ->  
          ([ Sub (result_reg, rs1, rs2); 
             Sltiu (result_reg, result_reg, 1) ], [])
      | "!=" -> 
          ([ Sub (result_reg, rs1, rs2); 
             Sltu (result_reg, Zero, result_reg) ], [])
      | "<" -> ([ Slt (result_reg, rs1, rs2) ], [])
      | "<=" -> 
          let t0, t0_spill = get_temp_reg ctx in
          let instrs = t0_spill @ [
            Slt (t0, rs2, rs1); 
            Xori (result_reg, t0, 1)
          ] in
          (instrs, [t0])
      | ">" -> ([ Slt (result_reg, rs2, rs1) ], [])
      | ">=" -> 
          let t0, t0_spill = get_temp_reg ctx in
          let instrs = t0_spill @ [
            Slt (t0, rs1, rs2); 
            Xori (result_reg, t0, 1)
          ] in
          (instrs, [t0])
      | "&&" ->
          let t0, t0_spill = get_temp_reg ctx in
          let t1, t1_spill = get_temp_reg ctx in
          let instrs = t0_spill @ t1_spill @ [
            Sltu (t0, Zero, rs1);
            Sltu (t1, Zero, rs2);
            And (result_reg, t0, t1)
          ] in
          (instrs, [t0; t1])
      | "||" ->
          let t0, t0_spill = get_temp_reg ctx in
          let instrs = t0_spill @ [
            Or (t0, rs1, rs2);
            Sltu (result_reg, Zero, t0)
          ] in
          (instrs, [t0])
      | _ -> failwith (Printf.sprintf "Unknown binary operator: %s" op)
    in
    (* 释放临时寄存器 *)
    let release_temp_instrs = 
      List.concat_map (fun reg -> release_temp_reg ctx reg) temp_regs_used in
    (* 释放操作数寄存器 *)
    let release_e1_instrs = release_temp_reg ctx e1_reg in
    let release_e2_instrs = release_temp_reg ctx e2_reg in
    (* 组合所有指令 *)
    let all_instrs = e1_instrs @ e2_instrs @ spill_instrs @ instrs @ 
                     op_instrs @ release_temp_instrs @ release_e1_instrs @ release_e2_instrs in
    result_reg, all_instrs

  | Ast.Call (fname, args) ->
    (* 优化：处理递归调用时减少寄存器压力 *)
    let is_recursive = fname = ctx.func_name in
    
    (* 1. 处理参数：前8个用寄存器，其余用栈参数区(sp+0开始) *)
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
               arg_code @ [ Sw (arg_reg, Sp, stack_arg_offset) ]  (* 使用Sp作为基地址 *)
           in
           let release_instrs = 
             if is_recursive && i < 8 then []  (* 递归调用保留参数寄存器 *)
             else release_temp_reg ctx arg_reg 
           in
           instrs @ release_instrs)
        args
      |> List.flatten
    in
  
    (* 2. 调用者分配栈空间保存函数调用结果 *)
    let result_offset = alloc_call_result ctx in
    let result_reg, spill_instrs = get_temp_reg ctx in
    
    (* 3. 函数调用并由调用者将返回值(A0)保存到栈上 *)
    let call_instr = [ 
      Jal (Ra, fname); 
      Sw (A0, Fp, result_offset);  (* 调用者保存结果到栈 *)
      Lw (result_reg, Fp, result_offset)  (* 从栈加载到结果寄存器 *)
    ] in
    
    (* 4. 返回结果寄存器和完整指令序列 *)
    result_reg, arg_instrs @ spill_instrs @ call_instr   

(* 函数序言 - 分配栈帧并保存ra、fp和临时寄存器(T0-T6) *)
let gen_prologue_instrs ctx frame_size =
  (* 首先分配整个栈帧 *)
  let prologue = [ 
    Addi (Sp, Sp, -frame_size);  (* 一次性分配整个栈帧 *)
    Sw (Ra, Sp, frame_size-4);   (* ra保存到sp+frame_size-4 (即fp-4) *)
    Sw (Fp, Sp, frame_size-8);   (* fp保存到sp+frame_size-8 (即fp-8) *)  
    Addi (Fp, Sp, frame_size)    (* fp = sp + frame_size（指向调用前的sp） *)
  ] in
  
  (* 被调用者保存临时寄存器T0-T6 *)
  prologue @ save_temp_regs ctx

(* 函数尾声 - 恢复临时寄存器(T0-T6)、ra和fp并释放栈帧 *)
let gen_epilogue_instrs ctx frame_size =
  (* 被调用者恢复临时寄存器T0-T6，然后恢复ra和fp *)
  restore_temp_regs ctx @ [
    Lw (Ra, Sp, frame_size-4);   (* 恢复ra *)
    Lw (Fp, Sp, frame_size-8);   (* 恢复fp *)
    Addi (Sp, Sp, frame_size);   (* 释放整个栈帧 *)
    Ret ]                        (* 返回 *)

(* 语句生成逻辑 - 适应寄存器溢出机制 *)
let rec gen_stmt ctx (stmt : Ast.stmt) : asm_item list =
  match stmt with
  | Ast.Empty -> []
  
  | Ast.ExprStmt e ->
    let reg, instrs = gen_expr ctx e in
    (* 对于表达式语句，使用后释放寄存器但保留计算结果 *)
    let release_instrs = release_temp_reg ctx reg in
    let all_instrs = instrs @ release_instrs in
    List.map (fun i -> Instruction i) all_instrs
  
  | Ast.Block stmts ->
    let old_vars = ctx.local_vars in
    let old_stack_offset = ctx.stack_offset in
    let old_call_offset = ctx.call_result_offset in
    let old_spill_offset = ctx.spill_offset in
    let old_total_locals = !(ctx.total_locals) in
    let old_used_regs = !(ctx.used_regs) in
    let old_free_regs = !(ctx.free_regs) in
    let old_spill_stack = !(ctx.spill_stack) in
    
    let items = List.map (gen_stmt ctx) stmts |> List.flatten in
    
    (* 恢复上下文 *)
    ctx.local_vars <- old_vars;
    ctx.stack_offset <- old_stack_offset;
    ctx.call_result_offset <- old_call_offset;
    ctx.spill_offset <- old_spill_offset;
    ctx.total_locals := old_total_locals;
    ctx.used_regs := old_used_regs;
    ctx.free_regs := old_free_regs;
    ctx.spill_stack := old_spill_stack;
    
    items
  
  | Ast.Return (Some e) ->
    let e_reg, e_instrs = gen_expr ctx e in
    (* 函数返回值放入A0 *)
    let move_instr = [ Mv (A0, e_reg) ] in
    let release_instrs = release_temp_reg ctx e_reg in
    let all_instrs = e_instrs @ move_instr @ release_instrs @ gen_epilogue_instrs ctx ctx.frame_size in
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
    let release_instrs = release_temp_reg ctx cond_reg in
    
    List.map (fun i -> Instruction i) cond_instrs
    @ [ Instruction (Beq (cond_reg, Zero, else_label)) ]
    @ then_items
    @ [ Instruction (J end_label); Label else_label ]
    @ else_items
    @ [ Label end_label ]
    @ List.map (fun i -> Instruction i) release_instrs
  
  | Ast.While (cond, body) ->
    let loop_label = new_label ctx "loop" in
    let end_label = new_label ctx "endloop" in
    ctx.break_labels <- end_label :: ctx.break_labels;
    ctx.continue_labels <- loop_label :: ctx.continue_labels;
    let cond_reg, cond_instrs = gen_expr ctx cond in
    let body_items = gen_stmt ctx body in
    ctx.break_labels <- List.tl ctx.break_labels;
    ctx.continue_labels <- List.tl ctx.continue_labels;
    let release_instrs = release_temp_reg ctx cond_reg in
    
    [ Label loop_label ]
    @ List.map (fun i -> Instruction i) cond_instrs
    @ [ Instruction (Beq (cond_reg, Zero, end_label)) ]
    @ body_items
    @ [ Instruction (J loop_label); Label end_label ]
    @ List.map (fun i -> Instruction i) release_instrs
  
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
    let store_instr = [ Sw (e_reg, Fp, offset) ] in
    let release_instrs = release_temp_reg ctx e_reg in
    let all_instrs = e_instrs @ store_instr @ release_instrs in
    List.map (fun i -> Instruction i) all_instrs
  
  | Ast.Assign (name, e) ->
    let offset = get_var_offset ctx name in
    let e_reg, e_instrs = gen_expr ctx e in
    (* 将函数调用结果赋值给变量 *)
    let store_instr = [ Sw (e_reg, Fp, offset) ] in
    let release_instrs = release_temp_reg ctx e_reg in
    let all_instrs = e_instrs @ store_instr @ release_instrs in
    List.map (fun i -> Instruction i) all_instrs

(* 栈帧大小计算 - 包含寄存器溢出区 *)
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
  
  (* 计算总栈帧大小 *)
  let frame_size = 
    stack_args_area_size +        (* 256字节 *)
    temp_regs_save_size +         (* 临时寄存器保存区28字节 *)
    spill_area_size +             (* 寄存器溢出区 *)
    call_results_area_size +      (* 128字节 *)
    locals_area_size +            (* 局部变量大小 *)
    ret_val_area_size +           (* 返回值保存区4字节 *)
    params_area_size +            (* 256字节 *)
    8                             (* ra和fp (2个寄存器) *)
  in
  
  (* 计算各区域偏移 (相对fp) *)
  let stack_args_offset = 0 in  (* 栈参数区从sp+0开始 *)
  
  (* 返回值保存区相对fp的偏移 *)
  let ret_val_offset = -(locals_area_size + params_area_size + 8) in
  
  (frame_size, call_results_area_size, ret_val_offset, stack_args_offset, spill_area_size)

(* 函数生成 - 支持寄存器溢出机制 *)
let gen_function symbol_table (func_def : Ast.func_def) : asm_item list =
  let (frame_size, call_results_area_size, ret_val_offset, stack_args_offset, spill_area_size) = 
    calculate_frame_size_and_offsets func_def in
  let ctx = create_context symbol_table func_def.fname frame_size 
      call_results_area_size ret_val_offset stack_args_offset spill_area_size in
  
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
           [ Instruction (Sw (arg_reg, Fp, param_offset)) ]
         else
           (* 第9+个参数从调用者栈参数区加载 (sp+0开始) *)
           let stack_arg_offset = ((i - 8) * 4) in  (* sp+0开始的偏移 *)
           let load_instr = Lw (T0, Fp, stack_arg_offset) in
           let store_instr = Sw (T0, Fp, param_offset) in
           [ Instruction load_instr; Instruction store_instr ]
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
    Comment ("Spill area size: " ^ string_of_int spill_area_size ^ " bytes");
    Comment ("Call results area size: " ^ string_of_int call_results_area_size ^ " bytes");
    Comment ("Temp registers save area: " ^ string_of_int temp_regs_save_size ^ " bytes") ]
  @ prologue 
  @ param_instrs 
  @ body_items 
  @ epilogue

(* 程序生成 *)
let gen_program symbol_table (program : Ast.program) =
  [ Directive ".text"; 
    Directive ".globl main"; 
    Directive ".align 2";
    Comment "Generated by RISC-V Code Generator with optimized register spilling";
    Comment "Stack layout from high to low address:";
    Comment "1. 栈参数区: sp+0 ~ sp+255 (256字节)";
    Comment "2. 临时寄存器保存区: 28字节 (保存T0-T6)";
    Comment "3. 寄存器溢出区: 16384字节";
    Comment "4. 函数调用结果保存区: 128字节";
    Comment "5. 局部变量区";
    Comment "6. 返回值保存区 (4字节)";
    Comment "7. 参数区 (256字节)";
    Comment "8. ra (fp-4) 和 旧fp (fp-8)";
    Comment "优化: 采用LRU算法选择溢出寄存器，减少溢出次数" ]
  @ List.flatten (List.map (gen_function symbol_table) program)





let compile_to_riscv symbol_table program =
  let asm_items = gen_program symbol_table program in
  List.iter
    (fun item -> print_endline (asm_item_to_string item))
    asm_items
    



































