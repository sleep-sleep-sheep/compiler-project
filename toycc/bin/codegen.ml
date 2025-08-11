open Ast

(* RISC-V 32位寄存器 *)
type reg =
  | Zero  (* 硬连线为0 *)
  | Ra    (* 返回地址 *)
  | Sp    (* 栈指针 *)
  | Gp    (* 全局指针 *)
  | Tp    (* 线程指针 *)
  | T0 | T1 | T2 | T3 | T4 | T5 | T6  (* 临时寄存器，调用者保存 *)
  | S0 | S1  (* 保存寄存器，被调用者保存 (S0=fp) *)
  | S2 | S3 | S4 | S5 | S6 | S7 | S8 | S9 | S10 | S11  (* 保存寄存器，被调用者保存 *)
  | A0 | A1 | A2 | A3 | A4 | A5 | A6 | A7  (* 参数/返回值寄存器，调用者保存 *)


(* 将寄存器转换为字符串 *)
let reg_to_string reg = 
  match reg with
  | Zero -> "zero"
  | Ra -> "ra"
  | Sp -> "sp"
  | Gp -> "gp"
  | Tp -> "tp"
  | T0 -> "t0" | T1 -> "t1" | T2 -> "t2" | T3 -> "t3" | T4 -> "t4" | T5 -> "t5" | T6 -> "t6"
  | S0 -> "s0" | S1 -> "s1"
  | S2 -> "s2" | S3 -> "s3" | S4 -> "s4" | S5 -> "s5" | S6 -> "s6" | S7 -> "s7" 
  | S8 -> "s8" | S9 -> "s9" | S10 -> "s10" | S11 -> "s11"
  | A0 -> "a0" | A1 -> "a1" | A2 -> "a2" | A3 -> "a3" | A4 -> "a4" | A5 -> "a5" | A6 -> "a6" | A7 -> "a7"


(* RISC-V 指令类型 *)
type instruction =
  (* 算术指令 *)
  | Add of reg * reg * reg (* add rd, rs1, rs2 *)
  | Addi of reg * reg * int (* addi rd, rs1, imm *)
  | Sub of reg * reg * reg (* sub rd, rs1, rs2 *)
  | Mul of reg * reg * reg (* mul rd, rs1, rs2 *)
  | Div of reg * reg * reg (* div rd, rs1, rs2 *)
  | Rem of reg * reg * reg (* rem rd, rs1, rs2 *)
  (* 逻辑指令 *)
  | And of reg * reg * reg (* and rd, rs1, rs2 *)
  | Or of reg * reg * reg (* or rd, rs1, rs2 *)
  | Xor of reg * reg * reg (* xor rd, rs1, rs2 *)
  | Xori of reg * reg * int (* xori rd, rs1, imm *)
  (* 比较指令 *)
  | Slt of reg * reg * reg (* slt rd, rs1, rs2 *)
  | Slti of reg * reg * int (* slti rd, rs1, imm *)
  | Sltu of reg * reg * reg (* sltu rd, rs1, rs2 *)
  | Sltiu of reg * reg * int (* sltiu rd, rs1, imm *)
  (* 加载/存储指令 *)
  | Lw of reg * int * reg (* lw rd, offset(rs1) *)
  | Sw of reg * int * reg (* sw rs2, offset(rs1) *)
  (* 分支指令 *)
  | Beq of reg * reg * string (* beq rs1, rs2, label *)
  | Bne of reg * reg * string (* bne rs1, rs2, label *)
  | Blt of reg * reg * string (* blt rs1, rs2, label *)
  | Bge of reg * reg * string (* bge rs1, rs2, label *)
  | Ble of reg * reg * string (* ble rs1, rs2, label - 伪指令 *)
  | Bgt of reg * reg * string (* bgt rs1, rs2, label - 伪指令 *)
  (* 跳转指令 *)
  | J of string (* j label *)
  | Jal of reg * string (* jal rd, label *)
  | Jalr of reg * reg * int (* jalr rd, rs1, offset *)
  | Ret (* ret - 伪指令 *)
  (* 立即数加载 *)
  | Li of reg * int (* li rd, imm - 伪指令 *)
  | Lui of reg * int (* lui rd, imm *)
  (* 移动指令 *)
  | Mv of reg * reg (* mv rd, rs - 伪指令 *)
  (* 其他 *)
  | Nop (* nop *)


(* 将指令转换为汇编字符串 *)
let instr_to_string instr = match instr with
  | Add (rd, rs1, rs2) ->
    Printf.sprintf "add %s, %s, %s"
      (reg_to_string rd) (reg_to_string rs1) (reg_to_string rs2)
  | Addi (rd, rs1, imm) ->
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
  | Ble (rs1, rs2, label) ->
    Printf.sprintf "ble %s, %s, %s" (reg_to_string rs1) (reg_to_string rs2) label
  | Bgt (rs1, rs2, label) ->
    Printf.sprintf "bgt %s, %s, %s" (reg_to_string rs1) (reg_to_string rs2) label
  | J label -> Printf.sprintf "j %s" label
  | Jal (rd, label) -> Printf.sprintf "jal %s, %s" (reg_to_string rd) label
  | Jalr (rd, rs1, offset) ->
    Printf.sprintf "jalr %s, %s, %d" (reg_to_string rd) (reg_to_string rs1) offset
  | Ret -> "ret"
  | Li (rd, imm) -> Printf.sprintf "li %s, %d" (reg_to_string rd) imm
  | Lui (rd, imm) -> Printf.sprintf "lui %s, %d" (reg_to_string rd) imm
  | Mv (rd, rs) -> Printf.sprintf "mv %s, %s" (reg_to_string rd) (reg_to_string rs)
  | Nop -> "nop"


(* 标签 *)
type label = string

(* 汇编代码项 *)
type asm_item =
  | Label of label
  | Instruction of instruction
  | Comment of string
  | Directive of string


(* 将汇编项转换为字符串 *)
let asm_item_to_string item = match item with 
  | Instruction instr -> "    " ^ instr_to_string instr
  | Label l -> l ^ ":"
  | Directive d -> "    " ^ d
  | Comment c -> "    # " ^ c


(* 输出汇编代码到文件 *)
let emit_asm_to_file filepath asm_items =
  let file = open_out filepath in
  List.iter
    (fun item ->
       output_string file (asm_item_to_string item);
       output_string file "\n")
    asm_items;
  close_out file


(* 输出汇编代码到标准输出 *)
let emit_asm_to_stdout asm_items =
  List.iter
    (fun item ->
       print_endline (asm_item_to_string item))
    asm_items


(* 代码生成上下文 *)
type codegen_context =
  { mutable label_counter : int (* 标签计数器 *)
  ; mutable temp_counter : int (* 临时寄存器计数器 *)
  ; mutable stack_offset : int (* 当前栈偏移 (相对于fp) *)
  ; mutable break_labels : string list (* break 跳转标签栈 *)
  ; mutable continue_labels : string list (* continue 跳转标签栈 *)
  ; mutable local_vars : (string * int) list (* 局部变量映射到栈偏移 *)
  ; func_name : string (* 函数名，用于生成唯一标签 *)
  ; frame_size : int ref (* 栈帧大小 *)
  }


(* 创建新的代码生成上下文 *)
let create_context _symbol_table func_name frame_size =
  { label_counter = 0;
    temp_counter = 0;
    stack_offset = -8; (* fp-based offset, 初始为-8是因为fp指向ra和s0保存的位置之上 *)
    break_labels = [];
    continue_labels = [];
    local_vars = [];
    func_name = func_name;
    frame_size = ref frame_size
  }


(* 生成新标签 - 使用函数名作为标签前缀，确保全局唯一 *)
let new_label ctx prefix =
  let label = Printf.sprintf "%s_%s%d" ctx.func_name prefix ctx.label_counter in
  ctx.label_counter <- ctx.label_counter + 1;
  label


(* 获取临时寄存器 - 只使用t0-t6，符合RISC-V ABI *)
let get_temp_reg ctx =
  let reg =
    match ctx.temp_counter mod 7 with
    | 0 -> T0
    | 1 -> T1
    | 2 -> T2
    | 3 -> T3
    | 4 -> T4
    | 5 -> T5
    | 6 -> T6
    | _ -> failwith "Should not happen"
  in
  ctx.temp_counter <- ctx.temp_counter + 1;
  reg

(* 释放临时寄存器 - 简单的计数器回退 *)
let free_temp_reg ctx =
  if ctx.temp_counter > 0 then
    ctx.temp_counter <- ctx.temp_counter - 1


(* 将变量添加到栈中 *)
let add_local_var ctx name =
  ctx.stack_offset <- ctx.stack_offset - 4;
  ctx.local_vars <- (name, ctx.stack_offset) :: ctx.local_vars;
  (* 确保栈帧足够大 *)
  let required_frame = -ctx.stack_offset in
  if required_frame > !(ctx.frame_size) then
    ctx.frame_size := required_frame;
  ctx.stack_offset


(* 获取变量的栈偏移 *)
let get_var_offset ctx name =
  match List.assoc_opt name ctx.local_vars with
  | Some offset -> offset
  | None -> 
      failwith (Printf.sprintf "Variable '%s' not found in scope (available: %s)"
        name 
        (String.concat ", " (List.map fst ctx.local_vars)))


(* 保存被调用者保存寄存器 - 返回instruction list *)
let save_callee_saved_regs frame_size =
  [ Sw (S0, frame_size - 4, Sp);  (* s0/fp *)
    Sw (S1, frame_size - 8, Sp);
    Sw (S2, frame_size - 12, Sp);
    Sw (S3, frame_size - 16, Sp);
    Sw (S4, frame_size - 20, Sp);
    Sw (S5, frame_size - 24, Sp);
    Sw (S6, frame_size - 28, Sp);
    Sw (S7, frame_size - 32, Sp);
    Sw (S8, frame_size - 36, Sp);
    Sw (S9, frame_size - 40, Sp);
    Sw (S10, frame_size - 44, Sp);
    Sw (S11, frame_size - 48, Sp) ]

(* 恢复被调用者保存寄存器 - 返回instruction list *)
let restore_callee_saved_regs frame_size =
  [ Lw (S0, frame_size - 4, Sp);  (* s0/fp *)
    Lw (S1, frame_size - 8, Sp);
    Lw (S2, frame_size - 12, Sp);
    Lw (S3, frame_size - 16, Sp);
    Lw (S4, frame_size - 20, Sp);
    Lw (S5, frame_size - 24, Sp);
    Lw (S6, frame_size - 28, Sp);
    Lw (S7, frame_size - 32, Sp);
    Lw (S8, frame_size - 36, Sp);
    Lw (S9, frame_size - 40, Sp);
    Lw (S10, frame_size - 44, Sp);
    Lw (S11, frame_size - 48, Sp) ]


(* 生成表达式代码，返回结果寄存器和指令列表 *)
let rec gen_expr ctx (expr : Ast.expr) : reg * instruction list =
  match expr with
  | Ast.Literal(IntLit n) ->
    let reg = get_temp_reg ctx in
    let instr = [ Li (reg, n) ] in
    reg, instr
  | Ast.Var id ->
    let reg = get_temp_reg ctx in
    let offset = get_var_offset ctx id in
    let instr = [ Lw (reg, offset, S0) ] in  (* 使用S0作为帧指针(fp) *)
    reg, instr
  | Ast.Paren e -> gen_expr ctx e
  | Ast.UnOp (op, e) ->
    let e_reg, e_instrs = gen_expr ctx e in
    let result_reg = get_temp_reg ctx in
    let instrs =
      match op with
      | "+" -> e_instrs @ [ Mv (result_reg, e_reg) ]  (* 一元正号：直接复制寄存器值 *)
      | "-" -> e_instrs @ [ Sub (result_reg, Zero, e_reg) ]
      | "!" -> e_instrs @ [ Sltiu (result_reg, e_reg, 1) ]
      | _ -> failwith (Printf.sprintf "Unknown unary operator: %s" op)
    in
    free_temp_reg ctx;  (* 释放e_reg *)
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
      | "==" ->  [ Sub (result_reg, e1_reg, e2_reg); Sltiu (result_reg, result_reg, 1) ]
      | "!=" -> [ Sub (result_reg, e1_reg, e2_reg); Sltu (result_reg, Zero, result_reg) ]
      | "<" -> [ Slt (result_reg, e1_reg, e2_reg) ]
      | "<=" -> [ Slt (result_reg, e2_reg, e1_reg); Xori (result_reg, result_reg, 1) ]
      | ">" -> [ Slt (result_reg, e2_reg, e1_reg) ]
      | ">=" -> [ Slt (result_reg, e1_reg, e2_reg); Xori (result_reg, result_reg, 1) ]
      | "&&" ->
        [ Sltu (T0, Zero, e1_reg); Sltu (T1, Zero, e2_reg); And (result_reg, T0, T1) ]
      | "||" ->
        [ Or (result_reg, e1_reg, e2_reg); Sltu (result_reg, Zero, result_reg) ]
      | _ -> failwith (Printf.sprintf "Unknown binary operator: %s" op)
    in
    let instrs = e1_instrs @ e2_instrs @ op_instrs in
    free_temp_reg ctx;  (* 释放e1_reg *)
    free_temp_reg ctx;  (* 释放e2_reg *)
    result_reg, instrs
  | Ast.Call (fname, args) ->
    let result_reg = A0 in  (* 函数返回值保存在a0中 *)
    let num_args = List.length args in
    
    (* 计算需要保存的寄存器和栈参数所需的空间 *)
    (* 保存a0-a7和t0-t6寄存器，因为它们是调用者保存的 *)
    let caller_saved_regs = [A0; A1; A2; A3; A4; A5; A6; A7; T0; T1; T2; T3; T4; T5; T6] in
    let num_caller_saved = List.length caller_saved_regs in
    let caller_saved_space = num_caller_saved * 4 in
    
    (* 计算栈参数空间 (超过8个的参数) *)
    let num_stack_args = max 0 (num_args - 8) in
    let stack_args_space = num_stack_args * 4 in
    
    (* 总栈空间，16字节对齐 *)
    let total_stack_space = caller_saved_space + stack_args_space in
    let aligned_stack_space = ((total_stack_space + 15) / 16) * 16 in
    
    (* 保存调用者保存的寄存器 *)
    let save_instrs = 
      if aligned_stack_space > 0 then
        Addi (Sp, Sp, -aligned_stack_space) ::
        List.mapi (fun i reg -> 
          Sw (reg, i * 4, Sp)
        ) caller_saved_regs
      else []
    in
    
    (* 处理参数：前8个用a0-a7寄存器，其余用栈 *)
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
               (* 超过8个的参数使用栈传递 *)
               let stack_pos = caller_saved_space + (i - 8) * 4 in
               arg_code @ [ Sw (arg_reg, stack_pos, Sp) ]
           in
           free_temp_reg ctx;  (* 释放arg_reg *)
           instrs)
        args
      |> List.flatten
    in
    
    (* 函数调用 *)
    let call_instr = [ Jal (Ra, fname) ] in
    
    (* 恢复调用者保存的寄存器和栈指针 *)
    let restore_instrs =
      if aligned_stack_space > 0 then
        (List.mapi (fun i reg -> 
          Lw (reg, i * 4, Sp)
        ) caller_saved_regs) @
        [ Addi (Sp, Sp, aligned_stack_space) ]
      else []
    in
    
    let all_instrs = save_instrs @ arg_instrs @ call_instr @ restore_instrs in
    result_reg, all_instrs   


(* 生成序言指令 - 返回instruction list *)
let gen_prologue_instructions frame_size =
  [ Addi (Sp, Sp, -frame_size);  (* 分配栈帧 *)
    Sw (Ra, frame_size - 4, Sp);  (* 保存返回地址 *)
    Sw (S0, frame_size - 8, Sp);  (* 保存旧的帧指针(s0) *)
    Addi (S0, Sp, frame_size) ]   (* 设置新的帧指针(s0) *)
  @ save_callee_saved_regs frame_size

(* 生成序言 - 带注释的asm_item list *)
let gen_prologue frame_size =
  [ Comment "函数序言: 分配栈帧并保存寄存器" ]
  @ List.map (fun i -> Instruction i) (gen_prologue_instructions frame_size)

(* 生成尾声指令 - 返回instruction list *)
let gen_epilogue_instructions frame_size =
  restore_callee_saved_regs frame_size @
  [ Lw (Ra, frame_size - 4, Sp);  (* 恢复返回地址 *)
    Lw (S0, frame_size - 8, Sp);  (* 恢复帧指针(s0) *)
    Addi (Sp, Sp, frame_size);    (* 释放栈帧 *)
    Ret ]  (* 返回 *)

(* 生成尾声 - 带注释的asm_item list *)
let gen_epilogue frame_size =
  [ Comment "函数尾声: 恢复寄存器并返回" ]
  @ List.map (fun i -> Instruction i) (gen_epilogue_instructions frame_size)


(* 生成语句代码 *)
let rec gen_stmt ctx (stmt : Ast.stmt) : asm_item list =
  match stmt with
  | Ast.Empty -> []
  | Ast.ExprStmt e ->
    let _, instrs = gen_expr ctx e in
    free_temp_reg ctx;  (* 释放表达式结果寄存器 *)
    List.map (fun i -> Instruction i) instrs
  | Ast.Block stmts ->
    let old_vars = ctx.local_vars in
    let old_offset = ctx.stack_offset in
    let items = List.map (gen_stmt ctx) stmts |> List.flatten in
    ctx.local_vars <- old_vars;
    ctx.stack_offset <- old_offset;
    items
  | Ast.Return (Some e) ->
    let e_reg, e_instrs = gen_expr ctx e in
    (* 先将表达式结果移动到A0，再添加尾声指令 *)
    let return_instrs = e_instrs @ [ Mv (A0, e_reg) ] in
    let epilogue_instrs = gen_epilogue_instructions !(ctx.frame_size) in
    let all_instrs = return_instrs @ epilogue_instrs in
    free_temp_reg ctx;  (* 释放e_reg *)
    List.map (fun i -> Instruction i) all_instrs
  | Ast.Return None -> 
    (* 直接生成尾声指令 *)
    List.map (fun i -> Instruction i) (gen_epilogue_instructions !(ctx.frame_size))
  | Ast.If (cond, then_stmt, else_stmt) ->
    let cond_reg, cond_instrs = gen_expr ctx cond in
    let else_label = new_label ctx "else" in
    let end_label = new_label ctx "endif" in
    let then_items = gen_stmt ctx then_stmt in
    let else_items =
      match else_stmt with
      | Some s -> gen_stmt ctx s
      | None -> []
    in
    let instrs = 
      List.map (fun i -> Instruction i) cond_instrs
      @ [ Instruction (Beq (cond_reg, Zero, else_label)) ]
      @ then_items
      @ [ Instruction (J end_label); Label else_label ]
      @ else_items
      @ [ Label end_label ]
    in
    free_temp_reg ctx;  (* 释放cond_reg *)
    instrs
  | Ast.While (cond, body) ->
    let loop_label = new_label ctx "loop" in
    let end_label = new_label ctx "endloop" in
    ctx.break_labels <- end_label :: ctx.break_labels;
    ctx.continue_labels <- loop_label :: ctx.continue_labels;
    let cond_reg, cond_instrs = gen_expr ctx cond in
    let body_items = gen_stmt ctx body in
    ctx.break_labels <- List.tl ctx.break_labels;
    ctx.continue_labels <- List.tl ctx.continue_labels;
    let instrs =
      [ Label loop_label ]
      @ List.map (fun i -> Instruction i) cond_instrs
      @ [ Instruction (Beq (cond_reg, Zero, end_label)) ]
      @ body_items
      @ [ Instruction (J loop_label); Label end_label ]
    in
    free_temp_reg ctx;  (* 释放cond_reg *)
    instrs
  | Ast.Break ->
    (match ctx.break_labels with
     | label :: _ -> [ Instruction (J label) ]
     | [] -> failwith "Break outside loop")
  | Ast.Continue ->
    (match ctx.continue_labels with
     | label :: _ -> [ Instruction (J label) ]
     | [] -> failwith "Continue outside loop")
  | Ast.Decl (name, e) ->
    let offset = add_local_var ctx name in
    let e_reg, e_instrs = gen_expr ctx e in
    let all_instrs = e_instrs @ [ Sw (e_reg, offset, S0) ] in  (* 使用S0作为帧指针 *)
    free_temp_reg ctx;  (* 释放e_reg *)
    List.map (fun i -> Instruction i) all_instrs
  | Ast.Assign (name, e) ->
    let offset = get_var_offset ctx name in
    let e_reg, e_instrs = gen_expr ctx e in
    let all_instrs = e_instrs @ [ Sw (e_reg, offset, S0) ] in  (* 使用S0作为帧指针 *)
    free_temp_reg ctx;  (* 释放e_reg *)
    List.map (fun i -> Instruction i) all_instrs


(* 计算函数所需的栈帧大小下限 *)
let calculate_min_frame_size (func_def : Ast.func_def) =
  (* 递归统计一个语句中包含的 Decl 数量 *)
  let rec count_decls_in_stmt (stmt:Ast.stmt) =
    match stmt with
    | Decl _ -> 1
    | Block stmts -> List.fold_left (fun acc s -> acc + count_decls_in_stmt s) 0 stmts
    | If (_, s1, Some s2) -> count_decls_in_stmt s1 + count_decls_in_stmt s2
    | If (_, s1, None) -> count_decls_in_stmt s1
    | While (_, s) -> count_decls_in_stmt s
    | _ -> 0
  in
  (* 统计函数体中所有的声明 *)
  let num_locals =
    List.fold_left (fun acc stmt -> acc + count_decls_in_stmt stmt) 0 func_def.body in
  let num_params = List.length func_def.params in
  
  (* 最小栈帧需求：ra(4) + s0(4) + 被调用者保存寄存器(48) + 参数 + 局部变量 *)
  56 + (num_params * 4) + (num_locals * 4)  (* 56 = 4 + 4 + 48 *)


(* 生成函数代码 *)
let gen_function symbol_table (func_def : Ast.func_def) : asm_item list =
  (* 计算初始栈帧大小并对齐到16字节 *)
  let min_frame_size = calculate_min_frame_size func_def in
  let aligned_frame_size = ((min_frame_size + 15) / 16) * 16 in
  
  (* 创建上下文 *)
  let ctx = create_context symbol_table func_def.fname aligned_frame_size in
  
  (* 函数序言 *)
  let prologue = gen_prologue !(ctx.frame_size) in
  
  (* 处理参数：前8个用a0-a7寄存器，其余用栈 *)
  let param_instrs =
    List.mapi
      (fun i { Ast.pname = name; _ } ->
       let offset = add_local_var ctx name in
       let instr =
         if i < 8 then
           (* 前8个参数使用a0-a7寄存器 *)
           let arg_reg =
             match i with
             | 0 -> A0 | 1 -> A1 | 2 -> A2 | 3 -> A3
             | 4 -> A4 | 5 -> A5 | 6 -> A6 | 7 -> A7
             | _ -> failwith "Invalid register index"
           in
           [ Instruction (Sw (arg_reg, offset, S0)) ]  (* 保存到栈帧 *)
         else
           (* 超过8个的参数从栈中读取 (调用者已将它们放在那里) *)
           let stack_offset = 16 + (i - 8) * 4 in  (* 跳过ra和s0 *)
           [ Instruction (Lw (T0, stack_offset, S0));
             Instruction (Sw (T0, offset, S0)) ]
       in
       instr)
      func_def.params
    |> List.flatten
  in
  
  (* 函数体 *)
  let body_items =
    func_def.body
    |> List.map (gen_stmt ctx)
    |> List.flatten
  in
  
  (* 函数尾声（如果函数没有显式 return） *)
  let epilogue =
    let has_ret =
      List.exists
        (function
          | Instruction Ret -> true
          | _ -> false)
        body_items
    in
    if has_ret then []
    else gen_epilogue !(ctx.frame_size)
  in
  
  [ Label func_def.fname; 
    Comment ("函数: " ^ func_def.fname ^ ", 栈帧大小: " ^ string_of_int !(ctx.frame_size)) ]
  @ prologue
  @ param_instrs
  @ body_items
  @ epilogue


(* 生成整个程序的代码 *)
let gen_program symbol_table (program : Ast.program) =
  (* 全局声明和数据段 *)
  let header =
    [ Directive ".text";
      Directive ".align 2";
      Directive ".globl main";
      Comment "ToyC 编译器生成的RISC-V汇编代码" ]
  in
  
  (* 生成所有函数 *)
  let func_asm_items =
    List.map
      (fun func_def -> gen_function symbol_table func_def)
      program
    |> List.flatten
  in
  
  header @ func_asm_items


(* 编译程序并输出汇编代码 *)
let compile_to_riscv symbol_table program =
  let asm_items = gen_program symbol_table program in
  emit_asm_to_stdout asm_items
