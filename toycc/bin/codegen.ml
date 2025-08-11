open Ast

(* RISC-V 32位寄存器定义 *)
type reg =
  | Zero  (* 硬连线为0 *)
  | Ra    (* 返回地址 *)
  | Sp    (* 栈指针 *)
  | Gp    (* 全局指针 *)
  | Tp    (* 线程指针 *)
  | T0 | T1 | T2 | T3 | T4 | T5 | T6  (* 临时寄存器，调用者保存 *)
  | S0 | S1 | S2 | S3 | S4 | S5 | S6 | S7 | S8 | S9 | S10 | S11  (* 保存寄存器，被调用者保存 *)
  | A0 | A1 | A2 | A3 | A4 | A5 | A6 | A7  (* 参数/返回值寄存器 *)

(* 寄存器转字符串 *)
let reg_to_string = function
  | Zero -> "zero"
  | Ra -> "ra"
  | Sp -> "sp"
  | Gp -> "gp"
  | Tp -> "tp"
  | T0 -> "t0" | T1 -> "t1" | T2 -> "t2" | T3 -> "t3" | T4 -> "t4" | T5 -> "t5" | T6 -> "t6"
  | S0 -> "s0" | S1 -> "s1" | S2 -> "s2" | S3 -> "s3" | S4 -> "s4" | S5 -> "s5" 
  | S6 -> "s6" | S7 -> "s7" | S8 -> "s8" | S9 -> "s9" | S10 -> "s10" | S11 -> "s11"
  | A0 -> "a0" | A1 -> "a1" | A2 -> "a2" | A3 -> "a3" | A4 -> "a4" | A5 -> "a5" | A6 -> "a6" | A7 -> "a7"

(* RISC-V指令类型 *)
type instruction =
  (* 算术指令 *)
  | Add of reg * reg * reg
  | Addi of reg * reg * int
  | Sub of reg * reg * reg
  | Mul of reg * reg * reg
  | Div of reg * reg * reg
  | Rem of reg * reg * reg
  (* 逻辑指令 *)
  | And of reg * reg * reg
  | Or of reg * reg * reg
  | Xor of reg * reg * reg
  | Xori of reg * reg * int
  (* 比较指令 *)
  | Slt of reg * reg * reg
  | Slti of reg * reg * int
  | Sltu of reg * reg * reg
  | Sltiu of reg * reg * int
  (* 加载/存储指令 *)
  | Lw of reg * int * reg
  | Sw of reg * int * reg
  (* 分支指令 *)
  | Beq of reg * reg * string
  | Bne of reg * reg * string
  | Blt of reg * reg * string
  | Bge of reg * reg * string
  (* 跳转指令 *)
  | J of string
  | Jal of reg * string
  | Jalr of reg * reg * int
  | Ret
  (* 立即数加载 *)
  | Li of reg * int
  | Lui of reg * int
  (* 移动指令 *)
  | Mv of reg * reg
  | Nop

(* 指令转字符串 *)
let instr_to_string = function
  | Add (rd, rs1, rs2) ->
    Printf.sprintf "add %s, %s, %s" (reg_to_string rd) (reg_to_string rs1) (reg_to_string rs2)
  | Addi (rd, rs1, imm) ->
    Printf.sprintf "addi %s, %s, %d" (reg_to_string rd) (reg_to_string rs1) imm
  | Sub (rd, rs1, rs2) ->
    Printf.sprintf "sub %s, %s, %s" (reg_to_string rd) (reg_to_string rs1) (reg_to_string rs2)
  | Mul (rd, rs1, rs2) ->
    Printf.sprintf "mul %s, %s, %s" (reg_to_string rd) (reg_to_string rs1) (reg_to_string rs2)
  | Div (rd, rs1, rs2) ->
    Printf.sprintf "div %s, %s, %s" (reg_to_string rd) (reg_to_string rs1) (reg_to_string rs2)
  | Rem (rd, rs1, rs2) ->
    Printf.sprintf "rem %s, %s, %s" (reg_to_string rd) (reg_to_string rs1) (reg_to_string rs2)
  | And (rd, rs1, rs2) ->
    Printf.sprintf "and %s, %s, %s" (reg_to_string rd) (reg_to_string rs1) (reg_to_string rs2)
  | Or (rd, rs1, rs2) ->
    Printf.sprintf "or %s, %s, %s" (reg_to_string rd) (reg_to_string rs1) (reg_to_string rs2)
  | Xor (rd, rs1, rs2) ->
    Printf.sprintf "xor %s, %s, %s" (reg_to_string rd) (reg_to_string rs1) (reg_to_string rs2)
  | Xori (rd, rs1, imm) ->
    Printf.sprintf "xori %s, %s, %d" (reg_to_string rd) (reg_to_string rs1) imm
  | Slt (rd, rs1, rs2) ->
    Printf.sprintf "slt %s, %s, %s" (reg_to_string rd) (reg_to_string rs1) (reg_to_string rs2)
  | Slti (rd, rs1, imm) ->
    Printf.sprintf "slti %s, %s, %d" (reg_to_string rd) (reg_to_string rs1) imm
  | Sltu (rd, rs1, rs2) ->
    Printf.sprintf "sltu %s, %s, %s" (reg_to_string rd) (reg_to_string rs1) (reg_to_string rs2)
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
  | Li (rd, imm) -> Printf.sprintf "li %s, %d" (reg_to_string rd) imm
  | Lui (rd, imm) -> Printf.sprintf "lui %s, %d" (reg_to_string rd) imm
  | Mv (rd, rs) -> Printf.sprintf "mv %s, %s" (reg_to_string rd) (reg_to_string rs)
  | Nop -> "nop"

(* 汇编代码项 *)
type asm_item =
  | Label of string
  | Instruction of instruction
  | Comment of string
  | Directive of string

(* 汇编项转字符串 *)
let asm_item_to_string = function
  | Instruction instr -> "    " ^ instr_to_string instr
  | Label l -> l ^ ":"
  | Directive d -> "    " ^ d
  | Comment c -> "    # " ^ c

(* 代码生成上下文 - 关键改进：完善寄存器管理 *)
type codegen_context = {
  mutable label_counter: int;
  mutable temp_counter: int;
  mutable stack_offset: int;  (* FP相对偏移，向下增长 *)
  mutable break_labels: string list;
  mutable continue_labels: string list;
  mutable local_vars: (string * int) list;  (* 变量名 -> 栈偏移 *)
  func_name: string;
  saved_regs: reg list;  (* 需要保存的寄存器 *)
  mutable used_saved_regs: reg list;  (* 已使用的保存寄存器 *)
}

(* 创建新的代码生成上下文 *)
let create_context symbol_table func_name = {
  label_counter = 0;
  temp_counter = 0;
  stack_offset = -8;  (* 初始偏移，为ra和fp预留空间 *)
  break_labels = [];
  continue_labels = [];
  local_vars = [];
  func_name = func_name;
  saved_regs = [S0; S1; S2; S3; S4; S5; S6; S7; S8; S9; S10; S11];  (* 被调用者保存寄存器 *)
  used_saved_regs = [];
}

(* 生成唯一标签 *)
let new_label ctx prefix =
  let label = Printf.sprintf "%s_%s%d" ctx.func_name prefix ctx.label_counter in
  ctx.label_counter <- ctx.label_counter + 1;
  label

(* 获取临时寄存器 (遵循ABI: 调用者保存) *)
let get_temp_reg ctx =
  let regs = [T0; T1; T2; T3; T4; T5; T6] in
  let reg = List.nth regs (ctx.temp_counter mod 7) in
  ctx.temp_counter <- ctx.temp_counter + 1;
  reg

(* 获取保存寄存器 (遵循ABI: 被调用者保存) *)
let get_saved_reg ctx =
  match ctx.used_saved_regs with
  | [] ->
    let reg = List.hd ctx.saved_regs in
    ctx.used_saved_regs <- [reg];
    reg
  | rs ->
    let rec find_unused = function
      | [] -> failwith "No more saved registers available"
      | r::rest ->
        if List.mem r rs then find_unused rest
        else begin
          ctx.used_saved_regs <- r::rs;
          r
        end
    in
    find_unused ctx.saved_regs

(* 变量管理：添加局部变量到栈 *)
let add_local_var ctx name =
  ctx.stack_offset <- ctx.stack_offset - 4;
  ctx.local_vars <- (name, ctx.stack_offset) :: ctx.local_vars;
  ctx.stack_offset

(* 获取变量的栈偏移 *)
let get_var_offset ctx name =
  match List.assoc_opt name ctx.local_vars with
  | Some offset -> offset
  | None -> 
      failwith (Printf.sprintf "变量 '%s' 未在作用域中找到 (可用变量: %s)"
        name 
        (String.concat ", " (List.map fst ctx.local_vars)))

(* 生成表达式代码 *)
let rec gen_expr ctx (expr: Ast.expr) : reg * instruction list =
  match expr with
  | Ast.Literal(IntLit n) ->
    let reg = get_temp_reg ctx in
    reg, [Li (reg, n)]
  
  | Ast.Var id ->
    let reg = get_temp_reg ctx in
    let offset = get_var_offset ctx id in
    reg, [Lw (reg, offset, S0)]  (* 使用S0作为帧指针 (符合ABI) *)
  
  | Ast.Paren e -> gen_expr ctx e
  
  | Ast.UnOp (op, e) ->
    let e_reg, e_instrs = gen_expr ctx e in
    let result_reg = get_temp_reg ctx in
    let instrs = match op with
      | "+" -> e_instrs @ [Mv (result_reg, e_reg)]
      | "-" -> e_instrs @ [Sub (result_reg, Zero, e_reg)]
      | "!" -> e_instrs @ [Sltiu (result_reg, e_reg, 1)]
      | _ -> failwith (Printf.sprintf "未知的一元运算符: %s" op)
    in
    result_reg, instrs
  
  | Ast.BinOp (e1, op, e2) ->
    let e1_reg, e1_instrs = gen_expr ctx e1 in
    let e2_reg, e2_instrs = gen_expr ctx e2 in
    let result_reg = get_temp_reg ctx in
    let op_instrs = match op with
      | "+" -> [Add (result_reg, e1_reg, e2_reg)]
      | "-" -> [Sub (result_reg, e1_reg, e2_reg)]
      | "*" -> [Mul (result_reg, e1_reg, e2_reg)]
      | "/" -> [Div (result_reg, e1_reg, e2_reg)]
      | "%" -> [Rem (result_reg, e1_reg, e2_reg)]
      | "==" -> [Sub (result_reg, e1_reg, e2_reg); Sltiu (result_reg, result_reg, 1)]
      | "!=" -> [Sub (result_reg, e1_reg, e2_reg); Sltu (result_reg, Zero, result_reg)]
      | "<" -> [Slt (result_reg, e1_reg, e2_reg)]
      | "<=" -> [Slt (result_reg, e2_reg, e1_reg); Xori (result_reg, result_reg, 1)]
      | ">" -> [Slt (result_reg, e2_reg, e1_reg)]
      | ">=" -> [Slt (result_reg, e1_reg, e2_reg); Xori (result_reg, result_reg, 1)]
      | "&&" ->
        [Sltu (T0, Zero, e1_reg); Sltu (T1, Zero, e2_reg); And (result_reg, T0, T1)]
      | "||" ->
        [Or (result_reg, e1_reg, e2_reg); Sltu (result_reg, Zero, result_reg)]
      | _ -> failwith (Printf.sprintf "未知的二元运算符: %s" op)
    in
    result_reg, e1_instrs @ e2_instrs @ op_instrs
  
  | Ast.Call (fname, args) ->
    (* 关键改进：严格遵循RISC-V ABI的函数调用规范 *)
    let num_args = List.length args in
    let num_stack_args = max 0 (num_args - 8) in  (* 超过8个参数用栈传递 *)
    let stack_args_space = num_stack_args * 4 in
    
    (* 计算需要保存的寄存器：临时寄存器T0-T6和参数寄存器A0-A7 *)
    let caller_saved = [T0; T1; T2; T3; T4; T5; T6; A0; A1; A2; A3; A4; A5; A6; A7] in
    let num_caller_saved = List.length caller_saved in
    let save_area_size = num_caller_saved * 4 in
    
    (* 总栈空间 = 参数空间 + 保存寄存器空间，保持16字节对齐 *)
    let total_stack_space = 
      let space = stack_args_space + save_area_size in
      if space mod 16 = 0 then space else space + (16 - space mod 16)
    in
    
    (* 1. 保存调用者寄存器 *)
    let save_instrs = 
      if total_stack_space > 0 then
        Addi (Sp, Sp, -total_stack_space) ::
        List.mapi (fun i reg -> 
          Sw (reg, i * 4 + stack_args_space, Sp)
        ) caller_saved
      else []
    in
    
    (* 2. 处理参数：前8个用A0-A7，其余用栈 *)
    let arg_instrs = 
      List.mapi (fun i arg ->
        let arg_reg, arg_code = gen_expr ctx arg in
        if i < 8 then
          let target_reg = match i with
            | 0 -> A0 | 1 -> A1 | 2 -> A2 | 3 -> A3
            | 4 -> A4 | 5 -> A5 | 6 -> A6 | 7 -> A7
            | _ -> failwith "无效的参数索引"
          in
          arg_code @ [Mv (target_reg, arg_reg)]
        else
          let stack_pos = (i - 8) * 4 in  (* 栈参数从Sp开始存放 *)
          arg_code @ [Sw (arg_reg, stack_pos, Sp)]
      ) args
      |> List.flatten
    in
    
    (* 3. 调用函数 *)
    let call_instr = [Jal (Ra, fname)] in
    
    (* 4. 恢复调用者寄存器 *)
    let restore_instrs = 
      if total_stack_space > 0 then
        List.mapi (fun i reg -> 
          Lw (reg, i * 4 + stack_args_space, Sp)
        ) caller_saved @
        [Addi (Sp, Sp, total_stack_space)]
      else []
    in
    
    (* 结果在A0中返回 *)
    A0, save_instrs @ arg_instrs @ call_instr @ restore_instrs

(* 生成函数序言：保存被调用者寄存器和设置栈帧 *)
let gen_prologue ctx frame_size =
  (* 计算需要保存的寄存器数量 *)
  let num_saved_regs = List.length ctx.used_saved_regs in
  
  (* 1. 分配栈空间 *)
  let prologue = [Addi (Sp, Sp, -frame_size)] in
  
  (* 2. 保存返回地址和帧指针(S0) *)
  let prologue = prologue @ [
    Sw (Ra, frame_size - 4, Sp);
    Sw (S0, frame_size - 8, Sp);
    Addi (S0, Sp, frame_size)  (* S0作为帧指针 *)
  ] in
  
  (* 3. 保存被调用者寄存器 *)
  let prologue = prologue @ 
    List.mapi (fun i reg ->
      Sw (reg, frame_size - 12 - i * 4, Sp)
    ) ctx.used_saved_regs
  in
  
  List.map (fun i -> Instruction i) prologue

(* 生成函数尾声：恢复寄存器和返回 *)
let gen_epilogue ctx frame_size =
  let num_saved_regs = List.length ctx.used_saved_regs in
  
  (* 1. 恢复被调用者寄存器 *)
  let epilogue = 
    List.mapi (fun i reg ->
      Lw (reg, frame_size - 12 - i * 4, Sp)
    ) ctx.used_saved_regs
  in
  
  (* 2. 恢复返回地址和帧指针，释放栈空间 *)
  let epilogue = epilogue @ [
    Lw (Ra, frame_size - 4, Sp);
    Lw (S0, frame_size - 8, Sp);
    Addi (Sp, Sp, frame_size);
    Ret
  ] in
  
  epilogue

(* 生成语句代码 *)
let rec gen_stmt ctx frame_size (stmt: Ast.stmt) : asm_item list =
  match stmt with
  | Ast.Empty -> []
  
  | Ast.ExprStmt e ->
    let _, instrs = gen_expr ctx e in
    List.map (fun i -> Instruction i) instrs
  
  | Ast.Block stmts ->
    let old_vars = ctx.local_vars in
    let old_offset = ctx.stack_offset in
    let old_used_saved = ctx.used_saved_regs in
    let items = List.concat_map (gen_stmt ctx frame_size) stmts in
    ctx.local_vars <- old_vars;
    ctx.stack_offset <- old_offset;
    ctx.used_saved_regs <- old_used_saved;
    items
  
  | Ast.Return (Some e) ->
    let e_reg, e_instrs = gen_expr ctx e in
    let instrs = e_instrs @ [Mv (A0, e_reg)] @ gen_epilogue ctx frame_size in
    List.map (fun i -> Instruction i) instrs
  
  | Ast.Return None ->
    List.map (fun i -> Instruction i) (gen_epilogue ctx frame_size)
  
  | Ast.If (cond, then_stmt, else_stmt) ->
    let cond_reg, cond_instrs = gen_expr ctx cond in
    let else_label = new_label ctx "else" in
    let end_label = new_label ctx "endif" in
    let then_items = gen_stmt ctx frame_size then_stmt in
    let else_items = match else_stmt with
      | Some s -> gen_stmt ctx frame_size s
      | None -> []
    in
    
    List.map (fun i -> Instruction i) cond_instrs
    @ [Instruction (Beq (cond_reg, Zero, else_label))]
    @ then_items
    @ [Instruction (J end_label); Label else_label]
    @ else_items
    @ [Label end_label]
  
  | Ast.While (cond, body) ->
    let loop_label = new_label ctx "loop" in
    let end_label = new_label ctx "endloop" in
    ctx.break_labels <- end_label :: ctx.break_labels;
    ctx.continue_labels <- loop_label :: ctx.continue_labels;
    
    let cond_reg, cond_instrs = gen_expr ctx cond in
    let body_items = gen_stmt ctx frame_size body in
    
    ctx.break_labels <- List.tl ctx.break_labels;
    ctx.continue_labels <- List.tl ctx.continue_labels;
    
    [Label loop_label]
    @ List.map (fun i -> Instruction i) cond_instrs
    @ [Instruction (Beq (cond_reg, Zero, end_label))]
    @ body_items
    @ [Instruction (J loop_label); Label end_label]
  
  | Ast.Break ->
    (match ctx.break_labels with
     | label :: _ -> [Instruction (J label)]
     | [] -> failwith "break语句不在循环中")
  
  | Ast.Continue ->
    (match ctx.continue_labels with
     | label :: _ -> [Instruction (J label)]
     | [] -> failwith "continue语句不在循环中")
  
  | Ast.Decl (name, e) ->
    let offset = add_local_var ctx name in
    let e_reg, e_instrs = gen_expr ctx e in
    let instrs = e_instrs @ [Sw (e_reg, offset, S0)] in  (* 使用S0作为帧指针 *)
    List.map (fun i -> Instruction i) instrs
  
  | Ast.Assign (name, e) ->
    let offset = get_var_offset ctx name in
    let e_reg, e_instrs = gen_expr ctx e in
    let instrs = e_instrs @ [Sw (e_reg, offset, S0)] in  (* 使用S0作为帧指针 *)
    List.map (fun i -> Instruction i) instrs

(* 计算函数所需的栈帧大小 *)
let calculate_frame_size ctx num_params =
  (* 已分配的局部变量空间 *)
  let local_vars_space = -ctx.stack_offset - 8 in  (* 减去ra和fp的8字节 *)
  
  (* 被调用者保存寄存器的空间 *)
  let saved_regs_space = List.length ctx.used_saved_regs * 4 in
  
  (* 参数空间 (对于超过8个的参数) *)
  let params_space = max 0 (num_params - 8) * 4 in
  
  (* 总需求空间: 局部变量 + 保存寄存器 + 8字节(ra和fp) *)
  let total_space = 8 + local_vars_space + saved_regs_space + params_space in
  
  (* 16字节对齐 *)
  if total_space mod 16 = 0 then total_space else total_space + (16 - total_space mod 16)

(* 生成函数代码 *)
let gen_function symbol_table (func_def: Ast.func_def) : asm_item list =
  let ctx = create_context symbol_table func_def.fname in
  let num_params = List.length func_def.params in
  
  (* 1. 处理参数：映射到寄存器或栈 *)
  let param_instrs =
    List.mapi (fun i { Ast.pname = name; _ } ->
      let offset = add_local_var ctx name in
      if i < 8 then
        (* 前8个参数使用A0-A7 *)
        let arg_reg = match i with
          | 0 -> A0 | 1 -> A1 | 2 -> A2 | 3 -> A3
          | 4 -> A4 | 5 -> A5 | 6 -> A6 | 7 -> A7
          | _ -> failwith "无效的参数索引"
        in
        [Instruction (Sw (arg_reg, offset, S0))]  (* 保存到栈帧 *)
      else
        (* 超过8个的参数从栈中读取 *)
        let stack_offset = (i - 8) * 4 + 16 in  (* 跳过ra(4)、fp(4)和前8个参数(32) *)
        [Instruction (Lw (T0, stack_offset, Sp));
         Instruction (Sw (T0, offset, S0))]
    ) func_def.params
    |> List.flatten
  in
  
  (* 2. 生成函数体 *)
  let body_items = List.concat_map (gen_stmt ctx 0) func_def.body in
  
  (* 3. 计算栈帧大小 *)
  let frame_size = calculate_frame_size ctx num_params in
  
  (* 4. 生成序言和尾声 *)
  let prologue = gen_prologue ctx frame_size in
  let has_explicit_return =
    List.exists (function
      | Instruction Ret -> true
      | _ -> false) body_items
  in
  let epilogue = if has_explicit_return then [] 
                 else List.map (fun i -> Instruction i) (gen_epilogue ctx frame_size) in
  
  (* 组合所有部分 *)
  [Label func_def.fname; Comment ("函数: " ^ func_def.fname)]
  @ prologue
  @ param_instrs
  @ body_items
  @ epilogue

(* 生成整个程序的代码 *)
let gen_program symbol_table (program: Ast.program) =
  let header = [
    Directive ".text";
    Directive ".globl main";
    Comment "ToyC编译器生成的RISC-V汇编代码"
  ] in
  
  let func_asm_items =
    List.concat_map (fun func_def ->
      gen_function symbol_table func_def
    ) program
  in
  
  header @ func_asm_items

(* 编译为RISC-V汇编并输出到标准输出 *)
let compile_to_riscv symbol_table program =
  let asm_items = gen_program symbol_table program in
  List.iter (fun item -> print_endline (asm_item_to_string item)) asm_items

