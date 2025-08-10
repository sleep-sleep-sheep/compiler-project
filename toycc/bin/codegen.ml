(* RISC-V 32位寄存器 *)
type reg =
  | Zero 
  | Ra 
  | Sp 
  | Gp 
  | Tp 
  | T0 
  | T1 
  | T2 
  | S0  (* 使用S0作为帧指针，替代原来的Fp *)
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
  | S0 -> "s0"  (* 帧指针寄存器 *)
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


(* 将指令转换为字符串 *)
let instr_to_string instr = match instr with
  | Add (rd, rs1, rs2) ->
    Printf.sprintf
      "add %s, %s, %s"
      (reg_to_string rd)
      (reg_to_string rs1)
      (reg_to_string rs2)
  | Addi (rd, rs1, imm) ->
    Printf.sprintf "addi %s, %s, %d" (reg_to_string rd) (reg_to_string rs1) imm
  | Sub (rd, rs1, rs2) ->
    Printf.sprintf
      "sub %s, %s, %s"
      (reg_to_string rd)
      (reg_to_string rs1)
      (reg_to_string rs2)
  | Mul (rd, rs1, rs2) ->
    Printf.sprintf
      "mul %s, %s, %s"
      (reg_to_string rd)
      (reg_to_string rs1)
      (reg_to_string rs2)
  | Div (rd, rs1, rs2) ->
    Printf.sprintf
      "div %s, %s, %s"
      (reg_to_string rd)
      (reg_to_string rs1)
      (reg_to_string rs2)
  | Rem (rd, rs1, rs2) ->
    Printf.sprintf
      "rem %s, %s, %s"
      (reg_to_string rd)
      (reg_to_string rs1)
      (reg_to_string rs2)
  | And (rd, rs1, rs2) ->
    Printf.sprintf
      "and %s, %s, %s"
      (reg_to_string rd)
      (reg_to_string rs1)
      (reg_to_string rs2)
  | Or (rd, rs1, rs2) ->
    Printf.sprintf
      "or %s, %s, %s"
      (reg_to_string rd)
      (reg_to_string rs1)
      (reg_to_string rs2)
  | Xor (rd, rs1, rs2) ->
    Printf.sprintf
      "xor %s, %s, %s"
      (reg_to_string rd)
      (reg_to_string rs1)
      (reg_to_string rs2)
  | Xori (rd, rs1, imm) ->
    Printf.sprintf "xori %s, %s, %d" (reg_to_string rd) (reg_to_string rs1) imm
  | Slt (rd, rs1, rs2) ->
    Printf.sprintf
      "slt %s, %s, %s"
      (reg_to_string rd)
      (reg_to_string rs1)
      (reg_to_string rs2)
  | Slti (rd, rs1, imm) ->
    Printf.sprintf "slti %s, %s, %d" (reg_to_string rd) (reg_to_string rs1) imm
  | Sltu (rd, rs1, rs2) ->
    Printf.sprintf
      "sltu %s, %s, %s"
      (reg_to_string rd)
      (reg_to_string rs1)
      (reg_to_string rs2)
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


(*---------------------------------------------------------*)

(* 代码生成上下文 *)
type codegen_context =
  { mutable label_counter : int (* 标签计数器 *)
  ; mutable temp_counter : int (* 临时寄存器计数器 *)
  ; mutable stack_offset : int (* 当前栈偏移 *)
  ; mutable break_labels : string list (* break 跳转标签栈 *)
  ; mutable continue_labels : string list (* continue 跳转标签栈 *)
  ; mutable local_vars : (string * int) list (* 局部变量映射到栈偏移 *)
  ; mutable used_regs : reg list (* 跟踪已使用的寄存器 *)
  ; func_name : string (* 函数名，用于生成唯一标签 *)
  }


(* 创建新的代码生成上下文 *)
let create_context _symbol_table func_name =
  { label_counter = 0;
    temp_counter = 0;    
    stack_offset = -8 ;(* 基于S0的偏移，从0开始向下增长 *)
    break_labels = [];
    continue_labels = [];
    local_vars = [];
    used_regs = [];  (* 初始化空的已使用寄存器列表 *)
    func_name = func_name  (* 存储当前函数名 *)
  }


(* 生成新标签 - 使用函数名作为前缀确保标签唯一性 *)
let new_label ctx prefix =
  let label = Printf.sprintf "%s_%s%d" ctx.func_name prefix ctx.label_counter in
  ctx.label_counter <- ctx.label_counter + 1;
  label


(* 获取临时寄存器 *)
let get_temp_reg ctx =
  (* 可用的临时寄存器列表 *)
  let temp_regs = [T0; T1; T2; T3; T4; T5; T6] in
  
  (* 寻找第一个未使用的寄存器 *)
  let rec find_unused_reg regs =
    match regs with
    | [] -> 
        (* 所有临时寄存器都在使用中，需要溢出到栈 *)
        let spill_offset = ctx.stack_offset in
        ctx.stack_offset <- ctx.stack_offset - 4;
        
        (* 选择一个寄存器进行溢出 - 简单选择第一个 *)
        let reg_to_spill = List.hd temp_regs in
        
        (* 保存寄存器内容到栈，使用S0作为帧指针 *)
        let spill_instr = Sw (reg_to_spill, spill_offset, S0) in
        
        (* 标记该寄存器为未使用 *)
        ctx.used_regs <- List.filter (fun r -> r <> reg_to_spill) ctx.used_regs;
        
        (* 返回该寄存器和溢出指令 *)
        (reg_to_spill, [spill_instr])
    | reg::rest ->
        if List.mem reg ctx.used_regs then
          find_unused_reg rest
        else
          (reg, [])
  in
  
  let reg, spill_instrs = find_unused_reg temp_regs in
  
  (* 标记寄存器为已使用 *)
  ctx.used_regs <- reg :: ctx.used_regs;
  
  (* 返回寄存器和可能的溢出指令 *)
  (reg, spill_instrs)


(* 释放临时寄存器 *)
let free_temp_reg ctx reg =
  ctx.used_regs <- List.filter (fun r -> r <> reg) ctx.used_regs


(* 将变量添加到栈中 *)
let add_local_var ctx name =
  ctx.stack_offset <- ctx.stack_offset - 4;
  ctx.local_vars <- (name, ctx.stack_offset) :: ctx.local_vars;
  ctx.stack_offset


(* 获取变量的栈偏移 *)
let get_var_offset ctx name =
  match List.assoc_opt name ctx.local_vars with
  | Some offset -> offset
  | None -> 
      failwith (Printf.sprintf "Variable '%s' not found in scope (available: %s)"
        name 
        (String.concat ", " (List.map fst ctx.local_vars)))


(* 生成表达式代码，返回结果寄存器和指令列表 *)
let rec gen_expr ctx (expr : Ast.expr) : reg * instruction list =
  match expr with
  | Ast.Literal(IntLit n) ->
    let (reg, spill_instrs) = get_temp_reg ctx in
    let instr = [ Li (reg, n) ] in
    reg, spill_instrs @ instr
  | Ast.Var id ->
    let (reg, spill_instrs) = get_temp_reg ctx in
    let offset = get_var_offset ctx id in
    let instr = [ Lw (reg, offset, S0) ] in  (* 使用S0作为帧指针 *)
    reg, spill_instrs @ instr
  | Ast.Paren e -> gen_expr ctx e
  | Ast.UnOp (op, e) ->
    let e_reg, e_instrs = gen_expr ctx e in
    let (result_reg, spill_instrs) = get_temp_reg ctx in
    let instrs =
      match op with
      | "+" -> [ Mv (result_reg, e_reg) ]  (* 一元+直接复制寄存器值 *)
      | "-" -> [ Sub (result_reg, Zero, e_reg) ]
      | "!" -> [ Sltiu (result_reg, e_reg, 1) ]
      | _ -> failwith (Printf.sprintf "Unknown unary operator: %s" op)
    in
    (* 释放临时寄存器 *)
    free_temp_reg ctx e_reg;
    result_reg, e_instrs @ spill_instrs @ instrs
  | Ast.BinOp (e1, op, e2) ->
    let e1_reg, e1_instrs = gen_expr ctx e1 in
    let e2_reg, e2_instrs = gen_expr ctx e2 in
    let (result_reg, spill_instrs) = get_temp_reg ctx in
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
    (* 释放临时寄存器 *)
    free_temp_reg ctx e1_reg;
    free_temp_reg ctx e2_reg;
    let instrs = e1_instrs @ e2_instrs @ spill_instrs @ op_instrs in
    result_reg, instrs
  | Ast.Call (fname, args) ->
    let result_reg = A0 in
    let num_args = List.length args in
    
    (* 计算需要保存到栈的参数数量 *)
    let stack_args = if num_args > 8 then num_args - 8 else 0 in
    let stack_arg_size = stack_args * 4 in
    
    (* 计算需要保存的寄存器 - 只保存s寄存器，t寄存器可以被调用者破坏 *)
    let save_regs = [S0; S1; S2; S3; S4; S5; S6; S7; S8; S9; S10; S11] in
    let num_save_regs = List.length save_regs in
    let save_regs_size = num_save_regs * 4 in
    
    (* 为栈参数和保存的寄存器分配栈空间 *)
    let stack_alloc_size = stack_arg_size + save_regs_size in
    let stack_alloc_instr = if stack_alloc_size > 0 then [Addi (Sp, Sp, -stack_alloc_size)] else [] in
    
    (* 保存寄存器 - 先移动栈指针，再保存寄存器 *)
    let save_instrs = 
      List.mapi (fun i reg -> 
        Sw (reg, i * 4, Sp)
      ) save_regs
    in
    
    (* 处理参数 - 前8个参数放入a0-a7，其余放入栈 *)
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
               (* 超过8个的参数放入栈 *)
               let stack_pos = save_regs_size + (i - 8) * 4 in
               arg_code @ [ Sw (arg_reg, stack_pos, Sp) ]
           in
           (* 释放参数寄存器 *)
           free_temp_reg ctx arg_reg;
           instrs)
        args
      |> List.flatten
    in
    
    (* 函数调用 *)
    let call_instr = [ Jal (Ra, fname) ] in
    
    (* 恢复寄存器 - 先恢复寄存器，再释放栈空间 *)
    let restore_instrs = 
      List.mapi (fun i reg -> 
        Lw (reg, i * 4, Sp)
      ) save_regs
    in
    
    (* 释放栈空间 *)
    let stack_free_instr = if stack_alloc_size > 0 then [Addi (Sp, Sp, stack_alloc_size)] else [] in
    
    result_reg, stack_alloc_instr @ save_instrs @ arg_instrs @ call_instr @ restore_instrs @ stack_free_instr   


(* 生成序言 *)
let gen_prologue_instrs frame_size =
  [ Instruction(Addi (Sp, Sp, -frame_size));
    Instruction(Sw (Ra, frame_size - 4, Sp));  (* 保存返回地址 *)
    Instruction(Sw (S0, frame_size - 8, Sp));  (* 保存S0寄存器(帧指针) *)
    Instruction(Sw (S1, frame_size - 12, Sp)); (* 保存S1寄存器 *)
    Instruction(Addi (S0, Sp, frame_size))    (* 设置S0作为帧指针 *)
  ]

(* 生成尾声 *)
let gen_epilogue_instrs frame_size =
  [ Lw (Ra, frame_size - 4, Sp);  (* 恢复返回地址 *)
    Lw (S0, frame_size - 8, Sp);  (* 恢复S0寄存器(帧指针) *)
    Lw (S1, frame_size - 12, Sp); (* 恢复S1寄存器 *)
    Addi (Sp, Sp, frame_size);    (* 释放栈帧 *)
    Ret                           (* 返回 *)
  ]

(* 生成语句代码 *)
let rec gen_stmt ctx frame_size (stmt : Ast.stmt) : asm_item list =
  match stmt with
  | Ast.Empty -> []
  | Ast.ExprStmt e ->
    let reg, instrs = gen_expr ctx e in
    (* 释放表达式使用的寄存器 *)
    free_temp_reg ctx reg;
    List.map (fun i -> Instruction i) instrs
  | Ast.Block stmts ->
    let old_vars = ctx.local_vars in
    let old_offset = ctx.stack_offset in
    let old_used_regs = ctx.used_regs in
    let items = List.map (gen_stmt ctx frame_size) stmts |> List.flatten in
    ctx.local_vars <- old_vars;
    ctx.stack_offset <- old_offset;
    ctx.used_regs <- old_used_regs;
    items
  | Ast.Return (Some e) ->
    (* 优化简单的0常量返回 *)
    (match e with
     | Ast.Literal(IntLit 0) ->
       let all_instrs = [ Li (A0, 0) ] @ gen_epilogue_instrs frame_size in
       List.map (fun i -> Instruction i) all_instrs
     | _ ->
       let e_reg, e_instrs = gen_expr ctx e in
       let all_instrs = e_instrs @ [ Mv (A0, e_reg) ] @ gen_epilogue_instrs frame_size in
       free_temp_reg ctx e_reg;
       List.map (fun i -> Instruction i) all_instrs)
  | Ast.Return None -> List.map (fun i -> Instruction i) (gen_epilogue_instrs frame_size)
  | Ast.If (cond, then_stmt, else_stmt) ->
    let cond_reg, cond_instrs = gen_expr ctx cond in
    let else_label = new_label ctx "else" in
    let end_label = new_label ctx "endif" in
    let then_items = gen_stmt ctx frame_size then_stmt in
    let else_items =
      match else_stmt with
      | Some s -> gen_stmt ctx frame_size s
      | None -> []
    in
    (* 释放条件寄存器 *)
    free_temp_reg ctx cond_reg;
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
    let body_items = gen_stmt ctx frame_size body in
    ctx.break_labels <- List.tl ctx.break_labels;
    ctx.continue_labels <- List.tl ctx.continue_labels;
    (* 释放条件寄存器 *)
    free_temp_reg ctx cond_reg;
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
    let offset = add_local_var ctx name in
    let e_reg, e_instrs = gen_expr ctx e in
    let all_instrs = e_instrs @ [ Sw (e_reg, offset, S0) ] in  (* 使用S0作为帧指针 *)
    free_temp_reg ctx e_reg;
    List.map (fun i -> Instruction i) all_instrs
  | Ast.Assign (name, e) ->
    let offset = get_var_offset ctx name in
    let e_reg, e_instrs = gen_expr ctx e in
    let all_instrs = e_instrs @ [ Sw (e_reg, offset, S0) ] in  (* 使用S0作为帧指针 *)
    free_temp_reg ctx e_reg;
    List.map (fun i -> Instruction i) all_instrs


(* 计算函数所需的栈帧大小 *)
let calculate_frame_size (func_def : Ast.func_def) =
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
  (* 注意 func_def.body 是 stmt list，要先统计每个 stmt 的 decl *)
  let num_locals =
    List.fold_left (fun acc stmt -> acc + count_decls_in_stmt stmt) 0 func_def.body in
  let num_params = List.length func_def.params in
  
  (* 计算超过8个的参数数量 *)
  let extra_params = max 0 (num_params - 8) in
  
  (* ra, s0, s1 + 额外参数 + 局部变量 *)
  let required_space = 12 + (extra_params * 4) + (num_locals * 4) in
  
  (* 对齐到16字节 *)
  (required_space + 15) / 16 * 16


(* 生成函数代码 *)
let gen_function symbol_table (func_def : Ast.func_def) : asm_item list =
  (* 创建上下文时传入函数名，用于生成唯一标签 *)
  let ctx = create_context symbol_table func_def.fname in
  (* 计算栈帧 *)
  let frame_size = calculate_frame_size func_def in
  (* 函数序言 *)
  let prologue = gen_prologue_instrs frame_size  in
  (* 处理参数 *)
  let param_instrs =
    List.mapi
      (fun i { Ast.pname = name; _ } ->
       let offset = add_local_var ctx name in
       let (arg_reg, spill_instrs) = 
         if i < 8 then
           (* 前8个参数使用a0-a7寄存器 *)
           (match i with
            | 0 -> (A0, []) | 1 -> (A1, []) | 2 -> (A2, []) | 3 -> (A3, [])
            | 4 -> (A4, []) | 5 -> (A5, []) | 6 -> (A6, []) | 7 -> (A7, [])
            | _ -> failwith "Invalid parameter index")
         else
           (* 超过8个的参数从栈中获取 *)
           let stack_offset = 12 + (i - 8) * 4 in  (* 跳过ra, s0, s1的空间 *)
           let reg, spill = get_temp_reg ctx in
           (reg, spill @ [Lw (reg, stack_offset, Sp)])
       in
       (* 生成指令列表 *)
       spill_instrs @ [ Sw (arg_reg, offset, S0) ])  (* 使用S0作为帧指针 *)
      func_def.params
    |> List.flatten  (* 将列表的列表展平为单一列表 *)
    |> List.map (fun instr -> Instruction instr)  (* 转换为asm_item类型 *)
  in
  (* 函数体 *)
  let body_items =
    func_def.body
    |> List.map (gen_stmt ctx frame_size)
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
    if has_ret
    then []
    else List.map (fun i -> Instruction i) (gen_epilogue_instrs frame_size)
  in
  [ Label func_def.fname; Comment ("Function: " ^ func_def.fname) ] @ prologue @ param_instrs @ body_items @ epilogue


(* 生成整个程序的代码 *)
let gen_program symbol_table (program : Ast.program) =
  (* 全局声明 *)
  let header =
    [ Directive ".text"; 
      Directive ".globl main"; 
      Comment "ToyC Compiler Generated Code";
      Comment "遵循RISC-V ABI规范" ]
  in
  (* 生成所有函数 *)
  let func_asm_items =
    List.map
      (fun func_def ->
         gen_function symbol_table func_def)
      program
    |> List.flatten
  in
  header @ func_asm_items


let compile_to_riscv symbol_table program =
  let asm_items = gen_program symbol_table program in
  (* 直接将汇编项转换为字符串并打印到标准输出 *)
  List.iter
    (fun item -> print_endline (asm_item_to_string item))
    asm_items














