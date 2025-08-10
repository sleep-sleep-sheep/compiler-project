open Ast

(* RISC-V 32位寄存器定义 *)
type reg =
  | Zero | Ra | Sp | Gp | Tp 
  | T0 | T1 | T2 | Fp | S1 
  | A0 | A1 | A2 | A3 | A4 | A5 | A6 | A7 
  | S2 | S3 | S4 | S5 | S6 | S7 | S8 | S9 | S10 | S11 
  | T3 | T4 | T5 | T6 

(* 寄存器到字符串的转换 *)
let reg_to_string reg = 
  match reg with
  | Zero -> "zero" | Ra -> "ra" | Sp -> "sp" | Gp -> "gp" | Tp -> "tp"
  | T0 -> "t0" | T1 -> "t1" | T2 -> "t2" | Fp -> "fp" | S1 -> "s1"
  | A0 -> "a0" | A1 -> "a1" | A2 -> "a2" | A3 -> "a3"
  | A4 -> "a4" | A5 -> "a5" | A6 -> "a6" | A7 -> "a7"
  | S2 -> "s2" | S3 -> "s3" | S4 -> "s4" | S5 -> "s5"
  | S6 -> "s6" | S7 -> "s7" | S8 -> "s8" | S9 -> "s9"
  | S10 -> "s10" | S11 -> "s11"
  | T3 -> "t3" | T4 -> "t4" | T5 -> "t5" | T6 -> "t6"

(* RISC-V指令类型 *)
type instruction =
  (* 算术指令 *)
  | Add of reg * reg * reg | Addi of reg * reg * int
  | Sub of reg * reg * reg | Mul of reg * reg * reg
  | Div of reg * reg * reg | Rem of reg * reg * reg
  (* 逻辑指令 *)
  | And of reg * reg * reg | Or of reg * reg * reg
  | Xor of reg * reg * reg | Xori of reg * reg * int
  (* 比较指令 *)
  | Slt of reg * reg * reg | Slti of reg * reg * int
  | Sltu of reg * reg * reg | Sltiu of reg * reg * int
  (* 加载/存储指令 *)
  | Lw of reg * int * reg | Sw of reg * int * reg
  (* 分支指令 *)
  | Beq of reg * reg * string | Bne of reg * reg * string
  | Blt of reg * reg * string | Bge of reg * reg * string
  (* 跳转指令 *)
  | J of string | Jal of reg * string | Jalr of reg * reg * int | Ret
  (* 立即数加载 *)
  | Li of reg * int | Lui of reg * int
  (* 移动指令 *)
  | Mv of reg * reg
  (* 其他 *)
  | Nop

(* 指令到字符串的转换 *)
let instr_to_string instr = match instr with
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

(* 汇编项到字符串的转换 *)
let asm_item_to_string item = match item with 
  | Instruction instr -> "    " ^ instr_to_string instr
  | Label l -> l ^ ":"
  | Directive d -> "    " ^ d
  | Comment c -> "    # " ^ c

(* 代码生成上下文 *)
type codegen_context =
  { mutable label_counter : int
  ; mutable temp_counter : int
  ; mutable stack_offset : int
  ; mutable break_labels : string list
  ; mutable continue_labels : string list
  ; mutable local_vars : (string * int) list
  ; func_name : string
  }

(* 创建新的代码生成上下文 *)
let create_context _symbol_table func_name =
  { label_counter = 0;
    temp_counter = 0;
    stack_offset = -8;  (* 从-8开始，为ra和fp预留空间 *)
    break_labels = [];
    continue_labels = [];
    local_vars = [];
    func_name = func_name
  }

(* 生成新标签 *)
let new_label ctx prefix =
  let label = Printf.sprintf "%s_%s%d" ctx.func_name prefix ctx.label_counter in
  ctx.label_counter <- ctx.label_counter + 1;
  label

(* 获取临时寄存器 *)
let get_temp_reg ctx =
  let reg =
    match ctx.temp_counter mod 7 with
    | 0 -> T0 | 1 -> T1 | 2 -> T2 | 3 -> T3 | 4 -> T4 | 5 -> T5 | 6 -> T6
    | _ -> failwith "Invalid temp register index"
  in
  ctx.temp_counter <- ctx.temp_counter + 1;
  reg

(* 重置临时寄存器计数器 *)
let reset_temp_regs ctx =
  ctx.temp_counter <- 0

(* 添加局部变量到栈 *)
let add_local_var ctx name =
  ctx.stack_offset <- ctx.stack_offset - 4;
  (* 确保栈偏移是4的倍数 *)
  if ctx.stack_offset mod 4 != 0 then
    ctx.stack_offset <- ctx.stack_offset - (ctx.stack_offset mod 4);
  ctx.local_vars <- (name, ctx.stack_offset) :: ctx.local_vars;
  ctx.stack_offset

(* 获取变量的栈偏移 *)
let get_var_offset ctx name =
  match List.assoc_opt name ctx.local_vars with
  | Some offset -> offset
  | None -> 
      failwith (Printf.sprintf "Variable '%s' not found in scope" name)

(* 生成表达式代码 *)
let rec gen_expr ctx (expr : Ast.expr) : reg * instruction list * asm_item list =
  match expr with
  | Ast.Literal(IntLit n) ->
    let reg = get_temp_reg ctx in
    let instr = [ Li (reg, n) ] in
    reg, instr, []
  | Ast.Var id ->
    let reg = get_temp_reg ctx in
    let offset = get_var_offset ctx id in
    let instr = [ Lw (reg, offset, Fp) ] in
    reg, instr, []
  | Ast.Paren e -> gen_expr ctx e
  | Ast.UnOp (op, e) ->
    let e_reg, e_instrs, e_items = gen_expr ctx e in
    let result_reg = get_temp_reg ctx in
    let (instrs, items) =
      match op with
      | "-" -> ([ Sub (result_reg, Zero, e_reg) ], [])
      | "+" -> ([ Mv (result_reg, e_reg) ], [])
      | "!" -> ([ Sltiu (result_reg, e_reg, 1) ], [])
      | _ -> failwith (Printf.sprintf "Unknown unary operator: %s" op)
    in
    result_reg, e_instrs @ instrs, e_items @ items
  | Ast.BinOp (e1, op, e2) ->
    let e1_reg, e1_instrs, e1_items = gen_expr ctx e1 in
    let e2_reg, e2_instrs, e2_items = gen_expr ctx e2 in
    let result_reg = get_temp_reg ctx in
    
    (* 保存e1的结果，如果e2的计算可能会覆盖它 *)
    let save_e1 = if List.length e2_instrs > 0 then
      let temp = get_temp_reg ctx in
      ([Mv (temp, e1_reg)], temp)
    else ([], e1_reg) in
    
    let (save_instrs, e1_reg_saved) = save_e1 in
    
    let (op_instrs, op_items) =
      match op with
      | "+" -> ([ Add (result_reg, e1_reg_saved, e2_reg) ], [])
      | "-" -> ([ Sub (result_reg, e1_reg_saved, e2_reg) ], [])
      | "*" -> ([ Mul (result_reg, e1_reg_saved, e2_reg) ], [])
      | "/" -> ([ Div (result_reg, e1_reg_saved, e2_reg) ], [])
      | "%" -> ([ Rem (result_reg, e1_reg_saved, e2_reg) ], [])
      | "==" ->  
          ([ Sub (T0, e1_reg_saved, e2_reg); 
             Sltiu (result_reg, T0, 1) ], [])
      | "!=" -> 
          ([ Sub (T0, e1_reg_saved, e2_reg); 
             Sltu (result_reg, Zero, T0) ], [])
      | "<" -> ([ Slt (result_reg, e1_reg_saved, e2_reg) ], [])
      | "<=" -> 
          ([ Slt (T0, e2_reg, e1_reg_saved); 
             Xori (result_reg, T0, 1) ], [])
      | ">" -> ([ Slt (result_reg, e2_reg, e1_reg_saved) ], [])
      | ">=" -> 
          ([ Slt (T0, e1_reg_saved, e2_reg); 
             Xori (result_reg, T0, 1) ], [])
      | "&&" ->
        let false_label = new_label ctx "and_false" in
        let end_label = new_label ctx "and_end" in
        let instrs = [
          Sltu (T0, Zero, e1_reg_saved);     (* T0 = e1 != 0 *)
          Beq (T0, Zero, false_label);       (* 如果e1为假，跳转到false_label *)
          Sltu (result_reg, Zero, e2_reg);   (* result_reg = e2 != 0 *)
          J end_label;                       (* 跳过false分支 *)
        ] in
        let items = [
          Label false_label;
          Instruction (Li (result_reg, 0));
          Label end_label
        ] in
        (instrs, items)
      | "||" ->
        let true_label = new_label ctx "or_true" in
        let end_label = new_label ctx "or_end" in
        let instrs = [
          Sltu (T0, Zero, e1_reg_saved);     (* T0 = e1 != 0 *)
          Bne (T0, Zero, true_label);        (* 如果e1为真，跳转到true_label *)
          Sltu (result_reg, Zero, e2_reg);   (* result_reg = e2 != 0 *)
          J end_label;                       (* 跳过true分支 *)
        ] in
        let items = [
          Label true_label;
          Instruction (Li (result_reg, 1));
          Label end_label
        ] in
        (instrs, items)
      | _ -> failwith (Printf.sprintf "Unknown binary operator: %s" op)
    in
    
    let all_instrs = e1_instrs @ save_instrs @ e2_instrs @ op_instrs in
    let all_items = e1_items @ e2_items @ op_items in
    result_reg, all_instrs, all_items
  | Ast.Call (fname, args) ->
    let result_reg = A0 in
    (* 保存临时寄存器和A0-A7寄存器 *)
    let save_instrs = [
      Addi (Sp, Sp, -60);  (* 扩大栈空间保存寄存器 *)
      Sw (T0, 0, Sp);
      Sw (T1, 4, Sp);
      Sw (T2, 8, Sp);
      Sw (T3, 12, Sp);
      Sw (T4, 16, Sp);
      Sw (T5, 20, Sp);
      Sw (T6, 24, Sp);
      Sw (A0, 28, Sp);    (* 保存A0-A7寄存器 *)
      Sw (A1, 32, Sp);
      Sw (A2, 36, Sp);
      Sw (A3, 40, Sp);
      Sw (A4, 44, Sp);
      Sw (A5, 48, Sp);
      Sw (A6, 52, Sp);
      Sw (A7, 56, Sp);
    ] in
    
    (* 处理函数参数 *)
    let arg_instrs, arg_items = 
      let num_args = List.length args in
      let stack_args = max 0 (num_args - 8) in
      
      (* 为栈上的参数预留空间，确保16字节对齐 *)
      let stack_alloc = 
        if stack_args > 0 then 
          let alloc = 4 * stack_args in
          let alloc_aligned = (alloc + 15) / 16 * 16 in
          [Addi (Sp, Sp, -alloc_aligned)] 
        else [] 
      in
      
      (* 处理寄存器参数 (A0-A7) 和栈参数 *)
      let process_arg (i, arg) =
        let arg_reg, arg_code, arg_items = gen_expr ctx arg in
        if i < 8 then
          (* 前8个参数使用寄存器A0-A7 *)
          let target_reg = 
            match i with
            | 0 -> A0 | 1 -> A1 | 2 -> A2 | 3 -> A3
            | 4 -> A4 | 5 -> A5 | 6 -> A6 | 7 -> A7
            | _ -> failwith "Invalid register index"
          in
          (arg_code @ [Mv (target_reg, arg_reg)], arg_items)
        else
          (* 超过8个的参数放到栈上 *)
          let stack_pos = 4 * (i - 8) in
          (arg_code @ [Sw (arg_reg, stack_pos, Sp)], arg_items)
      in
      
      let indexed_args = List.mapi (fun i a -> (i, a)) args in
      let arg_codes, arg_item_lists = List.split (List.map process_arg indexed_args) in
      
      let all_arg_instrs = stack_alloc @ List.flatten arg_codes in
      let all_arg_items = List.flatten arg_item_lists in
      
      (all_arg_instrs, all_arg_items)
    in
    
    (* 函数调用 *)
    let call_instr = [ Jal (Ra, fname) ] in
    
    (* 恢复栈空间（如果有栈参数） *)
    let stack_dealloc = 
      let num_args = List.length args in
      let stack_args = max 0 (num_args - 8) in
      if stack_args > 0 then 
        let alloc = 4 * stack_args in
        let alloc_aligned = (alloc + 15) / 16 * 16 in
        [Addi (Sp, Sp, alloc_aligned)] 
      else [] 
    in
    
    (* 恢复临时寄存器和A0-A7寄存器 *)
    let restore_instrs = [
      Lw (T0, 0, Sp);
      Lw (T1, 4, Sp);
      Lw (T2, 8, Sp);
      Lw (T3, 12, Sp);
      Lw (T4, 16, Sp);
      Lw (T5, 20, Sp);
      Lw (T6, 24, Sp);
      Lw (A0, 28, Sp);    (* 恢复A0-A7寄存器 *)
      Lw (A1, 32, Sp);
      Lw (A2, 36, Sp);
      Lw (A3, 40, Sp);
      Lw (A4, 44, Sp);
      Lw (A5, 48, Sp);
      Lw (A6, 52, Sp);
      Lw (A7, 56, Sp);
      Addi (Sp, Sp, 60);
    ] in
    
    let all_instrs = save_instrs @ arg_instrs @ call_instr @ stack_dealloc @ restore_instrs in
    result_reg, all_instrs, arg_items

(* 生成函数序言 *)
let gen_prologue_instrs frame_size =
  [ Instruction(Addi (Sp, Sp, -frame_size));
    Instruction(Sw (Ra, frame_size - 4, Sp));  (* 保存返回地址 *)
    Instruction(Sw (Fp, frame_size - 8, Sp));  (* 保存帧指针 *)
    Instruction(Addi (Fp, Sp, frame_size))     (* 设置新的帧指针 *)
  ]

(* 生成函数尾声 *)
let gen_epilogue_instrs frame_size =
  [ Instruction(Lw (Ra, frame_size - 4, Sp));  (* 恢复返回地址 *)
    Instruction(Lw (Fp, frame_size - 8, Sp));  (* 恢复帧指针 *)
    Instruction(Addi (Sp, Sp, frame_size));    (* 恢复栈指针 *)
    Instruction(Ret)                           (* 返回 *)
  ]

(* 生成语句代码 *)
let rec gen_stmt ctx frame_size (stmt : Ast.stmt) : asm_item list =
  match stmt with
  | Ast.Empty -> []
  | Ast.ExprStmt e ->
    let _, instrs, items = gen_expr ctx e in
    items @ List.map (fun i -> Instruction i) instrs
  | Ast.Block stmts ->
    (* 保存当前变量状态以便块结束后恢复 *)
    let old_vars = ctx.local_vars in
    let old_offset = ctx.stack_offset in
    let items = List.map (gen_stmt ctx frame_size) stmts |> List.flatten in
    ctx.local_vars <- old_vars;  (* 恢复变量作用域 *)
    ctx.stack_offset <- old_offset;
    items
  | Ast.Return (Some e) ->
    let reg, expr_instrs, expr_items = gen_expr ctx e in
    reset_temp_regs ctx;  (* 重置临时寄存器计数器 *)
    (* 确保返回值存入A0寄存器 *)
    expr_items 
    @ List.map (fun i -> Instruction i) expr_instrs 
    @ [ Instruction (Mv (A0, reg)) ]  (* 将计算结果移动到A0 *)
    @ gen_epilogue_instrs frame_size
  | Ast.Return None -> 
    (* 无返回值的函数返回0 *)
    reset_temp_regs ctx;
    [ Instruction (Li (A0, 0)) ] @ gen_epilogue_instrs frame_size
  | Ast.If (cond, then_stmt, else_stmt) ->
    let cond_reg, cond_instrs, cond_items = gen_expr ctx cond in
    let else_label = new_label ctx "else" in
    let end_label = new_label ctx "endif" in
    let then_items = gen_stmt ctx frame_size then_stmt in
    let else_items =
      match else_stmt with
      | Some s -> gen_stmt ctx frame_size s
      | None -> []
    in
    cond_items
    @ List.map (fun i -> Instruction i) cond_instrs
    @ [ Instruction (Beq (cond_reg, Zero, else_label)) ]  (* 条件为假时跳转到else *)
    @ then_items
    @ [ Instruction (J end_label); Label else_label ]
    @ else_items
    @ [ Label end_label ]
  | Ast.While (cond, body) ->
    let loop_label = new_label ctx "loop" in
    let end_label = new_label ctx "endloop" in
    (* 保存break和continue标签 *)
    ctx.break_labels <- end_label :: ctx.break_labels;
    ctx.continue_labels <- loop_label :: ctx.continue_labels;
    let cond_reg, cond_instrs, cond_items = gen_expr ctx cond in
    let body_items = gen_stmt ctx frame_size body in
    (* 恢复标签栈 *)
    ctx.break_labels <- List.tl ctx.break_labels;
    ctx.continue_labels <- List.tl ctx.continue_labels;
    [ Label loop_label ]
    @ cond_items
    @ List.map (fun i -> Instruction i) cond_instrs
    @ [ Instruction (Beq (cond_reg, Zero, end_label)) ]  (* 条件为假时退出循环 *)
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
    let e_reg, e_instrs, e_items = gen_expr ctx e in
    e_items 
    @ List.map (fun i -> Instruction i) e_instrs 
    @ [ Instruction (Sw (e_reg, offset, Fp)) ]  (* 存储变量到栈 *)
  | Ast.Assign (name, e) ->
    let offset = get_var_offset ctx name in
    let e_reg, e_instrs, e_items = gen_expr ctx e in
    e_items 
    @ List.map (fun i -> Instruction i) e_instrs 
    @ [ Instruction (Sw (e_reg, offset, Fp)) ]  (* 更新栈中的变量 *)

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
  (* 统计所有语句中的声明数量 *)
  let num_locals =
    List.fold_left (fun acc stmt -> acc + count_decls_in_stmt stmt) 0 func_def.body in
  let num_params = List.length func_def.params in
  (* 计算栈上参数所需空间（超过8个的参数） *)
  let stack_params = max 0 (num_params - 8) in
  (* ra, fp + 保存的寄存器 + 栈参数 + 局部变量 *)
  let required_space = 8 + (16 * 4) + (stack_params * 4) + (num_locals * 4) in
  (* 对齐到16字节 *)
  let aligned = (required_space + 15) / 16 * 16 in
  max aligned 16  (* 确保最小栈帧大小为16字节 *)

(* 生成函数代码 *)
let gen_function symbol_table (func_def : Ast.func_def) : asm_item list =
  let ctx = create_context symbol_table func_def.fname in
  let frame_size = calculate_frame_size func_def in
  
  (* 函数序言 *)
  let prologue = gen_prologue_instrs frame_size  in
  
  (* 处理参数 - 保存寄存器参数到栈 *)
  let param_instrs =
    List.mapi (fun i { Ast.pname = name; _ } ->
      let offset = add_local_var ctx name in
      if i < 8 then
        (* 前8个参数使用寄存器A0-A7 *)
        let arg_reg = 
          match i with
          | 0 -> A0 | 1 -> A1 | 2 -> A2 | 3 -> A3
          | 4 -> A4 | 5 -> A5 | 6 -> A6 | 7 -> A7
          | _ -> failwith "Invalid register index"
        in
        [ Instruction (Sw (arg_reg, offset, Fp)) ]
      else
        (* 超过8个的参数从栈上加载 *)
        let stack_pos = 16 + 4 * i in  (* 正确计算栈上参数位置 *)
        [ Instruction (Lw (T0, stack_pos, Sp));
          Instruction (Sw (T0, offset, Fp)) ]
    ) func_def.params
    |> List.flatten
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
    else [ Instruction (Li (A0, 0)) ] @ gen_epilogue_instrs frame_size
  in
  
  prologue @ param_instrs @ body_items @ epilogue

(* 生成整个程序的代码 *)
let gen_program symbol_table (program : Ast.program) =
  (* 全局声明 *)
  let header =
    [ Directive ".text"; Directive ".globl main"; Comment "Generated by ToyC Compiler" ]
  in
  
  (* 生成所有函数 *)
  let func_asm_items =
    List.map
      (fun func_def ->
         let items = gen_function symbol_table func_def in
         [ Label func_def.fname; Comment ("Function: " ^ func_def.fname) ] @ items)
      program
    |> List.flatten
  in
  
  header @ func_asm_items

(* 输出汇编代码到文件 *)
let emit_asm_to_file filepath asm_items =
  let file = open_out filepath in
  List.iter
    (fun item ->
       output_string file (asm_item_to_string item);
       output_string file "\n")
    asm_items;
  close_out file


let compile_to_riscv symbol_table program =
  let asm_items = gen_program symbol_table program in
  (* 直接将汇编项转换为字符串并打印到标准输出 *)
  List.iter
    (fun item -> print_endline (asm_item_to_string item))
    asm_items







