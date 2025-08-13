open Ast

(* RISC-V 32位寄存器 *)
type reg =
  | Zero | Ra | Sp | Gp | Tp 
  | T0 | T1 | T2 | Fp | S1 
  | A0 | A1 | A2 | A3 | A4 | A5 | A6 | A7 
  | S2 | S3 | S4 | S5 | S6 | S7 | S8 | S9 | S10 | S11 
  | T3 | T4 | T5 | T6 

(* 寄存器到字符串的转换 *)
let reg_to_string = function
  | Zero -> "zero" | Ra -> "ra" | Sp -> "sp" | Gp -> "gp" | Tp -> "tp"
  | T0 -> "t0" | T1 -> "t1" | T2 -> "t2" | Fp -> "fp" | S1 -> "s1"
  | A0 -> "a0" | A1 -> "a1" | A2 -> "a2" | A3 -> "a3"
  | A4 -> "a4" | A5 -> "a5" | A6 -> "a6" | A7 -> "a7"
  | S2 -> "s2" | S3 -> "s3" | S4 -> "s4" | S5 -> "s5"
  | S6 -> "s6" | S7 -> "s7" | S8 -> "s8" | S9 -> "s9"
  | S10 -> "s10" | S11 -> "s11"
  | T3 -> "t3" | T4 -> "t4" | T5 -> "t5" | T6 -> "t6"

(* 拆分立即数为高位和低位 *)
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
let instr_to_string = function
  | Add (rd, rs1, rs2) ->
    Printf.sprintf "add %s, %s, %s" (reg_to_string rd) (reg_to_string rs1) (reg_to_string rs2)
  | Addi (rd, rs1, imm) ->
    if imm < -2048 || imm > 2047 then
      failwith (Printf.sprintf "Addi immediate out of range: %d" imm)
    else
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
          (reg_to_string rd) upper (reg_to_string rd) (reg_to_string rd) lower
  | Lui (rd, imm) -> 
    if imm < 0 || imm > 1048575 then
      failwith (Printf.sprintf "LUI immediate out of range: %d" imm)
    else
      Printf.sprintf "lui %s, %d" (reg_to_string rd) imm
  | Mv (rd, rs) -> Printf.sprintf "mv %s, %s" (reg_to_string rd) (reg_to_string rs)
  | Nop -> "nop"

(* 汇编项类型 *)
type asm_item =
  | Label of string
  | Instruction of instruction
  | Comment of string
  | Directive of string

(* 汇编项到字符串的转换 *)
let asm_item_to_string = function
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

(* 代码生成上下文 *)
type codegen_context = {
  mutable label_counter : int;
  mutable temp_counter : int;
  mutable temp_regs : reg list;
  mutable stack_offset : int;
  mutable break_labels : string list;
  mutable continue_labels : string list;
  mutable local_vars : (string * int) list;
  func_name : string;
  total_locals : int ref;
  param_count : int ref;
}

(* 创建代码生成上下文 *)
let create_context _symbol_table func_name = {
  label_counter = 0;
  temp_counter = 0;
  temp_regs = [];
  stack_offset = -8;
  break_labels = [];
  continue_labels = [];
  local_vars = [];
  func_name;
  total_locals = ref 0;
  param_count = ref 0;
}

(* 生成新标签 *)
let new_label ctx prefix =
  let label = Printf.sprintf "%s_%s%d" ctx.func_name prefix ctx.label_counter in
  ctx.label_counter <- ctx.label_counter + 1;
  label

(* 临时寄存器管理 *)
let get_temp_reg ctx =
  match ctx.temp_regs with
  | reg :: rest -> ctx.temp_regs <- rest; reg
  | [] ->
    let reg = match ctx.temp_counter mod 7 with
      | 0 -> T0 | 1 -> T1 | 2 -> T2 | 3 -> T3 | 4 -> T4 | 5 -> T5 | 6 -> T6
      | _ -> failwith "Invalid temp register index"
    in
    ctx.temp_counter <- ctx.temp_counter + 1;
    reg

let release_temp_reg ctx reg =
  ctx.temp_regs <- reg :: ctx.temp_regs

(* 局部变量管理 *)
let add_local_var ctx name =
  ctx.stack_offset <- ctx.stack_offset - 4;
  ctx.local_vars <- (name, ctx.stack_offset) :: ctx.local_vars;
  ctx.total_locals := !(ctx.total_locals) + 1;
  ctx.stack_offset

let get_var_offset ctx name =
  match List.assoc_opt name ctx.local_vars with
  | Some offset -> offset
  | None -> 
    failwith (Printf.sprintf "Variable '%s' not found in scope" name)

(* 生成表达式代码 
   返回: (结果寄存器, 指令列表, 额外的asm项如标签) *)
let rec gen_expr ctx (expr : Ast.expr) : reg * instruction list * asm_item list =
  match expr with
  | Ast.Literal(IntLit n) ->
    let reg = get_temp_reg ctx in
    let instrs =
      if n >= -2048 && n <= 2047 then [Li (reg, n)]
      else
        let (upper, lower) = split_imm n in
        [Lui (reg, upper); Addi (reg, reg, lower)]
    in
    (reg, instrs, [])

  | Ast.Var id ->
    let reg = get_temp_reg ctx in
    let offset = get_var_offset ctx id in
    (reg, [Lw (reg, offset, Fp)], [])

  | Ast.Paren e -> gen_expr ctx e

  | Ast.UnOp (op, e) ->
    let e_reg, e_instrs, e_extra = gen_expr ctx e in
    let result_reg = get_temp_reg ctx in
    let instrs = match op with
      | "+" -> [Mv (result_reg, e_reg)]
      | "-" -> [Sub (result_reg, Zero, e_reg)]
      | "!" -> [Sltiu (result_reg, e_reg, 1)]
      | _ -> failwith (Printf.sprintf "Unknown unary operator: %s" op)
    in
    release_temp_reg ctx e_reg;
    (result_reg, e_instrs @ instrs, e_extra)

  | Ast.BinOp (e1, op, e2) ->
    let e1_reg, e1_instrs, e1_extra = gen_expr ctx e1 in
    let e2_reg, e2_instrs, e2_extra = gen_expr ctx e2 in
    let result_reg = get_temp_reg ctx in

    let process_op op =
      match op with
      | "+" -> ([Add (result_reg, e1_reg, e2_reg)], [])
      | "-" -> ([Sub (result_reg, e1_reg, e2_reg)], [])
      | "*" -> ([Mul (result_reg, e1_reg, e2_reg)], [])
      | "/" -> ([Div (result_reg, e1_reg, e2_reg)], [])
      | "%" -> ([Rem (result_reg, e1_reg, e2_reg)], [])
      | "==" -> 
        ([Sub (result_reg, e1_reg, e2_reg); 
          Sltiu (result_reg, result_reg, 1)], [])
      | "!=" -> 
        ([Sub (result_reg, e1_reg, e2_reg); 
          Sltu (result_reg, Zero, result_reg)], [])
      | "<" -> ([Slt (result_reg, e1_reg, e2_reg)], [])
      | "<=" -> 
        ([Slt (result_reg, e2_reg, e1_reg); 
          Xori (result_reg, result_reg, 1)], [])
      | ">" -> ([Slt (result_reg, e2_reg, e1_reg)], [])
      | ">=" -> 
        ([Slt (result_reg, e1_reg, e2_reg); 
          Xori (result_reg, result_reg, 1)], [])
      
      (* 修复逻辑运算符的标签处理 *)
      | "&&" ->
        let and_skip = new_label ctx "and_skip" in
        let instrs = [
          Sltu (T0, Zero, e1_reg);
          Beq (T0, Zero, and_skip);
          Sltu (result_reg, Zero, e2_reg)
        ] in
        (instrs, [Label and_skip])
      
      | "||" ->
        let or_skip = new_label ctx "or_skip" in
        let instrs = [
          Sltu (T0, Zero, e1_reg);
          Bne (T0, Zero, or_skip);
          Sltu (result_reg, Zero, e2_reg)
        ] in
        (instrs, [Label or_skip])
      
      | _ -> failwith (Printf.sprintf "Unknown binary operator: %s" op)
    in

    let (op_instrs, op_extra) = process_op op in
    let all_instrs = e1_instrs @ e2_instrs @ op_instrs in
    let all_extra = e1_extra @ e2_extra @ op_extra in

    release_temp_reg ctx e1_reg;
    release_temp_reg ctx e2_reg;
    (result_reg, all_instrs, all_extra)

  | Ast.Call (fname, args) ->
    let result_reg = A0 in
    let num_args = List.length args in
    let num_stack_args = max 0 (num_args - 8) in
    
    let stack_args_space = num_stack_args * 4 in
    let saved_regs_space = 7 * 4 in (* T0-T6 *)
    let stack_space = stack_args_space + saved_regs_space in
    let stack_space = 
      if stack_space mod 16 != 0 then stack_space + (16 - stack_space mod 16) 
      else stack_space 
    in
    
    let save_instrs = 
      if stack_space > 0 then
        Addi (Sp, Sp, -stack_space) ::
        [ Sw (T0, saved_regs_space - 4, Sp);
          Sw (T1, saved_regs_space - 8, Sp);
          Sw (T2, saved_regs_space - 12, Sp);
          Sw (T3, saved_regs_space - 16, Sp);
          Sw (T4, saved_regs_space - 20, Sp);
          Sw (T5, saved_regs_space - 24, Sp);
          Sw (T6, saved_regs_space - 28, Sp) ]
      else []
    in
    
    let arg_instrs, arg_extra = 
      List.mapi (fun i arg ->
        let arg_reg, arg_code, arg_extra = gen_expr ctx arg in
        let instrs =
          if i < 8 then
            let target_reg = match i with
              | 0 -> A0 | 1 -> A1 | 2 -> A2 | 3 -> A3
              | 4 -> A4 | 5 -> A5 | 6 -> A6 | 7 -> A7
              | _ -> failwith "Invalid argument index"
            in
            arg_code @ [Mv (target_reg, arg_reg)]
          else
            let stack_pos = (i - 8) * 4 + saved_regs_space in
            arg_code @ [Sw (arg_reg, stack_pos, Sp)]
        in
        release_temp_reg ctx arg_reg;
        (instrs, arg_extra)
      ) args
      |> List.split
    in
    let arg_instrs = List.flatten arg_instrs in
    let arg_extra = List.flatten arg_extra in
    
    let call_instr = [Jal (Ra, fname)] in
    
    let restore_instrs =
      if stack_space > 0 then
        [ Lw (T0, saved_regs_space - 4, Sp);
          Lw (T1, saved_regs_space - 8, Sp);
          Lw (T2, saved_regs_space - 12, Sp);
          Lw (T3, saved_regs_space - 16, Sp);
          Lw (T4, saved_regs_space - 20, Sp);
          Lw (T5, saved_regs_space - 24, Sp);
          Lw (T6, saved_regs_space - 28, Sp);
          Addi (Sp, Sp, stack_space) ]
      else []
    in
    
    let all_instrs = save_instrs @ arg_instrs @ call_instr @ restore_instrs in
    (result_reg, all_instrs, arg_extra)

(* 生成语句代码 *)
let rec gen_stmt ctx frame_size (stmt : Ast.stmt) : asm_item list =
  match stmt with
  | Ast.Empty -> []
  
  | Ast.ExprStmt e ->
    let reg, instrs, extra = gen_expr ctx e in
    release_temp_reg ctx reg;
    List.map (fun i -> Instruction i) instrs @ extra
  
  | Ast.Block stmts ->
    let old_vars = ctx.local_vars in
    let old_offset = ctx.stack_offset in
    let old_total = !(ctx.total_locals) in
    let old_temps = ctx.temp_regs in
    let items = List.concat_map (gen_stmt ctx frame_size) stmts in
    ctx.local_vars <- old_vars;
    ctx.stack_offset <- old_offset;
    ctx.total_locals := old_total;
    ctx.temp_regs <- old_temps;
    items
  
  | Ast.Return (Some e) ->
    let e_reg, e_instrs, e_extra = gen_expr ctx e in
    let epilogue = [
      Mv (A0, e_reg);
      Lw (Ra, frame_size - 4, Sp);
      Lw (Fp, frame_size - 8, Sp);
      Addi (Sp, Sp, frame_size);
      Ret
    ] in
    release_temp_reg ctx e_reg;
    List.map (fun i -> Instruction i) (e_instrs @ epilogue) @ e_extra
  
  | Ast.Return None ->
    [ Instruction (Li (A0, 0));
      Instruction (Lw (Ra, frame_size - 4, Sp));
      Instruction (Lw (Fp, frame_size - 8, Sp));
      Instruction (Addi (Sp, Sp, frame_size));
      Instruction Ret ]
  
  | Ast.If (cond, then_stmt, else_stmt) ->
    let cond_reg, cond_instrs, cond_extra = gen_expr ctx cond in
    let else_label = new_label ctx "else" in
    let end_label = new_label ctx "endif" in
    
    let then_items = gen_stmt ctx frame_size then_stmt in
    let else_items = match else_stmt with
      | Some s -> gen_stmt ctx frame_size s
      | None -> []
    in
    
    release_temp_reg ctx cond_reg;
    cond_extra
    @ List.map (fun i -> Instruction i) cond_instrs
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
    
    let cond_reg, cond_instrs, cond_extra = gen_expr ctx cond in
    let body_items = gen_stmt ctx frame_size body in
    
    ctx.break_labels <- List.tl ctx.break_labels;
    ctx.continue_labels <- List.tl ctx.continue_labels;
    release_temp_reg ctx cond_reg;
    
    [ Label loop_label ]
    @ cond_extra
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
    let e_reg, e_instrs, e_extra = gen_expr ctx e in
    let all_instrs = e_instrs @ [Sw (e_reg, offset, Fp)] in
    release_temp_reg ctx e_reg;
    List.map (fun i -> Instruction i) all_instrs @ e_extra
  
  | Ast.Assign (name, e) ->
    let offset = get_var_offset ctx name in
    let e_reg, e_instrs, e_extra = gen_expr ctx e in
    let all_instrs = e_instrs @ [Sw (e_reg, offset, Fp)] in
    release_temp_reg ctx e_reg;
    List.map (fun i -> Instruction i) all_instrs @ e_extra

(* 计算栈帧大小 *)
let calculate_frame_size (func_def : Ast.func_def) =
  let rec count_decls = function
    | Ast.Decl _ -> 1
    | Ast.Block stmts -> List.fold_left (fun acc s -> acc + count_decls s) 0 stmts
    | Ast.If (_, s1, Some s2) -> count_decls s1 + count_decls s2
    | Ast.If (_, s1, None) -> count_decls s1
    | Ast.While (_, s) -> count_decls s
    | _ -> 0
  in
  let num_locals = List.fold_left (fun acc s -> acc + count_decls s) 0 func_def.body in
  let num_params = List.length func_def.params in
  let required = 8 + 44 + (num_params * 4) + (num_locals * 4) + 512 in
  let aligned = (required + 15) / 16 * 16 in
  max aligned 64

(* 生成函数代码 *)
let gen_function symbol_table (func_def : Ast.func_def) : asm_item list =
  let ctx = create_context symbol_table func_def.fname in
  ctx.param_count := List.length func_def.params;
  let frame_size = calculate_frame_size func_def in
  
  (* 函数序言 *)
  let prologue = [
    Instruction (Addi (Sp, Sp, -frame_size));
    Instruction (Sw (Ra, frame_size - 4, Sp));
    Instruction (Sw (Fp, frame_size - 8, Sp));
    Instruction (Addi (Fp, Sp, frame_size))
  ] in
  
  (* 参数处理 *)
  let param_items =
    List.mapi (fun i { Ast.pname = name; _ } ->
      let offset = add_local_var ctx name in
      if i < 8 then
        let arg_reg = match i with
          | 0 -> A0 | 1 -> A1 | 2 -> A2 | 3 -> A3
          | 4 -> A4 | 5 -> A5 | 6 -> A6 | 7 -> A7
          | _ -> failwith "Invalid parameter index"
        in
        [ Instruction (Sw (arg_reg, offset, Fp)) ]
      else
        let stack_offset = (i - 8) * 4 + 16 in
        [ Instruction (Lw (T0, stack_offset, Sp));
          Instruction (Sw (T0, offset, Fp)) ]
    ) func_def.params
    |> List.flatten
  in
  
  (* 保存和恢复S寄存器 *)
  let save_s_regs = [
    Instruction (Sw (S1, -12, Fp));
    Instruction (Sw (S2, -16, Fp));
    Instruction (Sw (S3, -20, Fp));
    Instruction (Sw (S4, -24, Fp));
    Instruction (Sw (S5, -28, Fp));
    Instruction (Sw (S6, -32, Fp));
    Instruction (Sw (S7, -36, Fp));
    Instruction (Sw (S8, -40, Fp));
    Instruction (Sw (S9, -44, Fp));
    Instruction (Sw (S10, -48, Fp));
    Instruction (Sw (S11, -52, Fp))
  ] in
  
  let restore_s_regs = [
    Instruction (Lw (S2, -16, Fp));
    Instruction (Lw (S3, -20, Fp));
    Instruction (Lw (S4, -24, Fp));
    Instruction (Lw (S5, -28, Fp));
    Instruction (Lw (S6, -32, Fp));
    Instruction (Lw (S7, -36, Fp));
    Instruction (Lw (S8, -40, Fp));
    Instruction (Lw (S9, -44, Fp));
    Instruction (Lw (S10, -48, Fp));
    Instruction (Lw (S11, -52, Fp))
  ] in
  
  (* 函数体 *)
  let body_items = List.concat_map (gen_stmt ctx frame_size) func_def.body in
  
  (* 检查是否有返回指令 *)
  let has_ret = List.exists (function
    | Instruction Ret -> true | _ -> false
  ) body_items in
  
  (* 函数尾声 *)
  let epilogue = if has_ret then [] else [
    Instruction (Li (A0, 0));
    Instruction (Lw (Ra, frame_size - 4, Sp));
    Instruction (Lw (Fp, frame_size - 8, Sp));
    Instruction (Addi (Sp, Sp, frame_size));
    Instruction Ret
  ] in
  
  (* 组合所有部分 *)
  [ Label func_def.fname; Comment ("Function: " ^ func_def.fname) ]
  @ prologue
  @ save_s_regs
  @ param_items
  @ body_items
  @ restore_s_regs
  @ epilogue

(* 生成程序代码 *)
let gen_program symbol_table (program : Ast.program) =
  let header = [
    Directive ".text";
    Directive ".globl main";
    Directive ".align 2";
    Comment "ToyC Compiler Generated Code"
  ] in
  let func_items = List.concat_map (gen_function symbol_table) program in
  header @ func_items

(* 编译到RISC-V汇编 *)
let compile_to_riscv symbol_table program =
  let asm_items = gen_program symbol_table program in
  List.iter (fun item -> print_endline (asm_item_to_string item)) asm_items
    
