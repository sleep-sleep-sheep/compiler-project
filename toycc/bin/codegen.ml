open Ast

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

(* 寄存器分类 - 遵循RISC-V ABI *)
let is_caller_saved = function
  | T0 | T1 | T2 | T3 | T4 | T5 | T6 
  | A0 | A1 | A2 | A3 | A4 | A5 | A6 | A7 -> true
  | _ -> false

let is_callee_saved = function
  | S0 | S1 | S2 | S3 | S4 | S5 | S6 | S7 | S8 | S9 | S10 | S11 -> true
  | _ -> false

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
  | S0 -> "s0"  (* 原代码中的Fp其实是s0 *)
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

(* RISC-V 指令类型 - 保持不变 *)
type instruction =
  | Add of reg * reg * reg | Addi of reg * reg * int | Sub of reg * reg * reg
  | Mul of reg * reg * reg | Div of reg * reg * reg | Rem of reg * reg * reg
  | And of reg * reg * reg | Or of reg * reg * reg | Xor of reg * reg * reg
  | Xori of reg * reg * int | Slt of reg * reg * reg | Slti of reg * reg * int
  | Sltu of reg * reg * reg | Sltiu of reg * reg * int | Lw of reg * int * reg
  | Sw of reg * int * reg | Beq of reg * reg * string | Bne of reg * reg * string
  | Blt of reg * reg * string | Bge of reg * reg * string | Ble of reg * reg * string
  | Bgt of reg * reg * string | J of string | Jal of reg * string | Jalr of reg * reg * int
  | Ret | Li of reg * int | Lui of reg * int | Mv of reg * reg | Nop

(* 将指令转换为汇编字符串 - 保持不变 *)
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

(* 汇编代码项和转换函数 - 保持不变 *)
type label = string
type asm_item = Label of label | Instruction of instruction | Comment of string | Directive of string

let asm_item_to_string item = match item with 
  | Instruction instr -> "    " ^ instr_to_string instr
  | Label l -> l ^ ":"
  | Directive d -> "    " ^ d
  | Comment c -> "    # " ^ c

(* 输出函数 - 保持不变 *)
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

(* 代码生成上下文 *)
type codegen_context = {
  mutable label_counter : int;
  mutable temp_counter : int;          (* 临时寄存器计数器 *)
  mutable saved_counter : int;         (* 保存寄存器计数器 *)
  mutable stack_offset : int;          (* 当前栈偏移 *)
  mutable break_labels : string list;
  mutable continue_labels : string list;
  mutable local_vars : (string * int) list;  (* 局部变量映射到栈偏移 *)
  func_name : string;
  frame_size : int;                    (* 栈帧大小 *)
}

(* 创建新的代码生成上下文 *)
let create_context _symbol_table func_name frame_size = {
  label_counter = 0;
  temp_counter = 0;
  saved_counter = 0;
  stack_offset = -4;  (* s0(fp)为基址，从-4开始分配 *)
  break_labels = [];
  continue_labels = [];
  local_vars = [];
  func_name;
  frame_size;
}

(* 生成新标签 *)
let new_label ctx prefix =
  let label = Printf.sprintf "%s_%s%d" ctx.func_name prefix ctx.label_counter in
  ctx.label_counter <- ctx.label_counter + 1;
  label

(* 寄存器分配策略改进 *)
let get_temp_reg ctx =
  let regs = [T0; T1; T2; T3; T4; T5; T6] in
  let reg = List.nth regs (ctx.temp_counter mod 7) in
  ctx.temp_counter <- ctx.temp_counter + 1;
  reg

let get_saved_reg ctx =
  let regs = [S1; S2; S3; S4; S5; S6; S7; S8; S9; S10; S11] in  (* 排除s0(fp) *)
  let reg = List.nth regs (ctx.saved_counter mod 11) in
  ctx.saved_counter <- ctx.saved_counter + 1;
  reg

(* 变量栈管理 *)
let add_local_var ctx name =
  ctx.stack_offset <- ctx.stack_offset - 4;
  ctx.local_vars <- (name, ctx.stack_offset) :: ctx.local_vars;
  ctx.stack_offset

let get_var_offset ctx name =
  match List.assoc_opt name ctx.local_vars with
  | Some offset -> offset
  | None -> 
      failwith (Printf.sprintf "Variable '%s' not found in scope (available: %s)"
        name (String.concat ", " (List.map fst ctx.local_vars)))

(* 生成表达式代码 *)
let rec gen_expr ctx (expr : Ast.expr) : reg * instruction list =
  match expr with
  | Ast.Literal(IntLit n) ->
    let reg = get_temp_reg ctx in
    reg, [ Li (reg, n) ]
  | Ast.Var id ->
    let reg = get_temp_reg ctx in
    let offset = get_var_offset ctx id in
    reg, [ Lw (reg, offset, S0) ]  (* 使用s0作为帧指针 *)
  | Ast.Paren e -> gen_expr ctx e
  | Ast.UnOp (op, e) ->
    let e_reg, e_instrs = gen_expr ctx e in
    let result_reg = get_temp_reg ctx in
    let instrs = match op with
      | "+" -> e_instrs @ [ Mv (result_reg, e_reg) ]
      | "-" -> e_instrs @ [ Sub (result_reg, Zero, e_reg) ]
      | "!" -> e_instrs @ [ Sltiu (result_reg, e_reg, 1) ]
      | _ -> failwith (Printf.sprintf "Unknown unary operator: %s" op)
    in
    result_reg, instrs
  | Ast.BinOp (e1, op, e2) ->
    let e1_reg, e1_instrs = gen_expr ctx e1 in
    let e2_reg, e2_instrs = gen_expr ctx e2 in
    let result_reg = get_temp_reg ctx in
    let op_instrs = match op with
      | "+" -> [ Add (result_reg, e1_reg, e2_reg) ]
      | "-" -> [ Sub (result_reg, e1_reg, e2_reg) ]
      | "*" -> [ Mul (result_reg, e1_reg, e2_reg) ]
      | "/" -> [ Div (result_reg, e1_reg, e2_reg) ]
      | "%" -> [ Rem (result_reg, e1_reg, e2_reg) ]
      | "==" -> [ Sub (result_reg, e1_reg, e2_reg); Sltiu (result_reg, result_reg, 1) ]
      | "!=" -> [ Sub (result_reg, e1_reg, e2_reg); Sltu (result_reg, Zero, result_reg) ]
      | "<" -> [ Slt (result_reg, e1_reg, e2_reg) ]
      | "<=" -> [ Slt (result_reg, e2_reg, e1_reg); Xori (result_reg, result_reg, 1) ]
      | ">" -> [ Slt (result_reg, e2_reg, e1_reg) ]
      | ">=" -> [ Slt (result_reg, e1_reg, e2_reg); Xori (result_reg, result_reg, 1) ]
      | "&&" -> [ Sltu (T0, Zero, e1_reg); Sltu (T1, Zero, e2_reg); And (result_reg, T0, T1) ]
      | "||" -> [ Or (result_reg, e1_reg, e2_reg); Sltu (result_reg, Zero, result_reg) ]
      | _ -> failwith (Printf.sprintf "Unknown binary operator: %s" op)
    in
    result_reg, e1_instrs @ e2_instrs @ op_instrs
  | Ast.Call (fname, args) ->
    (* 1. 保存调用者需要保存的寄存器 *)
    let caller_saved = [T0; T1; T2; T3; T4; T5; T6] in
    let save_instrs, restore_instrs = 
      if List.length args > 0 then
        let save = List.mapi (fun i reg ->
          let offset = i * 4 + 4 in  (* 跳过ra *)
          Sw (reg, offset, Sp)
        ) caller_saved in
        let restore = List.mapi (fun i reg ->
          let offset = i * 4 + 4 in
          Lw (reg, offset, Sp)
        ) caller_saved in
        save, restore
      else [], []
    in

    (* 2. 计算栈参数空间并调整栈指针 *)
    let num_stack_args = max 0 (List.length args - 8) in
    let stack_args_space = num_stack_args * 4 in
    let stack_adjust = if stack_args_space > 0 then [Addi (Sp, Sp, -stack_args_space)] else [] in

    (* 3. 处理参数：前8个用a0-a7，其余用栈 *)
    let arg_instrs =
      List.mapi (fun i arg ->
        let arg_reg, arg_code = gen_expr ctx arg in
        if i < 8 then
          let target_reg = match i with
            | 0 -> A0 | 1 -> A1 | 2 -> A2 | 3 -> A3
            | 4 -> A4 | 5 -> A5 | 6 -> A6 | 7 -> A7
            | _ -> failwith "Invalid register index"
          in
          arg_code @ [ Mv (target_reg, arg_reg) ]
        else
          let stack_pos = (i - 8) * 4 in  (* 栈参数从0开始偏移 *)
          arg_code @ [ Sw (arg_reg, stack_pos, Sp) ]
      ) args |> List.flatten
    in

    (* 4. 函数调用 *)
    let call_instr = [ Jal (Ra, fname) ] in

    (* 5. 恢复栈指针和寄存器 *)
    let stack_restore = if stack_args_space > 0 then [Addi (Sp, Sp, stack_args_space)] else [] in
    let all_instrs = save_instrs @ stack_adjust @ arg_instrs @ call_instr @ stack_restore @ restore_instrs in

    A0, all_instrs  (* 返回值在a0中 *)

(* 生成序言：遵循ABI保存被调用者寄存器 *)
let gen_prologue_instrs frame_size =
  (* 需要保存的被调用者寄存器：s0-s11 *)
  let callee_saved = [S0; S1; S2; S3; S4; S5; S6; S7; S8; S9; S10; S11] in
  
  (* 分配栈帧空间 *)
  let prologue = [Addi (Sp, Sp, -frame_size)] in
  
  (* 保存ra和被调用者寄存器 *)
  let save_regs = 
    [Sw (Ra, frame_size - 4, Sp)]  (* 保存返回地址 *)
    @ List.mapi (fun i reg ->
        Sw (reg, frame_size - 8 - (i * 4), Sp)  (* 保存s0-s11 *)
      ) callee_saved
  in
  
  (* 设置帧指针(s0) *)
  let set_fp = [Addi (S0, Sp, frame_size)] in
  
  List.map (fun i -> Instruction i) (prologue @ save_regs @ set_fp)

(* 生成尾声：恢复被调用者寄存器 *)
let gen_epilogue_instrs frame_size =
  let callee_saved = [S0; S1; S2; S3; S4; S5; S6; S7; S8; S9; S10; S11] in
  
  (* 恢复被调用者寄存器 *)
  let restore_regs = 
    List.mapi (fun i reg ->
      Lw (reg, frame_size - 8 - (i * 4), Sp)
    ) callee_saved
    @ [Lw (Ra, frame_size - 4, Sp)]  (* 恢复返回地址 *)
  in
  
  (* 释放栈帧并返回 *)
  [Addi (Sp, Sp, frame_size); Ret] @ restore_regs

(* 生成语句代码 *)
let rec gen_stmt ctx (stmt : Ast.stmt) : asm_item list =
  match stmt with
  | Ast.Empty -> []
  | Ast.ExprStmt e ->
    let _, instrs = gen_expr ctx e in
    List.map (fun i -> Instruction i) instrs
  | Ast.Block stmts ->
    let old_vars = ctx.local_vars in
    let old_offset = ctx.stack_offset in
    let old_temp = ctx.temp_counter in
    let old_saved = ctx.saved_counter in
    let items = List.map (gen_stmt ctx) stmts |> List.flatten in
    ctx.local_vars <- old_vars;
    ctx.stack_offset <- old_offset;
    ctx.temp_counter <- old_temp;
    ctx.saved_counter <- old_saved;
    items
  | Ast.Return (Some e) ->
    let e_reg, e_instrs = gen_expr ctx e in
    let all_instrs = e_instrs @ [Mv (A0, e_reg)] @ gen_epilogue_instrs ctx.frame_size in
    List.map (fun i -> Instruction i) all_instrs
  | Ast.Return None -> 
    List.map (fun i -> Instruction i) (gen_epilogue_instrs ctx.frame_size)
  | Ast.If (cond, then_stmt, else_stmt) ->
    let cond_reg, cond_instrs = gen_expr ctx cond in
    let else_label = new_label ctx "else" in
    let end_label = new_label ctx "endif" in
    let then_items = gen_stmt ctx then_stmt in
    let else_items = match else_stmt with
      | Some s -> gen_stmt ctx s
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
    let body_items = gen_stmt ctx body in
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
     | [] -> failwith "Break outside loop")
  | Ast.Continue ->
    (match ctx.continue_labels with
     | label :: _ -> [Instruction (J label)]
     | [] -> failwith "Continue outside loop")
  | Ast.Decl (name, e) ->
    let offset = add_local_var ctx name in
    let e_reg, e_instrs = gen_expr ctx e in
    let all_instrs = e_instrs @ [Sw (e_reg, offset, S0)] in  (* 使用s0作为帧指针 *)
    List.map (fun i -> Instruction i) all_instrs
  | Ast.Assign (name, e) ->
    let offset = get_var_offset ctx name in
    let e_reg, e_instrs = gen_expr ctx e in
    let all_instrs = e_instrs @ [Sw (e_reg, offset, S0)] in
    List.map (fun i -> Instruction i) all_instrs

(* 计算函数所需的栈帧大小 *)
let calculate_frame_size (func_def : Ast.func_def) =
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
  
  (* 栈帧组成：
     - 12个被调用者保存寄存器(s0-s11)：12×4=48字节
     - 返回地址(ra)：4字节
     - 局部变量：num_locals×4字节
     - 栈参数空间：0（参数通过寄存器传递）
  *)
  let required_space = 48 + 4 + (num_locals * 4) + (num_params * 4) in
  
  (* 16字节对齐 *)
  (required_space + 15) / 16 * 16

(* 生成函数代码 *)
let gen_function symbol_table (func_def : Ast.func_def) : asm_item list =
  let frame_size = calculate_frame_size func_def in
  let ctx = create_context symbol_table func_def.fname frame_size in
  
  (* 函数序言 *)
  let prologue = gen_prologue_instrs frame_size in
  
  (* 处理参数：前8个用a0-a7寄存器，保存到栈帧 *)
  let param_instrs =
    List.mapi (fun i { Ast.pname = name; _ } ->
      let offset = add_local_var ctx name in
      let src_reg = match i with
        | 0 -> A0 | 1 -> A1 | 2 -> A2 | 3 -> A3
        | 4 -> A4 | 5 -> A5 | 6 -> A6 | 7 -> A7
        | _ -> failwith "超过8个参数需要栈处理"
      in
      [Instruction (Sw (src_reg, offset, S0))]  (* 保存到栈帧 *)
    ) func_def.params |> List.flatten
  in
  
  (* 函数体 *)
  let body_items = func_def.body |> List.map (gen_stmt ctx) |> List.flatten in
  
  (* 函数尾声（如果没有显式return） *)
  let has_ret = List.exists (function
    | Instruction Ret -> true | _ -> false) body_items in
  let epilogue = if has_ret then [] else 
    List.map (fun i -> Instruction i) (gen_epilogue_instrs frame_size) in
  
  [Label func_def.fname; Comment ("Function: " ^ func_def.fname)] 
  @ prologue @ param_instrs @ body_items @ epilogue

(* 生成整个程序的代码 *)
let gen_program symbol_table (program : Ast.program) =
  let header = [
    Directive ".text";
    Directive ".globl main";
    Comment "遵循RISC-V ABI规范的ToyC编译器生成代码"
  ] in
  
  let func_asm_items =
    List.map (fun func_def ->
      let items = gen_function symbol_table func_def in
      items
    ) program |> List.flatten
  in
  
  header @ func_asm_items

(* 编译入口 *)
let compile_to_riscv symbol_table program =
  let asm_items = gen_program symbol_table program in
  List.iter (fun item -> print_endline (asm_item_to_string item)) asm_items
