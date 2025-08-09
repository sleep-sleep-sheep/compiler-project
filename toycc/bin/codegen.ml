open Ast

(* RISC-V 32位寄存器定义 *)
type reg =
  | Zero | Ra | Sp | Gp | Tp
  | T0 | T1 | T2 | Fp | S1
  | A0 | A1 | A2 | A3 | A4 | A5 | A6 | A7
  | S2 | S3 | S4 | S5 | S6 | S7 | S8 | S9 | S10 | S11
  | T3 | T4 | T5 | T6


(* 寄存器转字符串 *)
let reg_to_string = function
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
  | Ble of reg * reg * string
  | Bgt of reg * reg * string
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
  (* 其他 *)
  | Nop


(* 指令转汇编字符串 *)
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


(* 汇编代码项 *)
type asm_item =
  | Label of string
  | Instruction of instruction
  | Comment of string
  | Directive of string


(* 汇编项转字符串 *)
let asm_item_to_string = function
  | Instruction i -> "    " ^ instr_to_string i
  | Label l -> l ^ ":"
  | Directive d -> "    " ^ d
  | Comment c -> "    # " ^ c


(* 代码生成上下文 *)
type codegen_context = {
  mutable label_counter: int;
  mutable temp_counter: int;
  mutable stack_offset: int;
  mutable break_labels: string list;
  mutable continue_labels: string list;
  mutable local_vars: (string * int) list;
}


(* 创建新的代码生成上下文 *)
let create_context () = {
  label_counter = 0;
  temp_counter = 0;
  stack_offset = -4;  (* 从-4开始，第一个变量位于-4(fp) *)
  break_labels = [];
  continue_labels = [];
  local_vars = [];
}


(* 生成新标签 *)
let new_label ctx prefix =
  let label = Printf.sprintf "%s%d" prefix ctx.label_counter in
  ctx.label_counter <- ctx.label_counter + 1;
  label


(* 获取临时寄存器 *)
let get_temp_reg ctx =
  let reg = match ctx.temp_counter mod 7 with
    | 0 -> T0 | 1 -> T1 | 2 -> T2 | 3 -> T3 | 4 -> T4 | 5 -> T5 | 6 -> T6
    | _ -> failwith "Invalid temp register index"
  in
  ctx.temp_counter <- ctx.temp_counter + 1;
  reg


(* 释放临时寄存器 *)
let release_temp_reg ctx =
  if ctx.temp_counter > 0 then ctx.temp_counter <- ctx.temp_counter - 1


(* 添加局部变量到栈 *)
let add_local_var ctx name =
  ctx.stack_offset <- ctx.stack_offset - 4;
  ctx.local_vars <- (name, ctx.stack_offset) :: ctx.local_vars;
  ctx.stack_offset


(* 获取变量的栈偏移 *)
let get_var_offset ctx name =
  match List.assoc_opt name ctx.local_vars with
  | Some offset -> offset
  | None -> failwith (Printf.sprintf "Variable %s not found" name)


(* 预扫描函数收集所有变量 *)
let pre_scan_function func_def =
  let param_names = List.map (fun p -> p.pname) func_def.params in
  
  let rec scan_stmt stmt acc = match stmt with
    | Decl (name, _) -> name :: acc
    | Block stmts -> List.fold_left (fun a s -> scan_stmt s a) acc stmts
    | If (_, s1, Some s2) -> scan_stmt s2 (scan_stmt s1 acc)
    | If (_, s1, None) -> scan_stmt s1 acc
    | While (_, s) -> scan_stmt s acc
    | _ -> acc
  in
  
  let local_vars = List.fold_left (fun acc s -> scan_stmt s acc) [] func_def.body in
  param_names @ local_vars


(* 生成表达式代码 *)
let rec gen_expr ctx = function
  | Literal (IntLit n) ->
    let reg = get_temp_reg ctx in
    reg, [ Li (reg, n) ]
  
  | Var id ->
    let reg = get_temp_reg ctx in
    let offset = get_var_offset ctx id in
    reg, [ Lw (reg, offset, Fp) ]
  
  | Paren e -> gen_expr ctx e
  
  | UnOp (op, e) ->
    let e_reg, e_instrs = gen_expr ctx e in
    let res_reg = get_temp_reg ctx in
    let instrs = match op with
      | "-" -> e_instrs @ [ Sub (res_reg, Zero, e_reg) ]
      | "!" -> e_instrs @ [ Sltiu (res_reg, e_reg, 1) ]
      | _ -> failwith ("Unknown unary operator: " ^ op)
    in
    release_temp_reg ctx;  (* 释放e_reg *)
    res_reg, instrs
  
  | BinOp (e1, op, e2) ->
    let r1, i1 = gen_expr ctx e1 in
    let r2, i2 = gen_expr ctx e2 in
    let res = get_temp_reg ctx in
    let ops = match op with
      | "+" -> [ Add (res, r1, r2) ]
      | "-" -> [ Sub (res, r1, r2) ]
      | "*" -> [ Mul (res, r1, r2) ]
      | "/" -> [ Div (res, r1, r2) ]
      | "%" -> [ Rem (res, r1, r2) ]
      | "==" -> [ Sub (res, r1, r2); Sltiu (res, res, 1) ]
      | "!=" -> [ Sub (res, r1, r2); Sltu (res, Zero, res) ]
      | "<" -> [ Slt (res, r1, r2) ]
      | "<=" -> [ Slt (res, r2, r1); Xori (res, res, 1) ]
      | ">" -> [ Slt (res, r2, r1) ]
      | ">=" -> [ Slt (res, r1, r2); Xori (res, res, 1) ]
      | "&&" -> [ Sltu (T0, Zero, r1); Sltu (T1, Zero, r2); And (res, T0, T1) ]
      | "||" -> [ Or (res, r1, r2); Sltu (res, Zero, res) ]
      | _ -> failwith ("Unknown binary operator: " ^ op)
    in
    release_temp_reg ctx;  (* 释放r1 *)
    release_temp_reg ctx;  (* 释放r2 *)
    res, i1 @ i2 @ ops
  
  | Call (fname, args) ->
    let result_reg = A0 in
    let num_args = List.length args in
    let num_stack_args = max 0 (num_args - 8) in
    let stack_needed = num_stack_args * 4 + 28 in  (* 临时寄存器保存空间 *)
    
    let save_instrs = if stack_needed > 0 then
      Addi (Sp, Sp, -stack_needed) ::
      [ Sw (T0, num_stack_args * 4 + 0, Sp);
        Sw (T1, num_stack_args * 4 + 4, Sp);
        Sw (T2, num_stack_args * 4 + 8, Sp);
        Sw (T3, num_stack_args * 4 + 12, Sp);
        Sw (T4, num_stack_args * 4 + 16, Sp);
        Sw (T5, num_stack_args * 4 + 20, Sp);
        Sw (T6, num_stack_args * 4 + 24, Sp) ]
    else [] in
    
    let arg_instrs =
      List.mapi (fun i arg ->
        let arg_reg, arg_code = gen_expr ctx arg in
        let instrs = if i < 8 then
          let tgt = match i with
            | 0 -> A0 | 1 -> A1 | 2 -> A2 | 3 -> A3
            | 4 -> A4 | 5 -> A5 | 6 -> A6 | 7 -> A7
            | _ -> failwith "Invalid argument index"
          in
          arg_code @ [ Mv (tgt, arg_reg) ]
        else
          let pos = (i - 8) * 4 in
          arg_code @ [ Sw (arg_reg, pos, Sp) ]
        in
        release_temp_reg ctx;
        instrs) args
      |> List.flatten in
    
    let call_instr = [ Jal (Ra, fname) ] in
    
    let restore_instrs = if stack_needed > 0 then
      [ Lw (T0, num_stack_args * 4 + 0, Sp);
        Lw (T1, num_stack_args * 4 + 4, Sp);
        Lw (T2, num_stack_args * 4 + 8, Sp);
        Lw (T3, num_stack_args * 4 + 12, Sp);
        Lw (T4, num_stack_args * 4 + 16, Sp);
        Lw (T5, num_stack_args * 4 + 20, Sp);
        Lw (T6, num_stack_args * 4 + 24, Sp);
        Addi (Sp, Sp, stack_needed) ]
    else [] in
    
    result_reg, save_instrs @ arg_instrs @ call_instr @ restore_instrs


(* 生成函数序言 *)
let gen_prologue frame_size = [
  Instruction (Addi (Sp, Sp, -frame_size));
  Instruction (Sw (Ra, frame_size - 4, Sp));  (* 保存返回地址 *)
  Instruction (Sw (Fp, frame_size - 8, Sp));  (* 保存帧指针 *)
  Instruction (Addi (Fp, Sp, frame_size))     (* 设置新帧指针 *)
]


(* 生成函数尾声 *)
let gen_epilogue frame_size = [
  Lw (Ra, frame_size - 4, Sp);   (* 恢复返回地址 *)
  Lw (Fp, frame_size - 8, Sp);   (* 恢复帧指针 *)
  Addi (Sp, Sp, frame_size);     (* 释放栈帧 *)
  Ret                            (* 返回 *)
]


(* 生成语句代码 *)
let rec gen_stmt ctx frame_size = function
  | Empty -> []
  
  | ExprStmt e ->
    let _, instrs = gen_expr ctx e in
    List.map (fun i -> Instruction i) instrs
  
  | Block stmts ->
    List.map (gen_stmt ctx frame_size) stmts |> List.flatten
  
  | Return (Some e) ->
    let reg, instrs = gen_expr ctx e in
    let all_instrs = instrs @ [ Mv (A0, reg) ] @ gen_epilogue frame_size in
    release_temp_reg ctx;
    List.map (fun i -> Instruction i) all_instrs
  
  | Return None ->
    List.map (fun i -> Instruction i) (gen_epilogue frame_size)
  
  | If (cond, then_stmt, else_stmt) ->
    let cond_reg, cond_instrs = gen_expr ctx cond in
    let else_lbl = new_label ctx "else" in
    let end_lbl = new_label ctx "endif" in
    let then_items = gen_stmt ctx frame_size then_stmt in
    let else_items = match else_stmt with
      | Some s -> gen_stmt ctx frame_size s
      | None -> []
    in
    release_temp_reg ctx;
    List.map (fun i -> Instruction i) cond_instrs
    @ [ Instruction (Beq (cond_reg, Zero, else_lbl)) ]
    @ then_items
    @ [ Instruction (J end_lbl); Label else_lbl ]
    @ else_items
    @ [ Label end_lbl ]
  
  | While (cond, body) ->
    let loop_lbl = new_label ctx "loop" in
    let end_lbl = new_label ctx "endloop" in
    ctx.break_labels <- end_lbl :: ctx.break_labels;
    ctx.continue_labels <- loop_lbl :: ctx.continue_labels;
    let cond_reg, cond_instrs = gen_expr ctx cond in
    let body_items = gen_stmt ctx frame_size body in
    ctx.break_labels <- List.tl ctx.break_labels;
    ctx.continue_labels <- List.tl ctx.continue_labels;
    release_temp_reg ctx;
    [ Label loop_lbl ]
    @ List.map (fun i -> Instruction i) cond_instrs
    @ [ Instruction (Beq (cond_reg, Zero, end_lbl)) ]
    @ body_items
    @ [ Instruction (J loop_lbl); Label end_lbl ]
  
  | Break ->
    (match ctx.break_labels with
     | lbl :: _ -> [ Instruction (J lbl) ]
     | [] -> failwith "Break outside loop")
  
  | Continue ->
    (match ctx.continue_labels with
     | lbl :: _ -> [ Instruction (J lbl) ]
     | [] -> failwith "Continue outside loop")
  
  | Decl (name, e) ->
    let offset = add_local_var ctx name in
    let reg, instrs = gen_expr ctx e in
    let all_instrs = instrs @ [ Sw (reg, offset, Fp) ] in
    release_temp_reg ctx;
    List.map (fun i -> Instruction i) all_instrs
  
  | Assign (name, e) ->
    let offset = get_var_offset ctx name in
    let reg, instrs = gen_expr ctx e in
    let all_instrs = instrs @ [ Sw (reg, offset, Fp) ] in
    release_temp_reg ctx;
    List.map (fun i -> Instruction i) all_instrs


(* 计算栈帧大小 *)
let calculate_frame_size func_def =
  let all_vars = pre_scan_function func_def in
  let num_vars = List.length all_vars in
  let required = 8 + (num_vars * 4) in  (* 8字节用于ra和fp *)
  (required + 15) / 16 * 16  (* 16字节对齐 *)


(* 生成函数代码 *)
let gen_function func_def =
  let ctx = create_context () in
  let frame_size = calculate_frame_size func_def in
  
  (* 预注册所有变量 *)
  let all_vars = pre_scan_function func_def in
  List.iter (fun var -> ignore (add_local_var ctx var)) all_vars;
  
  (* 生成序言 *)
  let prologue = gen_prologue frame_size in
  
  (* 处理参数 *)
  let param_instrs =
    List.mapi (fun i { pname = name; _ } ->
      let offset = get_var_offset ctx name in
      if i < 8 then
        let arg_reg = match i with
          | 0 -> A0 | 1 -> A1 | 2 -> A2 | 3 -> A3
          | 4 -> A4 | 5 -> A5 | 6 -> A6 | 7 -> A7
          | _ -> failwith "Invalid parameter index"
        in
        [ Instruction (Sw (arg_reg, offset, Fp)) ]
      else
        let stack_off = (i - 8) * 4 + 16 in
        [ Instruction (Lw (T0, stack_off, Sp));
          Instruction (Sw (T0, offset, Fp)) ])
      func_def.params
    |> List.flatten in
  
  (* 重置上下文准备生成代码 *)
  ctx.stack_offset <- -4;  (* 关键修复：从-4开始 *)
  ctx.local_vars <- [];
  List.iter (fun var -> ignore (add_local_var ctx var)) all_vars;
  
  (* 生成函数体 *)
  let body =
    func_def.body
    |> List.map (gen_stmt ctx frame_size)
    |> List.flatten in
  
  (* 生成尾声（如果需要） *)
  let has_ret = List.exists (function Instruction Ret -> true | _ -> false) body in
  let epilogue = if has_ret then [] else
    List.map (fun i -> Instruction i) (gen_epilogue frame_size) in
  
  prologue @ param_instrs @ body @ epilogue


(* 生成整个程序的汇编代码 *)
let gen_program program =
  let header = [
    Directive ".text";
    Directive ".globl main";
    Comment "Generated by ToyC Compiler"
  ] in
  
  let funcs =
    List.map (fun func ->
      [ Label func.fname; Comment ("Function: " ^ func.fname) ] @ gen_function func)
    program
    |> List.flatten in
  
  String.concat "\n" (List.map asm_item_to_string (header @ funcs))


(* 编译到RISC-V汇编文件 *)
let compile_to_riscv program output_file =
  let asm = gen_program program in
  let oc = open_out output_file in
  output_string oc asm;
  close_out oc
