open Ast

(* RISC-V 32位寄存器定义 *)
type reg =
  | Zero | Ra | Sp | Gp | Tp 
  | T0 | T1 | T2 | T3 | T4 | T5 | T6  (* 调用者保存寄存器 *)
  | S0 | S1 | S2 | S3 | S4 | S5 | S6 | S7 | S8 | S9 | S10 | S11  (* 被调用者保存寄存器 *)
  | A0 | A1 | A2 | A3 | A4 | A5 | A6 | A7  (* 参数/结果寄存器，调用者保存 *)

(* 寄存器类型分类 *)
type reg_class =
  | CallerSaved  (* 调用者负责保存 *)
  | CalleeSaved  (* 被调用者负责保存 *)
  | Special      (* 特殊寄存器(zero, ra, sp等) *)

(* 获取寄存器分类 *)
let reg_class = function
  | Zero | Ra | Sp | Gp | Tp -> Special
  | T0 | T1 | T2 | T3 | T4 | T5 | T6 -> CallerSaved
  | S0 | S1 | S2 | S3 | S4 | S5 | S6 | S7 | S8 | S9 | S10 | S11 -> CalleeSaved
  | A0 | A1 | A2 | A3 | A4 | A5 | A6 | A7 -> CallerSaved

(* 将寄存器转换为字符串 *)
let reg_to_string = function
  | Zero -> "zero" | Ra -> "ra" | Sp -> "sp" | Gp -> "gp" | Tp -> "tp"
  | T0 -> "t0" | T1 -> "t1" | T2 -> "t2" | T3 -> "t3" | T4 -> "t4" | T5 -> "t5" | T6 -> "t6"
  | S0 -> "s0" | S1 -> "s1" | S2 -> "s2" | S3 -> "s3" | S4 -> "s4" | S5 -> "s5"
  | S6 -> "s6" | S7 -> "s7" | S8 -> "s8" | S9 -> "s9" | S10 -> "s10" | S11 -> "s11"
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

(* 汇编代码项 *)
type asm_item =
  | Label of string
  | Instruction of instruction
  | Comment of string
  | Directive of string

(* 将汇编项转换为字符串 *)
let asm_item_to_string = function
  | Instruction instr -> "    " ^ instr_to_string instr
  | Label l -> l ^ ":"
  | Directive d -> "    " ^ d
  | Comment c -> "    # " ^ c

(* 变量活跃区间表示 *)
type interval = {
  start_pos: int;
  end_pos: int;
}

(* 变量信息 *)
type var_info = {
  var_name: string;
  var_interval: interval;
  mutable var_reg: reg option;  (* 分配的寄存器 *)
  mutable stack_offset: int option;  (* 栈偏移，如果溢出到栈 *)
  mutable is_active: bool;      (* 标记变量是否在当前作用域活跃 *)
  scope_level: int;             (* 变量所在的作用域级别 *)
}

(* 代码生成上下文 *)
type codegen_context = {
  (* 标签和计数器 *)
  mutable label_counter: int;
  mutable temp_counter: int;
  mutable pos_counter: int;  (* 用于跟踪指令位置，计算活跃区间 *)
  
  (* 栈管理 *)
  mutable stack_offset: int;  (* 当前栈偏移，基于fp *)
  mutable frame_size: int;    (* 栈帧大小 *)
  mutable max_stack_usage: int; (* 跟踪最大栈使用量 *)
  
  (* 控制流标签栈 *)
  mutable break_labels: string list;
  mutable continue_labels: string list;
  
  (* 变量和寄存器管理 *)
  mutable local_vars: (string * var_info) list;  (* 局部变量信息 *)
  mutable regs_in_use: (reg * bool) list;  (* 寄存器使用状态 *)
  mutable spilled_vars: (string * int) list;  (* 溢出到栈的变量 *)
  
  (* 函数信息 *)
  func_name: string;
  mutable used_callee_regs: reg list;  (* 被使用的被调用者保存寄存器 *)
  
  (* 作用域管理 *)
  mutable scope_level: int;  (* 当前作用域级别 *)
}

(* 创建新的代码生成上下文 *)
let create_context func_name = {
  label_counter = 0;
  temp_counter = 0;
  pos_counter = 0;
  stack_offset = 0;  (* 从0开始计算局部变量偏移 *)
  frame_size = 0;
  max_stack_usage = 0;  (* 初始化最大栈使用量 *)
  break_labels = [];
  continue_labels = [];
  local_vars = [];
  regs_in_use = [
    (T0, false); (T1, false); (T2, false); (T3, false); (T4, false); (T5, false); (T6, false);
    (S1, false); (S2, false); (S3, false); (S4, false); (S5, false);
    (S6, false); (S7, false); (S8, false); (S9, false); (S10, false); (S11, false);
    (A0, false); (A1, false); (A2, false); (A3, false); (A4, false); (A5, false); (A6, false); (A7, false)
  ];
  spilled_vars = [];
  func_name;
  used_callee_regs = [];
  scope_level = 0;  (* 初始作用域级别为0 *)
}

(* 生成新标签 *)
let new_label ctx prefix =
  let label = Printf.sprintf "%s_%s%d" ctx.func_name prefix ctx.label_counter in
  ctx.label_counter <- ctx.label_counter + 1;
  label

(* 更新位置计数器 *)
let update_pos ctx =
  ctx.pos_counter <- ctx.pos_counter + 1

(* 标记寄存器使用状态 *)
let set_reg_used ctx reg used =
  ctx.regs_in_use <- List.map (fun (r, u) -> if r = reg then (r, used) else (r, u)) ctx.regs_in_use;
  (* 如果是被调用者保存寄存器且被使用，添加到列表 *)
  if reg_class reg = CalleeSaved && used && not (List.mem reg ctx.used_callee_regs) then
    ctx.used_callee_regs <- reg :: ctx.used_callee_regs

(* 查找空闲寄存器 *)
let find_free_reg ctx prefer_callee =
  (* 优先选择被调用者保存寄存器（适合长生命周期变量） *)
  if prefer_callee then
    match List.find_opt (fun (r, used) -> not used && reg_class r = CalleeSaved) ctx.regs_in_use with
    | Some (reg, _) -> Some reg
    | None ->
        (* 再找调用者保存寄存器 *)
        List.find_opt (fun (r, used) -> not used && reg_class r = CallerSaved) ctx.regs_in_use
        |> Option.map fst
  else
    (* 优先选择调用者保存寄存器（适合短生命周期变量） *)
    match List.find_opt (fun (r, used) -> not used && reg_class r = CallerSaved) ctx.regs_in_use with
    | Some (reg, _) -> Some reg
    | None ->
        (* 再找被调用者保存寄存器 *)
        List.find_opt (fun (r, used) -> not used && reg_class r = CalleeSaved) ctx.regs_in_use
        |> Option.map fst

(* 溢出寄存器到栈 *)
let spill_reg ctx reg =
  let offset = - (ctx.stack_offset + 4) in  (* 使用负偏移，栈向下生长 *)
  ctx.stack_offset <- ctx.stack_offset + 4;
  (* 更新最大栈使用量 *)
  if ctx.stack_offset > ctx.max_stack_usage then
    ctx.max_stack_usage <- ctx.stack_offset;
  ctx.spilled_vars <- (reg_to_string reg, offset) :: ctx.spilled_vars;
  [Sw (reg, offset, S0)], offset  (* 使用S0作为帧指针 *)

(* 从栈恢复寄存器 *)
let restore_reg _ reg offset =
  [Lw (reg, offset, S0)]  (* 使用S0作为帧指针 *)

(* 分配寄存器 - 改进的寄存器分配策略 *)
let allocate_reg ctx prefer_callee =
  match find_free_reg ctx prefer_callee with
  | Some reg ->
      set_reg_used ctx reg true;
      reg, []
  | None ->
      (* 没有空闲寄存器，需要溢出一个 *)
      let spill_candidate = 
        (* 优先溢出调用者保存寄存器 *)
        List.find_opt (fun (r, used) -> used && reg_class r = CallerSaved) ctx.regs_in_use
        |> Option.map fst
        (* 如果没有，溢出被调用者保存寄存器 *)
        |> function None -> List.find (fun (_, used) -> used) ctx.regs_in_use |> fst | Some r -> r
      in
      let spill_instrs, _ = spill_reg ctx spill_candidate in
      set_reg_used ctx spill_candidate false;
      let reg = spill_candidate in
      set_reg_used ctx reg true;
      reg, spill_instrs

(* 将变量添加到上下文 - 改进：添加作用域级别 *)
let add_local_var ctx name start_pos end_pos =
  let var_info = {
    var_name = name;
    var_interval = { start_pos; end_pos };
    var_reg = None;
    stack_offset = None;
    is_active = true;  (* 新变量默认在当前作用域活跃 *)
    scope_level = ctx.scope_level;  (* 记录当前作用域级别 *)
  } in
  ctx.local_vars <- (name, var_info) :: ctx.local_vars;
  var_info

(* 获取变量信息 - 改进：搜索所有活跃的作用域 *)
let get_var_info ctx name =
  let rec search_vars = function
    | [] -> None
    | (n, info) :: rest ->
        if n = name && info.is_active then Some info
        else search_vars rest
  in
  match search_vars ctx.local_vars with
  | Some info -> info
  | None -> failwith (Printf.sprintf "Variable '%s' not found in scope (current level: %d)" name ctx.scope_level)

(* 为变量分配寄存器 *)
let allocate_var_reg ctx var_name start_pos end_pos =
  let var_info = get_var_info ctx var_name in
  match var_info.var_reg with
  | Some reg -> reg, []
  | None ->
      (* 对于长生命周期变量优先分配被调用者保存寄存器 *)
      let prefer_callee = end_pos - start_pos > 5 in  (* 简单启发式判断 *)
      let reg, instrs = allocate_reg ctx prefer_callee in
      var_info.var_reg <- Some reg;
      reg, instrs

(* 释放变量使用的寄存器 *)
let release_var_reg ctx var_name =
  let var_info = get_var_info ctx var_name in
  match var_info.var_reg with
  | Some reg ->
      set_reg_used ctx reg false;
      var_info.var_reg <- None
  | None -> ()

(* 标记特定作用域级别的变量为不活跃 *)
(* 标记特定作用域级别的变量为不活跃 *)
let deactivate_scope_vars ctx level =
  ctx.local_vars <- List.map (fun (name, (info : var_info)) ->  (* 显式指定info的类型 *)
    if info.scope_level = level then 
      (name, { info with is_active = false })  (* 现在明确知道info是var_info类型 *)
    else 
      (name, info)
  ) ctx.local_vars

(* 安全释放变量寄存器的辅助函数 *)
let safe_release_var_reg ctx expr =
  match expr with
  | Ast.Var id -> 
      (try release_var_reg ctx id with _ -> ())  (* 忽略未找到的变量 *)
  | _ -> ()

(* 生成表达式代码 *)
let rec gen_expr ctx (expr : Ast.expr) : reg * instruction list =
  update_pos ctx;
  match expr with
  | Ast.Literal(IntLit n) ->
      let reg, instrs = allocate_reg ctx false in  (* 字面量使用调用者保存寄存器 *)
      reg, instrs @ [Li (reg, n)]
      
  | Ast.Var id ->
      let var_info = get_var_info ctx id in
      let start_pos = ctx.pos_counter in
      let end_pos = start_pos + 1 in  (* 简单估计 *)
      let reg, alloc_instrs = allocate_var_reg ctx id start_pos end_pos in
      let instrs = 
        match var_info.stack_offset with
        | Some offset -> alloc_instrs @ [Lw (reg, offset, S0)]
        | None -> alloc_instrs
      in
      reg, instrs
      
  | Ast.Paren e -> gen_expr ctx e
      
  | Ast.UnOp (op, e) ->
      let e_reg, e_instrs = gen_expr ctx e in
      let result_reg, alloc_instrs = allocate_reg ctx false in
      let op_instrs =
        match op with
        | "+" -> [Mv (result_reg, e_reg)]
        | "-" -> [Sub (result_reg, Zero, e_reg)]
        | "!" -> [Sltiu (result_reg, e_reg, 1)]
        | _ -> failwith (Printf.sprintf "Unknown unary operator: %s" op)
      in
      (* 安全释放操作数寄存器 - 只在是变量时才释放 *)
      safe_release_var_reg ctx e;
      result_reg, e_instrs @ alloc_instrs @ op_instrs

      
  | Ast.BinOp (e1, op, e2) ->
      let e1_reg, e1_instrs = gen_expr ctx e1 in
      let e2_reg, e2_instrs = gen_expr ctx e2 in
      let result_reg, alloc_instrs = allocate_reg ctx false in
      let op_instrs =
        match op with
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
        | _ -> failwith (Printf.sprintf "Unknown binary operator: %s" op)
      in
      (* 安全释放操作数寄存器 - 只在是变量时才释放 *)
      safe_release_var_reg ctx e1;
      safe_release_var_reg ctx e2;
      result_reg, e1_instrs @ e2_instrs @ alloc_instrs @ op_instrs
      
  | Ast.Call (fname, args) ->
      let result_reg = A0 in
      set_reg_used ctx result_reg true;
      
      (* 计算需要通过栈传递的参数数量 *)
      let num_stack_args = max 0 (List.length args - 8) in
      
      (* 保存需要保留的调用者保存寄存器 *)
      let caller_regs_to_save = 
        List.filter (fun (r, used) -> 
          used && reg_class r = CallerSaved && r <> result_reg
        ) ctx.regs_in_use
        |> List.map fst
      in
      
      (* 计算栈空间需求，确保16字节对齐 *)
      let stack_args_space = num_stack_args * 4 in
      let saved_regs_space = List.length caller_regs_to_save * 4 in
      let stack_space = stack_args_space + saved_regs_space in
      let stack_space = (stack_space + 15) land lnot 15 in  (* 16字节对齐 *)
      
      (* 为栈操作预留空间 *)
      let save_instrs = 
        if stack_space > 0 then
          Addi (Sp, Sp, -stack_space) ::
          (* 保存调用者保存寄存器 *)
          List.mapi (fun i r ->
            Sw (r, i * 4, Sp)  (* 从栈顶开始保存 *)
          ) caller_regs_to_save
        else []
      in
      
      (* 处理参数：前8个用寄存器，其余用栈 *)
      let arg_instrs =
        List.mapi
          (fun i arg ->
            let arg_reg, arg_code = gen_expr ctx arg in
            if i < 8 then
              (* 前8个参数使用a0-a7寄存器 *)
              let target_reg =
                match i with
                | 0 -> A0 | 1 -> A1 | 2 -> A2 | 3 -> A3
                | 4 -> A4 | 5 -> A5 | 6 -> A6 | 7 -> A7
                | _ -> failwith "Invalid register index"
              in
              let move_instr = if arg_reg = target_reg then [] else [Mv (target_reg, arg_reg)] in
              arg_code @ move_instr
            else
              (* 超过8个的参数使用栈传递 *)
              let stack_pos = saved_regs_space + (i - 8) * 4 in
              arg_code @ [Sw (arg_reg, stack_pos, Sp)]
          ) args
        |> List.flatten
      in
      
      (* 释放参数寄存器 *)
      List.iter (fun arg -> safe_release_var_reg ctx arg) args;
      
      (* 函数调用指令 - 确保调用前栈对齐 *)
      let call_instr = [Jal (Ra, fname)] in
      
      (* 恢复寄存器和栈指针 *)
      let restore_instrs =
        if stack_space > 0 then
          (* 恢复调用者保存寄存器 *)
          List.mapi (fun i r ->
            Lw (r, i * 4, Sp)
          ) caller_regs_to_save
          @ [Addi (Sp, Sp, stack_space)]
        else []
      in
      
      result_reg, save_instrs @ arg_instrs @ call_instr @ restore_instrs

(* 生成函数序言 *)
let gen_prologue ctx =
  (* 保存被调用者保存寄存器 *)
  let save_callee_regs =
    List.mapi (fun i reg ->
      Sw (reg, 8 + 4 * i, Sp)  (* 从ra和s0之后开始保存 (8字节) *)
    ) ctx.used_callee_regs
  in
  
  [ Instruction (Addi (Sp, Sp, -ctx.frame_size));  (* 分配栈帧 *)
    Instruction (Sw (Ra, 4, Sp));  (* 保存返回地址 *)
    Instruction (Sw (S0, 0, Sp));  (* 保存旧帧指针 *)
    Instruction (Addi (S0, Sp, ctx.frame_size)) ]  (* 设置帧指针 *)
  @ List.map (fun i -> Instruction i) save_callee_regs

(* 生成函数尾声 *)
let gen_epilogue ctx =
  (* 恢复被调用者保存寄存器 *)
  let restore_callee_regs =
    List.mapi (fun i reg ->
      Lw (reg, 8 + 4 * i, Sp)
    ) ctx.used_callee_regs
  in
  
  List.map (fun i -> Instruction i) restore_callee_regs
  @ [ Instruction (Lw (Ra, 4, Sp));  (* 恢复返回地址 *)
      Instruction (Lw (S0, 0, Sp));  (* 恢复帧指针 *)
      Instruction (Addi (Sp, Sp, ctx.frame_size));  (* 释放栈帧 *)
      Instruction Ret ]

(* 递归统计声明数量和收集变量名 *)
let rec count_decls_in_stmt = function
  | Ast.Decl (name, _) -> 1, [name]
  | Ast.Block stmts -> 
      List.fold_left (fun (cnt, vars) s ->
        let c, v = count_decls_in_stmt s in
        (cnt + c, vars @ v)
      ) (0, []) stmts
  | Ast.If (_, s1, Some s2) ->
      let c1, v1 = count_decls_in_stmt s1 in
      let c2, v2 = count_decls_in_stmt s2 in
      (c1 + c2, v1 @ v2)
  | Ast.If (_, s1, None) -> count_decls_in_stmt s1
  | Ast.While (_, s) -> count_decls_in_stmt s
  | _ -> 0, []

(* 计算函数所需的栈帧大小 *)
let calculate_frame_size func_def ctx =
  let total_decls, _ = List.fold_left (fun (acc, vars) stmt ->
    let c, v = count_decls_in_stmt stmt in
    (acc + c, vars @ v)
  ) (0, []) func_def.body in
  
  let num_locals = total_decls in
  let num_params = List.length func_def.params in
  let num_callee_saved = List.length ctx.used_callee_regs in
  
  (* 计算基本栈空间需求：ra(4) + s0(4) + 被调用者保存寄存器 + 局部变量和参数 *)
  let base_size = 
    8  (* ra和fp的保存区 *)
    + num_callee_saved * 4  (* 被调用者保存寄存器 *)
    + (num_locals + num_params) * 4  (* 局部变量和参数 *)
    + ctx.max_stack_usage * 4  (* 溢出变量所需空间 *)
  in
  
  (* 16字节对齐 *)
  let aligned_size = (base_size + 15) land lnot 15 in
  ctx.frame_size <- aligned_size;
  aligned_size

(* 生成语句代码 - 改进：正确的块作用域处理 *)
let rec gen_stmt ctx (stmt : Ast.stmt) : asm_item list =
  update_pos ctx;
  match stmt with
  | Ast.Empty -> []
  
  | Ast.ExprStmt e ->
      let _, instrs = gen_expr ctx e in
      List.map (fun i -> Instruction i) instrs
  
  | Ast.Block stmts ->
      (* 增加作用域级别 *)
      ctx.scope_level <- ctx.scope_level + 1;
      let current_level = ctx.scope_level in
      
      (* 保存当前栈偏移和最大栈使用量 *)
      let old_offset = ctx.stack_offset in
      let old_max_stack = ctx.max_stack_usage in
      
      (* 生成块内语句 *)
      let items = List.map (gen_stmt ctx) stmts |> List.flatten in
      
      (* 恢复作用域级别并标记该级别变量为不活跃 *)
      ctx.scope_level <- ctx.scope_level - 1;
      deactivate_scope_vars ctx current_level;
      
      (* 恢复栈偏移和最大栈使用量 *)
      ctx.stack_offset <- old_offset;
      ctx.max_stack_usage <- old_max_stack;
      
      items
  
  | Ast.Return (Some e) ->
    let e_reg, e_instrs = gen_expr ctx e in
    let move_instr = if e_reg = A0 then [] else [Mv (A0, e_reg)] in
    List.map (fun i -> Instruction i) e_instrs 
    @ List.map (fun i -> Instruction i) move_instr 
    @ gen_epilogue ctx
  
  | Ast.Return None ->
      [Instruction (Li (A0, 0))] @ gen_epilogue ctx
  
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
      let start_pos = ctx.pos_counter in
      let end_pos = start_pos + 10 in  (* 估计变量生命周期 *)
      let var_info = add_local_var ctx name start_pos end_pos in
      let e_reg, e_instrs = gen_expr ctx e in
      
      (* 为变量分配栈空间 - 使用负偏移 *)
      let offset = - (ctx.stack_offset + 4) in
      ctx.stack_offset <- ctx.stack_offset + 4;
      (* 更新最大栈使用量 *)
      if ctx.stack_offset > ctx.max_stack_usage then
        ctx.max_stack_usage <- ctx.stack_offset;
      var_info.stack_offset <- Some offset;
      
      (* 为变量分配寄存器 *)
      let _, alloc_instrs = allocate_var_reg ctx name start_pos end_pos in
      
      let store_instr = [Sw (e_reg, offset, S0)] in
      List.map (fun i -> Instruction i) (e_instrs @ alloc_instrs @ store_instr)
  
  | Ast.Assign (name, e) ->
      let var_info = get_var_info ctx name in
      let e_reg, e_instrs = gen_expr ctx e in
      
      let store_instr = 
        match var_info.var_reg with
        | Some reg -> [Mv (reg, e_reg)]  (* 如果在寄存器中，直接移动 *)
        | None -> 
            (* 否则存储到栈 *)
            match var_info.stack_offset with
            | Some offset -> [Sw (e_reg, offset, S0)]
            | None -> failwith "Variable not allocated"
      in
      
      List.map (fun i -> Instruction i) (e_instrs @ store_instr)

(* 生成函数代码 *)
let gen_function (func_def : Ast.func_def) : asm_item list =
  let ctx = create_context func_def.fname in
  
  (* 预扫描函数体，收集变量信息 *)
  let rec pre_scan_stmt stmt =
    match stmt with
    | Ast.Decl (name, _) ->
        let start_pos = 0 in
        let end_pos = 100 in  (* 简化处理 *)
        ignore (add_local_var ctx name start_pos end_pos)
    | Ast.Block stmts -> 
        ctx.scope_level <- ctx.scope_level + 1;
        List.iter pre_scan_stmt stmts;
        ctx.scope_level <- ctx.scope_level - 1
    | Ast.If (_, s1, Some s2) -> pre_scan_stmt s1; pre_scan_stmt s2
    | Ast.If (_, s1, None) -> pre_scan_stmt s1
    | Ast.While (_, s) -> pre_scan_stmt s
    | _ -> ()
  in
  
  List.iter pre_scan_stmt func_def.body;
  
  (* 处理参数 *)
  let _ =
    List.mapi
      (fun i { Ast.pname = name; _ } ->
        let start_pos = 0 in
        let end_pos = 100 in
        let var_info = add_local_var ctx name start_pos end_pos in
        let offset = - (ctx.stack_offset + 4) in
        ctx.stack_offset <- ctx.stack_offset + 4;
        if ctx.stack_offset > ctx.max_stack_usage then
          ctx.max_stack_usage <- ctx.stack_offset;
        var_info.stack_offset <- Some offset;
        
        if i < 8 then
          (* 前8个参数使用a0-a7寄存器 *)
          let arg_reg =
            match i with
            | 0 -> A0 | 1 -> A1 | 2 -> A2 | 3 -> A3
            | 4 -> A4 | 5 -> A5 | 6 -> A6 | 7 -> A7
            | _ -> failwith "Invalid parameter index"
          in
          [Instruction (Sw (arg_reg, offset, S0))]
        else
          (* 超过8个的参数从栈中读取 (调用者栈帧) *)
          let stack_offset = (i - 8) * 4 + 16 in  (* 正确计算栈上参数偏移 *)
          [Instruction (Lw (T0, stack_offset, Sp));
           Instruction (Sw (T0, offset, S0))]
      ) func_def.params
    |> List.flatten
  in
  
  (* 生成函数体以确定使用的寄存器 *)
  let _ = List.map (gen_stmt ctx) func_def.body in
  
  (* 计算栈帧大小 *)
  let _ = calculate_frame_size func_def ctx in
  
  (* 重置上下文进行实际代码生成 *)
  let ctx = create_context func_def.fname in
  List.iter pre_scan_stmt func_def.body;
  
  (* 重新处理参数 *)
  let param_instrs =
    List.mapi
      (fun i { Ast.pname = name; _ } ->
        let start_pos = 0 in
        let end_pos = 100 in
        let var_info = add_local_var ctx name start_pos end_pos in
        let offset = - (ctx.stack_offset + 4) in
        ctx.stack_offset <- ctx.stack_offset + 4;
        if ctx.stack_offset > ctx.max_stack_usage then
          ctx.max_stack_usage <- ctx.stack_offset;
        var_info.stack_offset <- Some offset;
        
        if i < 8 then
          let arg_reg =
            match i with
            | 0 -> A0 | 1 -> A1 | 2 -> A2 | 3 -> A3
            | 4 -> A4 | 5 -> A5 | 6 -> A6 | 7 -> A7
            | _ -> failwith "Invalid parameter index"
          in
          [Instruction (Sw (arg_reg, offset, S0))]
        else
          let stack_offset = (i - 8) * 4 + 16 in
          [Instruction (Lw (T0, stack_offset, Sp));
           Instruction (Sw (T0, offset, S0))]
      ) func_def.params
    |> List.flatten
  in
  
  (* 重新计算栈帧大小 *)
  let _ = calculate_frame_size func_def ctx in
  
  (* 生成函数序言 *)
  let prologue = gen_prologue ctx in
  
  (* 生成函数体 *)
  let body_items =
    func_def.body
    |> List.map (gen_stmt ctx)
    |> List.flatten
  in
  
  (* 检查是否有返回指令，如果没有则添加默认返回 *)
  let has_return =
    List.exists
      (function
       | Instruction Ret -> true
       | _ -> false)
      body_items
  in
  
  let epilogue = if has_return then [] else gen_epilogue ctx in
  
  [Label func_def.fname; Comment ("Function: " ^ func_def.fname)]
  @ prologue
  @ param_instrs
  @ body_items
  @ epilogue

(* 生成整个程序的代码 *)
let gen_program (program : Ast.program) =
  let header = [
    Directive ".text";
    Directive ".globl main";
    Comment "Generated by RISC-V Code Generator (遵循ABI规范)";
    Comment "-----------------------------------------"
  ] in
  
  let func_asm_items =
    List.map
      (fun func_def ->
        let items = gen_function func_def in
        items @ [Comment "-----------------------------------------"])
      program
    |> List.flatten
  in
  
  header @ func_asm_items

(* 输出汇编代码到文件 *)
let compile_to_riscv filepath program =
  let asm_items = gen_program program in
  let file = open_out filepath in
  List.iter
    (fun item ->
      output_string file (asm_item_to_string item);
      output_string file "\n")
    asm_items;
  close_out file

(* 输出汇编代码到标准输出 *)
let compile_to_stdout program =
  let asm_items = gen_program program in
  List.iter (fun item -> print_endline (asm_item_to_string item)) asm_items
