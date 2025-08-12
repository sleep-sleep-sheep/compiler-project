open Lexer
open Parser

let () =
  try
    (* 解析命令行选项 *)
    let print_ast = Array.mem "-ast" Sys.argv in
    let optimize = Array.mem "-opt" Sys.argv in
    
    (* 从标准输入读取所有内容 *)
    let input = 
      let rec read_all acc =
        match try Some (input_line stdin) with End_of_file -> None with
        | Some line -> read_all (acc ^ line ^ "\n")
        | None -> acc
      in
      read_all ""
    in
    
    (* 词法分析与语法分析 *)
    let lexbuf = Lexing.from_string input in
    let ast = program token lexbuf in
    
    (* 打印AST（如果需要） *)
    if print_ast then begin
      Printf.eprintf "===== 抽象语法树 (AST) =====\n";
      Print_ast.print_program ast;
      Printf.eprintf "\n"
    end;
    
    (* 语义分析（根据实际返回类型调整） *)
    let checked_ast = Semantic.check_program ast in  (* 修正：只接收一个返回值 *)
    
    (* 优化处理 *)
    let optimized_ast = if optimize then Optimize.fold_constants checked_ast else checked_ast in
    
    (* 代码生成并输出到标准输出 *)
    (* 假设symbol_table由其他方式获取或不需要，这里直接传递空表或调整参数 *)
    (*Codegen.compile_to_riscv [] optimized_ast;  (* 修正：根据实际参数要求调整 *)
    flush stdout*)
    Codegen.compile_to_stdout optimized_ast;
    flush stdout
    
  with
  | Sys_error msg ->
      Printf.eprintf "系统错误: %s\n" msg;
      exit 1
  | e ->
      Printf.eprintf "未知错误: %s\n" (Printexc.to_string e);
      exit 1
