(* 主程序 - 纯标准输入输出版本 *)

open Lexer
open Parser
open Semantic
open Codegen
open Optimize
open Print_ast

let () =
  try
    (* 解析命令行选项，只处理-ast和-opt，忽略其他参数 *)
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
      Printf.eprintf "===== Original AST =====\n";  (* 使用标准错误输出AST *)
      print_program ast;
      Printf.eprintf "\n"
    end;
    
    (* 语义分析 *)
    let checked_ast = check_program ast in
    
    (* 优化处理 *)
    let optimized_ast = if optimize then fold_constants checked_ast else checked_ast in
    
    (* 打印优化后的AST（如果需要） *)
    if print_ast then begin
      Printf.eprintf "===== Optimized AST =====\n";  (* 使用标准错误输出AST *)
      print_program optimized_ast;
      Printf.eprintf "=========================\n\n"
    end;
    
    (* 代码生成并输出到标准输出 *)
    let assembly = gen_program optimized_ast in
    print_string assembly;
    flush stdout
    
  with
  | Sys_error msg ->
      Printf.eprintf "输入错误: %s\n" msg;
      exit 1
  | Failure msg ->
      Printf.eprintf "语义错误: %s\n" msg;
      exit 1
  | e ->
      Printf.eprintf "错误: %s\n" (Printexc.to_string e);
      exit 1
