(* 主程序 *)

open Lexer
open Parser
open Semantic
open Codegen
open Optimize

let () =
  try
    (* 从标准输入读取源代码 *)
    let input = really_input_string stdin (in_channel_length stdin) in
    
    (* 词法分析 *)
    let lexbuf = Lexing.from_string input in
    let ast = program token lexbuf in
    
    (* 语义分析 *)
    let checked_ast = check_program ast in
    
    (* 检查是否启用优化 *)
    let optimize = 
      Array.fold_left (fun acc arg -> acc || arg = "-opt") false Sys.argv 
    in
    
    (* 优化AST *)
    let optimized_ast = 
      if optimize then fold_constants checked_ast
      else checked_ast 
    in
    
    (* 代码生成 *)
    let assembly = gen_program optimized_ast in
    
    (* 输出到标准输出 *)
    print_string assembly
    
  with
  
  | Failure msg ->
      Printf.eprintf "Semantic error: %s\n" msg;
      exit 1
  | e ->
      Printf.eprintf "Unknown error: %s\n" (Printexc.to_string e);
      exit 1  

