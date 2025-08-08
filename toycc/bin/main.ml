(* 主程序 *)

open Lexer
open Parser
open Semantic
open Codegen
open Optimize

(* 从标准输入读取所有内容（兼容流式输入） *)
let read_stdin () =
  let buffer = Buffer.create 1024 in  (* 初始化缓冲区 *)
  try
    while true do
      (* 每次读取一行并添加到缓冲区 *)
      let line = input_line stdin in
      Buffer.add_string buffer line;
      Buffer.add_char buffer '\n'  (* 还原换行符 *)
    done;
    Buffer.contents buffer  (* 理论上不会执行到这里 *)
  with End_of_file ->
    Buffer.contents buffer  (* 到达输入末尾，返回所有内容 *)

let () =
  try
    (* 从标准输入读取源代码（使用兼容流式输入的方法） *)
    let input = read_stdin () in
    
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
  | Lexer.Error msg ->
      Printf.eprintf "Lexer error: %s\n" msg;
      exit 1
  | Parser.Error ->
      let pos = Lexing.lexeme_start_p lexbuf in  (* 注意：需要在作用域内获取lexbuf *)
      Printf.eprintf "Parser error at line %d, column %d\n" 
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
      exit 1
  | Failure msg ->
      Printf.eprintf "Semantic error: %s\n" msg;
      exit 1
  | e ->
      Printf.eprintf "Unknown error: %s\n" (Printexc.to_string e);
      exit 1
