(* 主程序 *)

open Lexer
open Parser
open Semantic
open Codegen
open Optimize

(* 从标准输入读取所有内容（兼容流式输入） *)
let read_all_stdin () =
  let buf = Buffer.create 1024  (* 初始化缓冲区，初始大小1024字节 *)
  and chunk_size = 4096 in       (* 每次读取的块大小 *)
  let buffer = Bytes.create chunk_size in
  let rec read_loop () =
    match input stdin buffer 0 chunk_size with
    | 0 -> Buffer.contents buf  (* 读取到末尾，返回所有内容 *)
    | n ->
        Buffer.add_subbytes buf buffer 0 n;  (* 将读取的字节添加到缓冲区 *)
        read_loop ()
  in
  read_loop ()

let () =
  try
    (* 从标准输入读取源代码（替换原读取方式） *)
    let input = read_all_stdin () in
    
    (* 词法分析 *)
    let lexbuf = Lexing.from_string input in
    (* 设置位置信息的文件名（用于错误提示） *)
    Lexing.set_filename lexbuf "stdin";
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
