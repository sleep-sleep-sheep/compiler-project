open Lexing

let parse_channel channel =
  let lexbuf = from_channel channel in
  try Parser.program Lexer.token lexbuf
  with
  | Parsing.Parse_error ->
      let pos = lexbuf.lex_curr_p in
      failwith (Printf.sprintf "Syntax error at line %d, column %d"
                  pos.pos_lnum (pos.pos_cnum - pos.pos_bol))
let () =
  try
    let ast = parse_channel stdin in
    Semantic.check_program ast;
    let ast_opt = Optimize.optimize_program ast in
    let riscv_code = Codegen.emit_program ast_opt in
    print_string riscv_code;
    flush stdout;
  with
  | Failure msg ->
      print_endline ("Error: " ^ msg)
  | exn ->
      print_endline ("Unexpected error: " ^ Printexc.to_string exn)
