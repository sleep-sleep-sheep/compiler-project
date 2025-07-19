(* 词法分析器 *)

{
open Parser

let keywords = Hashtbl.create 10
let _ =
  List.iter (fun (kw, tok) -> Hashtbl.add keywords kw tok)
    [ "int", INT; "void", VOID; "if", IF; "else", ELSE; 
      "while", WHILE; "break", BREAK; "continue", CONTINUE; 
      "return", RETURN ]

let is_digit = function '0'..'9' -> true | _ -> false
let is_alpha = function 'a'..'z' | 'A'..'Z' | '_' -> true | _ -> false
let is_alphanum = function 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> true | _ -> false
}

rule token = parse
  | [' ' '\t' '\r']      { token lexbuf }
  | '\n'                { Lexing.new_line lexbuf; token lexbuf }
  | "//" [^'\n']* '\n'  { Lexing.new_line lexbuf; token lexbuf }
  | "/*"                { comment lexbuf }
  | '+'                 { PLUS }
  | '-'                 { MINUS }
  | '*'                 { TIMES }
  | '/'                 { DIVIDE }
  | '%'                 { MOD }
  | '!'                 { NOT }
  | '('                 { LPAREN }
  | ')'                 { RPAREN }
  | '{'                 { LBRACE }
  | '}'                 { RBRACE }
  | ';'                 { SEMI }
  | '='                 { ASSIGN }
  | "=="                { EQ }
  | "!="                { NEQ }
  | '<'                 { LT }
  | ">"                 { GT }
  | "<="                { LEQ }
  | ">="                { GEQ }
  | "&&"                { AND }
  | "||"                { OR }
  | ','                 { COMMA }
  | ['0'-'9']+ as num   { NUMBER (int_of_string num) }
  | ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* as id
    { try Hashtbl.find keywords id with Not_found -> ID id }
  | eof                 { EOF }
  | _ as char           { failwith ("Illegal character: " ^ Char.escaped char) }

and comment = parse
  | "*/"                { token lexbuf }
  | '\n'                { Lexing.new_line lexbuf; comment lexbuf }
  | _                   { comment lexbuf }
  | eof                 { failwith "Unclosed comment" }  