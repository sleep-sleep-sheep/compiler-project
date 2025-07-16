{ 
  open Parser   (* Menhir parser 接口 *)
}

rule token = parse
| [' ' '\t' '\r' '\n']      { token lexbuf }
| "//" [^ '\n']* '\n'       { token lexbuf }
| "/*"                      { comment lexbuf; token lexbuf }
| ['0'-'9']+ as num         { INT (int_of_string num) }
| ['_' 'A'-'Z' 'a'-'z']['_' 'A'-'Z' 'a'-'z' '0'-'9']* as id {
    match id with
    | "int"      -> INT_KW
    | "void"     -> VOID_KW
    | "if"       -> IF
    | "else"     -> ELSE
    | "while"    -> WHILE
    | "break"    -> BREAK
    | "continue" -> CONTINUE
    | "return"   -> RETURN
    | _          -> ID id
  }
| "=="                      { EQ }
| "="                       { ASSIGN }
| "!="                      { NE }
| "<="                      { LE }
| ">="                      { GE }
| "<"                       { LT }
| ">"                       { GT }
| "&&"                      { ANDAND }
| "||"                      { OROR }
| "!"                       { BANG }
| "+"                       { PLUS }
| "-"                       { MINUS }
| "*"                       { STAR }
| "/"                       { SLASH }
| "%"                       { PERCENT }
| "("                       { LPAREN }
| ")"                       { RPAREN }
| "{"                       { LBRACE }
| "}"                       { RBRACE }
| ";"                       { SEMI }
| ","                       { COMMA }
| eof                       { EOF }
| _                         { failwith "Unexpected character" }

and comment = parse
| "*/"                      { () }
| _                         { comment lexbuf }
