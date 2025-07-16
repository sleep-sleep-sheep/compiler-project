type token =
  | INT of (int)
  | ID of (string)
  | INT_KW
  | VOID_KW
  | IF
  | ELSE
  | WHILE
  | BREAK
  | CONTINUE
  | RETURN
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | PERCENT
  | EQ
  | NE
  | LT
  | GT
  | LE
  | GE
  | ASSIGN
  | ANDAND
  | OROR
  | BANG
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | SEMI
  | COMMA
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
