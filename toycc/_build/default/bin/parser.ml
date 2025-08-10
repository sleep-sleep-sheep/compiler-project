type token =
  | NUMBER of (int)
  | ID of (string)
  | INT
  | VOID
  | IF
  | ELSE
  | WHILE
  | BREAK
  | CONTINUE
  | RETURN
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MOD
  | NOT
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | SEMI
  | COMMA
  | ASSIGN
  | EQ
  | NEQ
  | LT
  | GT
  | LEQ
  | GEQ
  | AND
  | OR
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "bin/parser.mly"
open Ast
# 40 "bin/parser.ml"
let yytransl_const = [|
  259 (* INT *);
  260 (* VOID *);
  261 (* IF *);
  262 (* ELSE *);
  263 (* WHILE *);
  264 (* BREAK *);
  265 (* CONTINUE *);
  266 (* RETURN *);
  267 (* PLUS *);
  268 (* MINUS *);
  269 (* TIMES *);
  270 (* DIVIDE *);
  271 (* MOD *);
  272 (* NOT *);
  273 (* LPAREN *);
  274 (* RPAREN *);
  275 (* LBRACE *);
  276 (* RBRACE *);
  277 (* SEMI *);
  278 (* COMMA *);
  279 (* ASSIGN *);
  280 (* EQ *);
  281 (* NEQ *);
  282 (* LT *);
  283 (* GT *);
  284 (* LEQ *);
  285 (* GEQ *);
  286 (* AND *);
  287 (* OR *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* NUMBER *);
  258 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\004\000\004\000\005\000\005\000\
\007\000\007\000\008\000\006\000\009\000\009\000\010\000\010\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\013\000\013\000\012\000\014\000\014\000\
\015\000\015\000\016\000\016\000\016\000\016\000\016\000\016\000\
\016\000\017\000\017\000\017\000\018\000\018\000\018\000\018\000\
\019\000\019\000\019\000\019\000\020\000\020\000\020\000\020\000\
\021\000\021\000\022\000\022\000\000\000"

let yylen = "\002\000\
\002\000\001\000\002\000\006\000\001\000\001\000\000\000\001\000\
\001\000\003\000\002\000\003\000\000\000\001\000\001\000\002\000\
\001\000\001\000\002\000\004\000\005\000\005\000\007\000\005\000\
\002\000\002\000\003\000\000\000\001\000\001\000\001\000\003\000\
\001\000\003\000\001\000\003\000\003\000\003\000\003\000\003\000\
\003\000\001\000\003\000\003\000\001\000\003\000\003\000\003\000\
\001\000\002\000\002\000\002\000\001\000\001\000\003\000\004\000\
\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\005\000\006\000\061\000\000\000\002\000\000\000\
\001\000\003\000\000\000\000\000\000\000\000\000\000\000\009\000\
\011\000\000\000\000\000\000\000\004\000\010\000\054\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\018\000\017\000\000\000\000\000\015\000\000\000\
\000\000\000\000\000\000\000\000\000\000\045\000\049\000\000\000\
\000\000\000\000\000\000\000\000\025\000\026\000\000\000\029\000\
\000\000\051\000\050\000\052\000\000\000\012\000\016\000\019\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\059\000\000\000\000\000\
\000\000\000\000\000\000\000\000\027\000\055\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\046\000\047\000\048\000\056\000\000\000\020\000\000\000\000\000\
\000\000\060\000\021\000\000\000\024\000\000\000\023\000"

let yydgoto = "\002\000\
\005\000\006\000\007\000\008\000\014\000\036\000\015\000\016\000\
\037\000\038\000\039\000\040\000\057\000\041\000\042\000\043\000\
\044\000\045\000\046\000\047\000\079\000\080\000"

let yysindex = "\002\000\
\053\255\000\000\000\000\000\000\000\000\001\000\000\000\003\255\
\000\000\000\000\253\254\024\255\036\255\018\255\022\255\000\000\
\000\000\058\255\024\255\094\255\000\000\000\000\000\000\243\254\
\077\255\069\255\070\255\067\255\079\255\169\255\169\255\169\255\
\169\255\169\255\000\000\000\000\071\255\094\255\000\000\088\255\
\081\255\068\255\148\255\057\255\254\254\000\000\000\000\169\255\
\169\255\093\255\169\255\169\255\000\000\000\000\102\255\000\000\
\109\255\000\000\000\000\000\000\113\255\000\000\000\000\000\000\
\169\255\169\255\169\255\169\255\169\255\169\255\169\255\169\255\
\169\255\169\255\169\255\169\255\169\255\000\000\114\255\111\255\
\115\255\169\255\116\255\119\255\000\000\000\000\068\255\148\255\
\057\255\057\255\057\255\057\255\057\255\057\255\254\254\254\254\
\000\000\000\000\000\000\000\000\169\255\000\000\130\255\094\255\
\094\255\000\000\000\000\134\255\000\000\094\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\135\255\000\000\000\000\136\255\000\000\
\000\000\000\000\000\000\132\255\000\000\000\000\000\000\034\255\
\000\000\000\000\000\000\000\000\000\000\137\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\141\255\000\000\000\000\
\021\255\019\255\013\000\166\255\096\255\000\000\000\000\139\255\
\000\000\000\000\000\000\000\000\000\000\000\000\004\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\160\255\
\000\000\000\000\000\000\000\000\000\000\000\000\020\000\015\000\
\180\255\194\255\208\255\222\255\241\255\255\255\117\255\138\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\073\255\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\149\000\000\000\000\000\161\000\000\000\163\000\
\000\000\000\000\218\255\228\255\000\000\000\000\118\000\123\000\
\241\000\253\255\232\255\000\000\000\000\000\000"

let yytablesize = 313
let yytable = "\063\000\
\009\000\056\000\001\000\048\000\011\000\061\000\058\000\059\000\
\060\000\049\000\075\000\076\000\077\000\012\000\053\000\053\000\
\053\000\053\000\053\000\078\000\081\000\053\000\083\000\084\000\
\053\000\053\000\013\000\053\000\053\000\053\000\053\000\053\000\
\053\000\053\000\053\000\018\000\031\000\017\000\030\000\031\000\
\031\000\030\000\030\000\019\000\053\000\053\000\053\000\053\000\
\053\000\031\000\097\000\098\000\099\000\103\000\053\000\003\000\
\004\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
\053\000\108\000\109\000\073\000\074\000\095\000\096\000\111\000\
\106\000\022\000\022\000\022\000\020\000\022\000\050\000\022\000\
\022\000\022\000\022\000\022\000\022\000\051\000\052\000\053\000\
\022\000\022\000\062\000\022\000\022\000\022\000\023\000\024\000\
\025\000\066\000\026\000\054\000\027\000\028\000\029\000\030\000\
\031\000\032\000\042\000\042\000\064\000\033\000\034\000\065\000\
\020\000\042\000\035\000\082\000\042\000\042\000\048\000\042\000\
\042\000\042\000\042\000\042\000\042\000\042\000\042\000\043\000\
\043\000\085\000\086\000\100\000\101\000\104\000\043\000\102\000\
\105\000\043\000\043\000\110\000\043\000\043\000\043\000\043\000\
\043\000\043\000\043\000\043\000\044\000\044\000\107\000\013\000\
\007\000\008\000\010\000\044\000\057\000\028\000\044\000\044\000\
\014\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
\044\000\023\000\055\000\067\000\068\000\069\000\070\000\071\000\
\072\000\058\000\021\000\031\000\032\000\022\000\087\000\035\000\
\033\000\034\000\035\000\035\000\088\000\035\000\035\000\035\000\
\035\000\035\000\035\000\035\000\035\000\036\000\000\000\000\000\
\036\000\036\000\000\000\036\000\036\000\036\000\036\000\036\000\
\036\000\036\000\036\000\037\000\000\000\000\000\037\000\037\000\
\000\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
\037\000\038\000\000\000\000\000\038\000\038\000\000\000\038\000\
\038\000\038\000\038\000\038\000\038\000\038\000\038\000\039\000\
\000\000\000\000\039\000\039\000\000\000\039\000\039\000\039\000\
\039\000\039\000\039\000\039\000\039\000\000\000\000\000\000\000\
\000\000\000\000\040\000\003\000\004\000\040\000\040\000\000\000\
\040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
\041\000\000\000\000\000\041\000\041\000\000\000\041\000\041\000\
\041\000\041\000\041\000\041\000\041\000\041\000\033\000\000\000\
\034\000\033\000\033\000\034\000\034\000\032\000\000\000\000\000\
\032\000\032\000\033\000\033\000\034\000\034\000\000\000\000\000\
\000\000\000\000\032\000\089\000\090\000\091\000\092\000\093\000\
\094\000"

let yycheck = "\038\000\
\000\000\030\000\001\000\017\001\002\001\034\000\031\000\032\000\
\033\000\023\001\013\001\014\001\015\001\017\001\011\001\012\001\
\013\001\014\001\015\001\048\000\049\000\018\001\051\000\052\000\
\021\001\022\001\003\001\024\001\025\001\026\001\027\001\028\001\
\029\001\030\001\031\001\018\001\018\001\002\001\018\001\021\001\
\022\001\021\001\022\001\022\001\011\001\012\001\013\001\014\001\
\015\001\031\001\075\000\076\000\077\000\082\000\021\001\003\001\
\004\001\024\001\025\001\026\001\027\001\028\001\029\001\030\001\
\031\001\104\000\105\000\011\001\012\001\073\000\074\000\110\000\
\101\000\001\001\002\001\003\001\019\001\005\001\002\001\007\001\
\008\001\009\001\010\001\011\001\012\001\017\001\017\001\021\001\
\016\001\017\001\020\001\019\001\020\001\021\001\001\001\002\001\
\003\001\030\001\005\001\021\001\007\001\008\001\009\001\010\001\
\011\001\012\001\011\001\012\001\021\001\016\001\017\001\031\001\
\019\001\018\001\021\001\023\001\021\001\022\001\017\001\024\001\
\025\001\026\001\027\001\028\001\029\001\030\001\031\001\011\001\
\012\001\021\001\018\001\018\001\022\001\018\001\018\001\021\001\
\018\001\021\001\022\001\006\001\024\001\025\001\026\001\027\001\
\028\001\029\001\030\001\031\001\011\001\012\001\021\001\020\001\
\018\001\018\001\006\000\018\001\018\001\021\001\021\001\022\001\
\020\001\024\001\025\001\026\001\027\001\028\001\029\001\030\001\
\031\001\001\001\002\001\024\001\025\001\026\001\027\001\028\001\
\029\001\018\001\018\000\011\001\012\001\019\000\065\000\018\001\
\016\001\017\001\021\001\022\001\066\000\024\001\025\001\026\001\
\027\001\028\001\029\001\030\001\031\001\018\001\255\255\255\255\
\021\001\022\001\255\255\024\001\025\001\026\001\027\001\028\001\
\029\001\030\001\031\001\018\001\255\255\255\255\021\001\022\001\
\255\255\024\001\025\001\026\001\027\001\028\001\029\001\030\001\
\031\001\018\001\255\255\255\255\021\001\022\001\255\255\024\001\
\025\001\026\001\027\001\028\001\029\001\030\001\031\001\018\001\
\255\255\255\255\021\001\022\001\255\255\024\001\025\001\026\001\
\027\001\028\001\029\001\030\001\031\001\255\255\255\255\255\255\
\255\255\255\255\018\001\003\001\004\001\021\001\022\001\255\255\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\018\001\255\255\255\255\021\001\022\001\255\255\024\001\025\001\
\026\001\027\001\028\001\029\001\030\001\031\001\018\001\255\255\
\018\001\021\001\022\001\021\001\022\001\018\001\255\255\255\255\
\021\001\022\001\030\001\031\001\030\001\031\001\255\255\255\255\
\255\255\255\255\031\001\067\000\068\000\069\000\070\000\071\000\
\072\000"

let yynames_const = "\
  INT\000\
  VOID\000\
  IF\000\
  ELSE\000\
  WHILE\000\
  BREAK\000\
  CONTINUE\000\
  RETURN\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  MOD\000\
  NOT\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  SEMI\000\
  COMMA\000\
  ASSIGN\000\
  EQ\000\
  NEQ\000\
  LT\000\
  GT\000\
  LEQ\000\
  GEQ\000\
  AND\000\
  OR\000\
  EOF\000\
  "

let yynames_block = "\
  NUMBER\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'func_def_list) in
    Obj.repr(
# 31 "bin/parser.mly"
                    ( _1 )
# 287 "bin/parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'func_def) in
    Obj.repr(
# 34 "bin/parser.mly"
                       ( [_1] )
# 294 "bin/parser.ml"
               : 'func_def_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'func_def_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'func_def) in
    Obj.repr(
# 35 "bin/parser.mly"
                         ( _1 @ [_2] )
# 302 "bin/parser.ml"
               : 'func_def_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'func_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'param_list_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 40 "bin/parser.mly"
    ( { ftype = _1; fname = _2; params = _4; body = _6 } )
# 312 "bin/parser.ml"
               : 'func_def))
; (fun __caml_parser_env ->
    Obj.repr(
# 43 "bin/parser.mly"
        ( Int )
# 318 "bin/parser.ml"
               : 'func_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "bin/parser.mly"
        ( Void )
# 324 "bin/parser.ml"
               : 'func_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "bin/parser.mly"
                       ( [] )
# 330 "bin/parser.ml"
               : 'param_list_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param_list) in
    Obj.repr(
# 49 "bin/parser.mly"
                       ( _1 )
# 337 "bin/parser.ml"
               : 'param_list_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param) in
    Obj.repr(
# 52 "bin/parser.mly"
                       ( [_1] )
# 344 "bin/parser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'param_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'param) in
    Obj.repr(
# 53 "bin/parser.mly"
                         ( _1 @ [_3] )
# 352 "bin/parser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 56 "bin/parser.mly"
                       ( { ptype = Int; pname = _2 } )
# 359 "bin/parser.ml"
               : 'param))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list_opt) in
    Obj.repr(
# 60 "bin/parser.mly"
                              ( _2 )
# 366 "bin/parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "bin/parser.mly"
                       ( [] )
# 372 "bin/parser.ml"
               : 'stmt_list_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 64 "bin/parser.mly"
                       ( _1 )
# 379 "bin/parser.ml"
               : 'stmt_list_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 67 "bin/parser.mly"
                       ( [_1] )
# 386 "bin/parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 68 "bin/parser.mly"
                       ( _1 @ [_2] )
# 394 "bin/parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 72 "bin/parser.mly"
                       ( Block _1 )
# 401 "bin/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "bin/parser.mly"
                       ( Empty )
# 407 "bin/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 74 "bin/parser.mly"
                       ( ExprStmt _1 )
# 414 "bin/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 75 "bin/parser.mly"
                       ( Assign (_1, _3) )
# 422 "bin/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 76 "bin/parser.mly"
                          ( Decl (_2, _4) )
# 430 "bin/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 78 "bin/parser.mly"
    ( If (_3, _5, None) )
# 438 "bin/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 80 "bin/parser.mly"
    ( If (_3, _5, Some _7) )
# 447 "bin/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 82 "bin/parser.mly"
    ( While (_3, _5) )
# 455 "bin/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "bin/parser.mly"
                       ( Break )
# 461 "bin/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "bin/parser.mly"
                       ( Continue )
# 467 "bin/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 85 "bin/parser.mly"
                       ( Return _2 )
# 474 "bin/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "bin/parser.mly"
                       ( None )
# 480 "bin/parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 89 "bin/parser.mly"
                       ( Some _1 )
# 487 "bin/parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'LOrExpr) in
    Obj.repr(
# 93 "bin/parser.mly"
                       ( _1 )
# 494 "bin/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'LAndExpr) in
    Obj.repr(
# 96 "bin/parser.mly"
                       ( _1 )
# 501 "bin/parser.ml"
               : 'LOrExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'LOrExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'LAndExpr) in
    Obj.repr(
# 97 "bin/parser.mly"
                       ( BinOp (_1, "||", _3) )
# 509 "bin/parser.ml"
               : 'LOrExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'RelExpr) in
    Obj.repr(
# 100 "bin/parser.mly"
                       ( _1 )
# 516 "bin/parser.ml"
               : 'LAndExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'LAndExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'RelExpr) in
    Obj.repr(
# 101 "bin/parser.mly"
                       ( BinOp (_1, "&&", _3) )
# 524 "bin/parser.ml"
               : 'LAndExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AddExpr) in
    Obj.repr(
# 104 "bin/parser.mly"
                       ( _1 )
# 531 "bin/parser.ml"
               : 'RelExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'RelExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'AddExpr) in
    Obj.repr(
# 105 "bin/parser.mly"
                       ( BinOp (_1, "==", _3) )
# 539 "bin/parser.ml"
               : 'RelExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'RelExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'AddExpr) in
    Obj.repr(
# 106 "bin/parser.mly"
                       ( BinOp (_1, "!=", _3) )
# 547 "bin/parser.ml"
               : 'RelExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'RelExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'AddExpr) in
    Obj.repr(
# 107 "bin/parser.mly"
                       ( BinOp (_1, "<", _3) )
# 555 "bin/parser.ml"
               : 'RelExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'RelExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'AddExpr) in
    Obj.repr(
# 108 "bin/parser.mly"
                       ( BinOp (_1, ">", _3) )
# 563 "bin/parser.ml"
               : 'RelExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'RelExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'AddExpr) in
    Obj.repr(
# 109 "bin/parser.mly"
                       ( BinOp (_1, "<=", _3) )
# 571 "bin/parser.ml"
               : 'RelExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'RelExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'AddExpr) in
    Obj.repr(
# 110 "bin/parser.mly"
                       ( BinOp (_1, ">=", _3) )
# 579 "bin/parser.ml"
               : 'RelExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'MulExpr) in
    Obj.repr(
# 113 "bin/parser.mly"
                       ( _1 )
# 586 "bin/parser.ml"
               : 'AddExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'AddExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'MulExpr) in
    Obj.repr(
# 114 "bin/parser.mly"
                       ( BinOp (_1, "+", _3) )
# 594 "bin/parser.ml"
               : 'AddExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'AddExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'MulExpr) in
    Obj.repr(
# 115 "bin/parser.mly"
                        ( BinOp (_1, "-", _3) )
# 602 "bin/parser.ml"
               : 'AddExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'UnaryExpr) in
    Obj.repr(
# 118 "bin/parser.mly"
                       ( _1 )
# 609 "bin/parser.ml"
               : 'MulExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'MulExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'UnaryExpr) in
    Obj.repr(
# 119 "bin/parser.mly"
                          ( BinOp (_1, "*", _3) )
# 617 "bin/parser.ml"
               : 'MulExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'MulExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'UnaryExpr) in
    Obj.repr(
# 120 "bin/parser.mly"
                           ( BinOp (_1, "/", _3) )
# 625 "bin/parser.ml"
               : 'MulExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'MulExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'UnaryExpr) in
    Obj.repr(
# 121 "bin/parser.mly"
                        ( BinOp (_1, "%", _3) )
# 633 "bin/parser.ml"
               : 'MulExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'PrimaryExpr) in
    Obj.repr(
# 124 "bin/parser.mly"
                       ( _1 )
# 640 "bin/parser.ml"
               : 'UnaryExpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'UnaryExpr) in
    Obj.repr(
# 125 "bin/parser.mly"
                               ( UnOp ("-", _2) )
# 647 "bin/parser.ml"
               : 'UnaryExpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'UnaryExpr) in
    Obj.repr(
# 126 "bin/parser.mly"
                             ( UnOp ("+", _2) )
# 654 "bin/parser.ml"
               : 'UnaryExpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'UnaryExpr) in
    Obj.repr(
# 127 "bin/parser.mly"
                       ( UnOp ("!", _2) )
# 661 "bin/parser.ml"
               : 'UnaryExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 130 "bin/parser.mly"
                       ( Var _1 )
# 668 "bin/parser.ml"
               : 'PrimaryExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 131 "bin/parser.mly"
                       ( Literal (IntLit _1) )
# 675 "bin/parser.ml"
               : 'PrimaryExpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 132 "bin/parser.mly"
                       ( Paren _2 )
# 682 "bin/parser.ml"
               : 'PrimaryExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'arg_list_opt) in
    Obj.repr(
# 133 "bin/parser.mly"
                                ( Call (_1, _3) )
# 690 "bin/parser.ml"
               : 'PrimaryExpr))
; (fun __caml_parser_env ->
    Obj.repr(
# 137 "bin/parser.mly"
                       ( [] )
# 696 "bin/parser.ml"
               : 'arg_list_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg_list) in
    Obj.repr(
# 138 "bin/parser.mly"
                       ( _1 )
# 703 "bin/parser.ml"
               : 'arg_list_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 141 "bin/parser.mly"
                       ( [_1] )
# 710 "bin/parser.ml"
               : 'arg_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 142 "bin/parser.mly"
                       ( _1 @ [_3] )
# 718 "bin/parser.ml"
               : 'arg_list))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
