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

open Parsing;;
let _ = parse_error;;
# 2 "lib/parser.mly"
  open Ast
# 40 "lib/parser.ml"
let yytransl_const = [|
  259 (* INT_KW *);
  260 (* VOID_KW *);
  261 (* IF *);
  262 (* ELSE *);
  263 (* WHILE *);
  264 (* BREAK *);
  265 (* CONTINUE *);
  266 (* RETURN *);
  267 (* PLUS *);
  268 (* MINUS *);
  269 (* STAR *);
  270 (* SLASH *);
  271 (* PERCENT *);
  272 (* EQ *);
  273 (* NE *);
  274 (* LT *);
  275 (* GT *);
  276 (* LE *);
  277 (* GE *);
  278 (* ASSIGN *);
  279 (* ANDAND *);
  280 (* OROR *);
  281 (* BANG *);
  282 (* LPAREN *);
  283 (* RPAREN *);
  284 (* LBRACE *);
  285 (* RBRACE *);
  286 (* SEMI *);
  287 (* COMMA *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\004\000\004\000\006\000\
\006\000\005\000\007\000\007\000\008\000\008\000\008\000\008\000\
\008\000\008\000\008\000\008\000\008\000\010\000\010\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\011\000\011\000\012\000\012\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\006\000\006\000\000\000\001\000\002\000\
\004\000\003\000\000\000\002\000\002\000\005\000\004\000\005\000\
\007\000\005\000\002\000\002\000\003\000\000\000\001\000\001\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\002\000\002\000\
\004\000\003\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\047\000\000\000\000\000\000\000\001\000\003\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\008\000\000\000\000\000\000\000\011\000\004\000\000\000\005\000\
\000\000\009\000\024\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\010\000\012\000\000\000\
\000\000\000\000\000\000\000\000\000\000\019\000\020\000\000\000\
\000\000\000\000\040\000\039\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\013\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\021\000\042\000\000\000\000\000\028\000\029\000\
\030\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\015\000\041\000\000\000\000\000\000\000\000\000\000\000\
\014\000\000\000\018\000\000\000\017\000"

let yydgoto = "\002\000\
\003\000\004\000\008\000\014\000\022\000\015\000\025\000\039\000\
\040\000\050\000\070\000\071\000"

let yysindex = "\255\255\
\000\000\000\000\000\000\001\000\004\255\019\255\000\000\000\000\
\009\255\010\255\039\255\039\255\045\255\037\255\032\255\042\255\
\000\000\043\255\073\255\043\255\000\000\000\000\079\255\000\000\
\036\255\000\000\000\000\066\255\081\255\067\255\092\255\069\255\
\070\255\094\255\094\255\094\255\094\255\000\000\000\000\165\255\
\094\255\094\255\090\255\094\255\094\255\000\000\000\000\109\255\
\046\000\112\255\000\000\000\000\251\255\094\255\094\255\094\255\
\094\255\094\255\094\255\094\255\094\255\094\255\094\255\094\255\
\094\255\094\255\000\000\185\255\046\000\125\255\128\255\094\255\
\012\000\029\000\000\000\000\000\177\255\177\255\000\000\000\000\
\000\000\005\255\005\255\005\255\005\255\005\255\005\255\038\255\
\060\000\000\000\000\000\094\255\205\255\077\255\077\255\046\000\
\000\000\164\255\000\000\077\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\160\255\160\255\000\000\000\000\166\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\225\255\000\000\000\000\000\000\000\000\
\000\000\180\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\167\255\000\000\000\000\000\000\000\000\000\000\110\255\
\181\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\105\255\000\000\186\255\000\000\
\000\000\000\000\000\000\000\000\127\255\144\255\000\000\000\000\
\000\000\240\254\074\255\084\255\086\255\227\255\061\000\063\000\
\142\255\000\000\000\000\000\000\000\000\000\000\000\000\122\255\
\000\000\065\255\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\195\000\192\000\000\000\000\000\001\001\
\224\255\000\000\000\000\000\000"

let yytablesize = 357
let yytable = "\001\000\
\007\000\049\000\051\000\052\000\053\000\009\000\031\000\031\000\
\068\000\069\000\031\000\073\000\074\000\031\000\031\000\054\000\
\055\000\056\000\057\000\058\000\010\000\077\000\078\000\079\000\
\080\000\081\000\082\000\083\000\084\000\085\000\086\000\087\000\
\088\000\089\000\011\000\012\000\027\000\028\000\029\000\093\000\
\030\000\013\000\031\000\032\000\033\000\034\000\017\000\035\000\
\054\000\055\000\056\000\057\000\058\000\059\000\060\000\061\000\
\062\000\063\000\064\000\096\000\036\000\037\000\019\000\018\000\
\038\000\016\000\016\000\016\000\020\000\016\000\021\000\016\000\
\016\000\016\000\016\000\023\000\016\000\027\000\028\000\029\000\
\026\000\030\000\043\000\031\000\032\000\033\000\034\000\041\000\
\035\000\016\000\016\000\042\000\044\000\016\000\027\000\048\000\
\032\000\032\000\046\000\047\000\032\000\036\000\037\000\032\000\
\032\000\035\000\033\000\033\000\034\000\034\000\033\000\072\000\
\034\000\033\000\033\000\034\000\034\000\045\000\036\000\037\000\
\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
\025\000\025\000\025\000\045\000\025\000\025\000\042\000\045\000\
\025\000\026\000\026\000\025\000\025\000\075\000\026\000\026\000\
\026\000\026\000\026\000\026\000\046\000\026\000\026\000\091\000\
\046\000\026\000\027\000\027\000\026\000\026\000\092\000\027\000\
\027\000\027\000\027\000\027\000\027\000\038\000\027\000\027\000\
\038\000\100\000\027\000\038\000\038\000\027\000\027\000\054\000\
\055\000\056\000\057\000\058\000\059\000\060\000\061\000\062\000\
\063\000\064\000\006\000\065\000\066\000\056\000\057\000\058\000\
\007\000\043\000\067\000\054\000\055\000\056\000\057\000\058\000\
\059\000\060\000\061\000\062\000\063\000\064\000\016\000\065\000\
\066\000\022\000\023\000\024\000\044\000\000\000\090\000\054\000\
\055\000\056\000\057\000\058\000\059\000\060\000\061\000\062\000\
\063\000\064\000\000\000\065\000\066\000\000\000\000\000\000\000\
\000\000\000\000\097\000\025\000\025\000\025\000\025\000\025\000\
\025\000\025\000\025\000\025\000\025\000\025\000\000\000\025\000\
\025\000\035\000\035\000\000\000\000\000\035\000\025\000\000\000\
\035\000\035\000\000\000\005\000\006\000\054\000\055\000\056\000\
\057\000\058\000\059\000\060\000\061\000\062\000\063\000\064\000\
\000\000\065\000\066\000\000\000\000\000\076\000\054\000\055\000\
\056\000\057\000\058\000\059\000\060\000\061\000\062\000\063\000\
\064\000\000\000\065\000\066\000\000\000\000\000\094\000\054\000\
\055\000\056\000\057\000\058\000\059\000\060\000\061\000\062\000\
\063\000\064\000\000\000\065\000\066\000\000\000\000\000\095\000\
\054\000\055\000\056\000\057\000\058\000\059\000\060\000\061\000\
\062\000\063\000\064\000\000\000\065\000\066\000\054\000\055\000\
\056\000\057\000\058\000\059\000\060\000\061\000\062\000\063\000\
\064\000\000\000\065\000\036\000\036\000\037\000\037\000\036\000\
\000\000\037\000\036\000\036\000\037\000\037\000\098\000\099\000\
\000\000\000\000\000\000\000\000\101\000"

let yycheck = "\001\000\
\000\000\034\000\035\000\036\000\037\000\002\001\023\001\024\001\
\041\000\042\000\027\001\044\000\045\000\030\001\031\001\011\001\
\012\001\013\001\014\001\015\001\002\001\054\000\055\000\056\000\
\057\000\058\000\059\000\060\000\061\000\062\000\063\000\064\000\
\065\000\066\000\026\001\026\001\001\001\002\001\003\001\072\000\
\005\001\003\001\007\001\008\001\009\001\010\001\002\001\012\001\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\092\000\025\001\026\001\031\001\027\001\
\029\001\001\001\002\001\003\001\027\001\005\001\028\001\007\001\
\008\001\009\001\010\001\003\001\012\001\001\001\002\001\003\001\
\002\001\005\001\002\001\007\001\008\001\009\001\010\001\022\001\
\012\001\025\001\026\001\026\001\026\001\029\001\001\001\002\001\
\023\001\024\001\030\001\030\001\027\001\025\001\026\001\030\001\
\031\001\012\001\023\001\024\001\023\001\024\001\027\001\022\001\
\027\001\030\001\031\001\030\001\031\001\026\001\025\001\026\001\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\027\001\023\001\024\001\026\001\031\001\
\027\001\011\001\012\001\030\001\031\001\030\001\016\001\017\001\
\018\001\019\001\020\001\021\001\027\001\023\001\024\001\027\001\
\031\001\027\001\011\001\012\001\030\001\031\001\031\001\016\001\
\017\001\018\001\019\001\020\001\021\001\024\001\023\001\024\001\
\027\001\006\001\027\001\030\001\031\001\030\001\031\001\011\001\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\027\001\023\001\024\001\013\001\014\001\015\001\
\027\001\027\001\030\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\012\000\023\001\
\024\001\030\001\030\001\020\000\027\001\255\255\030\001\011\001\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\255\255\023\001\024\001\255\255\255\255\255\255\
\255\255\255\255\030\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\255\255\023\001\
\024\001\023\001\024\001\255\255\255\255\027\001\030\001\255\255\
\030\001\031\001\255\255\003\001\004\001\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\255\255\023\001\024\001\255\255\255\255\027\001\011\001\012\001\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\255\255\023\001\024\001\255\255\255\255\027\001\011\001\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\255\255\023\001\024\001\255\255\255\255\027\001\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\255\255\023\001\024\001\011\001\012\001\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\255\255\023\001\023\001\024\001\023\001\024\001\027\001\
\255\255\027\001\030\001\031\001\030\001\031\001\094\000\095\000\
\255\255\255\255\255\255\255\255\100\000"

let yynames_const = "\
  INT_KW\000\
  VOID_KW\000\
  IF\000\
  ELSE\000\
  WHILE\000\
  BREAK\000\
  CONTINUE\000\
  RETURN\000\
  PLUS\000\
  MINUS\000\
  STAR\000\
  SLASH\000\
  PERCENT\000\
  EQ\000\
  NE\000\
  LT\000\
  GT\000\
  LE\000\
  GE\000\
  ASSIGN\000\
  ANDAND\000\
  OROR\000\
  BANG\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  SEMI\000\
  COMMA\000\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'function_list) in
    Obj.repr(
# 30 "lib/parser.mly"
                                      ( List.rev _1 )
# 288 "lib/parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 33 "lib/parser.mly"
                                      ( [] )
# 294 "lib/parser.ml"
               : 'function_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'function_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'func_def) in
    Obj.repr(
# 34 "lib/parser.mly"
                                      ( _2 :: _1 )
# 302 "lib/parser.ml"
               : 'function_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'params) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 37 "lib/parser.mly"
                                         (
      { ret_type = `Int; name = _2; params = _4; body = _6 }
    )
# 313 "lib/parser.ml"
               : 'func_def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'params) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 40 "lib/parser.mly"
                                          (
      { ret_type = `Void; name = _2; params = _4; body = _6 }
    )
# 324 "lib/parser.ml"
               : 'func_def))
; (fun __caml_parser_env ->
    Obj.repr(
# 45 "lib/parser.mly"
                                      ( [] )
# 330 "lib/parser.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param_list) in
    Obj.repr(
# 46 "lib/parser.mly"
                                      ( List.rev _1 )
# 337 "lib/parser.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 49 "lib/parser.mly"
                                      ( [ _2 ] )
# 344 "lib/parser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'param_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 50 "lib/parser.mly"
                                      ( _4 :: _1 )
# 352 "lib/parser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 53 "lib/parser.mly"
                                      ( SBlock (List.rev _2) )
# 359 "lib/parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    Obj.repr(
# 56 "lib/parser.mly"
                                      ( [] )
# 365 "lib/parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 57 "lib/parser.mly"
                                      ( _2 :: _1 )
# 373 "lib/parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 60 "lib/parser.mly"
                                      ( SExpr _1 )
# 380 "lib/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 61 "lib/parser.mly"
                                      ( SDecl(_2, _4) )
# 388 "lib/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 62 "lib/parser.mly"
                                      ( SAssign(_1, _3) )
# 396 "lib/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 63 "lib/parser.mly"
                                             ( SIf(_3, _5, None) )
# 404 "lib/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 64 "lib/parser.mly"
                                         ( SIf(_3, _5, Some _7) )
# 413 "lib/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 65 "lib/parser.mly"
                                      ( SWhile(_3, _5) )
# 421 "lib/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 66 "lib/parser.mly"
                                      ( SBreak )
# 427 "lib/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 67 "lib/parser.mly"
                                      ( SContinue )
# 433 "lib/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 68 "lib/parser.mly"
                                      ( SReturn _2 )
# 440 "lib/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "lib/parser.mly"
                                      ( None )
# 446 "lib/parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 72 "lib/parser.mly"
                                      ( Some _1 )
# 453 "lib/parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 75 "lib/parser.mly"
                                      ( EInt _1 )
# 460 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 76 "lib/parser.mly"
                                      ( EVar _1 )
# 467 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 77 "lib/parser.mly"
                                      ( EBin(Add, _1, _3) )
# 475 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 78 "lib/parser.mly"
                                      ( EBin(Sub, _1, _3) )
# 483 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 79 "lib/parser.mly"
                                      ( EBin(Mul, _1, _3) )
# 491 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 80 "lib/parser.mly"
                                      ( EBin(Div, _1, _3) )
# 499 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 81 "lib/parser.mly"
                                      ( EBin(Mod, _1, _3) )
# 507 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 82 "lib/parser.mly"
                                      ( EBin(Eq, _1, _3) )
# 515 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 83 "lib/parser.mly"
                                      ( EBin(Ne, _1, _3) )
# 523 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 84 "lib/parser.mly"
                                      ( EBin(Lt, _1, _3) )
# 531 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 85 "lib/parser.mly"
                                      ( EBin(Gt, _1, _3) )
# 539 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 86 "lib/parser.mly"
                                      ( EBin(Le, _1, _3) )
# 547 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 87 "lib/parser.mly"
                                      ( EBin(Ge, _1, _3) )
# 555 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 88 "lib/parser.mly"
                                      ( EBin(Land, _1, _3) )
# 563 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 89 "lib/parser.mly"
                                      ( EBin(Lor, _1, _3) )
# 571 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 90 "lib/parser.mly"
                                      ( EUn(`Not, _2) )
# 578 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 91 "lib/parser.mly"
                                      ( EUn(`Neg, _2) )
# 585 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args) in
    Obj.repr(
# 92 "lib/parser.mly"
                                      ( ECall(_1, _3) )
# 593 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 93 "lib/parser.mly"
                                      ( _2 )
# 600 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "lib/parser.mly"
                                      ( [] )
# 606 "lib/parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg_list) in
    Obj.repr(
# 97 "lib/parser.mly"
                                      ( List.rev _1 )
# 613 "lib/parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 100 "lib/parser.mly"
                                      ( [ _1 ] )
# 620 "lib/parser.ml"
               : 'arg_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "lib/parser.mly"
                                      ( _3 :: _1 )
# 628 "lib/parser.ml"
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
