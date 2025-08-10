%{
open Ast
%}

%token <int> NUMBER
%token <string> ID
%token INT VOID IF ELSE WHILE BREAK CONTINUE RETURN
%token PLUS MINUS TIMES DIVIDE MOD NOT
%token LPAREN RPAREN LBRACE RBRACE SEMI COMMA
%token ASSIGN EQ NEQ LT GT LEQ GEQ AND OR
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NOT UMINUS UPLUS

%start program
%type <Ast.program> program

%%

/* 编译单元：多个函数定义（按出现顺序收集） */
program:
  func_def_list EOF { $1 }

func_def_list:
  func_def             { [$1] }
| func_def_list func_def { $1 @ [$2] }  /* 修正：顺序追加 */

/* 函数定义 */
func_def:
  func_type ID LPAREN param_list_opt RPAREN block
    { { ftype = $1; fname = $2; params = $4; body = $6 } }

func_type:
  INT   { Int }
| VOID  { Void }

/* 参数列表（按出现顺序收集） */
param_list_opt:
  /* empty */          { [] }
| param_list           { $1 }

param_list:
  param                { [$1] }
| param_list COMMA param { $1 @ [$3] }  /* 修正：顺序追加 */

param:
  INT ID               { { ptype = Int; pname = $2 } }

/* 语句块（按出现顺序收集语句） */
block:
  LBRACE stmt_list_opt RBRACE { $2 }  /* 修正：直接使用原始顺序列表 */

stmt_list_opt:
  /* empty */          { [] }
| stmt_list            { $1 }

stmt_list:
  stmt                 { [$1] }
| stmt_list stmt       { $1 @ [$2] }  /* 修正：顺序追加 */

/* 语句 */
stmt:
  block                { Block $1 }
| SEMI                 { Empty }
| expr SEMI            { ExprStmt $1 }
| ID ASSIGN expr SEMI  { Assign ($1, $3) }
| INT ID ASSIGN expr SEMI { Decl ($2, $4) }
| IF LPAREN expr RPAREN stmt %prec NOELSE
    { If ($3, $5, None) }
| IF LPAREN expr RPAREN stmt ELSE stmt
    { If ($3, $5, Some $7) }
| WHILE LPAREN expr RPAREN stmt
    { While ($3, $5) }
| BREAK SEMI           { Break }
| CONTINUE SEMI        { Continue }
| RETURN expr_opt SEMI { Return $2 }

expr_opt:
  /* empty */          { None }
| expr                 { Some $1 }

/* 表达式 */
expr:
  LOrExpr              { $1 }

LOrExpr:
  LAndExpr             { $1 }
| LOrExpr OR LAndExpr  { BinOp ($1, "||", $3) }

LAndExpr:
  RelExpr              { $1 }
| LAndExpr AND RelExpr { BinOp ($1, "&&", $3) }

RelExpr:
  AddExpr              { $1 }
| RelExpr EQ AddExpr   { BinOp ($1, "==", $3) }
| RelExpr NEQ AddExpr  { BinOp ($1, "!=", $3) }
| RelExpr LT AddExpr   { BinOp ($1, "<", $3) }
| RelExpr GT AddExpr   { BinOp ($1, ">", $3) }
| RelExpr LEQ AddExpr  { BinOp ($1, "<=", $3) }
| RelExpr GEQ AddExpr  { BinOp ($1, ">=", $3) }

AddExpr:
  MulExpr              { $1 }
| AddExpr PLUS MulExpr { BinOp ($1, "+", $3) }
| AddExpr MINUS MulExpr { BinOp ($1, "-", $3) }

MulExpr:
  UnaryExpr            { $1 }
| MulExpr TIMES UnaryExpr { BinOp ($1, "*", $3) }
| MulExpr DIVIDE UnaryExpr { BinOp ($1, "/", $3) }
| MulExpr MOD UnaryExpr { BinOp ($1, "%", $3) }

UnaryExpr:
  PrimaryExpr          { $1 }
| MINUS UnaryExpr %prec UMINUS { UnOp ("-", $2) }
| PLUS UnaryExpr %prec UPLUS { UnOp ("+", $2) }
| NOT UnaryExpr        { UnOp ("!", $2) }

PrimaryExpr:
  ID                   { Var $1 }
| NUMBER               { Literal (IntLit $1) }
| LPAREN expr RPAREN   { Paren $2 }
| ID LPAREN arg_list_opt RPAREN { Call ($1, $3) }

/* 函数调用参数（按出现顺序收集） */
arg_list_opt:
  /* empty */          { [] }
| arg_list             { $1 }

arg_list:
  expr                 { [$1] }
| arg_list COMMA expr  { $1 @ [$3] }  /* 修正：顺序追加 */