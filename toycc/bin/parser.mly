%{
  open Ast
%}

%token <int>    INT
%token <string> ID

%token INT_KW VOID_KW IF ELSE WHILE BREAK CONTINUE RETURN
%token PLUS MINUS STAR SLASH PERCENT
%token EQ NE LT GT LE GE ASSIGN ANDAND OROR BANG
%token LPAREN RPAREN LBRACE RBRACE SEMI COMMA EOF

%type <Ast.program> program
%start program

/* 优先级和结合性声明 */
%nonassoc NO_ELSE
%nonassoc ELSE
%left OROR
%left ANDAND
%nonassoc LT GT LE GE EQ NE
%left PLUS MINUS
%left STAR SLASH PERCENT
%right UMINUS  /* 一元负号的单独优先级 */
%right BANG

%%

program:
  | function_list EOF                 { List.rev $1 }

function_list:
  | /* empty */                       { [] }
  | function_list func_def            { $2 :: $1 }

func_def:
  | INT_KW ID LPAREN params RPAREN block {
      { ret_type = `Int; name = $2; params = $4; body = $6 }
    }
  | VOID_KW ID LPAREN params RPAREN block {
      { ret_type = `Void; name = $2; params = $4; body = $6 }
    }

params:
  | /* empty */                       { [] }
  | param_list                        { List.rev $1 }

param_list:
  | INT_KW ID                         { [ $2 ] }
  | param_list COMMA INT_KW ID        { $4 :: $1 }

block:
  | LBRACE stmt_list RBRACE           { SBlock (List.rev $2) }

stmt_list:
  | /* empty */                       { [] }
  | stmt_list stmt                    { $2 :: $1 }

stmt:
  | expr SEMI                         { SExpr $1 }
  | INT_KW ID ASSIGN expr SEMI        { SDecl($2, $4) }
  | ID ASSIGN expr SEMI               { SAssign($1, $3) }
  | IF LPAREN expr RPAREN stmt %prec NO_ELSE { SIf($3, $5, None) }
  | IF LPAREN expr RPAREN stmt ELSE stmt { SIf($3, $5, Some $7) }
  | WHILE LPAREN expr RPAREN stmt     { SWhile($3, $5) }
  | BREAK SEMI                        { SBreak }
  | CONTINUE SEMI                     { SContinue }
  | RETURN expr_opt SEMI              { SReturn $2 }

expr_opt:
  | /* empty */                       { None }
  | expr                              { Some $1 }

expr:
  | INT                               { EInt $1 }
  | ID                                { EVar $1 }
  | expr PLUS expr                    { EBin(Add, $1, $3) }
  | expr MINUS expr                   { EBin(Sub, $1, $3) }
  | expr STAR expr                    { EBin(Mul, $1, $3) }
  | expr SLASH expr                   { EBin(Div, $1, $3) }
  | expr PERCENT expr                 { EBin(Mod, $1, $3) }
  | expr EQ expr                      { EBin(Eq, $1, $3) }
  | expr NE expr                      { EBin(Ne, $1, $3) }
  | expr LT expr                      { EBin(Lt, $1, $3) }
  | expr GT expr                      { EBin(Gt, $1, $3) }
  | expr LE expr                      { EBin(Le, $1, $3) }
  | expr GE expr                      { EBin(Ge, $1, $3) }
  | expr ANDAND expr                  { EBin(Land, $1, $3) }
  | expr OROR expr                    { EBin(Lor, $1, $3) }
  | BANG expr                         { EUn(`Not, $2) }
  | MINUS expr %prec UMINUS           { EUn(`Neg, $2) }  /* 一元负号 */
  | ID LPAREN args RPAREN             { ECall($1, $3) }
  | LPAREN expr RPAREN                { $2 }

args:
  | /* empty */                       { [] }
  | arg_list                          { List.rev $1 }

arg_list:
  | expr                              { [ $1 ] }
  | arg_list COMMA expr               { $3 :: $1 }