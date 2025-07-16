type binop = Add | Sub | Mul | Div | Mod
           | Lt | Gt | Le | Ge | Eq | Ne
           | Land | Lor

and expr =
  | EInt of int
  | EVar of string
  | EBin of binop * expr * expr
  | EUn  of [ `Neg | `Not ] * expr
  | ECall of string * expr list

and stmt =
  | SBlock   of stmt list
  | SExpr    of expr
  | SDecl    of string * expr
  | SAssign  of string * expr
  | SIf      of expr * stmt * stmt option
  | SWhile   of expr * stmt
  | SBreak
  | SContinue
  | SReturn  of expr option

and func = {
  ret_type : [ `Int | `Void ];
  name     : string;
  params   : string list;
  body     : stmt;
}

and program = func list