(* 抽象语法树定义 *)

(* 类型定义 *)
type typ = Int | Void

(* 标识符 *)
type id = string

(* 字面量 *)
type literal = 
  | IntLit of int

(* 表达式 *)
type expr =
  | Literal of literal
  | Var of id
  | BinOp of expr * string * expr
  | UnOp of string * expr
  | Call of id * expr list
  | Paren of expr

(* 语句 *)
type stmt =
  | Block of stmt list
  | Empty
  | ExprStmt of expr
  | Assign of id * expr
  | Decl of id * expr
  | If of expr * stmt * stmt option
  | While of expr * stmt
  | Break
  | Continue
  | Return of expr option

(* 参数 *)
type param = { ptype: typ; pname: id }

(* 函数定义 *)
type func_def = {
  ftype: typ;
  fname: id;
  params: param list;
  body: stmt list;
}

(* 程序 *)
type program = func_def list  