

type literal = 
  | Number of float
  | String of string
  | Boolean of bool
  | Nil


type unary_operator = Neg

type binary_operator = Add | Sub | Mul | Div | Greater | Less | GreaterEq | LessEq | Eq | NotEq

type expr = 
  | Literal of literal
  | Unary of unary_operator * expr
  | Binary of binary_operator * expr * expr
  | Group of expr

let rec string_of_ast = function
  | Literal l -> "Literal(" ^ string_of_literal l ^ ")"
  | Unary (op, expr) -> "Unary(" ^ string_of_unary_operator op ^ ", " ^ string_of_expr expr ^ ")"
  | Binary (op, left, right) -> "Binary(" ^ string_of_binary_operator op ^ ", " ^ string_of_expr left ^ ", " ^ string_of_expr right ^ ")"
  | Group expr -> "Group(" ^ string_of_expr expr ^ ")"

and string_of_literal = function
  | Number n -> "Number(" ^ string_of_float n ^ ")"
  | String s -> "String(" ^ s ^ ")"
  | Boolean b -> "Boolean(" ^ string_of_bool b ^ ")"
  | Nil -> "Nil"

and string_of_unary_operator = function
  | Neg -> "Neg"

and string_of_binary_operator = function
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | Greater -> "Greater"
  | Less -> "Less"
  | GreaterEq -> "GreaterEq"
  | LessEq -> "LessEq"
  | Eq -> "Eq"
  | NotEq -> "NotEq"

and string_of_expr = function
  | Literal l -> string_of_literal l
  | Unary (op, expr) -> string_of_unary_operator op ^ "(" ^ string_of_expr expr ^ ")"
  | Binary (op, left, right) -> string_of_binary_operator op ^ "(" ^ string_of_expr left ^ ", " ^ string_of_expr right ^ ")"  
  | Group expr -> "(" ^ string_of_expr expr ^ ")"