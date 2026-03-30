type value = 
  | VNumber of float
  | VString of string
  | VBoolean of bool
  | VNil

let rec evaluate = function
  | Ast.Literal l -> evaluate_literal l
  | Ast.Unary (op, expr) -> evaluate_unary op (evaluate expr)
  | Ast.Binary (op, left, right) -> evaluate_binary op (evaluate left) (evaluate right)
  | Ast.Group expr -> evaluate expr
and evaluate_literal = function
  | Number n -> VNumber n
  | String s -> VString s
  | Boolean b -> VBoolean b
  | Nil -> VNil
and evaluate_unary op value = 
  match op with
  | Ast.Neg -> match value with
    | VNumber n -> VNumber (-.n)
    | _ -> failwith "Cannot negate non-number"
and evaluate_binary op left right = 
  match (left, right) with
  | (VNumber l, VNumber r) -> (match op with
    | Ast.Add -> VNumber (l +. r)
    | Ast.Sub -> VNumber (l -. r)
    | Ast.Mul -> VNumber (l *. r)
    | Ast.Div -> VNumber (l /. r)
    | Ast.Greater -> VBoolean (l > r)
    | Ast.Less -> VBoolean (l < r)
    | Ast.GreaterEq -> VBoolean (l >= r)
    | Ast.LessEq -> VBoolean (l <= r)
    | Ast.Eq -> VBoolean (l = r)
    | Ast.NotEq -> VBoolean (l <> r))
  | (VString l, VString r) -> (match op with
    | Ast.Add -> VString (l ^ r)
    | Ast.Sub -> failwith "Cannot subtract strings"
    | Ast.Mul -> failwith "Cannot multiply strings"
    | Ast.Div -> failwith "Cannot divide strings"
    | Ast.Greater -> VBoolean (l > r)
    | Ast.Less -> VBoolean (l < r)
    | Ast.GreaterEq -> VBoolean (l >= r)
    | Ast.LessEq -> VBoolean (l <= r)
    | Ast.Eq -> VBoolean (l = r)
    | Ast.NotEq -> VBoolean (l <> r))
  | (VBoolean l, VBoolean r) -> (match op with
    | Ast.Add -> VBoolean (l || r)
    | Ast.Sub -> failwith "Cannot subtract booleans"
    | Ast.Mul -> failwith "Cannot multiply booleans"
    | Ast.Div -> failwith "Cannot divide booleans"
    | Ast.Greater -> VBoolean (l > r)
    | Ast.Less -> VBoolean (l < r)
    | Ast.GreaterEq -> VBoolean (l >= r)
    | Ast.LessEq -> VBoolean (l <= r)
    | Ast.Eq -> VBoolean (l = r)
    | Ast.NotEq -> VBoolean (l <> r))
  | _ -> failwith "Cannot apply operator"