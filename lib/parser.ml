
(* 

expr := equality
equality := comparison ( ( "!=" | "==" ) comparison )*
comparison := term ( ( ">" | ">=" | "<" | "<=" ) term )
term := factor ( ( "-" | "+" ) factor )*
factor := unary ( ( "/" | "*" ) unary )*
unary := ( ( "!" | "-" ) unary ) | primary
primary := NUMBER | STRING | "true" | "false" | "nil" | "(" expr ")"

*)



let rec parse_expr (tokens : Lexer.token list) : Ast.expr * Lexer.token list =
  parse_equality tokens 
and parse_equality (tokens : Lexer.token list) : Ast.expr * Lexer.token list =
  let (left, tokens) = parse_comparison tokens in
  let rec loop (left, tokens) = match tokens with
  | Lexer.Equal :: rest -> 
      let (right, tokens) = parse_comparison rest in
      loop (Ast.Binary (Ast.Eq, left, right), tokens)
  | Lexer.NotEqual :: rest ->
      let (right, tokens) = parse_comparison rest in
      loop (Ast.Binary (Ast.NotEq, left, right), tokens)
  | _ -> (left, tokens)
in loop (left, tokens) 
and parse_comparison (tokens : Lexer.token list) : Ast.expr * Lexer.token list =
  let (left, tokens) = parse_term tokens in
  match tokens with
  | Lexer.Greater :: rest ->
      let (right, tokens) = parse_term rest in
      (Ast.Binary (Ast.Greater, left, right), tokens)
  | Lexer.GreaterEq :: rest ->
      let (right, tokens) = parse_term rest in
      (Ast.Binary (Ast.GreaterEq, left, right), tokens)
  | Lexer.Less :: rest ->
      let (right, tokens) = parse_term rest in
      (Ast.Binary (Ast.Less, left, right), tokens)
  | Lexer.LessEq :: rest ->
      let (right, tokens) = parse_term rest in
      (Ast.Binary (Ast.LessEq, left, right), tokens)
  | _ -> (left, tokens)
and parse_term (tokens : Lexer.token list) : Ast.expr * Lexer.token list =
  let (left, tokens) = parse_factor tokens in
  let rec loop (left, tokens) = match tokens with
  | Lexer.Plus :: rest ->
      let (right, tokens) = parse_factor rest in
      loop (Ast.Binary (Ast.Add, left, right), tokens)
  | Lexer.Minus :: rest ->
      let (right, tokens) = parse_factor rest in
      loop (Ast.Binary (Ast.Sub, left, right), tokens)
  | _ -> (left, tokens)
in loop (left, tokens) 
and parse_factor (tokens : Lexer.token list) : Ast.expr * Lexer.token list =
  let (left, tokens) = parse_unary tokens in
  let rec loop (left, tokens) = match tokens with
  | Lexer.Slash :: rest ->
      let (right, tokens) = parse_unary rest in
      loop (Ast.Binary (Ast.Div, left, right), tokens)
  | Lexer.Star :: rest ->
      let (right, tokens) = parse_unary rest in
      loop (Ast.Binary (Ast.Mul, left, right), tokens)
  | _ -> (left, tokens)
in loop (left, tokens)
and parse_unary (tokens : Lexer.token list) : Ast.expr * Lexer.token list =
  match tokens with
  | Lexer.Minus :: rest ->
      let (expr, tokens) = parse_unary rest in
      (Ast.Unary (Ast.Neg, expr), tokens)
  | _ -> parse_primary tokens
and parse_primary (tokens : Lexer.token list) : Ast.expr * Lexer.token list =
  match tokens with
  | Lexer.Number s :: rest -> (Ast.Literal (Ast.Number (float_of_string s)), rest)
  | Lexer.String s :: rest -> (Ast.Literal (Ast.String s), rest)
  | Lexer.Ident "true" :: rest -> (Ast.Literal (Ast.Boolean true), rest)
  | Lexer.Ident "false" :: rest -> (Ast.Literal (Ast.Boolean false), rest)
  | Lexer.Ident id :: _ -> failwith ("Unexpected identifier: " ^ id)
  | Lexer.LParen :: rest ->
      let (expr, tokens) = parse_expr rest in
      (match tokens with
      | Lexer.RParen :: rest -> (expr, rest)
      | _ -> failwith "Expected ')'")
  | _ -> failwith "Expected expression"

let parse (input : Lexer.token list) : Ast.expr =
  let (expr, tokens) = parse_expr input in
  match tokens with
  | [EOF] -> expr
  | _ -> failwith "Unexpected tokens"