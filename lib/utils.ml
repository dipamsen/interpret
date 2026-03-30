let read_file filename =
  let ic = open_in filename in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  content

let string_of_token = function
	| Lexer.Ident s -> "Ident(" ^ s ^ ")"
	| Lexer.Number s -> "Number(" ^ s ^ ")"
	| Lexer.String s -> "String(" ^ s ^ ")"
	| Lexer.Plus -> "Plus"
	| Lexer.Minus -> "Minus"
	| Lexer.Star -> "Star"
	| Lexer.Slash -> "Slash"
	| Lexer.LParen -> "LParen"
	| Lexer.RParen -> "RParen"
	| Lexer.Assign -> "Assign"
	| Lexer.Equal -> "Equal"
	| Lexer.NotEqual -> "NotEqual"
	| Lexer.Greater -> "Greater"
	| Lexer.Less -> "Less"
	| Lexer.GreaterEq -> "GreaterEq"
	| Lexer.LessEq -> "LessEq"
	| Lexer.EOF -> "EOF"
