type token =
  | Ident of string
  | Number of string
  | String of string
  | Plus
  | Minus
  | Star
  | Slash
  | LParen
  | RParen
  | Greater
  | Less
  | GreaterEq
  | LessEq
  | Assign
  | Equal
  | NotEqual
  | EOF

  let lex (input : char list) : token list =
    let is_digit = function '0' .. '9' -> true | _ -> false in
    let is_alpha = function
      | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
      | _ -> false
    in
    let is_alphanum c = is_alpha c || is_digit c in

    let rec collect_string acc = function
      | [] -> failwith "Unterminated string literal"
      | '"' :: rest -> (acc, rest)
      | c :: rest -> collect_string (acc ^ String.make 1 c) rest
    in

    let rec collect_digits acc = function
      | d :: ds when is_digit d -> collect_digits (acc ^ String.make 1 d) ds
      | other -> (acc, other)
    in

    let rec collect_ident acc = function
      | d :: ds when is_alphanum d -> collect_ident (acc ^ String.make 1 d) ds
      | other -> (acc, other)
    in

    let rec scan = function
      | [] -> [EOF]
      | (' ' | '\n' | '\t' | '\r') :: rest -> scan rest
      | '+' :: rest -> Plus :: scan rest
      | '-' :: rest -> Minus :: scan rest
      | '*' :: rest -> Star :: scan rest
      | '/' :: rest -> Slash :: scan rest
      | '(' :: rest -> LParen :: scan rest
      | ')' :: rest -> RParen :: scan rest
      | ':' :: ('=' :: rest) -> Assign :: scan rest
      | '=' :: rest -> Equal :: scan rest
      | '!' :: ('=' :: rest) -> NotEqual :: scan rest
      | '"' :: rest ->
        let (s, rest_after) = collect_string "" rest in
        String s :: scan rest_after
      | c :: rest when is_digit c ->
        let (num, rest_after) = collect_digits (String.make 1 c) rest in
        Number num :: scan rest_after
      | c :: rest when is_alpha c ->
        let (id, rest_after) = collect_ident (String.make 1 c) rest in
        Ident id :: scan rest_after
      | c :: _ -> failwith (Printf.sprintf "Unexpected character: '%c'" c)

    in
    scan input