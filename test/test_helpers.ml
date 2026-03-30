open OUnit2
open Taulang.Utils

let chars_of_string s = List.init (String.length s) (String.get s)

let lex_str s = Taulang.Lexer.lex (chars_of_string s)

let string_of_tokens ts = String.concat ", " (List.map string_of_token ts)

let assert_tokens ?(ctxt: OUnit2.test_ctxt option) input expected =
  let got = lex_str input in
  match ctxt with
  | None -> assert_equal ~printer:string_of_tokens expected got
  | Some c -> assert_equal ~ctxt:c ~printer:string_of_tokens expected got

let assert_raises_lex exc input =
  assert_raises exc (fun () -> ignore (lex_str input))
