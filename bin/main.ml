open Taulang

let chars_of_string s = List.init (String.length s) (String.get s)

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Usage: %s <file>\n" Sys.argv.(0);
    exit 1
  end;

  let filename = Sys.argv.(1) in
  let code = Utils.read_file filename in
  let tokens = Taulang.Lexer.lex (chars_of_string code) in
  let prog = Parser.parse tokens in
  print_endline (Ast.string_of_ast prog)