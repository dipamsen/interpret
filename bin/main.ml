open Taulang

let chars_of_string s = List.init (String.length s) (String.get s)
let run code =
  let tokens = Taulang.Lexer.lex (chars_of_string code) in
  let prog = Parser.parse tokens in
  let result = Evaluate.evaluate prog in
  Evaluate.print_value result

let repl () =
  let rec loop () =
    print_string "> ";
    flush stdout;
    try
      let code = read_line () in
      (try run code
       with 
      | Failure msg ->
        Printf.eprintf "Error: %s\n" msg;
        flush stderr       
      | e -> Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string e));
      loop ()
    with
    | End_of_file -> print_endline "Bye!"
  in
  loop ()

let () =
  if Array.length Sys.argv < 2 then repl ()
  else
    let filename = Sys.argv.(1) in
    let code = Utils.read_file filename in
    run code
