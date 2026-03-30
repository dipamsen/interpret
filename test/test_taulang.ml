open OUnit2
open Test_helpers
open Test_framework

let lexer_suite =
	"lexer" >::: [
		"assign" >:: (fun ctxt -> assert_tokens ~ctxt ":=" [Taulang.Lexer.Assign; Taulang.Lexer.EOF]);
		"equal" >:: (fun ctxt -> assert_tokens ~ctxt "=" [Taulang.Lexer.Equal; Taulang.Lexer.EOF]);
		"sequence" >:: (fun ctxt -> assert_tokens ~ctxt "foo 123 \"bar\"" [Taulang.Lexer.Ident "foo"; Taulang.Lexer.Number "123"; Taulang.Lexer.String "bar"; Taulang.Lexer.EOF]);
		"assign_expr" >:: (fun ctxt -> assert_tokens ~ctxt "a:=1" [Taulang.Lexer.Ident "a"; Taulang.Lexer.Assign; Taulang.Lexer.Number "1"; Taulang.Lexer.EOF]);
	]

let () =
	register_suite lexer_suite;
	run_all ()
