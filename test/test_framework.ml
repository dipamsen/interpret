open OUnit2

let suites : test list ref = ref []

let register_suite (t : test) =
  suites := t :: !suites

let run_all () =
  let all = "all" >::: List.rev !suites in
  run_test_tt_main all
