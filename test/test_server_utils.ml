open OUnit2
open Terminal_tok.Types
open Terminal_tok.Server_utils
open Terminal_tok

let test_write_clients_to_all _ = ()
(* let sock = Unix.ADDR_INET 3000 ) *)

let test_string_of_addr _ = ()
let test_format_clients _ = ()

let tests =
  "storage tests"
  >::: [
         "string of address" >:: test_string_of_addr;
         "format clients" >:: test_format_clients;
         "write_clients_to_all" >:: test_write_clients_to_all;
       ]

let _ = run_test_tt_main tests
