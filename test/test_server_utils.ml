open OUnit2
open Terminal_tok.Types
open Terminal_tok.Server_utils
open Terminal_tok
open Lwt

let client1 =
  {
    cnt_name = "bob";
    pub_key = Some "1231923014";
    cnt_addr = "127.0.0.1:5000";
    cnt_in = Lwt_io.stdin;
    cnt_out = Lwt_io.stdout;
    msg_addr = Some "127.0.0.1:5001";
    msg_in = Some Lwt_io.stdin;
    msg_out = Some Lwt_io.stdout;
  }

let client2 =
  {
    cnt_name = "sam";
    pub_key = Some "1231923014";
    cnt_addr = "127.0.0.1:5000";
    cnt_in = Lwt_io.stdin;
    cnt_out = Lwt_io.stdout;
    msg_addr = Some "127.0.0.1:5001";
    msg_in = Some Lwt_io.stdin;
    msg_out = Some Lwt_io.stdout;
  }

let client3 =
  {
    cnt_name = "tim";
    pub_key = Some "1231923014";
    cnt_addr = "127.0.0.1:5000";
    cnt_in = Lwt_io.stdin;
    cnt_out = Lwt_io.stdout;
    msg_addr = Some "127.0.0.1:5001";
    msg_in = Some Lwt_io.stdin;
    msg_out = Some Lwt_io.stdout;
  }

let test_string_of_addr _ =
  let sockaddr = Unix.ADDR_INET (Unix.inet_addr_loopback, 5000) in
  assert_equal ~printer:(fun s -> s) (string_of_addr sockaddr) "127.0.0.1:5000";
  let sockaddr = Unix.ADDR_UNIX "/socket/filepath/example" in
  assert_equal
    ~printer:(fun s -> s)
    (string_of_addr sockaddr) "/socket/filepath/example"

let test_format_clients _ =
  let client_lst = [ client1; client2; client3 ] in
  assert_equal
    ~printer:(fun s -> s)
    (format_clients client_lst ^ "")
    " tim sam bob"

let tests =
  "storage tests"
  >::: [
         "string of address" >:: test_string_of_addr;
         "format clients" >:: test_format_clients;
       ]

let _ = run_test_tt_main tests
