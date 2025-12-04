open OUnit2
open Terminal_tok

let test_encryption_functions _ =
  Encrypt.(
    let priv_key_A = generate_private_key () in

    (* print_endline ("\n PKA: " ^ Z.to_string priv_key_A); *)
    let pub_key_A = get_public_key priv_key_A in
    let msg = "bello!" in
    (* print_endline ("\n PubKA: " ^ Z.to_string pub_key_A); *)
    let priv_key_B = generate_private_key () in
    (* print_endline ("\n PKB: " ^ Z.to_string priv_key_B); *)
    let pub_key_B = get_public_key priv_key_B in
    (* print_endline ("\n PubKB: " ^ Z.to_string pub_key_B); *)

    let ss_a = get_shared_secret pub_key_B priv_key_A in
    let ss_b = get_shared_secret pub_key_A priv_key_B in

    (* print_endline (Z.to_string ss_a); print_endline (Z.to_string ss_b); *)
    assert_equal ss_a ss_b;

    let key = secret_to_key ss_a in

    let e_msg = encrypt_msg msg key in

    print_endline e_msg;

    assert_equal msg (decrypt_msg e_msg key))


let tests =
  "utils tests"
  >::: [
         "encryption functions" >:: test_encryption_functions;
       ]

let _ = run_test_tt_main tests
