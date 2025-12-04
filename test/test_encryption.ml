open OUnit2
open Terminal_tok

let test_encryption_functions _ =
  Encrypt.(
    let priv_key_A = generate_private_key () in

    (* print_endline ("\n PKA: " ^ Z.to_string priv_key_A); *)
    let pub_key_A = get_public_key priv_key_A in
    let msg = "bello!" in
    let priv_key_B = generate_private_key () in
    let pub_key_B = get_public_key priv_key_B in
    let ss_a = get_shared_secret pub_key_B priv_key_A in
    let ss_b = get_shared_secret pub_key_A priv_key_B in
    assert_equal ss_a ss_b;

    let key = secret_to_key ss_a in
    let e_msg = encrypt_msg msg key in

    assert_equal msg (decrypt_msg e_msg key))

let test_key_uniqueness _ =
  let k1 = Encrypt.generate_private_key () in
  let k2 = Encrypt.generate_private_key () in
  assert_bool "Keys should be different" (k1 <> k2)

let test_shared_secret_commutativity _ =
  let priv_a = Encrypt.generate_private_key () in
  let pub_a = Encrypt.get_public_key priv_a in
  let priv_b = Encrypt.generate_private_key () in
  let pub_b = Encrypt.get_public_key priv_b in

  let ss_a = Encrypt.get_shared_secret pub_b priv_a in
  let ss_b = Encrypt.get_shared_secret pub_a priv_b in

  assert_equal ss_a ss_b

let test_dh_on_empty_message _ =
  Encrypt.(
    let priv_key_A = generate_private_key () in

    (* print_endline ("\n PKA: " ^ Z.to_string priv_key_A); *)
    let pub_key_A = get_public_key priv_key_A in
    let msg = "" in
    let priv_key_B = generate_private_key () in
    let pub_key_B = get_public_key priv_key_B in
    let ss_a = get_shared_secret pub_key_B priv_key_A in
    let ss_b = get_shared_secret pub_key_A priv_key_B in
    assert_equal ss_a ss_b;

    let key = secret_to_key ss_a in
    let e_msg = encrypt_msg msg key in

    assert_equal msg (decrypt_msg e_msg key))

let test_dh_on_empty_message _ =
  Encrypt.(
    let priv_key_A = generate_private_key () in

    (* print_endline ("\n PKA: " ^ Z.to_string priv_key_A); *)
    let pub_key_A = get_public_key priv_key_A in
    let msg = "" in
    let priv_key_B = generate_private_key () in
    let pub_key_B = get_public_key priv_key_B in
    let ss_a = get_shared_secret pub_key_B priv_key_A in
    let ss_b = get_shared_secret pub_key_A priv_key_B in
    assert_equal ss_a ss_b;

    let key = secret_to_key ss_a in
    let e_msg = encrypt_msg msg key in

    assert_equal msg (decrypt_msg e_msg key))

let test_dh_on_newline _ =
  Encrypt.(
    let priv_key_A = generate_private_key () in

    (* print_endline ("\n PKA: " ^ Z.to_string priv_key_A); *)
    let pub_key_A = get_public_key priv_key_A in
    let msg = "\n" in
    let priv_key_B = generate_private_key () in
    let pub_key_B = get_public_key priv_key_B in
    let ss_a = get_shared_secret pub_key_B priv_key_A in
    let ss_b = get_shared_secret pub_key_A priv_key_B in
    assert_equal ss_a ss_b;

    let key = secret_to_key ss_a in
    let e_msg = encrypt_msg msg key in

    assert_equal msg (decrypt_msg e_msg key))

let test_dh_on_long_string _ =
  Encrypt.(
    let priv_key_A = generate_private_key () in

    (* print_endline ("\n PKA: " ^ Z.to_string priv_key_A); *)
    let pub_key_A = get_public_key priv_key_A in
    let msg =
      "ipuherwenrpuwefwer fweprwevuiwen rpvwenruvnweprn"
      ^ "pvweunrvnrweprvuwneprvuwenrvwervuwneprvnpw" ^ "wpergouwenrvnwev"
      ^ "pwuehrvpwehrvwev wepruhweprvhpwenrv wer"
    in
    let priv_key_B = generate_private_key () in
    let pub_key_B = get_public_key priv_key_B in
    let ss_a = get_shared_secret pub_key_B priv_key_A in
    let ss_b = get_shared_secret pub_key_A priv_key_B in
    assert_equal ss_a ss_b;

    let key = secret_to_key ss_a in
    let e_msg = encrypt_msg msg key in

    assert_equal msg (decrypt_msg e_msg key))

let tests =
  "utils tests"
  >::: [
         "encryption functions" >:: test_encryption_functions;
         "key uniqueness" >:: test_key_uniqueness;
         "shared secret commutativity" >:: test_shared_secret_commutativity;
         "newline encryption" >:: test_dh_on_newline;
         "empty string encryption" >:: test_dh_on_empty_message;
         "long string encryption" >:: test_dh_on_long_string;
       ]

let _ = run_test_tt_main tests
