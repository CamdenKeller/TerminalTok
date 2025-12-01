open OUnit2
open Terminal_tok.Recommender
open Terminal_tok.Types
open Terminal_tok

type test_client = {
  priv_key : string;
  pub_key : string;
  msg : string;
}

let test_recommend_basic _ =
  let v1 = { title = "Action1"; ascii = ""; genre = "action" } in
  let v2 = { title = "Comedy1"; ascii = ""; genre = "comedy" } in
  let v3 = { title = "Action2"; ascii = ""; genre = "action" } in
  let videos = [ v1; v2; v3 ] in

  let user_gc = Hashtbl.create 5 in
  Hashtbl.add user_gc "action" 5;
  Hashtbl.add user_gc "comedy" 2;

  let basic_user =
    { name = "greg"; vid_history = []; genre_counts = user_gc }
  in

  let result = recommend basic_user videos in

  match result with
  | None -> assert_failure "Expected Some(video), but got None"
  | Some video ->
      assert_equal "action" video.genre
        ~msg:"Recommended video's genre should match user's top genre"

let test_recommend_new_user _ =
  let v1 = { title = "Action1"; ascii = ""; genre = "action" } in
  let v2 = { title = "Comedy1"; ascii = ""; genre = "comedy" } in
  let videos = [ v1; v2 ] in

  let new_user =
    { name = "newbie"; vid_history = []; genre_counts = Hashtbl.create 5 }
  in

  let result = recommend new_user videos in

  let is_valid_recommendation =
    match result with
    | None -> false
    | Some v -> v = v1 || v = v2
  in
  assert_bool
    "New user with no history should receive a random video (not None)"
    is_valid_recommendation

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

let test_main_helpers _ =
  let clients =
    "him[24938097727779976536350844407426122624004392643301406290594978523281218176954375829017921584721189316644070807554099912653476281583883308969542962886226511632584880219238458320201810301153126]"
    ^ " \
       kim[24938097727779976536350844407426122624004392643301406290594978523281218176954375829017921584721189316644070807554099912653476281583883308969542962886226511632584880219238458320201810301153126]"
    ^ " \
       jim[24938097727779976536350844407426122624004392643301406290594978523281218176954375829017921584721189316644070807554099912653476281583883308969542962886226511632584880219238458320201810301153126]"
    ^ " \
       lim[24938097727779976536350844407426122624004392643301406290594978523281218176954375829017921584721189316644070807554099912653476281583883308969542962886226511632584880219238458320201810301153126]"
    ^ " \
       tim[24938097727779976536350844407426122624004392643301406290594978523281218176954375829017921584721189316644070807554099912653476281583883308969542962886226511632584880219238458320201810301153126]"
  in

  assert_equal "him kim jim lim tim " (Utils.format_names clients)

let tests =
  "test suite"
  >::: [
         "recommend basic" >:: test_recommend_basic;
         "recommend new user" >:: test_recommend_new_user;
         "encryption functions" >:: test_encryption_functions;
         "main helpers" >:: test_main_helpers;
       ]

let _ = run_test_tt_main tests
