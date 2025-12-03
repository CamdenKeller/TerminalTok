open OUnit2
open Terminal_tok.Recommender
open Terminal_tok.Types
open Terminal_tok

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

let tests =
  "recommender tests"
  >::: [
         "recommend basic" >:: test_recommend_basic;
         "recommend new user" >:: test_recommend_new_user;
       ]

let _ = run_test_tt_main tests
