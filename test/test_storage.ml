open OUnit2
open Terminal_tok.Types
open Terminal_tok

let pp_string s = Printf.sprintf "%S" s
let pp_float f = Printf.sprintf "%.3f" f
let pp_bool b = string_of_bool b
let pp_int = string_of_int
let pp_video (v : video) = Printf.sprintf "{title=%S; genre=%S}" v.title v.genre

let pp_interaction (i : interaction) =
  Printf.sprintf "{video=%s; liked=%b; watchtime=%.2f}" (pp_video i.video)
    i.liked i.watchtime

let pp_user (u : user) =
  Printf.sprintf "User(%S, history=%d)" u.name (List.length u.vid_history)

let pp_opt_user = function
  | None -> "None"
  | Some u -> "Some " ^ pp_user u

let test_save_and_load_user _ =
  let user_gc = Hashtbl.create 5 in
  Hashtbl.add user_gc "action" 5;
  Hashtbl.add user_gc "comedy" 2;
  let v1 = { title = "Action1"; ascii = ""; genre = "action" } in
  let interaction = { video = v1; watchtime = 10.5; liked = true } in
  let user =
    { name = "TestUser"; vid_history = [ interaction ]; genre_counts = user_gc }
  in

  Storage.save_user user;

  match Storage.load_user "TestUser" with
  | None -> assert_failure "Failed to load saved user"
  | Some loaded_user ->
      assert_equal ~printer:pp_string "TestUser" loaded_user.name;
      assert_equal ~printer:pp_int 1 (List.length loaded_user.vid_history);
      let loaded_interaction = List.hd loaded_user.vid_history in
      assert_equal ~printer:pp_string "Action1" loaded_interaction.video.title;
      assert_equal ~printer:pp_bool true loaded_interaction.liked;
      assert_equal ~printer:pp_float 10.5 loaded_interaction.watchtime;
      assert_equal ~printer:pp_int 5
        (Hashtbl.find loaded_user.genre_counts "action")

let test_load_nonexistent_user _ =
  match Storage.load_user "NonExistentUser" with
  | None -> () (* success *)
  | Some u ->
      assert_failure
        (Printf.sprintf "Should not load nonexistent user, but got %s"
           (pp_user u))

let test_save_user_with_special_chars _ =
  let user_gc = Hashtbl.create 5 in
  Hashtbl.add user_gc "weird, genre" 1;
  let v1 =
    {
      title = "Title with \"quotes\" and ,commas,";
      ascii = "";
      genre = "weird, genre";
    }
  in
  let interaction = { video = v1; watchtime = 5.0; liked = false } in
  let user =
    {
      name = "SpecialUser";
      vid_history = [ interaction ];
      genre_counts = user_gc;
    }
  in

  Storage.save_user user;

  match Storage.load_user "SpecialUser" with
  | None -> assert_failure "Failed to load special user"
  | Some loaded_user ->
      let loaded_interaction = List.hd loaded_user.vid_history in
      assert_equal ~printer:pp_string "Title with \"quotes\" and ,commas,"
        loaded_interaction.video.title;
      assert_equal ~printer:pp_string "weird, genre"
        loaded_interaction.video.genre;
      assert_equal ~printer:pp_int 1
        (Hashtbl.find loaded_user.genre_counts "weird, genre")

let test_save_user_empty_history _ =
  let user =
    { name = "EmptyUser"; vid_history = []; genre_counts = Hashtbl.create 5 }
  in
  Storage.save_user user;
  match Storage.load_user "EmptyUser" with
  | None -> assert_failure "Failed to load user with empty history"
  | Some loaded_user ->
      assert_equal ~printer:pp_string "EmptyUser" loaded_user.name;
      assert_equal ~printer:pp_int 0 (List.length loaded_user.vid_history)

let test_overwrite_user _ =
  let user_gc = Hashtbl.create 5 in
  let user =
    { name = "OverwriteUser"; vid_history = []; genre_counts = user_gc }
  in
  Storage.save_user user;

  let v1 = { title = "NewVideo"; ascii = ""; genre = "new" } in
  let interaction = { video = v1; watchtime = 10.0; liked = true } in
  let updated_user = { user with vid_history = [ interaction ] } in
  Storage.save_user updated_user;

  match Storage.load_user "OverwriteUser" with
  | None -> assert_failure "Failed to load overwritten user"
  | Some loaded_user ->
      assert_equal ~printer:pp_int 1 (List.length loaded_user.vid_history);
      let loaded_interaction = List.hd loaded_user.vid_history in
      assert_equal ~printer:pp_string "NewVideo" loaded_interaction.video.title

let tests =
  "storage tests"
  >::: [
         "save and load user" >:: test_save_and_load_user;
         "load nonexistent user" >:: test_load_nonexistent_user;
         "save user with special chars" >:: test_save_user_with_special_chars;
         "save user empty history" >:: test_save_user_empty_history;
         "overwrite user" >:: test_overwrite_user;
       ]

let _ = run_test_tt_main tests
