open OUnit2
open Terminal_tok.Types
open Terminal_tok

let test_save_and_load_user _ =
  let user_gc = Hashtbl.create 5 in
  Hashtbl.add user_gc "action" 5;
  Hashtbl.add user_gc "comedy" 2;
  let v1 = { title = "Action1"; ascii = ""; genre = "action" } in
  let interaction = { video = v1; watchtime = 10.5; liked = true } in
  let user = { name = "TestUser"; vid_history = [interaction]; genre_counts = user_gc } in

  Storage.save_user user;
  
  match Storage.load_user "TestUser" with
  | None -> assert_failure "Failed to load saved user"
  | Some loaded_user ->
      assert_equal "TestUser" loaded_user.name ~printer:(fun x -> x);
      assert_equal 1 (List.length loaded_user.vid_history) ~printer:string_of_int;
      let loaded_interaction = List.hd loaded_user.vid_history in
      assert_equal "Action1" loaded_interaction.video.title ~printer:(fun x -> x);
      assert_equal true loaded_interaction.liked ~printer:string_of_bool;
      assert_equal 10.5 loaded_interaction.watchtime ~printer:string_of_float;
      assert_equal 5 (Hashtbl.find loaded_user.genre_counts "action") ~printer:string_of_int

let test_load_nonexistent_user _ =
  match Storage.load_user "NonExistentUser" with
  | None -> ()
  | Some _ -> assert_failure "Should not load nonexistent user"

let test_save_user_with_special_chars _ =
  let user_gc = Hashtbl.create 5 in
  Hashtbl.add user_gc "weird, genre" 1;
  let v1 = { title = "Title with \"quotes\" and ,commas,"; ascii = ""; genre = "weird, genre" } in
  let interaction = { video = v1; watchtime = 5.0; liked = false } in
  let user = { name = "SpecialUser"; vid_history = [interaction]; genre_counts = user_gc } in

  Storage.save_user user;

  match Storage.load_user "SpecialUser" with
  | None -> assert_failure "Failed to load special user"
  | Some loaded_user ->
      let loaded_interaction = List.hd loaded_user.vid_history in
      assert_equal "Title with \"quotes\" and ,commas," loaded_interaction.video.title ~printer:(fun x -> x);
      assert_equal "weird, genre" loaded_interaction.video.genre ~printer:(fun x -> x);
      assert_equal 1 (Hashtbl.find loaded_user.genre_counts "weird, genre") ~printer:string_of_int

let tests =
  "storage tests"
  >::: [
         "save and load user" >:: test_save_and_load_user;
         "load nonexistent user" >:: test_load_nonexistent_user;
         "save user with special chars" >:: test_save_user_with_special_chars;
       ]

let _ = run_test_tt_main tests
