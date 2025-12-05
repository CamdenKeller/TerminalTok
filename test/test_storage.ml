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

let test_load_video_list _ =
  let temp_dir = "test_videos_" ^ string_of_int (Random.int 10000) in
  (try Unix.mkdir temp_dir 0o755 with _ -> ());
  
  let touch filename =
    let oc = open_out (Filename.concat temp_dir filename) in
    close_out oc
  in
  
  touch "vid1.mp4";
  touch "vid2.mov";
  touch "vid3.mkv";
  touch "ignore.txt";
  touch "image.jpg";

  let videos = Storage.load_video_list temp_dir in
  
  let files = Sys.readdir temp_dir in
  Array.iter (fun f -> Sys.remove (Filename.concat temp_dir f)) files;
  Unix.rmdir temp_dir;

  assert_equal ~printer:pp_int 3 (List.length videos);
  let base_names = List.map Filename.basename videos in
  assert_bool "vid1.mp4" (List.mem "vid1.mp4" base_names);
  assert_bool "vid2.mov" (List.mem "vid2.mov" base_names);
  assert_bool "vid3.mkv" (List.mem "vid3.mkv" base_names);
  assert_bool "ignore.txt" (not (List.mem "ignore.txt" base_names))

let test_load_user_partial_data _ =
  let user_name = "PartialUser" in
  let user_dir = Filename.concat "data/users" user_name in
  if not (Sys.file_exists "data/users") then Unix.mkdir "data/users" 0o755;
  if not (Sys.file_exists user_dir) then Unix.mkdir user_dir 0o755;
  let history_file = Filename.concat user_dir "history.csv" in
  let oc = open_out history_file in
  output_string oc "Video1,action,true,10.0\n";
  close_out oc;
  
  (match Storage.load_user user_name with
  | Some u -> 
      assert_equal 1 (List.length u.vid_history);
      assert_equal 0 (Hashtbl.length u.genre_counts)
  | None -> assert_failure "Should load user with only history");

  Sys.remove history_file;

  let stats_file = Filename.concat user_dir "stats.csv" in
  let oc = open_out stats_file in
  output_string oc "action,5\n";
  close_out oc;

  (match Storage.load_user user_name with
  | Some u -> 
      assert_equal 0 (List.length u.vid_history);
      assert_equal 5 (Hashtbl.find u.genre_counts "action")
  | None -> assert_failure "Should load user with only stats");

  Sys.remove stats_file;
  Unix.rmdir user_dir

let test_load_user_malformed_data _ =
  let user_name = "MalformedUser" in
  let user_dir = Filename.concat "data/users" user_name in
  if not (Sys.file_exists "data/users") then Unix.mkdir "data/users" 0o755;
  if not (Sys.file_exists user_dir) then Unix.mkdir user_dir 0o755;

  let history_file = Filename.concat user_dir "history.csv" in
  let oc = open_out history_file in
  output_string oc "Video1,action\n"; (* 2 cols *)
  output_string oc "Video2,action,true,10.0,extra\n"; (* 5 cols *)
  close_out oc;

  let stats_file = Filename.concat user_dir "stats.csv" in
  let oc = open_out stats_file in
  output_string oc "action,5,extra\n";
  close_out oc;

  (match Storage.load_user user_name with
  | Some u ->
      assert_equal 2 (List.length u.vid_history);
      let v1 = List.hd u.vid_history in
      assert_equal "Unknown" v1.video.title;
      
      let v2 = List.nth u.vid_history 1 in
      assert_equal "Unknown" v2.video.title;

      assert_equal 0 (Hashtbl.length u.genre_counts)
  | None -> assert_failure "Should load user despite malformed data");

  Sys.remove history_file;
  Sys.remove stats_file;
  Unix.rmdir user_dir

let tests =
  "storage tests"
  >::: [
         "save and load user" >:: test_save_and_load_user;
         "load nonexistent user" >:: test_load_nonexistent_user;
         "save user with special chars" >:: test_save_user_with_special_chars;
         "save user empty history" >:: test_save_user_empty_history;
         "overwrite user" >:: test_overwrite_user;
         "load video list" >:: test_load_video_list;
         "load user partial data" >:: test_load_user_partial_data;
         "load user malformed data" >:: test_load_user_malformed_data;
       ]

let _ = run_test_tt_main tests
