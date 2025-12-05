open OUnit2
module Types = Terminal_tok.Types
module Recommender = Terminal_tok.Recommender

(* Pretty print a float safely *)
let pp_float f =
  if Float.is_nan f then "NaN"
  else if Float.is_infinite f then "∞"
  else Printf.sprintf "%.3f" f

(* Print a video nicely *)
let pp_video (v : Types.video) =
  Printf.sprintf "{title=\"%s\"; genre=\"%s\"}" v.title v.genre

(* Print a user summary (no embeddings) *)
let pp_user (u : Types.user) =
  Printf.sprintf "User(name=\"%s\", history=%d videos)" u.name
    (List.length u.vid_history)

(* Print (video * score) pairs nicely *)
let pp_video_score (v, score) =
  Printf.sprintf "%s => %s" (pp_video v) (pp_float score)

(* Print a list with custom element printer *)
let pp_list pp xs =
  let body = String.concat "; " (List.map pp xs) in
  Printf.sprintf "[%s]" body

let write_file path lines =
  let oc = open_out path in
  List.iter (fun l -> output_string oc (l ^ "\n")) lines;
  close_out oc

let rec rm_dir dir =
  if Sys.file_exists dir then begin
    if Sys.is_directory dir then begin
      let contents = try Sys.readdir dir with _ -> [||] in
      Array.iter
        (fun entry ->
          let path = Filename.concat dir entry in
          if Sys.is_directory path then rm_dir path
          else try Sys.remove path with _ -> ())
        contents;
      try Unix.rmdir dir with _ -> ()
    end
    else try Sys.remove dir with _ -> ()
  end

let with_temp_users f =
  Random.self_init ();
  let unique_id =
    Printf.sprintf "%d_%d"
      (int_of_float (Unix.gettimeofday () *. 1000.0))
      (Random.bits ())
  in
  let base =
    Filename.concat (Sys.getcwd ()) ("test_data_users_temp_" ^ unique_id)
  in
  if Sys.file_exists base then rm_dir base;
  Unix.mkdir base 0o755;
  try
    let u1 = Filename.concat base "alice" in
    Unix.mkdir u1 0o755;
    write_file (Filename.concat u1 "stats.csv") [ "genre,count"; "action,2" ];
    write_file
      (Filename.concat u1 "history.csv")
      [ "title,genre,liked,watchtime"; "Action1,action,true,10.5" ];
    let u2 = Filename.concat base "bob" in
    Unix.mkdir u2 0o755;
    write_file (Filename.concat u2 "stats.csv") [ "genre,count"; "comedy,1" ];
    write_file
      (Filename.concat u2 "history.csv")
      [ "title,genre,liked,watchtime"; "Funny,comedy,false,3.0" ];

    let users = Recommender.CFRecommender.get_all_users ~users_dir:base () in
    let res = f base users in
    rm_dir base;
    res
  with e ->
    (try rm_dir base with _ -> ());
    raise e

let with_scenario_users f =
  Random.self_init ();
  let unique_id =
    Printf.sprintf "%d_%d"
      (int_of_float (Unix.gettimeofday () *. 1000.0))
      (Random.bits ())
  in
  let base =
    Filename.concat (Sys.getcwd ()) ("test_data_scenario_" ^ unique_id)
  in
  if Sys.file_exists base then rm_dir base;
  Unix.mkdir base 0o755;
  try
    let u_neighbor = Filename.concat base "neighbor" in
    Unix.mkdir u_neighbor 0o755;
    write_file
      (Filename.concat u_neighbor "stats.csv")
      [ "genre,count"; "action,2" ];
    write_file
      (Filename.concat u_neighbor "history.csv")
      [
        "title,genre,liked,watchtime";
        "Shared Video,action,true,20.0";
        "Hidden Gem,action,true,20.0";
      ];
    let res = f base in
    rm_dir base;
    res
  with e ->
    (try rm_dir base with _ -> ());
    raise e

(* --- Tests for CFRecommender --- *)
let test_get_all_users _ =
  with_temp_users (fun _ (users : Types.user list) ->
      assert_equal ~printer:string_of_int 2 (List.length users)
        ~msg:"Should load 2 users";
      let names = List.map (fun (u : Types.user) -> u.name) users in
      assert_bool "contains alice" (List.exists (( = ) "alice") names);
      assert_bool "contains bob" (List.exists (( = ) "bob") names);
      let alice = List.find (fun (u : Types.user) -> u.name = "alice") users in
      assert_equal ~printer:string_of_int 1
        (List.length alice.vid_history)
        ~msg:"Alice should have 1 video";
      let inter = List.hd alice.vid_history in
      assert_equal ~printer:(fun s -> s) "Action1" inter.video.title;
      assert_equal ~printer:string_of_bool true inter.liked;
      assert_equal ~printer:pp_float 10.5 inter.watchtime)

let test_embed_user_list _ =
  with_temp_users (fun _ (users : Types.user list) ->
      let embedded = Recommender.CFRecommender.embed_user_list users in
      assert_equal ~printer:string_of_int 2 (List.length embedded)
        ~msg:"Should have 2 user embeddings";
      List.iter
        (fun (u, emb) ->
          assert_bool "User should have a name" (u.Types.name <> "");
          assert_bool "User should be alice or bob"
            (u.Types.name = "alice" || u.Types.name = "bob");
          assert_equal ~printer:string_of_int 5 (Array.length emb)
            ~msg:"Embedding should have dimension 5";
          let has_nonzero = Array.exists (fun x -> x <> 0.0) emb in
          assert_bool "Embedding should have non-zero values" has_nonzero;
          Array.iter
            (fun x ->
              assert_bool "Embedding value should be finite"
                (not (Float.is_nan x));
              assert_bool "Embedding value should not be infinite"
                (not (Float.is_infinite x)))
            emb)
        embedded;
      let embedded_names = List.map (fun (u, _) -> u.Types.name) embedded in
      assert_bool "Should contain alice" (List.mem "alice" embedded_names);
      assert_bool "Should contain bob" (List.mem "bob" embedded_names))

let test_get_cf_scores_deterministic _ =
  let mk_video title genre = { Types.title; ascii = ""; genre } in
  let mk_inter video = { Types.video; watchtime = 20.0; liked = true } in
  let vid_shared = mk_video "Shared Video" "Action" in
  let vid_target = mk_video "Target Video" "Adventure" in
  let user_a =
    {
      Types.name = "User A";
      vid_history = [ mk_inter vid_shared ];
      genre_counts = Hashtbl.create 1;
    }
  in
  let user_b =
    {
      Types.name = "User B";
      vid_history = [ mk_inter vid_shared; mk_inter vid_target ];
      genre_counts = Hashtbl.create 1;
    }
  in
  let mock_embeddings =
    [ (user_a, [| 1.0; 0.0 |]); (user_b, [| 1.0; 0.0 |]) ]
  in
  let results =
    Recommender.CFRecommender.get_cf_scores_for_target user_a mock_embeddings 5
  in
  let found_video =
    List.exists
      (fun (v, score) -> v.Types.title = "Target Video" && score > 0.0)
      results
  in
  if not found_video then
    assert_failure
      (Printf.sprintf "Expected 'Target Video' in results.\nGot: %s"
         (pp_list pp_video_score results));
  let found_shared =
    List.exists (fun (v, _) -> v.Types.title = "Shared Video") results
  in
  assert_bool "Should NOT recommend Shared Video" (not found_shared)

let suite = "CFRecommender tests" >::: [
  "get_all_users" >:: test_get_all_users;
  "embed_user_list" >:: test_embed_user_list;
  "get_cf_scores_deterministic" >:: test_get_cf_scores_deterministic;
]

let () = run_test_tt_main suite
