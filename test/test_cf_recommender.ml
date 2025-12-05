open OUnit2

module Types = Terminal_tok.Types
module Recommender = Terminal_tok.Recommender

let write_file path lines =
  let oc = open_out path in
  List.iter (fun l -> output_string oc (l ^ "\n")) lines;
  close_out oc

let rec rm_dir dir =
  if Sys.file_exists dir then begin
    if Sys.is_directory dir then begin
      (* Read directory contents *)
      let contents = 
        try Sys.readdir dir 
        with _ -> [||] 
      in
      (* Remove all contents first *)
      Array.iter (fun entry ->
        let path = Filename.concat dir entry in
        if Sys.is_directory path then 
          rm_dir path 
        else 
          (try Sys.remove path with _ -> ())
      ) contents;
      (* Then remove the directory itself *)
      try Unix.rmdir dir with _ -> ()
    end else
      (* It's a file, just remove it *)
      try Sys.remove dir with _ -> ()
  end

let with_temp_users f =
  (* Initialize random seed to get different numbers each time *)
  Random.self_init ();
  
  (* Use timestamp + random for better uniqueness *)
  let unique_id = Printf.sprintf "%d_%d" 
    (int_of_float (Unix.gettimeofday () *. 1000.0))
    (Random.bits ()) 
  in
  let base = Filename.concat (Sys.getcwd ()) ("test_data_users_temp_" ^ unique_id) in
  
  (* Clean up if directory already exists *)
  if Sys.file_exists base then rm_dir base;
  Unix.mkdir base 0o755;
  try
    (* user: alice *)
    let u1 = Filename.concat base "alice" in
    Unix.mkdir u1 0o755;
    write_file (Filename.concat u1 "stats.csv") ["genre,count"; "action,2"];    
    write_file (Filename.concat u1 "history.csv") ["title,genre,liked,watchtime"; "Action1,action,true,10.5"];    
    (* user: bob *)
    let u2 = Filename.concat base "bob" in
    Unix.mkdir u2 0o755;
    write_file (Filename.concat u2 "stats.csv") ["genre,count"; "comedy,1"];    
    write_file (Filename.concat u2 "history.csv") ["title,genre,liked,watchtime"; "Funny,comedy,false,3.0"];    

    let users = Recommender.CFRecommender.get_all_users ~users_dir:base () in
    let res = f base users in
    (* cleanup *)
    rm_dir base;
    res
  with e ->
    (* cleanup on error *)
    (try rm_dir base with _ -> ());
    raise e
  
  let with_scenario_users f = 
  Random.self_init ();
  let unique_id = Printf.sprintf "%d_%d" (int_of_float (Unix.gettimeofday () *. 1000.0)) (Random.bits ()) in
  let base = Filename.concat (Sys.getcwd ()) ("test_data_scenario_" ^ unique_id) in
  
  if Sys.file_exists base then rm_dir base;
  Unix.mkdir base 0o755;
  
  try
    (* 1. Create a "Neighbor" user on disk *)
    (* This user likes "Shared Video" AND "Hidden Gem" *)
    let u_neighbor = Filename.concat base "neighbor" in
    Unix.mkdir u_neighbor 0o755;
    write_file (Filename.concat u_neighbor "stats.csv") ["genre,count"; "action,2"];
    
    (* Neighbor history *)
    write_file (Filename.concat u_neighbor "history.csv") [
      "title,genre,liked,watchtime"; 
      "Shared Video,action,true,20.0"; (* The link between users *)
      "Hidden Gem,action,true,20.0"    (* The target recommendation *)
    ];

    (* Run the test function passing the base directory *)
    let res = f base in
    rm_dir base;
    res
  with e ->
    (try rm_dir base with _ -> ());
    raise e

(* --- Tests for CFRecommender --- *)
let test_get_all_users _ =
  with_temp_users (fun _ (users : Types.user list) ->
    assert_equal 2 (List.length users) ~msg:"Should load 2 users";
    let names = List.map (fun (u : Types.user) -> u.name) users in
    assert_bool "contains alice" (List.exists ((=) "alice") names);
    assert_bool "contains bob" (List.exists ((=) "bob") names);
    let alice = List.find (fun (u : Types.user) -> u.name = "alice") users in
    assert_equal 1 (List.length alice.vid_history) ~msg:"Alice should have 1 video";
    let inter = List.hd alice.vid_history in
    assert_equal "Action1" inter.video.title;
    assert_equal true inter.liked;
    assert_equal 10.5 inter.watchtime
  )

let test_embed_user_list _ =
  with_temp_users (fun _ (users : Types.user list) ->
    (* Embed a list of users *)
    let embedded = Recommender.CFRecommender.embed_user_list users in
    
    (* Check that we get embeddings for all users *)
    assert_equal 2 (List.length embedded) ~msg:"Should have 2 user embeddings";
    
    (* Check each embedding *)
    List.iter (fun (u, emb) ->
      (* Check user is valid *)
      assert_bool "User should have a name" (u.Types.name <> "");
      assert_bool "User should be alice or bob" 
        (u.Types.name = "alice" || u.Types.name = "bob");
      
      (* Check embedding dimensions *)
      assert_equal 5 (Array.length emb) ~msg:"Embedding should have dimension 5";
      
      (* Check that embedding values are reasonable (not all zeros) *)
      let has_nonzero = Array.exists (fun x -> x <> 0.0) emb in
      assert_bool "Embedding should have non-zero values" has_nonzero;
      
      (* Check values are in reasonable range (roughly -1 to 1 after training) *)
      Array.iter (fun x ->
        assert_bool "Embedding value should be finite" (not (Float.is_nan x));
        assert_bool "Embedding value should not be infinite" (not (Float.is_infinite x))
      ) emb
    ) embedded;
    
    (* Verify all users are present *)
    let embedded_names = List.map (fun (u, _) -> u.Types.name) embedded in
    assert_bool "Should contain alice" (List.mem "alice" embedded_names);
    assert_bool "Should contain bob" (List.mem "bob" embedded_names)
  )

let test_get_cf_scores_deterministic _ =
  (* 1. Setup Data: Create two users with identical taste *)
  (* Helper to make video objects quickly *)
  let mk_video title genre = { Types.title = title; ascii = ""; genre = genre } in
  let mk_inter video = { Types.video = video; watchtime = 20.0; liked = true } in

  let vid_shared = mk_video "Shared Video" "Action" in
  let vid_target = mk_video "Target Video" "Adventure" in (* User B has seen this! *)

  (* User A has only seen the shared video *)
  let user_a = { 
    Types.name = "User A"; 
    vid_history = [mk_inter vid_shared]; 
    genre_counts = Hashtbl.create 1 
  } in

  (* User B has seen the shared video AND the target video *)
  let user_b = { 
    Types.name = "User B"; 
    vid_history = [mk_inter vid_shared; mk_inter vid_target]; 
    genre_counts = Hashtbl.create 1 
  } in

  (* 2. FORCE EMBEDDINGS (No Random ML!) *)
  (* We give them identical vectors [1.0; 0.0]. Cosine Similarity will be exactly 1.0. *)
  let mock_embeddings = [
    (user_a, [| 1.0; 0.0 |]);
    (user_b, [| 1.0; 0.0 |]); 
  ] in

  (* 3. Run the Pure Function *)
  let results = Recommender.CFRecommender.get_cf_scores_for_target 
    user_a 
    mock_embeddings 
    5 
  in

  (* 4. Assertions *)
  (* User B is a perfect match (score 1.0), so their "Target Video" score should transfer to User A *)
  let found_video = List.exists (fun (v, score) -> 
    v.Types.title = "Target Video" && score > 0.0
  ) results in

  assert_bool "User B should recommend Target Video to User A!" found_video;

  (* Ensure we don't recommend what User A already watched *)
  let found_shared = List.exists (fun (v, _) -> v.Types.title = "Shared Video") results in
  assert_bool "Should NOT recommend Shared Video" (not found_shared)

let suite = "CFRecommender tests" >::: [
  "get_all_users" >:: test_get_all_users;
  "embed_user_list" >:: test_embed_user_list;
  "get_cf_scores_deterministic" >:: test_get_cf_scores_deterministic;
]

let () = run_test_tt_main suite
