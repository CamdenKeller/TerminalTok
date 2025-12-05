open OUnit2
open Terminal_tok.Recommender
open Terminal_tok.Types

let () = Random.init 0

(* ===== Helper Functions ===== *)

let make_video title genre = { title; ascii = ""; genre }
let make_interaction video liked watchtime = { video; liked; watchtime }

let make_user_with_history name history =
  let genre_counts = Hashtbl.create 10 in
  List.iter
    (fun inter ->
      let genre = inter.video.genre in
      let current = try Hashtbl.find genre_counts genre with Not_found -> 0 in
      Hashtbl.replace genre_counts genre (current + 1))
    history;
  { name; vid_history = history; genre_counts }

(* ===== Test Cases ===== *)

let test_recommend_new_user _ =
  let v1 = make_video "Action1" "action" in
  let v2 = make_video "Comedy1" "comedy" in
  let videos = [ v1; v2 ] in
  let new_user =
    { name = "newbie"; vid_history = []; genre_counts = Hashtbl.create 5 }
  in
  let result = HybridRecommender.recommend_hybrid new_user videos [] in
  let is_valid_recommendation =
    match result with
    | None -> false
    | Some v -> v = v1 || v = v2
  in
  assert_bool
    "New user with no history should receive a\n\n   random\n video (not None)"
    is_valid_recommendation

let test_recommend_content_based _ =
  let v1 = make_video "Action1" "action" in
  let v2 = make_video "Comedy1" "comedy" in
  let v3 = make_video "Action2" "action" in
  let v4 = make_video "Drama1" "drama" in

  let inter1 = make_interaction v1 true 8.0 in
  let inter2 = make_interaction v2 false 2.0 in

  let user = make_user_with_history "content_user" [ inter1; inter2 ] in
  let videos = [ v3; v4 ] in

  let result = HybridRecommender.recommend_hybrid user videos [] in
  match result with
  | None -> assert_failure "Expected Some(video), but got None"
  | Some video ->
      assert_equal "action" video.genre
        ~msg:
          "Should recommend action\n\n\
          \   since user watched more action than comedy"

let test_recommend_hybrid_approach _ =
  let v1 = make_video "Action1" "action" in
  let v2 = make_video "Action2" "action" in
  let v3 = make_video "Action3" "action" in
  let v4 = make_video "Comedy1" "comedy" in
  let v5 = make_video "Action4" "action" in

  let inter1 = make_interaction v1 true 10.0 in
  let inter2 = make_interaction v2 true 9.0 in
  let inter3 = make_interaction v3 true 8.0 in

  let user = make_user_with_history "hybrid_user" [ inter1; inter2; inter3 ] in
  let videos = [ v4; v5 ] in

  let result = HybridRecommender.recommend_hybrid user videos [] in
  match result with
  | None -> assert_failure "Expected Some(video), but got None"
  | Some video -> assert_bool "Hybrid approach should return a video" true

(* let test_all_watched _ =
  let v1 = make_video "Action1" "action" in
  let v2 = make_video "Comedy1" "comedy" in

  let inter1 = make_interaction v1 true 5.0 in
  let inter2 = make_interaction v2 false 3.0 in

  let user = make_user_with_history "watched_all" [ inter1; inter2 ] in
  let videos = [ v1; v2 ] in

  let result = HybridRecommender.recommend_hybrid user videos in
  assert_equal None result
    ~msg:"Should return None when all videos have been watched" *)

let test_mixed_signals _ =
  let v1 = make_video "Action1" "action" in
  let v2 = make_video "Comedy1" "comedy" in
  let v3 = make_video "Action2" "action" in
  let v4 = make_video "Drama1" "drama" in
  let v5 = make_video "Action3" "action" in

  (* User liked action, didn't like comedy *)
  let inter1 = make_interaction v1 true 10.0 in
  let inter2 = make_interaction v2 false 1.0 in
  let inter3 = make_interaction v3 true 9.0 in

  let user = make_user_with_history "mixed_user" [ inter1; inter2; inter3 ] in
  let videos = [ v4; v5 ] in

  let result = HybridRecommender.recommend_hybrid user videos [] in
  match result with
  | None -> assert_failure "Expected Some(video), but got None"
  | Some video -> assert_bool "Should recommend a video" true

let test_single_video_available _ =
  let v1 = make_video "Action1" "action" in
  let v2 = make_video "Action2" "action" in

  let inter1 = make_interaction v1 true 5.0 in
  let user = make_user_with_history "single_user" [ inter1 ] in
  let videos = [ v2 ] in

  let result = HybridRecommender.recommend_hybrid user videos [] in
  match result with
  | None -> assert_failure "Expected Some(video), but got None"
  | Some video ->
      assert_equal v2 video
        ~msg:"Should recommend the only available\n\n\n   unwatched video"

let test_genre_preference _ =
  let v1 = make_video "Action1" "action" in
  let v2 = make_video "Drama1" "drama" in
  let v3 = make_video "Comedy1" "comedy" in
  let v4 = make_video "Action2" "action" in
  let v5 = make_video "Drama2" "drama" in
  let v6 = make_video "Comedy2" "comedy" in

  (* Strong preference for drama *)
  let inter1 = make_interaction v1 false 2.0 in
  let inter2 = make_interaction v2 true 15.0 in
  let inter3 = make_interaction v3 false 1.0 in
  let inter4 = make_interaction v2 true 12.0 in

  let user =
    make_user_with_history "diverse_user" [ inter1; inter2; inter3; inter4 ]
  in
  let videos = [ v4; v5; v6 ] in

  let result = HybridRecommender.recommend_hybrid user videos [] in
  match result with
  | None -> assert_failure "Expected Some(video), but got None"
  | Some video ->
      assert_equal "drama" video.genre
        ~msg:"Should recommend drama\n\n   based\n on high watch time and like"

let test_empty_video_list _ =
  let user =
    { name = "test"; vid_history = []; genre_counts = Hashtbl.create 5 }
  in
  let videos = [] in

  let result = HybridRecommender.recommend_hybrid user videos [] in
  assert_equal None result ~msg:"Should return None when video list is empty"

let test_same_genre_multiple _ =
  let v1 = make_video "Action1" "action" in
  let v2 = make_video "Action2" "action" in
  let v3 = make_video "Action3" "action" in
  let v4 = make_video "Action4" "action" in

  let inter1 = make_interaction v1 true 8.0 in
  let user = make_user_with_history "action_fan" [ inter1 ] in
  let videos = [ v2; v3; v4 ] in

  let result = HybridRecommender.recommend_hybrid user videos [] in
  match result with
  | None -> assert_failure "Expected Some(video), but got None"
  | Some video ->
      assert_bool "Should recommend one of the action videos"
        (video = v2 || video = v3 || video = v4)

let test_progressive_strategy _ =
  (* Test that strategy changes based on history length *)
  let v1 = make_video "Action1" "action" in
  let v2 = make_video "Action2" "action" in
  let videos = [ v2 ] in

  (* 0 interactions: random *)
  let user0 =
    { name = "user0"; vid_history = []; genre_counts = Hashtbl.create 5 }
  in
  let result0 = HybridRecommender.recommend_hybrid user0 videos [] in
  assert_bool "0\n\n\n   interactions should give result" (result0 <> None);

  (* 1 interaction: content-based *)
  let inter1 = make_interaction v1 true 5.0 in
  let user1 = make_user_with_history "user1" [ inter1 ] in
  let result1 = HybridRecommender.recommend_hybrid user1 videos [] in
  assert_bool "1\n\n   interaction\n should give result" (result1 <> None);

  (* 3 interactions: hybrid *)
  let v3 = make_video "Action3" "action" in
  let v4 = make_video "Action4" "action" in
  let inter2 = make_interaction v3 true 6.0 in
  let inter3 = make_interaction v4 true 7.0 in
  let user3 = make_user_with_history "user3" [ inter1; inter2; inter3 ] in
  let result3 = HybridRecommender.recommend_hybrid user3 videos [] in
  assert_bool "3\n\n\n   interactions should give result" (result3 <> None)

let test_filters_watched_videos _ =
  let v1 = make_video "Action1" "action" in
  let v2 = make_video "Action2" "action" in
  let v3 = make_video "Action3" "action" in

  let inter1 = make_interaction v1 true 5.0 in
  let inter2 = make_interaction v2 true 6.0 in

  let user = make_user_with_history "user" [ inter1; inter2 ] in
  let videos = [ v1; v2; v3 ] in

  let result = HybridRecommender.recommend_hybrid user videos [] in
  match result with
  | None -> assert_failure "Should recommend unwatched video"
  | Some video ->
      assert_equal v3 video ~msg:"Should only recommend the unwatched video"

let test_multiple_genre_balance _ =
  let v1 = make_video "Action1" "action" in
  let v2 = make_video "Action2" "action" in
  let v3 = make_video "Comedy1" "comedy" in
  let v4 = make_video "Comedy2" "comedy" in
  let v5 = make_video "Action3" "action" in
  let v6 = make_video "Comedy3" "comedy" in

  (* Equal action and comedy *)
  let inter1 = make_interaction v1 true 5.0 in
  let inter2 = make_interaction v2 true 5.0 in
  let inter3 = make_interaction v3 true 5.0 in
  let inter4 = make_interaction v4 true 5.0 in
  let user =
    make_user_with_history "balanced" [ inter1; inter2; inter3; inter4 ]
  in
  let videos = [ v5; v6 ] in

  let result = HybridRecommender.recommend_hybrid user videos [] in
  match result with
  | None -> assert_failure "Expected Some(video), but got None"
  | Some video ->
      assert_bool "Should recommend either action or comedy"
        (video.genre = "action" || video.genre = "comedy")

let test_hybrid_with_embeddings _ =
  let v1 = make_video "Action1" "action" in
  let v2 = make_video "Action2" "action" in
  let v3 = make_video "Action3" "action" in

  let inter1 = make_interaction v1 true 10.0 in
  let inter2 = make_interaction v2 true 9.0 in

  let user = make_user_with_history "test_user" [ inter1; inter2 ] in
  
  (* Create mock embeddings *)
  let mock_embeddings = [
    (user, [| 1.0; 0.0; 0.0; 0.0; 0.0 |]);
  ] in

  let videos = [ v3 ] in

  let result = HybridRecommender.recommend_hybrid user videos mock_embeddings in
  match result with
  | None -> assert_failure "Should recommend a video with embeddings"
  | Some video -> assert_equal v3 video

let test_content_based_score_missing_genre _ =
    let v1 = make_video "Action1" "action" in
    let v2 = make_video "Drama1" "drama" in
  
    let inter1 = make_interaction v1 true 5.0 in
    let user = make_user_with_history "user" [ inter1 ] in
  
    (* v2 is drama, but user only watched action so should have 0 for drama *)
    let videos = [ v2 ] in
    let result = HybridRecommender.recommend_hybrid user videos [] in
    
    assert_bool "Should handle missing genre gracefully" (result <> None)

(* test with single interaction (edge of ML threshold) *)
let test_single_interaction_ml_threshold _ =
  let v1 = make_video "Action1" "action" in
  let v2 = make_video "Action2" "action" in

  let inter1 = make_interaction v1 true 5.0 in
  let user = make_user_with_history "single_inter" [ inter1 ] in
  let videos = [ v2] in

  let result = HybridRecommender.recommend_hybrid user videos [] in
  assert_bool "handle single interaction case" (result <> None)

(* test with exactly 2 interactions (ML threshold boundary) *)
let test_two_interactions_ml_threshold _ =
  let v1 = make_video "Action1" "action" in
  let v2 = make_video "Action2" "action" in
  let v3 = make_video "Action3" "action" in

  let inter1 = make_interaction v1 true 5.0 in
  let inter2 = make_interaction v2 true 6.0 in
  let user = make_user_with_history "two_inter" [ inter1; inter2 ] in
  let videos = [ v3 ] in

  let result = HybridRecommender.recommend_hybrid user videos [] in
  assert_bool "handle two interactions (ML threshold)" (result <> None)

(* test with many interactions *)
let test_many_interactions _ =
  let videos_watched = 
    List.init 10 (fun i -> make_video (Printf.sprintf "Video%d" i) "action")
  in
  let history = List.map (fun v -> make_interaction v true 5.0) videos_watched in
  let user = make_user_with_history "power_user" history in
  
  let new_video = make_video "NewVideo" "action" in
  let result = HybridRecommender.recommend_hybrid user [new_video] [] in
  
  assert_bool "handle many interactions" (result <> None)

(* test different watchtime values *)
let test_watchtime_variations _ =
  let v1 = make_video "Short" "action" in
  let v2 = make_video "Medium" "action" in
  let v3 = make_video "Long" "action" in
  let v4 = make_video "VeryLong" "action" in
  let v_new = make_video "NewVideo" "action" in

  let inter1 = make_interaction v1 true 0.5 in 
  let inter2 = make_interaction v2 true 5.0 in
  let inter3 = make_interaction v3 true 15.0 in 
  let inter4 = make_interaction v4 true 100.0 in

  let user = make_user_with_history "var_user" [ inter1; inter2; inter3; inter4 ] in
  let result = HybridRecommender.recommend_hybrid user [v_new] [] in

  assert_bool " handle watchtime variations" (result <> None)

(* test liked vs not liked *)
let test_liked_not_liked_mix _ =
  let v1 = make_video "Liked1" "action" in
  let v2 = make_video "NotLiked1" "action" in
  let v3 = make_video "Liked2" "comedy" in
  let v4 = make_video "NotLiked2" "comedy" in
  let v_new = make_video "NewAction" "action" in

  let inter1 = make_interaction v1 true 10.0 in
  let inter2 = make_interaction v2 false 10.0 in
  let inter3 = make_interaction v3 true 10.0 in
  let inter4 = make_interaction v4 false 10.0 in

  let user = make_user_with_history "mixed_likes" [ inter1; inter2; inter3; inter4 ] in
  let result = HybridRecommender.recommend_hybrid user [v_new] [] in
  assert_bool "handle mixed liked/not-liked" (result <> None)

let tests =
  "recommender tests"
  >::: [
      "recommend new user" >:: test_recommend_new_user;
      "recommend content based" >:: test_recommend_content_based;
      "recommend hybrid approach" >:: test_recommend_hybrid_approach;
      (*"all watched" >:: test_all_watched;**)
      "mixed signals" >:: test_mixed_signals;
      "single video available" >:: test_single_video_available;
      "genre preference" >:: test_genre_preference;
      "empty video list" >:: test_empty_video_list;
      "same genre multiple" >:: test_same_genre_multiple;
      "progressive strategy" >:: test_progressive_strategy;
      "filters watched videos" >:: test_filters_watched_videos;
      "hybrid with embeddings" >:: test_hybrid_with_embeddings;
      "content score missing genre" >:: test_content_based_score_missing_genre;
      "single interaction ML threshold" >:: test_single_interaction_ml_threshold;
      "two interactions ML threshold" >:: test_two_interactions_ml_threshold;
      "many interactions" >:: test_many_interactions;
      "watchtime variations" >:: test_watchtime_variations;
      "liked not liked mix" >:: test_liked_not_liked_mix;
      "test multiple genre balance" >:: test_multiple_genre_balance;
      ]

let _ = run_test_tt_main tests
