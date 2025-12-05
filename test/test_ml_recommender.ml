open OUnit2
open Terminal_tok.Recommender
open Terminal_tok.Types

let () = Random.init 0

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

let test_recommend_ml_no_videos _ =
  let embeddings = MLRecommender.init_embeddings () in
  let user = make_user_with_history "user" [] in
  let videos = [] in
  
  let result = MLRecommender.recommend_ml embeddings user videos in
  assert_equal None result ~msg:"return None when no videos available"

let test_recommend_ml_all_watched _ =
  let embeddings = MLRecommender.init_embeddings () in
  let v1 = make_video "Action1" "action" in
  let v2 = make_video "Action2" "action" in
  
  let inter1 = make_interaction v1 true 5.0 in
  let inter2 = make_interaction v2 true 6.0 in
  
  let user = make_user_with_history "user" [inter1; inter2] in
  let videos = [v1; v2] in
  
  let result = MLRecommender.recommend_ml embeddings user videos in
  assert_equal None result ~msg:"return None when all videos watched"

let test_recommend_ml_cold_start _ =
  let embeddings = MLRecommender.init_embeddings () in
  let v1 = make_video "Action1" "action" in
  let v2 = make_video "Action2" "action" in
  
  (* 0 interactions *)
  let user = make_user_with_history "new_user" [] in
  let videos = [v1; v2] in
  
  let result = MLRecommender.recommend_ml embeddings user videos in
  match result with
  | None -> assert_failure "return a random video for cold start"
  | Some v -> assert_bool "return one of the available videos" 
      (v = v1 || v = v2)

let test_recommend_ml_one_interaction _ =
  let embeddings = MLRecommender.init_embeddings () in
  let v1 = make_video "Action1" "action" in
  let v2 = make_video "Action2" "action" in
  let v3 = make_video "Action3" "action" in
  
  (* user with only 1 interactcion*)
  let inter1 = make_interaction v1 true 5.0 in
  let user = make_user_with_history "user" [inter1] in
  let videos = [v2; v3] in
  
  let result = MLRecommender.recommend_ml embeddings user videos in
  match result with
  | None -> assert_failure "Should return a random video with 1 interaction"
  | Some v -> assert_bool "Should return one of the unwatched videos" 
      (v = v2 || v = v3)

let test_recommend_ml_with_history _ =
  let embeddings = MLRecommender.init_embeddings () in
  let v1 = make_video "Action1" "action" in
  let v2 = make_video "Action2" "action" in
  let v3 = make_video "Action3" "action" in
  let v4 = make_video "Action4" "action" in
  
  (* User with 2+ interactions so use ML *)
  let inter1 = make_interaction v1 true 10.0 in
  let inter2 = make_interaction v2 true 9.0 in
  let user = make_user_with_history "user" [inter1; inter2] in
  let videos = [v3; v4] in
  
  let result = MLRecommender.recommend_ml embeddings user videos in
  match result with
  | None -> assert_failure "return ML-based recommendation"
  | Some v -> assert_bool "return an unwatched video" 
      (v = v3 || v = v4)

let test_recommend_ml_filters_watched _ =
  let embeddings = MLRecommender.init_embeddings () in
  let v1 = make_video "Watched1" "action" in
  let v2 = make_video "Watched2" "action" in
  let v3 = make_video "Unwatched" "action" in
  
  let inter1 = make_interaction v1 true 5.0 in
  let inter2 = make_interaction v2 true 6.0 in
  let user = make_user_with_history "user" [inter1; inter2] in
  
  (* all three videos available, but only v3 unwatched *)
  let videos = [v1; v2; v3] in
  
  let result = MLRecommender.recommend_ml embeddings user videos in
  match result with
  | None -> assert_failure "recommend the unwatched video"
  | Some v -> assert_equal v3 v ~msg:"recommend unwatched video"

let test_recommend_ml_many_interactions _ =
  let embeddings = MLRecommender.init_embeddings () in
  
  (* User with many interactions *)
  let watched = List.init 10 (fun i -> 
    make_video (Printf.sprintf "Watched%d" i) "action"
  ) in
  let history = List.map (fun v -> make_interaction v true 8.0) watched in
  let user = make_user_with_history "power_user" history in
  
  let new_video = make_video "NewVideo" "action" in
  let videos = new_video :: watched in
  
  let result = MLRecommender.recommend_ml embeddings user videos in
  match result with
  | None -> assert_failure "recommend for power user"
  | Some v -> assert_equal new_video v 
      ~msg:"recommend the only unwatched video"

let test_recommend_ml_single_unwatched _ =
  let embeddings = MLRecommender.init_embeddings () in
  let v1 = make_video "Watched" "action" in
  let v2 = make_video "Unwatched" "action" in
  
  let inter1 = make_interaction v1 true 5.0 in
  let user = make_user_with_history "user" [inter1] in
  let videos = [v1; v2] in
  
  let result = MLRecommender.recommend_ml embeddings user videos in
  match result with
  | None -> assert_failure "recommend the single unwatched video"
  | Some v -> assert_equal v2 v 
      ~msg:"recommend the only unwatched video"

let test_recommend_ml_exact_two_interactions _ =
  let embeddings = MLRecommender.init_embeddings () in
  let v1 = make_video "Action1" "action" in
  let v2 = make_video "Action2" "action" in
  let v3 = make_video "Action3" "action" in
  
  (* user with 2 interactions *)
  let inter1 = make_interaction v1 true 5.0 in
  let inter2 = make_interaction v2 true 6.0 in
  let user = make_user_with_history "user" [inter1; inter2] in
  let videos = [v3] in
  
  let result = MLRecommender.recommend_ml embeddings user videos in
  match result with
  | None -> assert_failure "use ML with exactly 2 interactions"
  | Some v -> assert_equal v3 v 
      ~msg:"recommend using ML model"

let suite = "MLRecommender tests" >::: [
  "recommend_ml_no_videos" >:: test_recommend_ml_no_videos;
  "recommend_ml_all_watched" >:: test_recommend_ml_all_watched;
  "recommend_ml_cold_start" >:: test_recommend_ml_cold_start;
  "recommend_ml_one_interaction" >:: test_recommend_ml_one_interaction;
  "recommend_ml_with_history" >:: test_recommend_ml_with_history;
  "recommend_ml_filters_watched" >:: test_recommend_ml_filters_watched;
  "recommend_ml_many_interactions" >:: test_recommend_ml_many_interactions;
  "recommend_ml_single_unwatched" >:: test_recommend_ml_single_unwatched;
  "recommend_ml_exact_two_interactions" >:: test_recommend_ml_exact_two_interactions;
]

let _ = run_test_tt_main suite