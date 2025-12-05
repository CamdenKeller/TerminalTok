open Types

(* takes in a user and video list and recommends a video *)

(* ML-Based Recommendation System using Matrix Factorization *)
module MLRecommender = struct
  (* Embedding dimension for matrix factorization *)
  let embedding_dim = 5

  (** Abstraction Function:
      [embeddings] represents the learned latent factors for users and videos in the matrix factorization model.
      - [user_embeddings] maps user names to their latent vectors.
      - [video_embeddings] maps video titles to their latent vectors.
      - [learning_rate] is the step size for gradient descent.
      - [lambda] is the regularization parameter.

      Representation Invariant:
      - [learning_rate] > 0.0.
      - [lambda] >= 0.0.
      - All arrays in [user_embeddings] and [video_embeddings] have length [embedding_dim]. *)
  type embeddings = {
    user_embeddings : (string, float array) Hashtbl.t;
        (* user_name -> embedding *)
    video_embeddings : (string, float array) Hashtbl.t;
        (* video_title -> embedding *)
    learning_rate : float;
    lambda : float; (* regularization parameter *)
  }

  (* Initialize embeddings with small random values *)
  let init_embeddings () : embeddings =
    {
      user_embeddings = Hashtbl.create 50;
      video_embeddings = Hashtbl.create 100;
      learning_rate = 0.01;
      lambda = 0.01;
    }

  (* Get or create embedding for user *)
  let get_user_embedding (embeddings : embeddings) (user_name : string) :
      float array =
    try Hashtbl.find embeddings.user_embeddings user_name
    with Not_found ->
      let embedding =
        Array.init embedding_dim (fun _ -> Random.float 0.1 -. 0.05)
      in
      Hashtbl.add embeddings.user_embeddings user_name embedding;
      embedding

  (* Get or create embedding for video *)
  let get_video_embedding (embeddings : embeddings) (video_title : string) :
      float array =
    try Hashtbl.find embeddings.video_embeddings video_title
    with Not_found ->
      let embedding =
        Array.init embedding_dim (fun _ -> Random.float 0.1 -. 0.05)
      in
      Hashtbl.add embeddings.video_embeddings video_title embedding;
      embedding

  (* Dot product of two vectors *)
  let dot_product (a : float array) (b : float array) : float =
    let sum = ref 0.0 in
    for i = 0 to Array.length a - 1 do
      sum := !sum +. (a.(i) *. b.(i))
    done;
    !sum

  (* Calculate relevance score (0-1) from interaction *)
  let interaction_to_score (interaction : interaction) : float =
    (* Like weight: 60% *)
    let like_score = if interaction.liked then 0.6 else 0.0 in

    (* Watch time weight: 40% Normalize by sigmoid function so longer watch =
       higher score Formula: 1 - exp(-watchtime/5) gives 0 to 1 curve #Lit
       math*)
    let watch_score =
      let normalized = 1.0 -. exp (-.interaction.watchtime /. 5.0) in
      normalized *. 0.4
    in

    like_score +. watch_score

  (* Train embeddings using gradient descent *)
  let train_step (embeddings : embeddings) (user_name : string)
      (interaction : interaction) : unit =
    let user_emb = get_user_embedding embeddings user_name in
    let video_emb = get_video_embedding embeddings interaction.video.title in

    (* Predicted score *)
    let predicted = dot_product user_emb video_emb in

    (* Actual score from interaction *)
    let actual = interaction_to_score interaction in

    (* Error *)
    let error = actual -. predicted in

    (* update embeddings using gradient descent with L2 regularization *)
    for i = 0 to embedding_dim - 1 do
      let user_grad =
        (error *. video_emb.(i)) -. (embeddings.lambda *. user_emb.(i))
      in
      let video_grad =
        (error *. user_emb.(i)) -. (embeddings.lambda *. video_emb.(i))
      in

      user_emb.(i) <- user_emb.(i) +. (embeddings.learning_rate *. user_grad);
      video_emb.(i) <- video_emb.(i) +. (embeddings.learning_rate *. video_grad)
    done

  (* Train on user's entire history *)
  let train_on_history (embeddings : embeddings) (user : user) : unit =
    List.iter
      (fun interaction ->
        (* Train multiple epochs on each interaction for better learning *)
        for _ = 1 to 20 do
          train_step embeddings user.name interaction
        done)
      user.vid_history

  (* Predict relevance score for user-video pair *)
  let predict_score (embeddings : embeddings) (user_name : string)
      (video_title : string) : float =
    let user_emb = get_user_embedding embeddings user_name in
    let video_emb = get_video_embedding embeddings video_title in
    let score = dot_product user_emb video_emb in
    1.0 /. (1.0 +. exp (-.score))
  (* sigmoid function for bound from 0 to 1*)

  let has_watched (user : user) (v : video) : bool =
    List.exists (fun inter -> inter.video.title = v.title) user.vid_history

  (* Recommend video using ML model *)
  let recommend_ml (embeddings : embeddings) (user : user) (videos : video list)
      : video option =
    let unwatched = List.filter (fun v -> not (has_watched user v)) videos in

    if unwatched = [] then None
    else if List.length user.vid_history < 2 then
      Some (List.nth unwatched (Random.int (List.length unwatched)))
    else begin
      (* Train on recent history *)
      train_on_history embeddings user;

      (* Score unwatched videos only *)
      let scored_videos =
        List.map
          (fun v ->
            let score = predict_score embeddings user.name v.title in
            (v, score))
          unwatched
      in

      (* Sort by score descending *)
      let sorted =
        List.sort (fun (_, s1) (_, s2) -> compare s2 s1) scored_videos
      in

      match sorted with
      | [] -> None
      | (best_video, score) :: _ ->
          Printf.printf "[ML Confidence: %.2f]\n" score;
          Some best_video
    end
end

module CFRecommender = struct
  
  (* Get all users from a directory and return them as a list *)

  (** [get_all_users ?users_dir ()] returns a list of all users by reading the
    user directories in [users_dir] (default: "data/users") and loading
    their history and stats.
    Requires: The users directory exists (defaults to "data/users").
    Returns: A list of users with their complete data loaded. *)
let get_all_users ?(users_dir = "data/users") () : user list =
  try
    let user_folders = Sys.readdir users_dir in
    Array.to_list user_folders
    |> List.filter (fun entry ->
         let path = Filename.concat users_dir entry in
         Sys.is_directory path)
    |> List.map (fun user_name ->
         let user_path = Filename.concat users_dir user_name in
         let history_file = Filename.concat user_path "history.csv" in
         let stats_file = Filename.concat user_path "stats.csv" in
         
         (* Load genre counts from stats.csv using Csv.load for robust parsing *)
         let genre_counts = Hashtbl.create 10 in
         (try
            if Sys.file_exists stats_file then
              let rows = Csv.load stats_file in
              let data_rows = match rows with _ :: rest -> rest | [] -> [] in
              List.iter
                (function
                  | genre :: count :: _ ->
                      (try Hashtbl.add genre_counts genre (int_of_string count)
                       with _ -> ())
                  | _ -> ())
                data_rows
          with _ -> ());

         (* Load video history from history.csv using Csv.load *)
         let vid_history =
           (try
              if Sys.file_exists history_file then
                let rows = Csv.load history_file in
                let data_rows = match rows with _ :: rest -> rest | [] -> [] in
                List.map
                  (fun row ->
                    match row with
                    | title :: genre :: liked_str :: watchtime_str :: _ ->
                        let video = { title; ascii = ""; genre } in
                        { video; 
                          watchtime = (try float_of_string watchtime_str with _ -> 0.0); 
                          liked = (try bool_of_string liked_str with _ -> false) }
                    | title :: genre :: liked_str :: _ ->
                        let video = { title; ascii = ""; genre } in
                        { video; watchtime = 0.0; liked = (try bool_of_string liked_str with _ -> false) }
                    | _ ->
                        let video = { title = "Unknown"; ascii = ""; genre = "Unknown" } in
                        { video; watchtime = 0.0; liked = false })
                  data_rows
              else []
            with _ -> [])
         in

         {
           name = user_name;
           vid_history = vid_history;
           genre_counts = genre_counts;
         })
  with Sys_error _ -> []

(* embed all users using previous function return list (user * embedding) *)

let embed_user_list user_list = 
  let embeddings = MLRecommender.init_embeddings () in
    
    (* Train embeddings on all users *)
    List.iter (fun (user : user) -> 
      MLRecommender.train_on_history embeddings user
    ) user_list;
    
    (* Get embeddings for each user *)
    List.map (fun (user : user) ->
      let embedding = MLRecommender.get_user_embedding embeddings user.name in
      (user, embedding)
    ) user_list


(* HELPER compare two embeddings function *)

let cosine_similarity (emb1 : float array) (emb2 : float array) : float =
  assert (Array.length emb1 = Array.length emb2);

  let dot = MLRecommender.dot_product emb1 emb2 in

  (* Maybe refactor this to helper at some point, probably when move dot_product *)
  let mag1 = ref 0.0 in
  for i = 0 to Array.length emb1 - 1 do
    mag1 := !mag1 +. (emb1.(i) *. emb1.(i))
  done;
  let mag1 = sqrt !mag1 in

  let mag2 = ref 0.0 in
  for i = 0 to Array.length emb2 - 1 do
    mag2 := !mag2 +. (emb2.(i) *. emb2.(i))
  done;
  let mag2 = sqrt !mag2 in

  if mag1 = 0.0 || mag2 = 0.0 then 0.0
  else dot /. (mag1 *. mag2)
  
(* Case 1: Identical vectors should have similarity 1.0 *)
let%test "test_cosine_identity" =
  let v1 = [| 3.0; 4.0; 5.0 |] in
  (* Similarity with self is always 1.0 *)
  abs_float (cosine_similarity v1 v1 -. 1.0) < 0.0001

(* Case 2: Orthogonal vectors (90 degrees) should have similarity 0.0 *)
let%test "test_cosine_orthogonal" =
  let v1 = [| 1.0; 0.0; 0.0 |] in
  let v2 = [| 0.0; 1.0; 0.0 |] in
  (* These are completely unrelated directions *)
  abs_float (cosine_similarity v1 v2) < 0.0001

(* Case 3: Scaled vectors should still have similarity 1.0 *)
let%test "test_cosine_scaled" =
  let v1 = [| 1.0; 1.0 |] in
  let v2 = [| 10.0; 10.0 |] in
  (* Magnitude is different, but direction is the same! *)
  abs_float (cosine_similarity v1 v2 -. 1.0) < 0.0001

(* compare selected user to every other distinct user return list of users and closeness score *)
let user_closeness_list selected_user_name selected_user_embed user_embed_list =
  let user_cosine_similarity = cosine_similarity selected_user_embed in
 user_embed_list
  (* 1. Filter out the user themselves *)
  |> List.filter (fun (user, _) -> user.name <> selected_user_name)
  (* 2. Map the remaining users to (user, score) *)
  |> List.map (fun (user, embed) -> (user, user_cosine_similarity embed))

(* TEST: User Closeness List *)
let%test "test_user_closeness_list_basic" =
  (* 1. Setup: Create dummy users *)
  let make_dummy name = 
    { name = name; vid_history = []; genre_counts = Hashtbl.create 1 } 
  in
  let user_a = make_dummy "User A" in
  let user_b = make_dummy "User B" in
  (* 2. Define Embeddings *)
  let target_emb = [| 1.0; 0.0 |] in 
  let emb_a = [| 1.0; 0.0 |] in 
  let emb_b = [| 0.0; 1.0 |] in
  (* 3. Create the input list *)
  let input_list = [(user_a, emb_a); (user_b, emb_b)] in
  (* 4. Run the function - FIXED: Added "Target User" name *)
  let results = user_closeness_list "Target User" target_emb input_list in
  (* 5. Verify the results *)
  match results with
  | [(u1, score1); (u2, score2)] ->
      let check_a = (u1.name = "User A") && (abs_float (score1 -. 1.0) < 0.0001) in
      let check_b = (u2.name = "User B") && (abs_float score2 < 0.0001) in
      check_a && check_b
  | _ -> false

(* get top k users *)
let get_top_k_users (closeness_list : (user * float) list) (k : int) : (user * float) list =
  let sorted_list = 
    List.sort (fun (_, s1) (_, s2) -> compare s2 s1) closeness_list 
  in
  let rec take n lst =
    if n <= 0 then [] 
    else match lst with
    | [] -> []
    | x :: xs -> x :: take (n - 1) xs
  in
  take k sorted_list


let%test "test_get_top_k" =
  let make_dummy name = 
    { name = name; vid_history = []; genre_counts = Hashtbl.create 1 } 
  in
  let user_low = make_dummy "Low Match" in
  let user_high = make_dummy "High Match" in
  let user_mid = make_dummy "Mid Match" in
  let input_list = [
    (user_low, 0.1);
    (user_high, 0.9); 
    (user_mid, 0.5)   
  ] in
  let result = get_top_k_users input_list 2 in
  match result with
  | [(u1, s1); (u2, s2)] ->
      let check_1 = (u1.name = "High Match") && (abs_float (s1 -. 0.9) < 0.0001) in
      let check_2 = (u2.name = "Mid Match") && (abs_float (s2 -. 0.5) < 0.0001) in
      check_1 && check_2
  | _ -> false

(* HELPER check if video already watched by selected user *)
let filter_out_seen (candidates : interaction list) (seen_history : interaction list) : interaction list =
  List.filter (fun cand ->
    not (List.exists (fun seen -> seen.video.title = cand.video.title) seen_history)
  ) candidates

(* TEST: Filter Seen Videos - No duplicates allowed on this ship! *)
let%test "test_filter_seen" =
  let make_inter title = 
    let v = { title = title; ascii = ""; genre = "Action" } in
    { video = v; watchtime = 10.0; liked = true } 
  in
  let vid_A = make_inter "Video A" in
  let vid_B = make_inter "Video B" in
  let vid_C = make_inter "Video C" in

  let candidates = [vid_A; vid_B; vid_C] in
  let history = [vid_A; vid_C] in
  let result = filter_out_seen candidates history in
  match result with
  | [res] -> res.video.title = "Video B"
  | _ -> false

(* HELPER get all videos (use helper above) from list of users. returns Hash table key video - list (closeness score * ineraction) *)
let get_all_videos_from_users (similar_users : (user * float) list) (current_user_history : interaction list) =
  let video_table = Hashtbl.create 50 in
  List.iter (fun (other_user, closeness_score) ->
    let unseen_interactions = 
      filter_out_seen other_user.vid_history current_user_history 
    in
    List.iter (fun interaction ->
      let video_key = interaction.video in
      let current_list = 
        try Hashtbl.find video_table video_key 
        with Not_found -> [] 
      in
      Hashtbl.replace video_table video_key ((closeness_score, interaction) :: current_list)
    ) unseen_interactions
  ) similar_users;
  video_table

let%test "test_aggregate_videos" =
  let vid_x = { title = "Video X"; ascii = ""; genre = "Comedy" } in
  let vid_y = { title = "Video Y"; ascii = ""; genre = "Action" } in
  
  let inter_a_x = { video = vid_x; watchtime = 10.0; liked = true } in
  let inter_b_x = { video = vid_x; watchtime = 20.0; liked = true } in
  let inter_b_y = { video = vid_y; watchtime = 30.0; liked = true } in

  let user_a = { name = "User A"; vid_history = [inter_a_x]; genre_counts = Hashtbl.create 1 } in
  let user_b = { name = "User B"; vid_history = [inter_b_x; inter_b_y]; genre_counts = Hashtbl.create 1 } in

  let similar_users = [(user_a, 0.9); (user_b, 0.5)] in
  let my_history = [] in

  let table = get_all_videos_from_users similar_users my_history in

  let list_x = Hashtbl.find table vid_x in
  let list_y = Hashtbl.find table vid_y in

  let check_len = (List.length list_x = 2) && (List.length list_y = 1) in

  let has_score_9 = List.exists (fun (s, _) -> abs_float (s -. 0.9) < 0.0001) list_x in
  let has_score_5 = List.exists (fun (s, _) -> abs_float (s -. 0.5) < 0.0001) list_x in

  check_len && has_score_9 && has_score_5

  (* we account for their basically being none? *)
(* HELPER to calculate score from list of ( interactions) *)
let calculate_video_score (interactions : (float * interaction) list) : float =
  List.fold_left (fun acc (closeness, interaction) ->
    let inter_score = MLRecommender.interaction_to_score interaction in
    let weighted_score = closeness *. inter_score in
    acc +. weighted_score
  ) 0.0 interactions

let%test "test_calculate_score" =
  let inter_a = { 
    video = { title = "Vid"; ascii=""; genre="" }; 
    liked = true; 
    watchtime = 100.0 
  } in
  let inter_b = { 
    video = { title = "Vid"; ascii=""; genre="" }; 
    liked = false; 
    watchtime = 0.0 
  } in

  let input_list = [
    (1.0, inter_a); 
    (0.5, inter_b)
  ] in

  let total_score = calculate_video_score input_list in
  abs_float (total_score -. 1.0) < 0.01

(* HELPER go through hash table calculate list of tuples (video * score) *)
let score_videos (video_table : (video, (float * interaction) list) Hashtbl.t) : (video * float) list =
  Hashtbl.fold (fun video interactions acc ->
    let total_score = calculate_video_score interactions in
    (video, total_score) :: acc
  ) video_table []

let%test "test_score_videos_basic" =
  let table = Hashtbl.create 5 in
  let vid_high = { title = "High Score Video"; ascii=""; genre="Genre A" } in
  let vid_low = { title = "Low Score Video"; ascii=""; genre="Genre B" } in
  let inter_high = { video = vid_high; liked = true; watchtime = 100.0 } in
  Hashtbl.add table vid_high [(1.0, inter_high)];
  let inter_low = { video = vid_low; liked = false; watchtime = 0.0 } in
  Hashtbl.add table vid_low [(1.0, inter_low)];
  let results = score_videos table in

  try
    let high_score = List.assoc vid_high results in
    let low_score = List.assoc vid_low results in
    (abs_float (high_score -. 1.0) < 0.01) && (abs_float low_score < 0.01)
  with Not_found -> false

(* Get top k videos from collaborative filtering *)
let recommend_cf ?(users_dir="data/users") (target_user : user) (k_neighbors : int) : video option =
    
    let all_users = get_all_users ~users_dir () in
    let all_users_with_target = 
      if List.exists (fun u -> u.name = target_user.name) all_users 
      then all_users 
      else target_user :: all_users 
    in
    let user_embeddings = embed_user_list all_users_with_target in

    match List.find_opt (fun (u, _) -> u.name = target_user.name) user_embeddings with
    | None -> None
    | Some (_, target_emb) ->
        let closeness_list = 
          user_closeness_list target_user.name target_emb user_embeddings 
        in
        let top_neighbors = get_top_k_users closeness_list k_neighbors in
        let video_table = 
          get_all_videos_from_users top_neighbors target_user.vid_history 
        in
        let unsorted_scored_videos = score_videos video_table in

        let final_ranking = 
          List.sort (fun (_, s1) (_, s2) -> compare s2 s1) unsorted_scored_videos 
        in

        match final_ranking with
        | [] -> None
        | (best_video, score) :: _ -> 
            Printf.printf "[CF Confidence: %.2f] Recommended: %s\n" score best_video.title;
            Some best_video

let get_cf_scores_for_target (target_user : user) (system_embeddings : (user * float array) list) (k : int) : (video * float) list =
  (* 1. Find Target Embedding *)
  match List.find_opt (fun (u, _) -> u.name = target_user.name) system_embeddings with
  | None -> []
  | Some (_, target_emb) ->
      (* 2. Calculate Closeness *)
      let closeness = user_closeness_list target_user.name target_emb system_embeddings in
      (* 3. Filter Positive Only (Safety check against enemies!) *)
      let positive_closeness = List.filter (fun (_, s) -> s > 0.0) closeness in
      (* 4. Get Top Neighbors *)
      let neighbors = get_top_k_users positive_closeness k in
      (* 5. Aggregate Videos *)
      let video_table = get_all_videos_from_users neighbors target_user.vid_history in
      (* 6. Score Videos *)
      score_videos video_table
end

module HybridRecommender = struct
  (* Start the model with just recommending based on Genre Based signals*)
  let content_based_score (user : user) (video : video) : float =
    try
      let genre_count =
        float_of_int (Hashtbl.find user.genre_counts video.genre)
      in
      genre_count /. float_of_int (max 1 (List.length user.vid_history))
    with Not_found -> 0.0

  (* Collaborative filtering via ML *)
  let collaborative_score (embeddings : MLRecommender.embeddings) (user : user)
      (video : video) : float =
    if List.length user.vid_history < 2 then 0.0
    else MLRecommender.predict_score embeddings user.name video.title

  let has_watched (user : user) (v : video) : bool =
    List.exists (fun inter -> inter.video.title = v.title) user.vid_history

  let recommend_hybrid (user : user) (videos : video list) (system_embeddings : (user * float array) list) : video option =
    
    (* Filter out videos the user has already watched *)
    let unwatched = List.filter (fun v -> not (has_watched user v)) videos in

    (* Handle edge cases: No videos left, or Cold Start (New User) *)
    if unwatched = [] then None
    else if List.length user.vid_history = 0 then
       (* Cold Start: Just pick a random video if we know nothing about the user *)
       Some (List.nth unwatched (Random.int (List.length unwatched)))
    else begin
      
      let cf_results = 
        CFRecommender.get_cf_scores_for_target user system_embeddings 5 
      in
      
      (* Convert that list into a Hash Map so we can look it up instantly (O(1)) *)
      let cf_score_map = Hashtbl.create 10 in
      List.iter (fun (v, s) -> Hashtbl.replace cf_score_map v.title s) cf_results;

      let ml_model = MLRecommender.init_embeddings () in
      MLRecommender.train_on_history ml_model user;
      
      let scored =
        List.map
          (fun v ->
            (* Signal A: Content Score (Existing Logic) *)
            let content_s = content_based_score user v in
            
            (* Signal B: CF Score (Lookup from our new map) *)
            (* If not found, it means neighbors didn't watch/like it, so score is 0.0 *)
            let cf_s = try Hashtbl.find cf_score_map v.title with Not_found -> 0.0 in

            (* Signal C: ML Score (Calculate using the new local model) *)
            (* Ask the trained model: "How much does this user match this video?" *)
            let ml_s = MLRecommender.predict_score ml_model user.name v.title in
            
            (* 30% Genre, 35% Neighbors, 35% Personal ML Predictions *)
            let hybrid_score =
              (0.3 *. content_s) +. (0.35 *. cf_s) +. (0.35 *. ml_s)
            in
            
            (* Return the video paired with its final score *)
            (v, hybrid_score))
          unwatched
      in
      
      (* Sort by the Hybrid Score (High to Low) *)
      let sorted = List.sort (fun (_, s1) (_, s2) -> compare s2 s1) scored in
      
      (* Return the top result *)
      match sorted with
      | (v, _) :: _ -> Some v
      | [] -> None
    end
end
