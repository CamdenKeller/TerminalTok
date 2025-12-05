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
        for _ = 1 to 3 do
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

  let recommend_hybrid (user : user) (videos : video list) : video option =
    let embeddings = MLRecommender.init_embeddings () in
    (* Filter out already-watched videos *)
    let unwatched = List.filter (fun v -> not (has_watched user v)) videos in

    if unwatched = [] then
      (* If all videos watched, pick a random one to rewatch *)
      if videos = [] then None
      else Some (List.nth videos (Random.int (List.length videos)))
    else if List.length user.vid_history = 0 then
      Some (List.nth unwatched (Random.int (List.length unwatched)))
    else if List.length user.vid_history < 3 then
      let scored =
        List.map (fun v -> (v, content_based_score user v)) unwatched
      in
      let sorted = List.sort (fun (_, s1) (_, s2) -> compare s2 s1) scored in
      match sorted with
      | (v, _) :: _ -> Some v
      | [] -> None
    else begin
      (* Hybrid approach *)
      MLRecommender.train_on_history embeddings user;

      let scored =
        List.map
          (fun v ->
            let content_score = content_based_score user v in
            let collab_score = collaborative_score embeddings user v in
            let hybrid_score =
              (0.3 *. content_score) +. (0.7 *. collab_score)
            in
            (v, hybrid_score))
          unwatched
      in
      let sorted = List.sort (fun (_, s1) (_, s2) -> compare s2 s1) scored in
      match sorted with
      | (v, score) :: _ ->
          (* Commented out because it is printing to the user interface during chat *)
          (* Printf.printf "[Hybrid Score: %.3f]\n" score; *)
          Some v
      | [] -> None
    end
end
