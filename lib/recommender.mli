open Types

(* ===== ML-Based Matrix Factorization Recommender ===== *)
module MLRecommender : sig
  type embeddings

  (** [init_embeddings ()] initializes the embeddings for users and videos with
      random small values. *)
  val init_embeddings : unit -> embeddings

  (** [get_user_embedding embeddings user_name] gets or creates an embedding
      vector for the given user. *)
  val get_user_embedding : embeddings -> string -> float array
  
  (** [predict_score embeddings user_name video_title] predicts the relevance
      score (0.0 to 1.0) for a given [user_name] and [video_title] based on the
      learned [embeddings]. *)
  val predict_score : embeddings -> string -> string -> float

  (** [dot_product v1 v2] calculates the scalar dot product of two vectors [v1]
      and [v2].
      Requires: [v1] and [v2] must have the same length. *)
  val dot_product : float array -> float array -> float

  (** [recommend_ml embeddings user videos] recommends a video for the [user]
      from the list of [videos] using the ML model. Returns [Some video] if a
      recommendation is found, or [None] otherwise. *)
  val recommend_ml : embeddings -> user -> video list -> video option
end

(* ===== Collaborative Filtering Recommender ===== *)
module CFRecommender : sig

  (** [get_all_users ?users_dir ()] returns a list of all users by reading user
      directories and loading their history and stats. 
      Optional [users_dir] defaults to "data/users". *)
  val get_all_users : ?users_dir:string -> unit -> user list

  (** [embed_user_list users] initializes a new ML model, trains it on the 
      history of every user in the [users] list, and returns an association 
      list mapping each user to their learned latent embedding vector. *)
  val embed_user_list : user list -> (user * float array) list

  (** [get_cf_scores_for_target user system_embeddings k] computes recommendation
      scores for a [user] by finding their [k] nearest neighbors within the 
      [system_embeddings]. It returns a list of videos watched by those neighbors 
      (excluding videos the target [user] has already seen), weighted by the 
      similarity to the neighbors. *)
  val get_cf_scores_for_target : user -> (user * float array) list -> int -> (video * float) list
end


(* ===== Hybrid Recommender (Content + ML) ===== *)
module HybridRecommender : sig
  (** [recommend_hybrid user videos] recommends a video for the [user] from the
      list of [videos] using a hybrid approach that combines content-based
      filtering (genres) and collaborative filtering (ML model). Returns [Some
      video] if a recommendation is found, or [None] otherwise. *)
  val recommend_hybrid : user -> video list -> (user * float array) list -> video option
end
