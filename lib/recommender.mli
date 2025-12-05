open Types

(* ===== ML-Based Matrix Factorization Recommender ===== *)
module MLRecommender : sig
  type embeddings

  (** [init_embeddings ()] initializes the embeddings for users and videos with
      random small values. *)
  val init_embeddings : unit -> embeddings

  (** [predict_score embeddings user_name video_title] predicts the relevance
      score (0.0 to 1.0) for a given [user_name] and [video_title] based on the
      learned [embeddings]. *)
  val predict_score : embeddings -> string -> string -> float

  (** [recommend_ml embeddings user videos] recommends a video for the [user]
      from the list of [videos] using the ML model. Returns [Some video] if a
      recommendation is found, or [None] otherwise. *)
  val recommend_ml : embeddings -> user -> video list -> video option
end

(* ===== Hybrid Recommender (Content + ML) ===== *)
module HybridRecommender : sig
  (** [recommend_hybrid user videos] recommends a video for the [user] from the
      list of [videos] using a hybrid approach that combines content-based
      filtering (genres) and collaborative filtering (ML model). Returns [Some
      video] if a recommendation is found, or [None] otherwise. *)
  val recommend_hybrid : user -> video list -> video option
end
