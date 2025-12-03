open Types

(* ===== ML-Based Matrix Factorization Recommender ===== *)
module MLRecommender : sig
  type embeddings

  val init_embeddings : unit -> embeddings

  (* Predict the relevance score (0–1) of a user-video pair *)
  val predict_score : embeddings -> string -> string -> float

  (* Recommend using ML model *)
  val recommend_ml : embeddings -> user -> video list -> video option
end

(* ===== Hybrid Recommender (Content + ML) ===== *)
module HybridRecommender : sig
  val recommend_hybrid : user -> video list -> video option
end
