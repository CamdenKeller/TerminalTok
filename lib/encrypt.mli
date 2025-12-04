val generate_private_key : unit -> Z.t
(** [generate_private_key ()] generates a random private key for Diffie-Hellman
    key exchange. *)

val get_public_key : Z.t -> Z.t
(** [get_public_key private_key] calculates the public key corresponding to the
    given [private_key]. *)

val get_shared_secret : Z.t -> Z.t -> Z.t
(** [get_shared_secret other_public_key my_private_key] computes the shared
    secret using the other party's public key and my private key. *)

val secret_to_key : Z.t -> string
(** [secret_to_key secret] derives a string key from the shared [secret] (using
    SHA256). *)

val encrypt_msg : string -> string -> string
(** [encrypt_msg msg key] encrypts [msg] using [key]. *)

val decrypt_msg : string -> string -> string
(** [decrypt_msg msg key] decrypts [msg] using [key]. *)
