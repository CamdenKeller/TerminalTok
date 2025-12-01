(* Diffie-Hellman Encryption Algorithm *)
open Cryptokit

(* Large prime number (public) *)
let p =
  Z.of_string
    ("323170060713110073007148766886699519604441026697154840321303454275246551"
   ^ "109916869964572098063659042103561289451141903135125879172147227978625895"
   ^ "487996062548034500250628241981848775168233047708163507607030880515982534"
   ^ "197607984269819888751489009377198277230568661416366604510973014718009068"
   ^ "0664853948")

(* generator number *)
let g = Z.of_int 2

let generate_private_key () : Z.t =
  (* let fill (buf : bytes) (pos : bits) (len : bits) = let cs =
     Mirage_crypto_rng.generate len in Cstruct.blit_from_bytes buf 0 cs pos len
     in Z.random_int_gen ~fill p *)

  (* as of right now, not very cryptographically secure*)
  Z.(random_int (p - of_int 2))

let get_public_key (private_key : Z.t) : Z.t = Z.powm g private_key p

let get_shared_secret (recp_pub_key : Z.t) (private_key : Z.t) : Z.t =
  Z.powm recp_pub_key (private_key : Z.t) p

(* Implemented using SHA256 hashing *)
let secret_to_key (secret : Z.t) =
  hash_string (Hash.sha256 ()) (Z.to_string secret)

(* in this case, our shared secret is the key *)

let encrypt_msg (msg : string) (key : string) : string = msg ^ key
(* let transform = Cryptokit.Cipher.aes key Cipher.Encrypt in transform_string
   transform msg *)

let decrypt_msg (msg : string) (key : string) : string =
  let last = String.length msg - String.length key in
  BatString.slice ~first:0 ~last msg
(* let transform = Cryptokit.Cipher.aes key Cipher.Decrypt in transform_string
   transform msg *)
