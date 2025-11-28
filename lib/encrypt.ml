(* Diffie-Hellman Encryption Algorithm *)
open Z

(* large prime (public) *)
let p =
  Z.of_string
    "323170060713110073007148766886699519604441026697154840321303454275246551\n\
     109916869964572098063659042103561289451141903135125879172147227978625895\n\
     487996062548034500250628241981848775168233047708163507607030880515982534\n\
     197607984269819888751489009377198277230568661416366604510973014718009068\n\
     0664853948"

(* generator number *)
let g = Z.of_int 2

(* in the main.ml have each user randomly generate a private key *)

let get_public_value (private_key : Z.t) : Z.t = Z.powm g private_key p

let get_shared_secret (msngr_pub_val : Z.t) (private_key : Z.t) : Z.t =
  Z.powm msngr_pub_val (private_key : Z.t) p

(* let get_key (geshared_secret : z) : string??? = let encrypt_msg (msg :
   string) (key : string) : string = aes_encrypt *)
