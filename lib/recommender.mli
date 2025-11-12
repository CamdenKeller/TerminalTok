type video = {
  title : string;
  ascii : string;
  genre : string;
}

type user = {
  name : string;
  mutable vid_history : video * string * string list;
      (* feel free to expand here: you can add stuff like (video , watchtime,
         liked..??)*)
}

(* takes in a user and recommends a video *)
val recommend : user -> video
