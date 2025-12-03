type video = {
  title : string;
  ascii : string;
  genre : string;
}

type interaction = {
  video : video;
  mutable watchtime : float;
  mutable liked : bool;
}

type user = {
  name : string;
  mutable vid_history : interaction list;
  mutable genre_counts : (string, int) Hashtbl.t;
}

(* Limited version of user for chat usage *)
type client = {
  (* id : string; *)
  name : string;
  mutable pub_key : string option;
  cnt_addr : string;
  cnt_in : Lwt_io.input_channel;
  cnt_out : Lwt_io.output_channel;
  mutable msg_addr : string option;
  mutable msg_in : Lwt_io.input_channel option;
  mutable msg_out : Lwt_io.output_channel option;
}
