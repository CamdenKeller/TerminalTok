(** [video] represents a video with its metadata and ASCII content. *)
type video = {
  title : string; (** Title of the video *)
  ascii : string; (** ASCII art representation of the video *)
  genre : string; (** Genre of the video *)
}

(** [interaction] represents a user's interaction with a video. *)
type interaction = {
  video : video; (** The video interacted with *)
  mutable watchtime : float; (** Duration the video was watched in seconds *)
  mutable liked : bool; (** Whether the user liked the video *)
}

(** [user] represents a user of the application. *)
type user = {
  name : string; (** The user's username *)
  mutable vid_history : interaction list; (** List of past interactions *)
  mutable genre_counts : (string, int) Hashtbl.t; (** Count of watched videos per genre *)
}

(** [client] represents a connected client in the server. *)
type client = {
  name : string; (** The client's username *)
  mutable pub_key : string option; (** The client's public key for encryption *)
  cnt_addr : string; (** Address of the counting server connection *)
  cnt_in : Lwt_io.input_channel; (** Input channel for counting server *)
  cnt_out : Lwt_io.output_channel; (** Output channel for counting server *)
  mutable msg_addr : string option; (** Address of the messaging server connection *)
  mutable msg_in : Lwt_io.input_channel option; (** Input channel for messaging server *)
  mutable msg_out : Lwt_io.output_channel option; (** Output channel for messaging server *)
}
