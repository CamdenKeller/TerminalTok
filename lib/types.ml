(** Abstraction Function:
    A [video] represents a video file with a title, ascii art representation path, and genre.
    Representation Invariant:
    - [title] is not empty.
    - [ascii] is a valid path to an ascii file or empty string if not applicable.
    - [genre] is a valid genre string. *)
type video = {
  title : string;
  ascii : string;
  genre : string;
}

(** Abstraction Function:
    An [interaction] represents a user's interaction with a specific video, including watch time and like status.
    Representation Invariant:
    - [watchtime] >= 0.0. *)
type interaction = {
  video : video;
  mutable watchtime : float;
  mutable liked : bool;
}

(** Abstraction Function:
    A [user] represents a user of the system, tracking their viewing history and genre preferences.
    Representation Invariant:
    - [name] is not empty.
    - [vid_history] contains interactions with unique videos (no duplicate videos in history).
    - [genre_counts] maps genres to non-negative counts. *)
type user = {
  name : string;
  mutable vid_history : interaction list;
  mutable genre_counts : (string, int) Hashtbl.t;
}

(** Abstraction Function:
    A [client] represents a connected client in the chat system.
    Representation Invariant:
    - [name] is not empty.
    - [cnt_in] and [cnt_out] are open channels. *)
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

exception Server_not_started
