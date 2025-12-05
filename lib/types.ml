type video = {
  title : string;
  ascii : string;
  genre : string;
}
(** Abstraction Function: A [video] represents a video file with a title, ascii
    art representation path, and genre. Representation Invariant:
    - [title] is not empty.
    - [ascii] is a valid path to an ascii file or empty string if not
      applicable.
    - [genre] is a valid genre string. *)

type interaction = {
  video : video;
  mutable watchtime : float;
  mutable liked : bool;
}
(** Abstraction Function: An [interaction] represents a user's interaction with
    a specific video, including watch time and like status. Representation
    Invariant:
    - [watchtime] >= 0.0. *)

type user = {
  name : string;
  mutable vid_history : interaction list;
  mutable genre_counts : (string, int) Hashtbl.t;
}
(** Abstraction Function: A [user] represents a user of the system, tracking
    their viewing history and genre preferences. Representation Invariant:
    - [name] is not empty.
    - [vid_history] contains interactions with unique videos (no duplicate
      videos in history).
    - [genre_counts] maps genres to non-negative counts. *)

(* Limited version of user for chat usage *)
type client = {
  cnt_name : string;
  mutable pub_key : string option;
  cnt_addr : string;
  cnt_in : Lwt_io.input_channel;
  cnt_out : Lwt_io.output_channel;
  mutable msg_addr : string option;
  mutable msg_in : Lwt_io.input_channel option;
  mutable msg_out : Lwt_io.output_channel option;
}
(** Abstraction Function: A [client] represents a connected client in the chat
    system. Representation Invariant:
    - [cnt_name] is not empty.
    - [cnt_addr] is a valid IP address.
    - [cnt_in] and [cnt_out] are open channels.
    - [msg_addr] is either None or a valid IP address.
    - [msg_in] and [msg_out] are open channels. *)

exception Server_not_started
(** [Server_not_started] is thrown when the server is not started. *)
