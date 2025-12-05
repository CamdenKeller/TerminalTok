(** [video] represents a video with its metadata and ASCII content.
    Abstraction Function:
    A [video] represents a video file with a title, ascii art representation path, and genre.
    Representation Invariant:
    - [title] is not empty.
    - [ascii] is a valid path to an ascii file or empty string if not applicable.
    - [genre] is a valid genre string. *)
type video = {
  title : string; (** Title of the video *)
  ascii : string; (** ASCII art representation of the video *)
  genre : string; (** Genre of the video *)
}

(** [interaction] represents a user's interaction with a video.
    Abstraction Function:
    An [interaction] represents a user's interaction with a specific video, including watch time and like status.
    Representation Invariant:
    - [watchtime] >= 0.0. *)
type interaction = {
  video : video; (** The video interacted with *)
  mutable watchtime : float; (** Duration the video was watched in seconds *)
  mutable liked : bool; (** Whether the user liked the video *)
}

(** [user] represents a user of the application.
    Abstraction Function:
    A [user] represents a user of the system, tracking their viewing history and genre preferences.
    Representation Invariant:
    - [name] is not empty.
    - [vid_history] contains interactions with unique videos (no duplicate videos in history).
    - [genre_counts] maps genres to non-negative counts. *)
type user = {
  name : string; (** The user's username *)
  mutable vid_history : interaction list; (** List of past interactions *)
  mutable genre_counts : (string, int) Hashtbl.t; (** Count of watched videos per genre *)
}

(** [client] represents a connected client in the server.
    Abstraction Function:
    A [client] represents a connected client in the chat system.
    Representation Invariant:
    - [name] is not empty.
    - [cnt_in] and [cnt_out] are open channels. *)
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

(** [Server_not_started] is raised when a client tries to connect to a server that hasn't started. *)
exception Server_not_started
