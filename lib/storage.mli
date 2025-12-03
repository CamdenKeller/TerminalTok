(** [save_user user] saves the [user]'s data (history and genre counts) to CSV files. *)
val save_user : Types.user -> unit

(** [load_user name] loads the user data for [name] from CSV files.
    Returns [Some user] if found, [None] otherwise. *)
val load_user : string -> Types.user option

(** [load_video_list dir] returns a list of video filenames in the directory [dir]. *)
val load_video_list : string -> string list
