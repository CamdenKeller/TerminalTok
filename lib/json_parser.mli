(** [parse_camels filename] parses a JSON file containing video data and returns a list of [video] records. *)
val parse_camels : string -> Types.video list

(** [parse_videos filename] parses a JSON file containing video metadata and returns a list of (title, genre, file_path) tuples. *)
val parse_videos : string -> (string * string * string) list
