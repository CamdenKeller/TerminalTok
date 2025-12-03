(** [parse_camels filename] parses a JSON file containing video data and returns a list of [video] records. *)
val parse_camels : string -> Types.video list
