open Types
open Yojson.Basic.Util

let parse_camels (json_file : string) : video list =
  let json = Yojson.Basic.from_file json_file in
  let camels_json = json |> member "camels" |> to_list in
  List.map
    (fun c ->
      {
        title = c |> member "name" |> to_string;
        genre = c |> member "genre" |> to_string;
        ascii = c |> member "ascii" |> to_string;
      })
    camels_json
