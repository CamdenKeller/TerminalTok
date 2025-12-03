open Types
open Csv

let data_dir = "data/users"

let get_user_dir name = Filename.concat data_dir name

let init_storage () =
  if not (Sys.file_exists "data") then Unix.mkdir "data" 0o755;
  if not (Sys.file_exists data_dir) then Unix.mkdir data_dir 0o755

let save_user (u : user) =
  init_storage ();
  let user_dir = get_user_dir u.name in
  if not (Sys.file_exists user_dir) then Unix.mkdir user_dir 0o755;

  (* Save history *)
  let history_file = Filename.concat user_dir "history.csv" in
  let history_data = List.map (fun (i : interaction) ->
    [i.video.title; i.video.genre; string_of_bool i.liked]
  ) u.vid_history in
  Csv.save history_file history_data;

  (* Save stats *)
  let stats_file = Filename.concat user_dir "stats.csv" in
  let stats_data = Hashtbl.fold (fun genre count acc ->
    [genre; string_of_int count] :: acc
  ) u.genre_counts [] in
  Csv.save stats_file stats_data

let load_user (name : string) : user option =
  let user_dir = get_user_dir name in
  if not (Sys.file_exists user_dir) then None
  else
    let history_file = Filename.concat user_dir "history.csv" in
    let stats_file = Filename.concat user_dir "stats.csv" in

    let vid_history =
      if Sys.file_exists history_file then
        let rows = Csv.load history_file in
        List.map (fun row ->
          match row with
          | [title; genre; liked_str] ->
              let video = { title; genre; ascii = "" } in (* Dummy ascii *)
              { video; liked = bool_of_string liked_str }
          | _ -> 
              (* Handle potential malformed rows gracefully or skip *)
              let video = { title = "Unknown"; genre = "Unknown"; ascii = "" } in
              { video; liked = false }
        ) rows
      else []
    in

    let genre_counts = Hashtbl.create 10 in
    if Sys.file_exists stats_file then (
      let rows = Csv.load stats_file in
      List.iter (fun row ->
        match row with
        | [genre; count_str] ->
            Hashtbl.add genre_counts genre (int_of_string count_str)
        | _ -> ()
      ) rows
    );

    Some { name; vid_history; genre_counts }
