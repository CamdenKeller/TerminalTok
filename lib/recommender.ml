open Types

(* takes in a user and video list and recommends a video *)
let recommend (user : user) (videos : video list) : video option =

  if Hashtbl.length user.genre_counts = 0 then
    None
  else 
    let top_genre, _ = 
    Hashtbl.fold (fun genre count acc ->
      match acc with
      | None -> Some (genre, count)
      | Some (_, best_count) when count > best_count ->
        Some (genre, count)
    | _ -> acc
  ) user.genre_counts None
  |> Option.get
in
let candidates = List.filter (fun v -> v.genre = top_genre) videos in
match candidates with
    | [] -> None
    | hd :: _ -> Some hd
