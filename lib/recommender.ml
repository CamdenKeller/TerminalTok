open Types

(* takes in a user and video list and recommends a video *)
let recommend (user : user) (videos : video list) : video option =
  if Hashtbl.length user.genre_counts = 0 then
    if videos = [] then None
    else Some (List.nth videos (Random.int (List.length videos)))
  else
    let top_genre =
      Hashtbl.fold
        (fun genre count best ->
          match best with
          | None -> Some (genre, count)
          | Some (_, best_count) ->
              if count > best_count then Some (genre, count) else best)
        user.genre_counts None
      |> Option.get |> fst
    in
    let candidates = List.filter (fun v -> v.genre = top_genre) videos in
    match candidates with
    | [] -> None
    | _ ->
        let idx = Random.int (List.length candidates) in
        Some (List.nth candidates idx)
