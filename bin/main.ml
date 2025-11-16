open Terminal_tok.Types
open Terminal_tok.Ascii_art
open Terminal_tok.Recommender
open Terminal_tok.Json_parser
open Terminal_tok
open Lwt
open Lwt_process

(* Track user history *)
let add_to_history (inter : interaction) (user : user) =
  user.vid_history <- inter :: user.vid_history;

  let g = inter.video.genre in
  let old = try Hashtbl.find user.genre_counts g with Not_found -> 0 in
  Hashtbl.replace user.genre_counts g (old + 1)

(* Play a video using mpv in ASCII mode *)
let play_ascii_video (file : string) : unit Lwt.t =
  let command = ("mpv", [| "mpv"; "--vo=tct"; file |]) in
  let process = Lwt_process.open_process_none command in
  process#status >|= fun _ -> ()

(* Load all video files from a folder *)
let load_video_list (folder : string) : string list =
  Sys.readdir folder |> Array.to_list
  |> List.filter (fun f ->
      Filename.check_suffix f ".mp4"
      || Filename.check_suffix f ".mov"
      || Filename.check_suffix f ".mkv")
  |> List.map (Filename.concat folder)

(** [run ()] starts the TerminalTok session *)
let run () : unit Lwt.t =
  let video_index = ref 0 in

  let%lwt () =
    Lwt_io.printlf
      "Instructions:\n\
       Once the session has begun...\n\
      \  - Enter 'L' to like/unlike\n\
      \  - Enter 'Q' to quit the session\n\
       |  - Enter anything else to go to the next Tok\n\n\
       (press ENTER to begin session)"
  in

  let%lwt () =
    Lwt_io.printl "Select playback mode: (1) Normal ASCII (2) Video ASCII"
  in
  let%lwt choice = Lwt_io.read_line Lwt_io.stdin in
  let video_mode =
    match choice with
    | "2" -> true
    | _ -> false
  in
  let%lwt () = Lwt_io.printl "Please enter your name here: " in
  let%lwt name = Lwt_io.read_line Lwt_io.stdin in
  let user = { name; vid_history = []; genre_counts = Hashtbl.create 10 } in

  let videos = load_video_list "videos" in

  let ascii_lst =
    Json_parser.parse_camels "../data/ascii.json"
    (* List.map
      (fun x -> { title = "camel"; ascii = x; genre = "camel" })
      Ascii_art.ascii_lst *)
    (* TODO for ruslan- LOAD from a data directory - *)
  in
  let rec session_loop () =
    try%lwt
      let video = Recommender.recommend user ascii_lst in

      match video with
      | None ->
          let%lwt () = Lwt_io.printl "No videos; program ended" in
          Lwt.return_unit
      | Some v ->
          let watch = { video = v; liked = false } in

          (* Play either video or ASCII art *)
          let%lwt () =
            (* handles playing videos *)
            if video_mode then (
              if !video_index >= List.length videos then
                Lwt_io.printl "No more videos :(" >>= fun () -> Lwt.return_unit
              else
                let video_file = List.nth videos !video_index in
                incr video_index;
                play_ascii_video video_file)
            (* handles displaying ascii *)
              else Lwt_io.printl v.ascii
          in

          (* Handle user input for this "Tok" *)
          let rec session_input () =
            let%lwt input = Lwt_io.read_line Lwt_io.stdin in
            match String.uppercase_ascii input with
            | "Q" ->
                add_to_history watch user;
                Lwt.return_unit
            | "L" ->
                watch.liked <- not watch.liked;
                let%lwt () =
                  Lwt_io.printl
                    ("You have "
                    ^ (if watch.liked then "" else "un")
                    ^ "liked this video")
                in
                add_to_history watch user;
                session_input ()
            | _ ->
                add_to_history watch user;
                session_loop ()
          in
          session_input ()
    with _ ->
      let%lwt () = Lwt_io.printl "An error occurred with this video" in
      Lwt.return_unit
  in

  session_loop ()

let _ = Lwt_main.run (run ())
