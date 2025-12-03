open Terminal_tok.Types
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
  Hashtbl.replace user.genre_counts g (old + 1);
  Storage.save_user user

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
      \  - Select Online mode to connect to the server and share Toks. You \
       must first run 'dune exec bin/server.exe' to start the server.\n\
       Once the session has begun...\n\
      \  - Enter 'L' to like/unlike\n\
      \  - Enter 'Q' to quit the session\n\
      \  - Enter 'C' to enter an encrypted chat with other users\n\
      \  - Enter anything else to go to the next Tok\n\n\
       (press ENTER to begin session)"
  in
  let%lwt () = Lwt_io.printl "Select mode: (1) Online (2) Offline" in
  let%lwt choice = Lwt_io.read_line Lwt_io.stdin in
  let online_mode =
    match choice with
    | "1" -> true
    | _ -> false
  in
  let start_session () =
    let%lwt () =
      Lwt_io.printl "Select playback mode: (1) Normal ASCII (2) Video ASCII"
    in
    let%lwt choice = Lwt_io.read_line Lwt_io.stdin in
    let video_mode =
      match choice with
      | "2" -> true
      | _ -> false
    in
    if video_mode then Lwt_io.printl "This feature is not yet implemented"
    else
      let%lwt () = Lwt_io.printl "Please enter your name here: " in

      let localhost_5000 = Unix.ADDR_INET (Unix.inet_addr_loopback, 5000) in
      let localhost_5001 = Unix.ADDR_INET (Unix.inet_addr_loopback, 5001) in

      (* Note: we need two pairs of in/out channels: one for multiple *)
      let%lwt cnt_server_in, cnt_server_out =
        Lwt_io.open_connection localhost_5000
      in
      let%lwt msg_server_in, msg_server_out =
        Lwt_io.open_connection localhost_5001
      in

      (* names function as unique keys*)
      let%lwt clients = Lwt_io.read_line cnt_server_in in

      (* let clients = Utils.format_names clients in *)
      let rec name_loop () =
        let%lwt name = Lwt_io.read_line Lwt_io.stdin in
        let%lwt () = Lwt_io.printl ("Other active users: " ^ clients) in
        let%lwt () = Lwt_unix.sleep 1.0 in
        if BatString.exists (clients ^ " ") (name ^ " ") then (
          print_endline "Name taken. Please try again";
          name_loop ())
        else if
          BatString.(exists name " " || exists name "[" || exists name "]")
          || name = ""
        then (
          print_endline
            "Please enter a non-empty name without spaces or brackets";
          name_loop ())
        else Lwt.return name
      in
      let%lwt name = name_loop () in
      let user =
        match Storage.load_user name with
        | Some u ->
            print_endline ("Welcome back, " ^ name ^ "!");
            u
        | None -> { name; vid_history = []; genre_counts = Hashtbl.create 10 }
      in
      let private_key = Encrypt.generate_private_key () in

      let pub_key = Encrypt.get_public_key private_key in

      (* here we must have the server write its shared public key so we can find
         the shared secret*)
      let%lwt server_pub_key = Lwt_io.read_line msg_server_in in

      let shared_secret =
        Encrypt.get_shared_secret (Z.of_string server_pub_key) private_key
      in
      let key = Encrypt.secret_to_key shared_secret in

      (* Write name and pub_key to servers *)
      let%lwt () = Lwt_io.write_line cnt_server_out name in
      let%lwt () = Lwt_io.flush cnt_server_out in

      let%lwt () = Lwt_io.write_line cnt_server_out (Z.to_string pub_key) in
      let%lwt () = Lwt_io.flush cnt_server_out in

      let%lwt () = Lwt_io.write_line msg_server_out name in
      let%lwt () = Lwt_io.flush msg_server_out in
      let videos = load_video_list "videos" in
      let ascii_lst = Json_parser.parse_camels "data/ascii.json" in
      let rec session_loop () =
        (* allows program to catch Cntrl C exit *)
        Sys.catch_break true;
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
                    Lwt_io.printl "No more videos :(" >>= fun () ->
                    Lwt.return_unit
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
                    (* close channels to signal to server *)
                    let%lwt () = Lwt_io.close cnt_server_out in
                    let%lwt () = Lwt_io.close cnt_server_in in
                    let%lwt () = Lwt_io.close msg_server_out in
                    let%lwt () = Lwt_io.close msg_server_in in
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
                | "C" ->
                    (* Signal to cnt server to return clients *)
                    let%lwt () = Lwt_io.write_line cnt_server_out "" in
                    let%lwt clients = Lwt_io.read_line cnt_server_in in
                    (* let clients = Utils.format_names clients in *)
                    let continue = ref true in

                    let%lwt () =
                      Lwt_io.printl
                        ("Active users: " ^ clients
                       ^ "\n\
                          Please enter the name of the person you would like \
                          to message")
                    in
                    let%lwt () =
                      Lwt_io.printl "Start chatting! Enter 'R' to go back"
                    in
                    (* try%lwt *)
                    let rec handle_message () =
                      (* generate the promises to be run *)
                      let server_prom =
                        (* on completion of server read it calls the map *)
                        let%lwt msg = Lwt_io.read_line msg_server_in in

                        let%lwt () = Lwt_io.printl ("\n" ^ msg ^ "\n") in
                        Lwt_io.print "Enter message: "
                      in
                      let input_prom =
                        let%lwt msg = Lwt_io.read_line Lwt_io.stdin in
                        if String.uppercase_ascii msg = "R" then (
                          continue := false;
                          Lwt.return_unit)
                        else
                          let encrpt_msg = Encrypt.encrypt_msg msg key in
                          print_endline ("Encrypted message: " ^ encrpt_msg);
                          let%lwt () =
                            Lwt_io.write_line msg_server_out encrpt_msg
                          in
                          Lwt_io.flush msg_server_out
                      in

                      let%lwt message = Lwt.pick [ input_prom; server_prom ] in

                      if !continue then handle_message () else Lwt.return_unit
                    in
                    let%lwt () = handle_message () in
                    session_loop ()
                | _ ->
                    add_to_history watch user;
                    session_loop ()
              in
              session_input ()
        with
        | Sys.Break ->
            let%lwt () = Lwt_io.close cnt_server_out in
            let%lwt () = Lwt_io.close cnt_server_in in
            let%lwt () = Lwt_io.close msg_server_out in
            Lwt_io.close msg_server_in
        | Failure msg ->
            let%lwt () =
              Lwt_io.printl ("An error occurred with this video:" ^ msg)
            in
            Lwt.return ()
      in

      session_loop ()
  in
  let start_dino () : unit Lwt.t =
    Lwt_io.printl
      "\n\
       Press ENTER to start the game.\n\
       Jump over the 67s and try to get the highest score you can!\n\
       UP ARROW (↑) or ENTER: Jump\n\
       DOWN ARROW (↓): Switch between standing position and sliding position\n\
       RIGHT ARROW (→): Front flip\n\
       LEFT ARROW (←): Backflip"
    >>= fun () ->
    Lwt_io.read_line_opt Lwt_io.stdin >>= fun _ -> Dinotok.run_dino ()
  in
  if online_mode then start_session () else start_dino ()

let _ =
  try Lwt_main.run (run ()) with
  | Sys.Break -> print_endline "Thank you for joining!"
  | _ -> print_endline "Failed for unknown reason"
