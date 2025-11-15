open Terminal_tok.Types
open Terminal_tok.Ascii_art
open Terminal_tok

(* TODO: resize window these values (for example)*)
(* let term_rows = 100 let term_cols = 100 *)
let index = ref 0

let add_to_history (vid : interaction) (user : user) =
  user.vid_history <- vid :: user.vid_history

(** [run ()] takes in a unit, starting the TerminalTok session, and returns the
    promise that, when fulfilled, ends the session. *)
let run () : 'a Lwt.t =
  let%lwt () =
    Lwt_io.printlf
      "Instructions:\n\
       Once the session has begun...\n\
      \  - Press SPACEBAR to move to next Tok\n\
      \  - Press 'L' to like\n\
      \  - Press 'Q' to quit the session\n\
      \      \n\
       (press ENTER to begin session)"
  in

  let%lwt _ = Lwt_io.read_line Lwt_io.stdin in
  let%lwt () = Lwt_io.printl "Please enter your name here: " in
  let%lwt name = Lwt_io.read_line Lwt_io.stdin in
  let user = { name; vid_history = [] } in
  let rec session_loop () =
    try%lwt
      let watch =
        { video = { title = "no name"; ascii = ""; genre = "" }; liked = false }
      in

      (* When we have corrent recommend logic implemented... let%lwt ascii =
         Lwt.return (Recommender.recommend user).ascii in *)
      let%lwt ascii' = Lwt.return (List.nth Ascii_art.ascii_lst !index) in
      let%lwt () = Lwt.return (incr index) in
      let%lwt () = Lwt_io.printl ascii' in

      let%lwt input = Lwt_io.read_line Lwt_io.stdin in

      match String.uppercase_ascii input with
      | "Q" ->
          add_to_history watch user;
          Lwt.return_unit
      | " " ->
          add_to_history watch user;
          session_loop ()
      | "L" ->
          let%lwt () = Lwt_io.printl "You have liked this video" in
          watch.liked <- true;
          add_to_history watch user;
          session_loop ()
      | _ -> session_loop ()
    with _ ->
      let%lwt () = Lwt_io.printl "No more videos :( " in
      Lwt.return_unit
  in

  session_loop ()

(* TODOS: - Make it so that the user can like the video without moving on
   (potentially put pattern match inside a helper function) - Added
   specifications - re(optional) OCaml doesn't have a built in read on first
   character, so find a way to do this using lower level functions in Unix
   library (also ChatGPT), and add feature to unlike videos as well *)

let _ = Lwt_main.run (run ())
