open Terminal_tok.Types
open Terminal_tok

let run () : 'a Lwt.t =
  let user = { name = ""; vid_history = [] } in
  let%lwt () = Lwt_io.printl "Instructions \n press enter to continue" in
  let%lwt a = Lwt_io.read_line Lwt_io.stdin in
  let%lwt ascii = Lwt.return (Recommender.recommend user).ascii in
  let%lwt () = Lwt_io.printl ascii in

  Lwt.return ()

let _ = Lwt_main.run (run ())
