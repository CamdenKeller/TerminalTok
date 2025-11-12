open Terminal_tok
type video = {
  title : string;
  ascii : string;
  genre : string;
}

type user = {
  name : string;
  mutable vid_history : video * string * string list;
      (* feel free to expand here: you can add stuff like (video , watchtime,
         liked..??)*)
}
let run () : 'a Lwt.t =
  let user = 
  let%lwt () = Lwt_io.printl "Instructions \n press enter to continue" in
  let%lwt a = Lwt_io.read_line Lwt_io.stdin in
  let%lwt ascii = Recommender.recommend user in
  Lwt.return ()

let _ = Lwt_main.run (run ())
