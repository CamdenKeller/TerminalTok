open Terminal_tok.Ascii_art

let main () : unit Lwt.t =
  (* 1. Print the welcome message *)
  let%lwt () =
    Lwt_io.printl "Welcome! Lwt (with lwt_ppx) will control execution."
  in

  (* 2. Print the first piece of art *)
  let%lwt () = Lwt_io.printl Art.a in

  (* 3. Pause the execution for 2.0 seconds (non-blocking) *)
  let%lwt () = Lwt_io.printl "\n...pausing for 2 seconds...\n" in
  let%lwt () = Lwt_unix.sleep 2.0 in

  (* 4. Print the second piece of art *)
  let%lwt () = Lwt_io.printl "And now, the second piece:" in
  let%lwt () = Lwt_io.printl Art.i in

  (* 5. Finish and return *)
  Lwt_io.printl "Goodbye!"

let () = Lwt_main.run (main ())
