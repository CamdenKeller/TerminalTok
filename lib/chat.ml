let clients : Types.user list ref = ref []

let client_handler client_addr (client_in, client_out) : unit Lwt.t =
  let%lwt name = Lwt_io.read_line client_in in
  let%lwt () = Lwt_io.flush client_out in
  Lwt.return_unit

let attempt_bind_listen sockadr =
  let server () =
    let%lwt running_server =
      Lwt_io.establish_server_with_client_address sockadr client_handler
    in
    let (never_resolved : unit Lwt.t), _unused_resolver = Lwt.wait () in
    never_resolved
  in
  Lwt_main.run (server ())

let run_client sockadr user =
  let client () =
    let%lwt server_in, server_out = Lwt_io.open_connection sockadr in
    let%lwt () = Lwt_io.printlf "I\n   connected to the server" in

    (* send username to the server *)
    (* let%lwt () = Lwt_io.write_line server_out username in
    let%lwt () = Lwt_io.flush server_out in *)
    let rec handle_message () =
      (* generate the promises to be run *)
      let server_prom =
        (* on completion of server read it calls the map *)
        let%lwt msg = Lwt_io.read_line server_in in
        let%lwt () = Lwt_io.printl ("\n" ^ msg ^ "\n") in
        Lwt_io.print "Enter message: "
      in
      let input_prom =
        let%lwt msg = Lwt_io.read_line Lwt_io.stdin in
        let%lwt () = Lwt_io.write_line server_out msg in
        Lwt_io.flush server_out
      in

      let%lwt message = Lwt.choose [ input_prom; server_prom ] in

      handle_message ()
    in
    try%lwt handle_message () with Failure msg -> Lwt_io.printlf "%s" msg
  in
  try Lwt_main.run (client ()) with Failure msg -> print_endline msg
