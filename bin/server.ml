open Terminal_tok.Types
open Terminal_tok

let localhost_5000 = Unix.ADDR_INET (Unix.inet_addr_loopback, 5000)
let localhost_5001 = Unix.ADDR_INET (Unix.inet_addr_loopback, 5001)
let (all_clients : client list ref) = ref []
let num_clients = ref 0

(** [string_of_addr] provides a channel address as a string]*)
let string_of_addr = function
  | Unix.ADDR_UNIX s -> s
  | ADDR_INET (ip, port) ->
      Printf.sprintf "%s:%d" (Unix.string_of_inet_addr ip) port

(** [format_clients clients] returns the list of clients a string]*)
let format_clients (clients : client list) =
  let string = ref "" in
  List.iter (fun x -> string := !string ^ " " ^ x.name) (List.rev clients);
  !string

(* Writes list of clients to all clients*)
let write_all_clients_to_all =
  Lwt_list.iter_p
    (fun client ->
      let%lwt () =
        Lwt_io.write_line client.cnt_out (format_clients !all_clients)
      in
      Lwt_io.flush client.cnt_out)
    !all_clients

let run_counting_server sockadr () =
  (* Define server keys*)
  let%lwt () = Lwt_io.printl "Counting server started" in
  let client_handler client_addr (client_in, client_out) : unit Lwt.t =
    let%lwt () = Lwt_io.write_line client_out (format_clients !all_clients) in
    let%lwt () = Lwt_io.flush client_out in

    let address_string = string_of_addr client_addr in
    num_clients := !num_clients + 1;
    let%lwt name = Lwt_io.read_line client_in in
    let%lwt cl_pub_key = Lwt_io.read_line client_in in

    let client =
      {
        name;
        pub_key = None;
        cnt_addr = address_string;
        cnt_in = client_in;
        cnt_out = client_out;
        msg_addr = None;
        msg_in = None;
        msg_out = None;
      }
    in

    all_clients := client :: !all_clients;
    let rec handle_message () =
      (* Whenever someone joins, fill all channels with new list of clients*)
      let%lwt _ = Lwt_io.read_line client_in in
      let%lwt () =
        Lwt_list.iter_p
          (fun client ->
            let%lwt () =
              Lwt_io.write_line client.cnt_out (format_clients !all_clients)
            in
            Lwt_io.flush client.cnt_out)
          !all_clients
      in

      handle_message ()
    in
    try%lwt handle_message ()
    with _ ->
      (* Handle clients leaving *)
      let new_clients = List.filter (fun x -> x.name <> name) !all_clients in
      all_clients := new_clients;
      write_all_clients_to_all
  in
  let server () =
    let%lwt running_server =
      Lwt_io.establish_server_with_client_address sockadr client_handler
    in
    let (never_resolved : unit Lwt.t), _unused_resolver = Lwt.wait () in
    never_resolved
  in
  Lwt.return (server ())

let run_messaging_server sockadr () =
  let srv_priv_key = Encrypt.(generate_private_key ()) in
  let pub_key = Z.to_string Encrypt.(get_public_key srv_priv_key) in
  let client_handler client_addr (client_in, client_out) : unit Lwt.t =
    let%lwt () = Lwt_io.write_line client_out pub_key in
    let%lwt () = Lwt_io.flush client_out in

    (* update the client to have msg channel *)
    let%lwt name = Lwt_io.read_line client_in in
    
    let rec find_client_with_retry name retries =
      try
        let client = List.find (fun c -> c.name = name) !all_clients in
        Lwt.return client
      with Not_found ->
        if retries <= 0 then Lwt.fail Not_found
        else
          let%lwt () = Lwt_unix.sleep 0.1 in
          find_client_with_retry name (retries - 1)
    in
    
    let%lwt this_client = find_client_with_retry name 20 in
    this_client.msg_addr <- Some (string_of_addr client_addr);
    this_client.msg_in <- Some client_in;
    this_client.msg_out <- Some client_out;
    this_client.pub_key <- Some pub_key;
    let%lwt () =
      Lwt_list.iter_p
        (fun client ->
          match (client.msg_in, client.msg_out) with
          | Some msg_in, Some msg_out ->
              let%lwt () =
                Lwt_io.write_line msg_out (name ^ " has entered the chat.")
              in
              Lwt_io.flush msg_out
          | _ -> Lwt.return_unit)
        !all_clients
    in

    let rec receive_message () =
      let%lwt client_message = Lwt_io.read_line client_in in
      let%lwt () = Lwt_io.printl ("Encrypted msg: " ^ client_message) in

      match this_client.pub_key with
      | None -> failwith "Error with client key"
      | Some cl_pub_key ->
          let shared_secret =
            Encrypt.get_shared_secret (Z.of_string cl_pub_key) srv_priv_key
          in

          let key = Encrypt.secret_to_key shared_secret in

          let client_message = Encrypt.decrypt_msg client_message key in
          let%lwt () =
            Lwt_io.printlf "Message received from client '%s': %s" name
              client_message
          in
          let%lwt () =
            Lwt_list.iter_p
              (fun client ->
                match (client.msg_in, client.msg_out) with
                | Some msg_in, Some msg_out ->
                    let%lwt () =
                      Lwt_io.write_line msg_out
                        (name ^ " says " ^ client_message)
                    in
                    Lwt_io.flush msg_out
                | _ -> Lwt.return_unit)
              !all_clients
          in
          receive_message ()
    in
    try%lwt receive_message () with
    | End_of_file ->
        (* Handle clients leaving *)
        (* let%lwt () = format_client_names !all_clients in *)
        let%lwt () = Lwt_io.print "\n" in
        let%lwt () = Lwt_io.printl (string_of_addr client_addr) in

        let%lwt removed_client =
          (* filter out this client from the all_clients list if it catches a
             break *)
          Lwt.return (List.filter (fun x -> x.name = name) !all_clients)
        in
        let%lwt () =
          Lwt_list.iter_p (fun x -> Lwt_io.print (x.name ^ " ")) removed_client
        in
        let%lwt () =
          Lwt_io.printf
            "(%s) has left the chat. Reason of departure: End of file\n" name
        in

        let new_clients = List.filter (fun x -> x.name <> name) !all_clients in
        all_clients := new_clients;
        Lwt.return_unit
    | Failure msg ->
        Lwt_io.printf "%s (%s) has left the chat. Reason of departure: %s" name
          (string_of_addr client_addr)
          msg
    | _ ->
        Lwt_io.printf
          "%s (%s) has left the chat. Reason of departure: Unknown\n" name
          (string_of_addr client_addr)
  in
  let server () =
    let%lwt () = Lwt_io.printl "Messaging server started" in

    let%lwt running_server =
      Lwt_io.establish_server_with_client_address sockadr client_handler
    in
    let (never_resolved : unit Lwt.t), _unused_resolver = Lwt.wait () in
    never_resolved
  in
  Lwt.return (server ())

let _ =
  try
    Lwt_main.run
      (let%lwt _ = run_counting_server localhost_5000 () in
       let%lwt _ = run_messaging_server localhost_5001 () in
       let (never_resolved : unit Lwt.t), _unused_resolver = Lwt.wait () in
       never_resolved)
  with
  | Failure msg -> print_endline msg
  | _ -> print_endline "Server failed to start"
