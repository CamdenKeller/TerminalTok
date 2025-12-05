open Types

let string_of_addr = function
  | Unix.ADDR_UNIX s -> s
  | ADDR_INET (ip, port) ->
      Printf.sprintf "%s:%d" (Unix.string_of_inet_addr ip) port

let format_clients (clients : client list) =
  let string = ref "" in
  List.iter (fun x -> string := !string ^ " " ^ x.cnt_name) (List.rev clients);
  !string

let write_clients_to_all all_clients =
  Lwt_list.iter_p
    (fun client ->
      let%lwt () =
        Lwt_io.write_line client.cnt_out (format_clients !all_clients)
      in
      Lwt_io.flush client.cnt_out)
    !all_clients
