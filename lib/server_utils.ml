open Types

let string_of_addr = function
  | Unix.ADDR_UNIX s -> s
  | ADDR_INET (ip, port) ->
      Printf.sprintf "%s:%d" (Unix.string_of_inet_addr ip) port

let format_clients (clients : client list) =
  let string = ref "" in
  List.iter (fun x -> string := !string ^ " " ^ x.cnt_name) (List.rev clients);
  !string
