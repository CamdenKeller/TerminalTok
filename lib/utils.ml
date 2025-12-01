(** [format_names clients] formats names from the count server's repsonse.
    Clients names will not contain " " or "[" or "]"*)
let rec format_names (clients : string) : string =
  let result = ref "" in
  BatString.(
    try
      (* print_endline clients; *)
      let start_idx = find clients "[" in

      (* print_endline (string_of_int start_idx); *)
      let end_idx = find clients "]" in
      (* print_endline (string_of_int end_idx); *)

      result :=
        slice ~first:0 ~last:start_idx clients
        ^ slice ~first:(end_idx + 1) clients;
      if BatString.contains !result '[' then format_names !result
      else !result ^ " "
    with Not_found -> !result)

(* Note: this is potentially useless, unless one wants to enable bidirectional
   DF encryption *)
