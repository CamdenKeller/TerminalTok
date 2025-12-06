open Types

val string_of_addr : Unix.sockaddr -> string
(** [string_of_addr] provides a channel address as a string]*)

val format_clients : client list -> string
(** [format_clients clients] returns the list of clients a string]*)
