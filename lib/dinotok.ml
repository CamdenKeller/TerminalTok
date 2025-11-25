open Lwt.Infix

let () = Random.self_init ()
let init_distance = 30
let name = "YOU"
let fps = 0.05
let input_flag = ref false
let until_input = ref 0
let score = ref 0
let until_67 = ref init_distance

exception Clarkson of string

let line_num n =
  match n with
  | 6 | 2 -> 3
  | 5 | 3 -> 5
  | 4 -> 6
  | _ -> 0

let jump n =
  let line = line_num n in
  String.make 13 '\n' ^ "Score: " ^ string_of_int !score
  ^ String.make (10 - line) '\n'
  ^ "   " ^ name ^ String.make line '\n'
  ^ if line = 0 then "" else "   "

(* terminal setup *)
let original_termios = Unix.tcgetattr Unix.stdin

let enable_raw_mode () =
  let raw = { original_termios with Unix.c_icanon = false; c_echo = false } in
  Unix.tcsetattr Unix.stdin Unix.TCSANOW raw

let restore_terminal () =
  Unix.tcsetattr Unix.stdin Unix.TCSANOW original_termios

let rec print_loop () =
  Lwt.catch
    (fun () ->
      let had_input = !input_flag in
      input_flag := false;

      let new_67 = !until_67 = init_distance && Random.int 10 = 0 in
      if new_67 then until_67 := init_distance - 1
      else if !until_67 <> init_distance then
        until_67 := if !until_67 = 0 then init_distance else !until_67 - 1;

      score := !score + 1;

      let msg =
        jump !until_input
        ^
        if !until_67 <> init_distance then String.make !until_67 ' ' ^ "67"
        else ""
      in

      if (not had_input) && !until_input = 0 then ()
      else if not had_input then until_input := !until_input - 1
      else until_input := 6;

      Lwt_io.printl msg >>= fun () ->
      Lwt_unix.sleep fps >>= fun () ->
      if !until_67 = 0 && !until_input = 0 then
        raise (Clarkson "Clarkson is coming for you");
      print_loop ())
    (function
      | Clarkson msg ->
          Lwt_unix.sleep 1.8 >>= fun () ->
          restore_terminal ();
          print_endline msg;
          exit 0
      | e ->
          restore_terminal ();
          raise e)

let cooldown = ref Lwt.return_unit

let rec input_loop () =
  !cooldown >>= fun () ->
  Lwt_io.read_char_opt Lwt_io.stdin >>= function
  | None -> input_loop ()
  | Some '\n' | Some '\r' ->
      input_flag := true;
      cooldown := Lwt_unix.sleep (6.0 *. fps);
      input_loop ()
  | Some _ -> input_loop ()

(* --------- WRAPPED AS A FUNCTION --------- *)

let run_dino () : unit Lwt.t =
  enable_raw_mode ();
  Lwt.finalize
    (fun () -> Lwt.join [ print_loop (); input_loop () ])
    (fun () ->
      restore_terminal ();
      Lwt.return_unit)
