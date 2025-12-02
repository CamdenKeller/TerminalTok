open Lwt.Infix

let () = Random.self_init ()
let init_distance = 50

(* positions for character: pos1 is the default, pos2-pos8 are for flips*)
let pos1 = " O\n" ^ "   /|\\\n" ^ "   / \\"
let pos2 = "O_\n" ^ "   |\\_\n" ^ "     \\"
let pos3 = "O/__/\n" ^ "    \\  \\" (* maybe just 1 underscore? *)
let pos4 = " |_ \n" ^ "   |/_\n" ^ "   O"
let pos5 = "\\ / \n" ^ "   \\|/ \n" ^ "    O  "
let pos6 = "_|\n" ^ "    _\\|\n" ^ "      O"
let pos7 = "\\__\\\n" ^ "   /  /O"
let pos8 = "  _O\n" ^ "    _/|\n" ^ "     |"
let slide = " O_\n" ^ "    _\\ "
let fps = 0.05

(* 0.05 *)
(* make ref, increases over time (based on score, maybe just floor of score
   divided by something time constant for initial fps or smth *)
let input_flag = ref false

type action =
  | STAND
  | JUMP
  | FRONTFLIP
  | BACKFLIP
  | SLIDE

type input = {
  mutable until_input : int;
  mutable move : action;
}

let input_data = { until_input = 0; move = STAND }

(* let until_input = ref 0 *)
let score = ref 0
let max_obs = 10
let obstacles = Array.make max_obs (-1)
let obs = ref 0 (* number of obstacles *)
let last_67 = ref 0

exception Clarkson of string

let line = function
  | 7 | 1 -> 2
  | 6 | 2 -> 4
  | 5 | 3 -> 5
  | 4 -> 6
  | _ -> 0

(* maybe edit logic so 67 starts on nth character in line, and then add spaces
   based on that and also loc of person, num 67s, etc. *)
let move_obs () =
  for i = 0 to min !obs max_obs - 1 do
    obstacles.(i) <- obstacles.(i) - 1
  done

let obs_string () =
  let obs_line = ref "" in
  for i = 0 to !obs - 1 do
    let spaces =
      if i > 0 then obstacles.(i) - obstacles.(i - 1) else obstacles.(i)
    in
    obs_line := !obs_line ^ String.make (max 0 spaces) ' ' ^ "67"
  done;
  !obs_line

let output line pos =
  let third_line = if pos = pos3 || pos = pos7 || pos = slide then 1 else 0 in
  String.make 20 '\n' ^ "Score: " ^ string_of_int !score
  ^ String.make (10 - line + third_line) '\n'
  ^ "   " ^ pos ^ String.make line '\n'
  ^ if line = 0 then "" else "      "

let jump n =
  let line = line n in
  output line pos1

let slide () = output 0 slide

let backflip n =
  let line = line n in
  output line
    (match n with
    | 7 -> pos2
    | 6 -> pos3
    | 5 -> pos4
    | 4 -> pos5
    | 3 -> pos6
    | 2 -> pos7
    | 1 -> pos8
    | _ -> pos1)

let frontflip n =
  let line = line n in
  output line
    (match n with
    | 7 -> pos8
    | 6 -> pos7
    | 5 -> pos6
    | 4 -> pos5
    | 3 -> pos4
    | 2 -> pos3
    | 1 -> pos2
    | _ -> pos1)

(* must have the three spaces normal plus 3 for missing bob *)
(* also save high score for user at some point *)
(* needs logic for spaces (also note that username length matters; maybe only 
1st 3 chars, or have actual character in ascii). Basically when person is jumping
the 67 should be in the next spot as normal, and the 67 goes all the way to the 
back, and it's not just equals bc there is a range for the character *)
let wait_for_quiet () =
  let rec loop () =
    let ready, _, _ = Unix.select [ Unix.stdin ] [] [] 0.0 in
    if ready = [] then
      (* quiet moment: key released *)
      Lwt.return_unit
    else
      (* discard the repeat and keep waiting *)
      Lwt_io.read_char Lwt_io.stdin >>= fun _ -> Lwt_unix.sleep fps >>= loop
  in
  loop ()

let in_press = ref false

(* Terminal mode setup/restore *)
let original_termios = Unix.tcgetattr Unix.stdin

let enable_raw_mode () =
  let raw =
    {
      original_termios with
      Unix.c_icanon = false;
      (* disable canonical mode: no line buffering *)
      c_echo = false;
      (* disable echo: keys don't appear on screen *)
    }
  in
  Unix.tcsetattr Unix.stdin Unix.TCSANOW raw

let restore_terminal () =
  Unix.tcsetattr Unix.stdin Unix.TCSANOW original_termios

let rec print_loop () =
  Lwt.catch
    (fun () ->
      (* original print_loop body *)
      let had_input = !input_flag in
      input_flag := false;
      (* add ability for multiple 67s on the line *)
      (* also add 67s on above line(s)*)
      (* use array to store 67 loc vals bc there's a max amount there can be anyway *)
      (* (!obs = 0 || obstacles.(!obs) = init_distance-10) *)
      if obstacles.(0) = 0 then (
        obs := !obs - 1;
        for i = 0 to !obs do
          obstacles.(i) <- obstacles.(i + 1)
        done)
      else (* fix bc out of bounds *)
        move_obs ();
      let new_67 =
        !obs = 0
        || (obstacles.(!obs - 1) < init_distance - 13 && Random.int 10 = 1)
      in
      if new_67 then (
        obstacles.(!obs) <- init_distance - 1;
        (* information for new obstacle *)
        obs := !obs + 1)
      else ();
      score := !score + 1;
      let msg =
        (match input_data.move with
        | STAND -> jump input_data.until_input
        | JUMP -> jump input_data.until_input
        | FRONTFLIP -> frontflip input_data.until_input
        | BACKFLIP -> backflip input_data.until_input
        | SLIDE -> slide ())
        ^ obs_string ()
      in
      if (not had_input) && input_data.until_input = 0 then ()
      else if not had_input then
        input_data.until_input <- input_data.until_input - 1
      else input_data.until_input <- 7;
      (* 6 for jump*)
      Lwt_io.printl msg >>= fun () ->
      Lwt_unix.sleep fps >>= fun () ->
      if obstacles.(0) = 0 && input_data.until_input = 0 then
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

(* --- Input loop with cooldown + raw single-character reads --- *)

let cooldown = ref Lwt.return_unit (* resolves when input is allowed *)

let rec input_loop () =
  let do_jump () =
    input_flag := true;
    cooldown := Lwt_unix.sleep (7.0 *. fps);
    input_loop ()
  in
  (* Wait for cooldown to finish *)
  !cooldown >>= fun () ->
  (* Wait until keys are quiet *)
  wait_for_quiet () >>= fun () ->
  (* Read next input *)
  Lwt_io.read_char_opt Lwt_io.stdin >>= function
  (* ENTER: same behavior as before *)
  | Some '\n' | Some '\r' ->
      input_data.move <- JUMP;
      do_jump ()
  (* ESC sequence (possible arrow key) *)
  | Some '\027' -> (
      (* Try to read '[' next *)
      Lwt_io.read_char_opt Lwt_io.stdin
      >>= function
      | Some '[' -> (
          (* Third char determines which arrow *)
          Lwt_io.read_char_opt Lwt_io.stdin
          >>= function
          | Some 'A' ->
              (* Up arrow: treat like Enter *)
              input_data.move <- JUMP;
              do_jump ()
          | Some 'B' ->
              (* down *)
              if input_data.move = SLIDE then input_data.move <- STAND
              else input_data.move <- SLIDE;
              input_loop ()
          | Some 'C' ->
              (* right *)
              input_data.move <- FRONTFLIP;
              do_jump ()
          | Some 'D' ->
              (* left *)
              input_data.move <- BACKFLIP;
              do_jump ()
          | _ -> input_loop ())
      | _ ->
          (* ESC not starting an arrow sequence — ignore *)
          input_loop ())
  (* All other keys: ignore, same as before *)
  | Some _ -> input_loop ()
  | None -> input_loop ()

(* --- Program entry with terminal mode setup/cleanup --- *)
let run_dino () : unit Lwt.t =
  enable_raw_mode ();
  Lwt.finalize
    (fun () -> Lwt.join [ print_loop (); input_loop () ])
    (fun () ->
      restore_terminal ();
      Lwt.return_unit)
