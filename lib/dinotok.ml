open Lwt.Infix

let () = Random.self_init ()
let init_distance = 50

(* positions for character: pos1 is the default, pos2-pos8 are for flips*)

let pos1 = [ " O "; "   /|\\"; "   / \\" ]
let pos2 = [ "O_"; "   |\\_"; "     \\" ]
let pos3 = [ "O/__/"; "    \\  \\" ]
let pos4 = [ " |_ "; "   |/_"; "   O" ]
let pos5 = [ "\\ / "; "   \\|/ "; "    O  " ]
let pos6 = [ "_|"; "    _\\|"; "      O" ]
let pos7 = [ "\\__\\"; "   /  /O" ]
let pos8 = [ "  _O"; "    _/|"; "     |" ]
let slide = [ " O_"; "    _\\ " ]
let fps = ref 0.05

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
(* make array with record containing info about obstacles so maybe in air *)
(* this is test file so just go for it*)
type obstacle = {
  mutable loc : int;
  height : int;
}

let score = ref 0
let max_obs = 10
let obstacles = Array.init max_obs (fun _ -> { loc = -1; height = 0 })
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
    obstacles.(i).loc <- obstacles.(i).loc - 1
  done

let rec search j h =
  if j < 0 then None
  else if obstacles.(j).height = h then Some j
  else search (j - 1) h

let obs_string h =
  let obs_line = ref "" in
  for i = 0 to !obs - 1 do
    if obstacles.(i).height = h then begin
      (* find previous obstacle with height = 0 *)
      let prev = search (i - 1) h in
      let spaces =
        match prev with
        | None -> obstacles.(i).loc
        | Some j -> obstacles.(i).loc - obstacles.(j).loc - 2
        (* was one but case for first ob.loc = 0 was wrong bc didnt move just
           shifted``````````````````````````````````````````````````````````````````````````````````*)
      in
      obs_line := !obs_line ^ String.make (max 0 spaces) ' ' ^ "67"
    end
  done;
  !obs_line

let join_lines lines = String.concat "\n" lines

let stand_high lines =
  match lines with
  | [ l1; l2; l3 ] -> l1 ^ obs_string 2 ^ "\n" ^ l2 ^ "\n" ^ l3
  | _ -> invalid_arg "join_lines expects exactly 3 strings"

let output line pos =
  let third_line = if pos = pos3 || pos = pos7 || pos = slide then 1 else 0 in
  let no_high = search 9 2 = None in
  (* no high obstacles *)
  String.make 20 '\n' ^ "Score: " ^ string_of_int !score
  ^ "\t\t\t High Score: 0"
  ^
  if no_high || line >= 2 then
    String.make (10 - line + third_line) '\n'
    ^ "   " ^ join_lines pos
    ^ (if no_high then String.make line '\n'
       else
         String.make (line - 2) '\n'
         ^ (if line = 2 then ""
            else "      " (* compact so no_high condition is here or smth *))
         ^ obs_string 2 ^ String.make 2 '\n')
    ^ if line = 0 then "" else "      "
  else (* line=0 bc its never 1; ppos is either pos1 (stand) or slide *)
    (* String.make (10 - line + third_line) '\n' *)
    String.make 10 '\n'
    ^
    if pos = slide then "      " ^ obs_string 2 ^ "\n" ^ "   " ^ join_lines pos
    else "   " ^ stand_high pos
(* 1st string of pos then string of rest *)
(* basically add n to output func, if n≥2 then just add obstacles at h=2, else
   do string by string from pos *)

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
      Lwt_io.read_char Lwt_io.stdin >>= fun _ -> Lwt_unix.sleep !fps >>= loop
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
      if !fps > 0.03 && !score mod 100 = 0 then fps := !fps -. 0.001 else ();
      if obstacles.(0).loc = 0 then (
        obs := !obs - 1;
        for i = 0 to !obs do
          obstacles.(i) <-
            { (obstacles.(i + 1)) with loc = obstacles.(i + 1).loc - 1 }
        done)
      else (* fix bc out of bounds *)
        move_obs ();
      let new_67 =
        !obs = 0
        || (obstacles.(!obs - 1).loc < init_distance - 13 && Random.int 10 = 1)
      in
      if new_67 then (
        let level =
          match Random.int 3 with
          | 2 -> 2
          | _ -> 0
        in
        obstacles.(!obs) <- { loc = init_distance - 1; height = level };
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
        ^ obs_string 0
      in
      if (not had_input) && input_data.until_input = 0 then ()
      else if not had_input then
        input_data.until_input <- input_data.until_input - 1
      else input_data.until_input <- 7;
      (* 6 for jump*)
      Lwt_io.printl msg >>= fun () ->
      Lwt_unix.sleep !fps >>= fun () ->
      let hit_high =
        match input_data.move with
        | STAND -> true
        | JUMP | FRONTFLIP | BACKFLIP ->
            line input_data.until_input = 2 || line input_data.until_input = 0
        | SLIDE -> false
      in
      if
        obstacles.(0).loc = 0
        && ((obstacles.(0).height = 0 && input_data.until_input = 0)
           || (obstacles.(0).height = 2 && hit_high))
      then
        (* edit so based on object height and character height *)
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
    cooldown := Lwt_unix.sleep (7.0 *. !fps);
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
