(**
 * A library of ASCII art strings.
 *)

(**
 * The [Art] module contains all the ASCII art pieces as string literals.
 * You can access them using [Art.a], [Art.b], etc.
 *)
module Art : sig
  val a : string
  val b : string
  val c : string
  val d : string
  val e : string
  val f : string
  val g : string
  val h : string
  val i : string
  val j : string
  val k : string
  val l : string
  val m : string
  val n : string
  val o : string
  val p : string
  val q : string
  val r : string
  val s : string
  val t : string
  val u : string
  val v : string
  val w : string
end

(**
 * [print_art art_string]
 *
 * Prints the given string to standard output, followed by a newline.
 *)
val print_art : string -> unit