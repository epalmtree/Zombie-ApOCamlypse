open GameState
open Gui
open Memory
open Command
open Student
open Zombie
open GameMap

(* Lavanya Aprameya runner.mli *)

(**[parse_command cs] gets a command and turns it into an update, following
 * different logical paths dependent on the specific command or update. For
 * example, the command Exit turns into the update Exit (i.e., no extra
 * information is added), so that simply returns Exit. TeamMove turns into
 * TeamMove(l), so the function takes [cs] and uses the current team location
 * to prompt the user for [l] using the list of exits from the current location.
 * This logic follows for all the variations of command-to-update
 * transformations.
*)
val parse_command : curr_state -> update

(** [new_game n lst] is the curr_state defined by initializing a game of
  difficulty [n] with a team with names found in [lst]
*)
val new_game : int -> string list -> curr_state

(** [load_game f] is the curr_state defined by loading a game from file f
*)
val load_game : string -> curr_state

(**[run_game cs] is the curr_state defined by processing one loop of the game in
 * which a user gives the game an update, which makes a new curr_state when
 * processed, and then uses this new curr_state to update the general state
 * of the game, then running the game again if it has not terminated*)
val run_game : curr_state -> curr_state

(** [end_game cs] ends the game at the final state cs
*)
val end_game : curr_state -> unit

(** [main ()] is the entry point to a game from outside this module)
*)
val main : (int * string * string list) -> unit