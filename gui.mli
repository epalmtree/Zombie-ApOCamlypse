open GameState
open Graphics
open Student

(* Paul Devito gui.mli *)

(* The GUI header will hold information about the conditions of each
 * team member, and will update based on turns elapsed The data in each
 * display_node can be found in the [buffer]. *)
type buffer = {name: string; student: student; has_items: bool}

(* An active student is a current member of the team that is present. It
 * has a name, a location, and a stat buffer. If there are fewer than four
 * team members, some of the display_nodes will become hidden. *)
type display_node = Show of buffer | Hide

(* The maximum team size is four students, and as such there are four display
 * nodes in the header in addition to the progress bar and remaining time. *)
type header = {nw : display_node ; ne : display_node;
               sw : display_node ; se : display_node;
               progress : int ; remaining : int(* ; map: image *)}

(* [make_head cs] returns a header with progress, remaining time, and four
 * display nodes, all derived from [cs].
 * Requires: cs.team.students has 1 - 4 inclusive elements *)
val make_head: curr_state -> header

(* [display_inv cs] functions similarly to update_main, except in place
 * of the selection list, the party's inventory is displayed. *)
val display_inv: curr_state -> unit

(* [initialize_gui cs] will create the window which holds the gui. *)
val initialize_gui: curr_state -> unit

(* [update_main state sel prompt zom] will update the entire GUI, including
 * the header, based on the current state of the game [state], and the list
 * of possible user selections [sel] for command.ml to wait for. [prompt]
 * will be printed before selections and [zom] is a list of nearby locations
 * and the number of zombies they contain. *)
val update_main:
  curr_state -> string list -> string -> (string*int) list -> unit

(* [destroy_gui] will destroy the gui window after the game ends *)
val destroy_gui: unit -> unit