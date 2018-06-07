open Yojson
open GameState

(* Lavanya Aprameya memory.mli *)

(** [file_save cs] saves a game to a file
*)
val file_save : string -> curr_state -> unit

(** [file_load s] is the current_state determined by loading file s
*)
val file_load : string -> curr_state