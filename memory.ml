open Yojson
open GameState

(* Lavanya Aprameya memory.ml *)

(** [file_save cs] saves a game to a file
*)
let file_save (s:string) (cs:curr_state) : unit =
  Yojson.Safe.to_file (s^".json") (to_json cs)

(** [file_load s] is the current_state determined by loading file s
*)
let file_load (f:string) : GameState.curr_state =
  let j = Yojson.Safe.from_file f in let j_basic = Yojson.Safe.to_basic j in
  from_json j_basic