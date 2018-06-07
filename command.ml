(* Lavanya Aprameya command.ml *)

(** action during any stage of game
*)
type command = | Sleep | Take | Move | Pickup | Transfer  | Eat | Charge | Save
               | Exit | TeamMove | Inventory | Fight | Heal | Run | Work | None

(** update to the current state of the game
*)

type update =
  Sleep of string * int | Take of string * string * int
  | Move of string * string | Pickup of string
  | Eat of string * string * int | Charge of string * int | Save of string
  | Exit | TeamMove of string | Transfer of string | Inventory | Fight
  | Heal of string * int | Run of string | Work | None

(** [get_action ()] is the input_action represented by reading a user input
*)
let get_command () : command =
  let s = read_line () in
  let s1 = String.lowercase_ascii (String.trim s) in
  match s1 with
  | "sleep" -> Sleep
  | "take" -> Take
  | "move" -> Move
  | "pickup" -> Pickup
  | "transfer" -> Transfer
  | "eat" -> Eat
  | "charge" -> Charge
  | "save" -> Save
  | "exit" -> Exit
  | "teammove" -> TeamMove
  | "inventory" -> Inventory
  | "fight" -> Fight
  | "heal" -> Heal
  | "run" -> Run
  | "work" -> Work
  | _ -> None

(** [get_name ()] is the name represented by reading a user input
*)
let get_name lst =
  let s = read_line () in
  let s1 = String.lowercase_ascii (String.trim s) in
  let lst1 = List.filter (fun s -> String.lowercase_ascii s = s1) lst in
  match lst1 with
  | [] -> ""
  | h::_ ->
    if (String.contains h ' ') then String.sub h 0 (String.index h ' ') else h


(** [get_time ()] is the amount of time represented by reading a user input
*)
let get_time () = try(let i = read_int () in if i < 0 || i > 10 then 0 else i)
                  with | _ -> 0

let turns_of_update u =
  match u with
  | Sleep(_,i) -> 0
  | Take(_,_,_) -> 0
  | Move(_,_) -> 1
  | Pickup(_) -> 0
  | Eat(_,_,_) -> 0
  | Charge(_,i) -> 0
  | Save(_) -> 0
  | Exit -> 0
  | TeamMove(_) -> 2
  | Transfer(_) -> 1
  | Inventory -> 0
  | Fight -> 0
  | Heal(_,_) -> 0
  | Run(_) -> 1
  | Work -> 1
  | None -> 0