(* Lavanya Aprameya command.mli *)

(** action during any stage of game
 * [Sleep] allows a student to sleep
 * [Take] allows a student to take an item from the team inventory
 * [Move] allows a student to move to another location
 * [Pickup] allows a student to take items from their current location
 * [Transfer] allows a student to transfer all their items to the team inventory
 * [Eat] allows a student to eat
 * [Charge] allows a student to charge their laptop
 * [Save] saves the game
 * [Exit] ends the game
 * [TeamMove] moves the entire team to another location
 * [Inventory] displays all the student and team inventories
 * [Fight] allows a student to fight a zombie during a zombie attack
 * [Heal] allows a student to heal during a zombie attack
 * [Run] allows a student to run away from a zombie attack
 * [Work] allows the students to stay as are and just work
 * [None] does nothing
*)
type command = | Sleep | Take | Move | Pickup | Transfer  | Eat | Charge | Save
               | Exit | TeamMove | Inventory | Fight | Heal | Run | Work | None

(** update to the current state of the game
 * [Sleep(s,i)] ~ student s to sleep for i turns
 * [Take(s,itm,i)] ~ s to take i amount of itm from the team inventory
 * [Move(sl)] ~ s to move to location l
 * [Pickup(s)] ~ s to take all items from the current location
 * [Transfer(s)] ~ s to put all their items into the team inventory
 * [Eat(s,itm,i)] ~ s to eat i amount of itm
 * [Charge(s,i)] ~ s to charge for i turns
 * [Save(s)] saves the game ~ to save game to filename s.json
 * [Exit] ~ to end the game
 * [TeamMove(l)] ~ to move entire team to l
 * [Inventory] ~ to display all student and team inventories
 * [Fight] ~ to fight a zombie during a zombie attack
 * [Heal(itm,i)] ~ to heal i amount of itm during a zombie attack
 * [Run(l)] ~ to run away to location l
 * [Work] ~ to stay as are and update turns
 * [None] ~ to do nothing
*)

type update =
  Sleep of string * int | Take of string * string * int
  | Move of string * string | Pickup of string
  | Eat of string * string * int | Charge of string * int | Save of string
  | Exit | TeamMove of string | Transfer of string | Inventory | Fight
  | Heal of string * int | Run of string | Work | None

(** [get_action ()] is the input_action represented by reading a user input
*)
val get_command : unit -> command

(** [get_name ()] is the name represented by reading a user input
*)
val get_name : string list -> string

(** [get_time ()] is the amount of time represented by reading a user input
*)
val get_time : unit -> int

(** [turns_of_update u] is the amount of turns represented by [u]
*)
val turns_of_update : update -> int