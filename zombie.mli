open Yojson

(* Erick Palma, Victoria Litvinova, zombie.mli *)

(* zombie is a zombie in the game.
 *   - [power] is the amount of power a zombie has. Helps determine how much
 *     damage the zombie will do in battle.
 *   - [health] is the amount of Hit-Points a zombie has left.
 *   - [zomloc] is the location of the zombie.
 *   - [items] is the list of items the zombie is carrying.
 *)
type zombie = {power : int; health : int; zomloc : string;
              items : (string * (int * int)) list}

(* zombs is a list of all zombie names and the corresponding zombie. *)
type zombs = (string * zombie) list

(* [make_zombs d loclst itmlst] makes a list of zombies using difficulty [d] and
  a list of possible locations [loclst] and a list of possible items [itmlst]
*)
val make_zombs : int -> string list -> (string * (int * int)) list -> zombs

(* [get_zomb zname] gets the zombie with the name [zname] *)
val get_zomb : string -> zombs -> zombie

(* [upd_health zname num zlst] update the health of the zombie with the name
 * [zname]. [num] determines how much damage is taken. It returns the updated
 * list of zombies
 *)
val upd_health : string -> int -> zombs -> zombs * ((string * (int * int)) list)

(* [move_zombs zlst flocs] moves all of the zombies according to [flocs].
 * [flocs] contains the names of the zombies and the new location. *)
val move_zombs : zombs -> (string * string) list -> zombs

(* [get_zomb_locs zlst] gets all of the zombie locations. Returns a list of
 * zombie names and the associated location. *)
val get_zomb_locs : zombs -> (string * string) list

(* [get_z_atk zname zlst] gets the attack power of the zombie with the name
 * [zname]. *)
val get_zomb_power : string -> zombs -> int

(* [to_json_zombies z] is the json representation of z
*)
val to_json_zombies : zombs -> Yojson.Safe.json

(* [from_json_zombies j] is the zombs represented by j
*)
val from_json_zombies : Yojson.Basic.json -> zombs