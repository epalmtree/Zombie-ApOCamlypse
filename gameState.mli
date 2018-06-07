open Command
open GameMap
open Student
open Zombie
open Yojson

(* Lavanya Aprameya runner.ml *)

(**[stage] represents the current stage of the game, which is either SNormal or
  *SCombat(s,z). SNormal represents the stage of the game where all students
  *can carry out normal actions (such as sleeping or eating or moving).
  *SCombat(s,z) represents the stage of the game where the student with name s
  *is in combant with the zombie named z, and only s can carry out the following
  *actions: fighting, healing, and running.
*)
type stage = SNormal | SCombat of string * string

(**[curr_state] represent the state of the game at any given point
  *[map] is the game_map which represents all the current locations in the game
  *[team] is the team which represents the current list of students
  *and other properties of the team (location, progress, inventory)
  *[zombies] is the zombs which represents the current list of zombies
  *[stage] is the current stage of the game
  *[turns] is the number of turns elapsed since the start of the game
  *[diff] is the difficulty of the game
*)

type curr_state = {map : game_map; team : team; zombies : zombs;
                  stage : stage; turns : int; diff : int}

(* [new_cs i lst] is a new curr_state initialized using difficulty [i] and
  student names using [lst]
*)
val new_cs : int -> string list -> curr_state

(* [game_ended cs] is true if the game with state [cs] has ended and false
  otherwise
*)
val game_ended : curr_state -> bool

(* [single_update cs u] is the curr_state produced by executing the update
  [u] on the game with state [cs]
*)
val single_update : curr_state -> update -> curr_state

(* [upd_n cs] performs a normal update on the game with state cs. Normal updates
  are the following: [Sleep(s,i)], [Take(s,i,amt)], [Move(s,l)], [Pickup(s,i)],
  [Transfer(s,)], [Eat(s,i,amt)], [Charge(s,amt)], [Save], [Exit], [TeamMove(l)]
  , [Work], and [Inventory]*)
val upd_n : curr_state -> int -> curr_state

(* [upd_c cs] performs a combat update on the game with state cs. Combat updates
  are the following: [Heal(i,amt)], [Fight], and [Run(l)]*)
val upd_c : curr_state -> curr_state

(* [zombies_to_display cs] is an association list of all the locations of
  zombies within distance 4 of any student and the quantity of zombies in that
  location
*)
val zombies_to_display : curr_state -> (string * int) list

(* [to_json cs] is the json representation of curr_state [cs]
*)
val to_json : curr_state -> Yojson.Safe.json

(* [from_json j] is the state represented by the json [j]
*)
val from_json : Yojson.Basic.json -> curr_state