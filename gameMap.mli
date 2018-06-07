open Yojson

(* Erick Palma gameMap.mli *)

(** [location] is the type of a location in the map, which has:
 *   - [location_type] : which is an int from 0..3, where:
 *        0 is a neutral-type location
 *        1 is a sleep-type location
 *        2 is a food-type location
 *        3 is a work-type location
 *        4 is a sleep-food-type location
 *        5 is a food-work-type location
 *        6 is a sleep-food-work-type location
 *   - [exits] : a list of adjacent rooms' names
 *   - [itemlst] : a list of tuples representing items and the heal points and
 *        quantity of each from the current location
 *   - [point] : point (x,y) on the map *)
type location =  {location_type : int;
                  itemlst       : (string * (int * int)) list;
                  exits         : string list;
                  point         : int * int}

(** [game_map] is a list of tuples representing a location's name and its
 * location type holding information about it. *)
type game_map = (string * location) list

(** [items] is a list of all the possible items that can be in the game. *)
val items : (string * (int * int)) list

(** [make_map d] is the game_map produced at the beginning of a game where [d]
 * is the difficulty and determines the amount of food in the map. *)
val make_map : int -> game_map * (string list)

(** [get_location l gm] is the location associated with [l] in [gm]. *)
val get_location : string -> game_map -> location

(** [get_exits l gm] is the list of names of locations that are valid exits
 * from the location associated with [l] in [gm]. *)
val get_exits : string -> game_map -> string list

(** [get_distance l1 l2 gm] is the shortest distance between the location
 * associated with [l1] and the location associated with [l2] in [gm]. *)
val get_distance : string -> string -> game_map -> int

(** [get_items l gm] is the itemlst associated with location [l] in [gm]. *)
val get_items : string -> game_map -> (string * (int * int)) list

(** [get_item l i gm] is the ([i] * (heal points, quantity)) tuple associated
 * with [i] in the itemlst of the location associated with [l] in [gm]. *)
val get_item : string -> string -> game_map -> string * (int * int)

(** [add_item l itms gm] is the updated map after adding [itms] to location at
 * [l] in [gm]. *)
val add_items : string -> (string * (int * int)) list -> game_map -> game_map

(** [remove_items l gm] is the tuple of the map, created by removing all items
 * from the location associated with [l] in [gm], and the list of the
 * items that were removed. *)
val remove_items : string -> game_map -> game_map * ((string*(int*int)) list)

(** [get_location_type l gm] is the type of the location associated with [l]
 * in [gm], represented by an int from 1..3. *)
val get_location_type : string -> game_map -> int

(** [update_zombies sloc znamloc gm] is a [znamloc] with all the zombies
 * locations updated. A zombie will move towards the nearest student
 * if the distance between the two is 2, in Manhattan distance, in [gm]. *)
val update_zombies :
  string list -> (string*string) list -> game_map -> (string*string) list

(** [nearby_zombies zlocs slocs gm] returns zombie locations within distance 4 and
 * the quantity of zombies in that location, for all students. *)
val nearby_zombies : string list -> string list -> game_map -> (string*int) list

(** [to_json_map gm] is the json representation of gm. *)
val to_json_map : game_map -> Yojson.Safe.json

(** [to_json_map j] is the map represented by j. *)
val from_json_map : Yojson.Basic.json -> game_map