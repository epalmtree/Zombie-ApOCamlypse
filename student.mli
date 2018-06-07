open Yojson

(* Erick Palma, Victoria Litvinova, student.mli *)

(** [base] is the type representing the student's attributes. Each attribute can
 * have a max 5 and a min 1, where higher stats are more beneficial.
 *    - [pow] is the power of a student. The higher this is, the stronger a
 *        student's attack is.
 *    - [res] is the resilience of a student. The higher this is, the faster a
 *        student can heal, recover sleep, etc.
 *    - [endr] is the endurance of a student. The higher this is, the more time
 *        or turns a student can last without eating, sleeping, etc.
 *    - [eff] is the efficiency of a student. The higher this is the more
 *        progress is done per turn.
 *    - [hp] is the Health-Points a student has. The higher this is, the more
 *        health a student has. *)
type base = {pow : int; res: int; endr: int; eff: int; hp: int}

(** [stt] is the current state of a student. If a student is:
 *    - [Awake]: the student can functions normally and call any command
 *    - [Sleeping i]: will be asleep for [i] turns and then will wake up
 *    - [Charging i]: will be charging for [i] turns and then will 'wake up' *)
type stt =
  | Awake
  | Sleeping of int
  | Charging of int

(** [student] represents a student in the game with current stats and player
 * attributes. The fields of the student record are:
 *    - [sbase]:    the student's attributes represented by type base.
 *    - [studinv]:  the list of items that the student has. Each item is a
 *                    tuple of (name * (heal amount * quantity)).
 *    - [state]:    the student's current state represented by type stt.
 *    - [studloc]:  the current location of the student.
 *    - [sleep]:    the current amount of sleep a student has. If this
 *                    reaches 0, the student dies. Max: (140 + 20*sbase.endr)
 *    - [fullness]: the current amount of sleep a student has. If this
 *                    reaches 0, the student dies. Max: (140 + 20*sbase.endr)
 *    - [health]:   the current amount of sleep a student has. If this
 *                    reaches 0, the student dies. Max: (140 + 20*sbase.endr)
 *    - [battery]:  the current amount of sleep a student has. If this
 *                    reaches 0, the student dies. Max: (140 + 20*sbase.endr) *)
type student = {sbase : base; studinv : (string * (int * int)) list;
                state : stt; studloc : string;
                sleep : int; fullness : int; health : int; battery : int}

(** [team] is the team of students that the player controls in the game.
 *    - [students]: the list of student names associated with its student type.
 *        There are at most 4 students in the team. If all die, the game ends.
 *    - [teamloc] is the location of the team base.
 *    - [teaminv] is the list of items present in the team base.
 *    - [prog] is the current amount of progress for the assignment. *)
type team = {students : (string * student) list; teamloc : string;
             teaminv : (string * (int * int)) list; prog : int}

(** [make_team d nms] initializes a team of 4 students with difficulty [d].
 * Initial location is Duffield, teaminv has no items, and progress is 0. *)
val make_team : int -> string list -> team

(* [upd_team tm n d] returns the new team after updating team [tm] after
 * [n] number of turns with difficulty [d]. This includes updating fullness and
 * sleep depletion for each student, assignment progress, and dead students. *)
val upd_team : team -> int -> int -> team

(* [upd_stud stname stat num tm] updates student with the name [stname] in
 * [tm] based on what action occured. The stats and nums are strings and ints as
 * follows:
 *    - "sleep": puts the student to sleep if the student is awake. The [num] is
 *      the number of turns the student will sleep for. Must be true: [num] > 0
 *    - "charge": charges the computer, [num] determines how much the computer
 *      charges. Must be true: [num] > 0
 *    - "health": heals or damages the student. To heal, [num] > 0 and to
 *      damage, [num] < 0. Students with 0 health are removed.
 * For each stat, if the stat is at the max, it will remain at the max.
 *)
val upd_stud: string -> string -> int -> int ->
              team -> team * (string * (int * int)) list

(* [get_stud_names tm] gets the names of all students in [tm]. *)
val get_stud_names : team -> string list

(* [get_awake_studs tm] gets the names of all students in [tm] that are active*)
val get_awake_studs : team -> string list

(* [get_teaminv tm] returns the team inventory. *)
val get_teaminv : team -> (string * (int * int)) list

(* [get_studinv stname tm] returns the student with the name [stname] inventory.
 *)
val get_studinv : string -> team -> (string * (int * int)) list

(* [team_to_stud itm stname tm] transfers [itm] from [tm] inventory to
 * student with the name [stname] inventory. Fails if the item is not in [tm]
 * inventory. *)
val team_to_stud : string -> string -> int -> team -> team

(* [stud_to_team stname tm] transfers all items from student with the name
 * [stname] inventory to [tm] inventory.*)
val stud_to_team : string -> team -> team

(* [rem_team_item] removes an [itm] from [tm] inventory. Does so by
 * updating the number of items to be one less or removes the [itm] completely
* if last instance.*)
val rem_team_item : (string*(int*int)) -> team -> team

(* [add_team_items itms tm] adds [itms] to team [tm] inventory. If the [itm] is
   in the inventory, updates the number of [itm] instances in the inventory. *)
val add_team_items : (string*(int*int)) list -> team -> team

(* [rem_stud_item stname itm tm] removes an [itm] from student with the name
 * [stname] in team [tm]. Does so by updating the number of items to be one less
 * or removes the [itm] completely if last instance. Fails*)
val rem_stud_item : string -> (string*(int*int)) -> team -> team

(* [add_stud_item stname itm tm] adds [itm] to student with the name [stname]
 * inventory. If the [itm] is in the inventory, updates the number of [itm]
 * instances in the inventory. *)
val add_stud_item : string -> (string*(int*int)) -> team -> team

(* [get_studloc stname tm] gets the student with the name [stname] location in
 * [tm]. *)
val get_studloc : string -> team -> (string * string)

(* [get_teamloc tm] returns the [tm] location. *)
val get_teamloc : team -> string

(* [get_studs_locs tm] returns a list of students names and their associated
 * locations in [tm]*)
val get_studs_locs : team -> (string * string) list

(* [upd_studloc stname loc tm] updates the student with the name [stname]
 * location to [loc] in the current team, [tm]. *)
val upd_studloc : string -> string -> team -> team

(* [upd_teamloc loc tm] updates the team [tm] location to [loc]. Any students in
 * the same area are moved too. *)
val upd_teamloc : string -> team -> team

(* [get_stud_power stname tm] returns the attack power of the student with the
    name [stname]. *)
val get_stud_power : string -> team -> int

(* [stud_eat s i amt tm] updates team [tm] by having the student with name [s]
  attempt to eat [amt] amount of the item with name [i]
*)
val stud_eat : string -> string -> int -> team -> team

(* [stud_heal s i amt tm] updates team [tm] by having the student with name [s]
  attempt to heal [amt] amount of the item with name [i]
*)
val stud_heal : string -> string -> int -> team -> team

(* [add_to_inv s lst tm] adds all items in [lst] to the inventory of the student
  with name [s] in team [tm]
*)
val add_to_inv : string -> (string * (int * int)) list -> team -> team

(* [to_json_team tm] is the json representation of [tm]
*)
val to_json_team : team -> Yojson.Safe.json

(* [from_json_team] is the team represented by [j] *)
val from_json_team : Yojson.Basic.json -> team