open Command
open GameMap
open Student
open Zombie
open Yojson

(* Lavanya Aprameya gameState.ml *)

type stage = SNormal | SCombat of string * string

type curr_state = {map : game_map; team : team; zombies : zombs;
                  stage : stage; turns : int; diff : int}

(* [new_cs i lst] is a new curr_state initialized using difficulty [i] and
  student names using [lst]
*)
let new_cs i lst  =
  let map = make_map i in let new_map = fst map in let locs = snd map in
  {map = new_map; team = make_team i lst; zombies = make_zombs i locs items;
    stage = SNormal; turns = 0; diff = i}

(* [is_alive n lst] is true if n is a member of lst and false otherwise
*)
let is_alive n lst = List.mem n lst


let game_ended cs =
  List.length (get_awake_studs cs.team) = 0 (* ||  cs.team.prog >= 2177 *)
  || cs.turns >= 336

(* [update_stage cs] is [SNormal] if no students are in the same location as any
  zombie in state [cs] and [SCombat(s,z)] if student s and zombie z are in the
  same location in state [cs]
*)
let update_stage cs =
  try(
    let zlocs = get_zomb_locs cs.zombies in
    let slocs = get_studs_locs cs.team in
    let range = (List.length zlocs) - (List.length slocs) in
    let combined = List.fold_left (fun acc s ->
      let lst = List.filter (fun z -> snd s = snd z) zlocs in
      let lst1 = List.rev_map (fun z -> (fst s, fst z)) lst in lst1@acc)
      [] slocs in
    match combined with | h::_ -> SCombat(fst h, snd h)
                      | _ -> SNormal)
  with | _ -> cs.stage

(* [single_update cs u] is the curr_state produced by executing the update
  [u] on the game with state [cs]
*)
let single_update cs u =
  try (
  let t = turns_of_update u in
  match (u, cs.stage) with
  | (Sleep(s,i),_) ->
      let new_team = upd_stud s "sleep" i cs.diff cs.team in
      {cs with team = fst new_team; turns = cs.turns + t}
  | (Take(s,i,amt),_) ->
      let new_team = team_to_stud s i amt cs.team in
      {cs with team = new_team; turns = cs.turns + t}
  | (Move(s,l),_) ->
      let new_team = upd_studloc s l cs.team in
      {cs with team = new_team; turns = cs.turns + t}
  | (Pickup(s),_) ->
      let loc = snd (get_studloc s cs.team) in
      let removed = remove_items loc cs.map in
      let new_map = fst removed in let inv = snd removed in
      let new_team = add_to_inv s inv cs.team in
      {cs with map = new_map; team = new_team; turns = cs.turns + t}
  | (Eat(s,i,amt),_) ->
      let new_team = stud_eat s i amt cs.team in
      {cs with team = new_team; turns = cs.turns + t}
  | (Charge(s,i),_) ->
      let new_team = upd_stud s "charge" i cs.diff cs.team in
      {cs with team = fst new_team; turns = cs.turns + t}
  | (TeamMove(l),_) ->
      let new_team = upd_teamloc l cs.team in
      {cs with team = new_team; turns = cs.turns + t}
  | (Transfer(s),_) ->
      let new_team = stud_to_team s cs.team in
      {cs with team = new_team; turns = cs.turns + t}
  | (Work,_) -> {cs with turns = cs.turns + t}
  | (Fight,SCombat(s,z)) ->
      let power = get_stud_power s cs.team in
      let reduced_zombie = upd_health z (power) cs.zombies in
      let new_zombies = fst reduced_zombie in let inv = snd reduced_zombie in
      let new_team = add_to_inv s inv cs.team in
      {cs with zombies = new_zombies; team = new_team}
  | (Heal(i,amt),SCombat(s,z)) ->
      let new_team = stud_heal s i amt cs.team in
      {cs with team = new_team}
  | (Run(l),SCombat(s,z)) ->
      let new_team = upd_studloc s l cs.team in
      {cs with team = new_team; stage = SNormal; turns = cs.turns + t}
  | _ -> cs) with | _ -> cs

(* [upd_n cs] performs a normal update on the game with state cs. Normal updates
  are the following: [Sleep(s,i)], [Take(s,i,amt)], [Move(s,l)], [Pickup(s,i)],
  [Transfer(s,)], [Eat(s,i,amt)], [Charge(s,amt)], [Save], [Exit], [TeamMove(l)]
  , [Work], and [Inventory]*)
let upd_n cs i =
  try(let empty = Array.make i "" in
    let new_team = upd_team cs.team i cs.diff in
    let slocs= List.rev_map (fun x->snd x) (get_studs_locs new_team) in
    let zlocs = get_zomb_locs cs.zombies in
    let final_locs = Array.fold_left(fun acc i->update_zombies slocs acc cs.map)
      zlocs empty in
    let new_zombies = move_zombs cs.zombies final_locs in
    let new_stage=update_stage {cs with team= new_team; zombies= new_zombies} in
    {cs with team = new_team; zombies = new_zombies; stage = new_stage})
  with | _ -> cs

(* [upd_c cs] performs a combat update on the game with state cs. Combat updates
  are the following: [Heal(i,amt)], [Fight], and [Run(l)]*)
let upd_c cs =
  try (
  match cs.stage with
  | SCombat(s,z) ->
      let znames = List.rev_map (fun x -> fst x) cs.zombies in
      let dead_z = not (is_alive z znames) in
      let new_stage = if dead_z then update_stage cs else cs.stage in
      let loc = get_studloc s cs.team in
      let stud_updated = if not dead_z then upd_stud s "health"
        (get_zomb_power z cs.zombies) cs.diff cs.team else (cs.team, []) in
      let new_team = fst stud_updated in let inv = snd stud_updated in
      let new_map = add_items (snd loc) inv cs.map in
      let dead_s = not (is_alive s (get_stud_names new_team)) in
      let new_stage = if dead_s then update_stage {cs with stage = new_stage;
        team = new_team; map = new_map} else new_stage in
      {cs with team = new_team; stage = new_stage; map = new_map}
  | SNormal -> cs) with | _ -> cs

(* [zombies_to_display cs] is an association list of all the locations of
  zombies within distance 4 of any student and the quantity of zombies in that
  location
*)
let zombies_to_display cs =
  let slocs = List.rev_map (fun x->snd x) (get_studs_locs cs.team) in
  let zlocs = List.map (fun znl -> snd znl) (get_zomb_locs cs.zombies) in
  nearby_zombies zlocs slocs cs.map

(*helpers for json writing*)

let to_json_stage st =
  match st with
  | SNormal -> `Tuple[`String "normal"]
  | SCombat(s,z) -> `Tuple [`String "combat"; `String(s);`String(z)]

let to_json cs =
  `Assoc([("map",to_json_map cs.map);("team",to_json_team cs.team);
        ("zombies",to_json_zombies cs.zombies);("stage",to_json_stage cs.stage);
        ("turns",`Int(cs.turns));("diff",`Int(cs.diff))])

(*helpers for json parsing*)

let mem = Yojson.Basic.Util.member
let jlst = Yojson.Basic.Util.to_list
let jint = Yojson.Basic.Util.to_int
let jstr = Yojson.Basic.Util.to_string

(* [to_json cs] is the json representation of curr_state [cs]
*)
let from_json_stage st =
  let stage = jlst st in
  match stage with
  | [h1;h2;h3] ->
    SCombat(jstr h2, jstr h3)
  | _ -> SNormal

(* [from_json j] is the state represented by the json [j]
*)
let from_json j =
  let map = mem "map" j in let tm = mem "team" j in let zs = mem "zombies" j in
  let stage = mem "stage" j in let turns = mem "turns" j in
  let diff = mem "diff" j in
  {map = from_json_map map; team = from_json_team tm; zombies = from_json_zombies
  zs; stage = from_json_stage stage; turns = jint turns; diff = jint diff}