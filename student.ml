open Yojson

(* Erick Palma, Victoria Litvinova, student.ml *)

type base = {pow : int; res : int; endr : int; eff : int; hp : int}

type stt =
  | Awake
  | Sleeping of int
  | Charging of int

type student = {sbase : base; studinv : (string * (int * int)) list;
                state : stt; studloc : string;
                sleep : int; fullness : int; health : int; battery : int}

type team = {students : (string * student) list; teamloc : string;
             teaminv : (string * (int * int)) list; prog : int}

(** [random_range i1 i2] gets a random number between i1 (inclusive) and
 * i2 (inclusive). *)
let random_range i1 i2 =
  if i1 >= i2 then raise (Invalid_argument "i1 < i2 must be true.") else
  Random.self_init ();
  let r = Random.int (i2-i1+1) in
  i2-r

(** [list_rr_map f lst] does the same this as List.rev (List.rev_map f lst). *)
let rr_map f lst =
  List.rev (List.rev_map f lst)

(** [make_stud s] creates a student with name [s] and returns the association
  ([s],student)
*)
let make_stud s =
  let sbase =
     {pow = random_range 1 5; res = random_range 1 5;
      endr = random_range 1 5; eff = random_range 1 5;
      hp = random_range 1 5} in
  let endr_cap = 140 + sbase.endr*20 in
  let stud_t =
     {sbase = sbase; studinv = []; state = Awake;
      sleep = endr_cap; fullness = endr_cap; battery = endr_cap;
      health = 140 + sbase.hp*20; studloc = "Duffield"} in
  (s, stud_t)

(* [make_team d nms] creates a new team using difficulty [d] and a list of
  names [nms]
*)
let make_team d nms =
  let stud_lst = rr_map (fun s -> make_stud s) nms in
  {students = stud_lst; teamloc = "Duffield"; teaminv = []; prog = 0}

(** [deplete d chg] returns a new depleted value based on difficulty [d]
  and boolean [chg] that indicates whether a change must be made or not
*)
let deplete d chg =
  if chg then
    if d=1 || d=2 || d=3 then 5+d
    else if d=4 then 6+2*d
    else 9+2*d
  else 1+d

(**[reg_upd st d] performs a single normal depletion on the stats of the student
  with name [st] depending on difficulty [d]
*)
let reg_upd st d =
  let endr_cap = 140 + st.sbase.endr*20 in
  let sleep_chg_plus =
    st.sbase.res * 4 + 8 in
  match st.state with
  | Awake -> {st with fullness = max 0 (st.fullness - (deplete d false));
                  sleep = max 0 (st.sleep - (deplete d false));
                  battery = max 0 (st.battery - (deplete d true))}
  | Sleeping(0) -> {st with state = Awake;
                  fullness = max 0 (st.fullness - (deplete d false));
                  sleep = min endr_cap (st.sleep + sleep_chg_plus)}
  | Sleeping(i) ->let at_max =
                    min endr_cap (st.sleep + sleep_chg_plus) = endr_cap in
                  {st with state = if at_max then Awake else Sleeping(i-1);
                  fullness = max 0 (st.fullness - (deplete d false));
                  sleep = min endr_cap (st.sleep + sleep_chg_plus)}
  | Charging(0) -> {st with state = Awake;
                  fullness = max 0 (st.fullness - (deplete d false));
                  sleep = max 0 (st.sleep - (deplete d false));
                  battery = min endr_cap (st.battery + sleep_chg_plus)}
  | Charging(i) -> {st with state = Charging(i-1);
                  fullness = max 0 (st.fullness - (deplete d false));
                  sleep = max 0 (st.sleep - (deplete d false));
                  battery = min endr_cap (st.battery + sleep_chg_plus)}

(**[upd_team_single tm d] updates the team [tm] for a single turn with
  depletion based on difficulty [d]
*)
let upd_team_single tm d =
  let nms = rr_map (fun s -> fst s) tm.students in
  let studs_t = rr_map (fun s -> snd s) tm.students in
  let upd_studs = rr_map (fun st -> reg_upd st d) studs_t in
  let workable =
    List.filter (fun st ->
      st.studloc = tm.teamloc && st.battery > 0) studs_t in
  let progress = List.fold_left (fun p st -> st.sbase.eff + p) 0 workable in
  let joined = List.rev (List.rev_map2 (fun nm st -> (nm,st)) nms upd_studs) in
  let undead = List.filter (fun (n,st) -> (st.fullness > 0) && (st.sleep > 0)
                            && (st.health > 0)) joined in
  {tm with students = undead; prog = min 3110 (tm.prog + progress)}

(** [upd_team tm n d] updates the team [tm] for [n] number of turns with
  depletion based on difficulty [d]
*)
let upd_team tm n d =
  let empty = Array.make n "" in
  Array.fold_left (fun acc i -> upd_team_single acc d) tm empty

(** [get_student stname tm] is the student associated with name [stname] in
    team [tm]
*)
let get_student stname tm =
  let studs = tm.students in
  if List.mem_assoc stname studs then List.assoc stname studs
  else failwith "Student Not Found"

(** [match_upd new_st tm] takes a new student [new_st] and incorporates it into
  team [tm]
*)
let match_upd new_st tm =
  let upd_studs =
    rr_map (fun y ->
      if fst y = fst new_st then new_st else y) tm.students in
  {tm with students = upd_studs}

(** [upd_stud stname stat num tm] updates student with the name [stname] in
 * [tm] based on what action occured.
*)
let upd_stud stname stat num d tm =
  let st = get_student stname tm in
  let endr_cap = 140 + st.sbase.endr*20 in
  match (stat,num) with
  | ("sleep", i) when st.sleep = (2 * st.sbase.endr + 20) -> (tm,[])
  | ("sleep", i) when i > 0 -> let upd_st = {st with state = Sleeping(i)} in
                               (match_upd (stname, (reg_upd upd_st d)) tm,[])
  | ("charge", i) when endr_cap <= st.battery + i ->
                     let upd_st = {st with battery = endr_cap} in
                     (match_upd (stname, upd_st) tm,[])
  | ("charge", i) -> let upd_st = {st with state=Charging(i)} in
                     (match_upd (stname, upd_st) tm,[])
  | ("health", i) when (st.health - i) <= 0 ->
                     let inv = st.studinv in
                     let upd_studs = List.remove_assoc stname tm.students in
                     ({tm with students = upd_studs}, inv)
  | ("health", i) -> let upd_st = {st with health = min (st.sbase.hp*20 + 140)
                     (st.health - (15 + 5*i))} in
                     (match_upd (stname, upd_st) tm, [])
  | _ -> failwith "Not correct"

(* [upd_studloc stname loc tm] updates the student with the name [stname]
 * location to [loc] in the current team, [tm]. *)
let upd_studloc stname loc tm =
  let st = get_student stname tm in
  let new_st = (stname, {st with studloc = loc}) in
  match_upd new_st tm

(** [mgrt loc st tm] moves the student [st] to [loc] if [st] is in the same
  location as the current [tm]
*)
let mgrt loc st tm =
  if tm.teamloc = st.studloc then {st with studloc = loc} else st

(* [upd_team_loc loc tm] updates the team [tm] location to [loc]. Any students in
 * the same area are moved too. *)
let upd_teamloc loc tm =
  let moved = rr_map (fun (n,st) -> (n, (mgrt loc st tm))) tm.students in
  {tm with students = moved; teamloc = loc}

(* [get_teamloc tm] returns the [tm] location. *)
let get_teamloc tm = tm.teamloc

(* [get_studloc stname tm] gets the student with the name [stname] location in
 * [tm]. *)
let get_studloc stname tm =
  let st = get_student stname tm in (stname, st.studloc)

(* [get_studs_locs tm] returns a list of students names and their associated
 * locations in [tm]*)
let get_studs_locs tm =
  rr_map (fun (n,st) -> get_studloc n tm) tm.students

(* [add_stud_item stname itm tm] adds [itms] to student with the name [stname]
 * inventory. If the [itm] is in the inventory, updates the number of [itm]
 * instances in the inventory. *)
let add_stud_item stname itm tm =
  let st = get_student stname tm in
  let name = fst itm in let amt = snd (snd itm) in
  let new_inv = if List.mem_assoc name st.studinv then
  (let nums = List.assoc name st.studinv in
    (fst itm, (fst nums, (snd nums) + amt))::(List.remove_assoc name st.studinv))
  else itm::st.studinv in
  match_upd (stname, {st with studinv = new_inv}) tm

(*[add_team_items itms tm] adds [itm] to team [tm] inventory. If the [itm] is in
 * the inventory, updates the number of [itm] instances in the inventory. *)
let add_team_items itms tm =
  List.fold_left (fun tm itm ->
  let name = fst itm in let amt = snd (snd itm) in
  let new_inv = if List.mem_assoc name tm.teaminv then
  (let nums = List.assoc name tm.teaminv in
    (fst itm, (fst nums, (snd nums) + amt))::(List.remove_assoc name tm.teaminv))
  else itm::tm.teaminv in {tm with teaminv = new_inv}) tm itms

(* [rem_stud_item stname itm tm] removes an [itm] from student with the name
 * [stname] in team [tm]. Does so by updating the number of items to be one less
 * or removes the [itm] completely if last instance. Fails if [itm] is not
 * there. *)
let rem_stud_item stname itm tm =
  let st = get_student stname tm in
  let name = fst itm in let recov = fst (snd itm) in let amt = snd (snd itm) in
  let nums = List.assoc name st.studinv in
  let new_inv = if (snd nums) - amt <= 0 then List.remove_assoc name st.studinv
  else (name,(recov, (snd nums) - amt)):: (List.remove_assoc name st.studinv)
  in match_upd (stname, {st with studinv = new_inv}) tm

(* [rm_itm_tm itm tm] removes an [itm] from [tm] inventory. Does so by
 * updating the number of items to be one less or removes the [itm] completely
* if last instance. Fails if [itm] is not there. *)
let rem_team_item itm tm =
  let name = fst itm in let recov = fst (snd itm) in let amt = snd (snd itm) in
  let nums = List.assoc name tm.teaminv in
  let new_inv = if (snd nums) - amt <= 0 then List.remove_assoc name tm.teaminv
  else (name,(recov, (snd nums) - amt)):: (List.remove_assoc name tm.teaminv)
  in {tm with teaminv = new_inv}

(*FIX HOW THIS CHANGES STATS*)
(* [stud_eat s i amt tm] updates team [tm] by having the student with name [s]
  attempt to eat [amt] amount of the item with name [i]
*)
let stud_eat s i amt tm =
  let st = get_student s tm in
  let endr_cap = 140 + st.sbase.endr*20 in
  let itm = List.assoc i st.studinv in
  let upd_full= {st with fullness = min endr_cap (st.fullness + fst itm*amt);
    health = min endr_cap (st.health + fst itm*amt)} in
  let new_tm = match_upd (s,upd_full) tm in
  rem_stud_item s (i,(fst itm,amt)) new_tm

(* [stud_heal s i amt tm] updates team [tm] by having the student with name [s]
  attempt to heal [amt] amount of the item with name [i]
*)
let stud_heal s i amt tm =
  let st = get_student s tm in
  let endr_cap = 140 + st.sbase.endr*20 in
  let itm = List.assoc i st.studinv in
  let upd_ht = {st with health = min endr_cap (st.health + fst itm*amt);
    fullness = min endr_cap (st.fullness + fst itm*amt)} in
  let new_tm = match_upd(s,upd_ht) tm in
  rem_stud_item s (i,(fst itm,amt)) new_tm

(* [get_tm_inv tm] returns the team inventory. *)
let get_teaminv tm = tm.teaminv

(* [get_st_inv stname tm] returns the student with the name [stname] inventory.
 *)
let get_studinv stname tm =
  let st = get_student stname tm in
  st.studinv

(* [add_to_inv s lst tm] adds all items in [lst] to the inventory of the student
  with name [s] in team [tm]
*)
let add_to_inv s lst tm =
  List.fold_left (fun tm i -> add_stud_item s i tm) tm lst

(**[same_loc s tm] returns true if the student with name [s] is in the team
  location of team [tm] and false otherwise
*)
let same_loc s tm = s.studloc = tm.teamloc

(* [team_to_tud itm stname tm] transfers [itm] from [tm] inventory to
 * student with the name [stname] inventory.*)
let team_to_stud stname item amt tm =
  let st = get_student stname tm in
  let itm = List.assoc item tm.teaminv in
  let newitm = (item, (fst itm, amt)) in
  let new_team = if same_loc st tm then add_stud_item stname newitm tm else tm in
  let newer_team =
    if same_loc st tm then rem_team_item newitm new_team else new_team in
  newer_team

(* [stud_to_team stname tm] transfers all items from student with the name
 * [stname] inventory to [tm] inventory.*)
let stud_to_team stname tm =
  let st = get_student stname tm in
  let inv = if same_loc st tm then st.studinv else [] in
  let new_st = if same_loc st tm then {st with studinv = []} else st in
  let new_team = match_upd (stname, new_st) tm in
  add_team_items inv new_team

(* [get_stud_names tm] gets the names of all students in [tm]. *)
let get_stud_names tm =
  fst (List.split tm.students)

(* [get_awake_tuds tm] gets the names of all students in [tm] that are active *)
let get_awake_studs tm =
  let awks = List.filter (fun (n,st) -> st.state = Awake) tm.students in
  rr_map (fun (f,s) -> f) awks

(* [get_stud_power stname tm] returns the attack power of the student with the
  name [stname]. *)
let get_stud_power stname tm =
  let st = get_student stname tm in
  st.sbase.pow

(*helpers for writing to a json*)

let to_json_base b =
  `Assoc [("pow",`Int b.pow); ("res",`Int b.res); ("endr",`Int b.endr);
  ("eff",`Int b.eff);("hp",`Int b.hp)]

let to_json_state s =
  match s with
  | Awake -> `Tuple[`String "awk"]
  | Sleeping(i) -> `Tuple[`String "slp";`Int i]
  | Charging(i) -> `Tuple[`String "chr";`Int i]

let to_json_stud st =
  let stbase = to_json_base st.sbase in
  let inv = rr_map (fun (n,(i,a)) -> `Tuple [`String n;`Int i; `Int a])
  st.studinv in
  let ststate = to_json_state st.state in
  `Assoc [("sbase",stbase); ("studinv",`List inv); ("state",ststate);
  ("sleep",`Int st.sleep);("fullness",`Int st.fullness);
  ("health",`Int st.health);("studloc",`String st.studloc);
  ("battery",`Int st.battery)]

(* [to_json_team tm] is the json representation of [tm]
*)
let to_json_team tm =
  let slst = rr_map(fun (n,s) -> `Tuple [`String n; to_json_stud s])
  tm.students in
  let inv = rr_map (fun (n,(i,a)) -> `Tuple [`String n;`Int i; `Int a])
  tm.teaminv in
  `Assoc [("students",`List slst); ("teamloc",`String tm.teamloc);
  ("teaminv",`List inv); ("prog",`Int tm.prog)]

(* helper functions for reading jsons*)

let mem = Yojson.Basic.Util.member
let jlst = Yojson.Basic.Util.to_list
let jint = Yojson.Basic.Util.to_int
let jstr = Yojson.Basic.Util.to_string

let from_json_base j =
  let pow = mem "pow" j |> jint in let res = mem "res" j |> jint in
  let endr = mem "endr" j |> jint in let eff = mem "eff" j |> jint in
  let hp = mem "hp" j |> jint in
  {pow = pow; res = res; endr = endr; eff = eff; hp = hp}

let from_json_state j =
  let st = jlst j in
  match st with
  | [h1;h2] ->
    let state = Yojson.Basic.Util.to_string h1 in
    let amt = Yojson.Basic.Util.to_int h2 in if state = "slp" then Sleeping(amt)
    else Charging(amt)
  | _ -> Awake

let from_json_itm j =
  let lst = jlst j in
  match lst with
  | [h1;h2;h3] -> (jstr h1, (jint h2, jint h3))
  | _ -> ("",(0,0))

let from_json_stud j =
  let sbase = mem "sbase" j |> from_json_base in
  let inv = mem "studinv" j |> jlst in
  let studinv = rr_map(fun j -> from_json_itm j) inv in
  let state = mem "state" j |> from_json_state in
  let sleep = mem "sleep" j |> jint in
  let fullness = mem "fullness" j |> jint in
  let health = mem "health" j |> jint in
  let studloc = mem "studloc" j |> jstr in
  let battery = mem "battery" j |> jint in
  {sbase = sbase; studinv = studinv; state = state; sleep = sleep; fullness =
  fullness; health = health; studloc = studloc; battery = battery}

let from_tuple j =
  match j with
  | [n;s] -> (jstr n, from_json_stud s)
  | _ ->
    let sbase = {pow = 0; res = 0; endr = 0; eff = 0; hp = 0} in
    ("", {sbase = sbase;studinv = []; state = Awake; sleep = 0; fullness = 0;
    health = 0; studloc = ""; battery = 0})

(* [from_json_team] is the team represented by [j] *)
let from_json_team j =
  let ss = mem "students" j |> jlst in
  let studlst = rr_map (fun j -> jlst j) ss in
  let students = rr_map(fun j -> from_tuple j) studlst in
  let teamloc = mem "teamloc" j |> jstr in
  let inv = mem "teaminv" j |> jlst |> rr_map(fun j -> from_json_itm j) in
  let prog = mem "prog" j |> jint in
  {students = students; teamloc = teamloc; teaminv = inv; prog = prog}