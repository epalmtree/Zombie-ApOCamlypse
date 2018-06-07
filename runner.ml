open GameState
open Gui
open Memory
open Command
open Student
open Zombie
open GameMap

(* Lavanya Aprameya runner.ml *)

(** [ncommand] is the list of normal commands
*)
let ncommand = ["Sleep";"Take";"Move";"Pickup";"Transfer";"Eat";"Charge";
                "Work";"Save";"Exit";"TeamMove";"Inventory"]

(** [ccommand] is the list of combat commands
*)
let ccommand = ["Fight";"Heal";"Run"]

(** [tcommand] is the list of time commands
*)
let tcommand = ["0";"1";"2";"3";"4";"5"]

(** [amtcommand i] is the list of int commands from 0 to i+1 (inclusive)
*)
let amtcommand i =
  Array.make (i+1) ""|>Array.mapi (fun i e -> string_of_int i) |> Array.to_list

(**[parse_command cs] gets a command and turns it into an update, following
 * different logical paths dependent on the specific command or update. For
 * example, the command Exit turns into the update Exit (i.e., no extra
 * information is added), so that simply returns Exit. TeamMove turns into
 * TeamMove(l), so the function takes [cs] and uses the current team location
 * to prompt the user for [l] using the list of exits from the current location.
 * This logic follows for all the variations of command-to-update
 * transformations.
*)
let parse_command cs =
  try(let command = get_command () in
    let close_by = zombies_to_display cs in
  match (command,cs.stage) with
  | (Sleep,SNormal) ->
      let studs = get_awake_studs cs.team in
      update_main cs studs "Who?" close_by; let name = get_name studs in
      update_main cs tcommand "How long?" close_by; let time = get_time () in
      if name = "" then None else Sleep(name,time)
  | (Take,SNormal) ->
      let studs = get_awake_studs cs.team in
      update_main cs studs "Who?" close_by; let name = get_name studs in
      let items = List.rev_map (fun (x,(y,z)) -> x ^ " (recovery: " ^
        string_of_int y ^ ")") (get_teaminv cs.team) in
      let itemnames = List.rev_map (fun (x,(y,z)) -> x) (get_teaminv cs.team) in
      update_main cs items "What?" close_by; let item = get_name itemnames in
      let inv = get_teaminv cs.team in let amount = snd (List.assoc item inv) in
      let nums = amtcommand amount in
      update_main cs nums "How much?" close_by;
      let amt = get_name nums |> int_of_string
      in if name = "" || item = "" then None else Take(name,item,amt)
  | (Move,SNormal) ->
      let studs = get_awake_studs cs.team in
      update_main cs studs "Who?" close_by; let name = get_name studs in
      let loc = snd(get_studloc name cs.team) in let locs = get_exits loc cs.map
      in update_main cs locs "Where?" close_by; let place = get_name locs in
      if name = "" || place = "" then None else Move(name,place)
  | (Pickup,SNormal) ->
      let studs = get_awake_studs cs.team in
      update_main cs studs "Who?" close_by; let name = get_name studs in
      if name = "" then None else Pickup(name)
  | (Transfer,SNormal) ->
      let studs = get_awake_studs cs.team in
      update_main cs studs "Who?" close_by; let name = get_name studs in
      if name = "" then None else Transfer(name)
  | (Eat,SNormal) ->
      let studs = get_awake_studs cs.team in
      update_main cs studs "Who?" close_by; let name = get_name studs in
      let inv = get_studinv name cs.team in
      let items = List.rev_map (fun (x,(y,z)) -> x ^ " (recovery: " ^
        string_of_int y ^ ")") inv in
      let itemnames = List.rev_map (fun (x,(y,z)) -> x) inv in
      update_main cs items "What?" close_by; let item = get_name itemnames in
      let nums = amtcommand (snd (List.assoc item inv)) in
      update_main cs nums "How much?" close_by;
      let amt = get_name nums |> int_of_string
      in if name = "" || item = "" then None else Eat(name,item,amt)
  | (Work,SNormal) -> Work
  | (Charge,SNormal) ->
      let studs = get_awake_studs cs.team in
      update_main cs studs "Who?" close_by; let name = get_name studs in
      update_main cs tcommand "How long?" close_by; let time = get_time () in
      if name = "" then None else Charge(name,time)
  | (Save,SNormal) ->
    update_main cs [] "To what file name (minus the extension)?" close_by;
    let file_name = read_line () in Save(file_name)
  | (Exit,SNormal) -> Exit
  | (TeamMove,SNormal) ->
      let locs = get_exits (get_teamloc cs.team) cs.map in
      update_main cs locs "Where?" close_by; let place = get_name locs in
      if place = "" then None else TeamMove(place)
  | (Inventory,SNormal) -> Inventory
  | (Fight,SCombat(_,_)) -> Fight
  | (Heal,SCombat(s,_)) ->
      let inv = get_studinv s cs.team in
      let items = List.rev_map (fun (x,(y,z)) -> x ^ " (recovery: " ^
        string_of_int y ^ ")") inv in
      let itemnames = List.rev_map (fun (x,(y,z)) -> x) inv in
      update_main cs items "What?" close_by; let item = get_name itemnames in
      let nums = amtcommand (snd (List.assoc item inv)) in
      update_main cs nums "How much?" close_by;
      let amt = get_name nums |> int_of_string
      in if item = "" then None else Heal(item,amt)
  | (Run,SCombat(s,_)) ->
      let loc = snd (get_studloc s cs.team) in
      let locs = get_exits loc cs.map in
      update_main cs locs "Where?" close_by; let place = get_name locs in
      if place = "" then None else Run(place)
  | _ -> None) with | _ -> None

(**[new_game n lst] is the curr_state defined by initializing a game of
 * difficulty [n] with a team with names found in [lst]
*)
let new_game n lst=
  let cs = new_cs n lst in initialize_gui cs; cs

(** [load_game f] is the curr_state defined by loading a game from file [f]
*)
let load_game f =
  let cs = file_load f in initialize_gui cs; cs

(**[run_game cs] is the curr_state defined by processing one loop of the game in
 * which a user gives the game an update, which makes a new curr_state when
 * processed, and then uses this new curr_state to update the general state
 * of the game, then running the game again if it has not terminated*)
let rec run_game cs =
  if game_ended cs then cs else
  let close_by = zombies_to_display cs in
  let () = if cs.stage = SNormal then
    update_main cs ncommand "What next?" close_by
    else update_main cs ccommand "What next?" close_by in
  let u = parse_command cs in
  match u with
  | Exit -> cs
  | Save(s) -> file_save s cs; run_game cs
  | Inventory -> display_inv cs; read_line (); run_game cs
  | _ -> let cs1 = if (get_awake_studs cs.team) = [] then
        {cs with turns = cs.turns} else single_update cs u in
        let t = turns_of_update u in
        let cs2 = if cs1.stage = SNormal then upd_n cs1 t else upd_c cs1 in
        run_game cs2

(** [end_game cs] ends the game at the final state cs
*)
let end_game g  =
  let () = if g.team.prog >= 2177 then print_endline("YOU WIN!!")
    else print_endline("You've died and become zombified") in
  destroy_gui ()

(** [main ()] is the entry point to a game from outside this module
*)
let main t =
  match t with
  | (i,"",n) -> let cs = new_game i n in let cs1 = run_game cs in end_game cs1
  | (0,f,[]) -> let cs = load_game f in let cs1 = run_game cs in end_game cs1
  | _ -> failwith "not working"