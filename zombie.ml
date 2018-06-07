open Yojson

(* Erick Palma, Victoria Litvinova, zombie.ml *)

(* zombie is a zombie in the game.
 *   - [power] is the amount of power a zombie has. Helps determine how much
 *     damage the zombie will do in battle.
 *   - [health] is the amount of Hit-Points a zombie has left.
 *   - [zomloc] is the location of the zombie.
 *)
type zombie = {power : int; health : int; zomloc : string;
              items : (string * (int * int)) list}

(* all_zombs is a list of all zombie names and the corresponding zombie. *)
type zombs = (string * zombie) list

(** [random_range i1 i2] gets a random number between i1 (inclusive) and
 * i2 (inclusive). *)
let random_range i1 i2 =
  if i1 >= i2 then raise (Invalid_argument "i1 < i2 must be true.") else
  Random.self_init ();
  let r = Random.int (i2-i1+1) in
  i2-r

(* [init_itms d itmlst] is the inventory of a zombie, initialized randomly
  depending on difficulty [i] and using a list of items [itmlst]
*)
let init_itms d itmlst =
  Random.self_init ();
  let itms = Array.of_list itmlst in
  let num =
    (match d with
    | 1 -> 0
    | 2 -> random_range 0 1
    | 3 -> random_range 1 2
    | _ -> random_range 1 3) in
  let empty = Array.make num "" in
  Array.fold_left (fun acc i -> Random.self_init (); Array.get itms
  (Random.int (Array.length itms)) :: acc) [] empty

(* [make_zs d i locs itmlst] makes a list of [i] zombies using the list of
  locations [locs] and the list of items [itmlst] and difficulty [d]
*)
let make_zs i locs itmlst =
  let empty = Array.make i "" in
  let pow = random_range 1 5 in
  let init = Array.mapi (fun i s -> Random.self_init ();
    ("zombie" ^ string_of_int i,
    {zomloc = (Array.get locs (Random.int(Array.length locs))); power = pow;
    health = 35 + 5*(random_range 1 5); items = init_itms pow itmlst})) empty in
  Array.to_list init

(* [make_zombs d loclst itmlst] makes a list of zombies using difficulty [d] and
  a list of possible locations [loclst] and a list of possible items [itmlst]
*)
let make_zombs d loclst itmlst =
  let locs = Array.of_list loclst in
  match d with
  | 1 -> make_zs 10 locs itmlst
  | 2 -> make_zs 20 locs itmlst
  | 3 -> make_zs 30 locs itmlst
  | 4 -> make_zs 40 locs itmlst
  | 5 -> make_zs 50 locs itmlst
  | _ -> []

(* [get_zomb] gets the zombie with the name [zname] *)
let get_zomb zname zlst =
  if List.mem_assoc zname zlst then List.assoc zname zlst
    else failwith "Zombie Not Found"

let upd_health zname num zlst : zombs * (string * (int * int)) list =
  let zomb = get_zomb zname zlst in
  let new_zombs =
    let depl = 8 + 4 * num in
    if zomb.health - depl <= 0 then
      (List.remove_assoc zname zlst, zomb.items)
    else ((zname, {zomb with health = zomb.health - depl}) ::
      (List.remove_assoc zname zlst),[]) in
  new_zombs

let upd_zomloc zname loc zlst =
  let zomb = get_zomb zname zlst in (zname, {zomb with zomloc = loc})

let move_zombs zlst flocs =
  List.fold_left (fun acc (n,zl) -> (upd_zomloc n zl zlst)::acc) [] flocs

let get_zomb_locs zlst = List.rev_map (fun (nm,z) -> (nm, z.zomloc)) zlst

let get_zomb_power zname zlst = let zomb = get_zomb zname zlst in zomb.power

(*helper functions for writing jsons*)

let to_json_z z =
  let inv = List.rev_map (fun (n,(i,a)) -> `Tuple [`String n;`Int i; `Int a])
  z.items in
  `Assoc [("power",`Int z.power); ("health",`Int z.health); ("zomloc",
  `String z.zomloc);("items",`List inv)]

(* [to_json_zombies z] is the json representation of z
*)
let to_json_zombies zs =
  let zlst = List.rev_map(fun (n,z) -> `Tuple [`String n; to_json_z z]) zs in
  `List zlst

(*helper functions for reading jsons*)

let jlst = Yojson.Basic.Util.to_list
let jint = Yojson.Basic.Util.to_int
let jstr = Yojson.Basic.Util.to_string
let mem = Yojson.Basic.Util.member

let from_json_itm j =
  let lst = Yojson.Basic.Util.to_list j in
  match lst with
  | [h1;h2;h3] -> (jstr h1, (jint h2, jint h3))
  | _ -> ("",(0,0))


let from_json_z j =
  let power = mem "power" j |> jint in
  let health = mem "health" j |> jint in
  let zomloc = mem "zomloc" j |> jstr in
  let inv = mem "items" j |> jlst in
  let items = List.rev_map(fun j -> from_json_itm j) inv in
  {power = power; health = health; zomloc = zomloc; items = items}

let from_tuple j =
  match j with
  | [n;z] -> (jstr n, from_json_z z)
  | _ -> ("", {power = 0; health = 0; zomloc = ""; items = []})

(* [from_json_zombies j] is the zombs represented by j
*)
let from_json_zombies j =
  let lst = jlst j in
  let nmlst = List.rev_map (fun j -> jlst j) lst in
  List.rev_map(fun j -> from_tuple j) nmlst