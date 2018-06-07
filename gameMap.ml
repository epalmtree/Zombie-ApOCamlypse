open Yojson

(* Erick Palma gameMap.ml *)

type location = {location_type : int; itemlst : (string * (int*int)) list;
                 exits : string list; point : int * int}

type game_map = (string * location) list

let items =
 [("Apple", (60, 1)); ("Orange", (60, 1)); ("Cup_Noodles", (50, 1));
  ("Mini_Donuts", (40, 1)); ("Bubble_Tea", (40, 1)); ("Oishii_Bowl", (70, 1));
  ("Katsudon", (70, 1)); ("Pizza", (65, 1)); ("Chinese_TakeOut", (65, 1));
  ("Mac_n_Cheese", (60, 1));("Cookie", (40, 1));("Tazo_Tea", (40, 1));
  ("Insomnia_Cookie", (60, 1)); ("Naked_Juice", (40, 1))]

(** [item_avg] is the average of the points of the items in the list above. *)
let item_avg =
  let total =
    List.fold_left (fun pts it -> pts+(fst (snd it))) 0 items in
  total/(List.length items)

(** [random_range i1 i2] gets a random number between i1 (inclusive) and
 * i2 (inclusive). *)
let random_range i1 i2 =
  if i1 >= i2 then raise (Invalid_argument "i1 < i2 must be true.") else
  Random.self_init ();
  let r = Random.int (i2-i1+1) in
  i2-r

let get_location l gm : location =
  List.assoc l gm

let get_items l gm =
  (get_location l gm).itemlst

let get_exits l gm : string list =
  (get_location l gm).exits

let get_distance l1 l2 gm : int =
  let pt1 = (get_location l1 gm).point in
  let pt2 = (get_location l2 gm).point in
  let abs i = if i<0 then (-1)*i else i in
  (abs ((fst pt1) - (fst pt2))) + (abs ((snd pt1) - (snd pt2)))

let get_item l i gm : string * (int*int) =
  let loc_items = (get_location l gm).itemlst in
  (i, List.assoc i loc_items)

(** [update_quantity lst itms] returns an updated itemlst after adding
 * [itms] into the item[lst]. *)
let update_quantity lst itms : (string * (int*int)) list =
  List.fold_left (fun acc it ->
    let itm_nm = fst it in
    if List.mem_assoc itm_nm acc then
      let itm_hl = fst (snd it) in
      let itm_qt = snd (snd it) in
      let lst_qt = snd (List.assoc itm_nm acc) in
      let rem_it = List.remove_assoc itm_nm acc in
      let upd_qt = lst_qt+itm_qt in
      if upd_qt <= 0 then rem_it
      else [(itm_nm, (itm_hl,upd_qt))] @ rem_it
    else
      if snd (snd it) <= 0 then acc
      else [it] @ acc) lst itms

let add_items l itms gm : game_map =
  let pre_loc = get_location l gm in
  let pre_items = pre_loc.itemlst in
  let prev_gm = List.remove_assoc l gm in
  let upd_loc = {pre_loc with itemlst = update_quantity pre_items itms} in
  [(l,upd_loc)] @ prev_gm

let remove_items l gm : game_map * ((string * (int*int)) list) =
  let loc = List.assoc l gm in
  let items = loc.itemlst in
  let new_loc = {loc with itemlst = []} in
  let new_map = (l, new_loc)::List.remove_assoc l gm in
  (new_map, items)

let get_location_type l gm : int =
  (get_location l gm).location_type

(** [chase znl sl] is an updated tuple of the zombie name and updated
 * zombie location from moving zombie [znl] towards student location [sl]. *)
let chase znl sl gm =
  List.fold_left (fun acc ext ->
    let prev_dist = get_distance sl (snd acc) gm in
    let curr_dist = get_distance sl ext gm in
    if curr_dist <= prev_dist then (fst acc, ext)
    else acc) znl (get_exits (snd znl) gm)

(** [random_move znl]  *)
let random_move znl gm =
  let curr_n_exits = [snd znl] @ get_exits (snd znl) gm in
  let arr = Array.of_list curr_n_exits in
  let ind = random_range 0 ((Array.length arr) -1) in
  let new_loc = Array.get arr ind in
  (fst znl, new_loc)

let update_zombies sloc znamloc gm : (string * string) list =
  try(List.map (fun znl ->
    let z_loc = snd znl in
    let nearest =
      List.fold_left (fun acc sl -> if acc=[] then [sl] else
        let prev_nearest = (List.hd acc) in
        let prev_loc = get_distance z_loc prev_nearest gm in
        let curr_loc = get_distance z_loc sl gm in
        if curr_loc <= prev_loc then [sl] else acc) [] sloc in
    if get_distance (List.hd nearest) z_loc gm <= 2
    then chase znl (List.hd nearest) gm
    else random_move znl gm
  ) znamloc) with | _ -> []

(** [combine lst] combines all the duplicates and updates tuple quantities. *)
let combine lst =
  List.fold_left (fun acc s ->
    if List.mem_assoc (fst s) acc then
      let qt_of = List.assoc (fst s) acc in
      let pre_lst = List.remove_assoc (fst s) acc in
      let upd_qt = (snd s) + qt_of in
      [(fst s,upd_qt)]@pre_lst
    else [s]@acc) [] lst

let nearby_zombies zlocs slocs gm =
  let nearby =
    List.fold_left (fun nrb z_loc ->
      let nearest =
        List.fold_left (fun acc sl -> if acc=[] then [sl] else
          let prev_nearest = (List.hd acc) in
          let prev_loc = get_distance z_loc prev_nearest gm in
          let curr_loc = get_distance z_loc sl gm in
          if curr_loc <= prev_loc then [sl] else acc) [] slocs in
      if get_distance (List.hd nearest) z_loc gm <= 4 then [(z_loc,1)]@nrb
      else nrb) [] zlocs in
  combine nearby

(** [rand_num_items n]  *)
let rand_num_items n =
  let itms = Array.of_list items in
  let empty = Array.make n "" in
  let randos = Array.fold_left (fun acc _ ->
    let ind = random_range 0 ((Array.length itms)-1) in
    [Array.get itms ind] @ acc) [] empty in
  update_quantity [] randos

(** [random_item_lst d]  *)
let random_item_lst d =
  match d with
  | 0 -> rand_num_items (random_range 0 1)
  | 1 -> rand_num_items (random_range 2 4)
  | 2 -> rand_num_items (random_range 2 5)
  | 3 -> rand_num_items (random_range 3 5)
  | 4 -> rand_num_items (random_range 3 6)
  | 5 -> rand_num_items (random_range 4 7)
  | _ -> []

let make_map d : game_map * string list=
  let rec randomize_items i =
    if i=0 then
      random_item_lst 0
    (* For Louie's. *)
    else if i=1 then
      [("Milkshake", (60,4)); ("Chili_Cheese_Fries", (75,2));
      ("Parmesian_Chicken", (95,1)); ("Maven", (95,1));
      ("Garlic_Bread", (75,2))] @ randomize_items 6
    (* For dining halls. *)
    else if i=2 then
     [("Banana", (40,4)); ("French_Toast", (70,2)); ("Cornell_IceCream",(35,4));
      ("Apple_Cider", (35,4)); ("Shumai", (20,5)); ("Dumplings", (30,4));
      ("Stir_Fry", (65,2)); ("Burgers", (80,1)); ("Pasta", (75,1));
      ("Sushi", (35,4))] @ randomize_items 6
    (* For places that sell snacks. *)
    else if i=3 then
     [("Latte", (20,4)); ("Coffee", (15,8)); ("Muffin", (30,4));
      ("Calzone", (75,2)); ("Mini_Donuts", (40,2)); ("Naked_Juice", (40,2));
      ("Smoothie", (40,2))] @ randomize_items 6
    (* For places with both snack-selling and dining hall places. *)
    else if i=4 then
      update_quantity [] (randomize_items 3 @ randomize_items 2)
    (* For the base location. For the sake of keeping players alive. *)
    else if i=5 then
      update_quantity [] (randomize_items 3@random_item_lst d@random_item_lst d)
    else if i=6 then
      random_item_lst d
    else [] in
  (* Code style is off on purpose for the sake of readability. *)
  let places_info =
   [("Becker",                (4, randomize_items 2, ["Cook";"Rose"], (0,7)) );
    ("Cook",                  (4, randomize_items 2, ["Becker";"Baker_Tower"], (1,6)) );
    ("Rose",                  (4, randomize_items 2, ["Becker";"Baker_Tower";"Bethe";"Baker_Flagpole"], (1,7)) );
    ("Bethe",                 (4, randomize_items 2, ["Rose";"Baker_Flagpole";"Keeton";"Noyes_Center"], (1,8)) );
    ("Keeton",                (4, randomize_items 2, ["Bethe"], (1,9)) );
    ("Baker_Tower",           (1, randomize_items 0, ["Cook";"Rose";"N_Slope"], (2,6)) );
    ("Noyes_Center",          (0, randomize_items 0, ["Bethe";"S_Slope"], (2,8)) );
    ("Baker_Flagpole",        (0, randomize_items 0, ["Rose";"Bethe";"N_Slope";"S_Slope"], (3,7)) );
    ("Johnson_Museum",        (0, randomize_items 0, ["White"], (4,5)) );
    ("N_Slope",               (0, randomize_items 0, ["Baker_Tower";"Baker_Flagpole";"White";"McGraw"], (4,6)) );
    ("S_Slope",               (0, randomize_items 0, ["Baker_Flagpole";"Noyes_Center";"Willard_Straight";"Uris_Library";"Morrill"], (4,8)) );
    ("White",                 (3, randomize_items 0, ["Johnson_Museum";"N_Slope";"Sibley";"McGraw";"Arts_Quad"], (5,5)) );
    ("McGraw",                (3, randomize_items 0, ["N_Slope";"White";"Arts_Quad";"Morrill"], (5,6)) );
    ("Morrill",               (3, randomize_items 0, ["S_Slope";"Uris_Library";"McGraw";"Arts_Quad";"Olin_Library"], (5,7)) );
    ("Uris_Library",          (3, randomize_items 3, ["S_Slope";"Morrill";"Olin_Library";"Sage_Chapel"], (5,8)) );
    ("Willard_Straight",      (5, randomize_items 4, ["S_Slope";"Cornell_Store"], (5,9)) );
    ("Hollister",             (3, randomize_items 0, ["Carpenter";"Snee";"Bard"], (5,13)) );
    ("Snee",                  (3, randomize_items 0, ["Hollister"], (5,14)) );
    ("Sibley",                (3, randomize_items 0, ["White";"Arts_Quad";"Milstein"], (6,5)) );
    ("Arts_Quad",             (0, randomize_items 0, ["Morrill";"McGraw";"White";"Sibley";"Milstein";"Lincoln";"Goldwin_Smith"], (6,6)) );
    ("Olin_Library",          (5, randomize_items 3, ["Morrill";"Uris_Library";"Sage_Chapel"], (6,7)) );
    ("Sage_Chapel",           (0, randomize_items 0, ["Olin_Library";"Uris_Library";"Day";"Cornell_Store"], (6,8)) );
    ("Cornell_Store",         (2, randomize_items 3, ["Willard_Straight";"Sage_Chapel";"Day";"Olin_Hall"], (6,9)) );
    ("Olin_Hall",             (3, randomize_items 0, ["Cornell_Store";"Campus_&_College_Xing"], (6,10)) );
    ("Campus_&_College_Xing", (0, randomize_items 0, ["Olin_Hall";"Carpenter"], (6,11)) );
    ("Carpenter",             (3, randomize_items 0, ["Campus_&_College_Xing";"Hollister"], (6,12)) );
    ("Bard",                  (3, randomize_items 0, ["Hollister";"Thurston"], (6,13)) );
    ("Milstein",              (3, randomize_items 0, ["Sibley";"Arts_Quad";"Lincoln";"Rand"], (7,5)) );
    ("Lincoln",               (3, randomize_items 0, ["Arts_Quad";"Goldwin_Smith";"Milstein"], (7,6)) );
    ("Goldwin_Smith",         (3, randomize_items 0, ["Lincoln";"Klarman";"Arts_Quad"], (7,7)) );
    ("Stimson",               (0, randomize_items 0, ["Day";"Tower_&_East_Xing"], (7,8)) );
    ("Day",                   (0, randomize_items 0, ["Tower_&_East_Xing";"Stimson";"Sage_Chapel";"Cornell_Store"], (7,9)) );
    ("Thurston",              (3, randomize_items 0, ["Bard";"Kimball"], (7,14)) );
    ("Rand",                  (3, randomize_items 0, ["Milstein";"S_Thurston_Bridge"], (8,5)) );
    ("Klarman",               (5,(randomize_items 3)@[("3110",(10,31))], ["Goldwin_Smith"], (8,7)) );
    ("Tower_&_East_Xing",     (0, randomize_items 0, ["Stimson";"Day";"A.D.White_House";"Uris_Hall"], (8,9)) );
    ("Sage_Hall",             (3, randomize_items 0, ["Campus_&_East_Xing"], (8,11)) );
    ("Kimball",               (3, randomize_items 0, ["Thurston";"Upson"], (8,14)) );
    ("Louie's",               (2, randomize_items 1, ["Balch_BusStop"], (9,2)) );
    ("Risley",                (4, randomize_items 0, ["N_Thurston_Bridge";"Balch_BusStop"], (9,3)) );
    ("N_Thurston_Bridge",     (0, randomize_items 0, ["Risley";"Balch_BusStop";"Noyes_Lodge";"S_Thurston_Bridge"], (9,4)) );
    ("S_Thurston_Bridge",     (0, randomize_items 0, ["N_Thurston_Bridge";"Baker_Hall";"Rand"], (9,5)) );
    ("Baker_Hall",            (3, randomize_items 0, ["S_Thurston_Bridge";"PSB"], (9,6)) );
    ("PSB",                   (5, randomize_items 3, ["Clark";"Baker_Hall"], (9,7)) );
    ("Rockefeller",           (3, randomize_items 0, ["Clark";"Space_Sciences";"A.D.White_House"], (9,8)) );
    ("A.D.White_House",       (0, randomize_items 0, ["Rockefeller";"Tower_&_East_Xing";"Uris_Hall"], (9,9)) );
    ("Uris_Hall",             (3, randomize_items 3, ["A.D.White_House";"Tower_&_East_Xing";"Statler"], (9,10)) );
    ("Statler",               (6, randomize_items 3, ["Uris_Hall";"Campus_&_East_Xing"], (9,11)) );
    ("Campus_&_East_Xing",    (0, randomize_items 0, ["Sage_Hall";"Statler";"Duffield";"Gates"], (9,12)) );
    ("Duffield",              (5, randomize_items 5, ["Campus_&_East_Xing";"Phillips";"Upson"], (9,13)) );
    ("Upson",                 (3, randomize_items 0, ["Kimball";"Duffield";"Rhodes"], (9,14)) );
    ("Balch_BusStop",         (0, randomize_items 0, ["Louie's";"Risley";"N_Thurston_Bridge";"Balch"], (10,3)) );
    ("Noyes_Lodge",           (3, randomize_items 0, ["N_Thurston_Bridge"], (10,4)) );
    ("Clark",                 (3, randomize_items 0, ["PSB";"Rockefeller";"Bailey"], (10,7)) );
    ("Space_Sciences",        (3, randomize_items 0, ["Rockefeller";"Malott"], (10,8)) );
    ("Gates",                 (3,(randomize_items 0)@[("Rubber_Duck",(150,1)); ("Companion_Cube", (160,1))], ["Campus_&_East_Xing"], (10,12)) );
    ("Phillips",              (3, randomize_items 0, ["Duffield"], (10,13)) );
    ("Rhodes",                (3, randomize_items 0, ["Upson"], (10,14)) );
    ("Jameson",               (1, randomize_items 0, ["RPCC";"Donlon"], (11,0)) );
    ("Dickson",               (1, randomize_items 0, ["Donlon";"Balch"], (11,2)) );
    ("Balch",                 (1, randomize_items 0, ["Balch_BusStop";"CKB";"Dickson"], (11,3)) );
    ("Bailey",                (0, randomize_items 0, ["Clark";"Roberts"], (11,7)) );
    ("Malott",                (3, randomize_items 0, ["Space_Sciences";"Kennedy"], (11,8)) );
    ("RPCC",                  (5, randomize_items 4, ["Jameson";"Donlon"], (12,0)) );
    ("Donlon",                (1, randomize_items 0, ["RPCC";"Jameson";"Dickson";"CKB";"Mews"], (12,1)) );
    ("CKB",                   (1, randomize_items 0, ["Donlon";"Balch";"Mews"], (12,2)) );
    ("Roberts",               (3, randomize_items 0, ["Kennedy";"Bailey";"Surge_A"], (12,7)) );
    ("Kennedy",               (3, randomize_items 0, ["Roberts";"Malott";"Surge_A"], (12,8)) );
    ("Mews",                  (1, randomize_items 0, ["Donlon";"CKB";"Appel"], (13,2)) );
    ("Surge_A",               (3, randomize_items 0, ["Roberts";"Kennedy"], (13,8)) );
    ("Appel",                 (5, randomize_items 4, ["Mews"], (14,2)) )] in
  let gm =
    List.map (fun tup ->
      let record =
        (match snd tup with
        | (t,i,e,p) -> {location_type=t; itemlst=i; exits=e; point=p}) in
      (fst tup, record)) places_info in
  (gm,
   ["Olin_Library";"Klarman";"Uris_Library";"Keeton";"Bethe";"Rose";"Becker";
    "Cook";"Clark";"Surge_A";"Appel";"RPCC";"Louie's";"Dickson";"Malott"])

(*helper functions for writing jsons*)

let to_json_loc l =
  let items = List.rev_map (fun (n,(i,a)) -> `Tuple [`String n;`Int i; `Int a])
    l.itemlst in
  let exitslst = List.rev_map (fun s -> `String s) l.exits in
  let x = fst l.point in let y = snd l.point in
  `Assoc [("location_type",`Int l.location_type);("itemlst", `List items);
  ("exits", `List exitslst); ("point", `Tuple [`Int x; `Int y])]

let to_json_map gm : Yojson.Safe.json =
  let map = List.rev_map(fun (n,l) -> `Tuple [`String n; to_json_loc l]) gm in
  `List map

(*helper functions for reading jsons*)

let jlst = Yojson.Basic.Util.to_list
let jint = Yojson.Basic.Util.to_int
let jstr = Yojson.Basic.Util.to_string
let mem = Yojson.Basic.Util.member

let from_json_pt j =
  let ptlst = jlst j in
  match ptlst with
  | h1::h2::[] -> (jint h1, jint h2)
  | _ -> (0,0)

let from_json_itm j =
  let lst = Yojson.Basic.Util.to_list j in
  match lst with
  | [h1;h2;h3] -> (jstr h1, (jint h2, jint h3))
  | _ -> ("",(0,0))

let from_json_loc j =
  let location_type = mem "location_type" j in
  let items = mem "itemlst" j |> jlst in
  let itemlst = List.rev_map(fun j -> from_json_itm j) items in
  let exits = mem "exits" j |> jlst in
  let exitlst = List.rev_map (fun j -> jstr j) exits in
  let point = Yojson.Basic.Util.member "point" j in
  {location_type = Yojson.Basic.Util.to_int location_type;
  itemlst = itemlst ;exits = exitlst; point = from_json_pt point}

let from_tuple j =
  match j with
  | [n;l] -> (jstr n, from_json_loc l)
  | _ -> ("", {location_type = 0; itemlst = []; exits = []; point = (0,0)})

let from_json_map j =
  let lst = jlst j in
  let nmlst = List.rev_map (fun j -> jlst j) lst in
  List.rev_map(fun j -> from_tuple j) nmlst