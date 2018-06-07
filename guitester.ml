open Graphics
open Student
open GameState
open Gui
open Unix
open GameMap

(* type print_state = Active of string | Passive of string*int

type static_buff = {pow : int; res : int; endr : int; eff : int; hp : int}

type var_buff = {heath: int; fullness: int; sleep: int; battery: int} *)

let ex_loc = {location_type = 3; itemlst = []; exits = []; point = (0,0)}

let ex_map = [("Becker",ex_loc)]

let ex_base = {pow = 1; res = 2; endr = 3; eff = 4; hp = 10}

let ex_inv = [("Ex_item",(30,5))]

let ex_student = {sbase = ex_base; studinv =  ex_inv;
                state = Awake; sleep = 150; fullness = 100;
                health = 5 ; studloc = "Becker";
                battery = 60}

let ex_students =
  [("Erick",ex_student);("Lavanya", ex_student);
  ("Victoria",ex_student);("Paul",ex_student)]

let ex_team = {students = ex_students; teamloc = "Baker_Flagpole";
             teaminv = ex_inv; prog = 1456}

let ex_state = {map = ex_map; team = ex_team; zombies = [];
                  stage = SNormal; turns = 11; diff = 4}

let ex_selections = ["ExampleOption1";"ExampleOption2";"ExampleOption3";
  "ExampleOption4";"ExampleOption5";"ExampleOption6";"ExampleOption7"]

let ex_selections2 = ["ExampleOption1";"WUB";"ExampleOption3";
  "ExampleOption4";"ExampleOption5"]

let main () =
  print_endline("hello");
  initialize_gui ex_state;
  let y = read_line () in
  update_main ex_state ex_selections "what now" [("Becker",5000)];
  let z = read_line () in
  display_inv ex_state;
  let x = read_line () in ()


let () = main ()

