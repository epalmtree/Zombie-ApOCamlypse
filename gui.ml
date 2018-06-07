open GameState
open Graphics
open Student
open GameMap
open Zombie

(* Paul DeVito gui.ml *)

type buffer = {name: string; student: student; has_items: bool}

type display_node = Show of buffer | Hide

type header = {nw : display_node ; ne : display_node;
               sw : display_node ; se : display_node;
               progress : int ; remaining : int }


(* [unpack_students lst head len map] updates the header [head] to include
 * display_nodes built from the students in [lst] with length [len] and map
 * information from [map]. *)
let rec unpack_students lst head len map : header =
  match lst with
  | [] -> head
  | h::t ->
    let stu = snd h in
    let is_empty = ((get_location stu.studloc map).itemlst = []) in
    let node = {name = fst h; student = stu; has_items = not is_empty} in
    (match len with
    | 1 -> {head with nw = Show node}
    | 2 -> unpack_students t {head with ne = Show node} (len-1) map
    | 3 -> unpack_students t {head with sw = Show node} (len-1) map
    | 4 -> unpack_students t {head with se = Show node} (len-1) map
    | _ -> head)

(* Initialize the empty header with all nodes hidden *)
let head_empty =
  {nw = Hide ; ne = Hide; sw = Hide ; se = Hide;
  progress = 0 ; remaining = 0 }

let make_head (cs:curr_state) : header =
  let stdnts = cs.team.students in
  let head = unpack_students stdnts head_empty (List.length stdnts) cs.map in
  {head with remaining = 336 - cs.turns; progress = cs.team.prog}

(* Shorthand for printing a filled rectangle with a black border at
 * ([x],[y]) with width [w], height [h], and fill color [c] *)
let box x y w h c =
  let x_val = x + 5 in let y_val = y + 5 in
  set_color c;
  fill_rect (x_val) (y_val) w h;
  set_color black;
  draw_rect (x_val) (y_val) w h

(* Shorthand for printing a rectangle with border padding *)
let rect x y w h =
  let x_val = x + 5 in let y_val = y + 5 in
  set_color black;
  draw_rect (x_val) (y_val) w h


(* Shorthand for printing a line of text at ([x],[y]) *)
let write_line x y line =
  moveto x y;
  draw_string line


(* Returns a printable version of the stat [i] and its cap, calculated
 * from the base stat [b] *)
let ratio i b = string_of_int(i) ^ "/" ^ string_of_int(140 + b*20)


(* [draw_node x y n] organizes the information contained in the display_node
 * [n] and prints it in a box with lower left corner ([x],[y]). *)
let draw_node x y (n:display_node) : unit =
  let x_val = x+5 in let y_val = y+12 in
  set_color black;
  match n with
  | Hide -> write_line x_val (y_val+75) " Dead"
  | Show b ->
    write_line x_val (y_val+75) (" "^b.name);
    let stu = b.student in
    let state = match stu.state with
    | Awake -> "Awake"
    | Sleeping i -> ("Sleeping for " ^ string_of_int(i) ^ " turns")
    | Charging i -> ("Charging for " ^ string_of_int(i) ^ " turns") in
    let is_t = if b.has_items then "" else " (empty)" in
    let main_info = " Location: " ^ stu.studloc ^ is_t
      ^ "   State: " ^ state in
    write_line x_val (y_val+50) main_info;
    let base = stu.sbase in
    let var_info =
      " Health: " ^ (ratio stu.health base.hp)
      ^ "  Sleep: " ^ (ratio stu.sleep base.endr)
      ^ "  Fullness: " ^ (ratio stu.fullness base.endr)
      ^ "  Battery: " ^ (ratio stu.battery base.endr) in
    write_line x_val (y_val+25) var_info;
    let base_info = " POW: " ^ string_of_int(base.pow)
      ^ "  RES: " ^ string_of_int(base.res)
      ^ "  END: " ^ string_of_int(base.endr)
      ^ "  EFF: " ^ string_of_int(base.eff)
      ^ "  HP: " ^ string_of_int(base.hp) in
    write_line x_val (y_val+0) base_info


let initialize_gui cs =
  (* initialize frame and static sections *)
  open_graph " 910x510";
  let c1 = rgb 134 209 235 in
  let c2 = rgb 134 158 235 in
  let c3 = rgb 148 237 142 in
  let c4 = rgb 181 142 237 in
  box 0 0 900 500 c1;
  box 0 300 900 200 c2;
  box 500 0 400 300 c3;
  rect 0 300 900 100;
  rect 0 400 900 100;
  rect 0 300 450 200;
  rect 450 300 450 200;

  (* unpack game state *)
  let header = make_head cs in

  (* initialize header *)
  draw_node 0 400 header.nw;
  draw_node 450 400 header.ne;
  draw_node 0 300 header.sw;
  draw_node 450 300 header.se;

  (* initialize body *)
  let turns = " Turns Remaining: " ^ string_of_int(header.remaining)
    ^ "   Base Location: " ^ cs.team.teamloc in
  write_line 5 275 turns;
  let prog_check = min header.progress 3110 in
  let progress = (float_of_int prog_check) /. 3110. in
  write_line 5 250 " Progress: ";
  let bar_loc = 5 + (fst (text_size " Progress: ")) in
  box bar_loc 245 200 13 white;
  box bar_loc 245 (int_of_float (progress *. 200.)) 13 c4


(* [display_selections sel len] vertically prints all the possible next-inputs
 * intended for the command line given in [sel] with length [len] *)
let rec display_selections sel len =
  match sel with
  | [] -> ()
  | h::t ->
    write_line 10 (195 - 15*len) h;
    display_selections t (len-1)


(* Converts an item type into a string that prints it's contents *)
let make_item_display (item:string*(int*int)) =
  " " ^ (fst item) ^ " (recovery: " ^ string_of_int((fst (snd item)))
  ^ ", amount: " ^ string_of_int((snd (snd item))) ^ ")"


let display_inv cs =
  clear_graph ();
  initialize_gui cs;
  write_line 5 200 " Party inventory: ";
  let items = List.rev_map make_item_display cs.team.teaminv in
  display_selections items (List.length items)


(* nearby_zomb z len] prints the (building name, zombie number) list [z]
 * with length [len] *)
let rec nearby_zomb z len =
  match z with
  | [] -> ()
  | h::t ->
  let line = (fst h) ^ ": " ^ (string_of_int (snd h)) in
    write_line 510 (275 - 15*len) line;
    nearby_zomb t (len-1)


(* A wrapper for the recursive function [nearby_zomb] that includes a
 * heading statement *)
let display_zomb z =
  write_line 510 285 "There are zombies lurking in these nearby buildings:";
  nearby_zomb z (List.length z)


let update_main (cs:curr_state) (sel:string list) (s:string) z =
  clear_graph ();
  initialize_gui cs;
  display_zomb z;
  match cs.stage with
  | SNormal ->
    write_line 5 200 (" " ^ s);
    display_selections sel (List.length sel)
  | SCombat(s1,s2) ->
    let line = " " ^ s1
      ^ " is under attack by a frothing zombie! What will you do?" in
    write_line 5 200 line;
    display_selections sel (List.length sel);
    let z_health = string_of_int (get_zomb s2 cs.zombies).health in
    write_line 5 100 (" The zombie has " ^ z_health ^ " health left...")



let destroy_gui () = close_graph ()