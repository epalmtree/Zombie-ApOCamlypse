open OUnit2
open Student

let s1_base = {pow=5;res=5;endr=5;eff=5;hp=5}
let s2_base = {pow=4;res=4;endr=4;eff=4;hp=4}
let s3_base = {pow=3;res=3;endr=3;eff=3;hp=3}
let s4_base = {pow=2;res=2;endr=2;eff=5;hp=2}
let s1_inv = []
let s2_inv = [("apple",(1,2))]
let s3_inv = [("pear",(3,4));("banana",(5,6))]
let s4_inv = [("peach",(7,8));("grapes",(9,10));("orange",(11,12))]

let s1 = {sbase=s1_base;studinv=s1_inv;state=Awake;studloc="Here";sleep=100;
          health=100;battery=100;fullness=100}
let s2 = {sbase=s2_base;studinv=s2_inv;state=Sleeping(1);studloc="There";
          sleep=90;health=90;battery=90;fullness=90}
let s3 = {sbase=s3_base;studinv=s3_inv;state=Charging(2);studloc="Here";
          sleep=80;health=80;battery=80;fullness=80}
let s4 = {sbase=s4_base;studinv=s4_inv;state=Awake;studloc="Where";sleep=70;
          health=70;battery=70;fullness=70}

let teaminv = [("pineapple",(13,14));("berry",(15,16))]
let studs = [("s1",s1);("s2",s2);("s3",s3);("s4",s4)]

let tm = {students = studs; teamloc = "Here"; teaminv = teaminv; prog = 100}

let tm_to_stud = {students =[("s1",
    {sbase = {pow = 5; res = 5; endr = 5; eff = 5; hp = 5};
     studinv = [("pineapple", (13, 1))]; state = Awake; studloc = "Here";
     sleep = 100; fullness = 100; health = 100; battery = 100});
   ("s2",
    {sbase = {pow = 4; res = 4; endr = 4; eff = 4; hp = 4};
     studinv = [("apple", (1, 2))]; state = Sleeping 1; studloc = "There";
     sleep = 90; fullness = 90; health = 90; battery = 90});
   ("s3",
    {sbase = {pow = 3; res = 3; endr = 3; eff = 3; hp = 3};
     studinv = [("pear", (3, 4)); ("banana", (5, 6))]; state = Charging 2;
     studloc = "Here"; sleep = 80; fullness = 80; health = 80; battery = 80});
   ("s4",
    {sbase = {pow = 2; res = 2; endr = 2; eff = 5; hp = 2};
     studinv = [("peach", (7, 8)); ("grapes", (9, 10)); ("orange", (11, 12))];
     state = Awake; studloc = "Where"; sleep = 70; fullness = 70; health = 70;
     battery = 70})];
 teamloc = "Here"; teaminv = [("pineapple", (13, 13)); ("berry", (15, 16))];
 prog = 100}


 let stud_to_tm = {students =[("s1",
    {sbase = {pow = 5; res = 5; endr = 5; eff = 5; hp = 5}; studinv = [];
     state = Awake; studloc = "Here"; sleep = 100; fullness = 100;
     health = 100; battery = 100});
   ("s2",
    {sbase = {pow = 4; res = 4; endr = 4; eff = 4; hp = 4};
     studinv = [("apple", (1, 2))]; state = Sleeping 1; studloc = "There";
     sleep = 90; fullness = 90; health = 90; battery = 90});
   ("s3",
    {sbase = {pow = 3; res = 3; endr = 3; eff = 3; hp = 3}; studinv = [];
     state = Charging 2; studloc = "Here"; sleep = 80; fullness = 80;
     health = 80; battery = 80});
   ("s4",
    {sbase = {pow = 2; res = 2; endr = 2; eff = 5; hp = 2};
     studinv = [("peach", (7, 8)); ("grapes", (9, 10)); ("orange", (11, 12))];
     state = Awake; studloc = "Where"; sleep = 70; fullness = 70; health = 70;
     battery = 70})];
 teamloc = "Here";
 teaminv =
  [("banana", (5, 6)); ("pear", (3, 4)); ("pineapple", (13, 14));
   ("berry", (15, 16))];
 prog = 100}


let rm_tm_itm = {students =[("s1",
    {sbase = {pow = 5; res = 5; endr = 5; eff = 5; hp = 5}; studinv = [];
     state = Awake; studloc = "Here"; sleep = 100; fullness = 100;
     health = 100; battery = 100});
   ("s2",
    {sbase = {pow = 4; res = 4; endr = 4; eff = 4; hp = 4};
     studinv = [("apple", (1, 2))]; state = Sleeping 1; studloc = "There";
     sleep = 90; fullness = 90; health = 90; battery = 90});
   ("s3",
    {sbase = {pow = 3; res = 3; endr = 3; eff = 3; hp = 3};
     studinv = [("pear", (3, 4)); ("banana", (5, 6))]; state = Charging 2;
     studloc = "Here"; sleep = 80; fullness = 80; health = 80; battery = 80});
   ("s4",
    {sbase = {pow = 2; res = 2; endr = 2; eff = 5; hp = 2};
     studinv = [("peach", (7, 8)); ("grapes", (9, 10)); ("orange", (11, 12))];
     state = Awake; studloc = "Where"; sleep = 70; fullness = 70; health = 70;
     battery = 70})];
 teamloc = "Here"; teaminv = [("pineapple", (13, 9)); ("berry", (15, 16))];
 prog = 100}


let add_tm_itms = {students =[("s1",
    {sbase = {pow = 5; res = 5; endr = 5; eff = 5; hp = 5}; studinv = [];
     state = Awake; studloc = "Here"; sleep = 100; fullness = 100;
     health = 100; battery = 100});
   ("s2",
    {sbase = {pow = 4; res = 4; endr = 4; eff = 4; hp = 4};
     studinv = [("apple", (1, 2))]; state = Sleeping 1; studloc = "There";
     sleep = 90; fullness = 90; health = 90; battery = 90});
   ("s3",
    {sbase = {pow = 3; res = 3; endr = 3; eff = 3; hp = 3};
     studinv = [("pear", (3, 4)); ("banana", (5, 6))]; state = Charging 2;
     studloc = "Here"; sleep = 80; fullness = 80; health = 80; battery = 80});
   ("s4",
    {sbase = {pow = 2; res = 2; endr = 2; eff = 5; hp = 2};
     studinv = [("peach", (7, 8)); ("grapes", (9, 10)); ("orange", (11, 12))];
     state = Awake; studloc = "Where"; sleep = 70; fullness = 70; health = 70;
     battery = 70})];
 teamloc = "Here";
 teaminv =
  [("carrots", (1, 2)); ("broccoli", (3, 4)); ("pineapple", (13, 14));
   ("berry", (15, 16))];
 prog = 100}

let rm_st_itm = {students =[("s1",
    {sbase = {pow = 5; res = 5; endr = 5; eff = 5; hp = 5}; studinv = [];
     state = Awake; studloc = "Here"; sleep = 100; fullness = 100;
     health = 100; battery = 100});
   ("s2",
    {sbase = {pow = 4; res = 4; endr = 4; eff = 4; hp = 4}; studinv = [];
     state = Sleeping 1; studloc = "There"; sleep = 90; fullness = 90;
     health = 90; battery = 90});
   ("s3",
    {sbase = {pow = 3; res = 3; endr = 3; eff = 3; hp = 3};
     studinv = [("pear", (3, 4)); ("banana", (5, 6))]; state = Charging 2;
     studloc = "Here"; sleep = 80; fullness = 80; health = 80; battery = 80});
   ("s4",
    {sbase = {pow = 2; res = 2; endr = 2; eff = 5; hp = 2};
     studinv = [("peach", (7, 8)); ("grapes", (9, 10)); ("orange", (11, 12))];
     state = Awake; studloc = "Where"; sleep = 70; fullness = 70; health = 70;
     battery = 70})];
 teamloc = "Here"; teaminv = [("pineapple", (13, 14)); ("berry", (15, 16))];
 prog = 100}


let add_st_itm = {students = [("s1",
    {sbase = {pow = 5; res = 5; endr = 5; eff = 5; hp = 5};
     studinv = [("pie", (20, 25))]; state = Awake; studloc = "Here";
     sleep = 100; fullness = 100; health = 100; battery = 100});
   ("s2",
    {sbase = {pow = 4; res = 4; endr = 4; eff = 4; hp = 4};
     studinv = [("apple", (1, 2))]; state = Sleeping 1; studloc = "There";
     sleep = 90; fullness = 90; health = 90; battery = 90});
   ("s3",
    {sbase = {pow = 3; res = 3; endr = 3; eff = 3; hp = 3};
     studinv = [("pear", (3, 4)); ("banana", (5, 6))]; state = Charging 2;
     studloc = "Here"; sleep = 80; fullness = 80; health = 80; battery = 80});
   ("s4",
    {sbase = {pow = 2; res = 2; endr = 2; eff = 5; hp = 2};
     studinv = [("peach", (7, 8)); ("grapes", (9, 10)); ("orange", (11, 12))];
     state = Awake; studloc = "Where"; sleep = 70; fullness = 70; health = 70;
     battery = 70})];
 teamloc = "Here"; teaminv = [("pineapple", (13, 14)); ("berry", (15, 16))];
 prog = 100}


 let upd_stl = {students =[("s1",
    {sbase = {pow = 5; res = 5; endr = 5; eff = 5; hp = 5}; studinv = [];
     state = Awake; studloc = "There"; sleep = 100; fullness = 100;
     health = 100; battery = 100});
   ("s2",
    {sbase = {pow = 4; res = 4; endr = 4; eff = 4; hp = 4};
     studinv = [("apple", (1, 2))]; state = Sleeping 1; studloc = "There";
     sleep = 90; fullness = 90; health = 90; battery = 90});
   ("s3",
    {sbase = {pow = 3; res = 3; endr = 3; eff = 3; hp = 3};
     studinv = [("pear", (3, 4)); ("banana", (5, 6))]; state = Charging 2;
     studloc = "Here"; sleep = 80; fullness = 80; health = 80; battery = 80});
   ("s4",
    {sbase = {pow = 2; res = 2; endr = 2; eff = 5; hp = 2};
     studinv = [("peach", (7, 8)); ("grapes", (9, 10)); ("orange", (11, 12))];
     state = Awake; studloc = "Where"; sleep = 70; fullness = 70; health = 70;
     battery = 70})];
 teamloc = "Here"; teaminv = [("pineapple", (13, 14)); ("berry", (15, 16))];
 prog = 100}


let added = {students =[("s1",
    {sbase = {pow = 5; res = 5; endr = 5; eff = 5; hp = 5};
     studinv = [("pickle", (3, 4))]; state = Awake; studloc = "Here";
     sleep = 100; fullness = 100; health = 100; battery = 100});
   ("s2",
    {sbase = {pow = 4; res = 4; endr = 4; eff = 4; hp = 4};
     studinv = [("apple", (1, 2))]; state = Sleeping 1; studloc = "There";
     sleep = 90; fullness = 90; health = 90; battery = 90});
   ("s3",
    {sbase = {pow = 3; res = 3; endr = 3; eff = 3; hp = 3};
     studinv = [("pear", (3, 4)); ("banana", (5, 6))]; state = Charging 2;
     studloc = "Here"; sleep = 80; fullness = 80; health = 80; battery = 80});
   ("s4",
    {sbase = {pow = 2; res = 2; endr = 2; eff = 5; hp = 2};
     studinv = [("peach", (7, 8)); ("grapes", (9, 10)); ("orange", (11, 12))];
     state = Awake; studloc = "Where"; sleep = 70; fullness = 70; health = 70;
     battery = 70})];
 teamloc = "Here"; teaminv = [("pineapple", (13, 14)); ("berry", (15, 16))];
 prog = 100}



let tests = [
"get_stud_names" >:: (fun _ -> assert_equal ["s1"; "s2"; "s3"; "s4"]
  (get_stud_names tm));
"get_awake_studs" >:: (fun _ -> assert_equal ["s1";"s4"] (get_awake_studs tm));
"get_teaminv" >:: (fun _ -> assert_equal [("pineapple",(13,14));
  ("berry",(15,16))] (get_teaminv tm));
"get_studinv" >:: (fun _ -> assert_equal [("pear", (3, 4)); ("banana", (5, 6))]
  (get_studinv "s3" tm));
"team_to_stud" >:: (fun _ -> assert_equal tm_to_stud (team_to_stud
  "s1" "pineapple" 1 tm));
"stud_to_team" >:: (fun _ -> assert_equal stud_to_tm (stud_to_team "s3" tm));
"rem_team_item" >:: (fun _ -> assert_equal rm_tm_itm (rem_team_item
  ("pineapple",(13,5)) tm));
"add_team_items" >:: (fun _ -> assert_equal add_tm_itms (add_team_items
  [("broccoli",(3,4));("carrots",(1,2))] tm));
"rem_stud_item" >:: (fun _ -> assert_equal rm_st_itm (rem_stud_item
  "s2" ("apple",(1,2)) tm));
"add_stud_item" >:: (fun _ -> assert_equal add_st_itm (add_stud_item "s1"
  ("pie",(20,25)) tm));
"get_studloc" >:: (fun _ -> assert_equal ("s1","Here") (get_studloc "s1" tm));
"get_teamloc" >:: (fun _ -> assert_equal "Here" (get_teamloc tm));
"get_studs_locs" >:: (fun _ -> assert_equal [("s1", "Here"); ("s2", "There");
  ("s3", "Here"); ("s4", "Where")] (get_studs_locs tm));
"upd_studloc" >:: (fun _ -> assert_equal upd_stl (upd_studloc "s1" "There" tm));
"upd_teamloc" >:: (fun _ -> assert_equal "There" (get_teamloc
  (upd_teamloc "There" tm)));
"get_stud_power" >:: (fun _ -> assert_equal 5 (get_stud_power "s1" tm));
"add_to_inv" >:: (fun _ -> assert_equal added (add_to_inv "s1"
  [("pickle",(3,4))] tm));
]