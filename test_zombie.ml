open OUnit2
open Zombie

let z1 = {power = 10; health = 10; zomloc = "Here"; items = [("apple",(1,2))]}
let z2 = {power = 20; health = 20; zomloc = "There"; items = []}
let z3 = {power = 30; health = 30; zomloc = "Nowhere"; items = [("pear",(3,4));
("peach",(5,6))]}

let zombies = [("z1",z1);("z2",z2);("z3",z3)]

let tests = [
"get_zomb_locs" >:: (fun _ -> assert_equal [("z3","Nowhere");("z2","There");("z1","Here")]
  (get_zomb_locs zombies));
"get_zomb_power3" >:: (fun _ -> assert_equal 30 (get_zomb_power "z3" zombies));
"get_zomb_power2" >:: (fun _ -> assert_equal 20 (get_zomb_power "z2" zombies));
"get_zomb_power1" >:: (fun _ -> assert_equal 10 (get_zomb_power "z1" zombies));
]