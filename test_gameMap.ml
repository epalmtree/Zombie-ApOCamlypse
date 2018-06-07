open OUnit2
open GameMap

let it_lst = [("Apple", (60, 5)); ("Orange", (60, 4)); ("Cup_Noodles", (50, 9));
  ("Mini_Donuts", (40, 2)); ("Bubble_Tea", (40, 6))]

let l1 =
  {location_type = 1; itemlst = [("apple",(1,2));("banana",(3,4))]; exits =
  ["l2"]; point = (0,0)}
let l2 =
  {location_type = 2; itemlst = [("pear",(5,6));("grapes",(7,8))]; exits =
  ["l1";"l3";"l4"]; point = (1,0)}
let l3 =
  {location_type = 3; itemlst = [("orange",(9,10))]; exits = ["l2";"l4"];
  point = (1,1)}
let l4 =
  {location_type = 4; itemlst = []; exits = ["l2";"l3"]; point = (1,2)}
let l4_new =
  {location_type = 4; itemlst = [("apple",(1,2));("banana",(3,4))];
  exits = ["l2";"l3"]; point = (1,2)}
let l3_new =
  {location_type = 3; itemlst = []; exits = ["l2";"l4"]; point = (1,1)}
let map = [("l1",l1);("l2",l2);("l3",l3);("l4",l4)]

let tests = [
  "test get_location" >:: (fun _ -> assert_equal l2 (get_location "l2" map));
  "test get_exits" >:: (fun _ -> assert_equal ["l2"] (get_exits "l1" map));
  "test get_distance" >:: (fun _ -> assert_equal 1
    (get_distance "l3" "l4" map));
  "test get_items" >:: (fun _ -> assert_equal [("pear",(5,6));("grapes",(7,8))]
    (get_items "l2" map));
  "test get_item" >:: (fun _ -> assert_equal ("apple",(1,2))
    (get_item "l1" "apple" map));
  "test remove_items" >:: (fun _ -> assert_equal
    ([("l3",l3_new);("l1",l1);("l2",l2);("l4",l4)],[("orange",(9,10))])
    (remove_items "l3" map));
  "test get_location_type" >:: (fun _ -> assert_equal 4
    (get_location_type "l4" map));
]