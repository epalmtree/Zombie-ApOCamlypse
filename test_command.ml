open OUnit2
open Command

let tests = [
  "Sleep" >:: (fun _ -> assert_equal 0 (turns_of_update(Sleep("test",1))));
  "Take" >:: (fun _ -> assert_equal 0 (turns_of_update(Take("test","this",10))));
  "Move" >:: (fun _ -> assert_equal 1 (turns_of_update(Move("test","this"))));
  "Pickup" >:: (fun _ -> assert_equal 0 (turns_of_update(Pickup("test"))));
  "Eat" >:: (fun _ -> assert_equal 0 (turns_of_update(Eat("test","this",10))));
  "Charge" >:: (fun _ -> assert_equal 0 (turns_of_update(Charge("test",1))));
  "Save" >:: (fun _ -> assert_equal 0 (turns_of_update(Save("test"))));
  "Exit" >:: (fun _ -> assert_equal 0 (turns_of_update(Exit)));
  "TeamMove" >:: (fun _ -> assert_equal 2
    (turns_of_update(TeamMove("test"))));
  "Transfer" >:: (fun _ -> assert_equal 1 (turns_of_update(Transfer("test"))));
  "Inventory" >:: (fun _ -> assert_equal 0 (turns_of_update(Inventory)));
  "Fight" >:: (fun _ -> assert_equal 0 (turns_of_update(Fight)));
  "Heal" >:: (fun _ -> assert_equal 0 (turns_of_update(Heal("test",1))));
  "Run" >:: (fun _ -> assert_equal 1 (turns_of_update(Run("test"))));
  "Work" >:: (fun _ -> assert_equal 1 (turns_of_update(Work)));
  "None" >:: (fun _ -> assert_equal 0 (turns_of_update(None)));
]