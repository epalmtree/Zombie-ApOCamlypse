open Runner

(* Erick Palma main.ml *)

let ltrim s = String.(lowercase_ascii (trim s))

let split s =
  let i = String.index s ' ' in (String.sub s 0 i, String.sub s (i+1)
    (String.length s - i - 1))

let rec s_to_l s lst =
match (split s) with
|("","") -> lst
|(s,"") -> s::lst
|(s1,s2) -> s1::(s_to_l s2 lst)

let are_same lst =
match lst with
| [h1;h2;h3;h4] ->
    h1 = h2 || h1 = h3 || h1 = h4 || h2 = h3 || h2 = h3 || h3 = h4
| _ -> true


let () =
  ANSITerminal.(print_string [cyan]
    ("\n\nOne spooky morning, your 3110 teammates wake you from your hard\n" ^
    "earned rest to inform you that an unfortunate bio student has become\n" ^
    "patient zero in a local zombie apocalypse that Ithacans are calling\n" ^
    "'almost as bad as the drought'.\n\n" ^
    "But the worst of it is not the silent classrooms, the deserted\n" ^
    "libraries, nor the empty TCATs, but the most recent piazza post\n" ^
    "from your beloved professor, Clarkson:\n\n"));
  ANSITerminal.(print_string [red]
    ("    Despite recent events on campus, the deadline for A6 will remain\n" ^
     "    unchanged. And please do not ask about a grace period.\n\n"));
  ANSITerminal.(print_string [cyan]
    ("You have 336 turns to score 70% or higher on this assignment in order\n" ^
    "to maintain an acceptable GPA for graduate school applications.\n" ^
    "Navigate the campus while collecting enough food to outlast this\n" ^
    "assignment.\n\n" ^
    "Don't forget that progress can only be made on an assignment while\n" ^
    "students remain in the home base. Feel free to move the location\n" ^
    "of your base to anywhere on campus.\n\n" ^
    "Please remember that sleep is important for a functioning brain,\n" ^
    "though be careful not to allow all your teammates to sleep on the job.\n" ^
    "Zombies will catch you off-guard while sleeping and we all know the end" ^
    "to that story. Watch those battery levels, keep your tummy full,\n" ^
    "and try not to blow a fuse.\n\n" ^
    "This is the ApOCamplypse. This is 3110.\n\n\n"));
  ANSITerminal.(print_string [green]
    ("Intructions:\n" ^
    "    Typing the command:\n" ^
    "      'Inventory' pulls up the team inventory.\n" ^
    "      'TeamMove' moves the team base to a different location.\n" ^
    "      'Exit' exits the game without saving.\n" ^
    "      'Save' saves the game into a save file.\n" ^
    "      'Work' updates the progress on the assignment for one turn.\n" ^
    "      'Charge' charges a player's laptop for a number of turns.\n" ^
    "      'Eat' replenishes a student's fullness and health.\n" ^
    "      'Transfer' transfers the items in a student's inventory into\n" ^
    "          the team inventory. By doing this, you can share items in\n" ^
    "          the team inventory with others.\n" ^
    "      'Pickup' the items at a student's location into their inventory.\n" ^
    "      'Move' moves a player to a new location.\n" ^
    "      'Take' lets the player take an item from the team inventory.\n" ^
    "          When you send a player off to explore, stock them up!\n"^
    "      'Sleep' puts a player to sleep for a number of turns.\n\n" ^
    "    Miscellaneous:\n" ^
    "      If all students are sleeping at a time, zombies will devour them,\n"^
    "          losing the game.\n" ^
    "      'Move', 'Work', and 'Transfer' take 1 turn, and 'MoveTeam' takes\n" ^
    "          2 turns. All other commands take 0 turns.\n" ^
    "      The game ends if you run out of turns or all team members die.\n" ^
    "          If you running out of turns, you win if progress is 70+%.\n" ^
    "      You can only make progress for students inside the base. Similar,\n"^
    "          you can only use 'Transfer' properly if in the base.\n"));
  print_endline
      ("Please enter 'new' if you would like to start a new game or\n"^
      "'load' if you would like to load a game from a save file.");

  let rec init () =
    let _ = print_string  "> " in
    let inp = read_line () |> ltrim in
    if inp = "new" || inp = "load" then inp
    else
      let _ = print_endline "\nPlease enter only one of 'new' or 'load'." in
      init () in
  let tuple = (match init () with
    | "new" ->
      let rec diff () =
        (try(
          print_newline ();
          print_endline
            ("Please enter the difficulty level, with 1 being the easiest"^
            "\nand 5 being the most difficult.");
          print_string  "> ";
          let diff = read_line () |> String.trim |> int_of_string in
          print_newline ();
          print_endline
            ("Please enter 4 names, each separated by a space.");
          print_string "> ";
          let names = read_line () |> String.trim in let names = names ^ " " in
          let lst =  s_to_l names [] in
          if diff<1 || diff>5 || List.length lst <> 4 || are_same lst
          then failwith "Try again."
          else (diff, "", lst)
        ) with
          | _ -> print_endline "\nPlease try again."; diff ()) in
        diff ()
    | "load" ->
      let rec file () =
        (try(
          print_newline ();
          print_endline
            ("Please enter the save file name, including the extension.");
          print_string  "> ";
          let name = read_line () in
          (0, name, [])
        ) with
          | _ -> print_endline "\nPlease enter a valid file."; file ())
        in file ()
    | _ -> failwith "Should not fail with first input.") in
  main tuple