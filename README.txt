This file contains documentation and instructions for the project Zombie
ApOCamlypse.

PROJECT AND FILE NAMES
----------------------
This project is named Zombie ApOCamlypse and contains the following files:
Makefile - allows for the following targets: make, make play, make test
Game\ Map.png - an image of the map
save_file.json - a json file containing a loadable game, which can be used in
running the project (see below for those instructions)
main.ml - the access point of this project from outside of the various modules
test.ml - allows for OUnit testing
runner.ml, runner.mli - compile into runner.cmo, handles barebones game flow
gameState.ml, gameState.mli - compile into gameState.mli, handles updates to
the state of the game
command.ml, command.mli - compile into command.cmo, handles input parsing and
constructing an update out of various inputs
gui.ml, gui.mli - compile into gui.cmo, handles the GUI aspect of the project
memory.ml, memory.mli - compile into memory.cmo, handles saving to and loading
from files
gameMap.ml, gameMap.mli - compile into gameMap.cmo, handles the map of a game
student.ml, student.mli - compile into student.cmo, handles updates to the
students in a game
zombie.ml, zombie.mli - compile into zombie.cmo, handles updates to zombies in a
game
test_runner.ml, test_command.ml, test_gui.ml, test_memory.ml, test_gameMap.ml,
test_student.ml, test_zombie.ml - test suites for various modules


INSTALL INSTRUCTIONS
--------------------
This project requires that the following packages and libraries be installed:
Unix
Yojson
ANSITerminal
Graphics


CONFIGURE INSTRUCTIONS
----------------------
This project requires no further configuring.

RUN INSTRUCTIONS
----------------
To run this project, put all of the above files into a directory and once you
are in that directory, type "make play" into the command line. You should see
a description of the project, which is a survival strategy game. Once you have
familiarized yourself with the system, then follow the instructions to either
start a new game or load an old one. In general, this system will allow you to
type anything regardless of case but sensitive to spacing (i.e., typing
"apple pie" will not be the same as typing "apple_pie"). Then, follow the
instructions displayed on the terminal to start up your game. Once a game has
started up, you will see a GUI which displays all the relevant information for
you to decide what to do in the game. Then, type your command into the command
line and then, once the GUI has updated (either with a subset of commands linked
to the one you just entered or a fresh set of commands), repeat the process. If
you wish to exit the game, simply type "exit". If you wish to not carry through
with a chain of commands (i.e., "Sleep" -> not wanting a student to sleep),
simply hit enter until you get a fresh set of commands.

AUTHORS
-------
Erick Palma (ep434)
Lavanya Aprameya (la334)
Paul DeVito (pid7)
Victoria Litvinova (vl242)

                       THIS IS THE ZOMBIE APOCALYSE.
                              THIS IS 3110.