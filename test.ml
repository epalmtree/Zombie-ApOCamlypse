open OUnit2

let tests = "test suite" >:::
  Test_gameMap.tests@Test_command.tests@Test_zombie.tests@Test_student.tests

let _ = run_test_tt_main tests