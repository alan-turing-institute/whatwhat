open Whatwhat
open CalendarLib.Date
open OUnit2

let test_date_of_string _ =
  let this_march_13 = make 2023 3 13 in
  assert_equal (Ok this_march_13) (Utils.date_of_string ~lax:false "2023-03-13");
  assert_equal (Ok this_march_13) (Utils.date_of_string ~lax:true "20230313");
  assert_equal (Ok this_march_13) (Utils.date_of_string ~lax:true "2023-03-13");
  assert_equal (Ok this_march_13) (Utils.date_of_string ~lax:true "2023/03/13");
  assert_equal (Ok this_march_13) (Utils.date_of_string ~lax:true "2023.03.13")
;;

let test_default_start_date _ =
  assert_equal (make 2023 2 1) (Utils.default_start_date ~relative_to:(make 2023 3 13) ());
  assert_equal (make 2022 12 1) (Utils.default_start_date ~relative_to:(make 2023 1 1) ());
  assert_equal
    (make 2022 11 1)
    (Utils.default_start_date ~relative_to:(make 2022 12 31) ())
;;

let test_default_end_date _ =
  assert_equal (make 2023 4 30) (Utils.default_end_date ~relative_to:(make 2023 3 13) ());
  assert_equal (make 2023 2 28) (Utils.default_end_date ~relative_to:(make 2023 1 1) ());
  assert_equal (make 2023 1 31) (Utils.default_end_date ~relative_to:(make 2022 12 31) ());
  (* Check leap year *)
  assert_equal (make 2024 2 29) (Utils.default_end_date ~relative_to:(make 2024 1 1) ())
;;

let suite =
  "Utils"
  >::: [ "date_of_string" >:: test_date_of_string
       ; "default_start_date" >:: test_default_start_date
       ; "default_end_date" >:: test_default_end_date
       ]
;;

let () = run_test_tt_main suite
