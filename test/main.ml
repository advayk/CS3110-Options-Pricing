open OUnit2
open Blackscholes

let () = print_endline "CS 3110 Final Project: Options Pricing!"


let european_call_options_price_test (name : string) (european_option : Blackscholes.european_option) 
(current_stock_price : int) (current_time : int) 
(expected_output : float) :
    test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output (european_call_options_price european_option current_stock_price current_time) 


let blackscholes_test = []



let tests =
  "test suite for blackscholes" >::: List.flatten
         [blackscholes_test]

let _ = run_test_tt_main tests
