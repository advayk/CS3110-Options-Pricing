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


let time_to_maturity_test (name : string) (date1 : Blackscholes.date) (date2 : Blackscholes.date)  (expected_output : float) :
    test =
  name >:: fun _ ->
  assert_equal expected_output (time_to_maturity date1 date2) 

let euro_option_1 = create_european_option 45 80 0.02 0.3 

let time1 = create_time 22 0 0 0
let time2 = create_time 11 0 0 0

let date1 = create_date 1 1 2022 time1 
let date2 = create_date 2 1 2022 time2

let blackscholes_test = [
  european_call_options_price_test "estimated call option price of euro_option_1" euro_option_1 6 0 6.02;
  time_to_maturity_test "difference betwen date1 and date2" date1 date2 0.0837
]



let tests =
  "test suite for blackscholes" >::: List.flatten
         [blackscholes_test]

let _ = run_test_tt_main tests
