open OUnit2
open Blackscholes

let () = print_endline "CS 3110 Final Project: Options Pricing!"

let close_enough a b =
  Float.abs (a -. b) < 1e-2
  
  
let european_call_options_price_test (name : string) (european_option : Blackscholes.european_option) 
(current_stock_price : int) (current_time : int) 
(expected_output : float) :
    test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output (european_call_options_price european_option current_stock_price current_time) ~printer:string_of_float

let diff_between_dates_test (name : string) (date1 : Blackscholes.date) (date2 : Blackscholes.date) (expected_output : float) :
    test =
  name >:: fun _ -> assert (close_enough expected_output (diff_between_dates date1 date2))

let euro_option_1 = create_european_option 45 80 0.02 0.3 

let time1 = create_time 0 0 0 0
let time2 = create_time 0 0 0 0

let date1 = create_date 1 20 2022 time1 
let date2 = create_date 2 6 2022 time2

let blackscholes_test = [
  european_call_options_price_test "estimated call option price of euro_option_1" euro_option_1 6 0 6.02;
  diff_between_dates_test "difference betwen date1 and date2" date1 date2 0.04657
]

let tests =
  "test suite for blackscholes" >::: List.flatten
         [blackscholes_test]

let _ = run_test_tt_main tests
