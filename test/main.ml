open OUnit2
open Blackscholes
open Maths

let () = print_endline "CS 3110 Final Project: Options Pricing!"

(** Black Scholes *)
let close_enough a b =
  Float.abs (a -. b) < 1e-2 
  
let european_call_options_price_test (name : string) 
(european_option : Blackscholes.european_option) (current_stock_price : float) 
(current_date : date) (expected_output : float) : test =
  name >:: fun _ ->
  assert (close_enough expected_output (european_call_options_price european_option 
  current_stock_price current_date))

<<<<<<< HEAD
let euro_option_1 = create_european_option 45 80 0.02 0.3 
=======
let diff_between_dates_test (name : string) (date1 : Blackscholes.date) 
(date2 : Blackscholes.date) (expected_output : float) : test =
  name >:: fun _ -> 
    assert (close_enough expected_output (diff_between_dates date1 date2))

let euro_option_1_time = create_time 0 0 0 0
let euro_option_1_date =  create_date 1 1 2022 euro_option_1_time 

let euro_option_1 = create_european_option 45. euro_option_1_date 0.02 0.3 
>>>>>>> 4d616098a16dfa5703bcaa6d3dee8730810ef668
let time1 = create_time 0 0 0 0
let time2 = create_time 0 0 0 0
let date1 = create_date 1 20 2022 time1 
let date2 = create_date 2 6 2022 time2
let date3 = create_date 1 1 2022 time2
let date4 = create_date 1 1 2021 time1


(** Maths *)
(* if the difference between the floats is off by less than 1e-3 geometric 
  means then the floats are said to be equal*)
let float_about_eq a b = 
  a-.b |> Float.abs < 1e-2 *. ( a*.b |> Float.abs |> Float.sqrt) 

let diff_between_dates_test (name : string) (date1 : Blackscholes.date) (date2 : Blackscholes.date) (expected_output : float) : test =
  name >:: fun _ -> assert (close_enough expected_output (diff_between_dates date1 date2))

let integrate_test (name : string) (pdf : Maths.pdf) (a : float) (b : float) (expected_output : float) : test =
  name >:: fun _ -> 
    let result = Maths.integrate pdf a b in 
    result |> Printf.printf "\n%8f\n" ; expected_output |> Printf.printf "%8f\n" ;
    assert (result |> float_about_eq expected_output)

  let euro_option_1_expiry_time = create_time 0 0 0 0
  let euro_option_1_expiry_time_date = create_date 2 22 2022 euro_option_1_time

let blackscholes_test = [ 
  european_call_options_price_test "estimated call option price of euro_option_1" euro_option_1 50. euro_option_1_expiry_time_date 6.02 ;
  diff_between_dates_test "difference betwen date1 and date2" date1 date2 0.04657;
  diff_between_dates_test "difference betwen date2 and date3" date2 date3 0.09863;
  diff_between_dates_test "difference between date1 and date4" date1 date4 1.00273;
]

let strd_norm_cumulative_dist_test (name : string) (expected : float) (input : float) :
  test = name >:: fun _ -> 
  assert_equal (close_enough expected (strd_norm_cumulative_dist input)) true

let cdf_test = [
  strd_norm_cumulative_dist_test "Standard Normal Distribution" 0.5 0.;
  strd_norm_cumulative_dist_test "Standard Normal Distribution Positive Edge" 1. 20.;
  strd_norm_cumulative_dist_test "Standard Normal Distribution Negative Edge" 0. (-1. *. 20.);
  strd_norm_cumulative_dist_test "Standard Normal Distribution" 0.9332 1.5;
  strd_norm_cumulative_dist_test "Standard Normal Distribution" 0.8023 0.851 ;
  strd_norm_cumulative_dist_test "Standard Normal Distribution" 0.7611 0.711 ;
]

let a_normal_pdf x = exp( -1.*.Float.pi*.x*.x ) 

let maths_test = [
  integrate_test " normal pdf numerically integrated small bounds " 
  {functn = a_normal_pdf ; distribution_class = Maths.Other} (-0.1) (0.1) 0.1979251973547922;

  integrate_test " normal pdf numerically integrated med bounds" 
  {functn = a_normal_pdf ; distribution_class = Maths.Other} (-1.) (1.) 0.9878111178151971;

  integrate_test " normal pdf numerically integrated big bounds" 
  {functn = a_normal_pdf ; distribution_class = Maths.Other} (-100.) (100.) 1.;
  
  integrate_test " normal pdf with analytical sol" 
  {functn = a_normal_pdf ; distribution_class = 
  Maths.Normal {stddev = Float.sqrt (1. /. (2.*.Float.pi)); mean = 0.}} 
  (-999.) (999.) 0.5;
]


let tests =
  "Maths :::" >::: List.flatten
         [maths_test; cdf_test; blackscholes_test] 

let _ = run_test_tt_main tests