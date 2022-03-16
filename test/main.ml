open OUnit2
open Blackscholes
open Maths
open Montecarlo

(* Printing *)

(** [pp_float f] pretty-prints float [f]. *)
let pp_float f = "\"" ^ string_of_float f ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let () = print_endline "CS 3110 Final Project: Options Pricing!"

(** Black Scholes *)
let close_enough a b = Float.abs (a -. b) < 1e-2

let european_call_options_price_test
    (name : string)
    (european_option : Blackscholes.european_option)
    (current_stock_price : float)
    (current_date : date)
    (expected_output : float) : test =
  name >:: fun _ ->
  assert (
    close_enough expected_output
      (european_call_options_price european_option current_stock_price
         current_date))

let euro_option_1_time = create_time 0 0 0 0
let euro_option_1_date =  create_date 1 1 2022 euro_option_1_time 

let euro_option_1 = create_european_option 45. euro_option_1_date 0.02 0.3 

let time1 = create_time 0 0 0 0
let time2 = create_time 0 0 0 0
let date1 = create_date 1 20 2022 time1 
let date2 = create_date 2 6 2022 time2
let date3 = create_date 3 6 2022 time2
let date4 = create_date 1 29 2022 time1
let date5 = create_date 7 8 2021 time1 
let date6 = create_date 8 16 2022 time2 
let date7 = create_date 1 1 2022 time1 
let date8 = create_date 3 21 2022 time1 

(** Maths *)
(* if the difference between the floats is off by less than 1e-3 geometric 
  means then the floats are said to be equal*)
let float_about_eq a b = 
  a-.b |> Float.abs < 1e-3 *. ( a*.b |> Float.abs |> Float.sqrt) 

let diff_between_dates_test (name : string) (date1 : Blackscholes.date) (date2 : Blackscholes.date) (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output (diff_between_dates date1 date2) ~printer: string_of_int

let integrate_test (name : string) (pdf : Maths.pdf) (a : float) (b : float) (expected_output : float) : test =
  name >:: fun _ -> 
    let result = Maths.integrate pdf a b in 
    result |> Printf.printf "\n%8f\n" ; expected_output |> Printf.printf "%8f\n" ;
    assert (result |> float_about_eq expected_output)

  let euro_option_1_expiry_time = create_time 0 0 0 0
  let euro_option_1_expiry_time_date = create_date 2 22 2022 euro_option_1_time

let blackscholes_test = [ 
  european_call_options_price_test "estimated call option price of euro_option_1" euro_option_1 50. euro_option_1_expiry_time_date 6.02 ;
  diff_between_dates_test "difference betwen date1 and date2" date1 date2 18;
  diff_between_dates_test "difference betwen date2 and date3" date2 date3 29;
  diff_between_dates_test "difference between date1 and date4" date1 date4 10;
  diff_between_dates_test "difference between date5 and date6" date5 date6 405;
  diff_between_dates_test "difference between date7 and date8" date7 date8 80
]

let strd_norm_cumulative_dist_test (name : string) (expected : float) (input : float) :
  test = name >:: fun _ -> 
  assert (close_enough expected ((1. +. Float.erf (input /. Float.sqrt 2.))/. 2.))

let cdf_test = [
  strd_norm_cumulative_dist_test "Standard Normal Distribution" 0.5 0.;
  strd_norm_cumulative_dist_test "Standard Normal Distribution Positive Edge" 1. 20.;
  strd_norm_cumulative_dist_test "Standard Normal Distribution Negative Edge" 0. (-1. *. 20.);
  strd_norm_cumulative_dist_test "Standard Normal Distribution" 0.9332 1.5;
  strd_norm_cumulative_dist_test "Standard Normal Distribution" 0.8023 0.851 ;
  strd_norm_cumulative_dist_test "Standard Normal Distribution" 0.7611 0.711 
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

(* Monte Carlo Tests *)

let tuple_close (a, b) = Float.abs (a -. b) < 1e-2 

let deltas_test (name : string) (input : float list) (expected : float list ) : test = name >:: fun _ -> 
  deltas input |> List.combine expected |> List.map tuple_close |> assert_equal (List.map (fun x -> true) expected) 

let monte_carlo_test = [
  deltas_test "Zeros Test" [0.; 0.; 0.; 0.; 0.; 0.; 0.;] [1.; 1.; 1.; 1.; 1.;
  1.]; deltas_test "Generic Real Number Test" [101.5; 101.5; 101.5; 101.5;
  101.5; 101.5; 101.5;] [0.; 0.; 0.; 0.; 0.; 0.;]; deltas_test
  "Empty" [] []; deltas_test "Single" [5.] []; deltas_test "Double" [17.; 15.]
  [-0.117647]; deltas_test "Generic Real Number Test" [12.; 12.; 15.;
  11.; 13.; 0.; 15.; 225.] [0.; 0.25; -0.2666; 0.1818; -1.; 1.; 14.]
]

let tests =
  "Maths :::" >::: List.flatten
         [maths_test; cdf_test; blackscholes_test; monte_carlo_test] 
let diff_between_dates_test (name : string) (date1 : Blackscholes.date) (date2 : Blackscholes.date) (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output (diff_between_dates date1 date2) ~printer: string_of_int

let euro_option_1_time = create_time 0 0 0 0
let euro_option_1_expiration_date = create_date 3 22 2022 euro_option_1_time

  let time1 = create_time 0 0 0 0
  let time2 = create_time 0 0 0 0
  let date1 = create_date 1 20 2022 time1 
  let date2 = create_date 2 6 2022 time2
  let date3 = create_date 3 6 2022 time2
  let date4 = create_date 1 29 2022 time1
  let date5 = create_date 7 8 2021 time1 
  let date6 = create_date 8 16 2022 time2 
  let date7 = create_date 1 1 2022 time2 
  let date8 = create_date 3 21 2022 time2 

let euro_option_1 =
  create_european_option 45. date8 0.02 0.3
  

(* if the difference between the floats is off by less than 1e-3
   geometric means then the floats are said to be equal*)

(** Maths *)
let float_about_eq a b =
  a -. b |> Float.abs < 1e-2 *. (a *. b |> Float.abs |> Float.sqrt)

let integrate_test
    (name : string)
    (pdf : Maths.pdf)
    (a : float)
    (b : float)
    (expected_output : float) : test =
  name >:: fun _ ->
  let result = Maths.integrate pdf a b in
  (* result |> Printf.printf "\n%8f\n";
  expected_output |> Printf.printf "%8f\n"; *)
  assert (result |> float_about_eq expected_output)

let blackscholes_test =
  [
    european_call_options_price_test
      "estimated call option price of euro_option_1" euro_option_1 50.
      date7 6.02;
      diff_between_dates_test "difference betwen date1 and date2" date1 date2 18;
      diff_between_dates_test "difference betwen date2 and date3" date2 date3 29;
      diff_between_dates_test "difference between date1 and date4" date1 date4 10;
      diff_between_dates_test "difference between date5 and date6" date5 date6 405;
      diff_between_dates_test "difference between date7 and date8" date7 date8 80;
  ]

let maths_test =
  [
    integrate_test " f(x) = x^2 numerically integrated small bounds "
      { functn = (fun x -> x *. x); distribution_class = Maths.Other }
      (-1.) 1. 0.66666666667;
    integrate_test " f(x) = ln(x) numerically integrated med bounds "
      {
        functn = (fun x -> Float.log x);
        distribution_class = Maths.Other;
      }
      1. 4. 2.545177444479562;
    integrate_test " f(x) = ln(x) numerically integrated small bounds "
      {
        functn = (fun x -> Float.log x);
        distribution_class = Maths.Other;
      }
      0.01 0.42 (-0.7282985365761028);
    (* stddev = sqrt(1/2pi) ; mean = 0 *)
    integrate_test " normal pdf numerically integrated small bounds "
      {
        functn = (fun x -> exp (-1. *. Float.pi *. x *. x));
        distribution_class = Maths.Other;
      }
      (-0.1) 0.1 0.1979251973547922;
    integrate_test " normal pdf numerically integrated med bounds"
      {
        functn = (fun x -> exp (-1. *. Float.pi *. x *. x));
        distribution_class = Maths.Other;
      }
      (-1.) 1. 0.9878111178151971;
    integrate_test " normal pdf numerically integrated big bounds"
      {
        functn = (fun x -> exp (-1. *. Float.pi *. x *. x));
        distribution_class = Maths.Other;
      }
      (-100.) 100. 1.;
    integrate_test " normal pdf with analytical sol big bounds"
      {
        functn = (fun x -> exp (-1. *. Float.pi *. x *. x));
        distribution_class =
          Maths.Normal
            { stddev = Float.sqrt (1. /. (2. *. Float.pi)); mean = 0. };
      }
      (-999.) 999. 1.;
    integrate_test " normal pdf with analytical sol half bounds"
      {
        functn = (fun x -> exp (-1. *. Float.pi *. x *. x));
        distribution_class =
          Maths.Normal
            { stddev = Float.sqrt (1. /. (2. *. Float.pi)); mean = 6. };
      }
      (-99.) 6. 0.5;
    integrate_test " normal pdf with analytical sol 1 stddev width"
      {
        functn = (fun x -> exp (-1. *. Float.pi *. x *. x));
        distribution_class =
          Maths.Normal
            { stddev = Float.sqrt (1. /. (2. *. Float.pi)); mean = 0. };
      }
      (Float.minus_one *. Float.sqrt (1. /. (2. *. Float.pi)))
      (Float.sqrt (1. /. (2. *. Float.pi)))
      0.6827;
    integrate_test " normal pdf with analytical sol 2 stddev width"
      {
        functn = (fun x -> exp (-1. *. Float.pi *. x *. x));
        distribution_class =
          Maths.Normal
            { stddev = Float.sqrt (1. /. (2. *. Float.pi)); mean = 0. };
      }
      (2. *. Float.minus_one *. Float.sqrt (1. /. (2. *. Float.pi)))
      (2. *. Float.sqrt (1. /. (2. *. Float.pi)))
      0.9545;
    integrate_test " normal pdf with analytical sol 3 stddev width"
      {
        functn = (fun x -> exp (-1. *. Float.pi *. x *. x));
        distribution_class =
          Maths.Normal
            { stddev = Float.sqrt (1. /. (2. *. Float.pi)); mean = 0. };
      }
      (3. *. Float.minus_one *. Float.sqrt (1. /. (2. *. Float.pi)))
      (3. *. Float.sqrt (1. /. (2. *. Float.pi)))
      0.9973;
  ]

let tests =
  "Tests :::" >::: List.flatten [ maths_test; blackscholes_test ]

let _ = run_test_tt_main tests