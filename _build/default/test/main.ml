open OUnit2
open OFinance
open Blackscholes
open Maths
open Levy
open Csvreader
open Binomial
open Spread
open Portfolio

let () = print_endline "CS 3110 Final Project: Options Pricing!"


(*CSV Reader*)
(** [csv_test filename expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output]
    with [from_csv filename]. *)
let csv_test (name : string) (filename : string) (expected_output : string) =
  name >:: fun _ -> 
  assert (expected_output = (filename |> load_csv |> from_csv |> first))


(*CSV Reader*)
(** [from_csv_test filename expected_output] constructs an OUnit
  test named [name] that asserts the equality of [expected_output]
  with [from_csv filename]. *)
  let from_csv_test (name : string) (filename : string) (expected_output : string) =
    name >:: fun _ -> 
    assert (expected_output = (filename |> load_csv |> from_csv |> first))
  
(** [get_greek_test name greek row expected_output] constructs an OUnit
  test named [name] that asserts the equality of [expected_output]
  with [get_greek greek data row]. *)
  let get_greek_test (name : string) (greek : string) (data : d list) (symbol : string) (exp : string) (side : string) (expected_output : (float * float) list) =
    name >:: fun _ -> 
    assert (expected_output = get_greek greek data symbol exp side)

(** Black Scholes *)
let close_enough a b = 
  (* print_endline (string_of_float(Float.abs (a -. b))); *)
  Float.abs (a -. b) < 1e-2

let european_call_options_price_test
    (name : string)
    (european_option : Blackscholes.european_option)
    (current_stock_price : float)
    (current_date : date)
    (expected_output : float) : test =
  name >:: fun _ ->
  assert (
    close_enough expected_output
      (european_call_options_price european_option current_stock_price current_date))

let european_put_options_price_test
  (name : string)
  (european_option : Blackscholes.european_option)
  (current_stock_price : float)
  (current_date : date)
  (expected_output : float) : test =
  name >:: fun _ ->
  assert (
    close_enough expected_output
      (european_put_options_price european_option current_stock_price
        current_date))

let diff_between_dates_test (name : string) (date1 : Blackscholes.date) (date2 : Blackscholes.date) (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output (diff_between_dates date1 date2)

let portfolio_val_test (name : string) (stock_prices : float list) (portfolio_weights : float list) (acc : float) (expected_output : float) : test =
  name >:: fun _ -> assert_equal expected_output (portfolio_value stock_prices portfolio_weights acc)  
  
let average_price_test (name : string) (stock_prices : float list) (expected_output : float) : test =
  name >:: fun _ -> assert_equal expected_output (average_price stock_prices)  
    
let normalize_vector_test (name : string) (stock_prices : float list) (expected_output : float list) : test =
  name >:: fun _ -> assert_equal expected_output (normalize_weights stock_prices)  
    
let estimate_risk_first_moment_test (name : string) (stock_prices : float list) (expected_output : float) : test =
  name >:: fun _ -> assert_equal expected_output (estimate_risk_first_moment stock_prices)  

let get_price_at_date_test (name : string) (stock_data : (stock_date * float) list ) (d : stock_date) (expected_output : float) : test =
  name >:: fun _ -> assert_equal expected_output (get_price_at_date stock_data d)  

let get_price_at_time_test (name : string) (stock_data : (stock_time * float) list ) input_time (expected_output : float) : test =
  name >:: fun _ -> assert_equal expected_output (get_price_at_time stock_data input_time)  
    
let expected_returns_test (name : string) (expected_stock_returns :  float list ) (weights : float list) (expected_output : float) : test =
  name >:: fun _ -> assert_equal expected_output (expected_returns expected_stock_returns weights)  
    
let get_price_ticker_test (name : string) (stock_data :  (ticker*float) list ) (tick : ticker) (expected_output : float) : test =
  name >:: fun _ -> assert_equal expected_output (get_price_ticker stock_data tick)  
     
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
  let date9 = create_date 2 28 2022 time2 
  let date10 = create_date 3 1 2022 time2 
  let date11 = create_date 1 16 2022 time2 
  let date12 = create_date 1 10 2022 time2 
  let date13 = create_date 3 22 2022 time2 



(* if the difference between the floats is off by less than 1e-3
   geometric means then the floats are said to be equal*)

(** Maths *)
let float_about_eq a b =
  a -. b |> Float.abs < 1e-1 *. (a *. b |> Float.abs |> Float.sqrt)

let pdf_draw_test
  (name : string)
  (pdf : Maths.pdf)
  (x : float)
  (expected_output : float): test =
  name >:: fun _ ->
    let result = Maths.pdf_value pdf x in
    (* result |> Printf.printf "\n%8f\n";
    expected_output |> Printf.printf "%8f\n"; *)
    assert (float_about_eq expected_output result)

let cdf_test
    (name : string)
    (pdf : Maths.pdf)
    (x : float)
    (expected_output : float) : test =
  name >:: fun _ ->
  let result = Maths.cdf pdf x in
  result |> Printf.printf "\n%8f\n";
  expected_output |> Printf.printf "%8f\n";
  assert (result |> float_about_eq expected_output)

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


let rec print_float_list  = function
  | [] -> ()
  | h :: t -> print_endline (string_of_float h); print_float_list t

  let levy_test
    (name : string)
    (generator : Levy.generator) : test =
  name >:: fun _ ->
  let result = Levy.walk generator in
  (* result |> Printf.printf "\n%8f\n";
  expected_output |> Printf.printf "%8f\n"; *)
  (* print_float_list result ; *)
  (* print_endline "----------------------------------";  *)
  assert true

let euro_option_1 =
  create_european_option 45. date8 0.02 0.3

(* [create_european_option s k t r v ] creates am european_option. 
    Requires:  
    stock price (s): dollars 
    strike price (k): dollars 
    exercise_date (t): in date-time format 
    risk free rate (r): percentage in decimal (i.e 2% = 0.02)
    implied volatility (v) : percentage in decimal (i.e 30% = 0.03) *)
  
    (* let create_european_option (k:float) (d:date) (r:float) (v:float) = {  *)
 
let euro_option_2 =
  create_european_option 250. date8 0.03 0.15
  
let euro_option_3 =
  create_european_option 100. date9 0.05 0.5
  
let euro_option_4 =
  create_european_option 300. date9 0.05 0.3
  
let blackscholes_test =
  [
      european_call_options_price_test
      "estimated call option price of euro_option_1" euro_option_1 70.
      date7 25.20;
      european_call_options_price_test
      "estimated call option price of euro_option_1" euro_option_1 100.
      date7 55.20;
      european_call_options_price_test
      "estimated call option price of euro_option_1" euro_option_1 60.
      date7 15.25;
      european_call_options_price_test
      "estimated call option price of euro_option_1" euro_option_1 100.
      date7 55.20;
      european_call_options_price_test
      "estimated call option price of euro_option_1" euro_option_1 50.
      date7 6.02;
      european_call_options_price_test
      "estimated call option price of euro_option_1" euro_option_1 50.
      date7 6.02;
      european_call_options_price_test
      "estimated call option price of euro_option_3" euro_option_3 500.
      date7 400.81;
      european_call_options_price_test
      "estimated call option price of euro_option_3" euro_option_3 1000.
      date7 900.80;
      european_call_options_price_test
      "estimated call option price of euro_option_3" euro_option_3 100.
      date7 8.38;
      european_put_options_price_test
      "estimated put option price of euro_option_3" euro_option_3 20.
      date7 79.19;
      european_put_options_price_test
      "estimated put option price of euro_option_3" euro_option_3 1.
      date7 98.19;
      european_call_options_price_test
      "estimated call option price of euro_option_4" euro_option_4 300.
      date7 15.61;
      european_call_options_price_test
      "estimated call option price of euro_option_4" euro_option_4 500.
      date7 202.41;
      european_call_options_price_test
      "estimated call option price of euro_option_4" euro_option_4 1000.
      date7 702.41;
      european_call_options_price_test
      "estimated call option price of euro_option_2" euro_option_2 300.
      date7 51.66;
      european_call_options_price_test
      "estimated call option price of euro_option_2" euro_option_2 250.
      date7 7.83;
      european_call_options_price_test
      "estimated call option price of euro_option_2" euro_option_2 500.
      date7 251.64;
      european_put_options_price_test
      "estimated call option price of euro_option_1" euro_option_1 100.
      date7 0.0;
      european_put_options_price_test
      "estimated put option price of euro_option_1" euro_option_1 50.
      date7 0.82;
      european_put_options_price_test
      "estimated put option price of euro_option_1" euro_option_2 300.
      date7 0.02;
      european_put_options_price_test
      "estimated put option price of euro_option_1" euro_option_2 100.
      date7 148.36;
      european_put_options_price_test
      "estimated put option price of euro_option_1" euro_option_2 70.
      date7 178.36;
      european_put_options_price_test
      "estimated put option price of euro_option_1" euro_option_2 1000.
      date7 0.;
      european_put_options_price_test
      "estimated put option price of euro_option_3" euro_option_3 100.
      date7 7.58;
      european_put_options_price_test
      "estimated put option price of euro_option_3" euro_option_3 200.
      date7 0.;
      european_put_options_price_test
      "estimated put option price of euro_option_3" euro_option_3 50.
      date7 49.20;
      european_put_options_price_test
      "estimated put option price of euro_option_3" euro_option_3 30.
      date7 69.19;
      european_put_options_price_test
      "estimated put option price of euro_option_4" euro_option_4 400.
      date7 0.10;
      european_put_options_price_test
      "estimated put option price of euro_option_1" euro_option_4 300.
      date7 13.19;
      european_put_options_price_test
      "estimated put option price of euro_option_1" euro_option_4 200.
      date7 97.59;
      
      diff_between_dates_test "difference betwen date1 and date2" date1 date2 18;
      diff_between_dates_test "difference betwen date2 and date3" date2 date3 29;
      diff_between_dates_test "difference between date1 and date4" date1 date4 10;
      diff_between_dates_test "difference between date5 and date6" date5 date6 405;
      diff_between_dates_test "difference between date7 and date8" date7 date8 80;
      diff_between_dates_test "difference between date9 and date10" date9 date10 2;
      diff_between_dates_test "difference between date11 and date10" date11 date10 45;
      diff_between_dates_test "difference between date11 and date10" date11 date10 45;
      diff_between_dates_test "difference between date11 and date10" date12 date11 7;
  ]

  let stock_date_1 = make_stock_date 1 1 2022
  let stock_date_2 = make_stock_date 1 2 2022
  let stock_date_3 = make_stock_date 4 3 2022
  let stock_date_4 = make_stock_date 5 6 2022

  let stock_time_1 = make_stock_time 1 0 0  
  let stock_time_2 = make_stock_time 1 1 0  
  let stock_time_3 = make_stock_time 1 1 1  

  let stock_time_4 = make_stock_time 1 2 1  

  let appl = make_stock_ticker "AAPL"  
  let amzn = make_stock_ticker "AMZN"  
  let pfe = make_stock_ticker "PFE"  

  let mu = make_stock_ticker "MU" 
  let nvda = make_stock_ticker "NVDA" 



  let portfolio_test = [
    portfolio_val_test "compute value of portfolio 1" [1.;2.] [3.;4.] 0. 11.; 
    portfolio_val_test "0's" [0.;0.] [0.;0.] 0. 0.; 
    portfolio_val_test "empty list" [] [] 0. 0.;
    portfolio_val_test "1's" [1.;1.] [1.;1.] 0. 2.;
    average_price_test "average of stock price 0" [0.;0.] 0.;
    average_price_test "average of stock price" [1.] 1.;
    average_price_test "average of stock price [1,2]" [1.;2.] 1.5;
    average_price_test "average of stock price [2.;2.]" [2.;2.] 2.;
    average_price_test "average of stock price [5.;6.]" [5.;6.] 5.5;
    normalize_vector_test "normalize normal list" [] [];
    normalize_vector_test "normalize vector with 1 element" [1.] [1.];
    get_price_at_date_test "price at one date" [(stock_date_1,10.)] stock_date_1 10.;
    get_price_at_date_test "price at one date" [(stock_date_1,10.);(stock_date_2,20.)] stock_date_2 20.;
    get_price_at_date_test "price at one date" [(stock_date_1,10.);(stock_date_2,20.);(stock_date_3,30.);(stock_date_4,40.)] stock_date_4 40.;
    get_price_at_time_test "price at one date" [(stock_time_1,10.)] stock_time_1 10.;
    get_price_at_time_test "price at one date" [(stock_time_1,10.)] stock_time_1 10.;
    get_price_at_time_test "price at one date" [(stock_time_1,10.);(stock_time_2,20.) ] stock_time_1 10.;
    get_price_at_time_test "price at one date" [(stock_time_1,10.);(stock_time_2,20.);(stock_time_3,30.);(stock_time_4,40.) ] stock_time_4 40.;
    expected_returns_test "price at one date" [1.;2.;3.] [1.;2.;3.] 14.;
    expected_returns_test "price at one date" [1.;2.;4.] [1.;2.;3.] 17.;
    expected_returns_test "price at one date" [3.;2.;4.] [1.;2.;3.] 19.;
    expected_returns_test "price at one date" [3.;2.;4.] [1.;2.;4.] 23.;
    expected_returns_test "price at one date" [3.;2.;4.] [1.;2.;5.] 27.;
    expected_returns_test "price at one date" [3.;2.;4.] [1.;2.;6.] 31.;
    expected_returns_test "price at one date" [6.;2.;4.] [1.;2.;6.] 34.;
    get_price_ticker_test "price at one date" [(appl,20.)] appl 20.;
    get_price_ticker_test "price at one date" [(appl,0.)] appl 0.;
    get_price_ticker_test "price at one date" [(amzn,30.)] amzn 30.;
    get_price_ticker_test "ticker price 2 elements" [(amzn,30.); (pfe,50.)] pfe 50.;
    get_price_ticker_test "ticker price 3 elements" [(amzn,30.); (pfe,50.);(appl,20.)] appl 20.;
    get_price_ticker_test "ticker price 4 elements" [(amzn,30.); (pfe,50.);(appl,20.);(nvda,20.)] nvda 20.;
    get_price_ticker_test "ticker price 4 elements" [(amzn,30.); (pfe,50.);(appl,20.);(nvda,20.)] appl 20.;
    get_price_ticker_test "ticker price 4 elements" [(amzn,30.); (pfe,50.);(appl,20.);(nvda,20.)] amzn 30.;
    get_price_ticker_test "ticker price 5 elements" [(amzn,30.); (pfe,50.);(appl,20.);(mu,100.);(nvda,20.)] mu 100.;

  ]

  let one_row_data = "Data/test.csv" |> load_csv |> from_csv
  let clean_data = "Data/clean_data.csv" |> load_csv |> from_csv
  let csvreader_test = [
    from_csv_test "CSV with a Single Row" "Data/test.csv" "AB";
    get_greek_test "delta of test.csv" "delta" one_row_data "AB" "8/16/19" "call" [(42.5,1.0)];
    get_greek_test "gamma of test.csv" "gamma" one_row_data "AB" "8/16/19" "call" [(42.5,0.0)];
    get_greek_test "theta of test.csv" "theta" one_row_data "AB" "8/16/19" "call" [(42.5,-0.9762)];
    get_greek_test "vega of test.csv" "vega" one_row_data "AB" "8/16/19" "call" [(42.5,0.0)];
    get_greek_test "delta of AAPL 8/23/19 call" "delta" clean_data "AAPL" "8/23/19" "call" [(130.0,1.0);(135.0,1.0);(140.0,1.0);(145.0,1.0);(150.0,1.0);(152.5,1.0);(155.0,1.0);(157.5,1.0);(160.0,0.9999);(162.5,0.9998);(165.0,0.9995);(167.5,0.9989);(170.0,0.9976);(172.5,0.9952);(175.0,0.9907);(177.5,0.9831);(180.0,0.971);(182.5,0.9526);(185.0,0.9264);(187.5,0.9061);(190.0,0.8499);(192.5,0.8019);(195.0,0.7401);(197.5,0.6671);(200.0,0.5812);(202.5,0.4831);(205.0,0.38);(207.5,0.2804);(210.0,0.193);(212.5,0.1246);(215.0,0.0801);(217.5,0.0469);(220.0,0.0301);(222.5,0.0203);(225.0,0.0111);(227.5,0.0059);(230.0,0.003);(232.5,0.0014);(235.0,0.0007);(237.5,0.0003);(240.0,0.0001);(242.5,0.0001);(245.0,0.0);(247.5,0.0);(250.0,0.0);];
    get_greek_test "gamma of AAPL 8/23/19 call" "gamma" clean_data "AAPL" "8/23/19" "call" [(130.0,0.0);(135.0,0.0);(140.0,0.0);(145.0,0.0);(150.0,0.0);(152.5,0.0);(155.0,0.0);(157.5,0.0);(160.0,0.0);(162.5,0.0001);(165.0,0.0001);(167.5,0.0003);(170.0,0.0006);(172.5,0.0011);(175.0,0.002);(177.5,0.0034);(180.0,0.0053);(182.5,0.008);(185.0,0.0113);(187.5,0.0145);(190.0,0.0193);(192.5,0.0239);(195.0,0.0287);(197.5,0.0336);(200.0,0.0381);(202.5,0.0401);(205.0,0.0399);(207.5,0.0365);(210.0,0.0304);(212.5,0.0232);(215.0,0.0166);(217.5,0.011);(220.0,0.0075);(222.5,0.0052);(225.0,0.0031);(227.5,0.0018);(230.0,0.001);(232.5,0.0005);(235.0,0.0002);(237.5,0.0001);(240.0,0.0001);(242.5,0.0);(245.0,0.0);(247.5,0.0);(250.0,0.0);];
    get_greek_test "vega of AAPL 8/23/19 call" "vega" clean_data "AAPL" "8/23/19" "call" [(130.0,0.0);(135.0,0.0);(140.0,0.0);(145.0,0.0);(150.0,0.0001);(152.5,0.0003);(155.0,0.001);(157.5,0.0029);(160.0,0.0079);(162.5,0.0201);(165.0,0.0472);(167.5,0.1031);(170.0,0.21);(172.5,0.3998);(175.0,0.7136);(177.5,1.1971);(180.0,1.8921);(182.5,2.8246);(185.0,3.9913);(187.5,4.7935);(190.0,6.6717);(192.5,7.9588);(195.0,9.273);(197.5,10.3918);(200.0,11.1706);(202.5,11.3977);(205.0,10.8878);(207.5,9.6332);(210.0,7.8343);(212.5,5.8726);(215.0,4.2541);(217.5,2.8004);(220.0,1.9513);(222.5,1.4008);(225.0,0.8375);(227.5,0.4764);(230.0,0.2582);(232.5,0.1336);(235.0,0.0661);(237.5,0.0313);(240.0,0.0142);(242.5,0.0062);(245.0,0.0026);(247.5,0.0011);(250.0,0.0004);];
    get_greek_test "theta of AAPL 8/23/19 call" "theta" clean_data "AAPL" "8/23/19" "call" [(130.0,-2.9846);(135.0,-3.0994);(140.0,-3.2142);(145.0,-3.329);(150.0,-3.4446);(152.5,-3.5043);(155.0,-3.5689);(157.5,-3.6468);(160.0,-3.7583);(162.5,-3.9462);(165.0,-4.2946);(167.5,-4.9518);(170.0,-6.1551);(172.5,-8.2467);(175.0,-11.6649);(177.5,-16.8974);(180.0,-24.3884);(182.5,-34.4086);(185.0,-46.9158);(187.5,-51.9798);(190.0,-74.0091);(192.5,-84.1835);(195.0,-94.2931);(197.5,-100.666);(200.0,-102.1832);(202.5,-100.8078);(205.0,-92.1229);(207.5,-78.7463);(210.0,-62.2291);(212.5,-45.7829);(215.0,-33.4479);(217.5,-21.8595);(220.0,-15.662);(222.5,-11.6665);(225.0,-6.9701);(227.5,-3.9625);(230.0,-2.1471);(232.5,-1.1107);(235.0,-0.5494);(237.5,-0.2602);(240.0,-0.1182);(242.5,-0.0515);(245.0,-0.0216);(247.5,-0.0087);(250.0,-0.0034);];
    get_greek_test "delta of AAPL 8/23/19 put" "delta" clean_data "AAPL" "8/23/19" "put" [(130.0,0.0);(135.0,0.0);(140.0,-0.0022);(145.0,-0.0036);(150.0,-0.005);(152.5,-0.0063);(155.0,-0.0077);(157.5,-0.0097);(160.0,-0.0113);(162.5,-0.0141);(165.0,-0.017);(167.5,-0.0208);(170.0,-0.0254);(172.5,-0.0304);(175.0,-0.0369);(177.5,-0.0462);(180.0,-0.0565);(182.5,-0.0716);(185.0,-0.0913);(187.5,-0.1196);(190.0,-0.1538);(192.5,-0.1994);(195.0,-0.2592);(197.5,-0.3329);(200.0,-0.4193);(202.5,-0.517);(205.0,-0.62);(207.5,-0.7201);(210.0,-0.8108);(212.5,-0.8817);(215.0,-0.939);(217.5,-0.9666);(220.0,-0.9828);(222.5,-0.9917);(225.0,-0.9962);(227.5,-0.9984);(230.0,-0.9994);(232.5,-0.9998);(235.0,-0.9999);(237.5,-1.0);(240.0,-1.0);(242.5,-1.0);(245.0,-1.0);(247.5,-1.0);(250.0,-1.0);];
    get_greek_test "gamma of AAPL 8/23/19 put" "gamma" clean_data "AAPL" "8/23/19" "put" [(130.0,0.0);(135.0,0.0);(140.0,0.0003);(145.0,0.0004);(150.0,0.0006);(152.5,0.0008);(155.0,0.0009);(157.5,0.0012);(160.0,0.0014);(162.5,0.0018);(165.0,0.0022);(167.5,0.0026);(170.0,0.0033);(172.5,0.004);(175.0,0.0049);(177.5,0.0061);(180.0,0.0076);(182.5,0.0096);(185.0,0.0121);(187.5,0.0154);(190.0,0.0193);(192.5,0.0238);(195.0,0.0288);(197.5,0.0336);(200.0,0.0378);(202.5,0.0402);(205.0,0.0399);(207.5,0.0365);(210.0,0.0305);(212.5,0.023);(215.0,0.0148);(217.5,0.0091);(220.0,0.0052);(222.5,0.0028);(225.0,0.0014);(227.5,0.0006);(230.0,0.0003);(232.5,0.0001);(235.0,0.0);(237.5,0.0);(240.0,0.0);(242.5,0.0);(245.0,0.0);(247.5,0.0);(250.0,0.0);];
    get_greek_test "vega of AAPL 8/23/19 put" "vega" clean_data "AAPL" "8/23/19" "put" [(130.0,0.0001);(135.0,0.0004);(140.0,0.199);(145.0,0.3054);(150.0,0.4145);(152.5,0.5104);(155.0,0.6083);(157.5,0.7397);(160.0,0.8462);(162.5,1.0248);(165.0,1.2081);(167.5,1.4321);(170.0,1.6927);(172.5,1.9645);(175.0,2.3063);(177.5,2.77);(180.0,3.2492);(182.5,3.9067);(185.0,4.6922);(187.5,5.706);(190.0,6.7797);(192.5,7.9901);(195.0,9.2614);(197.5,10.3923);(200.0,11.1734);(202.5,11.3976);(205.0,10.8878);(207.5,9.6234);(210.0,7.7403);(212.5,5.6643);(215.0,3.4508);(217.5,2.1262);(220.0,1.2152);(222.5,0.6459);(225.0,0.3201);(227.5,0.1482);(230.0,0.0643);(232.5,0.0262);(235.0,0.01);(237.5,0.0036);(240.0,0.0012);(242.5,0.0004);(245.0,0.0001);(247.5,0.0);(250.0,0.0);];
    get_greek_test "theta of AAPL 8/23/19 put" "theta" clean_data "AAPL" "8/23/19" "put" [(130.0,-0.0009);(135.0,-0.0067);(140.0,-4.5882);(145.0,-6.7299);(150.0,-8.5617);(152.5,-10.2844);(155.0,-11.882);(157.5,-14.0513);(160.0,-15.4276);(162.5,-18.1167);(165.0,-20.5612);(167.5,-23.4542);(170.0,-26.6063);(172.5,-29.4084);(175.0,-32.8838);(177.5,-37.7969);(180.0,-41.923);(182.5,-47.9966);(185.0,-54.7733);(187.5,-63.8825);(190.0,-71.9324);(192.5,-80.5516);(195.0,-89.4057);(197.5,-96.1616);(200.0,-98.4201);(202.5,-95.7597);(205.0,-87.4164);(207.5,-73.6853);(210.0,-55.7496);(212.5,-38.173);(215.0,-19.7824);(217.5,-10.2183);(220.0,-3.6345);(222.5,0.4957);(225.0,2.8815);(227.5,4.1659);(230.0,4.8221);(232.5,5.1512);(235.0,5.3238);(237.5,5.4268);(240.0,5.5012);(242.5,5.5646);(245.0,5.6239);(247.5,5.682);(250.0,5.7395);];
    get_greek_test "delta of AAPL 12/20/19 call" "delta" clean_data "AAPL" "12/20/19" "call" [(130.0,0.9685);(135.0,0.9577);(140.0,0.9441);(145.0,0.9358);(150.0,0.9093);(155.0,0.8932);(160.0,0.8531);(165.0,0.8353);(170.0,0.8147);(175.0,0.7867);(180.0,0.7486);(185.0,0.7071);(190.0,0.6598);(195.0,0.6089);(200.0,0.5546);(205.0,0.4978);(210.0,0.44);(215.0,0.382);(220.0,0.3267);(225.0,0.2743);(230.0,0.2293);(235.0,0.1846);(240.0,0.1486);(245.0,0.1202);(250.0,0.0962);(255.0,0.0744);(260.0,0.0593);(265.0,0.045);(270.0,0.0365);(275.0,0.0285);(280.0,0.0228);(285.0,0.0197);(290.0,0.0167);(295.0,0.0134);(300.0,0.0107);];
    get_greek_test "gamma of AAPL 12/20/19 call" "gamma" clean_data "AAPL" "12/20/19" "call" [(130.0,0.0012);(135.0,0.0016);(140.0,0.0021);(145.0,0.0025);(150.0,0.0032);(155.0,0.0038);(160.0,0.0045);(165.0,0.0052);(170.0,0.006);(175.0,0.007);(180.0,0.0079);(185.0,0.0088);(190.0,0.0097);(195.0,0.0104);(200.0,0.011);(205.0,0.0114);(210.0,0.0115);(215.0,0.0114);(220.0,0.011);(225.0,0.0103);(230.0,0.0094);(235.0,0.0085);(240.0,0.0074);(245.0,0.0064);(250.0,0.0055);(255.0,0.0045);(260.0,0.0038);(265.0,0.0031);(270.0,0.0025);(275.0,0.0021);(280.0,0.0017);(285.0,0.0015);(290.0,0.0013);(295.0,0.001);(300.0,0.0008);];
    get_greek_test "vega of AAPL 12/20/19 call" "vega" clean_data "AAPL" "12/20/19" "call" [(130.0,6.9636);(135.0,9.3489);(140.0,12.1313);(145.0,13.7084);(150.0,18.3232);(155.0,20.8642);(160.0,26.4261);(165.0,28.6193);(170.0,30.9491);(175.0,33.8203);(180.0,37.1988);(185.0,40.2595);(190.0,43.0121);(195.0,45.1545);(200.0,46.5474);(205.0,47.0472);(210.0,46.564);(215.0,45.0594);(220.0,42.6481);(225.0,39.4218);(230.0,35.8721);(235.0,31.5569);(240.0,27.4478);(245.0,23.7224);(250.0,20.2121);(255.0,16.6796);(260.0,13.989);(265.0,11.2383);(270.0,9.4745);(275.0,7.7276);(280.0,6.4073);(285.0,5.6791);(290.0,4.9187);(295.0,4.0853);(300.0,3.3652);];
    get_greek_test "theta of AAPL 12/20/19 call" "theta" clean_data "AAPL" "12/20/19" "call" [(130.0,-3.3322);(135.0,-4.8386);(140.0,-6.5746);(145.0,-7.1849);(150.0,-10.2719);(155.0,-11.3768);(160.0,-15.6366);(165.0,-15.9481);(170.0,-16.1377);(175.0,-16.7509);(180.0,-18.0128);(185.0,-18.9011);(190.0,-19.7694);(195.0,-20.2472);(200.0,-20.374);(205.0,-20.104);(210.0,-19.4894);(215.0,-18.4169);(220.0,-17.145);(225.0,-15.5712);(230.0,-14.1026);(235.0,-12.1476);(240.0,-10.4833);(245.0,-9.068);(250.0,-7.7298);(255.0,-6.329);(260.0,-5.335);(265.0,-4.259);(270.0,-3.6336);(275.0,-2.9743);(280.0,-2.4879);(285.0,-2.2573);(290.0,-1.9882);(295.0,-1.6641);(300.0,-1.3794);];
    get_greek_test "delta of AAPL 12/20/19 put" "delta" clean_data "AAPL" "12/20/19" "put" [(130.0,-0.0332);(135.0,-0.041);(140.0,-0.0508);(145.0,-0.0617);(150.0,-0.0759);(155.0,-0.0921);(160.0,-0.1129);(165.0,-0.1389);(170.0,-0.1678);(175.0,-0.2012);(180.0,-0.2402);(185.0,-0.2835);(190.0,-0.3311);(195.0,-0.3839);(200.0,-0.4393);(205.0,-0.4972);(210.0,-0.5558);(215.0,-0.6134);(220.0,-0.6698);(225.0,-0.7245);(230.0,-0.769);(235.0,-0.7978);(240.0,-0.846);(245.0,-0.8602);(250.0,-0.9195);(255.0,-0.9372);(260.0,-0.9512);(265.0,-0.9621);(270.0,-0.9704);(275.0,-0.9767);(280.0,-0.9814);(285.0,-0.9848);(290.0,-0.9874);(295.0,-0.9892);(300.0,-0.9905);];
    get_greek_test "gamma of AAPL 12/20/19 put" "gamma" clean_data "AAPL" "12/20/19" "put" [(130.0,0.0014);(135.0,0.0017);(140.0,0.0021);(145.0,0.0026);(150.0,0.0031);(155.0,0.0037);(160.0,0.0045);(165.0,0.0053);(170.0,0.0062);(175.0,0.0071);(180.0,0.0081);(185.0,0.009);(190.0,0.0099);(195.0,0.0106);(200.0,0.0112);(205.0,0.0116);(210.0,0.0117);(215.0,0.0115);(220.0,0.0111);(225.0,0.0104);(230.0,0.0095);(235.0,0.0084);(240.0,0.0074);(245.0,0.0066);(250.0,0.0049);(255.0,0.004);(260.0,0.0032);(265.0,0.0025);(270.0,0.0019);(275.0,0.0015);(280.0,0.0011);(285.0,0.0008);(290.0,0.0006);(295.0,0.0005);(300.0,0.0003);];
    get_greek_test "vega of AAPL 12/20/19 put" "vega" clean_data "AAPL" "12/20/19" "put" [(130.0,8.7626);(135.0,10.4249);(140.0,12.3711);(145.0,14.4386);(150.0,16.9309);(155.0,19.5747);(160.0,22.689);(165.0,26.2207);(170.0,29.715);(175.0,33.2652);(180.0,36.8026);(185.0,40.0558);(190.0,42.8727);(195.0,45.126);(200.0,46.5511);(205.0,47.0474);(210.0,46.5256);(215.0,44.9979);(220.0,42.4917);(225.0,39.0534);(230.0,35.4607);(235.0,32.7262);(240.0,27.3178);(245.0,25.5164);(250.0,16.6329);(255.0,13.4456);(260.0,10.7063);(265.0,8.4054);(270.0,6.512);(275.0,4.9826);(280.0,3.768);(285.0,2.8183);(290.0,2.0863);(295.0,1.5295);(300.0,1.111);];
    get_greek_test "theta of AAPL 12/20/19 put" "theta" clean_data "AAPL" "12/20/19" "put" [(130.0,-5.5212);(135.0,-6.3496);(140.0,-7.2925);(145.0,-8.195);(150.0,-9.2987);(155.0,-10.3544);(160.0,-11.6384);(165.0,-13.1405);(170.0,-14.4458);(175.0,-15.6837);(180.0,-16.8965);(185.0,-17.8607);(190.0,-18.4936);(195.0,-19.1002);(200.0,-19.1536);(205.0,-18.7497);(210.0,-18.055);(215.0,-17.0182);(220.0,-15.5495);(225.0,-13.696);(230.0,-12.1712);(235.0,-11.4627);(240.0,-8.6315);(245.0,-8.3665);(250.0,-3.8057);(255.0,-2.5795);(260.0,-1.5102);(265.0,-0.5941);(270.0,0.1796);(275.0,0.8263);(280.0,1.3632);(285.0,1.8078);(290.0,2.1765);(295.0,2.4842);(300.0,2.7437);];
    get_greek_test "delta of AAPL 1/15/21 call" "delta" clean_data "AAPL" "1/15/21" "call" [(75.0,0.9629);(80.0,0.9593);(85.0,0.9547);(90.0,0.949);(95.0,0.942);(100.0,0.9339);(105.0,0.9244);(110.0,0.9137);(115.0,0.9017);(120.0,0.8885);(125.0,0.8792);(130.0,0.8682);(135.0,0.8583);(140.0,0.8397);(145.0,0.8251);(150.0,0.8096);(155.0,0.7849);(160.0,0.7708);(165.0,0.7479);(170.0,0.7247);(175.0,0.7007);(180.0,0.6744);(185.0,0.6477);(190.0,0.6202);(195.0,0.5918);(200.0,0.564);(205.0,0.5355);(210.0,0.5064);(215.0,0.4759);(220.0,0.4475);(225.0,0.4182);(230.0,0.3902);(235.0,0.3638);(240.0,0.3366);(245.0,0.3115);(250.0,0.2884);(255.0,0.266);(260.0,0.2455);(265.0,0.2243);(270.0,0.2095);(280.0,0.1726);(290.0,0.1411);(300.0,0.118);(310.0,0.0967);(320.0,0.0841);(330.0,0.0712);(340.0,0.053);(350.0,0.049);(360.0,0.0403);(370.0,0.0341);(380.0,0.0295);];
    get_greek_test "gamma of AAPL 1/15/21 call" "gamma" clean_data "AAPL" "1/15/21" "call" [(75.0,0.0002);(80.0,0.0003);(85.0,0.0004);(90.0,0.0006);(95.0,0.0007);(100.0,0.0009);(105.0,0.001);(110.0,0.0012);(115.0,0.0014);(120.0,0.0016);(125.0,0.0018);(130.0,0.0021);(135.0,0.0023);(140.0,0.0025);(145.0,0.0028);(150.0,0.0031);(155.0,0.0033);(160.0,0.0037);(165.0,0.004);(170.0,0.0043);(175.0,0.0046);(180.0,0.0048);(185.0,0.0051);(190.0,0.0054);(195.0,0.0055);(200.0,0.0055);(205.0,0.0057);(210.0,0.0058);(215.0,0.0059);(220.0,0.0059);(225.0,0.0059);(230.0,0.0059);(235.0,0.0058);(240.0,0.0057);(245.0,0.0056);(250.0,0.0054);(255.0,0.0052);(260.0,0.005);(265.0,0.0048);(270.0,0.0046);(280.0,0.0041);(290.0,0.0037);(300.0,0.0032);(310.0,0.0028);(320.0,0.0025);(330.0,0.0022);(340.0,0.0018);(350.0,0.0016);(360.0,0.0014);(370.0,0.0012);(380.0,0.0011);];
    get_greek_test "vega of AAPL 1/15/21 call" "vega" clean_data "AAPL" "1/15/21" "call" [(75.0,5.1672);(80.0,7.1902);(85.0,9.6293);(90.0,12.4785);(95.0,15.7167);(100.0,19.3097);(105.0,23.2126);(110.0,27.3729);(115.0,31.733);(120.0,36.2327);(125.0,39.2573);(130.0,42.6332);(135.0,45.5605);(140.0,50.6738);(145.0,54.4281);(150.0,58.1811);(155.0,63.6403);(160.0,66.5093);(165.0,70.8025);(170.0,74.7085);(175.0,78.3151);(180.0,81.7831);(185.0,84.806);(190.0,87.4109);(195.0,89.5743);(200.0,91.1823);(205.0,92.3287);(210.0,92.972);(215.0,93.0806);(220.0,92.6622);(225.0,91.7021);(230.0,90.2793);(235.0,88.4828);(240.0,86.1537);(245.0,83.561);(250.0,80.785);(255.0,77.7385);(260.0,74.6131);(265.0,71.0484);(270.0,68.3326);(280.0,60.7718);(290.0,53.2896);(300.0,47.1217);(310.0,40.8501);(320.0,36.8453);(330.0,32.4713);(340.0,25.829);(350.0,24.2682);(360.0,20.7471);(370.0,18.0654);(380.0,16.0322);];
    get_greek_test "theta of AAPL 1/15/21 call" "theta" clean_data "AAPL" "1/15/21" "call" [(75.0,1.7331);(80.0,1.3544);(85.0,0.9212);(90.0,0.4346);(95.0,-0.1022);(100.0,-0.6845);(105.0,-1.306);(110.0,-1.9593);(115.0,-2.6366);(120.0,-3.3297);(125.0,-3.6528);(130.0,-4.0188);(135.0,-4.2356);(140.0,-4.9584);(145.0,-5.3071);(150.0,-5.6156);(155.0,-6.4767);(160.0,-6.4118);(165.0,-6.8833);(170.0,-7.2409);(175.0,-7.5091);(180.0,-7.8719);(185.0,-8.1197);(190.0,-8.1868);(195.0,-8.4967);(200.0,-8.7834);(205.0,-8.8404);(210.0,-8.8146);(215.0,-8.6613);(220.0,-8.5962);(225.0,-8.4166);(230.0,-8.2359);(235.0,-8.0558);(240.0,-7.774);(245.0,-7.507);(250.0,-7.2486);(255.0,-6.9577);(260.0,-6.6752);(265.0,-6.3168);(270.0,-6.1364);(280.0,-5.385);(290.0,-4.6714);(300.0,-4.143);(310.0,-3.5792);(320.0,-3.285);(330.0,-2.9158);(340.0,-2.2583);(350.0,-2.1864);(360.0,-1.8692);(370.0,-1.6369);(380.0,-1.4676);];
    get_greek_test "delta of AAPL 1/15/21 put" "delta" clean_data "AAPL" "1/15/21" "put" [(75.0,-0.0065);(80.0,-0.0097);(85.0,-0.0139);(90.0,-0.0192);(95.0,-0.0256);(100.0,-0.0313);(105.0,-0.0395);(110.0,-0.0472);(115.0,-0.0557);(120.0,-0.0635);(125.0,-0.0737);(130.0,-0.0891);(135.0,-0.0912);(140.0,-0.119);(145.0,-0.1376);(150.0,-0.1543);(155.0,-0.1744);(160.0,-0.1928);(165.0,-0.2181);(170.0,-0.243);(175.0,-0.2685);(180.0,-0.2945);(185.0,-0.3224);(190.0,-0.3506);(195.0,-0.3794);(200.0,-0.4083);(205.0,-0.4373);(210.0,-0.467);(215.0,-0.496);(220.0,-0.5245);(225.0,-0.5521);(230.0,-0.5799);(235.0,-0.6056);(240.0,-0.6292);(245.0,-0.6545);(250.0,-0.6758);(255.0,-0.7043);(260.0,-0.7178);(265.0,-0.739);(270.0,-0.7704);(280.0,-0.7763);(290.0,-0.8041);(300.0,-0.8357);(310.0,-0.8438);(320.0,-0.8625);(330.0,-0.8787);(340.0,-0.8926);(350.0,-0.9046);(360.0,-0.9148);(370.0,-0.9235);(380.0,-0.9308);];
    get_greek_test "gamma of AAPL 1/15/21 put" "gamma" clean_data "AAPL" "1/15/21" "put" [(75.0,0.0002);(80.0,0.0003);(85.0,0.0004);(90.0,0.0005);(95.0,0.0007);(100.0,0.0008);(105.0,0.001);(110.0,0.0012);(115.0,0.0013);(120.0,0.0015);(125.0,0.0018);(130.0,0.002);(135.0,0.0023);(140.0,0.0026);(145.0,0.0029);(150.0,0.0032);(155.0,0.0035);(160.0,0.0038);(165.0,0.0041);(170.0,0.0044);(175.0,0.0047);(180.0,0.005);(185.0,0.0052);(190.0,0.0054);(195.0,0.0056);(200.0,0.0057);(205.0,0.0058);(210.0,0.0059);(215.0,0.006);(220.0,0.006);(225.0,0.0059);(230.0,0.0059);(235.0,0.0058);(240.0,0.0056);(245.0,0.0055);(250.0,0.0053);(255.0,0.0052);(260.0,0.005);(265.0,0.0048);(270.0,0.0046);(280.0,0.0042);(290.0,0.0038);(300.0,0.0034);(310.0,0.0031);(320.0,0.0028);(330.0,0.0025);(340.0,0.0022);(350.0,0.0019);(360.0,0.0017);(370.0,0.0015);(380.0,0.0013);];
    get_greek_test "vega of AAPL 1/15/21 put" "vega" clean_data "AAPL" "1/15/21" "put" [(75.0,4.3747);(80.0,6.2176);(85.0,8.483);(90.0,11.1745);(95.0,14.2794);(100.0,16.8597);(105.0,20.4131);(110.0,23.5343);(115.0,26.8423);(120.0,29.7305);(125.0,33.329);(130.0,38.4523);(135.0,39.1248);(140.0,47.3787);(145.0,52.3964);(150.0,56.5387);(155.0,61.1664);(160.0,65.0708);(165.0,69.9444);(170.0,74.2204);(175.0,78.0898);(180.0,81.5603);(185.0,84.7381);(190.0,87.4102);(195.0,89.5959);(200.0,91.2599);(205.0,92.3892);(210.0,93.0034);(215.0,93.074);(220.0,92.6348);(225.0,91.7226);(230.0,90.322);(235.0,88.5903);(240.0,86.6166);(245.0,84.0816);(250.0,81.6123);(255.0,77.8042);(260.0,75.7958);(265.0,72.3514);(270.0,66.5878);(280.0,65.4023);(290.0,59.4418);(300.0,51.7465);(310.0,49.5824);(320.0,44.3191);(330.0,39.4045);(340.0,34.8691);(350.0,30.7254);(360.0,26.9721);(370.0,23.5976);(380.0,20.5832);];
    get_greek_test "theta of AAPL 1/15/21 put" "theta" clean_data "AAPL" "1/15/21" "put" [(75.0,-0.5617);(80.0,-0.7974);(85.0,-1.0867);(90.0,-1.4297);(95.0,-1.8244);(100.0,-2.108);(105.0,-2.5359);(110.0,-2.8614);(115.0,-3.1907);(120.0,-3.411);(125.0,-3.7264);(130.0,-4.3019);(135.0,-3.9858);(140.0,-5.1249);(145.0,-5.6317);(150.0,-5.9031);(155.0,-6.2851);(160.0,-6.4303);(165.0,-6.9212);(170.0,-7.2706);(175.0,-7.531);(180.0,-7.6879);(185.0,-7.9252);(190.0,-8.0409);(195.0,-8.1236);(200.0,-8.1637);(205.0,-8.1621);(210.0,-8.0328);(215.0,-7.9029);(220.0,-7.731);(225.0,-7.5246);(230.0,-7.2405);(235.0,-6.9755);(240.0,-6.7181);(245.0,-6.3239);(250.0,-6.0195);(255.0,-5.3918);(260.0,-5.2543);(265.0,-4.7826);(270.0,-3.8966);(280.0,-4.0646);(290.0,-3.2906);(300.0,-2.2325);(310.0,-2.015);(320.0,-1.2846);(330.0,-0.5868);(340.0,0.075);(350.0,0.6994);(360.0,1.2863);(370.0,1.8368);(380.0,2.3525);];
    get_greek_test "delta of AAPL 6/19/20 call" "delta" clean_data "AAPL" "6/19/20" "call" [(75.0,0.9771);(80.0,0.9744);(85.0,0.9707);(90.0,0.966);(95.0,0.96);(100.0,0.9528);(105.0,0.948);(110.0,0.9336);(115.0,0.9271);(120.0,0.9201);(125.0,0.9161);(130.0,0.9124);(135.0,0.8975);(140.0,0.8739);(145.0,0.8642);(150.0,0.8568);(155.0,0.8367);(160.0,0.8162);(165.0,0.7914);(170.0,0.7589);(175.0,0.7283);(180.0,0.6974);(185.0,0.6642);(190.0,0.6297);(195.0,0.594);(200.0,0.5572);(205.0,0.5196);(210.0,0.4815);(215.0,0.4441);(220.0,0.4071);(225.0,0.3687);(230.0,0.3343);(235.0,0.3003);(240.0,0.2686);(245.0,0.2392);(250.0,0.2127);(260.0,0.1646);(270.0,0.1265);(280.0,0.0966);(290.0,0.073);(300.0,0.0568);(310.0,0.0434);(320.0,0.0348);(330.0,0.0271);(340.0,0.0224);];
    get_greek_test "gamma of AAPL 6/19/20 call" "gamma" clean_data "AAPL" "6/19/20" "call" [(75.0,0.0002);(80.0,0.0003);(85.0,0.0004);(90.0,0.0005);(95.0,0.0006);(100.0,0.0008);(105.0,0.0009);(110.0,0.0012);(115.0,0.0014);(120.0,0.0015);(125.0,0.0017);(130.0,0.0019);(135.0,0.0023);(140.0,0.0026);(145.0,0.003);(150.0,0.0034);(155.0,0.0038);(160.0,0.0043);(165.0,0.0048);(170.0,0.0052);(175.0,0.0056);(180.0,0.006);(185.0,0.0064);(190.0,0.0067);(195.0,0.0071);(200.0,0.0073);(205.0,0.0075);(210.0,0.0076);(215.0,0.0076);(220.0,0.0076);(225.0,0.0075);(230.0,0.0073);(235.0,0.0071);(240.0,0.0068);(245.0,0.0064);(250.0,0.006);(260.0,0.0052);(270.0,0.0044);(280.0,0.0036);(290.0,0.003);(300.0,0.0024);(310.0,0.0019);(320.0,0.0016);(330.0,0.0013);(340.0,0.0011);];
    get_greek_test "vega of AAPL 6/19/20 call" "vega" clean_data "AAPL" "6/19/20" "call" [(75.0,2.6926);(80.0,3.9465);(85.0,5.5318);(90.0,7.462);(95.0,9.737);(100.0,12.3432);(105.0,13.9791);(110.0,18.6164);(115.0,20.5501);(120.0,22.5638);(125.0,23.6866);(130.0,24.7181);(135.0,28.6287);(140.0,34.276);(145.0,36.4408);(150.0,38.0198);(155.0,42.0937);(160.0,45.904);(165.0,50.0894);(170.0,54.9311);(175.0,58.8795);(180.0,62.3224);(185.0,65.4392);(190.0,68.0523);(195.0,70.1263);(200.0,71.6123);(205.0,72.4563);(210.0,72.6275);(215.0,72.1261);(220.0,70.9745);(225.0,69.0791);(230.0,66.755);(235.0,63.8563);(240.0,60.6028);(245.0,57.0622);(250.0,53.4378);(260.0,45.6626);(270.0,38.2677);(280.0,31.5641);(290.0,25.5946);(300.0,21.0635);(310.0,17.0142);(320.0,14.198);(330.0,11.5552);(340.0,9.8568);];
    get_greek_test "theta of AAPL 6/19/20 call" "theta" clean_data "AAPL" "6/19/20" "call" [(75.0,1.8178);(80.0,1.3688);(85.0,0.8312);(90.0,0.2018);(95.0,-0.5192);(100.0,-1.3279);(105.0,-1.7466);(110.0,-3.2484);(115.0,-3.6695);(120.0,-4.0681);(125.0,-4.0764);(130.0,-4.0217);(135.0,-4.9244);(140.0,-6.4879);(145.0,-6.5947);(150.0,-6.3904);(155.0,-7.1004);(160.0,-7.6398);(165.0,-8.3056);(170.0,-9.4135);(175.0,-10.1391);(180.0,-10.6326);(185.0,-11.1276);(190.0,-11.4833);(195.0,-11.5615);(200.0,-11.8075);(205.0,-11.8191);(210.0,-11.6994);(215.0,-11.5402);(220.0,-11.2657);(225.0,-10.7622);(230.0,-10.3505);(235.0,-9.807);(240.0,-9.2441);(245.0,-8.6528);(250.0,-8.082);(260.0,-6.8356);(270.0,-5.7054);(280.0,-4.7023);(290.0,-3.8108);(300.0,-3.1673);(310.0,-2.5724);(320.0,-2.1806);(330.0,-1.7901);(340.0,-1.5562);];
    get_greek_test "delta of AAPL 6/19/20 put" "delta" clean_data "AAPL" "6/19/20" "put" [(75.0,-0.002);(80.0,-0.0035);(85.0,-0.0056);(90.0,-0.0087);(95.0,-0.0128);(100.0,-0.0181);(105.0,-0.0248);(110.0,-0.0303);(115.0,-0.0368);(120.0,-0.0443);(125.0,-0.0534);(130.0,-0.0636);(135.0,-0.0748);(140.0,-0.089);(145.0,-0.1068);(150.0,-0.1233);(155.0,-0.1452);(160.0,-0.1678);(165.0,-0.1924);(170.0,-0.2206);(175.0,-0.2496);(180.0,-0.2822);(185.0,-0.3154);(190.0,-0.3511);(195.0,-0.3881);(200.0,-0.4258);(205.0,-0.4639);(210.0,-0.5041);(215.0,-0.5408);(220.0,-0.5782);(225.0,-0.6167);(230.0,-0.654);(235.0,-0.6899);(240.0,-0.7251);(245.0,-0.7446);(250.0,-0.7598);(260.0,-0.823);(270.0,-0.8762);(280.0,-0.9038);(290.0,-0.9249);(300.0,-0.9408);(310.0,-0.9525);(320.0,-0.9611);(330.0,-0.9674);(340.0,-0.9718);];
    get_greek_test "gamma of AAPL 6/19/20 put" "gamma" clean_data "AAPL" "6/19/20" "put" [(75.0,0.0001);(80.0,0.0001);(85.0,0.0002);(90.0,0.0003);(95.0,0.0004);(100.0,0.0006);(105.0,0.0008);(110.0,0.0009);(115.0,0.0011);(120.0,0.0014);(125.0,0.0016);(130.0,0.0019);(135.0,0.0022);(140.0,0.0026);(145.0,0.003);(150.0,0.0034);(155.0,0.0038);(160.0,0.0043);(165.0,0.0048);(170.0,0.0052);(175.0,0.0057);(180.0,0.0061);(185.0,0.0066);(190.0,0.0069);(195.0,0.0072);(200.0,0.0075);(205.0,0.0076);(210.0,0.0079);(215.0,0.0078);(220.0,0.0077);(225.0,0.0076);(230.0,0.0075);(235.0,0.0072);(240.0,0.0069);(245.0,0.0064);(250.0,0.006);(260.0,0.0052);(270.0,0.0042);(280.0,0.0033);(290.0,0.0026);(300.0,0.002);(310.0,0.0015);(320.0,0.0012);(330.0,0.0009);(340.0,0.0006);];
    get_greek_test "vega of AAPL 6/19/20 put" "vega" clean_data "AAPL" "6/19/20" "put" [(75.0,1.196);(80.0,1.943);(85.0,2.981);(90.0,4.3543);(95.0,6.0955);(100.0,8.2227);(105.0,10.7378);(110.0,12.6716);(115.0,14.8942);(120.0,17.2818);(125.0,20.086);(130.0,23.0179);(135.0,26.072);(140.0,29.7353);(145.0,33.9522);(150.0,37.6067);(155.0,42.0526);(160.0,46.2227);(165.0,50.3497);(170.0,54.566);(175.0,58.374);(180.0,62.07);(185.0,65.2206);(190.0,67.9615);(195.0,70.125);(200.0,71.6403);(205.0,72.4751);(210.0,72.6088);(215.0,72.0637);(220.0,70.8445);(225.0,68.8798);(230.0,66.2752);(235.0,63.082);(240.0,59.2681);(245.0,56.8458);(250.0,54.7986);(260.0,44.6709);(270.0,33.7563);(280.0,27.0048);(290.0,21.1937);(300.0,16.3533);(310.0,12.4301);(320.0,9.3229);(330.0,6.91);(340.0,5.0679);];
    get_greek_test "theta of AAPL 6/19/20 put" "theta" clean_data "AAPL" "6/19/20" "put" [(75.0,-0.2829);(80.0,-0.4592);(85.0,-0.7042);(90.0,-1.0279);(95.0,-1.4379);(100.0,-1.9383);(105.0,-2.5291);(110.0,-2.9048);(115.0,-3.3282);(120.0,-3.7568);(125.0,-4.2653);(130.0,-4.7598);(135.0,-5.2341);(140.0,-5.8466);(145.0,-6.5954);(150.0,-7.0837);(155.0,-7.8224);(160.0,-8.4141);(165.0,-8.9579);(170.0,-9.5687);(175.0,-9.9851);(180.0,-10.4924);(185.0,-10.7377);(190.0,-10.9935);(195.0,-11.2081);(200.0,-11.2441);(205.0,-11.208);(210.0,-10.8362);(215.0,-10.7077);(220.0,-10.3254);(225.0,-9.7276);(230.0,-9.0667);(235.0,-8.3345);(240.0,-7.499);(245.0,-7.2826);(250.0,-7.1565);(260.0,-4.9393);(270.0,-2.8219);(280.0,-1.6646);(290.0,-0.6358);(300.0,0.2594);(310.0,1.0281);(320.0,1.6841);(330.0,2.2439);(340.0,2.7247);];
    get_greek_test "delta of AAPL 9/18/20 call" "delta" clean_data "AAPL" "9/18/20" "call" [(95.0,0.965);(100.0,0.9596);(105.0,0.9529);(110.0,0.9447);(115.0,0.9348);(120.0,0.9233);(125.0,0.9102);(130.0,0.8995);(135.0,0.8853);(140.0,0.8642);(145.0,0.8574);(150.0,0.8396);(155.0,0.8196);(160.0,0.7971);(165.0,0.7684);(170.0,0.7435);(175.0,0.7163);(180.0,0.6877);(185.0,0.6572);(190.0,0.626);(195.0,0.594);(200.0,0.5609);(205.0,0.5277);(210.0,0.4943);(215.0,0.4611);(220.0,0.4275);(225.0,0.3952);(230.0,0.3664);(235.0,0.3334);(240.0,0.3013);(250.0,0.2481);(260.0,0.2041);(270.0,0.1635);(280.0,0.1287);(290.0,0.1037);(300.0,0.0826);(310.0,0.0666);(320.0,0.0531);];
    get_greek_test "gamma of AAPL 9/18/20 call" "gamma" clean_data "AAPL" "9/18/20" "call" [(95.0,0.0004);(100.0,0.0006);(105.0,0.0008);(110.0,0.001);(115.0,0.0012);(120.0,0.0015);(125.0,0.0017);(130.0,0.002);(135.0,0.0023);(140.0,0.0026);(145.0,0.0029);(150.0,0.0033);(155.0,0.0037);(160.0,0.0041);(165.0,0.0044);(170.0,0.0047);(175.0,0.0051);(180.0,0.0055);(185.0,0.0057);(190.0,0.006);(195.0,0.0063);(200.0,0.0065);(205.0,0.0066);(210.0,0.0067);(215.0,0.0068);(220.0,0.0068);(225.0,0.0067);(230.0,0.0066);(235.0,0.0065);(240.0,0.0063);(250.0,0.0058);(260.0,0.0052);(270.0,0.0046);(280.0,0.004);(290.0,0.0034);(300.0,0.0029);(310.0,0.0024);(320.0,0.002);];
    get_greek_test "vega of AAPL 9/18/20 call" "vega" clean_data "AAPL" "9/18/20" "call" [(95.0,7.0144);(100.0,9.4374);(105.0,12.2955);(110.0,15.5715);(115.0,19.2309);(120.0,23.2245);(125.0,27.4916);(130.0,30.7299);(135.0,34.8048);(140.0,40.3664);(145.0,42.0538);(150.0,46.2498);(155.0,50.6014);(160.0,55.065);(165.0,60.1691);(170.0,64.1102);(175.0,67.9198);(180.0,71.4002);(185.0,74.5511);(190.0,77.1903);(195.0,79.3255);(200.0,80.9218);(205.0,81.9188);(210.0,82.3174);(215.0,82.1142);(220.0,81.3027);(225.0,79.9381);(230.0,78.2288);(235.0,75.6935);(240.0,72.6101);(250.0,66.0916);(260.0,59.2495);(270.0,51.6713);(280.0,44.0316);(290.0,37.7805);(300.0,31.9467);(310.0,27.1248);(320.0,22.7404);];
    get_greek_test "theta of AAPL 9/18/20 call" "theta" clean_data "AAPL" "9/18/20" "call" [(95.0,0.7725);(100.0,0.274);(105.0,-0.2931);(110.0,-0.9257);(115.0,-1.6178);(120.0,-2.3611);(125.0,-3.1456);(130.0,-3.6325);(135.0,-4.2934);(140.0,-5.3584);(145.0,-5.228);(150.0,-5.7858);(155.0,-6.356);(160.0,-6.9458);(165.0,-7.8571);(170.0,-8.3058);(175.0,-8.7609);(180.0,-9.1433);(185.0,-9.5568);(190.0,-9.8261);(195.0,-10.0168);(200.0,-10.0776);(205.0,-10.1589);(210.0,-10.1377);(215.0,-10.0481);(220.0,-9.8451);(225.0,-9.6164);(230.0,-9.4563);(235.0,-8.9882);(240.0,-8.4744);(250.0,-7.6483);(260.0,-6.8606);(270.0,-5.9316);(280.0,-5.0034);(290.0,-4.311);(300.0,-3.6515);(310.0,-3.1211);(320.0,-2.6267);];
    get_greek_test "delta of AAPL 9/18/20 put" "delta" clean_data "AAPL" "9/18/20" "put" [(95.0,-0.0196);(100.0,-0.0268);(105.0,-0.0311);(110.0,-0.0389);(115.0,-0.0465);(120.0,-0.0531);(125.0,-0.0638);(130.0,-0.075);(135.0,-0.0903);(140.0,-0.1031);(145.0,-0.1218);(150.0,-0.1406);(155.0,-0.1605);(160.0,-0.1818);(165.0,-0.2072);(170.0,-0.233);(175.0,-0.2607);(180.0,-0.2897);(185.0,-0.3203);(190.0,-0.3516);(195.0,-0.3841);(200.0,-0.4175);(205.0,-0.4508);(210.0,-0.4851);(215.0,-0.5163);(220.0,-0.553);(225.0,-0.5846);(230.0,-0.6162);(235.0,-0.6443);(240.0,-0.6756);(250.0,-0.7275);(260.0,-0.7855);(270.0,-0.8214);(280.0,-0.8317);(290.0,-0.8469);(300.0,-0.8627);(310.0,-0.8704);(320.0,-0.8881);];
    get_greek_test "gamma of AAPL 9/18/20 put" "gamma" clean_data "AAPL" "9/18/20" "put" [(95.0,0.0006);(100.0,0.0007);(105.0,0.0009);(110.0,0.0011);(115.0,0.0013);(120.0,0.0015);(125.0,0.0017);(130.0,0.002);(135.0,0.0023);(140.0,0.0026);(145.0,0.0029);(150.0,0.0033);(155.0,0.0037);(160.0,0.0041);(165.0,0.0044);(170.0,0.0048);(175.0,0.0051);(180.0,0.0055);(185.0,0.0058);(190.0,0.0061);(195.0,0.0063);(200.0,0.0066);(205.0,0.0067);(210.0,0.0068);(215.0,0.0068);(220.0,0.0069);(225.0,0.0068);(230.0,0.0067);(235.0,0.0065);(240.0,0.0063);(250.0,0.0058);(260.0,0.0053);(270.0,0.0046);(280.0,0.0041);(290.0,0.0036);(300.0,0.0032);(310.0,0.0029);(320.0,0.0026);];
    get_greek_test "vega of AAPL 9/18/20 put" "vega" clean_data "AAPL" "9/18/20" "put" [(95.0,10.0109);(100.0,13.0317);(105.0,14.737);(110.0,17.7055);(115.0,20.4354);(120.0,22.74);(125.0,26.2478);(130.0,29.7236);(135.0,34.1433);(140.0,37.6444);(145.0,42.3756);(150.0,46.7591);(155.0,51.0552);(160.0,55.2421);(165.0,59.7846);(170.0,63.8902);(175.0,67.792);(180.0,71.341);(185.0,74.5024);(190.0,77.1696);(195.0,79.3322);(200.0,80.9389);(205.0,81.9316);(210.0,82.3211);(215.0,82.1233);(220.0,81.2138);(225.0,79.8377);(230.0,77.9101);(235.0,75.7097);(240.0,72.7185);(250.0,66.4084);(260.0,57.2106);(270.0,50.2283);(280.0,48.0177);(290.0,44.5837);(300.0,40.7404);(310.0,38.7919);(320.0,34.0176);];
    get_greek_test "theta of AAPL 9/18/20 put" "theta" clean_data "AAPL" "9/18/20" "put" [(95.0,-1.7629);(100.0,-2.3033);(105.0,-2.5065);(110.0,-2.973);(115.0,-3.3478);(120.0,-3.5819);(125.0,-4.0607);(130.0,-4.4896);(135.0,-5.1163);(140.0,-5.4591);(145.0,-6.0987);(150.0,-6.6114);(155.0,-7.0665);(160.0,-7.4557);(165.0,-7.9973);(170.0,-8.3898);(175.0,-8.763);(180.0,-9.066);(185.0,-9.3338);(190.0,-9.4713);(195.0,-9.5465);(200.0,-9.5185);(205.0,-9.5108);(210.0,-9.322);(215.0,-9.3007);(220.0,-8.7846);(225.0,-8.4975);(230.0,-8.1014);(235.0,-7.7572);(240.0,-7.1677);(250.0,-6.1684);(260.0,-4.58);(270.0,-3.6121);(280.0,-3.5377);(290.0,-3.1235);(300.0,-2.5537);(310.0,-2.3075);(320.0,-1.4424);];
    get_greek_test "delta of AAPL 3/20/20 call" "delta" clean_data "AAPL" "3/20/20" "call" [(110.0,0.9764);(115.0,0.9706);(120.0,0.963);(125.0,0.9534);(130.0,0.9391);(135.0,0.9193);(140.0,0.9085);(145.0,0.8923);(150.0,0.8749);(155.0,0.8566);(160.0,0.8258);(165.0,0.8113);(170.0,0.784);(175.0,0.7522);(180.0,0.7183);(185.0,0.682);(190.0,0.6425);(195.0,0.6007);(200.0,0.5572);(205.0,0.5122);(210.0,0.4672);(215.0,0.423);(220.0,0.3782);(225.0,0.3364);(230.0,0.2958);(235.0,0.2576);(240.0,0.2216);(245.0,0.1901);(250.0,0.1634);(255.0,0.1364);(260.0,0.1178);(265.0,0.0996);(270.0,0.0841);(275.0,0.071);(280.0,0.0595);(285.0,0.0506);(290.0,0.0421);(295.0,0.0364);(300.0,0.0318);];
    get_greek_test "gamma of AAPL 3/20/20 call" "gamma" clean_data "AAPL" "3/20/20" "call" [(110.0,0.0005);(115.0,0.0008);(120.0,0.001);(125.0,0.0013);(130.0,0.0017);(135.0,0.0022);(140.0,0.0025);(145.0,0.0029);(150.0,0.0034);(155.0,0.0039);(160.0,0.0043);(165.0,0.005);(170.0,0.0056);(175.0,0.0062);(180.0,0.0068);(185.0,0.0074);(190.0,0.0079);(195.0,0.0083);(200.0,0.0087);(205.0,0.009);(210.0,0.0091);(215.0,0.009);(220.0,0.0089);(225.0,0.0087);(230.0,0.0083);(235.0,0.0079);(240.0,0.0073);(245.0,0.0068);(250.0,0.0062);(255.0,0.0055);(260.0,0.005);(265.0,0.0044);(270.0,0.0039);(275.0,0.0034);(280.0,0.003);(285.0,0.0026);(290.0,0.0022);(295.0,0.002);(300.0,0.0017);];
    get_greek_test "vega of AAPL 3/20/20 call" "vega" clean_data "AAPL" "3/20/20" "call" [(110.0,4.9371);(115.0,6.8853);(120.0,9.2607);(125.0,12.0591);(130.0,15.9032);(135.0,20.6875);(140.0,23.1123);(145.0,26.5071);(150.0,29.8873);(155.0,33.1647);(160.0,38.1617);(165.0,40.2944);(170.0,43.9784);(175.0,47.7535);(180.0,51.2107);(185.0,54.3044);(190.0,57.0099);(195.0,59.1512);(200.0,60.6162);(205.0,61.3344);(210.0,61.2525);(215.0,60.3921);(220.0,58.7156);(225.0,56.4063);(230.0,53.4426);(235.0,49.9731);(240.0,46.0615);(245.0,42.0668);(250.0,38.2487);(255.0,33.9173);(260.0,30.6406);(265.0,27.149);(270.0,23.9559);(275.0,21.0665);(280.0,18.3712);(285.0,16.1784);(290.0,13.9461);(295.0,12.383);(300.0,11.0798);];
    get_greek_test "theta of AAPL 3/20/20 call" "theta" clean_data "AAPL" "3/20/20" "call" [(110.0,-0.2036);(115.0,-0.9235);(120.0,-1.7755);(125.0,-2.7576);(130.0,-4.155);(135.0,-6.004);(140.0,-6.6152);(145.0,-7.6399);(150.0,-8.5701);(155.0,-9.3417);(160.0,-11.1818);(165.0,-10.9413);(170.0,-11.7162);(175.0,-12.5903);(180.0,-13.2824);(185.0,-13.7784);(190.0,-14.2271);(195.0,-14.5808);(200.0,-14.6294);(205.0,-14.5445);(210.0,-14.3622);(215.0,-14.0507);(220.0,-13.4252);(225.0,-12.7897);(230.0,-11.9745);(235.0,-11.0703);(240.0,-10.0745);(245.0,-9.1311);(250.0,-8.2928);(255.0,-7.2601);(260.0,-6.6045);(265.0,-5.8447);(270.0,-5.159);(275.0,-4.5435);(280.0,-3.9634);(285.0,-3.509);(290.0,-3.0235);(295.0,-2.7093);(300.0,-2.4509);];
    get_greek_test "delta of AAPL 3/20/20 put" "delta" clean_data "AAPL" "3/20/20" "put" [(110.0,-0.0191);(115.0,-0.0266);(120.0,-0.0332);(125.0,-0.0406);(130.0,-0.0494);(135.0,-0.0597);(140.0,-0.0722);(145.0,-0.0868);(150.0,-0.104);(155.0,-0.1235);(160.0,-0.1463);(165.0,-0.1715);(170.0,-0.2007);(175.0,-0.2312);(180.0,-0.2664);(185.0,-0.3051);(190.0,-0.3452);(195.0,-0.3877);(200.0,-0.4319);(205.0,-0.4765);(210.0,-0.5222);(215.0,-0.5688);(220.0,-0.6122);(225.0,-0.656);(230.0,-0.6968);(235.0,-0.7326);(240.0,-0.7673);(245.0,-0.8057);(250.0,-0.8366);(255.0,-0.8403);(260.0,-0.8675);(265.0,-0.8856);(270.0,-0.9014);(275.0,-0.9151);(280.0,-0.9269);(285.0,-0.937);(290.0,-0.9455);(295.0,-0.9528);(300.0,-0.959);];
    get_greek_test "gamma of AAPL 3/20/20 put" "gamma" clean_data "AAPL" "3/20/20" "put" [(110.0,0.0007);(115.0,0.001);(120.0,0.0012);(125.0,0.0014);(130.0,0.0017);(135.0,0.002);(140.0,0.0024);(145.0,0.0029);(150.0,0.0033);(155.0,0.0039);(160.0,0.0044);(165.0,0.005);(170.0,0.0057);(175.0,0.0063);(180.0,0.0069);(185.0,0.0075);(190.0,0.008);(195.0,0.0084);(200.0,0.0089);(205.0,0.009);(210.0,0.0091);(215.0,0.0092);(220.0,0.009);(225.0,0.0088);(230.0,0.0084);(235.0,0.0079);(240.0,0.0073);(245.0,0.0068);(250.0,0.0061);(255.0,0.0056);(260.0,0.005);(265.0,0.0044);(270.0,0.0039);(275.0,0.0035);(280.0,0.003);(285.0,0.0026);(290.0,0.0023);(295.0,0.002);(300.0,0.0017);];
    get_greek_test "vega of AAPL 3/20/20 put" "vega" clean_data "AAPL" "3/20/20" "put" [(110.0,7.2391);(115.0,9.5632);(120.0,11.4944);(125.0,13.5377);(130.0,15.8571);(135.0,18.4286);(140.0,21.3458);(145.0,24.5275);(150.0,28.0153);(155.0,31.6736);(160.0,35.5558);(165.0,39.4559);(170.0,43.4676);(175.0,47.1655);(180.0,50.8406);(185.0,54.1886);(190.0,56.9537);(195.0,59.1417);(200.0,60.6276);(205.0,61.3349);(210.0,61.2441);(215.0,60.3009);(220.0,58.6356);(225.0,56.1651);(230.0,53.1176);(235.0,49.8172);(240.0,46.0227);(245.0,41.0835);(250.0,36.4867);(255.0,35.8898);(260.0,31.2395);(265.0,27.8261);(270.0,24.6195);(275.0,21.6454);(280.0,18.918);(285.0,16.4422);(290.0,14.2157);(295.0,12.2301);(300.0,10.473);];
    get_greek_test "theta of AAPL 3/20/20 put" "theta" clean_data "AAPL" "3/20/20" "put" [(110.0,-2.4869);(115.0,-3.2828);(120.0,-3.8502);(125.0,-4.4064);(130.0,-5.0188);(135.0,-5.6698);(140.0,-6.3978);(145.0,-7.1622);(150.0,-7.9846);(155.0,-8.8021);(160.0,-9.6562);(165.0,-10.4481);(170.0,-11.2779);(175.0,-11.8627);(180.0,-12.5291);(185.0,-13.1718);(190.0,-13.5158);(195.0,-13.8169);(200.0,-13.6534);(205.0,-13.7151);(210.0,-13.3369);(215.0,-12.7104);(220.0,-12.1365);(225.0,-11.2631);(230.0,-10.3404);(235.0,-9.4797);(240.0,-8.4472);(245.0,-6.9816);(250.0,-5.7727);(255.0,-6.0613);(260.0,-4.7538);(265.0,-3.8957);(270.0,-3.0827);(275.0,-2.3204);(280.0,-1.612);(285.0,-0.9586);(290.0,-0.3595);(295.0,0.1869);(300.0,0.6835);];
    get_greek_test "delta of AAPL 1/17/20 call" "delta" clean_data "AAPL" "1/17/20" "call" [(50.0,0.9919);(55.0,0.9919);(60.0,0.9918);(65.0,0.9918);(70.0,0.9918);(75.0,0.9917);(80.0,0.9915);(85.0,0.9911);(90.0,0.9903);(95.0,0.9891);(100.0,0.9871);(105.0,0.9842);(110.0,0.9801);(115.0,0.9744);(120.0,0.967);(125.0,0.9575);(130.0,0.9457);(135.0,0.9316);(140.0,0.9173);(145.0,0.9142);(150.0,0.8953);(155.0,0.8808);(160.0,0.8446);(165.0,0.8326);(170.0,0.8021);(175.0,0.7693);(180.0,0.7363);(185.0,0.6963);(190.0,0.6523);(195.0,0.6049);(200.0,0.5547);(205.0,0.5025);(210.0,0.4493);(215.0,0.396);(220.0,0.3449);(225.0,0.2947);(230.0,0.2483);(235.0,0.2088);(240.0,0.1735);(245.0,0.1394);(250.0,0.114);(255.0,0.0923);(260.0,0.0735);(265.0,0.0601);(270.0,0.0467);(275.0,0.0385);(280.0,0.0313);(285.0,0.0258);(290.0,0.0226);(300.0,0.0151);(310.0,0.0125);(320.0,0.0068);(330.0,0.004);(340.0,0.0024);];
    get_greek_test "gamma of AAPL 1/17/20 call" "gamma" clean_data "AAPL" "1/17/20" "call" [(50.0,0.0);(55.0,0.0);(60.0,0.0);(65.0,0.0);(70.0,0.0);(75.0,0.0);(80.0,0.0);(85.0,0.0);(90.0,0.0001);(95.0,0.0001);(100.0,0.0002);(105.0,0.0004);(110.0,0.0005);(115.0,0.0007);(120.0,0.001);(125.0,0.0013);(130.0,0.0017);(135.0,0.0021);(140.0,0.0025);(145.0,0.0028);(150.0,0.0033);(155.0,0.0039);(160.0,0.0045);(165.0,0.0052);(170.0,0.0059);(175.0,0.0066);(180.0,0.0075);(185.0,0.0083);(190.0,0.009);(195.0,0.0096);(200.0,0.0101);(205.0,0.0105);(210.0,0.0106);(215.0,0.0106);(220.0,0.0102);(225.0,0.0098);(230.0,0.0092);(235.0,0.0084);(240.0,0.0075);(245.0,0.0066);(250.0,0.0057);(255.0,0.0049);(260.0,0.0042);(265.0,0.0036);(270.0,0.0029);(275.0,0.0025);(280.0,0.0021);(285.0,0.0018);(290.0,0.0015);(300.0,0.0011);(310.0,0.0009);(320.0,0.0005);(330.0,0.0003);(340.0,0.0002);];
    ]

let maths_test =
  [
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
      levy_test "lorentzian walk" {pdf = { functn = (fun x -> x) ; 
      distribution_class = Maths.Lorentzian {gamma = 1. ; peak = 0.0}} ; 
      init = 0.0 ; 
      numsteps = 1000};
      
      levy_test "exponential walk" {pdf = { functn = (fun x -> x) ; 
      distribution_class = Maths.Laplace {lambda = 1. ; peak = 0.0}} ; 
      init = 0.0 ; 
      numsteps = 1000};
      cdf_test " normal cdf"
      {functn = (fun x -> x); 
      distribution_class =
      Maths.Normal { stddev = 1.; mean = 0. };
      }
      0. 0.5;
    
      cdf_test " normal cdf"
      {functn = (fun x -> x); 
      distribution_class =
      Maths.Normal { stddev = 1.; mean = 2. };
      }
      2. 0.5;

    
      cdf_test "exp cdf"
      {functn = (fun x -> x); 
      distribution_class =
      Maths.Laplace { lambda = 1.; peak = 0. };
      }
      0. 0.5;

      cdf_test "exp cdf"
      {functn = (fun x -> x); 
      distribution_class =
      Maths.Laplace { lambda = 1.; peak = 0. };
      }
      0.4 0.664840;

      cdf_test "exp cdf"
      {functn = (fun x -> x); 
      distribution_class =
      Maths.Laplace { lambda = 1.; peak = 0. };
      }
      0.8 0.775336;

      cdf_test "lorentz cdf"
      {functn = (fun x -> x); 
      distribution_class =
      Maths.Lorentzian { gamma = 1.; peak = 0. };
      }
      0. 0.5;
      
  ]
 let price_spread_test
    (name : string)
    (spread : Spread.t)
    (underlying : float)
    (today : Blackscholes.date)
    (expected_output : float) : test =
  name >:: fun _ ->
  let result = Spread.price_spread spread underlying today in
  assert (result |> float_about_eq expected_output)

let pdf_spead_test = [
  pdf_draw_test "exponential value" {functn = (fun x -> x); distribution_class = 
  Maths.Laplace{lambda = 1. ; peak = 0. }} 0. (0.5*.(1.));
  pdf_draw_test "exponential value" {functn = (fun x -> x); distribution_class = 
  Maths.Laplace{lambda = 2. ; peak = 0. }} 0. (0.5*.(2.));
  pdf_draw_test "exponential value" {functn = (fun x -> x); distribution_class = 
  Maths.Laplace{lambda = 0.1 ; peak = 0. }} 0. (0.5*.(0.1));
  pdf_draw_test "exponential value" {functn = (fun x -> x); distribution_class = 
  Maths.Laplace{lambda = 2. ; peak = 0. }} 3. (0.002479);

  pdf_draw_test "normal value" {functn = (fun x -> x); distribution_class = 
  Maths.Normal{stddev = 1. ; mean = 0. }} 3. (0.004432);
  pdf_draw_test "normal value" {functn = (fun x -> x); distribution_class = 
  Maths.Normal{stddev = Float.pi ; mean = 0. }} 3. (0.080491);
  pdf_draw_test "normal value" {functn = (fun x -> x); distribution_class = 
  Maths.Normal{stddev = 3. ; mean = 0. }} 1. (0.125794);
  pdf_draw_test "normal value" {functn = (fun x -> x); distribution_class = 
  Maths.Normal{stddev = 80. ; mean = 0. }} 199. (0.000226);
  
  pdf_draw_test "log normal value" {functn = (fun x -> x); distribution_class = 
  Maths.LogNormal{sigma_of_log = 1. ; mean_of_log = 0. }} 3. (0.072728);
  pdf_draw_test "log normal value" {functn = (fun x -> x); distribution_class = 
  Maths.LogNormal{sigma_of_log = Float.pi ; mean_of_log = 0. }} 3. (0.039818);
  pdf_draw_test "log normal value" {functn = (fun x -> x); distribution_class = 
  Maths.LogNormal{sigma_of_log = 3. ; mean_of_log = 0. }} 1. (0.132981);
  pdf_draw_test "log normal value" {functn = (fun x -> x); distribution_class = 
  Maths.LogNormal{sigma_of_log = 80. ; mean_of_log = 0. }} 199. (0.0000256);
  
  pdf_draw_test "lorentzian value" {functn = (fun x -> x); distribution_class = 
  Maths.Lorentzian{gamma = 1. ; peak = 0. }} 3. (0.031831);
  pdf_draw_test "lorentzian value" {functn = (fun x -> x); distribution_class = 
  Maths.Lorentzian{gamma = Float.pi ; peak = 0. }} 3. (0.052995);
  pdf_draw_test "lorentzian value" {functn = (fun x -> x); distribution_class = 
  Maths.Lorentzian{gamma = 3. ; peak = 0. }} 1. (0.095493);
  pdf_draw_test "lorentzian value" {functn = (fun x -> x); distribution_class = 
  Maths.Lorentzian{gamma = 80. ; peak = 0. }} 199. (0.000554);
]

let spread_test =
  [
    price_spread_test "simple call, 1 month out -- test" {
      spread = LongCall {strike1 = 50.; bwpc_list = ["bc1"]}; 
      expiry = (Blackscholes.create_date 06 01 2022 (Blackscholes.create_time 0 0 0 0)); 
      options = 
      [ Blackscholes.create_european_option 
      50. (Blackscholes.create_date 06 01 2022 (Blackscholes.create_time 0 0 0 0))  
      0.025 0.1 ] }
      50.
      (Blackscholes.create_date 05 01 2022 (Blackscholes.create_time 0 0 0 0))
      0.64;

      price_spread_test "in the money call at expiry -- test" {
      spread = LongCall {strike1 = 50.; bwpc_list = ["bc1"]}; 
      expiry = (Blackscholes.create_date 05 01 2022 (Blackscholes.create_time 0 0 0 0)); 
      options = 
      [ Blackscholes.create_european_option 
      50. (Blackscholes.create_date 05 01 2022 (Blackscholes.create_time 0 0 0 0))  
      0.025 0.1 ] }
      55.
      (Blackscholes.create_date 05 01 2022 (Blackscholes.create_time 0 0 0 0))
      5.;

      price_spread_test "out of the money condor, high volatility, 1 month out -- test" {
      spread = Condor {strike1 = 50.; strike2 = 55.; strike3 = 60.; strike4 = 65.; 
               bwpc_list = ["bc1"; "wc1"; "wc1"; "bc1"]}; 
      expiry = (Blackscholes.create_date 06 01 2022 (Blackscholes.create_time 0 0 0 0)); (*Expiry*)
      options = 
      [ Blackscholes.create_european_option 
      50. (Blackscholes.create_date 06 01 2022 (Blackscholes.create_time 0 0 0 0))  (*Expiry*)
      0.025 0.5 ] }
      48.
      (Blackscholes.create_date 05 01 2022 (Blackscholes.create_time 0 0 0 0)) (*Today*)
      2.04;

      price_spread_test "out of the money condor, low volatility, 1 month out -- test" {
      spread = Condor {strike1 = 50.; strike2 = 55.; strike3 = 60.; strike4 = 65.; 
               bwpc_list = ["bc1"; "wc1"; "wc1"; "bc1"]}; 
      expiry = (Blackscholes.create_date 06 01 2022 (Blackscholes.create_time 0 0 0 0)); (*Expiry*)
      options = 
      [ Blackscholes.create_european_option 
      50. (Blackscholes.create_date 06 01 2022 (Blackscholes.create_time 0 0 0 0))  (*Expiry*)
      0.025 0.1 ] }
      48.
      (Blackscholes.create_date 05 01 2022 (Blackscholes.create_time 0 0 0 0)) (*Today*)
      0.06
  ]

let tree_test tree = print_tree tree 
let sum_test tree = assert_equal (check_sum 0. tree) 1.
let expected_value_test input tree = assert_equal (expected_val tree) input 


let yay = cons_american 110. 0.05 date2 12 1.01 0.99 100.

let binomial_test = [
  (** tree_test (create (100. , 1.) 0.54 (init_tree 100.) 1.2 0.8 false 0 3); *)
  sum_test (create (100. , 1.) 0.54 (init_tree 100.) 1.2 0.8 false 0 10);
  (** 
  print_day (american_option_price date1 yay) 0 0;
  print_day (american_option_price date1 yay) 1 0;
  print_day (american_option_price date1 yay) 2 0; 
  *)
  print_day (american_option_price date1 yay) 0 0; 

]

let tests =
  "Tests :::" >::: List.flatten [ maths_test; csvreader_test; blackscholes_test;spread_test;portfolio_test;pdf_spead_test]

let _ = run_test_tt_main tests