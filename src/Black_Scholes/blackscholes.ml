open Maths

type time = {
  hour: int; 
  minute : int ;
  seconds : int ;
  milliseconds : int
}

type date = { 
  month : int ;
  day : int ;
  year : int ;
  time : time 
}

type european_option = { 
  strike_price : float ; 
  exercise_date : date ; 
  risk_free_rate : float;  
  implied_volatility : float;
}

let create_time h m s ms = {
  hour = h; minute = m; seconds = s; milliseconds = ms
}

let create_date m d y (t: time)  = {month = m; day = d; year = y; time = t}

let create_european_option (k:float) (d:date) (r:float) (v:float) = { 
  strike_price = k ; 
  exercise_date = d; 
  risk_free_rate = r; 
  implied_volatility = v
}

(* returns: true if [date1] comes strictly before [date2];
 *   false otherwise
 * requires: [date1] and [date2] are valid date triples
*)
let is_before date1 date2 =
  date1.year < date2.year || (date1.year = date2.year && date1.month < date2.month) || 
  (date1.year = date2.year && date1.month = date2.month && date1.month < date2.month)

(*[days_of_month m] returns the number of days in a given month. Requires m in between [0,12] *)
let days_of_month m = 
  if m = 1 ||  m = 3 || m = 5 ||  m = 7 ||  m = 8 ||  m = 10 ||  m = 12 then 31 else if m = 2 then 28 else 30

let diff_between_dates date1 date2 = 
  let days_inbetwen = if is_before date1 date2 then ((days_of_month date1.month) - date1.day) + date2.day 
  else (days_of_month date2.month) - date2.day + date1.day in (float_of_int(days_inbetwen) /. 365.) 

(* [d1] computes the d1 part of the black-scholes equation *)
let d1 (european_option : european_option) (current_stock_price : float) (time_to_expiry : float )  : float =  
  let num = Float.log (current_stock_price /. european_option.strike_price ) +. ((european_option.risk_free_rate  +. 
  ((european_option.implied_volatility **2.) /. 2.)) *. time_to_expiry)in  
  let den = european_option.implied_volatility  *. Float.sqrt time_to_expiry in (num/.den)

let d2 (european_option : european_option) (d1 : float) (time_to_expiry : float) : float = 
  d1 -. european_option.implied_volatility *.Float.sqrt time_to_expiry

let european_call_options_price (european_call : european_option) (current_stock_price : float) (current_date : date) = 
  let time_to_expiry = diff_between_dates european_call.exercise_date current_date in 
  let d1 = d1 european_call current_stock_price time_to_expiry in 
  let d2 = (d2 european_call d1 time_to_expiry ) in 
  let a_normal_pdf =
  {functn = (fun x -> Float.sqrt(1. /. (2.*.Float.pi)) *. Float.exp(-1. *. x *. x /. ((1.0*.1.0)))) ; 
  (* stddev = sqrt(1/2pi) ; mean = 0 *)
  distribution_class = Maths.Normal {stddev = 1.0; mean = 0.}} in
  let term1 = current_stock_price *. (Maths.integrate a_normal_pdf (-20.) d1) in 
  let term2 = european_call.strike_price *. 
  Float.exp ( -1. *. european_call.risk_free_rate *. time_to_expiry) *. Maths.integrate a_normal_pdf (-20.) d2 in 
  term1 -. term2


