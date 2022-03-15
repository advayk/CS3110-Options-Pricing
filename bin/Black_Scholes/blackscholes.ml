
type european_option = { 
  strike_price : int ; 
  exercise_date : int ; 
  risk_free_rate : float;  
  implied_volatility : float;
}

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

let create_time h m s ms = {
  hour = h; minute = m; seconds = s; milliseconds = ms
}

let create_date m d y (t: time)  = {month = m; day = d; year = y; time = t}

let create_european_option k t r v = { 
  strike_price = k ; 
  exercise_date = t; 
  risk_free_rate = r; 
  implied_volatility = v
}


(*[days_of_month m] returns the number of days in a given month. Requires m in between [0,12] *)
let days_of_month m = 
  if m = 1 ||  m = 3 || m = 5 ||  m = 7 ||  m = 8 ||  m = 10 ||  m = 12 then 31 else if m = 2 then 28 else 30


(* [diff_between_dates date1 date2] is the number of days (date1 .... date2] between date1 and date2.
  Requires: date1 and date2 are either the same year or in directly adjacent years. date2 comes after date1.  *)
let rec diff_between_dates date1 date2 =
  if date1.month = date2.month && date1.year = date2.year then date2.day - date1.day else if 
  date1.year = date2.year && date2.month <> date1.month then days_of_month date1.month - date1.day + diff_between_dates {year = date1.year; month = date1.month + 1; day = date1.day; time = date1.time } date2 
  else diff_between_dates date1 {month = 12; day = 31; year=date1.year; time=date1.time} + diff_between_dates {month = 1; day = 1; year=date2.year; time=date2.time} date2

(* [strd_norm_cumulative_dist] is the standard normal cumulative distribution function. *)
let strd_norm_cumulative_dist (input : float) = (1. +. Float.erf (input /. Float.sqrt 2.))/. 2.

(* [d1] computes the d1 part of the black-scholes equation *)
let d1 european_option : european_option = raise (Failure "Unimplemented: european_call")

(* [d2] computes the d1 part of the black-scholes equation *)
let d2 european_option : european_option = raise (Failure "Unimplemented: european_call")

let european_call_options_price (european_call : european_option) (current_stock_price : int) (current_time : int) = 
  raise (Failure "Unimplemented: european_call")
