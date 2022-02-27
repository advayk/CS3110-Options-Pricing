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

(* [create_time h m s ms] creates a time type. 
  Requires: 
  hours (h) : between [0,24]
  minute (m): between [0,60] 
  seconds (s) : between [0,60]
  milliseconds (s) : between [0,1000]*)
let create_time h m s ms = {
  hour = h; minute = m; seconds = s; milliseconds = ms
}

(* create_date m d y t 
  Requires:
  month (m) : to be a valid number between [0,12]
  day (d) : requires d to be a valid date for the month 
  time (t) : required time to be a valid 24 hour time *)
let create_date m d y (t: time)  = {month = m; day = d; year = y; time = t}

(* [create_european_option s k t r v ] creates am european_option. 
    Requires:  
    stock price (s): dollars 
    strike price (k): dollars 
    exercise_date (t): in date-time format 
    risk free rate (r): percentage in decimal (i.e 2% = 0.02)
    implied volatility (v) : percentage in decimal (i.e 30% = 0.03) *)
let create_european_option k t r v = { 
  strike_price = k ; 
  exercise_date = t; 
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

(* [time_to_maturity date1 date2] takes two valid dates and computes the time in between in as a ratio of 1 year 
example: one month would be 1/12 ~  0.0833...*)
let diff_between_dates date1 date2 =
  let days_inbetwen = if is_before date1 date2 then ((days_of_month date1.month) - date1.day) + date2.day else (days_of_month date2.month) - date2.day + date1.day in 
  (float_of_int(days_inbetwen) /. 365.) 

(* [strd_norm_cumulative_dist] is the standard normal cumulative distribution function. *)
let strd_norm_cumulative_dist input : float = raise (Failure "Unimplemented: strd_norm_cumulative_dist")

(* [d1] computes the d1 part of the black-scholes equation *)
let d1 european_option : european_option = raise (Failure "Unimplemented: european_call")

(* [d2] computes the d1 part of the black-scholes equation *)
let d2 european_option : european_option = raise (Failure "Unimplemented: european_call")

(* [european_call_options_price call european option] computes estimates the price of a European call option. *)
let european_call_options_price (european_call : european_option) (current_stock_price : int) (current_time : int) = 
  raise (Failure "Unimplemented: european_call")
