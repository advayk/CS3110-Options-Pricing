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

(* [time_to_maturity date1 date2] takes two valid dates and computes the time in between in as a ratio of 1 year 
example: one month would be 1/12 ~  0.0833...*)
let time_to_maturity date1 date2 = 
  raise (Failure "Unimplemented: time_to_maturity")



(* [strd_norm_cumulative_dist] is the standard normal cumulative distribution function. *)
let strd_norm_cumulative_dist x = raise (Failure "Unimplemented: european_call")


(* [d1] computes the d1 part of the black-scholes equation *)
let d1 european_option : european_option = raise (Failure "Unimplemented: european_call")

(* [d2] computes the d1 part of the black-scholes equation *)
let d2 european_option : european_option = raise (Failure "Unimplemented: european_call")

(* [european_call_options_price call european option] computes estimates the price of a European call option. *)
let european_call_options_price (european_call : european_option) (current_stock_price : int) (current_time : int) = 
  raise (Failure "Unimplemented: european_call")
