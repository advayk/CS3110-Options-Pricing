let () = print_endline "CS 3110 Final Project: Options Pricing!"

type european_option = { 
  strike_price : int ; 
  exercise_date : int ; 
  risk_free_rate : float;  
  implied_volatility : float;
}

(* [create_european_option s k t r v ] creates am european_option. 
    Requires: all the arguments be of the type required for an european_option *)
let create_european_option k t r v = { 
  strike_price = k ; 
  exercise_date = t; 
  risk_free_rate = r; 
  implied_volatility = v
}



(* [strd_norm_cumulative_dist] is the standard normal cumulative distribution function. *)
let strd_norm_cumulative_dist x = raise (Failure "Unimplemented: european_call")


(* [d1] computes the d1 part of the black-scholes equation *)
let d1 european_option : european_option = raise (Failure "Unimplemented: european_call")

(* [d2] computes the d1 part of the black-scholes equation *)
let d2 european_option : european_option = raise (Failure "Unimplemented: european_call")

(* [european_call_options_price call european option] computes estimates the price of a European call option. *)
let european_call_options_price (european_call : european_option) (current_stock_price : int) (current_time : int) = 
  raise (Failure "Unimplemented: european_call")
