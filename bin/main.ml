let () = print_endline "CS 3110 Final Project: Options Pricing!"


type european_option = { 
stock_price : int ;  
strike_price : int ; 
time_to_expiration : int ; 
risk_free_rate : float;  
implied_volatility : float;
}

let create_european_option s k t r v = { 
  stock_price = s ; 
  strike_price = k ; time_to_expiration = t; 
  risk_free_rate = r; implied_volatility = v
}


