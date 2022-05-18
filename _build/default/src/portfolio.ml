
let rec portfolio_value (stock_prices: float list) (weights : float list) (acc : float) = 
  match (stock_prices,weights) with 
  | [],[] -> acc
  | [] , _ -> acc 
  | _ , [] -> acc 
  | (h1::t1, h2::t2) -> portfolio_value t1 t2 (acc +. h1*.h2)
  
let average_price (stock_prices : float list) = 
  (List.fold_left (fun acc v -> acc +. v) 0. stock_prices) /. (float_of_int (List.length stock_prices))