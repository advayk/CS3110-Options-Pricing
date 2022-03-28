type 'a tree = | Leaf | Node of 'a * 'a tree * 'a tree

type american_option = { 
  strike_price : float ; 
  risk_free_rate : float;  
  num_periods : int;
  up : float;
  down : float
}

let q_calculation risk_free_rate up down time_step = (Float.exp (risk_free_rate *. time_step) -. down) /. (up -. down)

let print_pair pair = 
    match pair with
    | (price, probability) -> 
    (print_string "( "; print_string "Price: "; print_string (string_of_float price); print_string "Probability: "; print_string (string_of_float
    probability); print_string " )";)

let init_tree price = Node((price, 1.), Leaf, Leaf)

let rec print_tree = function 
    | Leaf -> () | Node (y, left, right) -> (print_pair y; print_tree left;
    print_tree right;)

let create prev_data q tree up down is_up depth num_periods= 
    let prb = q in 
        let rec creator prev_data q tree up down is_up depth num_periods=
            match prev_data with 
            | (price, probability) -> if num_periods = depth then tree else 
                match tree with 
                | Leaf -> begin 
                    match (((if is_up then price *. up else price *. down), (probability *. q)), Leaf, Leaf) with 
                    | ((x, y), left, right) -> Node ((x,y), creator (x, y) (1.-.prb) left up down false (depth + 1) num_periods, creator (x,y) prb  right up down true (depth + 1) num_periods)
                    end 
                | Node ((x, y), left, right) -> Node ((x,y), creator (x, y) (1.-.prb) left up down false (depth + 1) num_periods, creator (x,y) prb right up down true (depth + 1) num_periods)
        in creator prev_data prb tree up down is_up depth num_periods

let american_call_option_price yay = 0.









