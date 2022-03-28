open Blackscholes

type 'a tree = | Leaf | Node of 'a * 'a tree * 'a tree

type american_option = { 
  strike_price : float ; 
  risk_free_rate : float;
  expiration_date : date; 
  num_periods : int;
  up : float;
  down : float;
  current_price : float
}

let date_to_string date = (string_of_int (get_month date)) ^ "/" ^ (string_of_int (get_day date)) ^ "/" ^ (string_of_int (get_year date))

let q_calculation risk_free_rate up down time_step = (Float.exp (risk_free_rate
*. time_step) -. down) /. (up -. down)

let print_pair pair = 
    match pair with | (price, probability) -> (print_string "( "; print_string
    "Price: "; print_string (string_of_float price); print_string "Probability:
    "; print_string (string_of_float probability); print_string " )";)

let init_tree price = Node((price, 1.), Leaf, Leaf)

let rec print_tree = function 
    | Leaf -> () | Node (y, left, right) -> (print_pair y; print_tree left;
    print_tree right;)

let rec check_sum prev tree = match tree with 
  | Leaf -> prev /. 2.
  | Node ((x, y), left, right) -> check_sum y left +. check_sum y right

let rec expected_val prev tree = match tree with 
    | Leaf -> begin match prev with 
        (x,y) -> ((x *. y) /. 2.) end 
    | Node ((x, y), left, right) -> expected_val prev left +. expected_val prev right 

let is_between date0 date1 date2 = 
    if get_year date0 < get_year date1 || get_year date0 > get_year date2 then false else 
        if get_month date0 < get_month date1 || get_month date0 > get_month date2 then false else
            if get_day date0 < get_day date1 || get_day date0 > get_day date2 then false else
            true 

let create prev_data q tree up down is_up depth num_periods= 
    let prb = q in 
        let rec creator prev_data q tree up down is_up depth num_periods=
            match prev_data with 
            | (price, probability) -> if num_periods = depth then tree else 
                match tree with | Leaf -> begin 
                    match (((if is_up then price *. up else price *. down),
                    (probability *. q)), Leaf, Leaf) with | ((x, y), left,
                    right) -> Node ((x,y), creator (x, y) (1.-.prb) left up down
                    false (depth + 1) num_periods, creator (x,y) prb  right up
                    down true (depth + 1) num_periods) end 
                | Node ((x, y), left, right) -> Node ((x,y), creator (x, y)
                (1.-.prb) left up down false (depth + 1) num_periods, creator
                (x,y) prb right up down true (depth + 1) num_periods)
        in creator prev_data prb tree up down is_up depth num_periods

exception Bad of string 

let american_option_price cur_date exercise_date option = 
    if not (is_between exercise_date cur_date option.expiration_date) then raise (Bad (date_to_string exercise_date)) else 
        let q = q_calculation option.risk_free_rate option.up option.down
        (float_of_int ((diff_between_dates cur_date option.expiration_date) /
        (option.num_periods * 365))) in let price_tree = create
        (option.strike_price , 1.) q (init_tree option.current_price) 1.2 0.8
        false 0 option.num_periods

    













