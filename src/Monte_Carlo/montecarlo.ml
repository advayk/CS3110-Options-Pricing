
type pair = float * float 

type 'a price_tree = 
    | Leaf 
    | Node of pair
    * pair price_tree * pair price_tree 
    * pair price_tree * pair price_tree 
    * pair price_tree * pair price_tree 
    * pair price_tree * pair price_tree 
    * pair price_tree * pair price_tree

let rec deltas lst = 
    match lst with 
    | [] -> []
    | h :: [] -> []
    | h :: t :: [] -> if h <> 0. then [(t -. h) /. h] else [1.]
    | h :: m :: t -> if h <> 0. then (m -. h) /. h :: deltas (m :: t) else 1. :: deltas (m :: t)


let european_call prices num_days discount_rate = 0.


