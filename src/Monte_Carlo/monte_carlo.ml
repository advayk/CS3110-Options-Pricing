
type pair = float * float 

type price_tree = 
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
    | h :: [] -> [0]
    | h :: t :: [] -> [(t -. h) /. h]
    | h :: m :: t -> (m -. h) /. h :: deltas m :: t


let european_call prices num_days discount_rate = 


