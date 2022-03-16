(** REPRESENTATION INVARIANT: num_bins = num_deltas *)
type node_record = {cur_price : float; probability : float; hist : float list; deltas : float list}

type 'a price_tree = 
    | Leaf 
    | Node of 'a * 'a price_tree list 

let rec deltas lst = 
    match lst with 
    | [] -> []
    | h :: [] -> []
    | h :: t :: [] -> if h <> 0. then [(t -. h) /. h] else [1.]
    | h :: m :: t -> if h <> 0. then (m -. h) /. h :: deltas (m :: t) else 1. :: deltas (m :: t)

(* Given a histogram of the probabiltiies of price movements -10% through 10% in
incrememnts of 2% (10 bins), [add_depth hist init_tree] returns a tree of depth
one greater than init_tree, that, at a given node has a price and associated
probability. *) 

let init_node price prb hst deltas =  Node({cur_price = price; probability = prb; hist = hst; deltas = deltas}, [Leaf; Leaf; Leaf; Leaf; Leaf; Leaf;
    Leaf; Leaf; Leaf; Leaf]) 


let rec construct_trees lst1 lst2 hst deltas = 
    match lst1, lst2 with | ([], []) -> [] | ([], _::_) -> [] | (_::_, []) -> []
    | (h1 :: t1, h2 :: t2) -> init_node h1 h2 hst deltas :: construct_trees t1
    t2 hst deltas

let create_nodes parent = 
    let new_probs = List.map (fun x -> x *. parent.probability) parent.hist in 
        let new_prices = List.map (fun x -> (1. +. x) *. parent.cur_price) parent.deltas in 
            Node({cur_price = parent.cur_price; probability = parent.probability; hist = parent.hist; deltas = parent.deltas}, construct_trees new_prices new_probs parent.hist parent.deltas) 

(** Given a list of trees, as well as that lists parent tree, constructs a final tree. *)
let rec tree_glue parent tree_list = Node(parent, tree_list)

let rec create_tree cur_node current_depth max_depth = 
    if current_depth <> max_depth then create_nodes cur_node |> List.map  



(**  
let create_tree prices hist max_depth = 
    let rec recurse_create hist (cur_tree : 'a price_tree) cur_depth max_depth parent_record = 
        match cur_tree with 
        | Node (record, children) -> begin
            match (children, record.hist) with 
                | ([], []) -> []
                | ([], _::_) -> []
                | (_::_, []) -> []
                | (h1 :: t1, h2 :: t2) ->  recurse_create t2 h1 (cur_depth + 1) max_depth parent_record end 
        | Leaf -> if cur_depth = max_depth then Leaf else (create_nodes parent_record.cur_price parent_record.probability parent_record.hist parent_record.deltas)
    in recurse_create hist (init_node (List.rev prices |> List.hd) 1. hist (deltas prices)) 1 10 {cur_price = (List.rev prices |> List.hd); probability = 1.; hist = hist; deltas = deltas prices}
*)

let european_call prices num_days discount_rate = 0.


