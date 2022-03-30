open ANSITerminal
open Blackscholes
open Stdlib

type spread = 
| Condor of {strike1:float; strike2:float; strike3:float; strike4:float ; bwpc_list: string list } 
| Butterfly of {strike1:float; strike2:float; strike3:float; bwpc_list: string list}
| BearPut of {strike1:float; strike2:float; bwpc_list: string list}
| LongCall of {strike1:float; bwpc_list: string list}
| LongPut of {strike1:float; bwpc_list: string list}
| BullCallLadder of {strike1:float; strike2:float; strike3:float; bwpc_list : string list}

type t = { spread : spread; expiry : Blackscholes.date; options : european_option list}

let get_spread_bwpc_list = function
| Condor {strike1; strike2; strike3; strike4; bwpc_list} -> ["bc1"; "wc1"; "wc1"; "bc1"]
| Butterfly {strike1; strike2; strike3; bwpc_list} -> ["bc1"; "wc2"; "bc1"]
| BearPut {strike1; strike2; bwpc_list} -> ["wp1"; "bp1"]
| LongCall {strike1; bwpc_list} -> ["bc1"]
| LongPut {strike1; bwpc_list} -> ["bp1"]
| BullCallLadder{strike1; strike2; strike3; bwpc_list} -> ["bc1"; "wc1"; "wc1"]

let blackscholes_date  lst = 
  let time = Blackscholes.create_time 0 0 0 0  in 
  Blackscholes.create_date 
  (int_of_string (List.nth lst 0)) 
  (int_of_string (List.nth lst 1)) 
  (int_of_string (List.nth lst 2)) time

let rec get_strikes (num_strikes : int) = 
  if num_strikes = 0 then [] else
  match read_line () with
  | strike ->  let s = float_of_string_opt strike in
    match s with
    | Some flt -> flt :: get_strikes (num_strikes-1) 
    | None -> get_strikes num_strikes
    
let rec make_options (strikes : float list) (expiry : date) (r:float) (v:float) =
  match strikes with 
  | [] -> []
  | h :: t -> (create_european_option h expiry r v) :: make_options t expiry r v

let make_t (name : string) (expiry : date) (r : float) (v : float)  = 
  print_endline "Please enter the strike prices of the options in increasing order (float).";
  match name with
    | "condor" -> ( let strikes = (get_strikes 4) in 
      let option_spread = 
        Condor {
          strike1 = List.nth strikes 0; 
          strike2 = List.nth strikes 1; 
          strike3 = List.nth strikes 2; 
          strike4 = List.nth strikes 3; 
          bwpc_list = ["bc1"; "wc1"; "wc1"; "bc1"]} in 
      {spread = option_spread; expiry = expiry; options = make_options strikes expiry r v})
    | "butterfly" -> (
      let strikes = get_strikes 3 in 
      let option_spread =  Butterfly 
        {strike1 = List.nth strikes 0;
        strike2 = List.nth strikes 1;
        strike3 = List.nth strikes 2;
        bwpc_list = ["bc1"; "wc2"; "bc1"]} in 
        {spread = option_spread; expiry = expiry; options = make_options strikes expiry r v})
    | "bear put" -> (
      let strikes = get_strikes 2 in 
      let option_spread = BearPut 
        {strike1 = List.nth strikes 0; 
        strike2 = List.nth strikes 1;
        bwpc_list = ["wp1"; "bp1"]} in 
        {spread = option_spread; expiry = expiry; options = make_options strikes expiry r v})
    | "long call" -> (
      let strikes = get_strikes 1 in 
      let option_spread = LongCall
      {strike1 = List.nth strikes 0;
      bwpc_list = ["bc1"]} in 
        {spread = option_spread; expiry = expiry; options = make_options strikes expiry r v})
    | "long put" -> (
      let strikes = get_strikes 1 in 
      let option_spread = LongPut
      {strike1 = List.nth strikes 0;
      bwpc_list = ["bp1"]} in
        {spread = option_spread; expiry = expiry; options = make_options strikes expiry r v})
    | "bull call ladder" -> (
      let strikes = get_strikes 3 in 
      let option_spread =  BullCallLadder 
        {strike1 = List.nth strikes 0;
        strike2 = List.nth strikes 1; 
        strike3 = List.nth strikes 2;
        bwpc_list = ["bc1"; "wc1"; "wc1"]} in 
        {spread = option_spread; expiry = expiry; options = make_options strikes expiry r v})
    | _ -> failwith ("Spread not recognized")

let rec price_options (options : european_option list) 
(bwpc_list : string list) (underlying : float) (today : date) = 
match options with
| [] -> 0.
| h :: t ->
  match bwpc_list with
    | [] -> failwith "invalid bwpc list"
    | info :: tl -> let code = String.sub info 0 2 and num = String.sub info 2 1 in
    match code with
      | "bc" -> (match float_of_string_opt num with 
        | None -> failwith "invalid bwpc string"
        | Some flt -> (flt *. (european_call_options_price h underlying today) +. price_options t tl underlying today))
      | "bp" -> (match float_of_string_opt num with 
        | None -> failwith "invalid bwpc string"
        | Some flt -> (flt *. (european_put_options_price h underlying today) +. price_options t tl underlying today) )
      | "wc" -> (match float_of_string_opt num with 
        | None -> failwith "invalid bwpc string"
        | Some flt -> (-1. *. flt *. (european_call_options_price h underlying today) +. price_options t tl underlying today) )
      | "wp" -> (match float_of_string_opt num with 
        | None -> failwith "invalid bwpc string"
        | Some flt -> (-1.*. flt *. (european_put_options_price h underlying today) +. price_options t tl underlying today) )
      | _ -> failwith "invalid bwpc string"

let price_spread (spread : t) (underlying : float) (today : date) = 
  match spread with
  | {spread; expiry; options} -> price_options options (get_spread_bwpc_list spread) underlying today

  
let make_spread (spread : string) =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\n\nThank you for choosing to buy a spread !\n";
  let name = String.lowercase_ascii spread in    
  print_endline("Spread type: " ^ name); 
  print_endline "\nPlease enter the expiry of this spread as (mm/dd/2022).";
  match read_line () with
    | date -> let expiry = blackscholes_date (String.split_on_char '/' date) in
    print_endline "Please enter the risk free rate (float).";
    match read_line () with
      | r -> print_endline "Please enter the volatility (float).";
      match read_line () with
        | v -> make_t name expiry (float_of_string r) (float_of_string v)