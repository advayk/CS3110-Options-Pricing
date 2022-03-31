open Csv
open Blackscholes

type d = {
  ul_symbol : string;
  ul_price : float;
  symbol : string;
  opt_type : string;
  expiration : date;
  strike : float;
  last : float;
  bid : float;
  ask : float;
  volume : int;
  oi : int;
  iv : float;
  delta : float;
  gamma : float;
  theta : float;
  vega : float;
}

let parse_date date = 
  let mdy_list = String.split_on_char '/' date in
  let m = List.nth mdy_list 0 |> int_of_string in
  let d = List.nth mdy_list 1 |> int_of_string in
  let y = List.nth mdy_list 2 |> int_of_string in 
  let t = create_time 0 0 0 0 in
  create_date m d y t

let rec from_csv csv_data = match csv_data with
| h :: t -> 
  let value_at index = List.nth h index in
  {
    ul_symbol = String.trim (value_at 0);
    ul_price = value_at 1 |> float_of_string;
    symbol = value_at 2;
    opt_type = value_at 3;
    expiration = value_at 4 |> parse_date;
    strike = value_at 5 |> float_of_string;
    last = value_at 6 |> float_of_string;
    bid = value_at 7 |> float_of_string;
    ask = value_at 8 |> float_of_string;
    volume = value_at 9 |> int_of_string;
    oi = value_at 10 |> int_of_string;
    iv = value_at 11 |> float_of_string;
    delta = value_at 12 |> float_of_string;
    gamma = value_at 13 |> float_of_string;
    theta = value_at 14 |> float_of_string;
    vega = value_at 15 |> float_of_string;
  } :: from_csv t
| [] -> []

let load_csv filename = 
  Csv.load filename

let first lst = let () = print_endline (List.nth lst 0).ul_symbol in (List.nth lst 0).ul_symbol 