open Base
open Matplotlib
open Csvreader

type colour =
  | Red
  | Green
  | Blue
  | White
  | Black
  | Yellow
  | Orange
let data = "Data/clean_data.csv" |> load_csv |> from_csv
let plot_2d title xlabel ylabel h = 
  Pyplot.title title;
  Pyplot.xlabel xlabel;
  Pyplot.ylabel ylabel;
  Pyplot.grid true;
  Pyplot.scatter (Array.of_list h);
  Mpl.show ()
let call = get_greek "delta" data "A" "8/16/19" "call"
let symbol = "A"
let exp = "8/16/19"
let side = "call"
let greek = "delta"
let exps = get_exps data symbol
let d_list side greek symbol exp exps = 
  let rec iter_exps lst = match lst with
    | h :: t -> (get_greek greek data symbol h side) :: iter_exps t
    | [] -> [] in
    iter_exps exps

let main () = plot_2d ("Stock " ^symbol^" "^side^" "^greek^"s across expirations") 
"Strike Price ($)" "Delta" call
