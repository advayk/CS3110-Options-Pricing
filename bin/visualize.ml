open Base
open Matplotlib
open Csvreader

let colors : Mpl.Color.t list = 
  [Red
  ; Green
  ; Blue
  ; White
  ; Black
  ; Yellow
  ; Orange]

let contents z = 
  match z with
  Some c -> c
let data = "Data/clean_data.csv" |> load_csv |> from_csv
let plot_2d title xlabel ylabel d_list = 
  Pyplot.title title;
  Pyplot.xlabel xlabel;
  Pyplot.ylabel ylabel;
  Pyplot.grid true;
  let rec iter_d_list lst index = let proper_i = index mod 7 in
    match lst with
  | h :: t -> Pyplot.scatter (Array.of_list h); iter_d_list t (proper_i + 1);
  | [] -> ();
  in iter_d_list d_list 0;
  Mpl.show ()
let call = get_greek "delta" data "A" "8/16/19" "call"
let symbol = "A"
let side = "call"
let greek = "delta"

let d_list exps = 
  let rec iter_exps lst = match lst with
    | h :: t -> (get_greek greek data symbol h side) :: iter_exps t
    | [] -> [] 
  in iter_exps exps

let exps = get_exps data symbol
let datas = d_list exps

let () = plot_2d ("Stock " ^symbol^" "^side^" "^greek^"s across expirations") 
"Strike Price ($)" "Delta" datas