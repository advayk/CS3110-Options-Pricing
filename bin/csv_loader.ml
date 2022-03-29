open ANSITerminal
open Blackscholes
open Csvreader


let data_dir_prefix = "data" ^ Filename.dir_sep

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
  "\n\nWelcome to the Cornell Quant Funds Blackscholes options pricing model (csv).\n";
  print_endline
    "Please enter the name of the csvfile you want to load.\n";
  print_endline "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> 
    print_endline (Csvreader.(data_dir_prefix ^ file_name ^ ".csv") |> load_csv |> from_csv |> first)
    
let () = main ()