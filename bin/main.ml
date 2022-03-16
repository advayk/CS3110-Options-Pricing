open Blackscholes
open Maths
open Montecarlo
open ANSITerminal

let black_scholes_fun stock strike = 
  print_endline (string_of_int stock^string_of_int strike)

let data_dir_prefix = "data" ^ Filename.dir_sep

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the 3110 Text Adventure Game engine.\n";
  print_endline
    "Please enter the name of the game file you want to load.\n";
  let stock_price = read_line () in let strike_price = read_line () in black_scholes_fun stock_price strike_price

(* Execute the game engine. *)
let () = main () 