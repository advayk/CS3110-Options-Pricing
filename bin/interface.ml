open ANSITerminal
open Blackscholes
open Maths
open Levy
open Montecarlo
open Csvreader
open Binomial
open Spread

let rec interface_main () =
  ANSITerminal.print_string [ ANSITerminal.green ]
  "\n\nHello ! \n";
  print_endline "try 'quit' or 'make spread'";
    match read_line () with
    | "quit" ->  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nThank You ! \n";
    | "make spread" -> Spread_loader.spread_main (); interface_main ()
    | _ -> interface_main ()

  let () =  interface_main ()