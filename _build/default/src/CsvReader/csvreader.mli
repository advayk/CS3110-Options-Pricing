open Blackscholes
(** Module to process csv files that contain financial data in a standard format *)

type d
(** The abstract type of values representing data on a single option. *)

val parse_date : string -> Blackscholes.date
(** [parse_date d] is the date that d represents. Requires: [d] is in the format MM/DD/YYYY *)

val from_csv : Csv.t -> d list
(** [from_csv c] is the data that [c] represents. Requires: [c] is a valid options data representation *)

val load_csv : string -> Csv.t
(** [load_csv s] is the [Csv] module representation of a given csv file. *)
val delta : date -> d list -> unit

val first : d list -> string