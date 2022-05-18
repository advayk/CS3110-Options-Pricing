(** Representation of the Binomial Option Pricing Model.*)

open Blackscholes

type t

type 'a tree

val cons_american : float -> float -> date -> int -> float -> float -> float -> t

val american_option_price : date -> t -> (float * float) tree 

val print_tree : (float * float) tree -> unit 

val valuation_tree :  float -> float -> float -> float -> (float * float) tree -> (float * float) tree

val create : (float * float) -> float -> (float * float) tree -> float -> float
-> bool -> int -> int -> (float * float) tree

val expected_val : (float * float ) -> (float * float) tree -> float 

val is_between : date -> date -> date -> bool

val init_tree : float -> (float * float) tree

val check_sum : float -> (float * float) tree -> float 

val first_node : (float * float) tree -> unit 

val print_day : (float * float) tree -> int -> int -> unit 

exception Bad of string