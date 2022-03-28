(** Representation of the binomial model.
    This module represents the data stored in adventure files, including
    the rooms and exits. It handles loading of that data from JSON as
    well as querying the data. *)


(** Abstract type representing an American Option. Can be exercised anytime between now and expiration. *)

open Blackscholes

type american_option

type 'a tree

val american_call_option_price : date -> american_option -> float 

val american_put_option_price : date -> american_option -> float 

val print_tree : (float * float) tree -> unit 

val create : (float * float) -> float -> (float * float) tree -> float -> float
-> bool -> int -> int -> (float * float) tree

val is_between : date -> date -> date -> bool

val init_tree : float -> (float * float) tree

val check_sum : float -> (float * float) tree -> float 

exception Bad of string 