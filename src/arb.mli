(** Module for checking arbitrage pricing boundaries between different market prices for an option. 
    Will possibly read an order book and scan for arbitrage, currently checks between different prices. *)

open Blackscholes

type arbitrage_condition
(** The abstract type of an arbitrage condition. *)

type arbitrage_decision 
(** The options that arbitrage exists between, as well as what positions to take in each. *)

type decsion 
(**  The quantity and direction of a trade in a given option. *)

val is_arb : arbitrage_condition -> european_option list -> float -> bool
(** [is_arb arb_c options underlying] is whether there is arbitrage. *)

val arb_decision : arbitrage_condition -> european_option list -> float -> arbitrage_decision
(** [arb_decision arb_c options underlying] is the trade to capitalize off of arbitrage. *)