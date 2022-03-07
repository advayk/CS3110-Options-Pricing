(** Module containing mathematical processes necessary for pricing.
    This module represents ...*)

(**********************************************************************
 * DO NOT CHANGE THIS FILE
 * This is the interface file
 **********************************************************************)

 type pdf_class = | Normal of {stddev : float; mean : float} | Other
 (** The abstract type of values representing a class of pdf functions. *)

type pdf = { functn : float -> float ; distribution_class : pdf_class ;}
(** The abstract type of values representing a pdf function. *)

 val integrate : pdf -> float -> float -> float
(* [integrate pdf a b n] 
  integrates the function [pdf] from lower bound [a] to upper bound [b]. 
  Requires: 
  pdf is C0 smooth over [a, b]
  a <= b 
  *)