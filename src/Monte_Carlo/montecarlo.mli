(** A tree-like probabilistic representation of the theoretical value of an American Option on any given day between start and expiration.
    The accuracy of this model is predicated upon the assumption that option prices are effected by: 
 *)

type 'a price_tree 
(** The abstract type of a Monte-Carlo tree *)

(** val ceate_tree : int list -> 'a price_tree *)

val european_call : int list -> int -> float -> float 
(** [european_call prices num_days discount_rate] is the theoretical value of the option after num_days. 
    Calculated by the probability-weighted sum of the discounted payoffs. *)

val deltas : float list -> float list (** [deltas price] is the array of day-to-day
percent changes. *)
