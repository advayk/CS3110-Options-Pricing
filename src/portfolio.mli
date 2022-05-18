(** Module to process information for a portfolio of stocks *)

val portfolio_value : float list -> float list -> float -> float
(** [portfolio_value list list] produces the total value of your portfolio x*)

val average_price : float list -> float
(** [average_price list] returns the average price of a prices of a certain stock*)
