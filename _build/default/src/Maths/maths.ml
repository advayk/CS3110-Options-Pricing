type pdf_class = 
    | Normal of {stddev : float; mean : float}
    | Other

type pdf = { 
  functn : float -> float ;
  distribution_class : pdf_class ;
}
    
(** Helper function for 4 point gauss-legendre quadrature numeric_integrate. sums
    = *)
let rec numeric_sum (fn : float -> float) (a : float) (acc : int) (n : int) 
(h : float) (weights : float list) (points : float list) = 
  let k = (Float.of_int acc)  in
   if acc = n then 0. else
    h/.2. *. (
  (0 |> List.nth weights) *. fn(h/.2.*. (0 |> List.nth points)+. a+. k*.h +. h/.2.) +.
  (1 |> List.nth weights) *. fn(h/.2.*. (1 |> List.nth points)+. a+. k*.h +. h/.2.) +.
  (1 |> List.nth weights) *. fn(h/.2.*. (2 |> List.nth points)+. a+. k*.h +. h/.2.) +.
  (0 |> List.nth weights) *. fn(h/.2.*. (3 |> List.nth points)+. a+. k*.h +. h/.2.) ) 
  +. numeric_sum fn a (acc+1) n h weights points
 
(** Helper function for integrate, performs a 4 point gauss-legendre quadrature
    numeric integration with a resolution of ~2 per unit, not s.i.s. atm*)
let numeric_integrate (fn : float -> float) (a: float) (b : float) = 
  let weights = [(18. -. Float.sqrt(30.)) /. 36. ;
                 (18. +. Float.sqrt(30.)) /. 36. ] 
  and points = [
    -1. *. Float.sqrt(3. /. 7.) +. (2. /. 7.) *. Float.sqrt(6. /. 5.) ;
    -1. *. Float.sqrt(3. /. 7.) -. (2. /. 7.) *. Float.sqrt(6. /. 5.) ;
           Float.sqrt(3. /. 7.) -. (2. /. 7.) *. Float.sqrt(6. /. 5.) ;
           Float.sqrt(3. /. 7.) +. (2. /. 7.) *. Float.sqrt(6. /. 5.) ]
  (* change the 100 below to whatever maximizes accuracy and minimizes runtime *)
  and n =  100 * ((b-.a) |> Float.ceil |> Float.to_int) in
  let h = (b-.a) /. (Float.of_int n) in
  numeric_sum fn a 0 n h weights points

(* Main function of the module, integrates [p] from [a] to [b] 
 Requires:  [p.functn] is C0 smooth on [a, b], a < b *)
let integrate (p : pdf) (a : float) (b : float) = 
match p.distribution_class with 
| Normal {stddev; mean} -> 0.5 *. 
 (Float.erf( (b-.mean) /. (stddev*.Float.sqrt(2.))) -. 
  Float.erf( (a-.mean) /. (stddev*.Float.sqrt(2.))))
| Other -> numeric_integrate p.functn a b 
