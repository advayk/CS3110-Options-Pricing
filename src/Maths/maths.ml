type pdf_class = 
    | Normal of {stddev : float; mean : float}
    | LogNormal of {sigma_of_log : float; mean_of_log : float}
    | Lorentzian of {gamma : float ; peak : float}
    | Laplace of {lambda : float ; peak : float}
    | Other

type pdf = { 
  functn : float -> float ;
  distribution_class : pdf_class ;
}
    
(** Helper function for 4 point gauss-legendre quadrature numeric_integrate. *)
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

  (* integrates [p] from [a] to [b] 
 Requires:  [p.functn] is C0 smooth on [a, b], a < b *)
let integrate (p : pdf) (a : float) (b : float) = 
match p.distribution_class with 
| Normal {stddev; mean} -> 0.5 *. 
 (Float.erf( (b-.mean) /. (stddev*.Float.sqrt(2.))) -. 
  Float.erf( (a-.mean) /. (stddev*.Float.sqrt(2.))))
| LogNormal {sigma_of_log; mean_of_log} -> 0.5 *. 
 (Float.erf( Float.log (b) -.mean_of_log /. (sigma_of_log*.Float.sqrt(2.))) -. 
  Float.erf( Float.log (a) -.mean_of_log /. (sigma_of_log*.Float.sqrt(2.))))
| Lorentzian {gamma; peak} -> (1. /. Float.pi) *. 
 (Float.atan( (b -. peak) /. gamma) -. 
  Float.atan( (a -. peak) /. gamma))
| Laplace {lambda;peak} -> if b < peak then 
     0.5*.Float.exp(-1. *. lambda *. (b-.peak)) else 
      1. -. 0.5*.Float.exp(-1. *. lambda *. (b-.peak))
| Other -> numeric_integrate p.functn a b 


(* Main function of the module, integrates [p] from -inf to [b] 
 Requires:  [p.functn] is C0 smooth on [-inf, b], integrates to 1 *)
 let cdf (p : pdf) (x : float) = 
  let a =  -999999.0  and b = x in
match p.distribution_class with 
| Normal {stddev; mean} -> 0.5 *. 
 (Float.erf( (b-.mean) /. (stddev*.Float.sqrt(2.))) -. 
  Float.erf( (a-.mean) /. (stddev*.Float.sqrt(2.))))
| LogNormal {sigma_of_log; mean_of_log} -> 0.5 *. 
 (Float.erf( Float.log (b) -.mean_of_log /. (sigma_of_log*.Float.sqrt(2.))) -. 
  Float.erf( Float.log (a) -.mean_of_log /. (sigma_of_log*.Float.sqrt(2.))))
| Lorentzian {gamma; peak} -> (1. /. Float.pi) *. 
 (Float.atan( (b -. peak) /. gamma) -. 
  Float.atan( (a -. peak) /. gamma))
| Laplace {lambda;peak} -> if b < peak then 
     0.5*.Float.exp(-1. *. lambda *. (x-.peak)) else 
      1. -. 0.5*.Float.exp(-1. *. lambda *. (x-.peak))
| Other -> numeric_integrate p.functn a b 


let pdf_value (p : pdf) (x : float) = match p.distribution_class with
| Normal {stddev; mean} -> (1. /. (stddev*.Float.sqrt(2.*.Float.pi)))*.
      Float.exp(- 0.5 *. Float.pow ((x -. mean) /. (stddev)) 2.)
| LogNormal {sigma_of_log; mean_of_log} -> (1. /. (x*.sigma_of_log*.Float.sqrt(2.*.Float.pi)))*.
Float.exp(- 0.5 *. Float.pow ((Float.log(x) -. mean_of_log) /. (sigma_of_log)) 2.)
| Lorentzian {gamma; peak} ->
  Float.pow (gamma *. Float.pi *. (1. +. Float.pow ((x-.peak)/.gamma) 2.)) (-1.)
| Laplace {lambda;peak} -> 
  0.5 *. lambda *. Float.exp(-1.*.lambda*.Float.abs(x-.peak))
| Other -> p.functn x


let generate (p : pdf) = Random.init (Random.int 1000) ; 
match p.distribution_class with
| Normal {stddev ; mean} -> mean +. Float.sqrt(2.)*.stddev*.Float.erfc(2.*. (Random.float 1.) -. 1.)
| Laplace{lambda ; peak} -> let u = (Random.float 1.) in if u > 0.5 then 
  peak -. 1. /. lambda  *. Float.log(2.-.2.*.u) else 
  peak +. 1. /. lambda  *. Float.log(2.*.u)
| LogNormal {sigma_of_log; mean_of_log} -> Float.exp(mean_of_log +. 
  Float.sqrt(2.*.sigma_of_log*.sigma_of_log)*.
  Float.erfc(2.*.(Random.float 1.) -. 1.))
| Lorentzian { gamma;peak} -> peak +. gamma*.Float.tan(Float.pi*.((Random.float 1.) -. 0.5))
| Other -> failwith "unimplemented"