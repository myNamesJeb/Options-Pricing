
(* black_scholes.ml *)
(* Standard normal cumulative distribution function *)
let erf x =
  let sign = if x < 0.0 then -1.0 else 1.0 in
  let x = Float.abs x in
  let t = 1.0 /. (1.0 +. 0.3275911 *. x) in
  let y =
    1.0 -. (((((1.061405429 *. t
                 -. 1.453152027) *. t)
                 +. 1.421413741) *. t
                 -. 0.284496736) *. t
                 +. 0.254829592) *. t
                 *. exp (-.x *. x)
  in
  sign *. y

let cdf x =
  0.5 *. (1.0 +. erf (x /. sqrt 2.0))

(* Black-Scholes price for a European call option *)
let call_price~s ~k ~r ~t ~sigma =
  let d1 = (log (s /. k) +. (r +. 0.5 *. sigma ** 2.0) *. t) /. (sigma *. sqrt t) in
  let d2 = d1 -. sigma *. sqrt t in
  s *. cdf d1 -. k *. exp (-. r *. t) *. cdf d2

(* Black-Scholes price for a European put option *)
let put_price ~s ~k ~r ~t ~sigma =
  let d1 = (log (s /. k) +. (r +. 0.5 *. sigma ** 2.0) *. t) /. (sigma *. sqrt t) in
  let d2 = d1 -. sigma *. sqrt t in
  k *. exp (-. r *. t) *. cdf (-.d2) -. s *. cdf (-.d1)
