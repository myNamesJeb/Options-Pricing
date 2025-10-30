
(* Monte Carlo simulation for European call and put options *)

(*open Random*)

(* Helper: draw a standard normal random variable via Box-Muller *)
let gauss () =
  let u1 = Random.float 1.0 in
  let u2 = Random.float 1.0 in
  sqrt (-2.0 *. log u1) *. cos (2.0 *. Float.pi *. u2)

(* Monte Carlo call price *)
let mc_call_price ~s ~k ~r ~t ~sigma ~n =
  let discount = exp (-. r *. t) in
  let sum = ref 0.0 in
  for _ = 1 to n do
    let z = gauss () in
    let st = s *. exp ((r -. 0.5 *. sigma *. sigma) *. t +. sigma *. sqrt t *. z) in
    let payoff = max 0.0 (st -. k) in
    sum := !sum +. payoff
  done;
  discount *. (!sum /. float_of_int n)

(* Monte Carlo put price *)
let mc_put_price ~s ~k ~r ~t ~sigma ~n =
  let discount = exp (-. r *. t) in
  let sum = ref 0.0 in
  for _ = 1 to n do
    let z = gauss () in
    let st = s *. exp ((r -. 0.5 *. sigma *. sigma) *. t +. sigma *. sqrt t *. z) in
    let payoff = max 0.0 (k -. st) in
    sum := !sum +. payoff
  done;
  discount *. (!sum /. float_of_int n)
