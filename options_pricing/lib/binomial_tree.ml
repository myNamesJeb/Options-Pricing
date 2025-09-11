
(* binomial_tree.ml *)

(* Binomial Tree Option Pricing (Cox-Ross-Rubinstein model) *)

(* European call option price *)
let call_price ~s ~k ~r ~t ~sigma ~n =
  let dt = t /. float_of_int n in
  let u = exp (sigma *. sqrt dt) in
  let d = 1.0 /. u in
  let p = (exp (r *. dt) -. d) /. (u -. d) in

  (* terminal payoffs *)
  let prices = Array.init (n + 1) (fun j -> s *. (u ** float_of_int j) *. (d ** float_of_int (n - j))) in
  let payoffs = Array.map (fun st -> max 0.0 (st -. k)) prices in

  (* backward induction *)
  let rec step m arr =
    if m = 0 then arr.(0)
    else
      let new_arr = Array.init m (fun j ->
        let expected = p *. arr.(j+1) +. (1.0 -. p) *. arr.(j) in
        exp (-.r *. dt) *. expected
      ) in
      step (m - 1) new_arr
  in
  step n payoffs

(* European put option price *)
let put_price ~s ~k ~r ~t ~sigma ~n =
  let dt = t /. float_of_int n in
  let u = exp (sigma *. sqrt dt) in
  let d = 1.0 /. u in
  let p = (exp (r *. dt) -. d) /. (u -. d) in

  (* terminal payoffs *)
  let prices = Array.init (n + 1) (fun j -> s *. (u ** float_of_int j) *. (d ** float_of_int (n - j))) in
  let payoffs = Array.map (fun st -> max 0.0 (k -. st)) prices in

  (* backward induction *)
  let rec step m arr =
    if m = 0 then arr.(0)
    else
      let new_arr = Array.init m (fun j ->
        let expected = p *. arr.(j+1) +. (1.0 -. p) *. arr.(j) in
        exp (-.r *. dt) *. expected
      ) in
      step (m - 1) new_arr
  in
  step n payoffs
