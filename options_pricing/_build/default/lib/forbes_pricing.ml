
(* forbes_pricing.ml *)

(* ========================================================================= *)
(* == SECTION 1: BASIC MATH HELPERS                                        == *)
(* ========================================================================= *)

(* Error function approximation (Abramowitz-Stegun) *)
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

(* Standard normal cumulative distribution function *)
let cdf x =
  0.5 *. (1.0 +. erf (x /. sqrt 2.0))

(* Standard normal PDF *)
let pdf x =
  (1.0 /. sqrt (2.0 *. Float.pi)) *. exp (-0.5 *. x *. x)

(* ========================================================================= *)
(* == SECTION 2: TYPES                                                    == *)
(* ========================================================================= *)

type option_type = Call | Put

type market_data = {
  s : float;           (* Underlying spot price *)
  k : float;           (* Strike price *)
  r : float;           (* Risk-free rate *)
  q : float;           (* Continuous dividend yield *)
  t : float;           (* Time to maturity in years *)
  base_sigma : float;  (* Base volatility (ATM) *)
}

(* ========================================================================= *)
(* == SECTION 3: IMPLIED VOL SURFACE                                      == *)
(* ========================================================================= *)

let implied_vol_surface md =
  let moneyness = md.k /. md.s in
  let skew =
    if moneyness < 1.0 then 0.03 *. (1.0 -. moneyness)
    else if moneyness > 1.0 then 0.02 *. (moneyness -. 1.0)
    else 0.0
  in
  let term_structure = 0.5 *. sqrt md.t in
  md.base_sigma +. skew +. 0.1 *. term_structure

(* ========================================================================= *)
(* == SECTION 4: INSTITUTIONAL PRICING                                    == *)
(* ========================================================================= *)

let institutional_price ~md ~opt_type =
  let sigma = implied_vol_surface md in
  let d1 =
    (log (md.s /. md.k) +. (md.r -. md.q +. 0.5 *. sigma ** 2.0) *. md.t)
    /. (sigma *. sqrt md.t)
  in
  let d2 = d1 -. sigma *. sqrt md.t in

  let price =
    match opt_type with
    | Call ->
        md.s *. exp (-.md.q *. md.t) *. cdf d1
        -. md.k *. exp (-.md.r *. md.t) *. cdf d2
    | Put ->
        md.k *. exp (-.md.r *. md.t) *. cdf (-.d2)
        -. md.s *. exp (-.md.q *. md.t) *. cdf (-.d1)
  in

  (* Add “realistic” institutional adjustments *)
  let bid_ask_spread = 0.01 *. price in
  let liquidity_premium = 0.005 *. price in
  let hedging_cost = 0.002 *. md.base_sigma in

  let mid_price = price +. liquidity_premium -. hedging_cost in
  let bid = mid_price -. bid_ask_spread /. 2.0 in
  let ask = mid_price +. bid_ask_spread /. 2.0 in

  (bid, ask, sigma)

(* ========================================================================= *)
(* == SECTION 5: GREEKS                                                   == *)
(* ========================================================================= *)

let greeks ~md ~opt_type =
  let sigma = implied_vol_surface md in
  let d1 =
    (log (md.s /. md.k) +. (md.r -. md.q +. 0.5 *. sigma ** 2.0) *. md.t)
    /. (sigma *. sqrt md.t)
  in
  let d2 = d1 -. sigma *. sqrt md.t in
  let nd1 = pdf d1 in

  match opt_type with
  | Call ->
      let delta = exp (-.md.q *. md.t) *. cdf d1 in
      let gamma = exp (-.md.q *. md.t) *. nd1 /. (md.s *. sigma *. sqrt md.t) in
      let vega = md.s *. exp (-.md.q *. md.t) *. nd1 *. sqrt md.t *. 0.01 in
      let theta =
        (-.(md.s *. nd1 *. sigma *. exp (-.md.q *. md.t))
         /. (2.0 *. sqrt md.t))
        -. md.r *. md.k *. exp (-.md.r *. md.t) *. cdf d2
        +. md.q *. md.s *. exp (-.md.q *. md.t) *. cdf d1
      in
      let rho = md.k *. md.t *. exp (-.md.r *. md.t) *. cdf d2 *. 0.01 in
      ("call", delta, gamma, vega, theta, rho)
  | Put ->
      let delta = exp (-.md.q *. md.t) *. (cdf d1 -. 1.0) in
      let gamma = exp (-.md.q *. md.t) *. nd1 /. (md.s *. sigma *. sqrt md.t) in
      let vega = md.s *. exp (-.md.q *. md.t) *. nd1 *. sqrt md.t *. 0.01 in
      let theta =
        (-.(md.s *. nd1 *. sigma *. exp (-.md.q *. md.t))
         /. (2.0 *. sqrt md.t))
        +. md.r *. md.k *. exp (-.md.r *. md.t) *. cdf (-.d2)
        -. md.q *. md.s *. exp (-.md.q *. md.t) *. cdf (-.d1)
      in
      let rho = -.md.k *. md.t *. exp (-.md.r *. md.t) *. cdf (-.d2) *. 0.01 in
      ("put", delta, gamma, vega, theta, rho)
