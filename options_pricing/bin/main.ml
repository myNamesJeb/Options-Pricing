
(* main.ml *)
open Quote_csv

(* ========================================================================= *)
(* == SECTION 1: DATA PARSING AND HELPER FUNCTIONS == *)
(* ========================================================================= *)

(* Handle both MM/DD/YYYY and YYYY-MM-DD *)
let parse_date s =
  let s = String.trim s in
  if String.contains s '/' then
    (* Old format: MM/DD/YYYY *)
    match String.split_on_char '/' s with
    | [m; d; y] -> (int_of_string y, int_of_string m, int_of_string d)
    | _ -> failwith ("bad date: " ^ s)
  else if String.contains s '-' then
    (* ISO format: YYYY-MM-DD *)
    match String.split_on_char '-' s with
    | [y; m; d] -> (int_of_string y, int_of_string m, int_of_string d)
    | _ -> failwith ("bad date: " ^ s)
  else
    failwith ("unrecognized date format: " ^ s)

let date_newer (y1, m1, d1) (y2, m2, d2) =
  if y1 <> y2 then y1 > y2 else if m1 <> m2 then m1 > m2 else d1 > d2

let ensure_chronological quotes =
  match quotes with
  | a :: b :: _ ->
      let da = parse_date a.date in
      let db = parse_date b.date in
      if date_newer da db then List.rev quotes else quotes
  | _ -> quotes

(* ========================================================================= *)
(* == SECTION 2: GENERIC PRICING AND SIMULATION LOGIC                     == *)
(* ========================================================================= *)

type pricer = {
  call: s:float -> k:float -> r:float -> t:float -> sigma:float -> float;
  put: s:float -> k:float -> r:float -> t:float -> sigma:float -> float;
}

let calculate_price_data ~pricer ~strike_delta ~window ~trading_days
    ?(default_sigma = 0.20) ?(prediction_horizon = 182) ?(risk_free_rate = 0.01)
    () =
  let quotes = parse_file "HistoricalQuotes.csv" in
  let quotes = ensure_chronological quotes in
  let closes = List.map (fun q -> q.close_last) quotes in
  let vols =
    Volatility.rolling_historical_volatility ~window ~trading_days closes
  in
  let vol_map = List.sort (fun (a, _) (b, _) -> compare a b) vols in
  quotes
  |> List.mapi (fun i q ->
         let sigma =
           match List.assoc_opt i vol_map with
           | Some (Some s) -> s
           | _ -> default_sigma
         in
         let s = q.close_last in
         let k = s +. strike_delta in
         let r = risk_free_rate in
         let t = float_of_int prediction_horizon /. float_of_int trading_days in
         match List.nth_opt quotes (i + prediction_horizon) with
         | Some future_q ->
             let call = pricer.call ~s ~k ~r ~t ~sigma in
             let put = pricer.put ~s ~k ~r ~t ~sigma in
             let call_profit = max 0.0 (future_q.close_last -. k) -. call in
             let put_profit = max 0.0 (k -. future_q.close_last) -. put in
             Some
               ( i,
                 q.date,
                 s,
                 sigma,
                 call,
                 put,
                 future_q.close_last,
                 k,
                 call_profit,
                 put_profit )
         | None -> None)
  |> List.filter_map Fun.id

let run_and_print_simulation ~model_name ~data =
  let n = List.length data in
  if n = 0 then Printf.printf "No data to process for %s model.\n" model_name
  else (
    let total_premiums = ref 0.0 in
    let total_payoffs = ref 0.0 in
    let total_buyer_call_profit = ref 0.0 in
    let total_buyer_put_profit = ref 0.0 in

    List.iter
      (fun (_i, _date, _s, _sigma, call_price, put_price, future_price, k,
            call_profit, put_profit) ->
        total_buyer_call_profit := !total_buyer_call_profit +. call_profit;
        total_buyer_put_profit := !total_buyer_put_profit +. put_profit;
        total_premiums := !total_premiums +. call_price +. put_price;
        let call_payoff = max 0.0 (future_price -. k) in
        let put_payoff = max 0.0 (k -. future_price) in
        total_payoffs := !total_payoffs +. call_payoff +. put_payoff)
      data;

    let company_profit = !total_premiums -. !total_payoffs in
    let avg_call = !total_buyer_call_profit /. float_of_int n in
    let avg_put = !total_buyer_put_profit /. float_of_int n in

    Printf.printf "========================================================\n";
    Printf.printf " SIMULATION RESULTS: %s Model\n" model_name;
    Printf.printf "========================================================\n";
    Printf.printf "Average BUYER call profit = %.2f\n" avg_call;
    Printf.printf "Average BUYER put profit = %.2f\n" avg_put;
    Printf.printf "--------------------------------------------------------\n";
    Printf.printf "Total Premiums Collected (Company Income) = %.2f\n"
      !total_premiums;
    Printf.printf "Total Payoffs Paid Out (Company Expense) = %.2f\n"
      !total_payoffs;
    Printf.printf "Net Company Profit (Income - Expense) = %.2f\n"
      company_profit;
    Printf.printf "========================================================\n\n")

(* ========================================================================= *)
(* == SECTION 2.5: MONTE CARLO MODEL IMPLEMENTATION                       == *)
(* ========================================================================= *)

let gauss () =
  let u1 = Random.float 1.0 in
  let u2 = Random.float 1.0 in
  sqrt (-2.0 *. log u1) *. cos (2.0 *. Float.pi *. u2)

let mc_call_price ~s ~k ~r ~t ~sigma ~n =
  let discount = exp (-.r *. t) in
  let sum = ref 0.0 in
  for _ = 1 to n do
    let z = gauss () in
    let st =
      s *. exp ((r -. 0.5 *. sigma *. sigma) *. t +. sigma *. sqrt t *. z)
    in
    let payoff = max 0.0 (st -. k) in
    sum := !sum +. payoff
  done;
  discount *. (!sum /. float_of_int n)

let mc_put_price ~s ~k ~r ~t ~sigma ~n =
  let discount = exp (-.r *. t) in
  let sum = ref 0.0 in
  for _ = 1 to n do
    let z = gauss () in
    let st =
      s *. exp ((r -. 0.5 *. sigma *. sigma) *. t +. sigma *. sqrt t *. z)
    in
    let payoff = max 0.0 (k -. st) in
    sum := !sum +. payoff
  done;
  discount *. (!sum /. float_of_int n)

(* ========================================================================= *)
(* == SECTION 3: MODEL-SPECIFIC SIMULATION RUNS == *)
(* ========================================================================= *)

(* --- RUN 1: Binomial Tree Model --- *)
let () =
  print_endline "--> Running Binomial Tree Simulation...";
  let steps = 100 in
  let strike_delta = 0. in
  let window = 30 in
  let trading_days = 182 in
  let binomial_pricer =
    {
      call =
        (fun ~s ~k ~r ~t ~sigma ->
          Binomial_tree.call_price ~s ~k ~r ~t ~sigma ~n:steps);
      put =
        (fun ~s ~k ~r ~t ~sigma ->
          Binomial_tree.put_price ~s ~k ~r ~t ~sigma ~n:steps);
    }
  in
  let data =
    calculate_price_data ~pricer:binomial_pricer ~strike_delta ~window
      ~trading_days ()
  in
  run_and_print_simulation ~model_name:"Binomial Tree" ~data:data

;;
(* --- RUN 2: Black-Scholes Model --- *)
let () =
  print_endline "--> Running Black-Scholes Simulation...";
  let strike_delta = 0. in
  let window = 30 in
  let trading_days = 252 in
  let black_scholes_pricer =
    { call = Black_scholes.call_price; put = Black_scholes.put_price }
  in
  let data =
    calculate_price_data ~pricer:black_scholes_pricer ~strike_delta ~window
      ~trading_days ()
  in
  run_and_print_simulation ~model_name:"Black-Scholes" ~data:data

;;
(* --- RUN 3: Monte Carlo Model --- *)
let () =
  print_endline "--> Running Monte Carlo Simulation...";
  let paths = 10000 in
  let strike_delta = 0. in
  let window = 30 in
  let trading_days = 252 in
  let monte_carlo_pricer =
    {
      call =
        (fun ~s ~k ~r ~t ~sigma ->
          mc_call_price ~s ~k ~r ~t ~sigma ~n:paths);
      put =
        (fun ~s ~k ~r ~t ~sigma ->
          mc_put_price ~s ~k ~r ~t ~sigma ~n:paths);
    }
  in
  let data =
    calculate_price_data ~pricer:monte_carlo_pricer ~strike_delta ~window
      ~trading_days ()
  in
  run_and_print_simulation ~model_name:"Monte Carlo" ~data:data

;;
(* --- RUN 4: Forbes Institutional Model --- *)
let () =
  print_endline "--> Running Forbes Institutional Simulation...";
  let strike_delta = 0. in
  let window = 30 in
  let trading_days = 252 in
  let q = 0.01 in
  let forbes_pricer =
    {
      call =
        (fun ~s ~k ~r ~t ~sigma ->
          let md = Forbes_pricing.{ s; k; r; q; t; base_sigma = sigma } in
          let bid, ask, _ =
            Forbes_pricing.institutional_price ~md ~opt_type:Call
          in
          (bid +. ask) /. 2.0);
      put =
        (fun ~s ~k ~r ~t ~sigma ->
          let md = Forbes_pricing.{ s; k; r; q; t; base_sigma = sigma } in
          let bid, ask, _ =
            Forbes_pricing.institutional_price ~md ~opt_type:Put
          in
          (bid +. ask) /. 2.0);
    }
  in
  let data =
    calculate_price_data ~pricer:forbes_pricer ~strike_delta ~window
      ~trading_days ()
  in
  run_and_print_simulation ~model_name:"Forbes Institutional" ~data:data
