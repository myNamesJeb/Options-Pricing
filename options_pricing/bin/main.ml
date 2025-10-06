(* main.ml *)

open Quote_csv

(* ========================================================================= *)
(* == SECTION 1: DATA PARSING AND HELPER FUNCTIONS                        == *)
(* ========================================================================= *)
(* These functions are for processing the historical quotes CSV. *)

let parse_date s =
  try
    match String.split_on_char '/' (String.trim s) with
    | [m; d; y] -> (int_of_string y, int_of_string m, int_of_string d)
    | _ -> failwith "bad date"
  with _ -> failwith ("parse_date: " ^ s)

let date_newer (y1, m1, d1) (y2, m2, d2) =
  if y1 <> y2 then y1 > y2
  else if m1 <> m2 then m1 > m2
  else d1 > d2

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
(* This section defines a generic "pricer" interface and the main
   simulation logic, which can work with ANY pricing model. *)

(** A record to hold pricing functions for any model. This makes our
    simulation logic independent of the specific pricing implementation. *)
type pricer = {
  call: s:float -> k:float -> r:float -> t:float -> sigma:float -> float;
  put: s:float -> k:float -> r:float -> t:float -> sigma:float -> float;
}

(** The main data calculation function, now parameterized by a `pricer`. *)
let calculate_price_data
    ~pricer
    ~strike_delta
    ~window         (* rolling volatility window, e.g. 30 *)
    ~trading_days   (* trading days in a year, e.g. 252 *)
    ?(default_sigma = 0.20)
    ?(prediction_horizon = 182)
    ?(risk_free_rate = 0.01)
    () =
  (* Load and preprocess quotes *)
  let quotes = parse_file "HistoricalQuotes.csv" in
  let quotes = ensure_chronological quotes in

  (* Extract closing prices and compute rolling volatilities *)
  let closes = List.map (fun q -> q.close_last) quotes in
  (* Assuming Volatility.rolling_historical_volatility returns a (int * float option) list *)
  let vols = Volatility.rolling_historical_volatility ~window ~trading_days closes in
  (* Note: Using List.assoc_opt for lookups in a loop can be inefficient (O(n^2) total).
     For better performance, consider using a Hashtbl or an Array for the volatilities. *)
  let vol_map = List.sort (fun (a, _) (b, _) -> compare a b) vols in

  (* Main loop over quotes *)
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
          (* USE THE GENERIC PRICER *)
          let call = pricer.call ~s ~k ~r ~t ~sigma in
          let put  = pricer.put  ~s ~k ~r ~t ~sigma in
          let call_profit = max 0.0 (future_q.close_last -. k) -. call in
          let put_profit  = max 0.0 (k -. future_q.close_last) -. put in
          Some (i, q.date, s, sigma, call, put, future_q.close_last, k, call_profit, put_profit)
      | None -> None)
  |> List.filter_map Fun.id


(** A generic function to run the simulation and print a summary report. *)
let run_and_print_simulation ~model_name ~data =
  let n = List.length data in
  if n = 0 then
    Printf.printf "No data to process for %s model.\n" model_name
  else
    begin
      let total_premiums = ref 0.0 in
      let total_payoffs = ref 0.0 in
      let total_buyer_call_profit = ref 0.0 in
      let total_buyer_put_profit = ref 0.0 in

      List.iter (fun (_i, _date, _s, _sigma, call_price, put_price, future_price, k, call_profit, put_profit) ->
        (* Accumulate buyer profits *)
        total_buyer_call_profit := !total_buyer_call_profit +. call_profit;
        total_buyer_put_profit := !total_buyer_put_profit +. put_profit;

        (* Accumulate company income (premiums) *)
        total_premiums := !total_premiums +. call_price +. put_price;

        (* Accumulate company expense (payoffs) *)
        let call_payoff = max 0.0 (future_price -. k) in
        let put_payoff = max 0.0 (k -. future_price) in
        total_payoffs := !total_payoffs +. call_payoff +. put_payoff;
      ) data;

      let company_profit = !total_premiums -. !total_payoffs in
      let avg_call = !total_buyer_call_profit /. float_of_int n in
      let avg_put = !total_buyer_put_profit /. float_of_int n in

      Printf.printf "========================================================\n";
      Printf.printf "          SIMULATION RESULTS: %s Model\n" model_name;
      Printf.printf "========================================================\n";
      Printf.printf "Average BUYER call profit = %.2f\n" avg_call;
      Printf.printf "Average BUYER put profit  = %.2f\n" avg_put;
      Printf.printf "--------------------------------------------------------\n";
      Printf.printf "Total Premiums Collected (Company Income)   = %.2f\n" !total_premiums;
      Printf.printf "Total Payoffs Paid Out (Company Expense)    = %.2f\n" !total_payoffs;
      Printf.printf "Net Company Profit (Income - Expense)       = %.2f\n" company_profit;
      Printf.printf "========================================================\n\n"
    end

(* ========================================================================= *)
(* == SECTION 3: MODEL-SPECIFIC SIMULATION RUNS                           == *)
(* ========================================================================= *)

(* --- RUN 1: Binomial Tree Model --- *)
let () =
  print_endline "--> Running Binomial Tree Simulation...";

  (* Define the parameters for this run *)
  let steps = 100 in
  let strike_delta = 0. in
  let window = 30 in
  let trading_days = 182 in

  (* Create a pricer instance for the Binomial Tree model *)
  let binomial_pricer = {
    call = (fun ~s ~k ~r ~t ~sigma -> Binomial_tree.call_price ~s ~k ~r ~t ~sigma ~n:steps);
    put  = (fun ~s ~k ~r ~t ~sigma -> Binomial_tree.put_price ~s ~k ~r ~t ~sigma ~n:steps);
  } in

  (* Calculate data using the specific pricer and parameters *)
  let data =
    calculate_price_data
      ~pricer:binomial_pricer
      ~strike_delta
      ~window
      ~trading_days
      ()
  in

  (* Run the simulation and print the report *)
  run_and_print_simulation ~model_name:"Binomial Tree" ~data:data
;;


(* --- RUN 2: Black-Scholes Model --- *)
let () =
  print_endline "--> Running Black-Scholes Simulation...";

  (* Define the parameters for this run (kept same for direct comparison) *)
  let strike_delta = 0. in
  let window = 30 in
  let trading_days = 252 in

  (* Create a pricer instance for the Black-Scholes model *)
  let black_scholes_pricer = {
    call = Black_scholes.call_price;
    put  = Black_scholes.put_price;
  } in

  (* Calculate data using the specific pricer and parameters *)
  let data =
    calculate_price_data
      ~pricer:black_scholes_pricer
      ~strike_delta
      ~window
      ~trading_days
      ()
  in

  (* Run the simulation and print the report *)
  run_and_print_simulation ~model_name:"Black-Scholes" ~data:data
;;
