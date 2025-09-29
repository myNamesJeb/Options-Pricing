
open Quote_csv

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
(*
let () =
  let quotes = parse_file "HistoricalQuotes.csv" in
  let quotes = ensure_chronological quotes in
  Printf.printf "Parsed %d quotes\n" (List.length quotes);

  let closes = List.map (fun q -> q.close_last) quotes in
  let window = 20 in
  let trading_days = 252 in
  let vols =
    Volatility.rolling_historical_volatility ~window ~trading_days closes
  in
  let default_sigma = 0.20 in
  let vol_map = List.sort (fun (a, _) (b, _) -> compare a b) vols in

  let prediction_horizon = 182 in  (* about half a trading year *)

  quotes
  |> List.mapi (fun i q ->
         (* get sigma for current index i *)
         let sigma =
           match List.assoc_opt i vol_map with
           | Some (Some s) -> s
           | _ -> default_sigma
         in
         let s = q.close_last in
         let k = s in
         let r = 0.01 in
         let t = float_of_int prediction_horizon /. float_of_int trading_days in
         (* predict if we have enough candles ahead *)
         match List.nth_opt quotes (i + prediction_horizon) with
         | Some future_q ->
             let call = Black_scholes.call_price ~s ~k ~r ~t ~sigma in
             let put = Black_scholes.put_price ~s ~k ~r ~t ~sigma in
             Some (i, q.date, s, sigma, call, put, future_q.close_last)
         | None -> None)
  |> List.filter_map (fun x -> x) 
  |> List.iter (fun (i, date, s, sigma, call, put, future) ->
       if i mod prediction_horizon = 0 then
         Printf.printf
           "%s | spot=%.2f | sigma=%.4f | Call=%.2f | Put=%.2f | Future Close[%d]=%.2f\n"
           date s sigma call put (i + prediction_horizon) future)


let () =
  let quotes = parse_file "HistoricalQuotes.csv" in
  let quotes = ensure_chronological quotes in
  Printf.printf "Parsed %d quotes\n" (List.length quotes);

  let closes = List.map (fun q -> q.close_last) quotes in
  let window = 20 in
  let trading_days = 252 in
  let vols =
    Volatility.rolling_historical_volatility ~window ~trading_days closes
  in
  let default_sigma = 0.20 in
  let vol_map = List.sort (fun (a, _) (b, _) -> compare a b) vols in

  let prediction_horizon = 182 in  (* about half a trading year *)
  let steps = 100 in  (* binomial tree steps, tweak for accuracy/speed tradeoff *)

  quotes
  |> List.mapi (fun i q ->
         (* get sigma for current index i *)
         let sigma =
           match List.assoc_opt i vol_map with
           | Some (Some s) -> s
           | _ -> default_sigma
         in
         let s = q.close_last in
         let k = s in
         let r = 0.01 in
         let t = float_of_int prediction_horizon /. float_of_int trading_days in
         (* predict if we have enough candles ahead *)
         match List.nth_opt quotes (i + prediction_horizon) with
         | Some future_q ->
             let call = Binomial_tree.call_price ~s ~k ~r ~t ~sigma ~n:steps in
             let put  = Binomial_tree.put_price  ~s ~k ~r ~t ~sigma ~n:steps in
             Some (i, q.date, s, sigma, call, put, future_q.close_last)
         | None -> None)
  |> List.filter_map (fun x -> x) 
  |> List.iter (fun (i, date, s, sigma, call, put, future) ->
       if i mod prediction_horizon = 0 then
         Printf.printf
           "%s | spot=%.2f | sigma=%.4f | Call=%.2f | Put=%.2f | Future Close[%d]=%.2f\n"
           date s sigma call put (i + prediction_horizon) future)

*)
let calculate_price ~strike_delta =
  let quotes = parse_file "HistoricalQuotes.csv" in
  let quotes = ensure_chronological quotes in
  Printf.printf "Parsed %d quotes\n" (List.length quotes);

  let closes = List.map (fun q -> q.close_last) quotes in
  let window = 20 in
  let trading_days = 252 in
  let vols = Volatility.rolling_historical_volatility ~window ~trading_days closes in
  let default_sigma = 0.20 in
  let vol_map = List.sort (fun (a, _) (b, _) -> compare a b) vols in

  let prediction_horizon = 182 in
  let steps = 100 in

  quotes
  |> List.mapi (fun i q ->
         let sigma =
           match List.assoc_opt i vol_map with
           | Some (Some s) -> s
           | _ -> default_sigma
         in
         let s = q.close_last in
         let k = s +. strike_delta in  (* define strike based on parameter *)
         let r = 0.01 in
         let t = float_of_int prediction_horizon /. float_of_int trading_days in
         match List.nth_opt quotes (i + prediction_horizon) with
         | Some future_q ->
             let call = Binomial_tree.call_price ~s ~k ~r ~t ~sigma ~n:steps in
             let put  = Binomial_tree.put_price  ~s ~k ~r ~t ~sigma ~n:steps in
             let call_profit = max 0.0 (future_q.close_last -. k) -. call in
             let put_profit  = max 0.0 (k -. future_q.close_last) -. put in
             Some (i, q.date, s, sigma, call, put, future_q.close_last, k, call_profit, put_profit)
         | None -> None)
  |> List.filter_map Fun.id
  |> List.iter (fun (i, date, s, sigma, call, put, future, k, call_profit, put_profit) ->
       if i mod prediction_horizon = 0 then
         Printf.printf
           "%s | spot=%.2f | sigma=%.4f | Call=%.2f | Put=%.2f | Future Close[%d]=%.2f | Strike: %.2f | call_profit: %.2f | put_profit: %.2f\n"
           date s sigma call put (i + prediction_horizon) future k call_profit put_profit)


let calculate_price_data ~strike_delta =
  let quotes = parse_file "HistoricalQuotes.csv" in
  let quotes = ensure_chronological quotes in

  let closes = List.map (fun q -> q.close_last) quotes in
  let window = 20 in
  let trading_days = 252 in
  let vols = Volatility.rolling_historical_volatility ~window ~trading_days closes in
  let default_sigma = 0.20 in
  let vol_map = List.sort (fun (a, _) (b, _) -> compare a b) vols in

  let prediction_horizon = 182 in
  let steps = 100 in

  quotes
  |> List.mapi (fun i q ->
         let sigma =
           match List.assoc_opt i vol_map with
           | Some (Some s) -> s
           | _ -> default_sigma
         in
         let s = q.close_last in
         let k = s +. strike_delta in
         let r = 0.01 in
         let t = float_of_int prediction_horizon /. float_of_int trading_days in
         match List.nth_opt quotes (i + prediction_horizon) with
         | Some future_q ->
             let call = Binomial_tree.call_price ~s ~k ~r ~t ~sigma ~n:steps in
             let put  = Binomial_tree.put_price  ~s ~k ~r ~t ~sigma ~n:steps in
             let call_profit = max 0.0 (future_q.close_last -. k) -. call in
             let put_profit  = max 0.0 (k -. future_q.close_last) -. put in
             Some (i, q.date, s, sigma, call, put, future_q.close_last, k, call_profit, put_profit)
         | None -> None)
  |> List.filter_map Fun.id




let () = calculate_price ~strike_delta:1.
let () =
  let _data = calculate_price_data ~strike_delta:5.0 in
  List.iter (fun (_i, date, s, _sigma, _call, _put, _future, k, call_profit, put_profit) ->
  Printf.printf "%s | spot=%.2f | Strike=%.2f | call_profit=%.2f | put_profit=%.2f\n"
    date s k call_profit put_profit
) _data
