
open Quote_csv
(* open Black_scholes *)

(* Ensure chronological order (oldest first). The parser returns file order; many CSVs
   are newest-first, so we reverse if necessary. We'll guess by comparing first two dates:
   if first date is after second date we assume newest-first and reverse. *)

(* Simple date parse for MM/DD/YYYY -> (y,m,d) *)
let parse_date s =
  try
    match String.split_on_char '/' (String.trim s) with
    | [m; d; y] -> (int_of_string y, int_of_string m, int_of_string d)
    | _ -> failwith "bad date"
  with _ -> failwith ("parse_date: " ^ s)

let date_newer (y1,m1,d1) (y2,m2,d2) =
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

let () =
  let quotes = parse_file "HistoricalQuotes.csv" in
  let quotes = ensure_chronological quotes in
  Printf.printf "Parsed %d quotes (chronological %B)\n" (List.length quotes) true;

  let closes = List.map (fun q -> q.close_last) quotes in

  let window = 20 in
  let trading_days = 252 in
  let vols = Volatility.rolling_historical_volatility ~window ~trading_days closes in
  (* vols : (index * float option) list where index aligns with closes/quotes *)

  let default_sigma = 0.20 in

  (* Create a map from index -> vol option for quick lookup *)
  let vol_map = List.fold_left (fun acc (i, v) -> (i, v) :: acc) [] vols in
  let vol_map = List.sort (fun (a,_) (b,_) -> compare a b) vol_map in
(*
  (* Print first 10 rows with computed sigma *)
  quotes
  |> List.mapi (fun i q ->
       let sigma =
         match List.assoc_opt i vol_map with
         | Some (Some s) -> s
         | _ -> default_sigma
       in
       let s = q.close_last in
       let k = s in
       let r = 0.01 in
       let t = 0.5 in
       let call = Black_scholes.call_price ~s ~k ~r ~t ~sigma in
       let put = Black_scholes.put_price ~s ~k ~r ~t ~sigma in
       (i, q.date, s, sigma, call, put))
  |> List.iter (fun (i, date, s, sigma, call, put) ->
       if i < 10 then
         Printf.printf "%s | close=%.2f | sigma=%.4f | Call=%.2f | Put=%.2f\n"
           date s sigma call put
     )
*)
  
  quotes
  |> List.mapi (fun i q ->
       let sigma =
         match List.assoc_opt i vol_map with
         | Some (Some s) -> s
         | _ -> default_sigma
       in
       let s = q.close_last in
       let k = s in
       let r = 0.01 in
       let t = 0.5 in
       let n = 8 in
       let call = Binomial_tree.call_price ~s ~k ~r ~t ~sigma ~n in
       let put = Binomial_tree.put_price ~s ~k ~r ~t ~sigma ~n in
       (i, q.date, s, sigma, call, put))
  |> List.iter (fun (i, date, s, sigma, call, put) ->
       if i < 10 then
         Printf.printf "%s | close=%.2f | sigma=%.4f | Call=%.2f | Put=%.2f\n"
           date s sigma call put
     )
