
(* Volatility computations: log returns, sample stddev, annualization. *)

let log_returns_of_closes closes =
  (* returns list in same chronological order: returns[i] = ln(closes[i+1]/closes[i]) *)
  let rec aux acc = function
    | [] | [_] -> List.rev acc
    | a :: (b :: _ as tl) ->
        let r = log (b /. a) in
        aux (r :: acc) tl
  in
  aux [] closes

let sample_stddev xs =
  let n = List.length xs in
  if n < 2 then None
  else
    let sum = List.fold_left ( +. ) 0.0 xs in
    let mean = sum /. float_of_int n in
    let sumsq = List.fold_left (fun acc x -> let d = x -. mean in acc +. d *. d) 0.0 xs in
    let variance = sumsq /. float_of_int (n - 1) in
    Some (sqrt variance)

let historical_volatility_from_returns ?(trading_days=252) ~window returns =
  (* uses most recent [window] returns from the returns list *)
  let rlen = List.length returns in
  if rlen < window then None
  else
    let start = rlen - window in
    let rec slice i acc = function
      | [] -> List.rev acc
      | x :: xs ->
          if i >= start then slice (i+1) (x :: acc) xs
          else slice (i+1) acc xs
    in
    let window_returns = slice 0 [] returns in
    match sample_stddev window_returns with
    | None -> None
    | Some s -> Some (s *. sqrt (float_of_int trading_days))

let rolling_historical_volatility ?(trading_days=252) ~window closes =
  (* closes must be chronological (oldest first). returns list aligned to closes indices.
     For index i, volatility is computed using returns ending at i (so requires at least
     window returns before i), i.e., volatility at i is based on closes [i-window .. i]. *)
  let returns = log_returns_of_closes closes in
  
  (* For closes len = m, returns len = m - 1. Vol at close index 0 is None (no returns).
     We'll compute volatility for each close index i by using returns that end at i-1. *)
  let m = List.length closes in
  let rec build idx acc =
    if idx >= m then List.rev acc
    else
      let vol =
        (* returns that end at idx-1 are returns[0 .. idx-2] ; to get last `window` returns
           ending at idx-1 we need there to be >= window returns up to idx-1 => idx-1 >= window-1 => idx >= window *)
        if idx < window then None
        else
          let upto = idx - 1 in
          (* take returns[upto-window+1 .. upto] i.e. last window returns ending at upto *)
          let start = upto - (window - 1) in
          let rec slice i acc = function
            | [] -> List.rev acc
            | x :: xs ->
                if i > upto then List.rev acc
                else if i >= start then slice (i+1) (x :: acc) xs
                else slice (i+1) acc xs
          in
          let window_returns = slice 0 [] returns in
          match sample_stddev window_returns with
          | None -> None
          | Some s -> Some (s *. sqrt (float_of_int trading_days))
      in
      build (idx + 1) ((idx, vol) :: acc)
  in
  build 0 []
