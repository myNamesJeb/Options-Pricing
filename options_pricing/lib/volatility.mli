
(** Volatility helpers *)

val log_returns_of_closes : float list -> float list
(** Given a list of closes in chronological order (oldest first), returns the
    list of log returns between consecutive closes. If closes = [s0; s1; s2],
    returns [ln(s1/s0); ln(s2/s1)]. *)

val sample_stddev : float list -> float option
(** Sample standard deviation of a list of values. Returns None if length < 2. *)

val historical_volatility_from_returns :
  ?trading_days:int ->
  window:int ->
  float list ->
  float option
(** Given a list of log returns (chronological), compute sample stddev over the
    most recent [window] returns and annualize by sqrt(trading_days). *)

val rolling_historical_volatility :
  ?trading_days:int ->
  window:int ->
  float list ->
  (int * float option) list
(** Given a list of closes (chronological; oldest first), returns a list of
    (index, volatility_option) for each close index. The volatility at index i
    uses the previous [window] returns ending at i (i.e., returns between
    closes (i-window) .. i). Index corresponds to the closes list index. *)
