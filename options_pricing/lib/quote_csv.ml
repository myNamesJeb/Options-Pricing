
(* Lightweight CSV parser for the provided HistoricalQuotes.csv format.
   No external dependencies. *)

type quote = {
  date : string;
  close_last : float;
  volume : int;
  open_ : float;
  high : float;
  low : float;
}

(* Helpers *)
let trim = String.trim

let remove_chars s =
  (* remove $ signs and any spaces; leave period and digits and minus *)
  let b = Buffer.create (String.length s) in
  String.iter (fun c ->
    match c with
    | '$' | ' ' -> ()  (* drop dollar sign and spaces *)
    | ',' -> ()        (* drop thousand separators if present *)
    | _ -> Buffer.add_char b c
  ) s;
  Buffer.contents b

let parse_float_field field =
  try
    let cleaned = remove_chars field |> trim in
    float_of_string cleaned
  with Failure _ as e ->
    raise e

let parse_int_field field =
  try
    let cleaned = remove_chars field |> trim in
    (* allow large volumes; int_of_string may raise if too large for int.
       If that becomes an issue, switch to int64. *)
    int_of_string cleaned
  with Failure _ as e ->
    raise e

let split_comma line =
  (* naive split on comma; adequate for your example where fields are not quoted. *)
  let rec loop i last acc =
    if i >= String.length line then
      List.rev ((String.sub line last (i - last)) :: acc)
    else if line.[i] = ',' then
      loop (i + 1) (i + 1) ((String.sub line last (i - last)) :: acc)
    else loop (i + 1) last acc
  in
  loop 0 0 []

let parse_line ~lineno line =
  let cols = split_comma line |> List.map trim in
  match cols with
  | date :: close :: volume :: open_ :: high :: low :: [] ->
      let date = date in
      let close_last = parse_float_field close in
      let volume = parse_int_field volume in
      let open_ = parse_float_field open_ in
      let high = parse_float_field high in
      let low = parse_float_field low in
      Ok { date; close_last; volume; open_; high; low }
  | _ ->
      Error (Printf.sprintf "line %d: expected 6 columns, got %d" lineno (List.length cols))

let parse_lines lines =
  (* Skip header if it looks like a header; skip blank lines *)
  let rec consume_header = function
    | [] -> []
    | h :: t ->
        let low = String.lowercase_ascii (trim h) in
        if String.length low = 0 then consume_header t
        else if String.contains low 'd' && String.contains low 'o' then
          (* heuristically treat this as header: "Date, Close/Last, ..." *)
          t
        else
          (h :: t)
  in
  let body = consume_header lines in
  let rec go idx acc_ok acc_err = function
    | [] -> (List.rev acc_ok, List.rev acc_err)
    | line :: rest ->
        let lineno = idx in
        if String.trim line = "" then go (idx + 1) acc_ok acc_err rest
        else
          match parse_line ~lineno line with
          | Ok q -> go (idx + 1) (q :: acc_ok) acc_err rest
          | Error e -> go (idx + 1) acc_ok ((lineno, e) :: acc_err) rest
  in
  go 1 [] [] body

let read_all_lines path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
       let rec loop acc =
         try
           let line = input_line ic in
           loop (line :: acc)
         with End_of_file -> List.rev acc
       in
       loop [])

let parse_file_with_errors path =
  let lines = read_all_lines path in
  parse_lines lines

let parse_file path =
  let quotes, errs = parse_file_with_errors path in
  (* Print warnings for parse errors *)
  List.iter (fun (lineno, msg) ->
    prerr_endline (Printf.sprintf "Warning parsing %s: %s" (string_of_int lineno) msg)
  ) errs;
  quotes
