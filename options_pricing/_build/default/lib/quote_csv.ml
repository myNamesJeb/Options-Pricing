
(* quote_csv.ml *)

type quote = {
  date : string;
  open_ : float;
  high : float;
  low : float;
  close_last : float;  (* we will store Adj Close here for pricing *)
  raw_close : float;   (* keep original Close column too, in case needed *)
  volume : int;
}

let trim = String.trim

let remove_chars s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | '$' | ' ' | ',' -> ()  (* drop dollar signs, spaces, commas *)
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let parse_float_field field =
  float_of_string (remove_chars (trim field))

let parse_int_field field =
  int_of_string (remove_chars (trim field))

let split_comma line =
  let rec loop i last acc =
    if i >= String.length line then
      List.rev ((String.sub line last (i - last)) :: acc)
    else if line.[i] = ',' then
      loop (i + 1) (i + 1) ((String.sub line last (i - last)) :: acc)
    else loop (i + 1) last acc
  in
  loop 0 0 []

let parse_line ~lineno line =
  match split_comma line |> List.map trim with
  | [date; open_; high; low; close_; adj_close; volume] ->
      let open_ = parse_float_field open_ in
      let high = parse_float_field high in
      let low = parse_float_field low in
      let raw_close = parse_float_field close_ in
      let adj_close = parse_float_field adj_close in
      let volume = parse_int_field volume in
      (* store adj_close as the main `close_last` for pricing *)
      Ok { date; open_; high; low; close_last = adj_close; raw_close; volume }
  | cols ->
      Error
        (Printf.sprintf "line %d: expected 7 columns, got %d"
           lineno (List.length cols))

let parse_lines lines =
  let consume_header = function
    | [] -> []
    | h :: t ->
        if String.lowercase_ascii (trim h) |> String.starts_with ~prefix:"date"
        then t
        else h :: t
  in
  let body = consume_header lines in
  let rec go idx acc_ok acc_err = function
    | [] -> (List.rev acc_ok, List.rev acc_err)
    | line :: rest ->
        if String.trim line = "" then go (idx + 1) acc_ok acc_err rest
        else
          match parse_line ~lineno:idx line with
          | Ok q -> go (idx + 1) (q :: acc_ok) acc_err rest
          | Error e -> go (idx + 1) acc_ok ((idx, e) :: acc_err) rest
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
  List.iter
    (fun (lineno, msg) ->
      prerr_endline
        (Printf.sprintf "Warning parsing line %d: %s" lineno msg))
    errs;
  quotes
