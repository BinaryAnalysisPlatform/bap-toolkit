open Core_kernel
open Bap.Std
open Find_symbol_types
open Format

module Incidents = struct

  let output_sexp ch s =
    let fmt = Format.formatter_of_out_channel ch in
    Format.fprintf fmt "%a\n%!" Sexp.pp_hum s

  let sexp_of_addr addr =
    Sexp.Atom
      (sprintf "0x%s" @@ Addr.string_of_value ~hex:true addr)

  let sexp_of_check check =
    Sexp.Atom
      (Sexp.to_string (sexp_of_check check) |>
       String.lowercase |>
       String.map ~f:(fun c -> if c = '_' then '-' else c))

  let sexp_of_incident check sym addr =
    let open Sexp in
    let data = match addr with
      | None -> [sexp_of_check check; Atom sym]
      | Some addr ->
        [ sexp_of_check check; Atom sym; sexp_of_addr addr] in
    List [Atom "incident-static"; List data]


  let output ch check name addr =
    output_sexp ch (sexp_of_incident check name addr);
    Out_channel.flush ch

  let dump results =
    Out_channel.with_file "incidents" ~f:(fun ch ->
        Map.iteri results ~f:(fun ~key:{name;addr} ~data:results ->
            List.iter results ~f:(fun {check} ->
                output ch check name addr)))
end

module Ansi = struct
  let pp_bold ppf = fprintf ppf "\027[1m"
  let pp_norm ppf = fprintf ppf "\027[0m"
end

type level =
  | Brief
  | Total

let level_of_int x =
  if x > 1 then Total
  else Brief

let string_of_result r =
  let string_of_data = function
    | None -> ""
    | Some (Complexity x) -> sprintf "%d" x
    | Some (Non_structural_block a) -> sprintf "at %a" Addr.pps a in
  match r.check with
  | Complex_function   -> sprintf "Complexity %s" (string_of_data r.data)
  | Forbidden_function -> sprintf "Forbidden"
  | Recursive_function -> sprintf "Recursive-call"
  | Non_structural_cfg -> sprintf "Non-structural cfg %s" (string_of_data r.data)

let string_of_check = function
  | x ->
    Sexp.to_string (sexp_of_check x) |>
    String.lowercase |>
    String.tr ~target:'_' ~replacement:' '

let print_table fmt ~print_addrs rs =
  let print_sym name addr results =
    fprintf fmt "%-25s " name;
    if print_addrs then
      begin
        match addr with
        | Some addr -> fprintf fmt "%a " Addr.pp addr
        | None -> ()
      end;
    fprintf fmt "[";
    match results with
    | []  -> fprintf fmt "]\n"
    | [r] -> fprintf fmt "%s]\n" @@ string_of_result r
    | rs ->
      List.fold rs ~init:[] ~f:(fun ac r -> string_of_result r :: ac) |>
      List.rev |>
      String.concat ~sep:"; " |>
      fprintf fmt "%s]\n" in
  fprintf fmt "\n%t%-25s " Ansi.pp_bold  "Symbol found";
  if print_addrs then
    fprintf fmt "%-8s " "Address";
  fprintf fmt "Criteria\n%t" Ansi.pp_norm;
  Map.iteri rs ~f:(fun ~key:{name;addr} ~data:results ->
      print_sym name addr results)

let brief_report fmt requested rs =
  let output status check =
    fprintf fmt "%-25s %s\n" (string_of_check check) status in
  let fails =
    Map.fold rs ~init:(Set.empty (module Check))
      ~f:(fun ~key:_ ~data:results checks ->
          List.fold results ~init:checks ~f:(fun rs {check} ->
              Set.add rs check)) in
  let requested = Set.of_list (module Check) requested in
  let oks = Set.diff requested fails in
  fprintf fmt "%t%-25s %s\n%t" Ansi.pp_bold "Check" "Status" Ansi.pp_norm;
  Set.iter fails ~f:(output "FAIL");
  Set.iter oks   ~f:(output "OK")

let dump_results ?(print_addrs=true) ~level requested rs =
  let open Format in
  brief_report std_formatter requested rs;
  match level with
  | Total ->
    if not (Map.is_empty rs) then
      print_table std_formatter ~print_addrs rs
  | Brief -> ()

let dump ?(print_addrs=true) ?(requested=[]) ~level results =
  dump_results ~print_addrs ~level requested results

let dump_incidents = Incidents.dump
