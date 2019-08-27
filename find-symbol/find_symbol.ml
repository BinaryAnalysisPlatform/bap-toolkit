open Core_kernel
open Bap.Std
open Graphlib.Std
open Format

module Cfg = Graphs.Cfg
module Callg = Graphs.Callgraph

module Incident = Find_symbol_incident

let () = Incident.enable ()

open Find_symbol_utils
open Find_symbol_types

include Self ()

type result = {
  addr : addr;
  reason : reason;
  data : symbol_data option;
}

type check =
  | Symbols of (symtab -> result list)
  | Program of (program term -> result list)

let result ?data addr reason = {addr;reason;data}

let find_non_structured symtab =
  let reason = Non_structural_cfg in
  Symtab.to_sequence symtab |>
    Seq.fold ~init:[] ~f:(fun rs (name,entry,cfg) ->
        match find_nonstructural_component cfg entry with
        | None -> rs
        | Some addr ->
           let start = Block.addr entry in
           Incident.notify reason start;
           result ~data:(Non_structural_block addr) start reason :: rs)

let find_names names symtab =
  let reason = Forbidden_function in
  Symtab.to_sequence symtab |>
  Seq.fold ~init:[] ~f:(fun rs (name,entry,_) ->
      if List.mem ~equal:String.equal names name then
        let addr = Block.addr entry in
        let () = Incident.notify reason addr in
        result addr reason :: rs
      else rs)

let find_usage names prog =
  let reason = Forbidden_function in
  let g = Program.to_graph prog in
  let names = Set.of_list (module String) names in
  let tids =
    Term.to_sequence sub_t prog |>
      Seq.fold ~init:(Map.empty (module Tid)) ~f:(fun xs s ->
          if Set.mem names (Sub.name s) then
            Map.set xs (Term.tid s) s
          else xs) in
  Map.fold tids ~init:[] ~f:(fun ~key:tid ~data:sub xs ->
      match Callg.Node.inputs tid g |> Seq.to_list with
      | []  -> xs
      | inputs ->
         match Term.get_attr sub address with
         | None -> xs
         | Some addr ->
            Incident.notify reason addr;
            result addr reason :: xs)

let find_recursive prog =
  let reason = Recursive_function in
  List.filter_map (Find_symbol_rec.find prog)
    ~f:(fun (_name,addr) ->
      Incident.(notify reason addr);
      Some (result addr reason))

let find_complex threshold symtab =
  let reason = Complex_function in
  match threshold with
  | None -> []
  | Some threshold ->
     Symtab.to_sequence symtab |>
       Seq.fold ~init:[] ~f:(fun rs (name,entry,cfg) ->
           let c = complexity cfg entry in
           if c > threshold then
             let start = Block.addr entry in
             let () = Incident.notify reason start in
             result ~data:(Complexity c) start reason :: rs
           else rs)

let report_symbol symtab addr results =
  let string_of_data = function
    | None -> ""
    | Some (Complexity x) -> sprintf "%d" x
    | Some (Non_structural_block a) -> sprintf "at %a" Addr.pps a in
  let string_of_reason d  = function
    | Complex_function -> sprintf "Complexity %s" (string_of_data d)
    | Forbidden_function -> sprintf "Forbidden"
    | Recursive_function -> sprintf "Recursive-call"
    | Non_structural_cfg -> sprintf "Non-structural cfg %s" (string_of_data d) in
  match Symtab.find_by_start symtab addr with
  | None -> ()
  | Some (name,_,_) ->
    printf "%-25s %a [" name Addr.pp addr;
    match results with
    | [sym] -> printf "%s]\n" @@ string_of_reason sym.data sym.reason;
    | syms ->
       List.fold syms ~init:[] ~f:(fun ac s ->
           string_of_reason s.data s.reason :: ac) |>
         List.rev |>
         String.concat ~sep:"; " |>
      printf "%s]\n"


let plain_report symtab results =
  let print header names =
    if Set.is_empty names then ()
    else
      printf "\n%s:\n" header;
      let all = Set.to_list names in
      let rec pr = function
        | [] -> ()
        | xs ->
           let fst = List.take xs 4 in
           let lst = List.drop xs 4 in
           let str = List.fold fst ~init:"" ~f:(sprintf "%s%s ") in
           printf "%s\n" str;
           pr lst in
      pr all in
  let r = String.Set.empty in
  let c = String.Set.empty in
  let f = String.Set.empty in
  let n = String.Set.empty in
  let r,c,f,n =
  Map.fold results ~init:(r,c,f,n) ~f:(fun ~key:addr ~data acc ->
      match Symtab.find_by_start symtab addr with
      | None -> acc
      | Some (name,_,_) ->
         List.fold data ~init:acc ~f:(fun (r,c,f,n) {reason} ->
             match reason with
             | Complex_function -> r, Set.add c name, f, n
             | Forbidden_function -> r,c,Set.add f name, n
             | Recursive_function -> Set.add r name, c,f,n
             | Non_structural_cfg -> r,c,f, Set.add n name)) in
  print "Forbidden" f;
  print "Recursive" r;
  print "Complexity" c;
  print "Non-structural cfg" n

let detailed_report symtab results =
  let () = printf "%-16s %-7s %s\n" "Symbol found" "Address" "Criteria" in
  Map.iteri results ~f:(fun ~key:addr ~data ->
      report_symbol symtab addr data)

let report = detailed_report

let run checks proj =
  let (++) init results =
    List.fold results ~init ~f:(fun data x ->
        Map.add_multi data x.addr x) in
  List.fold checks ~init:(Map.empty (module Addr))
    ~f:(fun data -> function
        | Symbols check -> data ++ check (Project.symbols proj)
        | Program check -> data ++ check (Project.program proj))

let main checks fail_on_found silent proj =
  match checks with
  | [] -> ()
  | checks ->
     Incident.notify_symbols (Project.symbols proj);
     let locs = run checks proj in
     if not (Map.is_empty locs) then
       let () = if not silent then report (Project.symbols proj) locs in
       if fail_on_found then exit 1

let split s =
  List.map s ~f:(String.split ~on:',') |> List.concat

let () =
  let () = Config.manpage [
      `S "DESCRIPTION";
      `P "Finds symbols that satisfy to some criteria";

      `P "This plugin finds and outputs information about symbols that
         satisfy to one or many criteria listed below. The output format is
         `<symbol name> <symbol address> <list of criteria>`, e.g.
         `foo 0x100500 [Name; Recursive-call; Complexity]`"
    ] in
  let names : string list Config.param =
    let doc = "Find symbol by it's name. You can
               specify this option several times" in
    Config.(param_all string "name" ~docv:"NAME" ~doc) in
  let used =
    let doc = "Outputs only such symbols
               that were actually used in the program. You can
               specify this option several times" in
    Config.(param_all string "usage" ~docv:"USAGE" ~doc) in
  let complexity: int option Config.param =
    let doc = "Find symbols with cyclomatic complexity > $(docv)" in
    Config.(param (some int) "complexity" ~default:None ~docv:"THRESHOLD" ~doc) in
  let recursive =
    let doc = "Find recursive (and mutually recursive) subroutines" in
    Config.(flag "recursive" ~doc) in
  let fail_on_found =
    let doc = "Fails with exit status 1 if any symbols were found" in
    Config.(flag "fail-on-found" ~doc) in
  let non_structured =
    let doc = "Find symbols that are not well-structured" in
    Config.(flag "non-structured" ~doc) in
  let silent =
    let doc = "Don't output the results" in
    Config.(flag "silent" ~doc) in
  Config.when_ready (fun {Config.get=(!)} ->
      let checks = [
        not @@ List.is_empty !names, Symbols (find_names (split !names));
        Option.is_some !complexity, Symbols (find_complex !complexity);
        !recursive, Program find_recursive;
        !non_structured, Symbols find_non_structured;
        not @@ List.is_empty !used, Program (find_usage (split !used));
        ] |> List.filter_map
          ~f:(fun x -> if fst x then Some (snd x) else None) in
      Project.register_pass' (main checks !fail_on_found !silent) ~runonce:true)
