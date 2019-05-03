open Core_kernel
open Bap.Std
open Graphlib.Std
open Format

module Cfg = Graphs.Cfg
module Callg = Graphs.Callgraph

open Find_symbol_utils

include Self ()

type reason =
  | Recursive_call
  | By_symbol_name
  | Non_structured
  | Big_complexity

type result = {
  addr : addr;
  reason : reason;
}

type check =
  | Symbols of (symtab -> result list)
  | Program of (program term -> result list)

let find_non_structured symtab =
  Symtab.to_sequence symtab |>
  Seq.fold ~init:[] ~f:(fun rs (name,entry,cfg) ->
      if not (is_reducible cfg entry) then
        {addr = Block.addr entry; reason = Non_structured} :: rs
      else rs)

let find_names names symtab =
  Symtab.to_sequence symtab |>
  Seq.fold ~init:[] ~f:(fun rs (name,entry,_) ->
      if List.mem ~equal:String.equal names name then
        {addr = Block.addr entry; reason = By_symbol_name} :: rs
      else rs)

let find_recursive prog =
  List.filter_map (find_recursive prog)
    ~f:(fun sub ->
      match Term.get_attr sub address with
      | None -> None
      | Some addr ->
         Some {addr; reason = Recursive_call})

let find_complex threshold symtab = match threshold with
  | None -> []
  | Some threshold ->
     Symtab.to_sequence symtab |>
       Seq.fold ~init:[] ~f:(fun rs (name,entry,cfg) ->
           if complexity cfg entry > threshold then
             {addr = Block.addr entry; reason = Big_complexity} :: rs
           else rs)

let report_symbol symtab addr reasons =
  let string_of_reason = function
    | Big_complexity -> sprintf "Complexity"
    | By_symbol_name -> sprintf "Name"
    | Recursive_call -> sprintf "Recursive-call"
    | Non_structured -> sprintf "Non-structured" in
  match Symtab.find_by_start symtab addr with
  | None -> ()
  | Some (name,_,_) ->
    printf "%-15s %a [" name Addr.pp addr;
    match reasons with
    | [reason] -> printf "%s]\n" @@ string_of_reason reason
    | reasons ->
      List.map reasons ~f:string_of_reason |>
      String.concat ~sep:"; " |>
      printf "%s]\n"

let report symtab results =
  let () = printf "Symbols found:\n" in
  Map.iteri results ~f:(fun ~key:addr ~data ->
      report_symbol symtab addr data)

let run checks proj =
  let (++) init results =
    List.fold results ~init ~f:(fun data x ->
        Map.add_multi data x.addr x.reason) in
  List.fold checks ~init:(Map.empty (module Addr))
    ~f:(fun data -> function
        | Symbols check -> data ++ check (Project.symbols proj)
        | Program check -> data ++ check (Project.program proj))

let main checks fail_on_found silent proj =
  match checks with
  | [] -> ()
  | checks ->
    let locs = run checks proj in
    if not (Map.is_empty locs) then
      let () = if not silent then report (Project.symbols proj) locs in
      if fail_on_found then exit 1

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
        not @@ List.is_empty !names, Symbols (find_names !names);
        Option.is_some !complexity, Symbols (find_complex !complexity);
        !recursive, Program find_recursive;
        !non_structured, Symbols find_non_structured;
        ] |> List.filter_map
          ~f:(fun x -> if fst x then Some (snd x) else None) in
      Project.register_pass' (main checks !fail_on_found !silent) ~runonce:true)
