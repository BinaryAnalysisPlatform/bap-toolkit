open Core_kernel
open Bap.Std
open Bap_main
open Monads.Std

include Self()

module Utils = Find_symbol_utils
module Report = Find_symbol_report

open Find_symbol_types

module Scheme = struct
  open Ogre.Type
  open Image.Scheme

  let rel_addr = "relative-addr" %: int

  let symbol_entry () =
    Ogre.declare ~name:"symbol-entry"
      (scheme name $ rel_addr $ size $ off)
      (fun name addr size off -> name, addr, size, off)
end

module Symbols = struct
  include Ogre.Make(Monad.Ident)
  open Image.Scheme
  open Scheme

  let symbols =
    require base_address >>= fun base ->
    collect Ogre.Query.(select (from symbol_entry)) >>= fun s ->
    Seq.fold s ~init:(Map.empty (module String))
      ~f:(fun syms (name, relative_addr , _, _) ->
          let addr = Int64.(relative_addr + base) in
          return @@
          Map.set syms name
            Symbol.{name;addr=Addr.of_int64 addr})

  let find doc = eval symbols doc
end

let empty_results = Map.empty (module Symbol)

let add syms sym ?data check =
  Map.add_multi syms sym {data; check}

let find_forbidden file requested =
  let symbols_of_file path =
    Or_error.(
      Image.create ~backend:"llvm" path >>= fun (im,_) ->
      Symbols.find (Image.spec im)) |> function
    | Ok xs -> xs
    | Error er ->
      error "Can't find symbols: %s\n" (Error.to_string_hum er);
      Map.empty (module String) in
  let symbols = symbols_of_file file in
  List.fold requested ~init:empty_results ~f:(fun forb sym ->
      match Map.find symbols sym with
      | None -> forb
      | Some sym -> add forb sym Forbidden_function)

let find_recursive proj =
  let syms = Find_symbol_rec.find (Project.program proj) in
  List.fold syms ~init:empty_results ~f:(fun syms sym ->
      add syms sym Recursive_function)

let find_complex threshold proj = match threshold with
  | None -> empty_results
  | Some threshold ->
    let symtab = Project.symbols proj in
    Symtab.to_sequence symtab |>
    Seq.fold ~init:empty_results ~f:(fun syms (name,entry,cfg) ->
        let c = Utils.complexity cfg entry in
        if c > threshold then
          add syms {name;addr=Block.addr entry}
            ~data:(Complexity c) Complex_function
        else syms)

let find_non_structured proj =
  Project.symbols proj |>
  Symtab.to_sequence |>
  Seq.fold ~init:empty_results ~f:(fun syms (name,entry,cfg) ->
      match Utils.find_nonstructural_component cfg entry with
      | None -> syms
      | Some addr ->
        add syms {name;addr=Block.addr entry}
          ~data:(Non_structural_block addr) Non_structural_cfg)

let with_project recursive non_structural complex level proj =
  let merge a b =
    Map.merge a b ~f:(fun ~key:_ -> function
        | `Left x | `Right x -> Some x
        | `Both (x,y) -> Some (x @ y)) in
  let (++) a f = merge a (f proj) in
  let checks = [
    recursive, Recursive_function, find_recursive;
    Option.is_some complex, Complex_function, find_complex complex;
    non_structural, Non_structural_cfg, find_non_structured;
  ] in
  let checks =
    List.filter_map checks ~f:(fun (test,f,check) ->
        Option.some_if test (f, check)) in
  let requested = List.map checks ~f:fst  in
  let checks  = List.map checks ~f:snd in
  let results = List.fold checks ~init:empty_results ~f:(++) in
  Report.dump ~requested ~level results;
  Report.dump_incidents results

let verbosity_level xs =
  let level = Option.value ~default:1 @@
    List.max_elt xs ~compare:Int.compare in
  Report.level_of_int level

let input = Extension.Command.argument
    ~doc:"The input file" Extension.Type.("FILE" %: string)

let names =
  Extension.Command.parameters
    ~doc:"Look for symbols from the provided list"
    Extension.Type.("NAMES" %: list string) "names"

let verbose =
  Extension.Command.parameters
    ~as_flag:1
    ~doc:"Dumps the results"
    Extension.Type.("verbose" %: int)
    "verbose"

let recursive =
  Extension.Configuration.flag
    ~doc:"Find recursive (and mutually recursive) subroutines"
    "recursive"

let non_structured =
  Extension.Configuration.flag
    ~doc:"Find symbols that are not well-structured"
    "non-structured"

let complexity =
  Extension.Configuration.parameter
    ~doc:"Find symbols with cyclomatic complexity > threshold"
    Extension.Type.("COMPLEXITY" %: some int) "complexity"

let verbose' =
  Extension.Configuration.parameters
    ~as_flag:1
    ~doc:"Dumps the results"
    Extension.Type.("dump" %: int)
    "verbose"

let () =
  Extension.Command.(begin
      declare "find-symbol" ~requires:[] (args $input $names $verbose)
    end) @@ fun input requested verbose ctxt ->
  let level = verbosity_level verbose in
  let results = find_forbidden input @@ List.concat requested in
  Report.dump ~level ~print_addrs:false
    ~requested:[Forbidden_function] results;
  Report.dump_incidents results;
  Ok ()

let () =
  let open Extension.Syntax in
  Extension.declare
  @@ fun ctxt ->
  let level = verbosity_level (ctxt --> verbose') in
  let pass = with_project
      (ctxt --> recursive)
      (ctxt --> non_structured)
      (ctxt --> complexity)
      level in
  Ok (Project.register_pass' ~runonce:true pass)
