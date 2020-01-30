open Core_kernel
open Bap.Std

type location_id = string
type incident_name = string [@@deriving bin_io, compare,sexp]

type event =
  | Incident_location of location_id * addr
  | Incident of incident_name * location_id list
  | Incident_static of incident_name * string

type info =
  | Locations of addr list
  | Static_data of string
[@@deriving bin_io, compare,sexp]

type incident = {
  name : string;
  info : info;
}
[@@deriving bin_io, compare,sexp]

module Incident = struct
  module Set = Set.Make(struct
      type t = incident [@@deriving bin_io, compare,sexp]
    end)
end


type t = {
  hist  : addr String.Map.t;
  incs  : Incident.Set.t;
}


module Parse = struct

  let addr_of_string a =
    Addr.of_string @@ sprintf "0x%s:64u" a

  let point_of_sexp x = match x with
    | Sexp.List _ -> None
    | Sexp.Atom x ->
      match String.split ~on:':' x  with
      | [_; addr] -> Some (addr_of_string addr)
      | [addr] -> Some (addr_of_string addr)
      | _ -> None

  let last_addr xs =
    Option.value_map ~default:None ~f:point_of_sexp (List.hd xs)

  let locs_of_sexps xs =
    List.filter_map xs ~f:(function
        | Sexp.Atom s -> Some s
        | _ -> None)

  let of_sexp s =
    let open Sexp in
    match s with
    | List (Atom "incident-location" :: List [Atom id; List points] :: _)  ->
      Option.map (last_addr points) ~f:(fun p -> Incident_location (id, p))
    | List (Atom "incident" :: List (Atom name :: locs) :: _) ->
      Incident (name, locs_of_sexps locs) |> Option.some
    | List (Atom "incident-static" :: List (Atom name :: data) :: _)  ->
      Incident_static (name, Sexp.to_string (List data)) |> Option.some
    | _ -> None

  let of_sexp s =
    try of_sexp s
    with e -> None

  let sexp ch =
    try Some (Sexp.input_sexp ch)
    with _ -> None

  let rec of_in_channel ch =
    let rec read () =
      match sexp ch with
      | None -> None
      | Some s ->
        match of_sexp s with
        | None -> read ()
        | Some _ as r -> r in
    read ()
end

module Log = struct

  let logfile = "toolkit.log"

  let file_not_found check_name file =
    let reason = sprintf "%s failed: %s not found" check_name file in
    Out_channel.with_file logfile ~append:true ~f:(fun ch ->
        Out_channel.output_lines ch [reason])

  let check_failed check_name real ours =
    let tp = Set.inter real ours in
    let fp = Set.diff ours real in
    let fn = Set.diff real ours in
    let fabula =
      sprintf "%s failed: True-pos/False-pos/False-neg: %d/%d/%d"
        check_name (Set.length tp) (Set.length fp) (Set.length fn) in
    Out_channel.with_file logfile ~append:true ~f:(fun ch ->
        Out_channel.output_lines ch
          [fabula; "  The following incidents are missed"];
        let missed =
          Set.fold ~init:[] fn ~f:(fun acc inc ->
              let inc = sprintf "  %s"
                  (Sexp.to_string (sexp_of_incident inc)) in
              inc :: acc) in
        Out_channel.output_lines ch missed)

end



let incident_location s (id,addr) =
  {s with hist = Map.set s.hist ~key:id ~data:addr}

let incident s name locs =
  let locs = List.filter_map locs ~f:(Map.find s.hist) in
  {s with incs = Set.add s.incs {name; info=Locations locs}}

let incident_static s name data =
  {s with incs = Set.add s.incs {name; info=Static_data data}}

let event s = function
  | Incident_location (id,addr) -> incident_location s (id,addr)
  | Incident (name, locs) -> incident s name locs
  | Incident_static (name, data) -> incident_static s name data

let read ch =
  let rec loop s =
    match Parse.of_in_channel ch with
    | None -> s
    | Some ev -> event s ev |> loop in
  let s = loop { hist = Map.empty (module String);
                 incs=Incident.Set.empty; } in
  s.incs

let print_status name status =
  let margin = 60 in
  let dots = String.init (margin - String.length name) ~f:(fun _ -> '.') in
  printf "%s %s %s\n" name dots status

let compare_incidents exact real ours =
  if exact then Set.equal real ours
  else Set.is_subset real ~of_:ours

let file_exists check_name f =
  let r = Sys.file_exists f in
  if not r then Log.file_not_found check_name f;
  r

let main name real_incs ours_incs exact expected_fail =
  let of_ch c = In_channel.with_file c ~f:read in
  if not (file_exists name real_incs) || not (file_exists name ours_incs) then
    print_status name "FAIL"
  else
    let real = of_ch real_incs in
    let ours = of_ch ours_incs in
    let passed = compare_incidents exact real ours in
    let status =
      if passed then "PASS"
      else if expected_fail then "XFAIL"
      else "FAIL" in
    print_status name status;
    if not passed then
      Log.check_failed name real ours;
    if not passed && not expected_fail then
      exit 1

open Cmdliner

let doc = "Compares incidents in two files.

invocation: ./compare-incidents check-name real incidents exact expected

If the exact flag is set then incidents in two files must be
be the same, i.e. all the incidents from one file are present
in another and vice versa.

Otherwise, all the real incidents must be a subset of the
incidents we are checking, i.e. false positives are permitted,
but no regression is allowed.

Incidents are compared by the addresses of their locations and
by theirs name. Thus, if an incident has three locations, then
a corresponded incident considered as an equal one if it has
the same number of locations with the same addresses as the first one.

The status of the comparison is one of the PASS | FAIL | XFAIL.
The latter one is assigned if incidents don't match each other and
'expected' flag is set."

let test_name : string Term.t =
  let doc = "Name of the test to do" in
  Arg.(required & pos 0 (some string) None &
       info [] ~doc ~docv:"NAME")

let real : string Term.t =
  let doc = "File with real(proven) incidents" in
  Arg.(required & pos 1 (some string) None &
       info [] ~doc ~docv:"REAL")

let ours : string Term.t =
  let doc = "File with new incidents" in
  Arg.(required & pos 2 (some string) None &
       info [] ~doc ~docv:"FILE")

let exact : bool Term.t =
  let doc =
    "Compares incidents exactly, i.e. no False positives permitted" in
  Arg.(value & flag & info ["exact"] ~doc)

let expected_fail : bool Term.t =
  let doc =
    "Indicates, that it's not a bug if a test is failed, it was expected" in
  Arg.(value & flag & info ["expect-fail"] ~doc)


let prg =
  Term.(const main $test_name $real $ours $exact $expected_fail),
  Term.info "compare-incidents" ~doc

let _ = Term.eval prg
