
open Core_kernel
open Bap.Std
open Bap_primus.Std
open Bap_main

let check_name = "must check value"

type t = {
  addrs : Addr.Set.t;
  verbose : bool;
}

let state =
  Primus.Machine.State.declare
    ~name:"unchecked-return-value"
    ~uuid:"7390b60d-fac6-42f7-b13b-94b85bba7586"
    (fun _ -> {addrs = Set.empty (module Addr); verbose=false})

let atos a = sprintf "%a" Addr.pps a
let print_ok () = Format.printf "%s   OK\n" check_name
let print_fail () = Format.printf "%s   FAIL\n" check_name

let print_header () =
  let pp_bold ppf = Format.fprintf ppf "\027[1m" in
  let pp_norm ppf = Format.fprintf ppf "\027[0m" in
  Format.printf "\n%tUnchecked value at address\n%t" pp_bold pp_norm

let print_addr a = Format.printf "%s\n" @@ atos a

module Init(S : sig val verbose : bool end)(Machine : Primus.Machine.S) = struct
  open Machine.Syntax

  let output _ =
    Machine.Global.get state >>= fun {addrs} ->
    if Set.is_empty addrs then
      print_ok ();
    Machine.return ()


  let init () =
    Machine.Global.update state
      ~f:(fun s -> {s with verbose = S.verbose}) >>= fun () ->
    Primus.System.stop >>> output
end


module Notify(Machine : Primus.Machine.S) = struct
  module Value = Primus.Value.Make(Machine)
  open Machine.Syntax

  [@@@warning "-P"]
  let run [a] =
    let a = Value.to_word a in
    Machine.Global.get state >>= fun s ->
    if Set.is_empty s.addrs
    then print_fail ();
    if Set.is_empty s.addrs && s.verbose
    then print_header ();
    if not (Set.mem s.addrs a) && s.verbose
    then print_addr a;
    Machine.Global.put state
      { s with addrs = Set.add s.addrs a} >>= fun () ->
    Value.b0
end


module Interface(Machine : Primus.Machine.S) = struct
  module Lisp = Primus.Lisp.Make(Machine)
  open Primus.Lisp.Type.Spec

  let init () = Machine.sequence [
      Lisp.define "notify-unchecked-value" (module Notify)
        ~types:(tuple [a] @-> b)
        ~docs:
          {|(notify-unchecked-value addr) prints message that value introduced
         at [addr] was never used. |};
    ]
end

let enabled = Extension.Configuration.flag "enable" ~doc:"Enables the analysis"

let verbose =
  Extension.Configuration.parameter
    ~as_flag:1
    ~doc:"Dumps the results"
    Extension.Type.("verbose" %: int)
    "verbose"

let () =
  let open Extension.Syntax in
  Extension.declare
  @@ fun ctxt ->
  if ctxt --> enabled then
    begin
      Primus.Machine.add_component (module Interface);
      Primus.Machine.add_component
        (module Init(struct let verbose = (ctxt --> verbose) > 1 end));
    end;
  Ok ()
[@@warning "-D"]
