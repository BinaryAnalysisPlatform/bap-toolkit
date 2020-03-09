open Core_kernel
open Bap.Std
open Bap_primus.Std

include Self ()

let check_name = "null pointer dereference"

type verbose =
  | Brief
  | Detail

let verbose_of_int = function
  | 1 -> Brief
  | _ -> Detail

type state = {
  derefs  : Addr.Set.t;
  verbose : verbose;
}

let state = Primus.Machine.State.declare
    ~uuid:"3e065801-3af7-41f0-af1c-e20b9407da11"
    ~name:"null-ptr-dereference"
    (fun _ -> {
         derefs = Set.empty (module Addr);
         verbose = Brief;})

let atos = sprintf "%a" Addr.pps

let pp_bold ppf = Format.fprintf ppf "\027[1m"
let pp_norm ppf = Format.fprintf ppf "\027[0m"

module Reporter(Machine : Primus.Machine.S) = struct
  module Value = Primus.Value.Make(Machine)
  open Machine.Syntax

  let print_ok   () = Format.printf "%s   OK\n" check_name
  let print_fail () = Format.printf "%s   FAIL\n" check_name

  let on_exit () =
    Machine.Global.get state >>| fun s ->
    if Set.is_empty s.derefs then print_ok ()

  let print_header = function
    | Detail ->
      Format.printf "\n%t%-10s %s\n%t" pp_bold "Address" "Introduced" pp_norm
    | _ -> ()

  let report_deref intro addr = function
    | Detail ->
      Format.printf "%-10s %s\n%!" (atos addr) (atos intro)
    | _ -> ()

  let update intro addr =
    let intro = Value.to_word intro in
    let addr = Value.to_word addr in
    Machine.Global.update state ~f:(fun s ->
        if Set.mem s.derefs addr then s
        else
          let () =
            if Set.is_empty s.derefs then
              let () = print_fail () in
              print_header s.verbose in
          let () = report_deref addr intro s.verbose in
          {s with derefs = Set.add s.derefs addr}) >>= fun () ->
    Value.b0
end

module Notify(Machine : Primus.Machine.S) = struct
  module Reporter = Reporter(Machine)

  [@@@warning "-P"]
  let run [intro; addr] = Reporter.update intro addr
end


module Init_reports(S : sig val verbose : verbose end)(Machine : Primus.Machine.S) = struct
  module Reporter = Reporter(Machine)
  open Machine.Syntax

  let on_stop _ = Reporter.on_exit ()

  let init () =
    Machine.Global.update state
      ~f:(fun s -> {s with verbose = S.verbose}) >>= fun () ->
    Primus.System.stop >>> on_stop

end

module IsReported(Machine : Primus.Machine.S) = struct
  module Value = Primus.Value.Make(Machine)
  open Machine.Syntax

  [@@@warning "-P"]
  let run [a] =
    Machine.Global.get state >>= fun {derefs} ->
    Value.of_bool (Set.mem derefs (Value.to_word a))
end

let flags =
  Primus.Machine.State.declare
    ~uuid:"9b2c0dff-5adf-44e5-8257-d7cfb956165c"
    ~name:"cpu-flags"
    (fun _ -> Set.empty (module String))

module Flags(Machine : Primus.Machine.S) = struct
  open Machine.Syntax

  let init () =
    Machine.arch >>= fun a ->
    let module T = (val target_of_arch a) in
    let known =
      T.CPU.([sp; zf; cf; vf; nf]) |>
      List.map ~f:Var.name |>
      Set.of_list (module String) in
    Machine.Global.put flags known
end

module IsFlag(Machine : Primus.Machine.S) = struct
  module Value = Primus.Value.Make(Machine)
  open Machine.Syntax

  [@@@warning "-P"]
  let run [x] =
    Value.Symbol.of_value x >>= fun name ->
    Machine.Global.get flags >>= fun s ->
    Value.of_bool (Set.mem s name)
end

module Lisp(Machine : Primus.Machine.S) = struct
  module Lisp = Primus.Lisp.Make(Machine)
  open Primus.Lisp.Type.Spec

  let def name types docs closure = Lisp.define ~docs ~types name closure

  let init () =
    Machine.sequence Primus.Interpreter.[

        def "is-reported-deref" (tuple [a] @-> bool)
          "(is-reported-deref ADDR) returns true if
           a null pointer dereference at ADDR is the known
           incident and was reported before"
          (module IsReported);

        def "is-cpu-flag" (tuple [a] @-> bool)
          "(is-cpu-flag X) returns true if X is a flag variable"
          (module IsFlag);

        def "is-untrusted-return-value" (tuple [a] @-> bool)
          "(is-untrusted-return-value X) returns true if
           a value X is read from abi-specific register
           right after unresolved call returned OR
           is a return value of a function that wasn't actually
           called in the current machine because of the forced
           execution scheme in Primus"
          (module Untrusted_return.IsUntrustedReturn);

        def "notify-null-ptr-dereference" (tuple [a;b] @-> any)
          "(notify-null-ptr-dereference INTRO ADDR)
           print message to stdout when pointer that was
           introduced at address INTRO is dereferenced at
           address ADDR"
          (module Notify);
      ]
end

open Bap_main

let enabled =
  Extension.Configuration.flag
    ~doc:"Enables the analysis"
    "enable"

let verbose =
  Extension.Configuration.parameter
    ~doc:"Level of verbosity. Currently supported
           1 - prints a result message, if the check passed or not;
          >1 - prints locations of null pointer dereferences"
    Extension.Type.(int =? 1)
    "verbose"

let () =
  let open Extension.Syntax in
  Extension.declare
  @@ fun ctxt ->
  if ctxt --> enabled then
    begin
      Primus.Machine.add_component (module Lisp);
      Primus.Machine.add_component (module Flags);
      Untrusted_return.init ();
      Primus.Machine.add_component
        (module Init_reports(struct
             let verbose = verbose_of_int (ctxt --> verbose)
           end));
    end;
  Ok ()
