open Core_kernel
open Bap.Std
open Bap_primus.Std

module Subs = Subroutines_info

let check_name = "unused return value"

type verbose = Brief | Detail

type status = Used | Unused

type func = {
  name : string;
  status : status;
  callsite : addr;
}

type tracker = {
  locations : Primus.value Primus.Value.Map.t;
  functions : func Primus.Value.Id.Map.t;
  verbose  : verbose;
}

let tracker = Primus.Machine.State.declare
    ~name:"unused-return-tracker"
    ~uuid:"16DF068F-14F2-44FF-B5E7-B0B5194073BE"
    (fun _ -> {
         locations = Map.empty (module Primus.Value);
         functions = Map.empty (module Primus.Value.Id);
         verbose   = Brief
       })

let notify_unused, unused_return_value =
  Primus.Observation.provide
    ~inspect:Primus.sexp_of_value "notify-unused-return"

let verbose_of_int = function
  | 1 -> Brief
  | _ -> Detail

let pp_bold ppf = Format.fprintf ppf "\027[1m"
let pp_norm ppf = Format.fprintf ppf "\027[0m"
let print_ok   () = Format.printf "%s   OK\n" check_name
let print_fail () = Format.printf "%s   FAIL\n" check_name

let print_header () =
  Format.printf "\n%t%-10s %s\n%t" pp_bold "Function" "Called at" pp_norm

let print_incident name addr =
  Format.printf "%-10s %s\n%!" name (sprintf "%a" Addr.pps addr)

module Notify(S : sig val verbose : verbose end)(Machine : Primus.Machine.S) = struct
  module Value = Primus.Value.Make(Machine)
  open Machine.Syntax

  let has_fails funcs =
    Map.to_sequence funcs |>
    Seq.exists ~f:(fun (_,{status}) -> status = Unused)

  let print_result verbose funcs =
    if has_fails funcs then print_fail ()
    else print_ok ();
    match verbose with
    | Brief -> ()
    | Detail -> print_header ()

  let on_last_stop _ =
    Machine.Global.get tracker >>= fun {verbose; functions;} ->
    print_result verbose functions;
    Map.to_alist functions |>
    Machine.List.fold ~init:(Set.empty (module Addr))
      ~f:(fun reported (_,{name;callsite=addr;status}) ->
          match status with
          | Unused when not (Set.mem reported addr) ->
            if verbose = Detail then
              print_incident name addr;
            Value.of_word addr >>= fun addr' ->
            Machine.Observation.make unused_return_value addr' >>=
            fun () ->
            Machine.return (Set.add reported addr)
          | _ -> Machine.return reported) >>= fun _ ->
    Machine.return ()

  let on_stop () =
    Machine.forks () >>= fun forks ->
    if Seq.length forks = 1 then
      on_last_stop ()
    else Machine.return ()

  let init () =
    Machine.Global.update tracker ~f:(fun s -> { s with verbose = S.verbose }) >>= fun () ->
    Primus.System.fini >>> on_stop

end

module MaybeUnused(Machine : Primus.Machine.S) = struct
  module Value = Primus.Value.Make(Machine)
  open Machine.Syntax

  [@@@warning "-P"]
  let run [value; name; addr] =
    Value.Symbol.of_value name >>= fun name ->
    Machine.Global.update tracker ~f:(fun t ->
        { t with functions =
                   Map.set t.functions
                     (Value.id value)
                     {name;
                      callsite = Value.to_word addr;
                      status = Unused } }) >>= fun () ->
    Value.b1
end

module Status (Machine : Primus.Machine.S) = struct
  module Value = Primus.Value.Make(Machine)
  open Machine.Syntax

  let update value status =
    Machine.Global.update tracker ~f:(fun t ->
        { t with functions =
                   Map.change t.functions (Value.id value)
                     ~f:(function
                         | None -> None
                         | Some x -> Some {x with status}) }) >>=
    fun () -> Value.b1

end

module MarkUsed(Machine : Primus.Machine.S) = struct
  module Status = Status(Machine)
  open Machine.Syntax

  [@@@warning "-P"]
  let run [value] = Status.update value Used
end


module MarkUnused(Machine : Primus.Machine.S) = struct
  module Status = Status(Machine)
  open Machine.Syntax

  [@@@warning "-P"]
  let run [value] = Status.update value Unused
end

module Marked(Machine : Primus.Machine.S) = struct
  module Value = Primus.Value.Make(Machine)
  open Machine.Syntax

  [@@@warning "-P"]
  let run [value] =
    Machine.Global.get tracker >>= fun {functions} ->
    Value.of_bool @@ Map.mem functions (Value.id value)

end

module SaveLocation(Machine : Primus.Machine.S) = struct
  module Value = Primus.Value.Make(Machine)
  open Machine.Syntax

  [@@@warning "-P"]
  let run [addr; loc] =
    Machine.Global.update tracker ~f:(fun t ->
        {t with locations =
                  Map.set t.locations addr loc}) >>= fun () ->
    Value.b1
end

module GetLocation(Machine : Primus.Machine.S) = struct
  module Value = Primus.Value.Make(Machine)
  open Machine.Syntax

  [@@@warning "-P"]
  let run [addr;] =
    Machine.Global.get tracker >>= fun t ->
    match Map.find t.locations addr with
    | None -> Value.b0
    | Some loc -> Machine.return loc
end

module Interface(Machine : Primus.Machine.S) = struct
  module Lisp = Primus.Lisp.Make(Machine)
  open Primus.Lisp.Type.Spec

  let init () =
    Machine.sequence [
      Lisp.define "check-if-used" (module MaybeUnused)
        ~types:(tuple [a;b;c] @-> b)
        ~docs:{|(check-if-used VAL SUB ADDR)
                 marks for checking
                 the return value VAL of SUB called at ADDR |};

      Lisp.define "need-to-check" (module Marked)
        ~types:(tuple [a] @-> bool)
        ~docs:{|(need-to-check V) return true if the value V
                 previously  was marked for checking |};

      Lisp.define "is-known-symbol" (module Subs.Has_known_api)
        ~types:(tuple [a] @-> b)
        ~docs:
          {|(is-known-symbol ADDR) returns true if there is a
           subroutine at ADDR with a known API. |};

      Lisp.define "mark-used" (module MarkUsed)
        ~types:(tuple [a] @-> b)
        ~docs:"(mark-used Value) mark a value of as the used one";

      Lisp.define "mark-unused" (module MarkUnused)
        ~types:(tuple [a] @-> b)
        ~docs:"(mark-unused V) mark the value V as unused one";

      Lisp.define "callsite-addr" (module Subs.Callsite_addr)
        ~types:(tuple [a] @-> b)
        ~docs:
          ({|(callsite-addr SUB) returns the address of the
             callsite of the previous call to the subroutine SUB. |});

      Lisp.define "return-from-sub" (module Subs.Find)
        ~types:(tuple [a] @-> b)
        ~docs:
          {|(return-from-sub V) returns a name of a function
           which return argument is V. Retruns nil if there is no
           such function. |};

      Lisp.define "save-location" (module SaveLocation)
        ~types:(tuple [a; b] @-> c)
        ~docs:
          {|(save-location A L) saves the location L by address A |};

      Lisp.define "get-location" (module GetLocation)
        ~types:(tuple [a;] @-> b)
        ~docs:
          {|(get-location A) returns location L saved by address A,
            or nil if the location not found |};

      Lisp.signal
        ~doc:"Signal is raised when a new unused return value detected"
        notify_unused (fun t -> Machine.return [t]);
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
           >1 - prints locations where unchecked values were introduced"
    Extension.Type.(int =? 1)
    "verbose"


let () =
  let open Extension.Syntax in
  Extension.declare
  @@ fun ctxt ->
  if ctxt --> enabled then
    begin
      Subs.init ();
      Primus.Machine.add_component (module Interface);
      Primus.Machine.add_component (module Notify(struct
            let verbose = verbose_of_int (ctxt --> verbose)
          end));
    end;
  Ok ()
[@@warning "-D"]
