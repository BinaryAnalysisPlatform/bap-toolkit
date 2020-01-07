open Core_kernel
open Bap.Std
open Bap_primus.Std
open Bap_taint.Std

open Monads.Std

include Self ()

let check_name = "unused return value"

type func = string * addr
type proof = Used | Unused
type verbose = Brief | Detail

type state = {
  functions : func Primus.Value.Id.Map.t;
  proved    : proof Addr.Map.t;
  verbose   : verbose;
}

let state = Primus.Machine.State.declare
    ~name:"unused-results"
    ~uuid:"af66d451-fb62-44c3-9c2a-8969e111ad91"
    (fun _ -> {
         functions = Map.empty (module Primus.Value.Id);
         proved    = Map.empty (module Addr);
         verbose   = Brief
       })

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

module Reporter(Machine : Primus.Machine.S) = struct
  open Machine.Syntax

  let has_proved_fail () =
    Machine.Global.get state >>| fun s ->
    Seq.exists (Map.to_sequence s.proved)
      ~f:(fun (_,proof) -> match proof with
          | Unused -> true
          | _ -> false)

  let on_exit () =
    has_proved_fail () >>= fun has_fails ->
    if not has_fails then print_ok ();
    Machine.return ()

  let notify name addr = function
    | Used -> Machine.return ()
    | Unused ->
       has_proved_fail () >>= fun has_fails ->
       if not has_fails then print_fail ();
       Machine.Global.get state >>= fun {verbose} ->
       match verbose with
       | Brief  -> Machine.return ()
       | Detail ->
          if not has_fails then print_header ();
          print_incident name addr;
          Machine.return ()

end

module Init(S : sig val verbose : verbose end)(Machine : Primus.Machine.S) = struct
  module Reporter = Reporter(Machine)
  open Machine.Syntax

  let on_halt init_id () =
    Machine.current () >>= fun id ->
    if Machine.Id.(init_id = id) then
      Reporter.on_exit ()
    else Machine.return ()

  let init () =
    Machine.Global.update state ~f:(fun s -> { s with verbose = S.verbose }) >>= fun () ->
    Machine.current () >>= fun init ->
    Primus.Interpreter.halting >>> on_halt init
end


module Results(Machine : Primus.Machine.S) = struct
  module Value = Primus.Value.Make(Machine)
  module Object = Taint.Object.Make(Machine)
  module Tracker = Taints_tracker.Tracker(Machine)
  module Reporter = Reporter(Machine)
  open Machine.Syntax

  let create taint name addr =
    Tracker.on_new_taint taint >>= fun  () ->
    let addr = Value.to_word addr in
    let vid = Value.id taint in
    Value.Symbol.of_value name >>= fun name ->
    Machine.current () >>= fun cur ->
    Machine.Global.update state ~f:(fun s ->
        { s with
          functions = Map.set s.functions vid (name,addr)}) >>= fun () ->
    Value.b1

  let mark taint usage =
    let vid = Value.id taint in
    Machine.Global.get state >>= fun s ->
    match Map.find s.functions vid with
    | None -> Value.b0
    | Some (name,addr) ->
      Reporter.notify name addr usage >>= fun () ->
      let s = {s with proved = Map.set s.proved addr usage} in
      Machine.Global.put state s >>= fun () ->
      Value.b1

  let mark_used taint = mark taint Used
  let mark_unused taint = mark taint Unused

  let is_known taint =
    let vid = Value.id taint in
    Machine.Global.get state >>= fun s ->
    match Map.find s.functions vid with
    | None -> Value.b0
    | Some (name,addr) ->
      Value.of_bool (Map.mem s.proved addr)

end

module IsKnown(Machine : Primus.Machine.S) = struct
  module Results = Results(Machine)

  [@@@warning "-P"]
  let run [v] = Results.is_known v
end

module MaybeUnused(Machine : Primus.Machine.S) = struct
  module Results = Results(Machine)

  [@@@warning "-P"]
  let run [taint; name; addr] = Results.create taint name addr
end

module MarkUnused(Machine : Primus.Machine.S) = struct
  module Results = Results(Machine)

  [@@@warning "-P"]
  let run [v] = Results.mark_unused v
end

module MarkUsed(Machine : Primus.Machine.S) = struct
  module Results = Results(Machine)

  [@@@warning "-P"]
  let run [v] = Results.mark_used v
end

type callsite = {
  calls : addr String.Map.t;
  undef : addr option;
}

let callsite = Primus.Machine.State.declare
    ~name:"unused-results-callsite"
    ~uuid:"1715922a-4e6d-4960-bbd4-a6fb8e239ddb"
    (fun _ -> { calls = Map.empty (module String); undef = None})


module Callsite (Machine : Primus.Machine.S) = struct
  module Value  = Primus.Value.Make(Machine)
  module Linker = Primus.Linker.Make(Machine)
  module Interp = Primus.Interpreter.Make(Machine)

  open Machine.Syntax

  let on_call (name,_) =
    Machine.Local.update callsite ~f:(fun s ->
        match s.undef with
        | None -> s
        | Some pc ->
          {calls = Map.set s.calls name pc; undef = None})

  let on_jump j =
    match Jmp.kind j with
    | Goto _ | Int _ | Ret _ -> Machine.return ()
    | Call c ->
      Interp.pc >>= fun pc ->
      match Call.target c with
      | Indirect _ ->
        Machine.Local.update callsite
          ~f:(fun s -> {s with undef = Some pc})
      | Direct tid ->
        Linker.resolve_symbol (`tid tid) >>= function
        | None -> Machine.return ()
        | Some name ->
          Machine.Local.update callsite
            ~f:(fun s ->
                {calls = Map.set s.calls name pc; undef = None})

  let init() =
    Machine.sequence [
      Primus.Interpreter.enter_jmp >>> on_jump;
      Primus.Linker.Trace.call >>> on_call;
    ]

end

module Callsite_addr(Machine : Primus.Machine.S) = struct
  module Value = Primus.Value.Make(Machine)
  module Eval = Primus.Interpreter.Make(Machine)
  open Machine.Syntax

  [@@@warning "-P"]
  let run [sub] =
    Value.Symbol.of_value sub >>= fun name ->
    Machine.Local.get callsite >>= fun s ->
    match Map.find s.calls name with
    | Some addr -> Value.of_word addr
    | None -> Value.b0
end

let sub_args =
  Primus.Machine.State.declare
    ~name:"subroutines-arguments"
    ~uuid:"06f656ad-e922-4fa5-bc65-a3203b6819a2"
    (fun _ -> Map.empty (module String))

module Known_subs(Machine : Primus.Machine.S) = struct
  module Value = Primus.Value.Make(Machine)
  open Machine.Syntax

  let init () =
    Machine.get () >>= fun proj ->
    let subs = Term.to_sequence sub_t (Project.program proj) in
    let m = Seq.fold subs
        ~init:(Map.empty (module String))
        ~f:(fun m sub ->
            match Term.to_sequence arg_t sub |> Seq.to_list with
            | [] -> m
            | args ->
              Map.set m (Sub.name sub) args) in
    Machine.Global.put sub_args m

end

module Return_arg(Machine : Primus.Machine.S) = struct
  module Value = Primus.Value.Make(Machine)
  module Env = Primus.Env.Make(Machine)
  open Machine.Syntax

  let find sub =
    Value.Symbol.of_value sub >>= fun name ->
    Machine.Global.get sub_args >>= fun subs ->
    match Map.find subs name with
    | None -> Machine.return None
    | Some args ->
      Machine.return @@
      List.find args ~f:(fun a -> Arg.intent a = Some Out)

  [@@@warning "-P"]
  let run [sub] =
    find sub >>= function
    | None -> Value.b0
    | Some a -> Value.Symbol.to_value (Var.name (Arg.lhs a))
end

module Is_sym(Machine : Primus.Machine.S) = struct
  module Value = Primus.Value.Make(Machine)
  module Linker = Primus.Linker.Make(Machine)
  open Machine.Syntax

  [@@@warning "-P"]
  let run [addr] =
    let addr = `addr (Value.to_word addr) in
    Linker.resolve_symbol addr >>= function
    | None -> Value.b0
    | Some name ->
      Machine.Global.get sub_args >>= fun subs ->
      Value.of_bool (Map.mem subs name)

end

let return_args = Primus.Machine.State.declare
    ~name:"return-args"
    ~uuid:"5cf9c862-7f97-4e25-bd41-8f7c76d42855"
    (fun _ -> Map.empty (module String))

let return_vals = Primus.Machine.State.declare
    ~name:"return-values"
    ~uuid:"3ab8d8ee-1304-46d3-9860-0960aaf2ac42"
    (fun _ -> Map.empty (module Primus.Value.Id))

module Return_args(Machine : Primus.Machine.S) = struct
  open Machine.Syntax

  let init () =
    Machine.get () >>= fun proj ->
    Machine.Global.update return_args (fun rets ->
    Seq.fold (Project.program proj |> Term.enum sub_t)
      ~init:rets ~f:(fun rets sub ->
        match Seq.find (Term.enum arg_t sub) ~f:(fun a ->
                  match Arg.intent a with
                  | Some Out -> true
                  | _ -> false) with
        | None -> rets
        | Some a -> Map.set rets (Sub.name sub) (Arg.lhs a)))
end

module Return_vals(Machine : Primus.Machine.S) = struct
  module Env = Primus.Env.Make(Machine)
  module Value = Primus.Value.Make(Machine)

  open Machine.Syntax

  let on_call_return (name, args) =
    Machine.Global.get return_args >>= fun s ->
    match Map.find s name with
    | None -> Machine.return ()
    | Some _ ->
       match List.last args with
       | None -> Machine.return ()
       | Some value ->
         Machine.Local.update return_vals ~f:(fun vals ->
             Map.set vals (Primus.Value.id value) name)

  let init() =
    Primus.Linker.Trace.return >>> on_call_return

end

module Return_from(Machine : Primus.Machine.S) = struct
  module Value = Primus.Value.Make(Machine)
  open Machine.Syntax

  [@@@warning "-P"]
  let run [v] =
    Machine.Local.get return_vals >>= fun rets ->
    match Map.find rets (Primus.Value.id v) with
    | None -> Value.b0
    | Some name -> Value.Symbol.to_value name

end

module Interface(Machine : Primus.Machine.S) = struct
  module Lisp = Primus.Lisp.Make(Machine)
  open Primus.Lisp.Type.Spec

  let init () =
    Machine.sequence [

      Lisp.define "check-if-used" (module MaybeUnused)
        ~types:(tuple [a;b;c] @-> b)
        ~docs:{|(check-if-used T SUB ADDR) marks
                 the return argument of SUB called at ADDR and
                 tainted with T for checking|};

      Lisp.define "is-known-usage" (module IsKnown)
        ~types:(tuple [a] @-> b)
        ~docs:"(is-known-usage T) returns true if a taint T
                from a return value of a function was
                marked either as used or unused one";

      Lisp.define "mark-unused" (module MarkUnused)
        ~types:(tuple [a] @-> b)
        ~docs:"(mark-unused T) mark a return value of
               a function tainted by T as the unused one";

      Lisp.define "mark-used" (module MarkUsed)
        ~types:(tuple [a] @-> b)
        ~docs:"(mark-used T) mark a return value of
               a function tainted by T as the used one";

      (* Lisp.define "return-arg" (module Return_arg)
       *   ~types:(tuple [a] @-> b)
       *   ~docs:
       *     ({|(return-arg SUB) returns the name of output argument of the
       *       subroutine SUB. Returns NIL if the subroutine SUB doesn't
       *       return anything or subroutine's api is unknown|}); *)

      Lisp.define "callsite-addr" (module Callsite_addr)
        ~types:(tuple [a] @-> b)
        ~docs:
          ({|(callsite-addr SUB) returns the address of the
             callsite of the previous call to the subroutine SUB. |});

      Lisp.define "is-known-symbol" (module Is_sym)
        ~types:(tuple [a] @-> b)
        ~docs:
          {|(is-known-symbol ADDR) returns true if there is a
           subroutine at ADDR with a known API. |};

      Lisp.define "return-from-sub" (module Return_from)
        ~types:(tuple [a] @-> b)
        ~docs:
          {|(return-from-sub V) returns a name of a function
           which return argument is V. Retruns nil if there is no
           such function. |}

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
         Taints_tracker.init ();
         Primus.Machine.add_component (module Return_args);
         Primus.Machine.add_component (module Return_vals);
         Primus.Machine.add_component (module Interface);
         Primus.Machine.add_component (module Callsite);
         Primus.Machine.add_component (module Known_subs);
         Primus.Machine.add_component
           (module Init(struct
                       let verbose = verbose_of_int (ctxt --> verbose)
                     end));
    end;
  Ok ()
