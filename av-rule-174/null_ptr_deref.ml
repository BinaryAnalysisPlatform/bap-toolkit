open Core_kernel
open Bap.Std
open Bap_primus.Std

include Self ()

type derefs = { addrs : Addr.Set.t; }

let derefs = Primus.Machine.State.declare
    ~uuid:"3e065801-3af7-41f0-af1c-e20b9407da11"
    ~name:"null-ptr-dereference"
    (fun _ -> {addrs = Set.empty (module Addr)})

module IsReported(Machine : Primus.Machine.S) = struct
  module Value = Primus.Value.Make(Machine)
  open Machine.Syntax

  [@@@warning "-P"]
  let run [a] =
    Machine.Global.get derefs >>= fun {addrs} ->
    Value.of_bool (Set.mem addrs (Value.to_word a))
end

module ReportDereference(Machine : Primus.Machine.S) = struct
  module Value = Primus.Value.Make(Machine)
  open Machine.Syntax

  [@@@warning "-P"]
  let run [a] =
    Machine.Global.update derefs ~f:(fun {addrs} ->
        {addrs = Set.add addrs (Value.to_word a)})
end


module Notify(Machine : Primus.Machine.S) = struct
  open Machine.Syntax
  module Value = Primus.Value.Make(Machine)
  module Eval = Primus.Interpreter.Make(Machine)

  [@@@warning "-P"]
  let run [intro; addr] =
    let intro =  Value.to_word intro in
    let addr = Value.to_word addr in
    Machine.Global.get derefs >>= fun s ->
    if Set.mem s.addrs addr then Value.b1
    else
      Machine.Global.put derefs ({addrs = Set.add s.addrs addr}) >>=  fun () ->
      let () =
        printf
          "Got null pointer dereference at %a from the pointer introduced at %a\n%!"
          Addr.ppo addr Addr.ppo intro  in
      Value.b1

end

type checked = {
  checked : Primus.value list;
}

let checked = Primus.Machine.State.declare
    ~uuid:"441b2d94-c3dc-4c46-9d9d-60033f628860"
    ~name:"checked-taints"
    (fun _ -> {checked = []})

module MarkChecked(Machine : Primus.Machine.S) = struct
  module Value = Primus.Value.Make(Machine)
  open Machine.Syntax

  [@@@warning "-P"]
  let run [taint] =
    Machine.Local.update checked ~f:(fun s ->
        { checked = taint :: s.checked  }) >>= fun () ->
    Value.b1
end

module IsChecked(Machine : Primus.Machine.S) = struct
  open Bap_taint.Std
  module Value = Primus.Value.Make(Machine)
  module Object = Taint.Object.Make(Machine)
  module Tracker = Taint.Tracker.Make(Machine)
  open Machine.Syntax

  [@@@warning "-P"]
  let run [value] =
    Tracker.lookup value Taint.Rel.direct >>= fun dir ->
    Tracker.lookup value Taint.Rel.indirect >>= fun ind ->
    Machine.Local.get checked >>= fun {checked} ->
    Machine.List.exists checked ~f:(fun t ->
        Object.of_value t >>= fun t ->
        Machine.return (Set.mem dir t || Set.mem ind t)) >>=
    Value.of_bool
end

type initial = Primus.Value.Id.Set.t

let initial_values = Primus.Machine.State.declare
    ~uuid:"c52fc896-32fc-4132-ba6b-8559c8b25308"
    ~name:"initial-values"
    (fun _ -> Set.empty (module Primus.Value.Id))

module InitialValues (Machine : Primus.Machine.S) = struct
  module Value = Primus.Value.Make(Machine)
  module Env = Primus.Env.Make(Machine)
  open Machine.Syntax

  let get_env v = Env.get v >>| Option.some

  let init () =
    let get v = match Var.typ v with
      | Imm _ when Var.is_physical v ->
         Machine.catch (get_env v) (fun _ -> !! None)
      | _ -> !! None in
    Machine.Global.get initial_values >>= fun s ->
    Env.all >>= fun all ->
    Machine.Seq.fold all ~init:s ~f:(fun s v ->
        get v >>= function
        | None -> !! s
        | Some x -> !! (Set.add s (Value.id x))) >>= fun s ->
    Machine.Global.put initial_values s

end

module IsInitialValue(Machine : Primus.Machine.S) = struct
  module Value = Primus.Value.Make(Machine)
  open Machine.Syntax

  [@@@warning "-P"]
  let run [x] =
    Machine.Local.get initial_values >>= fun s ->
    Value.of_bool (Set.mem s (Value.id x))
end

module Lisp_primitives(Machine : Primus.Machine.S) = struct
  module Lisp = Primus.Lisp.Make(Machine)
  let def name types docs closure = Lisp.define ~docs ~types name closure
end


module Notify_prim(Machine : Primus.Machine.S) = struct
  include Lisp_primitives(Machine)
  open Primus.Lisp.Type.Spec

  let init () =
    Machine.sequence Primus.Interpreter.[
        def "notify-null-ptr-dereference" (tuple [a;b] @-> any)
          "(notify-null-ptr-dereference INTRO ADDR)
           print message to stdout when pointer that was
           introduced at address INTRO is dereferenced at
           address ADDR"
          (module Notify);
    ]
end

module Lisp(Machine : Primus.Machine.S) = struct
  include Lisp_primitives(Machine)
  open Primus.Lisp.Type.Spec

  let init () =
    Machine.sequence Primus.Interpreter.[
        def "is-reported-deref" (tuple [a] @-> bool)
          "(is-reported-deref ADDR) returns true if
           a null pointer dereference at ADDR is the known
           incident and was reported before"
          (module IsReported);

        def "is-untrusted" (unit @-> bool)
          "(is-untrusted) returns true if a machine
           during the execution skipped some function
           due to scheme in promiscuous mode."
          (module Calls_tracker.IsUntrusted);

        def "is-initial-value" (tuple [a] @-> bool)
          "(is-initial-value X) return true if value X
           is the same value that was assigned to one of GPR
           on init stage. Such values are excluded from
           analysis to reduce false positives"
          (module IsInitialValue);

        def "mark-taint-as-checked" (tuple [a] @-> bool)
          "(mark-taint-as-checked T) mark the taint T
           as checked one, e.g. if the taint reached
           a condition"
          (module MarkChecked);

        def "is-checked-pointer" (tuple [a] @-> bool)
          "(is-checked-pointer PTR) returns true if
           a taint from PTR was marked as checked"
          (module IsChecked);

        def "is-return-from-unresolved" (tuple [a] @-> bool)
          "(is-return-from-unresolved X) returns true if
           a value X is read from abi-specific register
           right after unresolved call returned"
          (module Calls_tracker.IsIgnoredReturn);
    ]
end

let main silent =
  if not silent then
    Primus.Machine.add_component (module Notify_prim);
  Primus.Machine.add_component (module Lisp);
  Primus.Machine.add_component (module InitialValues);
  Calls_tracker.init ()

open Config

;;
manpage [
  `S "DESCRIPTION";
  `P "";
];;

let enabled = flag "enable" ~doc:"Enables the analysis"
let silent  = flag "silent" ~doc:"Don't output results"

let () = when_ready (fun {get=(!!)} ->
             if !!enabled then  main !!silent)
