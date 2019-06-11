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

  let run inputs =
    Eval.pc >>= fun addr ->
    Machine.Local.update derefs
      ~f:(fun s -> {addrs = Set.add s.addrs addr}) >>= fun () ->
    Machine.Global.get derefs >>= fun s ->
    if Set.mem s.addrs addr then Value.b1
    else
      Machine.Global.put derefs ({addrs = Set.add s.addrs addr}) >>=  fun () ->
      let () =
        printf "Got null pointer dereference at %s\n%!"
          (Addr.to_string addr) in
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


module Lisp(Machine : Primus.Machine.S) = struct

  let init () =
    let module Lisp = Primus.Lisp.Make(Machine) in
    let open Primus.Lisp.Type.Spec in
    let def name types docs closure = Lisp.define ~docs ~types name closure in
    Machine.sequence Primus.Interpreter.[
        def "is-reported" (tuple [a] @-> bool) "" (module IsReported);
        def "null-ptr-dereference" (unit @-> any) "" (module Notify);
        def "is-untrusted" (unit @-> bool) "" (module Calls_tracker.IsUntrusted);

        def "mark-checked" (tuple [a] @-> bool) "" (module MarkChecked);
        def "is-checked" (tuple [a] @-> bool) "" (module IsChecked);
        def "is-return-from-unresolved" (tuple [a] @-> bool) "" (module Calls_tracker.IsIgnoredReturn);

    ]
end

let main () =
  Primus.Machine.add_component (module Lisp);
  No_return.init ()


open Config

;;
manpage [
  `S "DESCRIPTION";
  `P "";
];;



when_ready (fun _ -> main ())
