open Core_kernel
open Bap.Std
open Bap_primus.Std

include Self ()

type state = {
  addrs : Addr.Set.t;
}

let state = Primus.Machine.State.declare
    ~uuid:"3e065801-3af7-41f0-af1c-e20b9407da11"
    ~name:"null-ptr-dereference"
    (fun _ -> {addrs = Set.empty (module Addr)})

module Null_ptr_deref(Machine : Primus.Machine.S) = struct
  open Machine.Syntax
  module Value = Primus.Value.Make(Machine)
  module Eval = Primus.Interpreter.Make(Machine)

  let run inputs =
    Eval.pc >>= fun addr ->
    Machine.Global.get state >>= fun s ->
    if Set.mem s.addrs addr then Value.b1
    else
      Machine.Global.put state ({addrs = Set.add s.addrs addr}) >>=  fun () ->
      let () =
        printf "Got null pointer dereference at %s\n"
          (Addr.to_string addr) in
      Value.b1

end

module Lisp(Machine : Primus.Machine.S) = struct
  let init () =
    let module Lisp = Primus.Lisp.Make(Machine) in
    let open Primus.Lisp.Type.Spec in
    Machine.sequence Primus.Interpreter.[
        Lisp.define "null-ptr-dereference" (module Null_ptr_deref)
          ~types:(unit @-> any)
          ~docs:"Notifies that null pointer dereferenced";

        Lisp.define "has-control-dependency" (module Control_dependency.Lisp)
          ~types:((tuple [a] @-> bool))
          ~docs:"";
    ]
end

let main () =
  Primus.Machine.add_component (module Lisp);
  Primus.Machine.add_component (module Control_dependency.Trace)

open Config

;;
manpage [
  `S "DESCRIPTION";
  `P "";
];;


when_ready (fun _ -> main ())
