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

module Notify(Machine : Primus.Machine.S) = struct
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
        printf "Got null pointer dereference at %s\n%!"
          (Addr.to_string addr) in
      Value.b1

end

type calls = {
    callstack : string list;
    unresolved : String.Set.t;
  }

let calls = Primus.Machine.State.declare
    ~uuid:"95595c06-c9e0-47fe-a4d3-ba2e1a1a3bf6"
    ~name:"calls-tracker"
    (fun _ -> {callstack = []; unresolved = Set.empty (module String)})

module Calls_tracker(Machine : Primus.Machine.S) = struct
  open Machine.Syntax

  let on_enter (name,_) =
    Machine.Local.update calls ~f:(fun s -> {s with callstack = name :: s.callstack})

  let on_return (name,_) =
    Machine.Local.update calls ~f:(fun s ->
        match s.callstack with
        | [] -> s
        | _::callstack -> {s with callstack})

  let on_unresolved name =
    Machine.Local.get calls >>= fun s ->
    match name, s.callstack with
    | `addr _, name :: _ ->
       Machine.Local.put calls {s with unresolved = Set.add s.unresolved name}
    | `symbol name, _ ->
       Machine.Local.put calls {s with unresolved = Set.add s.unresolved name}
    | _ -> Machine.return ()

  let init () = Machine.sequence Primus.Linker.[
      Trace.call >>> on_enter;
      Trace.return >>> on_return;
      unresolved >>> on_unresolved
    ]
end

module Unresolved(Machine : Primus.Machine.S) = struct
  open Machine.Syntax
  module Value = Primus.Value.Make(Machine)

  [@@@warning "-P"]
  let run [name] =
    Value.Symbol.of_value name >>= fun name ->
    Machine.Local.get calls >>= fun {unresolved} ->
    Value.of_bool (Set.mem unresolved name)

end

(* TODO: default implentation, need to use an actual abi *)
module Abi_specific_return_arg(Machine : Primus.Machine.S) = struct
  module Value = Primus.Value.Make(Machine)
  open Machine.Syntax

  let run _ =
    Machine.arch >>= fun a ->
    match a with
    | `x86_64 ->
       Value.Symbol.to_value (Var.name X86_cpu.AMD64.rax)
    | _ -> Value.b0
end

module Output_arg(Machine : Primus.Machine.S) = struct
  module Value = Primus.Value.Make(Machine)
  module Env = Primus.Env.Make(Machine)
  open Machine.Syntax

  let find sub =
    Machine.get () >>= fun proj ->
    Value.Symbol.of_value sub >>= fun name ->
    let subs = Term.to_sequence sub_t (Project.program proj) in
    match Seq.find subs ~f:(fun s -> String.(Sub.name s = name)) with
    | None -> Machine.return None
    | Some sub ->
       Seq.find (Term.to_sequence arg_t sub)
         ~f:(fun a -> Arg.intent a = Some Out) |> function
    | None -> Machine.return None
    | Some out -> Machine.return (Some out)

end

module Return_var(Machine : Primus.Machine.S) = struct
  module Value = Primus.Value.Make(Machine)
  module Env = Primus.Env.Make(Machine)
  module Out = Output_arg(Machine)
  open Machine.Syntax

  [@@@warning "-P"]
  let run [sub] =
    Out.find sub >>= function
    | None -> Value.b0
    | Some a -> Value.Symbol.to_value (Var.name (Arg.lhs a))
end


module Lisp(Machine : Primus.Machine.S) = struct
  let init () =
    let module Lisp = Primus.Lisp.Make(Machine) in
    let open Primus.Lisp.Type.Spec in
    Machine.sequence Primus.Interpreter.[
        Lisp.define "null-ptr-dereference" (module Notify)
          ~types:(unit @-> any)
          ~docs:"Notifies that null pointer dereferenced";

        Lisp.define "has-control-dependency" (module Control_dependency.Lisp)
          ~types:(tuple [a] @-> bool)
          ~docs:"";

        Lisp.define "is-unresolved" (module Unresolved)
          ~types:(tuple [a] @-> bool)
          ~docs:"";

        Lisp.define "abi-specific-return-arg" (module Abi_specific_return_arg)
          ~types:(unit @-> b)
          ~docs:"";
    ]
end

let main () =
  Primus.Machine.add_component (module Lisp);
  Primus.Machine.add_component (module Control_dependency.Trace);
  Primus.Machine.add_component (module Calls_tracker)

module No_return = struct

  class mapper no_rets = object
    inherit Term.mapper

    method! map_jmp j =
      match Jmp.kind j with
      | Goto _ | Ret _ | Int _ -> j
      | Call c ->
         match Call.target c with
         | Indirect _ -> j
         | Direct tid ->
            if List.mem no_rets tid ~equal:Tid.equal then
              Jmp.with_kind j (Call (Call.with_noreturn c))
            else j
  end

  (* Subroutine is noreturn when:
     - all the calls from subroutine are non return
       (and a list of such calls is not empty)
     - id doesn't contain interupt or return jumps *)
  let is_no_return sub =
    let is_goto j =
      Jmp.kind j |> function | Goto _ -> true | _ -> false in
    let jumps =
      Seq.fold (Term.to_sequence blk_t sub) ~init:[]
        ~f:(fun acc b ->
          let jumps = Term.to_sequence jmp_t b in
          let jumps = Seq.filter jumps ~f:(Fn.non is_goto) in
          Seq.to_list jumps @ acc) in
    match jumps with
    | [] -> false
    | jumps ->
       List.for_all jumps ~f:(fun j ->
           match Jmp.kind j with
           | Call c -> Option.is_none (Call.return c)
           | _ -> false)

  let no_rets subs =
    Seq.filter subs ~f:is_no_return |> Seq.to_list

  let main proj =
    let prog = Project.program proj in
    let names = ["abort"; "__stack_chk_fail"] in
    let subs = Term.to_sequence sub_t prog in
    let no_rets = no_rets subs in
    let no_rets = no_rets @
      (Seq.filter_map subs ~f:(fun s ->
          if List.mem names (Sub.name s) ~equal:String.equal then
            Some s
          else None) |> Seq.to_list) in
    let no_rets = List.map ~f:Term.tid no_rets in
    Project.with_program proj ((new mapper no_rets)#run prog)

end

let () = Project.register_pass ~autorun:true No_return.main


open Config

;;
manpage [
  `S "DESCRIPTION";
  `P "";
];;


when_ready (fun _ -> main ())
