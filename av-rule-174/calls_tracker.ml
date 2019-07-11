open Core_kernel
open Bap.Std
open Bap_primus.Std


type tracker = {
    callstack  : string list;
    skipped    : String.Set.t;
    ignored    : Primus.Value.Id.Set.t;
  }

let tracker = Primus.Machine.State.declare
    ~uuid:"95595c06-c9e0-47fe-a4d3-ba2e1a1a3bf6"
    ~name:"calls-tracker"
    (fun _ -> {
         callstack  = [];
         skipped    = Set.empty (module String);
         ignored    = Set.empty (module Primus.Value.Id);
    })

module Abi_specific(Machine : Primus.Machine.S) = struct
  open Machine.Syntax

  (* TODO: default implentation, need to use an actual abi *)
  let return_arg =
    Machine.arch >>= fun a ->
    match a with
    | `x86_64 -> !! (Some X86_cpu.AMD64.rax)
    | `x86 -> !! (Some X86_cpu.IA32.rax)
    | _ -> !! None

end

module Tracker(Machine : Primus.Machine.S) = struct
  module Abi = Abi_specific(Machine)
  module Env = Primus.Env.Make(Machine)
  module Value = Primus.Value.Make(Machine)

  open Machine.Syntax

  let return = !! ()

  let increase_callstack (name,_) =
    Machine.Local.update tracker
      ~f:(fun s ->
        {s with
          callstack = name :: s.callstack;
          skipped = Set.remove s.skipped name})

  let get_name tid =
    let name = Tid.name tid in
    match String.chop_prefix name ~prefix:"@" with
    | None -> name
    | Some x -> x

  let on_jump jmp =
    match Jmp.kind jmp with
    | Goto _ | Ret _ | Int _ -> return
    | Call c -> match Call.target c with
      | Indirect _ -> return
      | Direct tid ->
         Machine.Local.update tracker
           ~f:(fun s -> {s with skipped = Set.add s.skipped (get_name tid)})

  let decrease_callstack _ =
    Machine.Local.update tracker ~f:(fun s ->
        match s.callstack with
        | [] -> s
        | _::callstack -> {s with callstack})

  let update_ignored_values _ =
    Abi.return_arg >>= function
    | None -> Machine.return ()
    | Some arg ->
       Env.get arg >>= fun v ->
       Machine.Local.update tracker ~f:(fun s ->
           {s with ignored = Set.add s.ignored (Value.id v)})

  let init () = Machine.sequence Primus.Linker.[
      Trace.call >>> increase_callstack;
      Trace.return >>> decrease_callstack;
      unresolved >>> update_ignored_values;
      Primus.Interpreter.enter_jmp >>> on_jump;
    ]
end

module IsIgnoredReturn(Machine : Primus.Machine.S) = struct
  module Value = Primus.Value.Make(Machine)
  open Machine.Syntax

  [@@@warning "-P"]
  let run [v] =
    Machine.Local.get tracker >>= fun t ->
    Value.of_bool (Set.mem t.ignored (Value.id v))
end


let _,skipped_fun =
  Primus.Observation.provide ~inspect:Sexp.of_string "skipped-fun"

module IsUntrusted(Machine : Primus.Machine.S) = struct
  module Value = Primus.Value.Make(Machine)
  open Machine.Syntax

  let run _ =
    Machine.Local.get tracker >>= fun {skipped;} ->
    Machine.List.iter (Set.to_list skipped)
      ~f:(Machine.Observation.make skipped_fun) >>= fun () ->
    Value.of_bool (not @@ Set.is_empty skipped)
end

let init () = Primus.Machine.add_component (module Tracker)
