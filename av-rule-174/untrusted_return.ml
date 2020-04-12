open Core_kernel
open Bap.Std
open Bap_primus.Std

type t = {
  untrusted : Primus.Value.Id.Set.t;
}

let state = Primus.Machine.State.declare
    ~uuid:"95595c06-c9e0-47fe-a4d3-ba2e1a1a3bf6"
    ~name:"untrusted-values"
    (fun _ -> {
         untrusted = Set.empty (module Primus.Value.Id);
       })

module UntrustedValues(Machine : Primus.Machine.S) = struct
  module Env = Primus.Env.Make(Machine)
  module Value = Primus.Value.Make(Machine)

  open Machine.Syntax

  let abi_specific_return_arg =
    Machine.arch >>= fun a ->
    match a with
    | `x86_64 -> !! (Some X86_cpu.AMD64.rax)
    | `x86 -> !! (Some X86_cpu.IA32.rax)
    | _ -> !! None

  let get_name tid =
    let name = Tid.name tid in
    match String.chop_prefix name ~prefix:"@" with
    | None -> name
    | Some x -> x

  let update_untrusted_values _ =
    abi_specific_return_arg >>= function
    | None -> Machine.return ()
    | Some arg ->
      Env.get arg >>= fun v ->
      Machine.Local.update state ~f:(fun s ->
          {untrusted = Set.add s.untrusted (Value.id v)})

  let init () = Machine.sequence Primus.Linker.[
      unresolved >>> update_untrusted_values;
    ]
end

module IsUntrustedReturn(Machine : Primus.Machine.S) = struct
  module Value = Primus.Value.Make(Machine)
  open Machine.Syntax

  [@@@warning "-P"]
  let run [v] =
    Machine.Local.get state >>= fun t ->
    Value.of_bool (Set.mem t.untrusted (Value.id v))
end

let init () = Primus.Machine.add_component (module UntrustedValues)
[@@warning "-D"]
