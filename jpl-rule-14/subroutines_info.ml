open Core_kernel
open Bap.Std
open Bap_primus.Std

let arguments =
  Primus.Machine.State.declare
    ~name:"subroutines-arguments"
    ~uuid:"06f656ad-e922-4fa5-bc65-a3203b6819a2"
    (fun _ -> Map.empty (module String))

let return_values = Primus.Machine.State.declare
    ~name:"return-values"
    ~uuid:"3ab8d8ee-1304-46d3-9860-0960aaf2ac42"
    (fun _ -> Map.empty (module Primus.Value.Id))


type callsite = {
  calls : addr String.Map.t;
  pc    : addr option;
}

let callsite = Primus.Machine.State.declare
    ~name:"unused-results-callsite"
    ~uuid:"1715922a-4e6d-4960-bbd4-a6fb8e239ddb"
    (fun _ -> { calls = Map.empty (module String); pc = None})

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
    Machine.Global.put arguments m
end

module Return_values(Machine : Primus.Machine.S) = struct
  module Env = Primus.Env.Make(Machine)
  module Value = Primus.Value.Make(Machine)

  open Machine.Syntax

  let on_call_return (name, args) =
    Machine.Global.get arguments >>= fun s ->
    match Map.find s name with
    | None -> Machine.return ()
    | Some _ ->
      match List.last args with
      | None -> Machine.return ()
      | Some value ->
        Machine.Local.update return_values ~f:(fun vals ->
            Map.set vals (Primus.Value.id value) name)

  let init() =
    Primus.Linker.Trace.return >>> on_call_return

end

module Has_known_api(Machine : Primus.Machine.S) = struct
  module Value = Primus.Value.Make(Machine)
  module Linker = Primus.Linker.Make(Machine)
  open Machine.Syntax

  [@@@warning "-P"]
  let run [addr] =
    let addr = `addr (Value.to_word addr) in
    Linker.resolve_symbol addr >>= function
    | None -> Value.b0
    | Some name ->
      Machine.Global.get arguments >>= fun subs ->
      Value.of_bool (Map.mem subs name)

end


module Callsite (Machine : Primus.Machine.S) = struct
  module Value  = Primus.Value.Make(Machine)
  module Linker = Primus.Linker.Make(Machine)
  module Interp = Primus.Interpreter.Make(Machine)

  open Machine.Syntax

  let on_call (name,_) =
    Machine.Local.update callsite ~f:(fun s ->
        match s.pc with
        | None -> s
        | Some pc ->
          {calls = Map.set s.calls name pc; pc = None})

  let on_jump j =
    match Jmp.kind j with
    | Goto _ | Int _ | Ret _ -> Machine.return ()
    | Call c ->
      Interp.pc >>= fun pc ->
      Machine.Local.update callsite
        ~f:(fun s -> {s with pc = Some pc})

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


module Find(Machine : Primus.Machine.S) = struct
  module Value = Primus.Value.Make(Machine)
  open Machine.Syntax

  [@@@warning "-P"]
  let run [v] =
    Machine.Local.get return_values >>= fun rets ->
    match Map.find rets (Primus.Value.id v) with
    | None -> Value.b0
    | Some name -> Value.Symbol.to_value name

end


let init () =
  Primus.Machine.add_component (module Known_subs);
  Primus.Machine.add_component (module Return_values);
  Primus.Machine.add_component (module Callsite)
[@@warning "-D"]
