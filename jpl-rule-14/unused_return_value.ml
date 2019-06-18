open Core_kernel
open Bap.Std
open Bap_primus.Std
open Bap_taint.Std

include Self ()

type value = Primus.value
type machine = Primus.Machine.id

module Machine_id = Monads.Std.Monad.State.Multi.Id
module Machines = Machine_id.Map
module Values = Primus.Value.Set
module Vids = Primus.Value.Id.Set

type candidate = string * addr

type proof = Used | Unused

type state = {
    names      : candidate Primus.Value.Id.Map.t;
    candidates : machine list Primus.Value.Id.Map.t;
    machines   : Vids.t Machines.t;
    proved     : proof Addr.Map.t
  }

let state = Primus.Machine.State.declare
    ~name:"unused-results"
    ~uuid:"af66d451-fb62-44c3-9c2a-8969e111ad91"
    (fun _ -> {
         names = Map.empty (module Primus.Value.Id);
         machines = Map.empty (module Machine_id);
         candidates  = Map.empty (module Primus.Value.Id);
         proved = Map.empty (module Addr)
    })


let same_machine x y = Machine_id.(x = y)

let _, unused =
  Primus.Observation.provide ~inspect:sexp_of_string "unused"

let notify name addr =
  printf "unused result of %s at %a\n%!" name Addr.ppo addr

module Results(Machine : Primus.Machine.S) = struct
  module Value = Primus.Value.Make(Machine)
  module Object = Taint.Object.Make(Machine)
  open Machine.Syntax

  let create taint name addr =
    let addr = Value.to_word addr in
    let vid = Value.id taint in
    Value.Symbol.of_value name >>= fun name ->
    Machine.current () >>= fun cur ->
    Machine.Global.update state ~f:(fun s ->
        { s with
          names = Map.set s.names vid (name,addr);

          machines = Map.update s.machines cur ~f:(function
                           | None -> Vids.singleton vid
                           | Some vs -> Set.add vs vid);

          candidates = Map.update s.candidates vid ~f:(function
                         | None -> [cur]
                         | Some ms -> cur :: ms)}) >>= fun () ->
    Value.b1


  let on_new_machine par id =
    Machine.Global.get state >>= fun s ->
    match Map.find s.machines par with
    | None -> !! ()
    | Some vs ->
      Machine.Global.put state
        { s with candidates =
                   Set.fold vs ~init:s.candidates ~f:(fun ms v ->
                       Map.update ms v ~f:(function
                           | None -> [id]
                           | Some ms -> id :: ms)) }

  let mark_used taint =
    let vid = Value.id taint in
    Machine.Global.get state >>= fun s ->
    let s =
      match Map.find s.names vid with
      | None -> s
      | Some (_,a) ->
         { s with proved = Map.set s.proved a Used; } in
    Machine.Global.put state
      { s with
        names = Map.remove s.names vid;
        candidates = Map.remove s.candidates vid;} >>= fun () ->
    Value.b1

  let on_machine_finished () =
    Machine.current () >>= fun cur ->
    Machine.Global.update state ~f:(fun s ->
    {s with machines = Map.remove s.machines cur})

  let get_addr v =
    Machine.Global.get state >>= fun s ->
    match Map.find s.names v with
    | None -> !! None
    | Some (_,a) -> !! (Some a)

  let is_proved how state addr =
    match Map.find state.proved addr with
    | Some how' -> how' = how
    | _  -> false

  let is_not_proved state addr = not (Map.mem state.proved addr)

  let is_proved_used v =
    Machine.Global.get state >>= fun s ->
    match Map.find s.names v with
    | None -> !! false
    | Some (_,a) -> !! (is_proved Used s a)

  let is_new_incident addr v =
    Machine.current () >>= fun cur ->
    Machine.Global.get state >>= fun s ->
    match Map.find s.names v with
    | None -> !! false
    | Some (_,addr) ->
       match Map.find s.candidates v with
       | Some [id] ->
          !! (same_machine id cur && is_not_proved s addr)
       | _  -> !!false

  let is_unused taint =
    let vid = Value.id taint in
    Machine.Global.get state >>= fun s ->
    match Map.find s.names vid with
    | None -> Value.b0
    | Some (name,addr) ->
       is_new_incident addr vid >>= Value.of_bool

  let mark_unused taint =
    let vid = Value.id taint in
    Machine.Global.get state >>= fun s ->
    match Map.find s.names vid with
    | None -> Value.b0
    | Some (name,addr) ->
       Machine.Observation.make unused name  >>= fun () ->
       notify name addr;
       let s = {s with proved = Map.set s.proved addr Unused} in
       Machine.Global.put state s >>= fun () ->
       Value.b1

  let mark_finalized taint =
    let vid = Value.id taint in
    Machine.current () >>= fun cur ->
    Machine.Global.update state ~f:(fun s ->
        match Map.find s.candidates vid with
        | None -> s
        | Some [id] when same_machine id cur ->
           { s with
             names = Map.remove s.names vid;
             candidates = Map.remove s.candidates vid;}
        | Some ids ->
           let ids = List.filter ids
                       ~f:(fun id -> not (same_machine id cur)) in
           {s with candidates = Map.set s.candidates vid ids }) >>=
      fun () -> Value.b1

end

module IsUnused(Machine : Primus.Machine.S) = struct
  module Results = Results(Machine)

  [@@@warning "-P"]
  let run [v] = Results.is_unused v
end

module MaybeUnused(Machine : Primus.Machine.S) = struct
  module Results = Results(Machine)

  [@@@warning "-P"]
  let run [v; name; addr] = Results.create v name addr
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

module MarkFinalized(Machine : Primus.Machine.S) = struct
  module Results = Results(Machine)

  [@@@warning "-P"]
  let run [v] = Results.mark_finalized v
end

let forks = Primus.Machine.State.declare
    ~name:"fork-detector"
    ~uuid:"10d77e40-b223-4456-9d00-9d3ed0118191"
    (fun _ -> None, Set.empty (module Machine_id))


module ForkDetector(Machine : Primus.Machine.S) = struct
  module Results = Results(Machine)
  open Machine.Syntax

  let empty = Set.empty (module Machine_id)
  let of_seq = Seq.fold ~init:empty ~f:Set.add

  let on_leave_pos _ =
    Machine.forks () >>= fun fs ->
    Machine.current () >>= fun id ->
    Machine.Global.put forks (Some id, of_seq fs)

  let check_forks _ =
    Machine.forks () >>= fun fs ->
    Machine.Global.get forks >>= fun (par, fs') ->
    Machine.current () >>= fun cur ->
    of_seq fs |> fun fs ->
    Set.diff fs fs' |> Set.to_list |> fun diff ->
    match par with
    | None -> Machine.Global.put forks (Some cur, fs)
    | Some _ when diff = [] -> !! ()
    | Some par ->
      Machine.List.iter diff ~f:(Results.on_new_machine par) >>= fun () ->
      Machine.Global.put forks (Some cur, fs)

  let init () = Machine.sequence Primus.Interpreter.[
      leave_pos >>> on_leave_pos;
      read >>> check_forks;
      enter_pos >>> check_forks;
      leave_blk >>> check_forks;
    ]

end

module HandleUnresolved(Machine : Primus.Machine.S) = struct
  module Value = Primus.Value.Make(Machine)
  module Interpreter = Primus.Interpreter.Make(Machine)
  open Machine.Syntax

  let set_zero v =
    match Var.typ v with
    | Mem _ -> !! ()
    | Imm width ->
       Value.of_int ~width 0 >>= fun x ->
       Interpreter.set v x

  let on_unresolved _ =
    Machine.arch >>= function
    | `x86_64 -> set_zero X86_cpu.AMD64.rax
    | `x86 -> set_zero X86_cpu.IA32.rax
    |  _ -> !! ()

  let init () =  Primus.Linker.unresolved >>> on_unresolved
end

module Return_arg(Machine : Primus.Machine.S) = struct
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

  [@@@warning "-P"]
  let run [sub] =
    find sub >>= function
    | None -> Value.b0
    | Some a -> Value.Symbol.to_value (Var.name (Arg.lhs a))
end

module Interface(Machine : Primus.Machine.S) = struct
  module Lisp = Primus.Lisp.Make(Machine)
  open Primus.Lisp.Type.Spec

  let init () =
    Machine.sequence [

        Lisp.define "check-if-used" (module MaybeUnused)
          ~types:(tuple [a;b;c] @-> b)
          ~docs:{|(check-if-used T SUB ADDR) - marks
                 the return argument of SUB called at ADDR and
                 tainted with T for checking|};

        Lisp.define "is-unused" (module IsUnused)
          ~types:(tuple [a] @-> b)
          ~docs:"";

        Lisp.define "mark-unused" (module MarkUnused)
          ~types:(tuple [a] @-> b)
          ~docs:"";

        Lisp.define "mark-used" (module MarkUsed)
          ~types:(tuple [a] @-> b)
          ~docs:"";

        Lisp.define "mark-finalized" (module MarkFinalized)
          ~types:(tuple [a] @-> b)
          ~docs:"";

        Lisp.define "return-arg" (module Return_arg)
          ~types:(tuple [a] @-> b)
          ~docs:
          ({|(return-arg SUB) returns the name of output argument of the
            subroutine SUB. Returns NIL if the subroutine SUB doesn't
            return anything or subroutine's api is unknown|});

      ]
end

open Config

let enabled = flag "enable" ~doc:"Enable the analysis."

let () = when_ready (fun {get=(!!)} ->
             if !!enabled then
               List.iter ~f:ident [
                   Primus.Machine.add_component (module Interface);
                   Primus.Machine.add_component (module ForkDetector);
                   Primus.Machine.add_component (module HandleUnresolved);
           ])
