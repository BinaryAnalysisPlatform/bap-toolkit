open Core_kernel
open Bap.Std
open Bap_taint.Std
open Bap_primus.Std

module Machine_id = Monads.Std.Monad.State.Multi.Id
module Machines = Machine_id.Map
module Values = Primus.Value.Set
module Vids = Primus.Value.Id.Set

type value = Primus.value
type machine = Primus.Machine.id


type state = {
    references : machine list Primus.Value.Id.Map.t;
    owners     : Vids.t Machines.t;
    taints     : value Primus.Value.Id.Map.t;
    forks      : Machine_id.Set.t
  }

let state = Primus.Machine.State.declare
    ~name:"taints-tracker"
    ~uuid:"d5dba994-da2c-4516-9591-a3bb0cdb9e4a"
    (fun _ -> {
         references = Map.empty (module Primus.Value.Id);
         owners     = Map.empty (module Machine_id);
         taints     = Map.empty (module Primus.Value.Id);
         forks      = Set.empty (module Machine_id);
    })


let taint_finish, taint_reached_finish =
  Primus.Observation.provide ~inspect:Primus.sexp_of_value "taint-reached-finish"

module Tracker(Machine : Primus.Machine.S) = struct
  module Value = Primus.Value.Make(Machine)
  open Machine.Syntax

  let on_new_taint taint =
    Machine.current () >>= fun cur ->
    let vid = Value.id taint in
    Machine.Global.update state (fun s ->
        {s with
         owners = Map.update s.owners cur ~f:(function
             | None -> Vids.singleton vid
             | Some vs -> Set.add vs vid);
         references =  Map.update s.references vid ~f:(function
             | None -> [cur]
             | Some ms -> cur :: ms);
         taints = Map.set s.taints vid taint})

  let on_new_machine s id =
    Machine.ancestor [id] >>= fun par ->
    match Map.find s.owners par with
    | None -> !! s
    | Some vs ->
       !!
         { s with
           references =
             Set.fold vs ~init:s.references ~f:(fun ms v ->
                 Map.update ms v ~f:(function
                     | None -> [id]
                     | Some ms -> id :: ms));
           owners = Map.set s.owners id vs}

  let on_last_reference = function
    | None -> !! ()
    | Some t -> Machine.Observation.make taint_reached_finish t

  let clear_references s vids machine_id =
    let s, rm =
      Set.fold vids ~init:(s,[]) ~f:(fun (s,rm) id ->
          match Map.find s.references id with
          | None -> s,rm
          | Some [mid] when Machine_id.(mid = machine_id) ->
             let taint = Map.find s.taints id in
             let taints = Map.remove s.taints id in
             let references = Map.remove s.references id in
             { s with taints; references}, taint :: rm
          | Some machines ->
            let machines =
              List.filter machines
                ~f:(fun mid -> Machine_id.(mid <> machine_id)) in
            {s with references = Map.set s.references id machines}, rm) in
    Machine.Global.put state s >>= fun () ->
    Machine.List.iter rm ~f:on_last_reference

  let on_machine_death () =
    Machine.current () >>= fun cur ->
    Machine.Global.get state >>= fun s ->
    match Map.find s.owners cur with
    | None -> !! ()
    | Some vids ->
       let s = {s with owners = Map.remove s.owners cur} in
       clear_references s vids cur

  let empty = Set.empty (module Machine_id)
  let of_seq = Seq.fold ~init:empty ~f:Set.add

  let detect_forks _ =
    Machine.forks () >>= fun glob ->
    Machine.Global.get state >>= fun s ->
    if Seq.length glob = Set.length s.forks then !! ()
    else
      let glob = of_seq glob in
      let diff = Set.diff glob s.forks |> Set.to_list in
      Machine.List.fold diff ~init:s ~f:on_new_machine >>= fun s ->
      Machine.Global.put state { s with forks = glob }

  let init () =
    Machine.sequence [
        Primus.Machine.finished >>> on_machine_death;
        Primus.Interpreter.jumping  >>> detect_forks;
        Primus.Linker.Trace.call    >>> detect_forks;
      ]

end

module Lisp(Machine : Primus.Machine.S) = struct
  module Lisp = Primus.Lisp.Make(Machine)
  open Primus.Lisp.Type.Spec

  let init () =
    Lisp.signal
      ~doc:"Lisp signal is called when there is no machines where
            taint is still alive"
      taint_finish (fun t -> Machine.return [t]);
end


let init () =
  Primus.Machine.add_component (module Tracker);
  Primus.Machine.add_component (module Lisp);
