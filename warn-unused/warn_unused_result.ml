(** The Algorithm

    When a call to a function that has any of its arguments marked
    with the warn-unused-result attribute is made we emit the
    warn-unused-result-call signal parameterized with the values that
    we have to check. This signal is received by the
    warn-unused-result Lisp code that defines the policy. The value is
    tainted and tracked in the dictionary of values to be checked.

    Once we have an external call that consumes a tainted value we
    dispatch the warn-unused-tainted-external-call signal for each
    taint of the warn-unused kind. The Lisp code uses this signal to
    sanitize the taint and mark the corresponding value as checked.

    In addition the Lisp code monitors the `jumping` signal and
    sanitizes the warn-unused taint associated with its condition.

    Finally, when the taint-finalize signal occurs for a taint that is
    not marked as sanitized we report an incident.

    See warn-unused-result.lisp for more information.
 **)

open Core_kernel
open Bap.Std
open Bap_primus.Std
open Bap_main
open Bap_taint.Std

let check_name = "warn unused result"

let print_results rs =
  let open Format in
  let pp_bold ppf = fprintf ppf "\027[1m" in
  let pp_norm ppf = fprintf ppf "\027[0m" in
  match rs with
  | [] -> printf "%s   OK\n" check_name
  | rs ->
    printf "%s   FAIL\n\n" check_name;
    printf "%t%-10s %-20s\n%t" pp_bold "Address" "Function" pp_norm;
    List.iter rs ~f:(fun (addr,name) ->
        let addr = sprintf "%a" Addr.pps addr in
        printf "%-10s %-20s\n%!" addr name)

let is_wur sub =
  Term.enum arg_t sub |> Seq.exists ~f:(fun arg ->
      Dict.mem (Term.attrs arg) Arg.warn_unused)

let is_ext sub = Term.has_attr sub Sub.stub

let select_subs is_our prog =
  Term.enum sub_t prog |> Seq.fold
    ~f:(fun subs sub ->
        if is_our sub
        then Map.add_exn subs (Term.tid sub) (Sub.name sub)
        else subs)
    ~init:(Map.empty (module Tid))

let callsite_collector ours = object
  inherit [string Map.M(Addr).t] Term.visitor
  method! enter_jmp t sites =
    match Jmp.alt t with
    | None -> sites
    | Some dst -> match Jmp.resolve dst with
      | Second _ -> sites
      | First dst -> match Map.find ours dst with
        | None -> sites
        | Some name -> match Term.get_attr t address with
          | None -> sites
          | Some addr -> Map.set sites addr name
end

let collect_sites pred proj =
  let prog = Project.program proj in
  let collector = callsite_collector (select_subs pred prog) in
  collector#run prog (Map.empty (module Addr))

let warn_unused_introduce,introduced =
  Primus.Observation.provide "warn-unused-result/introduce"
    ~package:"bap"
    ~inspect:Taint.Object.inspect
    ~desc:"Occurs on a return from a function that has \
           the GNU warn_unused_result attribute returns. \
           Parameterized with the returned value."

let warn_unused_sanitize,sanitized =
  Primus.Observation.provide "warn-unused-result/sanitize"
    ~package:"bap"
    ~inspect:Taint.Object.inspect
    ~desc:"Occurs when a value that has a warn-unused taint \
           is passed to an external function."

type state = {
  unchecked : addr Map.M(Taint.Object).t;
  callsites : string Map.M(Addr).t;
  externals : string Map.M(Addr).t;

}

let state =
  Primus.Machine.State.declare
    ~name:"warn-unused-result"
    ~uuid:"1e117d51-3b9d-4c93-b136-e15b77c5ea26" @@ fun proj ->
  {
    unchecked = Map.empty (module Taint.Object);
    callsites = collect_sites is_wur proj;
    externals = collect_sites is_ext proj;
  }

module Tracker(Machine : Primus.Machine.S) = struct
  open Machine.Syntax
  open Machine.Let
  module Eval = Primus.Interpreter.Make(Machine)
  module Lisp = Primus.Lisp.Make(Machine)
  module Kind = Taint.Kind.Make(Machine)
  module Object = Taint.Object.Make(Machine)
  module Tracker = Taint.Tracker.Make(Machine)

  let wur_kind = Kind.create "bap:warn-unused"

  let introduce v =
    wur_kind >>=
    Tracker.new_direct v

  let foreach_wur v f =
    wur_kind >>= fun k ->
    Tracker.lookup v Taint.Rel.direct >>|
    Set.to_sequence >>=
    Machine.Seq.iter ~f:(fun t ->
        Object.kind t >>= fun k' ->
        if Taint.Kind.equal k k' then f v t
        else Machine.return ())

  let summarize _ =
    Machine.Global.get state >>| fun {unchecked; callsites} ->
    Map.fold unchecked
      ~f:(fun ~key:_ ~data:site unchecked ->
          Map.set unchecked site (Map.find_exn callsites site))
      ~init:(Map.empty (module Addr)) |>
    Map.to_alist |>
    print_results

  let handle_wur_call (_name,args) =
    let* pc = Eval.pc in
    let* {callsites} as curr = Machine.Global.get state in
    if Map.mem callsites pc then match List.rev args with
      | rval :: _ ->
        introduce rval >>= fun t ->
        Machine.sequence [
          Machine.Global.put state {
            curr with
            unchecked = Map.add_exn curr.unchecked t pc
          };
          Machine.Observation.make introduced t
        ]
      | _ -> Machine.return ()
    else Machine.return ()

  let sanitize v t = Machine.sequence [
      wur_kind >>= Tracker.sanitize v Taint.Rel.direct;
      Machine.Observation.make sanitized t;
      Machine.Global.update state ~f:(fun curr -> {
            curr with
            unchecked = Map.remove curr.unchecked t
          })
    ]

  let handle_external (_name,args) =
    let* pc = Eval.pc in
    let* {externals} = Machine.Global.get state in
    if Map.mem externals pc
    then Machine.List.iter args ~f:(fun arg ->
        foreach_wur arg sanitize)
    else Machine.return ()

  let handle_jump (cnd,_) = foreach_wur cnd sanitize

  let reflect_signals =
    let params = Primus.Lisp.Type.Spec.(one Taint.Object.t) in
    let reflect obs =
      Lisp.signal ~params obs @@ fun v ->
      Object.to_value v >>| fun v -> [v] in
    Machine.sequence [
      reflect warn_unused_introduce;
      reflect warn_unused_sanitize;
    ]

  let init () =
    let* {callsites} = Machine.Global.get state in
    if Map.is_empty callsites
    then !!(print_results [])
    else Machine.sequence [
        Primus.System.stop >>> summarize;
        Primus.Linker.Trace.return >>> handle_wur_call;
        Primus.Linker.Trace.call >>> handle_external;
        Primus.Interpreter.jumping >>> handle_jump;
        reflect_signals;
      ]
end

let enabled = Extension.Configuration.flag "enable"
    ~doc:"Adds the analysis component to the bap:legacy-main system."

let () =
  let open Extension.Syntax in
  Extension.declare
  @@ fun ctxt ->
  Primus.Components.register_generic "warn-unused-tracker"
    ~package:"bap" (module Tracker);
  if ctxt-->enabled
  then Primus.Machine.add_component (module Tracker)
       [@warning "-D"];
  Ok ()
