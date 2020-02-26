
(**
   This pass removes 'return' edge from all the calls to the non-return
   functions.

   Algorithm

   We start from the initial set of non-return functions, such ones
   that either have GNU noreturn attribute OR have an edge between
   start and exit nodes in the Tid graph. For instance, the latter can
   happen when a loop without exit exists in a graph of a subroutine.

   Then, for each subroutine in the program, we remove such edges in the
   Tid graph of the subroutine that correspond to calls to
   non-return functions.

   By doing so, we can introduce unreachable blocks. And to be sure
   that they are trully unreachable, we need to prove that they can't
   be reached by indirect calls. We add a synthetic node, and connect
   each block with indirect call with this node and also connect this
   node with the exit.

   Now, the decision if the function is non-return one depends on
   the reachability of the exit node from the start. If it can't
   be reached then such subroutine is a non-return and we need to add
   it in the set of nonreturn subroutines and apply algorithm
   again to the all subroutines. *)


open Core_kernel
open Bap.Std
open Graphlib.Std

include Self ()

module G = Graphs.Tid

type call_edge = {
  src   : tid;
  label : tid;
}

type subroutine = {
  tid   : tid;
  graph : G.t;
  indirect : tid;
  callees : call_edge list Tid.Map.t;
}

let is_resolved j =
  List.filter_map ~f:ident [Jmp.dst j; Jmp.alt j] |>
  List.exists ~f:(fun d -> Either.is_first (Jmp.resolve d))

let is_indirect j = not (is_resolved j)

let indirects_of_blk b =
  Term.to_sequence jmp_t b |> Seq.filter ~f:is_indirect

let connect_indirects g node blk =
  Seq.fold (indirects_of_blk blk) ~init:g
    ~f:(fun g j ->
        let e = G.Edge.create (Term.tid blk) node (Term.tid j) in
        G.Edge.insert e g)

let calls_of_blk calls b =
  Term.to_sequence jmp_t b |>
  Seq.fold ~init:calls ~f:(fun acc j ->
      match Jmp.alt j with
      | None -> acc
      | Some callee -> match Jmp.resolve callee with
        | First callee ->
          Map.add_multi acc callee { src = Term.tid b; label = Term.tid j; }
        | _ -> acc)

let callees_of_sub sub =
  Term.to_sequence blk_t sub |>
  Seq.fold ~init:(Map.empty (module Tid)) ~f:calls_of_blk

let of_sub sub =
  let blks = Term.to_sequence blk_t sub in
  if Seq.is_empty blks then None
  else
    let indirect = Tid.create () in
    let graph = Sub.to_graph sub in
    let graph = G.Edge.(insert (create indirect G.exit indirect) graph) in
    let graph =
      Seq.fold blks ~init:graph
        ~f:(fun g b -> connect_indirects g indirect b) in
    let callees = callees_of_sub sub in
    Some {tid = Term.tid sub; graph; indirect; callees}

let prune_edge_to_nonreturn g {src; label} =
  G.Node.outputs src g |>
  Seq.fold ~init:g ~f:(fun g e ->
      if Tid.equal (G.Edge.label e) label
      then G.Edge.remove e g
      else g)

let update s no_ret =
  let is_unreachable b g = G.Node.degree ~dir:`Out b g = 0 in
  match Map.find s.callees no_ret with
  | None -> s
  | Some edges -> {
      s with
      graph =
        List.fold edges ~init:s.graph
          ~f:(fun g edge ->
              let dsts =
                G.Node.outputs edge.src g |> Seq.map ~f:G.Edge.dst in
              let g = prune_edge_to_nonreturn g edge in
              Seq.fold dsts ~init:g
                ~f:(fun g blk ->
                    if is_unreachable blk g then
                      G.Edge.(insert (create blk s.indirect s.indirect) g)
                    else g))
    }

let update s no_rets = Set.fold no_rets ~init:s ~f:update

let apply no_returns prog =
  (object
    inherit Term.mapper

    method! map_jmp j =
      match Jmp.kind j with
      | Goto _ | Ret _ | Int _ -> j
      | Call c ->
        match Call.target c with
        | Indirect _ -> j
        | Direct tid ->
          if Set.mem no_returns tid then
            Jmp.with_kind j (Call (Call.with_noreturn c))
          else j
  end)#run prog

let is_known_noreturn s g =
  Term.has_attr s Sub.noreturn ||
  G.Node.has_edge G.start G.exit g

let of_prog prog =
  Term.to_sequence sub_t prog |>
  Seq.fold ~init:([],Set.empty (module Tid))
    ~f:(fun (subs,no_rets) s -> match of_sub s with
        | None -> subs, no_rets
        | Some s' ->
          let no_rets =
            if is_known_noreturn s s'.graph then
              Set.add no_rets s'.tid
            else no_rets in
          s' :: subs, no_rets)

let is_no_return sub =
  not (Graphlib.is_reachable (module G) sub.graph G.start G.exit)

let run prog =
  let update_prog subs no_rets =
    List.fold subs ~init:([],no_rets) ~f:(fun (subs, no_rets) s ->
        let s = update s no_rets in
        let no_rets =
          if is_no_return s then
            Set.add no_rets s.tid
          else no_rets in
        s :: subs, no_rets) in
  let rec loop subs no_rets =
    let subs,no_rets' = update_prog subs no_rets in
    if Set.is_empty (Set.diff no_rets' no_rets) then no_rets
    else loop subs no_rets' in
  let subs,no_rets = of_prog prog in
  let no_rets = loop subs no_rets in
  apply no_rets prog

let main p = Project.with_program p (run (Project.program p))

let () =
  Config.when_ready (fun _ -> Project.register_pass ~deps:["api"] main)
