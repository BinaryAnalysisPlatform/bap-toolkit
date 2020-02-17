
open Core_kernel
open Bap.Std
open Graphlib.Std

include Self ()

module G = Graphlib.Make(Tid)(Unit)

type dest = [
  | `Call of (tid option * tid option) (* target, return *)
  | `Goto of tid
]

type graph = {
  entry  : tid;
  exit_  : tid;
  no_ret : tid;
  itself : G.t;
}

type subroutine = {
  term  : sub term;
  graph : graph option;
  dests : dest list Tid.Map.t;
}

let return_of_call c =
  match Call.return c with
  | Some Direct t -> Some t
  | _ -> None

let dests_of_call c =
  match Call.target c with
  | Direct t -> Some (Some t, return_of_call c)
  | Indirect _ ->
    match return_of_call c with
    | None -> None
    | Some _ as ret -> Some (None, ret)

let dests_of_block b =
  Seq.fold (Blk.elts b) ~init:[] ~f:(fun acc -> function
      | `Def _ | `Phi _ -> acc
      | `Jmp j ->
        match Jmp.kind j with
        | Int _ | Ret _ | Goto (Indirect _) -> acc
        | Goto (Direct t) -> `Goto t :: acc
        | Call c ->
          match dests_of_call c with
          | None -> acc
          | Some d -> `Call d :: acc)

(** [make_no_return g caller callee] removes edge to a
    non-return function with tid [callee] and connect
    caller with an synthetic no-ret node *)
let make_no_return g ~caller ~callee =
  let itself =
    Seq.fold (G.Node.outputs caller g.itself)
      ~init:g.itself
      ~f:(fun g e ->
          if Tid.(G.Edge.dst e = callee)
          then G.Edge.remove e g
          else g) in
  let itself = G.Edge.(insert (create caller g.no_ret ()) itself) in
  { g with itself }

let find_callees dests  =
  List.filter_map dests ~f:(function
      | `Call (Some x,_) -> Some x
      | _ -> None)

(** [update norets sub] updates sub's graph, so
    any edge to function in [norets] will be replaced
    with an edge to synthetic no-return node *)
let update no_rets sub =
  match sub.graph with
  | None -> sub
  | Some g ->
    let g =
      Map.fold sub.dests ~init:g
        ~f:(fun ~key:caller ~data:dests init ->
            List.fold (find_callees dests)
              ~init ~f:(fun g callee ->
                  if Set.mem no_rets callee then
                    make_no_return g caller callee
                  else g)) in
    {sub with graph = Some g;}

let to_graph start dests =
  let insert_dest g node = function
    | `Goto tid ->
      let g = G.Edge.(insert (create node tid ()) g) in
      g, [tid]
    | `Call (tar,ret) ->
      match tar,ret with
      | Some tar, Some ret ->
        let g = G.Edge.(insert (create node tar ()) g) in
        let g = G.Edge.(insert (create tar ret ()) g)  in
        g, [tar; ret]
      | Some tar, None ->
        G.Edge.(insert (create node tar ()) g), [tar]
      | None, None -> g, []
      | None, Some ret ->
        let tar = Tid.create () in
        let g = G.Edge.(insert (create node tar ()) g) in
        let g = G.Edge.(insert (create tar ret ()) g) in
        g, [ret] in
  let rec loop g node =
    let g = G.Node.insert node g in
    match Map.find dests node with
    | None -> g
    | Some dsts ->
      let g',dsts =
        List.fold dsts ~init:(g,[])
          ~f:(fun (g,dsts) dst ->
              let g, dsts' = insert_dest g node dst in
              g, dsts' @ dsts) in
      let dsts = List.filter ~f:(fun n -> not (G.Node.mem n g)) dsts in
      match dsts with
      | [] -> g'
      | dsts -> List.fold dsts ~init:g' ~f:loop in
  loop G.empty start

let collect_destinations sub =
  Term.to_sequence blk_t sub |>
  Seq.fold ~init:(Map.empty (module Tid))
    ~f:(fun (dests) b ->
        Map.set dests (Term.tid b) (dests_of_block b))

let of_sub s =
  let blks = Term.to_sequence blk_t s in
  let dests = collect_destinations s in
  match Seq.hd blks with
  | None -> {term=s; graph=None; dests}
  | Some hd ->
    let no_ret = Tid.create () in
    let exit_ = Tid.create () in
    let g = to_graph (Term.tid hd) dests in
    let entry = Term.tid hd in
    let g =
      G.nodes g |>
      Seq.fold ~init:g ~f:(fun g n ->
          if G.Node.degree ~dir:`Out n g = 0 && Tid.(n <> exit_) then
            G.Edge.(insert (create n exit_ ()) g)
          else g) in
    let g = G.Edge.(insert (create no_ret exit_ ()) g) in
    let graph = {entry;exit_;no_ret;itself=g} in
    {term = s; graph = Some graph; dests}


let map no_returns sub =
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
  end)#map_sub sub

let of_prog prog =
  Term.to_sequence sub_t prog |>
  Seq.fold ~init:([],Set.empty (module Tid))
    ~f:(fun (subs,no_rets) s ->
        let no_rets =
          match Term.get_attr s Sub.noreturn with
          | None -> no_rets
          | Some _ -> Set.add no_rets (Term.tid s) in
        of_sub s :: subs, no_rets)

(** [is_no_return sub] returns true if the no-ret node
    postdominate sub's entry node *)
let is_no_return sub =
  match sub.graph with
  | None -> false
  | Some g ->
    let doms = Graphlib.dominators (module G) ~rev:true g.itself g.exit_ in
    Tree.is_descendant_of doms ~parent:g.no_ret g.entry

let run prog =
  let run_prog subs no_rets =
    List.fold subs ~init:([],no_rets) ~f:(fun (subs, no_rets) s ->
        if Set.mem no_rets (Term.tid s.term) then s :: subs, no_rets
        else
          let s = update no_rets s in
          if is_no_return s then
            s :: subs, Set.add no_rets (Term.tid s.term)
          else s :: subs,no_rets) in
  let rec loop subs no_rets =
    let subs,no_rets' = run_prog subs no_rets in
    if Set.is_empty (Set.diff no_rets' no_rets) then subs,no_rets
    else loop subs no_rets' in
  let subs,no_rets = of_prog prog in
  let subs,no_rets = loop subs no_rets in
  let bld = Program.Builder.create () in
  List.iter subs ~f:(fun s ->
      let s = map no_rets s.term in
      Program.Builder.add_sub bld s);
  Program.Builder.result bld

let main p =
  Project.with_program p (run (Project.program p))

let () =
  Config.when_ready (fun _ -> Project.register_pass ~deps:["api"] main)
