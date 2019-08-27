open Core_kernel
open Bap.Std
open Graphlib.Std

module G = Graphlib.Make(Tid)(Unit)

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
    | Some _ as x -> Some (None, x)

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

let tids_of_dests dests =
  List.fold dests ~init:[]
    ~f:(fun acc -> function
        | `Goto tar -> tar :: acc
        | `Call (tar, ret) ->
          match ret with
          | None -> tar :: acc
          | Some ret -> tar :: ret :: acc)

type graph = {
  entry  : tid;
  exit_  : tid;
  no_ret : tid;
  itself : G.t;
}

type subroutine = {
  term  : sub term;
  graph : graph option;
  calls : tid list Tid.Map.t;  (* caller -> callees *)
}

let make_no_return g caller callee =
  let itself =
    Seq.fold (G.Node.outputs caller g.itself)
      ~init:g.itself
      ~f:(fun g e ->
          if Tid.(G.Edge.dst e = callee) then
            G.Edge.remove e g
          else g) in
  { g with itself =
             G.Edge.(insert (create caller g.no_ret ()) itself) }

let update no_rets sub =
  match sub.graph with
  | None -> sub
  | Some g ->
    let g =
      Map.fold sub.calls ~init:g
        ~f:(fun ~key:caller ~data:callees init ->
            List.fold callees ~init ~f:(fun g callee ->
                if Set.mem no_rets callee then
                  make_no_return g caller callee
                else g)) in
    {sub with graph = Some g;}

let of_sub s =
  let blks = Term.to_sequence blk_t s in
  match Seq.hd blks with
  | None -> {term=s; graph=None; calls = Map.empty (module Tid) }
  | Some hd ->
    let no_ret = Tid.create () in
    let exit_ = Tid.create () in
    let blks,calls =
      Seq.fold blks ~init:(Map.empty (module Tid),Map.empty (module Tid))
        ~f:(fun (blks,calls) b ->
            let dests = dests_of_block b in
            let blks = Map.set blks (Term.tid b) dests in
            let callees =
              List.filter_map dests ~f:(function
                  | `Call (Some x,_) -> Some x
                  | _ -> None) in
            blks, Map.set calls (Term.tid b) callees) in
    let rec loop g node =
      let g = G.Node.insert node g in
      match Map.find blks node with
      | None -> g
      | Some dsts ->
        let g',dsts =
          List.fold dsts ~init:(g,[])
            ~f:(fun (g,dsts) dst ->
                match dst with
                | `Goto tid ->
                  let g = G.Edge.(insert (create node tid ()) g) in
                  g, tid :: dsts
                | `Call (tar,ret) ->
                  match tar,ret with
                  | Some tar, Some ret ->
                    let g = G.Edge.(insert (create node tar ()) g) in
                    let g = G.Edge.(insert (create tar ret ()) g)  in
                    g, tar :: ret :: dsts
                  | Some tar, None ->
                    G.Edge.(insert (create node tar ()) g), tar :: dsts
                  | None, None -> g,dsts
                  | None, Some ret ->
                    let tar = Tid.create () in
                    let g = G.Edge.(insert (create node tar ()) g) in
                    let g = G.Edge.(insert (create tar ret ()) g) in
                    g, ret :: dsts) in
        let dsts = List.filter ~f:(fun n -> not (G.Node.mem n g)) dsts in
        match dsts with
        | [] -> g'
        | dsts -> List.fold dsts ~init:g' ~f:loop in
    let g = loop G.empty (Term.tid hd) in
    let entry = Term.tid hd in
    let g =
      G.nodes g |>
      Seq.fold ~init:g ~f:(fun g n ->
          if G.Node.degree ~dir:`Out n g = 0 && Tid.(n <> exit_) then
            G.Edge.(insert (create n exit_ ()) g)
          else g) in
    let g = G.Edge.(insert (create no_ret exit_ ()) g) in
    let graph = {entry;exit_;no_ret;itself=g} in
    {term = s; graph = Some graph; calls}


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


let is_no_return no_rets sub =
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
          if is_no_return no_rets s then
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

let () = Project.register_pass ~deps:["abi"] main
