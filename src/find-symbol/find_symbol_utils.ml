open Core_kernel
open Graphlib.Std
open Bap.Std

module Cfg = Graphs.Cfg
module Callgraph = Graphs.Callgraph

let connect_graph g entry =
  Cfg.nodes g |> Seq.fold ~init:g ~f:(fun g n ->
      if Cfg.Node.degree ~dir:`Out n g = 0
      then Cfg.Edge.insert (Cfg.Edge.create n entry `Jump) g
      else g)

let complexity graph entry =
  let graph = connect_graph graph entry in
  let edges = Seq.length (Cfg.edges graph) in
  let nodes = Seq.length (Cfg.nodes graph) in
  let parts = Graphlib.strong_components (module Cfg) graph |>
              Partition.number_of_groups in
  edges - nodes + parts

let scc_entries cfg group =
  let nodes = Group.enum group |>
                Seq.to_list |>
                Set.of_list (module Cfg.Node) in
  Set.fold nodes ~init:0 ~f:(fun entries node ->
      let inputs = Cfg.Node.inputs node cfg in
      let srcs = Seq.map inputs ~f:Cfg.Edge.src in
      let srcs = Seq.filter srcs ~f:(fun s -> not (Set.mem nodes s)) in
      entries + Seq.length srcs)

(* A flowgraph is reducible when it does NOT have
   a strongly connected subgraph with two (or more) entries. *)
let is_reducible cfg start =
  let cfg = connect_graph cfg start in
  let xs = Graphlib.strong_components (module Cfg) cfg in
  let groups = Partition.groups xs in
  not @@ Seq.exists groups ~f:(fun g -> scc_entries cfg g >= 2)


let find_recursive prog =
  Graphlib.depth_first_search
    (module Callgraph)
    ~init:(Set.empty (module Tid))
    ~enter_edge:(fun kind edge recs ->
      match kind with
      | `Back -> Set.add recs (Callgraph.Edge.dst edge)
      | _ -> recs) (Program.to_graph prog) |>
    Set.to_list |>
    List.filter_map ~f:(Term.find sub_t prog)
