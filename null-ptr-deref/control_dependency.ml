open Core_kernel
open Bap.Std
open Bap_primus.Std
open Bap_taint.Std
open Graphlib.Std

module Edges = Hashtbl.Make(struct
    type t = addr * addr [@@deriving compare, sexp]
    let hash (x,y) = Hashtbl.hash (Addr.hash x, Addr.hash y)
  end)

type static = {
  pdoms : addr tree Addr.Map.t;
  cdoms : bool Edges.t;
  cdeps : bool Edges.t;
}

module Static = struct
  module Cfg = Graphs.Cfg
  module G = Graphlib.Make(Addr)(Unit)
  let entry = G.Node.create Word.b1
  let exit = G.Node.create Word.b0

  let exit_edge n = G.Edge.create (Block.addr n) exit ()
  let entry_edge n = G.Edge.create entry (Block.addr n) ()
  let init = G.Node.insert exit (G.Node.insert entry G.empty)

  let graph_of_fn start cfg =
    Graphlib.depth_first_search (module Cfg) cfg
      ~init ~start
      ~start_tree:(fun n g -> G.Edge.insert (entry_edge n) g)
      ~enter_node:(fun _ n g ->
          if Cfg.Node.degree ~dir:`Out n cfg = 0
          then G.Edge.insert (exit_edge n) g
          else g)
      ~enter_edge:(fun _ e g ->
          let s = Cfg.Edge.src e and d = Cfg.Edge.dst e in
          let e = G.Edge.create (Block.addr s) (Block.addr d) () in
          G.Edge.insert e g)

  let create proj =
    Project.symbols proj |>
    Symtab.to_sequence |>
    Seq.fold ~init:{
      pdoms = Addr.Map.empty;
      cdoms = Edges.create ();
      cdeps = Edges.create ();
    }
      ~f:(fun ({pdoms} as s) (name,entry,cfg) ->
          let g = graph_of_fn entry cfg in
          let pdom = Graphlib.dominators (module G)
              ~rev:true g exit in
          {s with pdoms =
                    Cfg.nodes cfg |>
                    Seq.fold ~init:pdoms ~f:(fun pdoms b ->
                        Map.set pdoms ~key:(Block.addr b) ~data:pdom);

          })
end

let static = Primus.Machine.State.declare
    ~uuid:"143f7365-fe4f-4d05-a2e1-26910f92bd29"
    ~name:"static"
    Static.create

type tpoint = {
  jmps : Addr.Set.t;
  blk : addr;
}

type state = {
  trace : tpoint list;
}

let state = Primus.Machine.State.declare
    ~uuid:"a7da7825-9446-4af9-b9d4-fba0e55cd09d"
    ~name:"trace"
    (fun _ -> { trace = []; })

let dominates dom blk cnd = Tree.is_descendant_of dom ~parent:blk cnd

let find_pdom pdoms x y =
  match Map.find pdoms x, Map.find pdoms y with
  | Some x, Some y -> if phys_equal x y then Some x else None
  | _ -> None

(* given the backtrace [trace] determine that the basic
   block [blk] has a control dependency on a basic block
   [x] that contains the specified [cond]. *)
let has_control_dependency {pdoms; cdoms; cdeps} trace blk ~on:cond =
  let not_post_dominates pdom blk cnd =
    match Hashtbl.find cdoms (cnd,blk) with
    | Some answer -> not answer
    | None ->
      let answer = dominates pdom blk cnd in
      Hashtbl.set cdoms (cnd,blk) answer;
      not answer in
  match Hashtbl.find cdeps (cond,blk) with
  | Some answer ->
     answer
  | None ->
    let depends =
      List.find trace ~f:(fun {jmps} -> Set.mem jmps cond) |> function
      | None -> true
      | Some {blk=cnd} -> match find_pdom pdoms cnd blk with
        | Some pdom -> not_post_dominates pdom blk cnd
        | None ->
          List.find_map trace ~f:(fun {blk} ->
              match find_pdom pdoms cnd blk with
              | Some pdom -> Some (not_post_dominates pdom blk cnd)
              | None -> None) |> function
          | None -> false
          | Some r -> r in
    Hashtbl.set cdeps (cond,blk) depends;
    depends


module Trace(Machine : Primus.Machine.S) = struct
  open Machine.Syntax

  let update_trace blk =
    match Term.get_attr blk address with
    | None -> Machine.return ()
    | Some addr ->
       let jmps =
         Term.enum jmp_t blk |>
           Seq.filter_map ~f:(fun t -> Term.get_attr t address) |>
           Seq.to_list |> Addr.Set.of_list in
       Machine.Local.update state ~f:(fun s ->
           {trace = {jmps; blk = addr} :: s.trace})

  let init () =
    Machine.sequence Primus.Interpreter.[
        enter_blk >>> update_trace;
    ]

end

module Lisp(Machine : Primus.Machine.S) = struct
  open Machine.Syntax
  module Eval = Primus.Interpreter.Make(Machine)
  module Value = Primus.Value.Make(Machine)

  let blk_addr pos =
    let open Primus.Pos in match pos with
    | Jmp {up={me}} | Def {up={me}} -> Term.get_attr me address
    | _ -> None

  [@@@warning "-P"]
  let run [on] =
    let on = Primus.Value.to_word on in
    Eval.pos >>= fun p -> match blk_addr p with
    | Some addr ->
      Machine.Local.get state >>= fun {trace} ->
      Machine.Global.get static >>= fun pdoms ->
      Value.of_bool
        (has_control_dependency pdoms trace addr ~on)
    | None -> Value.b1

end
