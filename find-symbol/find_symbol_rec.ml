open Core_kernel
open Bap.Std
open Regular.Std
open Graphlib.Std

open Find_symbol_types

type signal =
  | Hi
  | Lo
[@@deriving compare, sexp]

let signal_equal s s' = compare_signal s s' = 0

module G = Graphlib.Make(Tid)(Unit)

let dsts_of_blk b =
  Seq.filter_map (Blk.elts b) ~f:(function
      | `Def _ | `Phi _ -> None
      | `Jmp j ->
        match Jmp.kind j with
        | Int _ | Ret _ | Goto (Indirect _) -> None
        | Goto (Direct t) -> Some (t, None)
        | Call c ->
          match Call.target c with
          | Direct t -> Some (t, Call.return c)
          | _ -> None)

let string_of_signal s = Sexp.to_string (sexp_of_signal s)

let to_seq = Term.to_sequence
let subs = Term.to_sequence sub_t
let blks = Term.to_sequence blk_t

type program = {
    g : G.t;
    entries : Tid.Set.t;
    callers : Tid.Set.t;
    entry   : tid;
    names   : (string * addr) Tid.Map.t;
  }

let of_subs subs =
  let program_entry = Tid.create () in
  let empty_prog = {
      g       = G.Node.insert program_entry G.empty;
      entries = Set.empty (module Tid);
      callers = Set.empty (module Tid);
      entry   = program_entry;
      names   = Map.empty (module Tid);
    } in
  let connect g tid tid' =
    let e = G.Edge.create tid tid' () in
    G.Edge.insert e g in
  let connect_with_entry g tid =
    G.Edge.insert (G.Edge.create program_entry tid ()) g in
  Seq.fold subs ~init:empty_prog
      ~f:(fun p sub ->
        let entries = Set.add p.entries (Term.tid sub) in
        let names = match Term.get_attr sub address with
          | None -> p.names
          | Some addr ->
             Map.set p.names (Term.tid sub) (Sub.name sub, addr) in
        let blks = blks sub in
          match Seq.hd blks with
          | None -> { p with entries; names }
          | Some fst ->
            let fst = Term.tid fst in
            let sub = Term.tid sub in
            let g = connect_with_entry p.g sub in
            let g,callers =
              Seq.fold blks ~init:(g,p.callers)
                ~f:(fun acc blk ->
                    let src =
                      if Tid.(Term.tid blk = fst) then sub
                      else Term.tid blk in
                    Seq.fold (dsts_of_blk blk) ~init:acc
                      ~f:(fun (g,callers) (dst,ret) ->
                          match ret with
                          | None | Some (Indirect _) -> connect g src dst, callers
                          | Some Direct dst' ->
                            let g = connect g src dst' in
                            let hi_node = Tid.create () in
                            let g = connect g src hi_node in
                            connect g hi_node dst,
                            Set.add callers hi_node)) in
            { p with entries; names; callers; g })


let meet x y = match x,y with
  | Hi,_ | _,Hi -> Hi
  | x,_ -> x

let raise_signal data =
  Map.map data ~f:(fun signal ->
      match signal with
      | Lo -> Hi
      | x -> x)

let merge data data'=
  Map.merge data data' ~f:(fun ~key:_ -> function
      | `Left x | `Right x -> Some x
      | `Both (x,y) -> Some (meet x y))

let transfer entries callers blk signals =
  let signals =
    if Set.mem entries blk then
      Map.update signals blk
        ~f:(function
            | Some Hi -> Hi
            | _ -> Lo)
    else signals in
  if Set.mem callers blk then raise_signal signals
  else signals

let initial_solution =
  Solution.create (Map.empty (module Tid)) (Map.empty (module Tid))

let calls_of_path entries path =
  let rec collect acc = function
    | [] -> acc
    | e :: edges when Set.mem entries (G.Edge.src e) ->
      collect (G.Edge.src e :: acc) edges
    | _ :: edges  -> collect acc edges in
  collect [] (Seq.to_list (Path.edges path))

let find_gen ~init ~f prog =
  let subs = subs prog in
  let p = of_subs subs in
  let sol = Graphlib.fixpoint (module G) p.g
      ~start:p.entry
      ~equal:(Tid.Map.equal signal_equal)
      ~init:initial_solution
      ~merge
      ~f:(fun blk data -> transfer p.entries p.callers blk data) in
  Set.fold p.entries ~init ~f:(fun acc blk  ->
      let x = Solution.get sol blk in
      match Map.find x blk with
      | None | Some Lo -> acc
      | Some Hi -> f acc p blk)

let find prog =
  find_gen prog ~init:[] ~f:(fun acc p blk ->
      match Map.find p.names blk with
      | Some (name,addr) -> {name;addr} :: acc
      | None -> acc)

(** [find' prog] returns a list of tids of recursive subroutines
    and a recursive path for each one. *)
let find' prog =
  find_gen prog ~init:[] ~f:(fun acc p blk ->
        let inputs = G.Node.inputs blk p.g in
        let path =
          Seq.find_map inputs
            ~f:(fun inp ->
                if Tid.(p.entry = G.Edge.src inp) then None
                else
                  Graphlib.shortest_path (module G)
                    p.g ~rev:true (G.Edge.src inp) blk) in
        match path with
        | None -> acc
        | Some path -> (blk, calls_of_path p.entries path) :: acc)
