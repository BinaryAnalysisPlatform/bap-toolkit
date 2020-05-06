open Core_kernel
open Bap.Std
open Bap_primus.Std
open Bap_taint.Std
open Bap_future.Std
open Graphlib.Std
open Format
include Self()

module Param = struct
  open Config
  let enable = flag "enable" ~doc:{| enables the analysis |}

  let max_length = param int "length" ~default:100 ~doc:{|
      The length of the speculative execution window. Currently,
      it is specified in the number of machine instructions.
    |}

  let untrust_inputs = flag "untrust-inputs" ~doc:{|
      By default all undefined variables and memory locations are
      tainted as untrusted as they denote free variables in a program,
      which we consider to be arguments. Sometimes it may lead to
      overtainting.
    |}

  let all_models = flag "all-models" ~doc:{|
      Only one model per each taint/condition/load1/load2 is shown
      by default. Enabling this option will generate all models, that
      can be a huge number.
    |}

  let no_deps = flag "ignore-control-dependencies" ~doc:{|
      We require the first load to be control/program dependent on
      the condition. If this option is specified, we no longer track
      control dependencies. Note, this option implies
      $(b,--ignore-program-dependencies).
  |}

  let no_prog = flag "ignore-program-dependencies" ~doc:{|
      Use more strict control dependency relation instead of
      program dependency between a condition and the first load.
  |}

  let control_as_data = flag "allow-control-dependency-for-load2" ~doc:{|
      When enabled, the control dependency of the second load on the
      first load could be used to satisfy the rule, if there is no
      direct data dependency. This is needed for detecting code patters such
      as test case 10 from Kocher's suite. This option requires that
      control dependency tracking is not disabled.
  |}
  ;;

  manpage [
    `S "DESCRIPTION";
    `P "Detects patterns of code that are vulnerable to the Spectre-1
    attack. The detector tracks data and control dependencies
    between program points and reports models that satisfy the
    following judgement";

    `Pre {|
      <L1>: when <c> branch _
      <L2>: load <i> -> <j>
      <L3>: load <j> -> _ \\/ store <j>
      distance (<L1>,<L2>) < SEW
      distance (<L1>,<L3>) < SEW
      <c> is-data-dependent-on <untrusted-input>
      <i> is-data-dependent-on <untrusted-input>
      <j> is-data-dependent-on <i> \/ <j> is-ctrl-dependent-on <i>
      <i> is-program-dependent-on <c>
      -------------------------------------------
      spectre-pattern (L1, L2, L3)
    |};


    `P "The `x is-program-dependent-on y` relation means that there is
   a path in a PDG between x and y no matter the kind of edges. Where
   PDG (program dependency graph) is a graph between program points
   with two kind of edges - data dependency edge and control
   dependency edge.";

    `P
      "If $(b,<j> is-ctrl-dependent-on <i>) clause is disabled by
      default, and is controlled with the $(b,--ddtbd-ctrl-as-data) option";

    `P "
   The program dependency is implemented by propagating the taint from
   a matching condition to all values that are defined in blocks that
   are not post-dominated by this condition (by the block in which
   this condition is used for branching)."
  ]
end
open Param


type point = Addr.t

let sexp_of_point p = Sexp.Atom (Addr.string_of_value p)

type taint = Taint.Object.t

type step1 = S1 of {
    cond : point;
    taint : taint sexp_opaque;
  } [@@deriving sexp_of]

(* we found a load from a tainted offset,
   and tainted the result with the specified taint
*)
type step2 = S2 of {
    taint : taint sexp_opaque;
    cond  : point;
    load  : point;
  } [@@deriving sexp_of]

(* we found all three ingredients,
   we can report and discard it *)
type step3 = S3 of {
    cond : point;
    load : point;
    last : point;
  } [@@deriving sexp_of]

type 'a hypot = {
  ttl : int;
  cnd_taint : Taint.Object.Set.t;
  off_taint : Taint.Object.Set.t;
  state : 'a;
} [@@deriving sexp_of]

module Edges = Hashtbl.Make(struct
    type t = addr * addr [@@deriving compare, sexp]
    let hash (x,y) = Hashtbl.hash (Addr.hash x, Addr.hash y)
  end)

type static = {
  pdoms : addr tree Addr.Map.t;
  cdoms : bool Edges.t;
  cdeps : bool Edges.t;
}

type tpoint = {
  jmps : Addr.Set.t;
  blk : addr;
}

type t = {
  step1 : step1 hypot list;
  step2 : step2 hypot list;
  trace : tpoint list;
}

type models = {
  srcs : Taint.Object.Set.t;
  locs : Addr.Set.t;
}

let state = Primus.Machine.State.declare
    ~uuid:"10d552e4-9c4d-40ce-8bfe-50faed839dcf"
    ~name:"ddtbd"
    (fun _ -> {
         step1 = [];
         step2 = [];
         trace = [];
       })

let models = Primus.Machine.State.declare
    ~uuid:"72298de9-f408-4300-88a5-97ba28a8f590"
    ~name:"ddtbd-models"
    (fun _ -> {
         srcs = Taint.Object.Set.empty;
         locs = Addr.Set.empty;
       })


let get par =
  Future.peek_exn (Config.determined par)

let sexp_of_hypot state h : Sexp.t = Sexp.List [
    sexp_of_int (get max_length - h.ttl);
    state h.state;
  ]

let detected,was_detected = Primus.Observation.provide
    ~inspect:([%sexp_of : Primus.value * step3 hypot])
    "spectre-path"

let partial,detected_partial = Primus.Observation.provide
    ~inspect:(sexp_of_hypot sexp_of_step2)
    "spectre-hypot-partial"

let hypot_init,was_inited = Primus.Observation.provide
    ~inspect:(sexp_of_hypot sexp_of_step1)
    "spectre-hypot-init"

let dominates dom blk cnd = Tree.is_descendant_of dom ~parent:blk cnd

let find_pdom pdoms x y =
  match Map.find pdoms x, Map.find pdoms y with
  | Some x, Some y -> if phys_equal x y then Some x else None
  | _ -> None

(* given the backtrace [trace] determine that the basic
   block [blk] has a control dependency on a basic block
   [x] that contains the specified [cond].

*)
let has_control_dependency {pdoms; cdoms; cdeps} trace blk ~on:cond =
  let not_post_dominates pdom blk cnd =
    match Hashtbl.find cdoms (cnd,blk) with
    | Some answer -> not answer
    | None ->
      let answer = dominates pdom blk cnd in
      Hashtbl.set cdoms (cnd,blk) answer;
      not answer in
  match Hashtbl.find cdeps (cond,blk) with
  | Some answer -> answer
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

let step2 has_dep pos ts t ({state = S1 {cond}} as h) =
  if has_dep h ts then Some {
      h with
      state = S2 {
          cond;
          load = pos;
          taint = t;
        };
      off_taint = ts;
    } else None

let step3 has_dep pos ts ({state = S2 {cond; taint; load}} as h) =
  if Addr.(pos = load) then None else
  if Set.mem ts taint || get control_as_data && has_dep load
  then Some {
      h with
      state = S3 {
          cond;
          load;
          last = pos;
        }
    } else None

(*   This implementation has complexity mlog(n) s.t., m <= n, where m,n
 *   are cardinalities of the input sets. An alternative implementation
 *   [not (Set.is_empty (Set.intersection s1 s2))] will have some
 *   overhead due to allocations but will have a complexity
 *   mlog(1+n/m) s.t. m <= n, which is better only if cardinality of
 *   the bigger set is an exponent of the cardinality of the smaller
 *   set. Since this will matter only for big `m` and we're not
 *   expecting that any of the sets will be 2^m, s.t. m >> 1 (in that
 *   case we are in a much more troubles already), this implementation
 *   is more efficient. (note - ideally this operation should be
 *   provided by the core library).
*)
let has_intersection x y =
  let x,y = if Set.length x < Set.length y then x,y else y,x in
  Set.exists x ~f:(Set.mem y)

let is_detected m h cs =
  List.exists cs ~f:(Set.mem m.locs) ||
  has_intersection m.srcs h.cnd_taint ||
  has_intersection m.srcs h.off_taint

let is_live s h c = not (is_detected s h c)
let is_live1 s ({state = S1 {cond}} as h) =
  is_live s h [cond]
let is_live2 s ({state = S2 {cond;load}} as h) =
  is_live s h [cond; load]
let is_live3 s ({state = S3 {cond;load;last}} as h) =
  is_live s h [cond; load; last]

let discard_ready m s = {
  s with
  step1 = List.rev_filter ~f:(is_live1 m) s.step1;
  step2 = List.rev_filter ~f:(is_live2 m) s.step2;
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
                        Map.add_exn pdoms ~key:(Block.addr b) ~data:pdom);

          })
end

let static = Primus.Machine.State.declare
    ~uuid:"b012f01c-76a9-4ba3-8d9e-71b3cc3de7e0"
    ~name:"ddtbd-static"
    Static.create


module Detector(Machine : Primus.Machine.S) = struct
  open Machine.Syntax
  module Tracker = Taint.Tracker.Make(Machine)
  module Eval = Primus.Interpreter.Make(Machine)
  module Kind = Taint.Kind.Make(Machine)
  module Object = Taint.Object.Make(Machine)
  module Lisp = Primus.Lisp.Make(Machine)
  module Value = Primus.Value.Make(Machine)

  let untrusted_offset = "spectre-untrusted-offset"
  let untrusted_branch = "spectre-untrusted-branch"

  let is_untrusted t =
    Object.kind t >>= Kind.name >>| fun s ->
    s = "untrusted" ||
    s = untrusted_offset ||
    s = untrusted_branch

  let get_untrusted v =
    Tracker.lookup v Taint.Rel.direct >>= fun td ->
    Set.to_sequence td |>
    Machine.Seq.fold ~init:Taint.Object.Set.empty ~f:(fun ts t ->
        is_untrusted t >>| function
        | true -> Set.add ts t
        | false -> ts)

  let init_hypot v =
    get_untrusted v >>= fun td ->
    if Set.is_empty td
    then Machine.return ()
    else Eval.pc >>= fun pos ->
      Kind.create untrusted_offset >>= fun k ->
      Object.create k >>= fun taint ->
      let hypot = {
        ttl = get max_length;
        cnd_taint = td;
        off_taint = Taint.Object.Set.empty;
        state = S1 {
            cond = pos;
            taint;
          }
      } in
      Machine.Observation.make was_inited hypot >>= fun () ->
      Machine.Local.update state ~f:(fun s ->
          {s with step1 = hypot :: s.step1})

  let on_jmp (_,dst) = init_hypot dst
  let on_cnd cnd = init_hypot cnd


  let update_trace blk =
    if get no_deps then Machine.return ()
    else match Term.get_attr blk address with
      | None ->
        warning "ignoring jumping at pos %s" (Term.name blk);
        Machine.return ()
      | Some addr ->
        let jmps =
          Term.enum jmp_t blk |>
          Seq.filter_map ~f:(fun t -> Term.get_attr t address) |>
          Seq.to_list |> Addr.Set.of_list in
        Machine.Local.update state ~f:(fun s ->
            {s with trace = {jmps; blk = addr} :: s.trace})

  let blk_addr pos =
    let open Primus.Pos in match pos with
    | Jmp {up={me}} | Def {up={me}} -> Term.get_attr me address
    | _ -> None

  let control_dependency =
    Eval.pos >>= fun p -> match blk_addr p with
    | Some addr ->
      Machine.Local.get state >>= fun {trace} ->
      Machine.Global.get static >>| fun pdoms ->
      fun on -> has_control_dependency pdoms trace addr ~on
    | None ->
      warning "can't establish a control dep for %s"
        (Primus.Pos.to_string p);
      Machine.return (fun _ -> true)

  (* If program dependency is enabled, then we turn control
     dependency into data dependency by propagating the taint
     from the condition to any definition that is in the
     transitive closure of its control dependence. Thus the
     program dependence is deteriorated to a simple check that
     the value is data dependent on a value that is program
     dependent on the condition.
  *)
  let program_dependency =
    if get no_prog (* then fall back to control dependency *)
    then control_dependency >>| fun has_cd ->
      fun {state = S1 {cond}} _ -> has_cd cond
    else Machine.return @@ fun {state = S1 {taint}} ts ->
      Set.mem ts taint

  (* if we found a tainted load,
     we promote all live step1 hypotheses to step2
  *)
  let promote_to_step2 pos ts v =
    Machine.Local.get state >>= fun s -> match s.step1 with
    | [] -> Machine.return ()
    | step1 ->
      Kind.create untrusted_offset >>= fun k ->
      Object.create k >>= fun t ->
      Taint.Object.Set.singleton t |>
      Tracker.attach v Taint.Rel.direct >>= fun () ->
      program_dependency >>= fun dep ->
      let s2 = List.rev_filter_map ~f:(step2 dep pos ts t) step1 in
      Machine.List.iter s2 ~f:(fun h2 ->
          Machine.Observation.make detected_partial h2) >>= fun () ->
      Machine.Local.put state {
        s with
        step2 = List.rev_append s2 s.step2;
      }

  let promote_all f xs = List.rev_filter_map xs ~f

  let promote_fst promote xs =
    snd @@ List.fold xs ~init:(Addr.Set.empty,[])
      ~f:(fun (promoted,hs) h -> match promote h with
          | Some ({state=S3 {cond}} as h) ->
            if Set.mem promoted cond then (promoted,hs)
            else ((Set.add promoted cond),h::hs)
          | None -> (promoted,hs))

  let promote_fst f xs = List.find_map xs ~f |> function
    | None -> []
    | Some x -> [x]

  let promote_to_step3 pos ts =
    Machine.Local.get state >>= fun s ->
    Machine.Global.get models >>= fun m ->
    control_dependency >>= fun has_dep ->
    let promote = if get all_models then promote_all else promote_fst in
    let step3 = promote (step3 has_dep pos ts) s.step2 in
    let locs =
      List.fold step3 ~init:m.locs ~f:(fun cs {state=S3 {cond}} ->
          List.fold [cond] ~init:cs ~f:Set.add) in
    let m = {m with locs} in
    let s = if get all_models then s else discard_ready m s in
    Machine.Global.put models m >>= fun () ->
    Machine.Local.put state s >>= fun () ->
    Machine.List.iter step3 ~f:(fun h3 ->
        let taints = Set.union h3.cnd_taint h3.off_taint |>
                     Set.to_sequence in
        let taints = if get all_models then taints
          else Seq.take taints 1 in
        Machine.Seq.iter taints ~f:(fun t ->
            Object.to_value t >>= fun v ->
            Machine.Observation.make was_detected (v,h3)))

  let access ~is_loaded (addr,value) =
    Eval.pc >>= fun pos ->
    get_untrusted addr >>= fun ts ->
    if is_loaded
    then
      promote_to_step2 pos ts value >>= fun () ->
      promote_to_step3 pos ts
    else
      promote_to_step3 pos ts

  let age =
    List.rev_filter_map ~f:(fun h ->
        if h.ttl > 0 then Some {h with ttl = h.ttl - 1} else None)

  let decay pc =
    Machine.Local.update state ~f:(fun s -> {
          s with
          step1 = age s.step1;
          step2 = age s.step2;
        })

  let reflect_observations = Machine.sequence [
      Lisp.signal detected  (fun (t,{state=S3 {cond; load; last}}) ->
          Value.of_word cond >>= fun br ->
          Value.of_word load >>= fun l1 ->
          Value.of_word last >>= fun l2 ->
          Machine.return [t; br; l1; l2]);
      Lisp.signal hypot_init (fun {state=S1 {cond}} ->
          Value.of_word cond >>= fun c ->
          Machine.return [c]);
      Lisp.signal partial (fun {state=S2 {load}} ->
          Value.of_word load >>= fun l ->
          Machine.return [l]);
    ]

  let taint_program_depenency (v,x) =
    control_dependency >>= fun has_dep ->
    Machine.Local.get state >>= fun {step1=h1} ->
    Machine.List.iter h1 ~f:(fun {state=S1 {cond; taint}} ->
        if has_dep cond
        then
          Tracker.lookup x Taint.Rel.direct >>= fun ts ->
          Tracker.attach x Taint.Rel.direct (Set.add ts taint)
        else Machine.return ())

  let enable_program_dependency_tracking =
    if get no_deps || get no_prog
    then Machine.return ()
    else Machine.sequence Primus.Interpreter.[
        written >>> taint_program_depenency;
      ]

  let init () = Machine.sequence Primus.Interpreter.[
      reflect_observations;
      eval_cond >>> on_cnd;
      pc_change >>> decay;
      jumping >>> on_jmp;
      enter_blk >>> update_trace;
      loaded >>> access ~is_loaded:true;
      stored >>> access ~is_loaded:false;
      enable_program_dependency_tracking;
    ]
end


type defined = {
  vars : Var.Set.t;
  ptrs : Addr.Set.t;
  static : value Memmap.t;
}

let trusted_environment proj =
  let arch = Project.arch proj in
  let size = Arch.addr_size arch |> Size.in_bits in
  let module Target = (val target_of_arch arch) in
  let var n = Var.create n (Type.imm size) in
  Var.Set.of_list [
    Target.CPU.sp;
    var "endp";
    var "brk";
    var "environ";
    var "FS_BASE";
    var "GS_BASE";
    var "*standard-output*";
    var "*standard-input*";
    var "*error-output*";
    var "*standard-error*";

  ]

let state = Primus.Machine.State.declare
    ~uuid:"c757b77a-ed7d-444e-a41f-85b3a0d2f22c"
    ~name:"input-detector"
    (fun p ->  {
         vars = trusted_environment p;
         ptrs = Addr.Set.empty;
         static = Project.memory p;
       })

let undefined_input,detected_input =
  Primus.Observation.provide
    ~inspect:Primus.Value.sexp_of_t
    "undefined-input"

let undefined_var,detected_var =
  Primus.Observation.provide
    ~inspect:Var.sexp_of_t
    "undefined-var"

let undefined_ptr,detected_ptr =
  Primus.Observation.provide
    ~inspect:(fun v -> Sexp.Atom (asprintf "%a" Addr.pp_hex v))
    "undefined-ptr"

module InputDetector(Machine : Primus.Machine.S) = struct
  open Machine.Syntax

  let define_ptr p =
    Machine.Local.update state ~f:(fun s -> {
          s with ptrs = Set.add s.ptrs (Primus.Value.to_word p)
        })

  let define_var v =
    Machine.Local.update state ~f:(fun s -> {
          s with vars = Set.add s.vars v;
        })

  let check defined ref v detected =
    if Set.mem defined ref
    then Machine.return ()
    else
      Machine.Observation.make detected ref >>= fun () ->
      Machine.Observation.make detected_input v

  let check_ptr (ptr,v) =
    Machine.Local.get state >>= fun {ptrs; static} ->
    let ptr = Primus.Value.to_word ptr in
    if Memmap.contains static ptr then Machine.return ()
    else check ptrs ptr v detected_ptr

  let check_var (var,v) =
    Machine.Local.get state >>= fun {vars} ->
    check vars var v detected_var

  let init () = Machine.sequence Primus.Interpreter.[
      storing >>> define_ptr;
      writing >>> define_var;
      loaded >>> check_ptr;
      read >>> check_var;
    ]
end

module Primitives (Machine : Primus.Machine.S) = struct
  module Lisp = Primus.Lisp.Make(Machine)
  let init () =
    Lisp.signal undefined_input
      ~doc:"Occurs when an undefined datum is introduced"
      ~params:Primus.Lisp.Type.Spec.(one int)
    @@ fun v -> Machine.return [v]
end

let () = Config.when_ready (fun {Config.get} ->
    if get enable then
      begin
        Primus.Machine.add_component (module Primitives);
        Primus.Machine.add_component (module Detector);
        if get untrust_inputs
        then Primus.Machine.add_component (module InputDetector)
      end)
[@@warning "-D"]
