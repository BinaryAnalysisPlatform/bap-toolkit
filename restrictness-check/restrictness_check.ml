(** Restrictness check

    The Algorithm

    For any pointer with the 'restrict' type qualifier,
    we check if there is another pointer among function
    arguments, which belongs to the same memory location in
    a heap or even exactly equal to the first one.  *)

open Core_kernel
open Bap.Std
open Bap_primus.Std
open Bap_main
open Bap_c.Std

let check_name = "restrictness check"

let is_restricted arg = match Term.get_attr arg C.t with
  | Some (`Pointer q) -> C.Type.Spec.(q.qualifier.restrict)
  | _ -> false

let state = Primus.Machine.State.declare
    ~name:"restrictness-check-results"
    ~uuid:"ab02bbc9-5573-4bdf-a010-605dc6851b9d"
    (fun _ -> [])

let print_results rs =
  let pp_bold ppf = Format.fprintf ppf "\027[1m" in
  let pp_norm ppf = Format.fprintf ppf "\027[0m" in
  match rs with
  | [] -> Format.printf "%s   OK\n" check_name
  | rs ->
    Format.printf "%s   FAIL\n\n" check_name;
    Format.printf "%t%-10s %-20s %s\n%t" pp_bold "Address" "Function"
      "Aliased arguments" pp_norm;
    let reported = Map.empty (module Addr) in
    Caml.ignore @@
    List.fold rs ~init:reported ~f:(fun reported (addr,sub,aliases) ->
        let args = List.sort aliases ~compare:String.compare in
        let args = String.concat args ~sep:"," in
        match Map.find reported addr with
        | Some a when Set.mem a args -> reported
        | a ->
          let a = Option.value ~default:(Set.empty (module String)) a in
          let reported = Map.set reported addr (Set.add a args) in
          let addr = sprintf "%a" Addr.pps addr in
          Format.printf "%-10s %-20s %s\n" addr sub args;
          reported)

module Regions = struct
  module I = Interval_tree.Make(struct
      type point = Primus.Value.t [@@deriving compare, sexp]
      type t = point * point [@@deriving compare, sexp]
      let lower = fst
      let upper = snd
    end)

  type t = unit I.t

  let empty = I.empty
  let alloc t start finish = I.add t (start, finish) ()
  let find t addr = I.lookup t addr |> Seq.map ~f:fst |> Seq.hd

  let release t ptr = match find t ptr with
    | None -> t
    | Some key -> I.remove t key

  (** [find_origin t addr]
      For heap bases pointers returns a beginning of the
      allocated region. For stack pointers - a pointer
      itself *)
  let find_origin t addr = match find t addr with
    | None -> addr
    | Some key -> fst key

  (** [is_shared t addr addr'] returns true if addr and addr' share the same region *)
  let is_shared t addr addr' =
    Primus.Value.equal (find_origin t addr) (find_origin t addr')

end

let regions = Primus.Machine.State.declare
    ~name:"restrictness-mem-regions"
    ~uuid:"19826f38-24ce-42ef-bef5-8d4a54b5975d"
    (fun _ -> Regions.empty)

module Regions_tracker(Machine : Primus.Machine.S) = struct
  module Value = Primus.Value.Make(Machine)
  module Eval = Primus.Interpreter.Make(Machine)
  module Env = Primus.Env.Make(Machine)

  open Machine.Syntax

  let alloc ptr len =
    if Value.is_zero ptr || Value.is_zero len
    then Machine.return ()
    else
      Value.add ptr len >>= fun fin ->
      Value.pred fin >>= fun fin ->
      Machine.Local.update regions
        ~f:(fun regs -> Regions.alloc regs ptr fin)

  let release ptr =
    Machine.Local.update regions
      ~f:(fun regs -> Regions.release regs ptr)

  let on_call_return = function
    | "malloc", [len; ptr] -> alloc ptr len
    | "free", [ptr] -> release ptr
    | _ -> Machine.return ()

  let init () = Machine.sequence [
      Primus.Linker.Trace.return >>> on_call_return;
    ]
end

module Dump_results(Machine : Primus.Machine.S) = struct
  open Machine.Syntax

  let on_stop _ =
    Machine.Global.get state >>| fun s ->
    print_results  s

  let init () =
    Primus.System.stop >>> on_stop
end

let is_out a = match Arg.intent a with
  | Some Out -> true
  | _ -> false

module Is_violation(Machine : Primus.Machine.S) = struct
  module Env = Primus.Env.Make(Machine)
  module Eval = Primus.Interpreter.Make(Machine)
  module Value = Primus.Value.Make(Machine)
  open Machine.Syntax

  let is_alias regs arg arg' =
    Eval.get (Arg.lhs arg) >>= fun ptr ->
    Eval.get (Arg.lhs arg') >>= fun ptr' ->
    Machine.return (Regions.is_shared regs ptr ptr')

  let find_aliases args =
    let restricted = Seq.filter ~f:is_restricted args in
    if Seq.is_empty restricted then Machine.return []
    else
      Machine.Local.get regions >>= fun regs ->
      Machine.Seq.fold restricted ~init:[] ~f:(fun fails res ->
          Machine.Seq.fold args ~init:fails ~f:(fun fails arg ->
              if Arg.equal res arg || is_out arg
              then Machine.return fails
              else
                is_alias regs res arg >>= fun is_alias ->
                Machine.return @@
                if is_alias
                then
                  (Arg.lhs res, Arg.lhs arg) :: fails
                else fails))

  let find_sub jmp =
    match Jmp.kind Primus.Pos.(jmp.me) with
    | Goto _ | Ret _ | Int _ -> Machine.return None
    | Call c -> match Call.target c with
      | Indirect _ -> Machine.return None
      | Direct tid ->
        Machine.get () >>= fun proj ->
        Machine.return @@
        Program.lookup sub_t (Project.program proj) tid

  let update_results sub aliases =
    Eval.pc >>= fun addr ->
    Machine.Global.update state (fun s ->
        List.fold aliases ~init:s
          ~f:(fun s (arg1,arg2) ->
              let names = [Var.name arg1; Var.name arg2] in
              (addr,sub,names) :: s))

  let run _ =
    Eval.pos >>= fun pos ->
    match pos with
    | Primus.Pos.Jmp jmp ->
      begin
        find_sub jmp >>= function
        | None -> Value.b0
        | Some sub ->
          find_aliases (Term.enum arg_t sub) >>= function
          | [] ->
            Value.b0
          | aliases ->
            update_results (Sub.name sub) aliases >>= fun () ->
            Value.b1
      end
    | _ -> Value.b0

end


module Interface(Machine : Primus.Machine.S) = struct
  module Lisp = Primus.Lisp.Make(Machine)
  open Primus.Lisp.Type.Spec

  let init () =
    Lisp.define "is-restrictness-violation" (module Is_violation)
      ~types:(unit @-> bool)
      ~docs:{|(is-restrictness-violation) returns true if a current
               position is jump and arguments with 'restrict' keyword
               of a target subroutine are aliased|};
end

let enabled =
  Extension.Configuration.flag "enable" ~doc:"Enables the analysis"

let () =
  let open Extension.Syntax in
  Extension.declare
  @@ fun ctxt ->
  if ctxt --> enabled then
    begin
      Primus.Machine.add_component (module Regions_tracker);
      Primus.Machine.add_component (module Dump_results);
      Primus.Machine.add_component (module Interface);
    end;
  Ok ()
