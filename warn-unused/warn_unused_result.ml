(** The Algorithm

    First, for each def term we deduce, if lhs is
    an argument of a function with warn-unused-result attribute.
    (actually this attribute belongs to output argument of
    such function, that simplify a task a little). And if it's
    true, then we taint rhs value.

    There are two possible points when we sanitize a taint:
    1) if control flow depends from the tainted value, i.e.
    a taint reached a condition in a jump term

    2) taint reached an external function, i.e. it's in the rhs
    of a callsite of such function.

    All the sources of not sanitized taints are considred as unused. **)

open Core_kernel
open Bap.Std
open Bap_primus.Std
open Bap_main

module Vid = Primus.Value.Id

let check_name = "warn unused result"

type callsite = {
  addr : addr;
  arg  : string;
  sub  : string;
}

let print_results rs =
  let pp_bold ppf = Format.fprintf ppf "\027[1m" in
  let pp_norm ppf = Format.fprintf ppf "\027[0m" in
  match rs with
  | [] -> Format.printf "%s   OK\n" check_name
  | rs ->
    Format.printf "%s   FAIL\n\n" check_name;
    Format.printf "%t%-10s %-20s %s\n%t" pp_bold "Address" "Function"
      "Argument" pp_norm;
    List.iter rs ~f:(fun {addr; sub; arg} ->
        let addr = sprintf "%a" Addr.pps addr in
        Format.printf "%-10s %-20s %s\n" addr sub arg)

let has_attr t attr =
  Seq.exists (Dict.to_sequence (Term.attrs t))
    ~f:(fun (typ,_) ->
        String.is_substring (Value.Typeid.to_string typ) attr)


module HasAttr(Machine : Primus.Machine.S) = struct
  module Eval = Primus.Interpreter.Make(Machine)
  module Value = Primus.Value.Make(Machine)

  open Machine.Syntax

  [@@@warning "-P"]
  let run [var; attr] =
    Value.Symbol.of_value var  >>= fun var ->
    Value.Symbol.of_value attr >>= fun attr ->
    Eval.pos >>= fun pos ->
    match pos with
    | Primus.Pos.(Def {me=def}) ->
      if not (has_attr def attr)
      then Value.b0
      else
        let lhs = Def.lhs def in
        if String.equal (Var.name lhs) var then Value.b1
        else
          Value.of_bool @@
          Set.exists (Exp.free_vars (Def.rhs def))
            ~f:(fun v -> String.equal (Var.name v) var)
    | _ -> Value.b0
end

let find_callee prog def =
  match Term.get_attr def Term.origin with
  | None -> None
  | Some tid -> match Program.lookup jmp_t prog tid with
    | None -> None
    | Some jmp -> match Jmp.kind jmp with
      | Int _ | Goto _ | Ret _ -> None
      | Call c ->
        match Call.target c with
        | Indirect _ -> None
        | Direct tid -> Program.lookup sub_t prog tid

let find_callsite prog pos =
  let open Option in
  match pos with
  | Primus.Pos.Def {me=def} ->
    find_callee prog def >>= fun sub ->
    Term.get_attr def address >>= fun addr ->
    Some { addr;
           sub=Sub.name sub;
           arg=Var.name (Def.lhs def) }
  | _ -> None

type state = {
  unchecked  : Primus.Value.Id.Set.t;
  callsites  : callsite Primus.Value.Id.Map.t;
}

let state =
  Primus.Machine.State.declare
    ~name:"warn-unused-result"
    ~uuid:"1e117d51-3b9d-4c93-b136-e15b77c5ea26"
    (fun _ -> {
         unchecked = Set.empty (module Primus.Value.Id);
         callsites = Map.empty (module Primus.Value.Id);
       })

module Output_results(Machine : Primus.Machine.S) = struct
  open Machine.Syntax

  let on_stop _ =
    Machine.Global.get state >>| fun s ->
    let unused = Set.fold s.unchecked ~init:(Map.empty (module Addr))
        ~f:(fun acc tid -> match Map.find s.callsites tid with
            | None -> acc
            | Some x -> Map.set acc x.addr x)  in
    print_results (Map.data unused)


  let init () =
    Primus.System.stop >>> on_stop
end

module Mark(Machine : Primus.Machine.S) = struct
  module Eval = Primus.Interpreter.Make(Machine)
  module Value = Primus.Value.Make(Machine)
  open Machine.Syntax

  [@@@warning "-P"]
  let run [taint] =
    Machine.get () >>= fun proj ->
    Eval.pos >>= fun pos ->
    Machine.Global.update state ~f:(fun s ->
        match find_callsite (Project.program proj) pos with
        | None -> s
        | Some cs ->
          { s with callsites =
                     Map.set s.callsites (Value.id taint) cs }) >>= fun () ->
    Value.b0
end

let is_section name v =
  match Value.get Image.section v with
  | Some x -> String.(x = name)
  | _ -> false

module Is_external_arg(Machine : Primus.Machine.S) = struct
  module Value = Primus.Value.Make(Machine)
  module Eval = Primus.Interpreter.Make(Machine)
  open Machine.Syntax

  let section_memory sec_name =
    Machine.get () >>| fun proj ->
    Memmap.filter (Project.memory proj) ~f:(is_section sec_name) |>
    Memmap.to_sequence |>
    Seq.map ~f:fst

  let is_in_plt addr =
    section_memory ".plt" >>= fun memory ->
    Value.of_bool @@
    Seq.exists memory ~f:(fun mem -> Memory.contains mem addr)

  let is_external_arg def =
    Machine.get () >>= fun proj ->
    match find_callee (Project.program proj) def with
    | None -> Value.b0
    | Some sub -> match Term.get_attr sub address with
      | None -> Value.b0
      | Some addr -> is_in_plt addr

  [@@@warning "-P"]
  let run [var] =
    Value.Symbol.of_value var >>= fun name ->
    Eval.pos >>= fun pos ->
    match pos with
    | Primus.Pos.Def {me=def} -> is_external_arg def
    | _ -> Value.b0
end

module NotifyUnused(Machine : Primus.Machine.S) = struct
  module Value = Primus.Value.Make(Machine)
  open Machine.Syntax

  [@@@warning "-P"]
  let run [taint] =
    Machine.Global.update state ~f:(fun s ->
        {s with unchecked = Set.add s.unchecked (Value.id taint) }) >>= fun () ->
    Value.b0
end

module Interface(Machine : Primus.Machine.S) = struct
  module Lisp = Primus.Lisp.Make(Machine)
  open Primus.Lisp.Type.Spec

  let init () =
    Machine.sequence [
      Lisp.define "has-attr" (module HasAttr)
        ~types:(tuple [a; b] @-> bool)
        ~docs:{|(has-attr var attr) returns true if
               [var] has attribute [attr] in the context of the current term|};

      Lisp.define "check-if-used" (module Mark)
        ~types:(tuple [a] @-> b)
        ~docs:{|(check-if-used T) marks current position as a source of taint T|};

      Lisp.define "is-external-argument" (module Is_external_arg)
        ~types:(tuple [a] @-> bool)
        ~docs:{|(is-external-argument V)| returns true if [V] is an
               argument of external function in the context of the current term|};

      Lisp.define "notify-warn-unused-result" (module NotifyUnused)
        ~types:(tuple [a] @-> b)
        ~docs:{|(notify-warn-unused-result T)|
               notifies that a source of taint T was never used|};

    ]
end

let enabled = Extension.Configuration.flag "enable" ~doc:"Enables the analysis"

let () =
  let open Extension.Syntax in
  Extension.declare
  @@ fun ctxt ->
  if ctxt --> enabled then
    begin
      Primus.Machine.add_component (module Interface);
      Primus.Machine.add_component (module Output_results);
    end;
  Ok ()
[@@warning "-D"]
