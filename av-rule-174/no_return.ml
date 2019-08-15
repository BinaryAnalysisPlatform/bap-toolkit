open Core_kernel
open Bap.Std

class mapper tids = object
  inherit Term.mapper

  method! map_jmp j =
    match Jmp.kind j with
    | Goto _ | Ret _ | Int _ -> j
    | Call c ->
       match Call.target c with
       | Indirect _ -> j
       | Direct tid ->
          if List.mem tids tid ~equal:Tid.equal then
            Jmp.with_kind j (Call (Call.with_noreturn c))
          else j
end

let is_no_return names s =
  List.mem names (Sub.name s) ~equal:String.equal

let no_rets subs names =
  let is_no_return s =
    List.mem names (Sub.name s) ~equal:String.equal in
  Seq.fold subs ~init:(Set.empty (module Tid))
    ~f:(fun acc s ->
      if is_no_return s then Set.add acc (Term.tid s)
      else acc)

let main proj =
  let prog = Project.program proj in
  let names = ["abort"; "exit"; "__stack_chk_fail"] in
  let tids =
    Term.to_sequence sub_t prog |>
      Seq.filter ~f:(is_no_return names) |>
     Seq.map ~f:Term.tid |> Seq.to_list in
  Project.with_program proj ((new mapper tids)#run prog)

let init () = Project.register_pass ~autorun:true main
