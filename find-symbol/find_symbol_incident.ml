open Core_kernel
open Bap.Std

open Find_symbol_types

module Id = struct
  let state = ref 1

  let next () =
    let r = !state in
    incr state;
    r

end

type location = addr [@@deriving sexp]

type t =  Out_channel.t option

let t = ref None

let () = at_exit
    (fun () ->
       match !t with
       | None -> ()
       | Some ch -> Out_channel.close ch)

let enable () =
  t := Some (Out_channel.create "incidents")

let output_sexp ch s =
  let fmt = Format.formatter_of_out_channel ch in
  Format.fprintf fmt "%a\n%!" Sexp.pp_hum s

let sexp_of_addr a =
  Sexp.Atom (Addr.string_of_value ~hex:true a)

let sexp_of_symbol s a =
  let open Sexp in
  List [Atom "symbol"; List [Atom s; sexp_of_addr a]]

let sexp_of_location id addr =
  let open Sexp in
  let point = sprintf "0:%s"
                (Sexp.to_string (sexp_of_addr addr)) in
  List [Atom "incident-location";
        List [sexp_of_int id; List [Atom point]]]

let sexp_of_reason reason =
  Sexp.Atom
    (Sexp.to_string (sexp_of_reason reason) |>
       String.lowercase |>
       String.map ~f:(fun c -> if c = '_' then '-' else c))

let sexp_of_incident reason loc_id =
  let open Sexp in
  List [Atom "incident";
        List
          [sexp_of_reason reason;
           sexp_of_int loc_id]]

let output ch reason addr =
  let id = Id.next () in
  output_sexp ch (sexp_of_location id addr);
  output_sexp ch (sexp_of_incident reason id);
  Out_channel.flush ch

let notify reason addr =
  Option.iter !t ~f:(fun t -> output t reason addr)

let notify_symbols s =
  Symtab.to_sequence s |>
    Seq.iter ~f:(fun (name,entry,_) ->
        let addr = Block.addr entry in
        Option.iter !t ~f:(fun ch ->
            output_sexp ch (sexp_of_symbol name addr)))
