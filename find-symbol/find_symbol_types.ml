open Core_kernel
open Bap.Std

type symbol = {
  name : string;
  addr : Addr.t option;
} [@@deriving compare,bin_io,hash,sexp]

type check =
  | Forbidden_function
  | Recursive_function
  | Non_structural_cfg
  | Complex_function
[@@deriving bin_io,compare,hash,sexp]

type symbol_data =
  | Complexity of int
  | Non_structural_block of addr
[@@deriving sexp]

type result = {
  data  : symbol_data option;
  check : check;
}

module Symbol = struct
  type t = symbol [@@deriving bin_io,compare,hash,sexp]
  module T = struct
    type nonrec t = t [@@deriving bin_io,compare,hash,sexp]
    let module_name = "Find_symbol.Types.Symbol"
    let to_string t = Sexp.to_string (sexp_of_t t)
    let of_string s = t_of_sexp (Sexp.of_string s)
  end
  include Identifiable.Make(T)
end

module Check = struct
  type t = check [@@deriving bin_io,compare,hash,sexp]
  module T = struct
    type nonrec t = t [@@deriving bin_io,compare,hash,sexp]
    let module_name = "Find_symbol.Types.Check"
    let to_string t = Sexp.to_string (sexp_of_t t)
    let of_string s = t_of_sexp (Sexp.of_string s)
  end
  include Identifiable.Make(T)
end

type results = result list Symbol.Map.t
