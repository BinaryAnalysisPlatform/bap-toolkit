open Core_kernel
open Bap.Std


type reason =
  | Forbidden_function
  | Recursive_function
  | Non_structural_cfg
  | Complex_function
[@@deriving sexp]


type symbol_data =
  | Complexity of int
  | Non_structural_block of addr
[@@deriving sexp]
