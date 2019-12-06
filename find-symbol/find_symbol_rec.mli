open Bap.Std
open Find_symbol_types

(** [find prog] returns a list of recursive subroutines along with an address *)
val find : program term -> symbol list
