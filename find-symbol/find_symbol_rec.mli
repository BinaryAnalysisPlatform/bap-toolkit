open Bap.Std


(** [find prog] returns a list of recursive subroutines along with an address *)
val find : program term -> (string * addr) list

(** [find' prog] returns a list of tids of recursive subroutines
    and a recursive path for each one. *)
val find' : program term -> (tid * tid list) list
