open Core_kernel
open Graphlib.Std
open Bap.Std
open Graphs


(** [complexity cfg entry]  *)
val complexity : Cfg.t -> Block.t -> int

(** [is_reducible cfg entry]  *)
val is_reducible : Cfg.t -> Block.t -> bool

(** [find_recursive prog]  *)
val find_recursive : program term -> sub term list
