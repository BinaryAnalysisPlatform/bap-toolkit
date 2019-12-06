open Find_symbol_types

type level

(** [level_of_int x] returns a level of verbosity
    for dump function. There are two levels:
    level 1: outputs results in a brief format (for x <= 1)
    level 2: also outputs results in a table format (for all other x)  *)
val level_of_int : int -> level

(** [dump ~print_addrs ~requested ~level results]
    dumps all the results into stdout.

    @print_addrs is true by default *)
val dump :
  ?print_addrs:bool ->
  ?requested : check list ->
  level:level ->
  results -> unit

(** [dump_incidents results] dumps results in "incidents.static" file *)
val dump_incidents : results -> unit
