open Type

val get_variables_from_predicate : predicate -> (string * int * int) list
val check_program : program -> unit