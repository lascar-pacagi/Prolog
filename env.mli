open Type
type env

val empty   : unit -> env
val add     : env -> int -> term -> env
val add_all : env -> int list -> term list -> env
val create  : (int * term) list -> env
val find    : env -> int -> term
val defined : env -> int -> bool
val map     : env -> (term -> term) -> env
val equal   : env -> env -> bool
val iter    : env -> (int -> unit) -> (term -> unit) -> unit
val filter  : env -> (int -> bool) -> env
val size    : env -> int
