open Env
open Type

val substitution_in_term : env -> term -> term
val substitution_in_builtin_predicate : env -> builtin_predicate -> builtin_predicate
val substitution_in_predicate : env -> predicate -> predicate
