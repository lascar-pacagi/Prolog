open Env
open Type

exception Not_unifiable

val unify_terms : env -> term -> term -> env
val unify_predicates : env -> (predicate * predicate) -> env
