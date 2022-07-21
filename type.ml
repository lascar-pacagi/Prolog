type builtin_functor =
| Plus
| Minus
| Mult
| Div

type line_number = int

type term =
| Functor of string * term list
| BuiltinFunctor of builtin_functor * term * term
| Integer of int
| Variable of string * int * line_number
| EmptyList
| Cons of term * term

type builtin_predicate =
| Is of term * term
| ArithmeticEquality of term * term
| ArithmeticInequality of term * term
| ArithmeticLess of term * term
| ArithmeticGreater of term * term
| ArithmeticLeq of term * term
| ArithmeticGeq of term * term
| TermEquality of term * term
| TermInequality of term * term
| TermUnify of term * term
| TermNotUnify of term * term
| TermVar of term
| TermNotVar of term
| TermInteger of term
| TermNotInteger of term

type predicate =
| Predicate of string * term list
| BuiltinPredicate of builtin_predicate

type clause = Clause of predicate * predicate list

type program = clause list

type query = predicate list
