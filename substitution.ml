open Type

let rec substitution_in_term env = function
  | Functor(f, term_list) -> Functor(f, List.map (substitution_in_term env) term_list)
  | BuiltinFunctor(f, t1, t2) -> BuiltinFunctor(f, substitution_in_term env t1, substitution_in_term env t2)
  | Cons(t1, t2) -> Cons(substitution_in_term env t1, substitution_in_term env t2)
  | Variable(_, id, _) as var ->
    begin
      try
        Env.find env id |> substitution_in_term env
      with Not_found -> var
    end
  | term -> term

let substitution_in_builtin_predicate env = function
| Is(t1, t2) -> Is(substitution_in_term env t1, substitution_in_term env t2)
| ArithmeticEquality(t1, t2) -> ArithmeticEquality(substitution_in_term env t1, substitution_in_term env t2)
| ArithmeticInequality(t1, t2) -> ArithmeticInequality(substitution_in_term env t1, substitution_in_term env t2)
| ArithmeticLess(t1, t2) -> ArithmeticLess(substitution_in_term env t1, substitution_in_term env t2)
| ArithmeticGreater(t1, t2) -> ArithmeticGreater(substitution_in_term env t1, substitution_in_term env t2)
| ArithmeticLeq(t1, t2) -> ArithmeticLeq(substitution_in_term env t1, substitution_in_term env t2)
| ArithmeticGeq(t1, t2) -> ArithmeticGeq(substitution_in_term env t1, substitution_in_term env t2)
| TermEquality(t1, t2) -> TermEquality(substitution_in_term env t1, substitution_in_term env t2)
| TermInequality(t1, t2) -> TermInequality(substitution_in_term env t1, substitution_in_term env t2)
| TermUnify(t1, t2) -> TermUnify(substitution_in_term env t1, substitution_in_term env t2)
| TermNotUnify(t1, t2) -> TermNotUnify(substitution_in_term env t1, substitution_in_term env t2)
| TermVar t -> TermVar(substitution_in_term env t)
| TermNotVar t -> TermNotVar(substitution_in_term env t)
| TermInteger t -> TermInteger(substitution_in_term env t)
| TermNotInteger t -> TermNotInteger(substitution_in_term env t)

let substitution_in_predicate env = function
  | Predicate(id, term_list) -> Predicate(id, List.map (substitution_in_term env) term_list)
  | BuiltinPredicate(builtin) -> BuiltinPredicate(substitution_in_builtin_predicate env builtin)
