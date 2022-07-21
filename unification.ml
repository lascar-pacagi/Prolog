open Type

exception Not_unifiable

let rec unify_terms env t1 t2 =
  match Substitution.substitution_in_term env t1, Substitution.substitution_in_term env t2 with
  | t1, t2 when t1 = t2 -> env
  | Functor(f1, l1), Functor(f2, l2) ->
    begin
      if f1 = f2 then unify_lists env l1 l2
      else raise Not_unifiable
    end
  | BuiltinFunctor(f1, t1, t2), BuiltinFunctor(f2, t1', t2') ->
    begin
      if f1 = f2 then unify_lists env [t1; t2] [t1'; t2']
      else raise Not_unifiable
    end
  | Cons(t1, t2), Cons(t1', t2') -> unify_lists env [t1; t2] [t1'; t2']
  | Variable(_, id, _), t | t, Variable(_, id, _) -> Env.add env id t
  | _ -> raise Not_unifiable

and unify_lists env l1 l2 =
  try
    List.fold_left2 (fun env t1 t2 -> unify_terms env t1 t2) env l1 l2
  with Invalid_argument _ -> raise Not_unifiable

let unify_predicates env = function
  | Predicate(p1, l1), Predicate(p2, l2) when p1 = p2 -> unify_lists env l1 l2
  | _ -> raise Not_unifiable
