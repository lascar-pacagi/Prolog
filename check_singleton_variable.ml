open Type

let rec get_variables_from_term = function
  | Functor(_, term_list) -> List.map get_variables_from_term term_list |> List.concat
  | BuiltinFunctor(_, t1, t2) | Cons(t1, t2) ->
    begin
      get_variables_from_term t1
      @ get_variables_from_term t2
    end
  | Integer _ | EmptyList -> []
  | Variable(x, id, pos) -> [x, id, pos]

let get_variables_from_builtin_predicate = function
  | Is(t1, t2)
  | ArithmeticEquality(t1, t2)
  | ArithmeticInequality(t1, t2)
  | ArithmeticLess(t1, t2)
  | ArithmeticGreater(t1, t2)
  | ArithmeticLeq(t1, t2)
  | ArithmeticGeq(t1, t2)
  | TermEquality(t1, t2)
  | TermInequality(t1, t2)
  | TermUnify(t1, t2)
  | TermNotUnify(t1, t2) -> get_variables_from_term t1 @ get_variables_from_term t2
  | TermVar t
  | TermNotVar t
  | TermInteger t
  | TermNotInteger t -> get_variables_from_term t

let get_variables_from_predicate = function
  | Predicate(_, term_list) ->
    begin
      List.map get_variables_from_term term_list |> List.concat
    end
  | BuiltinPredicate builtin -> get_variables_from_builtin_predicate builtin

let check_clause = function
  | Clause(predicate, predicate_list) ->
    let variables =
      predicate :: predicate_list
      |> List.map get_variables_from_predicate
      |> List.concat
      |> List.sort compare
    in
    let print_error (pos, x) =
      Printf.printf "singleton variable %s at line %d\n" x pos
    in
    let rec singleton_variable prev = function
      | [] -> []
      | (x, _, _) :: r when String.get x 0 = '_' -> singleton_variable None r
      | (_, id, _) :: r when prev = Some id -> singleton_variable prev r
      | (_, id1, _) :: (_, id2, _) :: r when id1 = id2 -> singleton_variable (Some id1) r
      | (x, id, pos) :: r -> (pos, x) :: singleton_variable (Some id) r
    in
    singleton_variable None variables
    |> List.sort compare
    |> List.iter print_error

let check_program prog = List.iter check_clause prog
