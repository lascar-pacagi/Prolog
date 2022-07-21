open Type

exception Runtime_error of string
exception Not_an_integer

let error msg = raise (Runtime_error msg)

let get_variables query =
  List.map Check_singleton_variable.get_variables_from_predicate query
  |> List.concat
  |> List.map (fun (x, id, _) -> x, id)
  |> List.sort_uniq compare

let rename_rule k = function
  | Clause(c, predicate_list) ->
    begin
      let variables = get_variables (c :: predicate_list) in
      let l, k' =
        List.fold_left
          (fun (res, k) (x, id) ->
            (id, Variable(x, k, -1)) :: res, k + 1
          )
          ([], k)
          variables
      in
      let env = Env.create l in
      (Substitution.substitution_in_predicate env c,
       List.map (Substitution.substitution_in_predicate env) predicate_list), k'
    end

let rec evaluate_term env = function
  | Functor(f, term_list) -> Functor(f, List.map (evaluate_term env) term_list)
  | BuiltinFunctor(f, t1, t2) ->
    begin
      let t1' = evaluate_term env t1 in
      let t2' = evaluate_term env t2 in
      let op =
        match f with
          | Plus  -> (+)
          | Minus -> (-)
          | Mult  -> ( * )
          | Div   -> (/)
      in
      match t1', t2' with
      | Integer i, Integer j -> Integer (op i j)
      | _ -> error "integer operands expected"
    end
  | Variable(_, id, _) as v ->
    begin
      try Env.find env id |> evaluate_term env with Not_found -> v
    end
  | Cons(t1, t2) -> Cons(evaluate_term env t1, evaluate_term env t2)
  | term -> term

let get_integer env term =
  match evaluate_term env term with
  | Integer i -> i
  | _ -> raise Not_an_integer

let compare_integers env f t1 t2 =
try
  if f (get_integer env t1) (get_integer env t2) then
    true, env
  else
    false, env
with Not_an_integer -> error "integers expected"

let rec evaluate_builtin_predicate env builtin =
  match builtin with
  | Is(t1, t2) ->
    begin
      let t1' = evaluate_term env t1 in
      match t1' with
      | Integer _ -> evaluate_builtin_predicate env (ArithmeticEquality(t1', t2))
      | Variable(_, id, _) ->
        begin
          try
            let i = get_integer env t2 in
            true, Env.add env id (Integer i)
          with Not_an_integer -> false, env
        end
      | _ -> false, env
    end
  | ArithmeticEquality(t1, t2)   -> compare_integers env (=) t1 t2
  | ArithmeticInequality(t1, t2) -> compare_integers env (<>) t1 t2
  | ArithmeticLess(t1, t2)       -> compare_integers env (<) t1 t2
  | ArithmeticGreater(t1, t2)    -> compare_integers env (>) t1 t2
  | ArithmeticLeq(t1, t2)        -> compare_integers env (<=) t1 t2
  | ArithmeticGeq(t1, t2)        -> compare_integers env (>=) t1 t2
  | TermEquality(t1, t2) ->
    begin
      let t1' = evaluate_term env t1 in
      let t2' = evaluate_term env t2 in
      t1' = t2', env
    end
  | TermInequality(t1, t2) ->
    begin
      let t1' = evaluate_term env t1 in
      let t2' = evaluate_term env t2 in
      t1' <> t2', env
    end
  | TermUnify(t1, t2) ->
    begin
      try
        true, Unification.unify_terms env t1 t2
      with Unification.Not_unifiable ->
        false, env
    end
  | TermNotUnify(t1, t2) ->
    begin
      try
        let _ = Unification.unify_terms env t1 t2 in
        false, env
      with Unification.Not_unifiable ->
        true, env
    end
  | TermVar t ->
    begin
      match evaluate_term env t with
      | Variable _ -> true, env
      | _ -> false, env
    end
  | TermNotVar t ->
    begin
      match evaluate_term env t with
      | Variable _ -> false, env
      | _ -> true, env
    end
  | TermInteger t ->
    begin
      match evaluate_term env t with
      | Integer _ -> true, env
      | _ -> false, env
    end
  | TermNotInteger t ->
    begin
      match evaluate_term env t with
      | Integer _ -> false, env
      | _ -> true, env
    end

let eval rules query =
  let variables = get_variables query in
  let print_and_ask env =
    let env' =
      Env.filter env
        (fun id -> List.exists (fun (x, id') -> id = id' && String.get x 0 <> '_') variables)
    in
    let env'' = Env.map env' (Substitution.substitution_in_term env) in
    Env.iter env''
      (fun id ->
        try
          let x, _ = List.find (fun (_, id') -> id = id') variables in
          Printf.printf "%s = " x
        with Not_found ->
          Printf.printf "V%d = " id
      )
      Ast.print_term;
    Printf.printf "Yes\n";
    Printf.printf "More?";
    let ans = read_line () in
    ans <> ";"
  in
  let rec backchain k env goals =
    match goals with
    | [] -> print_and_ask env
    | (BuiltinPredicate builtin) :: r ->
      begin
        let ok, env' = evaluate_builtin_predicate env builtin in
        ok && backchain k env' r
      end
    | g :: r ->
      begin
        List.exists
          (fun rule ->
            let (conclusion, premises), k' = rename_rule k rule in
            try
              backchain k' (Unification.unify_predicates env (conclusion, g)) (premises @ r)
            with Unification.Not_unifiable -> false
          )
          rules
      end
  in
  backchain 0 (Env.empty ()) query |> ignore;
  Printf.printf "No\n"
