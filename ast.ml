open Type

let indent () = Printf.printf "   "

let print_builtin_term t =
  Printf.printf "%s"
    (match t with
    | Plus  -> "+"
    | Minus -> "-"
    | Mult  -> "*"
    | Div   -> "/")

let rec print_term = function
  | Functor(name, term_list) ->
    begin
      Printf.printf "%s" name;
      if term_list != [] then
        begin
          Printf.printf "(";
          print_term_list term_list;
          Printf.printf ")";
        end
    end
  | BuiltinFunctor(builtin, op1, op2) ->
    begin
      print_builtin_term builtin;
      Printf.printf "(";
      print_term op1;
      Printf.printf ", ";
      print_term op2;
      Printf.printf ")"
    end
  | Integer i -> Printf.printf "%d" i
  | Variable(var, id, _) ->
    begin
      Printf.printf "%s" var;
      if id >= 0 then string_of_int id |> Printf.printf "%s"
    end
  | EmptyList -> Printf.printf "[]"
  | Cons _ as l ->
    begin
      Printf.printf "[";
      print_list l;
      Printf.printf "]";
    end

and print_list term =
  match term with
  | Cons(head, tail) ->
    begin
      (
        match head with
        | Cons _ ->
          Printf.printf "[";
          print_list head;
          Printf.printf "]";
        | _ -> print_term head
      );
      (
        match tail with
        | Cons _ -> Printf.printf ", "; print_list tail
        | EmptyList -> ()
        | _ -> Printf.printf " | "; print_list tail
      )
    end
  | EmptyList | Variable _ -> print_term term
  | _ -> assert false

and print_term_list term_list =
  print_term (List.hd term_list);
  List.iter
    (fun term ->
      Printf.printf ", ";
      print_term term)
    (List.tl term_list)

let print_builtin_predicate builtin =
  let print name t1 t2 =
    Printf.printf "%s(" name;
    print_term t1;
    Printf.printf ", ";
    print_term t2;
    Printf.printf ")"
  in
  let print' name t =
    Printf.printf "%s(" name;
    print_term t;
    Printf.printf ")"
  in
  match builtin with
  | Is(t1, t2)                   -> print "is" t1 t2
  | ArithmeticEquality(t1, t2)   -> print "=:=" t1 t2
  | ArithmeticInequality(t1, t2) -> print "=\\=" t1 t2
  | ArithmeticLess(t1, t2)       -> print "<" t1 t2
  | ArithmeticGreater(t1, t2)    -> print ">" t1 t2
  | ArithmeticLeq(t1, t2)        -> print "<=" t1 t2
  | ArithmeticGeq(t1, t2)        -> print ">=" t1 t2
  | TermEquality(t1, t2)         -> print "==" t1 t2
  | TermInequality(t1, t2)       -> print "\\==" t1 t2
  | TermUnify(t1, t2)            -> print "=" t1 t2
  | TermNotUnify(t1, t2)         -> print "\\=" t1 t2
  | TermVar t                    -> print' "var" t
  | TermNotVar t                 -> print' "not_var" t
  | TermInteger t                -> print' "integer" t
  | TermNotInteger t             -> print' "not_integer" t

let print_predicate = function
  | Predicate(name, term_list) ->
    begin
      Printf.printf "%s(" name;
      print_term_list term_list;
      Printf.printf ")"
    end
  | BuiltinPredicate builtin -> print_builtin_predicate builtin

let print_predicate_list predicate_list indentation =
  if indentation then indent ();
  print_predicate (List.hd predicate_list);
  List.iter
    (fun p ->
      Printf.printf "%s" (if indentation then ",\n" else ", ");
      if indentation then indent ();
      print_predicate p)
    (List.tl predicate_list)

let print_clause = function
  | Clause(predicate, predicate_list) ->
    begin
      print_predicate predicate;
      if predicate_list != [] then
        begin
          Printf.printf " :-\n";
          print_predicate_list predicate_list true
        end;
      Printf.printf "."
    end

let print_program program =
  print_clause (List.hd program);
  List.iter
    (fun c ->
      Printf.printf "\n\n";
      print_clause c)
    (List.tl program);
  Printf.printf "\n"

let print_query query =
  print_predicate_list query false;
  Printf.printf ".\n"
