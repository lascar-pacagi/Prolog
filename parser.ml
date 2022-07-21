open Lexer
open Type

let var_counter = ref min_int

let hashtable = Hashtbl.create 53

let reset () =
  line_number := 1;
  var_counter := min_int;
  Hashtbl.clear hashtable

let get_var_counter x =
  try
    if x = "_" then raise Not_found;
    Hashtbl.find hashtable x
  with Not_found ->
    begin
      let v = !var_counter in
      Hashtbl.add hashtable x v;
      incr var_counter;
      v
    end

exception Parse_error of string

let error msg = raise @@ Parse_error (msg ^ " at line " ^ string_of_int !line_number)

let init_tokens get_token =
  let nxt = ref (get_token ()) in
  let next () =
    let res = !nxt in
    nxt := get_token ();
    res
  in
  let peek () =
    !nxt
  in
  let junk () =
    nxt := get_token ()
  in
  (next, peek, junk)

let rec comma_separated_list_of parser ((_, peek, junk) as tokens) =
  let res = parser tokens in
  match peek () with
  | COMMA -> junk (); res :: comma_separated_list_of parser tokens
  | _ -> [res]
  

let rec lp_terms_rp ?(n=0) ((next, _, _) as tokens) =
  if next() <> LEFT_PAREN then error "'(' expected"
  else
    begin
      let l = comma_separated_list_of term tokens in
      if next () <> RIGHT_PAREN then error "')' expected";
      if n > 0 && List.length l <> n then 
        Printf.sprintf "%d arguments are expected" n |> error;
      l
    end
  
and [@warning "-8"] predicate ((next, _, _) as tokens) : predicate =
  match next () with
  | NAME name        -> lp_terms_rp tokens |> fun l -> Predicate(name, l)
  | TERM_EQ          -> lp_terms_rp ~n:2 tokens |> fun [t1; t2] -> BuiltinPredicate(TermEquality(t1, t2))
  | TERM_INEQ        -> lp_terms_rp ~n:2 tokens |> fun [t1; t2] -> BuiltinPredicate(TermInequality(t1, t2))
  | IS               -> lp_terms_rp ~n:2 tokens |> fun [t1; t2] -> BuiltinPredicate(Is(t1, t2))
  | TERM_UNIFY       -> lp_terms_rp ~n:2 tokens |> fun [t1; t2] -> BuiltinPredicate(TermUnify(t1, t2))
  | TERM_NOT_UNIFY   -> lp_terms_rp ~n:2 tokens |> fun [t1; t2] -> BuiltinPredicate(TermNotUnify(t1, t2))
  | ARITH_EQ         -> lp_terms_rp ~n:2 tokens |> fun [t1; t2] -> BuiltinPredicate(ArithmeticEquality(t1, t2))
  | ARITH_INEQ       -> lp_terms_rp ~n:2 tokens |> fun [t1; t2] -> BuiltinPredicate(ArithmeticInequality(t1, t2))
  | ARITH_LESS       -> lp_terms_rp ~n:2 tokens |> fun [t1; t2] -> BuiltinPredicate(ArithmeticLess(t1, t2))
  | ARITH_GREATER    -> lp_terms_rp ~n:2 tokens |> fun [t1; t2] -> BuiltinPredicate(ArithmeticGreater(t1, t2))
  | ARITH_GEQ        -> lp_terms_rp ~n:2 tokens |> fun [t1; t2] -> BuiltinPredicate(ArithmeticGeq(t1, t2))
  | ARITH_LEQ        -> lp_terms_rp ~n:2 tokens |> fun [t1; t2] -> BuiltinPredicate(ArithmeticLeq(t1, t2))
  | TERM_VAR         -> lp_terms_rp ~n:1 tokens |> fun [t] -> BuiltinPredicate(TermVar t)
  | TERM_NOT_VAR     -> lp_terms_rp ~n:1 tokens |> fun [t] -> BuiltinPredicate(TermNotVar t)
  | TERM_INTEGER     -> lp_terms_rp ~n:1 tokens |> fun [t] -> BuiltinPredicate(TermInteger t)
  | TERM_NOT_INTEGER -> lp_terms_rp ~n:1 tokens |> fun [t] -> BuiltinPredicate(TermNotInteger t)
  | _ -> error "user defined predicate or builtin predicate expected"

and [@warning "-8"] term ((next, peek, _) as tokens) : term =
  match next () with
  | PLUS  -> lp_terms_rp ~n:2 tokens |> fun [t1; t2] -> BuiltinFunctor(Plus, t1, t2)
  | MINUS -> lp_terms_rp ~n:2 tokens |> fun [t1; t2] -> BuiltinFunctor(Minus, t1, t2)
  | MULT  -> lp_terms_rp ~n:2 tokens |> fun [t1; t2] -> BuiltinFunctor(Mult, t1, t2)
  | DIV   -> lp_terms_rp ~n:2 tokens |> fun [t1; t2] -> BuiltinFunctor(Div, t1, t2)
  | INT i -> Integer i
  | VARIABLE name -> Variable(name, get_var_counter name, !line_number)
  | NAME name -> begin
      if peek () <> LEFT_PAREN then Functor(name, [])
      else lp_terms_rp tokens |> fun l -> Functor(name, l)
    end
  | LEFT_BRACKET -> list tokens
  | _ -> error "bad term expression"

and list ((next, peek, junk) as tokens) =
  if peek () = RIGHT_BRACKET then (junk (); EmptyList)
  else 
    let l = comma_separated_list_of term tokens in
    let remaining =
      match next () with
      | RIGHT_BRACKET -> EmptyList
      | PIPE -> begin 
          match next () with 
          | LEFT_BRACKET -> list tokens
          | VARIABLE name -> 
            if next () <> RIGHT_BRACKET then error "']' expected after variable";
            Variable(name, get_var_counter name, !line_number)
          | _ -> error "after a '|' you must have a '[' or a variable"    
      end
      | _ -> error "a ']' or a '|' must follow"
    in
    List.fold_right (fun t acc -> Cons(t, acc)) l remaining

let clause ((next, _, _) as tokens) =
  let p = predicate tokens in
  match p with
  | BuiltinPredicate _ -> error "builtin predicates are not allowed in head of clause"
  | _ -> ();
  let l =
    match next () with
    | DOT -> []
    | COLON_HYPHEN -> 
      let l = comma_separated_list_of predicate tokens in
      if next () <> DOT then error "'.' expected at the end of a clause body";
      l
    | _ -> error "head of clause is followed by '.' or ':-'"
  in
  Clause(p, l)
    
let rec clause_list ((_, peek, _) as tokens) =
  let c = clause tokens in
  if peek () = EOF then [c]
  else c :: clause_list tokens

let program get_token = clause_list (init_tokens get_token)

let query get_token = 
  let (next, _, _) as tokens = init_tokens get_token in
  let q = comma_separated_list_of predicate tokens in
  if next () <> DOT then error "a query must terminate with a '.'";
  if next () <> EOF then error "end of file expected";
  q
