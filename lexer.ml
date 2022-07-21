type token =
| INT of int
| VARIABLE of string
| NAME of string
| LEFT_PAREN | RIGHT_PAREN
| LEFT_BRACKET | RIGHT_BRACKET
| PIPE | DOT | COMMA | COLON_HYPHEN
| PLUS | MINUS | MULT | DIV
| TERM_EQ | TERM_INEQ | IS | TERM_UNIFY | TERM_NOT_UNIFY
| TERM_VAR | TERM_NOT_VAR | TERM_INTEGER | TERM_NOT_INTEGER
| ARITH_EQ | ARITH_INEQ | ARITH_LESS | ARITH_GREATER | ARITH_GEQ | ARITH_LEQ
| EOF

let print = function
  | INT i -> Printf.printf "INT %d\n" i
  | VARIABLE var -> Printf.printf "VARIABLE %s\n" var
  | NAME name -> Printf.printf "NAME %s\n" name
  | LEFT_PAREN -> Printf.printf "LEFT_PAREN\n"
  | RIGHT_PAREN -> Printf.printf "RIGHT_PAREN\n"
  | LEFT_BRACKET -> Printf.printf "LEFT_BRACKET\n"
  | RIGHT_BRACKET -> Printf.printf "RIGHT_BRACKET\n"
  | PIPE -> Printf.printf "PIPE\n"
  | DOT -> Printf.printf "DOT\n"
  | COMMA -> Printf.printf "COMMA\n"
  | COLON_HYPHEN -> Printf.printf "COLON_HYPHEN\n"
  | PLUS -> Printf.printf "PLUS\n"
  | MINUS -> Printf.printf "MINUS\n"
  | MULT -> Printf.printf "MULT\n"
  | DIV -> Printf.printf "DIV\n"
  | TERM_EQ -> Printf.printf "TERM_EQ\n"
  | TERM_INEQ -> Printf.printf "TERM_INEQ\n"
  | IS -> Printf.printf "IS\n"
  | TERM_UNIFY -> Printf.printf "TERM_UNIFY\n"
  | TERM_NOT_UNIFY -> Printf.printf "TERM_NOT_UNIFY\n"
  | TERM_VAR -> Printf.printf "TERM_VAR\n"
  | TERM_NOT_VAR -> Printf.printf "TERM_NOT_VAR\n"
  | TERM_INTEGER -> Printf.printf "TERM_INTEGER\n"
  | TERM_NOT_INTEGER -> Printf.printf "TERM_NOT_INTEGER\n"
  | ARITH_EQ -> Printf.printf "ARITH_EQ\n"
  | ARITH_INEQ -> Printf.printf "ARITH_INEQ\n"
  | ARITH_LESS -> Printf.printf "ARITH_LESS\n"
  | ARITH_GREATER -> Printf.printf "ARITH_GREATER\n"
  | ARITH_GEQ -> Printf.printf "ARITH_GEQ\n"
  | ARITH_LEQ -> Printf.printf "ARITH_LEQ\n"
  | EOF -> Printf.printf "EOF\n"

let line_number = ref 1

exception Lexical_error of string

let newline () = incr line_number

let error msg = raise @@ Lexical_error (msg ^ " at line " ^ string_of_int !line_number)

let keywords = Hashtbl.create 53

let () = List.iter
  (fun (keyword, token) ->
    Hashtbl.add keywords keyword token
  )
  [ "is", IS;
    "var", TERM_VAR;
    "not_var", TERM_NOT_VAR;
    "integer", TERM_INTEGER;
    "not_integer", TERM_NOT_INTEGER
  ]

let rec get_token stream =
  let next () = Stream.next stream in
  let peek () = Stream.peek stream in
  let junk () = Stream.junk stream |> ignore in
  let rec consume_space () =
    match peek () with
    | Some(' ' | '\t' | '\r') -> junk (); consume_space ()
    | Some('\n') -> newline (); junk (); consume_space ()
    | _ -> ()
  in
  let rec consume_comment () =
    try
      if peek () = Some('*') then
        begin
          junk ();
          if peek () <> Some('/') then consume_comment ()
          else junk ()
        end
      else
        begin
          if next () = '\n' then newline ();
          consume_comment ()
        end
    with Stream.Failure -> error "EOF in comment"
  in
  let rec consume_up_to_end_of_line () =
    try
      if next () = '\n' then newline ()
      else consume_up_to_end_of_line ()
    with Stream.Failure -> error "EOF in comment"
  in
  let char_to_int c = Char.code c - Char.code '0' in
  let rec get_integer res =
    match peek () with
    | Some('0'..'9' as c) -> junk (); 10 * res + char_to_int c |> get_integer
    | _ -> res
  in
  let var_name_or_keyword s =
    try
      Hashtbl.find keywords s
    with Not_found ->
      begin
        match String.get s 0 with
        | 'A'..'Z' | '_' -> VARIABLE s
        | _ -> NAME s
      end
  in
  let char_to_string c = String.make 1 c in
  let rec get_identifier res =
    match peek () with
    | Some('A'..'Z' | 'a'..'z' | '0'..'9' | '_' as c) ->
      begin
        junk ();
        res ^ char_to_string c |> get_identifier
      end
    | _ -> res
  in
  try
    consume_space ();
    match next () with
    | '.' -> DOT
    | '+' -> PLUS
    | '-' -> MINUS
    | '*' -> MULT
    | '%' -> begin
        consume_up_to_end_of_line ();
        get_token stream
      end
    | '/' -> begin
        if peek () = Some('*') then
          begin
            junk ();
            consume_comment ();
            get_token stream
          end
        else DIV
      end
    | ',' -> COMMA
    | '|' -> PIPE
    | '(' -> LEFT_PAREN
    | ')' -> RIGHT_PAREN
    | '[' -> LEFT_BRACKET
    | ']' -> RIGHT_BRACKET
    | '<' -> begin
        match peek () with
        | Some('=') -> junk (); ARITH_LEQ
        | _   -> ARITH_LESS
      end
    | '>' -> begin
        match peek () with
        | Some('=') -> junk (); ARITH_GEQ
        | _   -> ARITH_GREATER
      end
    | '=' -> begin
        match peek () with
        | Some(':') -> begin
            junk ();
            if peek () = Some('=') then (junk (); ARITH_EQ)
            else error "need an '=' after ':' in '=:'"
          end
        | Some('\\') -> begin
            junk ();
            if peek () = Some('=') then (junk (); ARITH_INEQ)
            else error "need an '=' after '\\' in '=\\'"
          end
        | Some('=') -> junk (); TERM_EQ
        | _   -> TERM_UNIFY
      end
    | ':' -> begin
        if peek () = Some('-') then (junk (); COLON_HYPHEN)
        else error "hyphen expected after ':'"
      end
    | '\\' -> begin
        if peek () = Some('=') then
          begin
            junk ();
            if peek () = Some('=') then (junk (); TERM_INEQ)
            else TERM_NOT_UNIFY
          end
        else error "'=' expected after '\\'"
      end
    | '0'..'9' as c -> INT(get_integer (char_to_int c))
    | 'A'..'Z' | 'a'..'z' | '_' as c -> begin
        get_identifier (char_to_string c) |> var_name_or_keyword
      end
    | _ as c -> error ("unexpected character: " ^ char_to_string c)
  with Stream.Failure -> EOF
