let load_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

let concat_files filenames =
  List.map load_file filenames
  |> String.concat ""

let prompt () =
  print_string ":- ";
  flush stdout

let rec loop_forever thunk =
  thunk ();
  loop_forever thunk

let read_eval_print rules =
  prompt ();
  try
    Parser.reset ();
    let stream = read_line () |> Stream.of_string in
    let query = Parser.query (fun () -> Lexer.get_token stream) in
    Eval.eval rules query
  with
  | Lexer.Lexical_error msg -> Printf.printf "Lexical error: %s\n" msg
  | Parser.Parse_error msg  -> Printf.printf "Parse error: %s\n" msg

let rec repl rules =
  try
    loop_forever (fun () -> read_eval_print rules)
  with
  | End_of_file -> Printf.printf "bye\n"; exit 0
  | Eval.Runtime_error msg -> Printf.printf "Runtime error: %s\n" msg; repl rules

let main () =
  let filenames = ref [] in
  let specs = [] in
  let add_to_filenames = fun s -> filenames := s :: !filenames in
  let usage = "Usage: prolog filename" in
  Arg.parse specs add_to_filenames usage;
  if !filenames = [] then begin Printf.printf "No file given\n%s\n" usage; exit 1 end;
  let stream = concat_files !filenames |> Stream.of_string in
  let rules = Parser.program (fun () -> Lexer.get_token stream) in
  Ast.print_program rules;
  Check_singleton_variable.check_program rules;
  repl rules

let () = main ()
