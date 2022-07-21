exception Parse_error of string
val reset   : unit -> unit
val program : (unit -> Lexer.token) -> Type.program
val query   : (unit -> Lexer.token) -> Type.query
