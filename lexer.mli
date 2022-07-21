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

val print : token -> unit
val line_number : int ref
exception Lexical_error of string
val get_token : char Stream.t -> token
