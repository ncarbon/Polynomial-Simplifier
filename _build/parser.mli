
(* The type of tokens. *)

type token = 
  | VAR of (char)
  | TIMES
  | RPAR
  | POW
  | PLUS
  | NUM of (int)
  | MINUS
  | LPAR
  | EOL

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Expr.expr)
