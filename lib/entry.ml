open Ast

let main () = 
  Parser.pp_exceptions ();
  Parser.parse_string {|(1 + 1 * 3 shl 3 | 9 xor 100 and '1' ++ '2' == '12')|}
  |> sexp_of_expr
  |> Sexplib.Sexp.to_string
  |> print_endline
