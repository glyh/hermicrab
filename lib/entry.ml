open Ast

let rec user_input prompt callback =
  match LNoise.linenoise prompt with
  | None -> ()
  | Some v ->
    callback v;
    user_input prompt callback

let dump_ast source =
  source
  |> Parser.parse_string
  |> sexp_of_expr
  |> Sexplib.Sexp.to_string
  |> print_endline


let main () = 
  Parser.pp_exceptions ();
  dump_ast |> user_input "Hermicrab> "
