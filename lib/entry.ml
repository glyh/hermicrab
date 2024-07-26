open Ast
open Environment

let rec user_input prompt callback =
  match LNoise.linenoise prompt with
  | None -> ()
  | Some v ->
    callback v;
    user_input prompt callback

let repl_env = new_sub_env init_env

let dump_ast source =
  let parsed = Parser.parse_string source in
  let evaled = Evaluator.evaluate repl_env parsed in
  evaled
  |> sexp_of_value
  |> Sexplib.Sexp.to_string
  |> print_endline


let main () = 
  Parser.pp_exceptions ();
  dump_ast |> user_input "Hermicrab> "
