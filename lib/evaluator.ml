open Environment
open Ast
open Typecheck

exception Unreachable
exception UndefinedVariable of ident
exception Unimplemented
exception CallingUncallable of ident * value
exception CallingExternalWithTypedArgs of ident

let bind_args (ids: ident list) (vs: value list) (env: eval_env) : eval_env =
    let expected_argc = List.length ids in
    let actual_argc = List.length vs in
    if expected_argc != actual_argc then
        raise (ArgNumMismatch (expected_argc, actual_argc))
    else
        List.fold_left
        (fun env (id, v) -> 
            Hashtbl.add env.current id v;
            env
        )
        env
        (List.combine ids vs)

let print_chan channel =
  let rec loop () =
      let () = print_endline (input_line channel) in
      loop ()
    in
  try loop ()
  with End_of_file -> close_in channel;;

let rec evaluate (env: eval_env) (ast: expr): value =
  match ast with
  | Val(v) -> v
  | Var(name) -> 
      begin match env_lookup name env with
      | Some(v) -> v
      | None -> raise (UndefinedVariable(name))
      end
  | If(cond, then_clause, else_clause) ->
      begin match (evaluate env cond) with
      | Bool b ->
          let sub_env = new_sub_env env in
          evaluate
            sub_env
            (if b
            then then_clause
            else else_clause)
      | v -> raise (TypeMismatch(TBool, get_value_type v))
      end
  | When(cond, then_clause) -> 
      begin match (evaluate env cond) with
      | Bool b ->
          let sub_env = new_sub_env env in
          if b 
          then (evaluate sub_env then_clause)
          else Unit
      | v -> raise (TypeMismatch(TBool, get_value_type v))
      end
  | MBlock([]) | Block([]) -> Unit
  | Block(e :: exprs) ->
      let sub_env = new_sub_env env in
      List.fold_left (
          fun _ exp -> evaluate sub_env exp
      ) (evaluate sub_env e) exprs
  | MBlock(e :: exprs) ->
      List.fold_left (
          fun _ exp -> evaluate env exp
      ) (evaluate env e) exprs
  | AndThen(lhs, rhs) ->
    begin match evaluate env lhs with
    | Int(0) -> Int(0)
    | _ -> evaluate env rhs
    end
  | OrElse(lhs, rhs) ->
    begin match evaluate env lhs with
    | Int(0) -> evaluate env rhs
    | v -> v
    end
  | Command(name, word_args, typed_args) ->
    begin match env_lookup name env with
    | Some(ProcLam(wids, tids, body)) ->
        let word_args_evaled = (word_args |> List.map (fun w -> Str (evaluate_word_arg env w))) in
        let typed_args_evaled = (typed_args |> List.map (evaluate env)) in
        let env_new = 
            env
            |> new_sub_env
            |> bind_args wids word_args_evaled 
            |> bind_args tids typed_args_evaled 
        in
        evaluate env_new body
    | Some(BuiltInLam(_, _, lam)) -> 
        let word_args_evaled = (word_args |> List.map (evaluate_word_arg env)) in
        let typed_args_evaled = (typed_args |> List.map (evaluate env)) in
            lam word_args_evaled typed_args_evaled
    | Some(_ as v) -> raise (CallingUncallable(name, v))
    | None ->
      if List.length typed_args != 0
      then 
          raise (CallingExternalWithTypedArgs name)
        else
          let word_args_evaled = (word_args |> List.map (evaluate_word_arg env)) in
          let (cmd_stdout, cmd_stdin, cmd_stderr) = 
              Unix.open_process_args_full name (word_args_evaled |> Array.of_list) (Unix.environment ())
          in
            close_out cmd_stdin;
            print_chan cmd_stdout;
            print_chan cmd_stderr;
            Unit
    end
  | _ -> raise Unimplemented
      
and evaluate_word_arg (env: eval_env) (w: word_arg) = 
    match w with
    | Just s -> s
    | Exp exp -> 
        match evaluate env (Command("stringify", [], [exp])) with
        | Str(s) -> s
        | _ -> raise Unreachable

