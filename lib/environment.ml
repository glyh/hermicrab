open Ast
open Typecheck

type eval_env = {
  outer: eval_env option;
  current: (ident, value) Hashtbl.t
}

let rec env_lookup (symbol: ident) (env: eval_env): value option = 
  match (Hashtbl.find_opt env.current symbol), env.outer with
  | Some(v), _ -> Some v 
  | None, Some(outer) -> env_lookup symbol outer
  | None, None -> None

let new_sub_env (outer: eval_env) = 
  {
    outer = Some outer;
    current = Hashtbl.create 10
  }

let v_numeric id op ws vs =
  if ws != [] 
  then
    raise (DontNeedWordArgs id)
  else
    match vs with
    | [Int a; Int b] -> Int (op a b)
    | [Int _; t] | [t; _] -> raise (TypeMismatch(TInt, get_value_type t))
    | args -> raise (ArgNumMismatch(2, List.length args))

let v_add = v_numeric "+" (+)
let v_sub = v_numeric "-" (-)
let v_mul = v_numeric "-" ( * )
let v_div = v_numeric "/" (/)
let v_mod = v_numeric "mod" (mod)

let v_shl = v_numeric "shl" (Int.shift_left)
let v_shr = v_numeric "shr" (Int.shift_right)

let v_compare id (f: int -> bool) ws vs =
  if ws != [] 
  then
    raise (DontNeedWordArgs id)
  else
    match vs with
    | [Unit; Unit] -> Bool true
    | [Int a; Int b] -> Bool (f (compare a b))
    | [Str a; Str b] -> Bool (f (compare a b))
    | [Bool a; Bool b] -> Bool (f (compare a b))
    | [ProcLam _ | BuiltInLam _ as lhs; _ as rhs] | [_ as lhs; ProcLam _ | BuiltInLam _ as rhs] -> 
        raise (CantCompare(get_value_type lhs, get_value_type rhs))
    | args -> raise (ArgNumMismatch(2, List.length args))

let v_eq = v_compare "==" (fun x -> x == 0)
let v_ne = v_compare "!=" (fun x -> x != 0)
let v_le = v_compare "<=" (fun x -> x <= 0)
let v_ge = v_compare ">=" (fun x -> x >= 0)
let v_lt = v_compare "<" (fun x -> x < 0)
let v_gt = v_compare ">" (fun x -> x > 0)

let v_boolean id (fb: bool -> bool -> bool) (fi: int -> int -> int) ws vs =
  if ws != [] 
  then
    raise (DontNeedWordArgs id)
  else
    match vs with
    | [Bool a; Bool b] -> Bool (fb a b)
    | [Bool _; _ as b] -> raise (TypeMismatch(TBool, get_value_type b))
    | [Int a; Int b] -> Int (fi a b)
    | [Int _; _ as b] -> raise (TypeMismatch(TInt, get_value_type b))
    | [_ as b; _] -> raise (TypeMismatch(TBool, get_value_type b))
    | args -> raise (ArgNumMismatch(2, List.length args))

let v_and = v_boolean "and" (&&) Int.logand
let v_or = v_boolean "or" (||) Int.logor
let v_xor = v_boolean "xor" (!=) Int.logxor

let v_concat ws vs =
  if ws != [] 
  then
    raise (DontNeedWordArgs "++")
  else
    match vs with
    | [Str a; Str b] -> Str (a ^ b)
    | [Str _; _ as b] | [_ as b; _] -> raise (TypeMismatch(TStr, get_value_type b))
    | args -> raise (ArgNumMismatch(2, List.length args))

let init_env = {
  outer = None;
  current = [
    "+", BuiltInLam(0, 2, v_add);
    "-", BuiltInLam(0, 2, v_sub);
    "*", BuiltInLam(0, 2, v_mul);
    "/", BuiltInLam(0, 2, v_div);
    "mod", BuiltInLam(0, 2, v_mod);

    "==", BuiltInLam(0, 2, v_eq);
    "!=", BuiltInLam(0, 2, v_ne);
    "<=", BuiltInLam(0, 2, v_le);
    ">=", BuiltInLam(0, 2, v_ge);
    "<",  BuiltInLam(0, 2, v_lt);
    ">",  BuiltInLam(0, 2, v_gt);

    "and", BuiltInLam(0, 2, v_and);
    "or", BuiltInLam(0, 2, v_or);
    "xor", BuiltInLam(0, 2, v_xor);

    "shl", BuiltInLam(0, 2, v_shl);
    "shr", BuiltInLam(0, 2, v_shr);

    "++", BuiltInLam(0, 2, v_concat);

  ] |> List.to_seq |> Hashtbl.of_seq
}
