open Base

type placeholder = unit
  [@@deriving sexp]

type ident = string
  [@@deriving sexp]

type redirector = placeholder
  [@@deriving sexp]

type value = 
  | Unit
  | Int of int
  | Bool of bool
  | Str of string
  (* word args -> typed args -> body *)
  | ProcLam of ident list * ident list * expr
  | BuiltInLam of int * int * (string list -> value list -> value)
  [@@deriving sexp]

and expr =
  | Var of ident
  | Val of value
  | Pipe of expr * expr
  | AndThen of expr * expr
  | OrElse of expr * expr
  | If of expr * expr * expr
  | When of expr * expr
  | For of ident * expr * expr
  | Block of expr list
  (* 
     Mergable blocks, which doesn't create any scopes,
     for the ease of writing a parser
  *)
  | MBlock of expr list
  | Assign of ident * expr * bool (* true: is declare, false: not declare *)
  | Command of string * word_arg list * expr list
  | WithRedirection of expr * redirector
  [@@deriving sexp]

and word_arg = 
  | Just of string
  | Exp of expr
  [@@deriving sexp]

