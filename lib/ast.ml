open Base

type placeholder = unit
  [@@deriving sexp]

type ident = string
  [@@deriving sexp]

type binop =
  | Pipe
  | AndThen
  | OrElse

  | Add
  | Sub
  | Mul
  | Div
  | Rem

  | Eq
  | Ne
  | Le
  | Ge
  | Lt
  | Gt

  | LAnd
  | LOr

  | BShiftL
  | BShiftR
  | BXor
  | BAnd
  | BOr
  
  | SConcat
  [@@deriving sexp]


type unop =
  | Not
  [@@deriving sexp]

type redirector = placeholder
  [@@deriving sexp]

type value = 
  | Unit
  | Int of int
  | Str of string
  (* typed args -> body *)
  | ProcLambda of ident list * expr
  [@@deriving sexp]

and expr =
  | Var of ident
  | Val of value
  | Unary of unop * expr
  | If of expr * expr * expr
  | When of expr * expr
  | For of ident * expr * expr
  | Block of expr list
  (* 
     Mergable blocks, which doesn't create any scopes,
     for the ease of writing a parser
  *)
  | MBlock of expr list
  | Binary of binop * expr * expr
  | Assign of ident * expr * bool (* true: is declare, false: not declare *)
  | Command of string * word_arg list * expr list
  | WithRedirection of expr * redirector
  [@@deriving sexp]

and word_arg = 
  | Just of string
  | Exp of expr
  [@@deriving sexp]

