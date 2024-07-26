open Ast

type value_type =
  | TUnit
  | TInt
  | TBool
  | TStr
  | TLambda of int * int (* number of word args and type args *)

exception TypeMismatch of value_type * value_type
exception ArgNumMismatch of int * int
exception DontNeedWordArgs of ident
exception CantCompare of value_type * value_type

let get_value_type (v: value) =
  match v with
  | Unit -> TUnit
  | Int _ -> TInt
  | Bool _ -> TBool
  | Str _ -> TStr
  | ProcLam(wids, tids, _) -> TLambda (List.length wids, List.length tids)
  | BuiltInLam(widc, tidc, _) -> TLambda (widc, tidc)
