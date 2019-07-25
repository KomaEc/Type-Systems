
type level = int
type name = string

type ty = 
  | TConst of type_const
  | TVar of tv ref
  | TArrow of ty * ty
  [@@deriving sexp, show]
and tv = Unbound of string * level | Link of ty [@@deriving sexp, show]
(* and levels = { mutable level_old : level; mutable level_new : level } [@@deriving sexp, show] *)
and type_const = name (* currently, type constant is represented by name  *)
    [@@deriving sexp, show]

type term = 
  | Var of name
  | Lam of name * term
  | Call of term * term
  | Let of name * term * term
  [@@deriving sexp, show]
