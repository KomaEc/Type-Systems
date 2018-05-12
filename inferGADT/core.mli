open Format
open Syntax
val unify : (monoTy * monoTy) list -> substitution option
val gather_subst : type_env -> term -> monoTy -> substitution option
val infer : type_env -> term -> (int -> monoTy) option
val gather_exp_ty_substitution :
  type_env -> term -> monoTy -> (proof * substitution) option
val niceInfer_exp : (type_env -> term -> monoTy -> (proof * substitution) option) ->
  type_env -> term -> string
