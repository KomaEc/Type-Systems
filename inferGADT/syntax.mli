open Format

type tyVar = int
type monoTy =
    TyVar of tyVar
  | TyArrow of monoTy * monoTy
  | TyCstr of string * monoTy list
  | TyInt
  | TyBool
  | TyUnit
type polyTy = tyVar list * monoTy
type term =
    TmVar of string
  | TmAbs of string * term
  | TmApp of term * term
  | TmLet of string * term * term
  | TmLetAnnot of string * polyTy * term * term
  | TmMatchWith of term * (pattern * term) list
  | TmMonOp of mon_op * term
  | TmBinOp of bin_op * term * term
  | Tmif of term * term * term
  | TmConst of const
and const = TmTrue | TmFalse | TmNil | TmUnit | TmInt of int
and pattern = PVar of string | PCstr of string * string list
and mon_op = IntNegOp | HdOp | TlOp | FstOp | SndOp
and bin_op =
    IntPlusOp
  | IntMinusOp
  | IntTimesOp
  | IntDivOp
  | EqOp
  | ModOp
  | GreaterOp
  | ConsOp
  | CommaOp
type refinement = (tyVar * monoTy) list
type modifiers = W | R
type 'a env = (string * 'a) list
type substitution = (tyVar * monoTy) list


val drop : 'a -> 'a list -> 'a list
val delete_duplicates : 'a list -> 'a list
val accumulate_freeVarsMonoTy : tyVar list -> monoTy -> tyVar list
val freeVarsMonoTy : monoTy -> tyVar list
val freeVarsPolyTy : polyTy -> tyVar list

type nextuvar = NextUVar of monoTy * uvargenerator
and uvargenerator = unit -> nextuvar
val uvargen : uvargenerator
val fresh : unit -> monoTy
val reset : unit -> unit
val mk_pair_ty : monoTy -> monoTy -> monoTy
val mk_list_ty : monoTy -> monoTy
val monoTy2polyTy : monoTy -> polyTy
val int_op_ty : polyTy
val pair_signature : tyVar list * monoTy
val list_signature : tyVar list * monoTy

val const_signature : const -> polyTy
val binop_signature : bin_op -> polyTy
val monop_signature : mon_op -> tyVar list * monoTy

val freeVarsEnv : ('a * polyTy) list -> tyVar list
val lookup : ('a * 'b) list -> 'a -> 'b option
type type_env = polyTy env
val make_env : string -> 'a -> 'a env
val lookup_env : 'a env -> string -> 'a option
val sum_env : 'a env -> 'a env -> 'a env
val ins_env : 'a env -> string -> 'a -> 'a env
val subst_fun : substitution -> tyVar -> monoTy
val substitute : tyVar * monoTy -> monoTy -> monoTy
val app_monoTy : substitution -> monoTy -> monoTy
val app_monoTy_list : substitution -> monoTy list -> monoTy list
val subst_compose : substitution -> substitution -> substitution
val gen : type_env -> monoTy -> polyTy
val freshInstance : polyTy -> monoTy
val first_not_in : tyVar -> tyVar list -> tyVar list
val alpha_conv : tyVar list -> polyTy -> tyVar list * monoTy
val app_polyTy : substitution -> polyTy -> polyTy
val app_env : substitution -> polyTy env -> polyTy env
val mk_bty_renaming : tyVar -> 'a list -> ('a * tyVar) list * tyVar list

val monoTy_rename_tyvars : (tyVar * tyVar) list -> monoTy -> monoTy
val polyTy_rename_tyvars :
  (tyVar * tyVar) list -> tyVar list * monoTy -> tyVar list * monoTy
val env_rename_tyvars :
  (tyVar * tyVar) list ->
  (tyVar list * monoTy) env -> (tyVar list * monoTy) env
val occur : tyVar -> monoTy -> bool
val occur_list : tyVar -> monoTy list -> bool

val string_of_const : const -> string
val string_of_bin_op : bin_op -> string
val string_of_mon_op : mon_op -> string
val string_of_term : term -> string
val paren_string_of_term : term -> string
val non_app_paren_string_of_term : term -> string
val print_term : term -> unit
val index2charlist : int * int list -> int list
val string_of_typeVar : int -> string
val string_of_monoTy : monoTy -> string
val string_of_polyTy : int list * monoTy -> string
val string_of_env : ('a -> string) -> (string * 'a) list -> string
val string_of_type_env : (string * (int list * monoTy)) list -> string
type judgment = TermJudgment of type_env * term * monoTy
val string_of_judgment : judgment -> string
type proof = Proof of proof list * judgment
val string_of_proof : proof -> string
type consList = (monoTy * monoTy) list
val proof_lift_subst : substitution -> proof -> proof
val proof_rename_tyvars : (int * int) list -> proof -> proof
val get_ty : ('a * 'b) option -> 'a
val get_proof : ('a * 'b) option -> 'b
val string_of_substitution : (int * monoTy) list -> string
