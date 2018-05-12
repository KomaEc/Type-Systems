
open Format

(*

(* Example for GADTs *)
type _ term =
  | Lit : int -> int term
  | Inc : int term -> int term
  | IsZ : int term -> bool term
  | If : (bool term * 'a term * 'a term) -> 'a term
  | Pair : ('a term * 'b term) -> ('a * 'b) term
  | Fst : ('a * 'b) term -> 'a term
  | Snd : ('a * 'b) term -> 'b term


let rec eval : type a. a term -> a = function
  | Lit i -> i
  | Inc t -> eval t + 1
  | IsZ t -> eval t = 0
  | If (b, t, e) -> if eval b then eval t else eval e
  | Pair (a, b) -> (eval a, eval b)
  | Fst t -> fst (eval t)
  | Snd t -> snd (eval t)

let f : type a. a term -> a -> int = fun x y ->
  (* could've been annotated with [a term -> int -> int]
   * neither is more general than the other *)
  match x with
  | Lit i -> i + y
  | _ -> 0

*)






type tyVar = int

type monoTy =
    TyVar of tyVar                               (* type variables *)
  | TyArrow of monoTy * monoTy                   (* arrow type [tau1 -> tau2] *)
  | TyCstr of string * monoTy list               (* constructor [T x1...xn]*)
  | TyInt
  | TyBool
  | TyUnit

type polyTy = tyVar list * monoTy                (* list indicates what types are universally quantified *)

type term =
    TmVar of string                              (* term variables, x, y, ..*)
  | TmAbs of string * term                       (* lambda x. t *)
  | TmApp of term * term                         (* application *)
  | TmLet of string * term * term                (* let x = t1 in t2, recursive binding *)
  | TmLetAnnot of string * polyTy * term * term  (* let x = t1 : [Type Scheme] in t2, recursive binding *)
  | TmMatchWith of term * (pattern * term) list  (* pattern matching *)
  | TmMonOp of mon_op * term                     (* $ t where $ is a monadic operator *)
  | TmBinOp of bin_op * term * term                     (* t1 $ t2 where $ is a binary operator *)
  | Tmif of term * term * term                   (* if t1 then t2 else t2 *)
  | TmConst of const
and const =
  | TmTrue | TmFalse | TmNil | TmUnit | TmInt of int
and pattern =
    PVar of string                               (* single variable binding *)
  | PCstr of string * (string list)              (* constructor binding *)
and mon_op =
    IntNegOp | HdOp | TlOp | FstOp | SndOp
and bin_op =  | IntPlusOp  | IntMinusOp  | IntTimesOp  | IntDivOp  | EqOp
  | ModOp  | GreaterOp  | ConsOp  | CommaOp

type refinement = (tyVar * monoTy) list
type modifiers = W | R
type 'a env = (string * 'a) list

type substitution = (tyVar * monoTy) list



(* helper functions *)
let rec drop y = function
    [] -> []
  | x::xs -> if x = y then drop y xs
    else x :: drop y xs

let rec delete_duplicates = function
    [] -> []
  | x::xs -> x :: delete_duplicates (drop x xs)

(* free variables counting *)
let rec accumulate_freeVarsMonoTy fvs ty =
  match ty with
    TyVar n -> n :: fvs
  | TyArrow (ty1, ty2) ->
    List.fold_left accumulate_freeVarsMonoTy fvs [ty1; ty2]
  | TyCstr (x, tyl) ->
    List.fold_left accumulate_freeVarsMonoTy fvs tyl
  | _ -> []

let freeVarsMonoTy ty = delete_duplicates (accumulate_freeVarsMonoTy [] ty)

let freeVarsPolyTy ((tvs, ty) : polyTy) = delete_duplicates
    (List.filter (fun x -> not (List.mem x tvs)) (freeVarsMonoTy ty))



(* generate freeVars *)

type nextuvar = NextUVar of monoTy * uvargenerator
and uvargenerator = unit -> nextuvar

let uvargen =
  let rec f n () = NextUVar (TyVar n, f (n + 1))
  in f 0

(* alternatives *)
let (fresh, reset) =
  let nxt = ref 0 in
  let f () = (nxt := !nxt + 1; TyVar(!nxt)) in
  let r () = nxt := 0 in
  (f, r)

(* built-in type constructors *)
let mk_pair_ty ty1 ty2 = TyCstr("pair",[ty1;ty2])
let mk_list_ty ty = TyCstr("list",[ty])

(* built-in type signatures *)
let monoTy2polyTy mty = (([], mty) : polyTy)
let int_op_ty = monoTy2polyTy(TyArrow(TyInt, TyArrow(TyInt, TyInt)))

let pair_signature =                                      (* Constructor signatures *)
  let ty1, ty2 = TyVar 0, TyVar 1 in
  ([0; 1], TyArrow(ty1, TyArrow(ty2, mk_pair_ty ty1 ty2)))

let list_signature =                                      (* Constructor signatures *)
  let ty = TyVar 0 in
  ([0], TyArrow(ty, mk_list_ty ty))

let const_signature const =
  match const with
    TmTrue | TmFalse -> monoTy2polyTy TyBool
  | TmNil -> ([0], mk_list_ty (TyVar 0))
  | TmUnit -> monoTy2polyTy TyUnit
  | TmInt _ -> monoTy2polyTy TyInt

let binop_signature binop = match binop with
    IntPlusOp | IntMinusOp | IntTimesOp | IntDivOp | ModOp -> int_op_ty
  | GreaterOp -> monoTy2polyTy(TyArrow(TyInt, TyArrow(TyInt, TyBool)))
  | EqOp ->
    let alpha = TyVar 0
    in ([0],
        TyArrow(alpha, TyArrow(alpha, TyBool)))
  | ConsOp ->
    let alpha = TyVar 0
    in ([0],
        TyArrow(alpha, TyArrow(mk_list_ty alpha, mk_list_ty alpha)))
  | CommaOp ->
    let alpha = TyVar 0 in
    let beta = TyVar 1 in
    ([0; 1],
     TyArrow(alpha, TyArrow(alpha, mk_pair_ty alpha beta )))

let monop_signature monop = match monop with
    HdOp ->
    let alpha = TyVar 0 in
    ([0], TyArrow(mk_list_ty alpha, alpha))
  | TlOp ->
    let alpha = TyVar 0 in
    ([0], TyArrow(mk_list_ty alpha, mk_list_ty alpha))
  | IntNegOp ->
    ([], TyArrow(TyInt, TyInt))
  | FstOp ->
    let ty1, ty2 = TyVar 0, TyVar 1 in
    ([0; 1], TyArrow(mk_pair_ty ty1 ty2, ty1))
  | SndOp ->
    let ty1, ty2 = TyVar 0, TyVar 1 in
    ([0; 1], TyArrow(mk_pair_ty ty1 ty2, ty2))


(* operations on enviroment *)
let freeVarsEnv l = delete_duplicates (
    List.fold_right (fun (_,pty) fvs -> freeVarsPolyTy pty @ fvs) l [])

let rec lookup mapping x =
  match mapping with
    []        -> None
  | (y,z)::ys -> if x = y then Some z else lookup ys x

type type_env = polyTy env

let make_env x y = ([(x,y)]:'a env)
let lookup_env (gamma:'a env) x = lookup gamma x
let sum_env (delta:'a env) (gamma:'a env) = ((delta@gamma):'a env)
let ins_env (gamma:'a env) x y = sum_env (make_env x y) gamma

(* operations on substitution *)
let rec substitute (ie : tyVar * monoTy) (ty : monoTy) : monoTy =
  let n,sub = ie
  in match ty with
    TyVar m -> if n=m then sub else ty
  | TyCstr(st, typelist) -> TyCstr(st, List.map (fun t -> substitute ie t) typelist)
  | TyArrow(ty1, ty2) -> TyArrow(substitute ie ty1, substitute ie ty2)
  | _ -> ty

let subst_fun (s : substitution) : tyVar -> monoTy =
  fun i ->
    try let res_ty = List.assoc i s in
      res_ty
    with Not_found -> TyVar i

let rec app_monoTy (s : substitution) : monoTy -> monoTy = function
    TyVar i -> subst_fun s i
  | TyArrow(ty1, ty2) -> TyArrow(app_monoTy s ty1, app_monoTy s ty2)
  | TyCstr(c, mtyl) -> TyCstr(c, app_monoTy_list s mtyl)
  | mty -> mty
and app_monoTy_list (s : substitution) : monoTy list -> monoTy list = function
    [] -> []
  | mty :: mtyl -> app_monoTy s mty :: app_monoTy_list s mtyl

let subst_compose (s2 : substitution) (s1 : substitution) : substitution =
  (List.filter (fun (tv,_) -> not(List.mem_assoc tv s1)) s2) @
  (List.map (fun (tv,residue) -> (tv, app_monoTy s2 residue)) s1)

let gen (env : type_env) ty =
  let env_fvs = freeVarsEnv env in
  ((List.filter (fun v -> not (List.mem v env_fvs)) (freeVarsMonoTy ty), ty) : polyTy)


let freshInstance ((tvs, ty):polyTy) =
  let fresh_subst = List.fold_right (fun tv s -> ((tv,fresh())::s)) tvs [] in
  app_monoTy fresh_subst ty

let first_not_in n l =
  let rec first m n l =
    if n > 0 then
      if List.mem m l then first (m+1) n l else m :: (first (m+1) (n - 1) l)
    else []
  in first 0 n l

let alpha_conv ftvs (pty:polyTy) =
  match pty with (btvs, ty) ->
    (let fresh_bvars =
       first_not_in (List.length btvs) (ftvs @ (freeVarsPolyTy pty))
     in (fresh_bvars,
         app_monoTy (List.combine btvs (List.map (fun v -> TyVar v) fresh_bvars))
           ty))

let app_polyTy s pty =
  let rec fvsfun x r = match x with
    | TyVar n -> n :: r
    | TyCstr(_, l) -> List.fold_right fvsfun l r
    | TyArrow(ty1, ty2) -> List.fold_right fvsfun [ty1; ty2] r
    | _ -> r
  in
  let fvs = List.fold_right fvsfun (snd(List.split s)) [] in
  let (nbvs, nty) = alpha_conv fvs pty in
  ((nbvs, app_monoTy s nty):polyTy)

let app_env s (env:'a env) =
  ((List.map (fun (x,polyTy) -> (x,app_polyTy s polyTy)) env):'a env)


let rec mk_bty_renaming n bty =
  match bty with [] -> ([],[])
               | (x::xs) -> (match mk_bty_renaming (n-1) xs
                             with (s,l) -> (((x,n) :: s), n :: l))

let rec monoTy_rename_tyvars s mty =
  match mty with
    TyVar n -> (match lookup s n with Some m -> TyVar m | _ -> mty)
  | TyCstr(c, tys) -> TyCstr(c, List.map (monoTy_rename_tyvars s) tys)
  | TyArrow(ty1, ty2) -> TyArrow(monoTy_rename_tyvars s ty1, monoTy_rename_tyvars s ty2)
  | mty -> mty

let polyTy_rename_tyvars s (bty, mty) =
  let (renaming,new_bty) = mk_bty_renaming (~-7) bty in
  (new_bty, monoTy_rename_tyvars s (monoTy_rename_tyvars renaming mty))

let env_rename_tyvars s (env: 'a env) =
  ((List.map
      (fun (x,polyTy) -> (x,polyTy_rename_tyvars s polyTy)) env): 'a env)



let rec occur (i : tyVar) (mty : monoTy) : bool =
  match mty with
    TyVar j -> i = j
  | TyArrow(ty1, ty2) -> occur i ty1 || occur i ty2
  | TyCstr(c, tyl) -> occur_list i tyl
  | _ -> false
and occur_list (i : tyVar) (mtyl : monoTy list) : bool =
  match mtyl with
    [] -> false
  | mty :: tyl -> if occur i mty then true
    else occur_list i tyl


(* Printing *)
let string_of_const c =
  match c with
    TmTrue -> "true"
  | TmFalse -> "false"
  | TmInt n -> if n < 0 then "~" ^ string_of_int (abs n) else string_of_int n
  | TmUnit -> "()"
  | TmNil -> "[]"

let string_of_bin_op = function
    IntPlusOp  -> " + "
  | IntMinusOp -> " - "
  | IntTimesOp -> " * "
  | IntDivOp -> " / "
  | ConsOp -> " :: "
  | CommaOp -> " , "
  | EqOp  -> " = "
  | GreaterOp -> " > "
  | ModOp   -> "mod"

let string_of_mon_op m =
  match m with HdOp  -> "hd"
             | TlOp  -> "tl"
             | IntNegOp   -> "~"
             | FstOp   -> "fst"
             | SndOp   -> "snd"

let rec string_of_term = function
    TmVar x -> x
  | TmConst c -> string_of_const c
  | Tmif(t1, t2, t3) -> "if " ^ (string_of_term t1) ^
                        " then " ^ (string_of_term t2) ^
                        " else " ^ (string_of_term t3)
  | TmMonOp(mop, t) -> (string_of_mon_op mop) ^ " " ^ (paren_string_of_term t)
  | TmBinOp(bop, t1, t2) -> (match bop with
        CommaOp -> ("(" ^ (paren_string_of_term t1) ^ (string_of_bin_op bop) ^
                    (paren_string_of_term t2) ^ ")")
      | _ -> (paren_string_of_term t1) ^ " " ^ (string_of_bin_op bop) ^
             " " ^ (paren_string_of_term t2))
  | TmApp(t1, t2) -> (non_app_paren_string_of_term t1) ^ " " ^ (paren_string_of_term t2)
  | TmAbs(x, t) -> "lambda " ^ x ^ ". " ^ (string_of_term t)
  | TmLet(x, t1, t2) -> "let " ^ x ^ " = " ^ (string_of_term t1) ^ " in " ^ (string_of_term t2)
  | _ -> ""
and paren_string_of_term t =
  match t with TmVar _ | TmConst _ -> string_of_term t
             | _ -> "(" ^ string_of_term t ^ ")"
and non_app_paren_string_of_term t =
  match t with TmApp _ -> string_of_term t
             | _ -> paren_string_of_term t

let print_term t = print_string (string_of_term t)

let rec index2charlist (n, acc) =
  let q = n / 26 in
  if q = 0 then n :: acc
  else index2charlist (q, (n mod 26) :: acc)

let string_of_typeVar n =
  let cl =
    match index2charlist (n, []) with
    [] -> failwith "impossible"
    | [c] -> [c]
  | c :: cs -> (c - 1) :: cs in
  let s =
    List.fold_left (fun acc c -> acc ^ (String.make 1 (Char.chr (c + 97)))) "" cl in
  "'" ^ s


let rec string_of_monoTy t =
  let rec string_of_tylist = function
      [] -> ""
    | t'::[] -> string_of_monoTy t'
    | t'::ts -> string_of_monoTy t' ^ "," ^ string_of_tylist ts
  in
  let string_of_subty s =
    match s with
      TyCstr ("pair", _) | TyArrow _ -> ("(" ^ string_of_monoTy s ^ ")")
    | _ -> string_of_monoTy s
  in
  match t with
    TyVar n -> string_of_typeVar n
  | TyCstr(name, []) -> name
  | TyCstr(name, [ty]) -> string_of_subty ty ^ " " ^ name
  | TyCstr("pair", [ty1; ty2]) -> (string_of_subty ty1 ^ " * " ^ string_of_subty ty2)
  | TyArrow(ty1, ty2) -> string_of_subty ty1 ^ " -> " ^ string_of_monoTy ty2
  | TyCstr(name, tys) -> ("(" ^ string_of_tylist tys ^ ")" ^ name)
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyUnit -> "unit"

let string_of_polyTy (bndVars, t) = match bndVars with
    [] -> string_of_monoTy t
  | _ -> (List.fold_left
            (fun s v -> s ^ " " ^ string_of_typeVar v)
           "Forall"
           bndVars)
         ^ ". " ^ string_of_monoTy t

let string_of_env string_of_entry gamma =
  let rec string_of_env_aux gamma =
    match gamma with
      [] -> ""
    | (x, y) :: xs -> x ^ " : " ^ string_of_entry y ^
                      match xs with [] -> "" |  _ -> ", " ^ string_of_env_aux xs
  in
  "{" ^ string_of_env_aux gamma ^ "}"

let string_of_type_env gamma = string_of_env string_of_polyTy gamma

(* judgment for printing *)
type judgment =
    TermJudgment of type_env * term * monoTy

let string_of_judgment judgment =
  match judgment with TermJudgment(gamma, term, monoTy) ->
    string_of_type_env gamma ^ " |- " ^ string_of_term term ^
    " : " ^ string_of_monoTy monoTy

type proof = Proof of proof list * judgment

let string_of_proof p =
  let depth_max = 10 in
  let rec string_of_struts = function
     []    -> ""
    | x::[] -> "|-"
   | x::xs -> (if x then "  " else "| ")^ string_of_struts xs
  in let rec string_of_proof_aux (Proof(ant,conc)) depth lst =
    "\n"^ "  "^ string_of_struts lst^
    (if (depth > 0) then "-" else "")^
    let assum = ant in
      string_of_judgment conc ^
      if depth <= depth_max
         then string_of_assum depth lst assum
      else ""
  and string_of_assum depth lst assum =
    match assum with
       []     -> ""
     | p'::ps -> string_of_proof_aux p' (depth + 1) (lst@[ps=[]])^
                 string_of_assum depth lst ps
  in
  string_of_proof_aux p 0 []^ "\n\n"


  type consList = (monoTy * monoTy) list



let rec proof_lift_subst f = function
    Proof(assum, TermJudgment(gamma, exp, monoTy)) ->
    Proof(List.map (proof_lift_subst f) assum,
          TermJudgment(app_env f gamma, exp, app_monoTy f monoTy))

let rec proof_rename_tyvars f = function
    Proof(assum, TermJudgment(gamma, exp, monoTy)) ->
    Proof(List.map (proof_rename_tyvars f) assum,
          TermJudgment(env_rename_tyvars f gamma, exp,
                      monoTy_rename_tyvars f monoTy))

let get_ty = function
    None       -> raise(Failure "None")
  | Some(ty,p) -> ty

let get_proof = function
    None       -> raise(Failure "None")
  | Some(ty,p) -> p



let string_of_substitution s =
  let rec aux s =
    match s with
    | [] -> ""
    | [(i,t)] -> ((string_of_typeVar i)  ^ " --> " ^ string_of_monoTy t)
    | (i,t)::s' -> (((string_of_typeVar i)  ^ " --> ")^
                    string_of_monoTy t^ "; "^ aux s')
  in ("["^ aux s^ "]\n")
