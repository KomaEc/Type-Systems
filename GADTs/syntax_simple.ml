type tyVar = int

type monoTy =
    TyVar of tyVar                               (* type variables *)
  | TyArrow of monoTy * monoTy                   (* arrow type [tau1 -> tau2] *)
  | TyCstr of string * monoTy list               (* constructor [T x1...xn]*)
  | TyUnit

type polyTy = tyVar list * monoTy                (* list indicates what types are universally quantified *)

type term =
    TmVar of string                              (* term variables, x, y, ..*)
  | TmAbs of string * term                       (* lambda x. t *)
  | TmApp of term * term                         (* application *)
  | TmLet of string * term * term                (* let x = t1 in t2, recursive binding *)
  | TmLetAnnot of string * polyTy * term * term  (* let x = t1 : [Type Scheme] in t2, recursive binding *)
  | TmFlatMatchWith of term * (flat_pattern * term) list  (* flat_pattern matching *)
  | TmMatchWith of term * (pattern * term) list
  | TmUnit
and flat_pattern =
    FlPVar of string                               (* single variable binding *)
  | FlPCstr of string * (string list)              (* constructor binding *)
and pattern =
    PVar of string
  | PCstr of string * pattern list

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
  in f 1

(* primitive type constructors *)
(* [type 'a list = Nil of Unit | Cons of 'a * 'a list]
 * [type ('a, 'b, 'c, ...) tuple = {1 : 'a, 2 : 'b,  ...}]
 * and [make_n : 'a -> 'b -> ... -> ('a, 'b, ...) tuple]
 **)
let mk_list_ty ty = TyCstr("list",[ty])
let mk_pair_ty ty1 ty2 = TyCstr("tuple", [ty1; ty2])
let label_Nil_ty =
  let alpha = TyVar 0 in
  [0], TyArrow(TyUnit, mk_list_ty alpha)
let label_Cons_ty =
  let alpha = TyVar 0 in
  [0], TyArrow(mk_pair_ty alpha (mk_list_ty alpha), mk_list_ty alpha)
let label_nth_ty tplen n =
  let rec gather curlen acc =
    if curlen = 0 then acc
    else gather (curlen - 1) (curlen :: acc) in
  let bndVars = gather tplen [] in
  if tplen < n then assert false
  else
    bndVars, TyArrow(TyCstr("tuple", (List.map (fun i -> TyVar i) bndVars)), TyVar n)
let fst_ty = label_nth_ty 2 1
let snd_ty = label_nth_ty 2 2
let label_make_n_ty n =
  assert (n > 0);
  let rec gather cur acc =
    if cur = 0 then acc
    else gather (cur - 1) (cur :: acc) in
  let bndVars = gather n [] in
  bndVars, List.fold_right (fun i rev -> TyArrow(TyVar i, rev)) bndVars (TyCstr("tuple", (List.map (fun i -> TyVar i) bndVars)))
let pair_ty = label_make_n_ty 2


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

let monoTy2polyTy mty = (([], mty) : polyTy)

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

let fresh_Instance ((tvs, ty):polyTy) (fresh : uvargenerator) =
  let rec fresh_aux tvs acc fresh =
    match tvs with
      [] -> acc, fresh
    | t :: ts -> let (NextUVar(tau, fresh)) = fresh () in
      fresh_aux ts ((t, tau)::acc) fresh in
  let sub, fresh = fresh_aux tvs [] fresh in
  (app_monoTy sub ty, fresh)

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
  let (nbvs, nty) = alpha_conv (fst(List.split s) @ fvs) pty in
  ((nbvs, app_monoTy s nty):polyTy)

let app_env s (env:'a env) =
  ((List.map (fun (x,polyTy) -> (x,app_polyTy s polyTy)) env):'a env)

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
