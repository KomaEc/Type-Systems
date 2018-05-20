type tyVar = int

type monoTy =
    TyVar of tyVar                               (* type variables *)
  | TyArrow of monoTy * monoTy                   (* arrow type [tau1 -> tau2] *)
  | TyCstr of string * monoTy list               (* constructor [T x1...xn]*)
  | TyTuple of monoTy list
  | TyUnit

type polyTy = tyVar list * monoTy                (* list indicates what types are universally quantified *)

type const =
    TmTrue | TmFalse | TmInt of int | TmUnit

type bin_op =
    PlusOp | MinusOp | TimesOp | DivOp
  | EqOp | GreaterOp | ModOp

type mon_op =
    NegOp


type term =
    TmVar of string                              (* term variables, x, y, ..*)
  | TmFix of term
  | TmCstr of string
  | TmBinOp of bin_op * term * term
  | TmMonOp of mon_op * term
  | TmIf of term * term * term
  | TmFold of term * term
  | TmTuple of term list
  | TmAbs of string * term                       (* lambda x. t *)
  | TmApp of term * term                         (* application *)
  | TmLet of string * term * term                (* let x = t1 in t2, recursive binding *)
  | TmLetAnnot of string * polyTy * term * term  (* let x = t1 : [Type Scheme] in t2, recursive binding *)
  | TmFlatMatchWith of term * (flat_pattern * term) list  (* flat_pattern matching *)
  | TmMatchWith of term * (pattern * term) list (* a pattern matching acts exactly like an unfold *)
  | TmConst of const
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
  | TyTuple tyl ->
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
let mk_pair_ty ty1 ty2 = TyTuple([ty1; ty2])
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
    bndVars, TyArrow(TyTuple((List.map (fun i -> TyVar i) bndVars)), TyVar n)
let fst_ty = label_nth_ty 2 1
let snd_ty = label_nth_ty 2 2
let label_make_n_ty n =
  assert (n > 0);
  let rec gather cur acc =
    if cur = 0 then acc
    else gather (cur - 1) (cur :: acc) in
  let bndVars = gather n [] in
  bndVars, List.fold_right (fun i rev -> TyArrow(TyVar i, rev)) bndVars (TyTuple((List.map (fun i -> TyVar i) bndVars)))
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
  | TyTuple typelist -> TyTuple(List.map (fun t -> substitute ie t) typelist)
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
  | TyTuple mtyl -> TyTuple(app_monoTy_list s mtyl)
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
    | TyTuple l -> List.fold_right fvsfun l r
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
  | TyTuple tyl -> occur_list i tyl
  | _ -> false
and occur_list (i : tyVar) (mtyl : monoTy list) : bool =
  match mtyl with
    [] -> false
  | mty :: tyl -> if occur i mty then true
    else occur_list i tyl

let index2string n =
  let rec aux n acc =
    if n < 26 then n :: acc
    else let r = n mod 26 in
      aux (n / 26) (r :: acc) in
  let refine = function
      [] -> failwith "impossible"
    | [x] -> [x]
    | x :: xs -> (x - 1) :: xs in
  let rec convert = function
      [] -> ""
    | x :: xs -> String.make 1 (Char.chr (x + 97)) ^ (convert xs) in
  convert (refine (aux n []))




    (*
let y = 3 in
  (fun f -> fun y -> f y) (fun x -> x + y);;
*)

let rec termSubst (ts : (string * term) list) = function
    TmVar y -> (try List.assoc y ts with _ -> TmVar y)
  | TmFix t -> termSubst ts t
  | TmFold(t1, t2) -> TmFold(termSubst ts t1, termSubst ts t2)
  | TmApp(t1, t2) -> TmFold(termSubst ts t1, termSubst ts t2)
  | TmAbs(x, t) -> TmAbs(x, termSubst (List.filter (fun (y, _) -> x <> y) ts) t)
  | TmLet(x, t1, t2) -> let ts' = List.filter (fun (y, _) -> x <> y) ts in
    TmLet(x, termSubst ts' t1, termSubst ts' t2)
  | TmFlatMatchWith(t1, fptl) ->
    TmFlatMatchWith(termSubst ts t1,
                    List.map (fun (fp, t') -> fpSubst_ ts fp t') fptl)
  | t -> t
and fpSubst_ (ts : (string * term) list) (fp : flat_pattern)
    (t' : term) : flat_pattern * term =
  match fp with
    FlPVar(x) -> fp, termSubst (List.filter (fun (y, _) -> x <> y) ts) t'
  | FlPCstr(_, namelist) ->
    fp, termSubst (List.filter (fun (y, _) -> not (List.mem y namelist)) ts) t'


let rec freeNamesInTerm = function
    TmVar x -> [x]
  | TmFix t -> freeNamesInTerm t
  | TmFold(t1, t2) -> freeNamesInTerm t1 @ freeNamesInTerm t2
  | TmTuple tl -> List.fold_left (fun acc t -> freeNamesInTerm t @ acc) [] tl
  | TmAbs(x, t) ->
    List.filter (fun y -> x <> y) (freeNamesInTerm t)
  | TmApp(t1, t2) -> freeNamesInTerm t1 @ freeNamesInTerm t2
  | TmLet(x, t1, t2) ->
    List.filter (fun y -> x <> y) (freeNamesInTerm t1) @
    (List.filter (fun y -> x <> y) (freeNamesInTerm t2))
  | TmFlatMatchWith(t, fptl) ->
    let (fps, ts) = List.split fptl in
    freeNamesInTerm t @
    List.fold_left2 (fun acc fp t -> let bvs = freeNamesInFPattern fp in
                      List.filter (fun x -> not (List.mem x bvs)) (freeNamesInTerm t) @ acc)
      [] fps ts
  | _ -> []
and freeNamesInFPattern = function
    FlPVar x -> [x]
  | FlPCstr(_, nl) -> nl


let rename_for (t : term) (fs : string list) =
  let rec first_not_in x fs =
    if List.mem x fs then first_not_in (x ^ "'") fs else x in
  let rec aux fs = function
      TmAbs(x, t') when List.mem x fs ->
      let x' = first_not_in (x ^ "'") fs in TmAbs(x', (aux (x'::fs) (termSubst [x, (TmVar x')] t')))
    | TmAbs(x, t') -> TmAbs(x, aux fs t')
    | TmFix t -> aux fs t
    | TmFold(t1, t2) -> TmFold(aux fs t1, aux fs t2)
    | TmApp(t1, t2) -> TmApp(aux fs t1, aux fs t2)
    | TmLet(x, t1, t2) when List.mem x fs ->
      let x' = first_not_in (x ^ "'") fs in
      TmLet(x', aux (x'::fs) (termSubst [x, (TmVar x')] t1), aux (x'::fs) (termSubst [x, (TmVar x')] t2))
    | TmLet(x, t1, t2) -> TmLet(x, aux fs t1, aux fs t2)
    | TmTuple tl ->
      TmTuple(List.map (fun t -> aux fs t) tl)
    | TmFlatMatchWith(t, fptl) ->
      TmFlatMatchWith(aux fs t, List.map (fun (fp, t) -> aux_fp fs fp t) fptl)
    | t -> t
  and aux_fp fs fp t =
    match fp with
      FlPVar(x) when List.mem x fs ->
      let x' = first_not_in (x ^ "'") fs in
      FlPVar(x'), aux (x'::fs) (termSubst [x, (TmVar x')] t)
    | FlPCstr(cstr_name, namelist) ->
      let (namelist', subst, newvars) = aux_fps fs namelist in
      FlPCstr(cstr_name, namelist'), aux (newvars @ fs) (termSubst subst t)
    | _ -> fp, (aux fs t)
  and aux_fps fs namelist =
    match namelist with
      [] -> [], [], []
    | n::ns -> if List.mem n fs then
        let n' = first_not_in n fs in
        let (nl, sub, nv) = aux_fps (n'::fs) ns in
        (n'::nl, (n, TmVar n')::sub, n'::nv)
      else let (nl, sub, nv) = aux_fps fs ns in
        (n::nl, sub, nv)
  in
  aux fs t

let termSubstOp (x : string) (t : term) (t' : term) : term =
  let fs = delete_duplicates (freeNamesInTerm t @ freeNamesInTerm t') in
  let t'' = rename_for t' fs in
  termSubst [x, t] t''





(* printing *)

let string_of_bin_op = function
     PlusOp  -> " + "
   | MinusOp -> " - "
   | TimesOp -> " * "
   | DivOp -> " / "
   | EqOp  -> " = "
   | GreaterOp -> " > "
   | ModOp   -> "mod"

let string_of_mon_op m =
  match m with NegOp   -> "~"

let string_of_const c =
  match c
  with TmInt n    -> if n < 0 then "~"^string_of_int(abs n) else string_of_int n
     | TmTrue     -> "true"
     | TmFalse    -> "false"
     | TmUnit     -> "()"



let rec string_of_flatpattern = function
    FlPVar x -> x
  | FlPCstr (x, xs) -> x ^ "(" ^ string_of_pvars xs ^ ")"
and string_of_pvars = function
    [] -> ""
  | [n] -> n
  | n :: ns -> n ^ ", " ^ string_of_pvars ns

let rec string_of_term = function
    TmVar x -> x
  | TmCstr x -> x
  | TmConst c -> string_of_const c
  | TmIf(e1,e2,e3)->"if " ^ (string_of_term e1) ^
                       " then " ^ (string_of_term e2) ^
                       " else " ^ (string_of_term e3)
  | TmMonOp (m, t) -> (string_of_mon_op m) ^ " " ^ (paren_string_of_term t)
  | TmBinOp (b,e1,e2) ->
    ((paren_string_of_term e1) ^ " " ^ (string_of_bin_op b)
                         ^ " " ^ (paren_string_of_term e2))
  | TmApp(t1, t2) -> (non_app_paren_string_of_term t1) ^ " " ^ (paren_string_of_term t2)
  | TmFold(t1, t2) -> (non_app_paren_string_of_term t1) ^ " " ^ (paren_string_of_term t2)
  | TmAbs(x, t) -> "lambda " ^ x ^ ". " ^ (string_of_term t)
  | TmLet(x, t1, t2) -> "let " ^ x ^ " = " ^ (string_of_term t1) ^ " in " ^ (string_of_term t2)
  | TmFlatMatchWith(t, mcthl) -> "match " ^ string_of_term t ^ " with " ^ string_of_matches mcthl
  | _ -> ""
and paren_string_of_term t =
  match t with TmVar _ | TmCstr _ -> string_of_term t
             | _ -> "(" ^ string_of_term t ^ ")"
and non_app_paren_string_of_term t =
  match t with TmApp _ -> string_of_term t
             | _ -> paren_string_of_term t
and string_of_matches = function
    [] -> ""
  | (pat, t) :: ms -> " " ^ "|" ^ string_of_flatpattern pat ^ " -> " ^
                      string_of_term t ^ string_of_matches ms
