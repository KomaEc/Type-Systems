open Syntax_simple

(* unification algorithm *)
let rec unify eqlst : substitution option =
  match eqlst with
    [] -> Some []
  | (s, t) :: eqs when s = t -> unify eqs
  | (TyVar(n), t) :: eqs when not (occur n t) ->
    let eqs' = List.map (fun (t1, t2) -> (substitute (n, t) t1, substitute (n, t) t2)) eqs
    in (match unify eqs' with
          None -> None
        | Some(phi) -> Some ((n, app_monoTy phi t) :: phi))
  | (s, TyVar(m)) :: eqs -> unify ((TyVar(m), s) :: eqs)
  | (TyCstr(c, tl), TyCstr(c', tl')) :: eqs when c = c' ->
    (try let new_eq = List.fold_left2 (fun acc t t'-> (t, t') :: acc) eqs tl tl' in
       unify new_eq
     with Invalid_argument _ -> None)
  | (TyTuple(tl), TyTuple(tl')) :: eqs ->
    (try let new_eq = List.fold_left2 (fun acc t t'-> (t, t') :: acc) eqs tl tl' in
       unify new_eq
     with Invalid_argument _ -> None)
  | (TyArrow(ty1, ty2), TyArrow(ty1', ty2')) :: eqs ->
    unify ((ty2, ty2') :: (ty1, ty1') :: eqs)
  | _ -> None

(* type inference algorithm -- algorithm J *)
(* algorithm properties : [Gamma |- t : tau | sigma] iff
   [sigma_Gamma |- t : sigma_tau] *)
let rec gather_term_subst (gamma : type_env) (t : term) (tau : monoTy)
    (fresh : uvargenerator) : substitution option * uvargenerator =
  match t with
    TmUnit -> unify [(tau, TyUnit)], fresh
  | TmVar x ->
    (match lookup_env gamma x with
       None -> (None, fresh)
     | Some tau' ->
       (let (fresh_tau', fresh) = fresh_Instance tau' fresh in
        unify [(tau, fresh_tau')], fresh))
  | TmCstr x ->
    (match lookup_env gamma x with
       None -> (None, fresh)
     | Some tau' ->
       (let (fresh_tau', fresh) = fresh_Instance tau' fresh in
        unify [(tau, fresh_tau')], fresh))
  | TmAbs (x, t) ->
    let (NextUVar(tau1, fresh)) = fresh () in
    let (NextUVar(tau2, fresh)) = fresh () in
    (match gather_term_subst
             (ins_env gamma x (monoTy2polyTy tau1)) t tau2 fresh with
      None, fresh -> (None, fresh)
    | Some sigma, fresh ->
      (match unify [(app_monoTy sigma tau,
                     app_monoTy sigma (TyArrow(tau1, tau2)))] with
        None -> (None, fresh)
      | Some sigma' -> Some (subst_compose sigma' sigma), fresh))
  | TmApp (t1, t2) ->
    let (NextUVar(tau1, fresh)) = fresh () in
    (match gather_term_subst gamma t1 (TyArrow(tau1, tau)) fresh with
       None, fresh -> (None, fresh)
     | Some sigma, fresh ->
       (match gather_term_subst
                (app_env sigma gamma)
                t2
                (app_monoTy sigma tau1) fresh with
         None, fresh -> (None, fresh)
       | Some sigma', fresh ->
         Some (subst_compose sigma' sigma), fresh))
  | TmFold (t1, t2) ->
    let (NextUVar(tau1, fresh)) = fresh () in
    (match gather_term_subst gamma t1 (TyArrow(tau1, tau)) fresh with
       None, fresh -> (None, fresh)
     | Some sigma, fresh ->
       (match gather_term_subst
                (app_env sigma gamma)
                t2
                (app_monoTy sigma tau1) fresh with
         None, fresh -> (None, fresh)
       | Some sigma', fresh ->
         Some (subst_compose sigma' sigma), fresh))
  | TmLet (x, t1, t2) ->
    let (NextUVar(tau1, fresh)) = fresh () in
    (match gather_term_subst
             (ins_env gamma x (monoTy2polyTy tau1)) t1 tau1 fresh with
      None, fresh -> (None, fresh)
    | Some sigma1, fresh ->
      let sigma1_gamma = app_env sigma1 gamma in
      let sigma1_tau1 = app_monoTy sigma1 tau1 in
      let sigma1_tau = app_monoTy sigma1 tau in
      (match gather_term_subst
               (ins_env sigma1_gamma
                  x (gen sigma1_gamma sigma1_tau1)) t2
               sigma1_tau fresh with
        None, fresh -> (None, fresh)
      | Some sigma2, fresh -> Some (subst_compose sigma2 sigma1), fresh))
  | TmTuple tl ->
    (match gather_iter_terms gamma tl [] [] fresh with
       None, fresh -> None, fresh
     | Some (sigma, tyl), fresh ->
       match unify [(app_monoTy sigma tau, TyTuple(app_monoTy_list sigma tyl))] with
         None -> None, fresh
       | Some sigma' -> Some sigma', fresh)
  | TmFlatMatchWith (t_s, fptl) ->
    let (NextUVar(taup, fresh)) = fresh () in
    (match gather_term_subst gamma t_s taup fresh with
       None, fresh -> (None, fresh)
     | Some sigma1, fresh ->
       gather_matches_subst (app_env sigma1 gamma) fptl
         (app_monoTy sigma1 taup) (app_monoTy sigma1 tau) sigma1 fresh)
  | _ -> (None, fresh)
and gather_matches_subst (gamma : type_env)
    (fptl : (flat_pattern * term) list) (taup : monoTy) (tau : monoTy)
    (cur_sub : substitution) (fresh : uvargenerator) : substitution option * uvargenerator =
  match fptl with
    [] -> Some cur_sub, fresh
  | (fpt, t)::res ->
    match gather_flpat_subst gamma fpt t taup tau fresh with
      None, fresh -> (None, fresh)
    | Some sigma, fresh ->
      gather_matches_subst
        (app_env sigma gamma) res
        (app_monoTy sigma taup) (app_monoTy sigma tau) (subst_compose sigma cur_sub) fresh
and gather_flpat_subst (gamma : type_env) (fpt : flat_pattern)
    (t : term) (taup : monoTy) (tau : monoTy)
    (fresh : uvargenerator) : substitution option * uvargenerator =
  match fpt with
    FlPVar x -> gather_term_subst (ins_env gamma x (monoTy2polyTy taup)) t tau fresh
  | FlPCstr (label, bindlist) ->
    match lookup_env gamma label with
      None -> (None, fresh)
    | Some label_ty ->
      begin match (fresh_Instance label_ty fresh, taup) with
          ((TyArrow(TyTuple(tau1s), TyCstr(cstr_name, alphas)), fresh), TyCstr(cstr_name', taups))
          when cstr_name = cstr_name' ->
          if List.length alphas <> List.length taups || List.length bindlist <> List.length tau1s then failwith "arity mismatch"
          else let alphas = List.map (fun mty -> match mty with TyVar i -> i | _ -> assert false) alphas in
            let theta = List.combine alphas taups in
            let pat_env =
              List.combine bindlist (List.map (fun mty -> monoTy2polyTy (app_monoTy theta mty)) tau1s) in
            gather_term_subst (sum_env pat_env gamma) t tau fresh
        | ((TyArrow(tau1s, TyCstr(cstr_name, alphas)), fresh), TyCstr(cstr_name', taups))
          when cstr_name = cstr_name' ->
          if List.length bindlist <> 1 || List.length alphas <> List.length taups then failwith "arity mismatch"
          else let alphas = List.map (fun mty -> match mty with TyVar i -> i | _ -> assert false) alphas in
            let theta = List.combine alphas taups in
            let pat_env = match bindlist with [] -> failwith "impossible"
                                            | [x] -> [(x, monoTy2polyTy (app_monoTy theta tau1s))]
                                            | _ -> failwith "impossible" in
            gather_term_subst (sum_env pat_env gamma) t tau fresh
        | _ -> (None, fresh)
      end
and gather_iter_terms (gamma : type_env) (tl : term list) (cur_sub : substitution)
    (cur_tyl : monoTy list) (fresh : uvargenerator) : (substitution * monoTy list) option * uvargenerator =
  match tl with
    [] -> Some (cur_sub, List.rev cur_tyl), fresh
  | t :: ts ->
    let (NextUVar(tau, fresh)) = fresh () in
    match gather_term_subst gamma t tau fresh with
      None, fresh -> None, fresh
    | Some sigma, fresh ->
      gather_iter_terms (app_env sigma gamma) ts (subst_compose sigma cur_sub)
        (tau :: cur_tyl) fresh


let show_subst (gamma : type_env) (t : term) =
  let (NextUVar(ty, fresh)) = uvargen () in
  let result =
    match gather_term_subst gamma t ty fresh  with
      None, _ -> None
    | Some sigma, _ -> Some sigma
  in result


let gather_subst (gamma : type_env) (t : term) =
  let (NextUVar(ty, fresh)) = uvargen () in
  let result =
    match gather_term_subst gamma t ty fresh  with
      None, _ -> None
    | Some sigma, _ -> match ty with
        TyVar n -> Some (subst_fun sigma)
      | _ -> None in
  result

let infer (gamma : type_env) (t : term) : monoTy option =
  match gather_subst gamma t with
    None -> None
  | Some sigma -> Some (sigma 1)



(*  evaluation *)
exception NoRulesApplies

    (* warning! substituion has incorrect functionality *)

let rec isval = function
    TmVar _ | TmUnit | TmCstr _ | TmAbs _ -> true
  | TmFold (t1, t2) -> isval t1 && isval t2
  | TmTuple tl -> List.for_all (fun t -> isval t) tl
  | _ -> false

type valbindings = (string * term) list

let rec eval1 (ctx : valbindings) = function
  | TmFix t when isval t ->
    (match t with
      TmAbs(x, t') -> termSubstOp x (TmFix t) t'
     | _ -> eval1 ctx t)
  | TmFix t -> TmFix (eval1 ctx t)
  | TmFold(TmCstr(lab), t) -> TmFold(TmCstr(lab), eval1 ctx t)
  | TmFold(t1, t2) -> TmFold(eval1 ctx t1, t2)
  | TmTuple(tl) ->
    let rec evalfields = function
        [] -> raise NoRulesApplies
      | v :: rest when isval v -> v :: (evalfields rest)
      | t :: rest -> (eval1 ctx t) :: rest in
    TmTuple(evalfields tl)
  | TmApp(TmAbs(x, t1), t2) when isval t2 ->
    termSubstOp x t2 t1
  | TmApp(TmAbs(x, t1), t2) ->
    TmApp(TmAbs(x, t1), eval1 ctx t2)
  | TmApp(t1, t2) ->
    TmApp(eval1 ctx t1, t2)
  | TmLet(x, t1, t2) when isval t1 && isval t2 ->
    termSubstOp x (TmFix(TmAbs(x, t1))) t2
  | TmLet(x, t1, t2) when isval t1 ->
    TmLet(x, t1, eval1 ctx t2)
  | TmLet(x, t1, t2) ->
    TmLet(x, eval1 ctx t1, t2)
  | _ -> raise NoRulesApplies

let rec eval (ctx : valbindings) (t : term) : term =
  try let t = eval1 ctx t in
    eval ctx t
  with NoRulesApplies -> t
