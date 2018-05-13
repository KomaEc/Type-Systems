open Format
open Syntax

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
  | (TyArrow(ty1, ty2), TyArrow(ty1', ty2')) :: eqs ->
    unify ((ty2, ty2') :: (ty1, ty1') :: eqs)
  | _ -> None

let fresh_Instance ((tvs, ty):polyTy) (fresh : uvargenerator) =
  let rec fresh_aux tvs acc fresh =
    match tvs with
      [] -> acc, fresh
    | t :: ts -> let (NextUVar(tau, fresh)) = fresh () in
      fresh_aux ts ((t, tau)::acc) fresh in
  let sub, fresh = fresh_aux tvs [] fresh in
  app_monoTy sub ty, fresh

(* type inference algorithm *)

    (*
(* side_effect alternatives *)
let rec gather_subst (gamma : type_env) (t : term) (tau : monoTy) : substitution option =
  match t with
    TmConst c ->
    let tau' = const_signature c in
    unify [(tau, freshInstance tau')]
  | TmVar x ->
    (match lookup_env gamma x with
       None -> None
     | Some tau' ->
       unify [(tau, freshInstance tau')])
  | TmBinOp (binop, t1, t2) ->
    let tau' = binop_signature binop in
    let tau1 = fresh () in
    let tau2 = fresh () in
    (match gather_subst gamma t1 tau1 with
       None -> None
     | Some sigma1 ->
       (match gather_subst (app_env sigma1 gamma) t2 tau2 with
          None -> None
        | Some sigma2 ->
          let sigma21 = subst_compose sigma2 sigma1 in
          (match unify [(app_monoTy sigma21
                           (TyArrow(tau1, TyArrow(tau2, tau))),
                         freshInstance tau')] with
             None -> None
           | Some sigma3 ->
             Some(subst_compose sigma3 sigma21))))
  | TmMonOp (monop, t1) ->
    let tau' = monop_signature monop in
    let tau1 = fresh () in
    (match gather_subst gamma t1 tau1 with
       None -> None
     | Some sigma ->
       (match unify [(app_monoTy sigma (TyArrow(tau1, tau)), freshInstance tau')] with
          None -> None
        | Some sigma' ->
          Some (subst_compose sigma' sigma)))
  | Tmif (t1, t2, t3) ->
    (match gather_subst gamma t1 TyBool with
       None -> None
     | Some sigma1 ->
       (match gather_subst
                (app_env sigma1 gamma)
                t2
                (app_monoTy sigma1 tau) with
         None -> None
       | Some sigma2 ->
         let sigma21 = subst_compose sigma2 sigma1 in
         (match gather_subst
                  (app_env sigma21 gamma)
                  t3
                  (app_monoTy sigma21 tau) with
           None -> None
         | Some sigma3 -> Some (subst_compose sigma3 sigma21))))
  | TmAbs (x, t) ->
    let tau1 = fresh () in
    let tau2 = fresh () in
    (match gather_subst
             (ins_env gamma x (monoTy2polyTy tau1)) t tau2 with
      None -> None
    | Some sigma ->
      (match unify [(app_monoTy sigma tau,
                     app_monoTy sigma (TyArrow(tau1, tau2)))] with
        None -> None
      | Some sigma' -> Some (subst_compose sigma' sigma)))
  | TmApp (t1, t2) ->
    let tau1 = fresh () in
    (match gather_subst gamma t1 (TyArrow(tau1, tau)) with
       None -> None
     | Some sigma ->
       (match gather_subst
                (app_env sigma gamma)
                t2
                (app_monoTy sigma tau1) with
         None -> None
       | Some sigma' ->
         Some (subst_compose sigma' sigma)))
  | TmLet (x, t1, t2) ->
    let tau1 = fresh () in
    (match gather_subst
             (ins_env gamma x (monoTy2polyTy tau1)) t1 tau1 with
      None -> None
    | Some sigma1 ->
      let sigma1_gamma = app_env sigma1 gamma in
      let sigma1_tau1 = app_monoTy sigma1 tau1 in
      let sigma1_tau = app_monoTy sigma1 tau in
      (match gather_subst
               (ins_env sigma1_gamma
                  x (gen sigma1_gamma sigma1_tau1)) t2
               sigma1_tau with
        None -> None
      | Some sigma2 -> Some (subst_compose sigma2 sigma1)))
  | _ -> None

let infer (gamma : type_env) (t : term) =
  let ty = fresh () in
  let result =
    match gather_subst gamma t ty with
      None -> None
    | Some sigma -> match ty with
        TyVar n -> Some (subst_fun sigma)
      | _ -> None
  in let _ = reset () in
  result     *)


(* type inference algorithm *)
let rec gather_subst (gamma : type_env) (t : term) (tau : monoTy) (fresh : uvargenerator) : substitution option * uvargenerator =
  match t with
    TmConst c ->
    let tau' = const_signature c in
    let fresh_tau', fresh = fresh_Instance tau' fresh in
    unify [(tau, fresh_tau')], fresh
  | TmVar x ->
    (match lookup_env gamma x with
       None -> None
     | Some tau' ->
       let fresh_tau', fresh = fresh_Instance tau' fresh in
       unify [(tau, fresh_tau')]), fresh
  | TmBinOp (binop, t1, t2) ->
    let tau' = binop_signature binop in
    let (NextUVar(tau1, fresh)) = fresh () in
    let (NextUVar(tau2, fresh)) = fresh () in
    (match gather_subst gamma t1 tau1 fresh with
       None, fresh -> None, fresh
     | Some(sigma1), fresh ->
       (match gather_subst (app_env sigma1 gamma) t2 tau2 fresh with
          None, fresh -> None, fresh
        | Some sigma2, fresh ->
          let sigma21 = subst_compose sigma2 sigma1 in
          let fresh_tau', fresh = fresh_Instance tau' fresh in
          (match unify [(app_monoTy sigma21
                           (TyArrow(tau1, TyArrow(tau2, tau))),
                         fresh_tau')] with
             None -> None, fresh
           | Some sigma3 ->
             Some(subst_compose sigma3 sigma21), fresh)))
  | TmMonOp (monop, t1) ->
    let tau' = monop_signature monop in
    let (NextUVar(tau1, fresh)) = fresh () in
    (match gather_subst gamma t1 tau1 fresh with
       None, fresh -> None, fresh
     | Some sigma, fresh ->
       let fresh_tau', fresh = fresh_Instance tau' fresh in
       (match unify [(app_monoTy sigma (TyArrow(tau1, tau)), fresh_tau')] with
          None -> None, fresh
        | Some sigma' ->
          Some (subst_compose sigma' sigma), fresh))
  | Tmif (t1, t2, t3) ->
    (match gather_subst gamma t1 TyBool fresh with
       None, fresh -> None, fresh
     | Some sigma1, fresh ->
       (match gather_subst
                (app_env sigma1 gamma)
                t2
                (app_monoTy sigma1 tau) fresh with
         None, fresh -> None, fresh
       | Some sigma2, fresh ->
         let sigma21 = subst_compose sigma2 sigma1 in
         (match gather_subst
                  (app_env sigma21 gamma)
                  t3
                  (app_monoTy sigma21 tau) fresh with
           None, fresh -> None, fresh
         | Some sigma3, fresh -> Some (subst_compose sigma3 sigma21), fresh)))
  | TmAbs (x, t) ->
    let (NextUVar(tau1, fresh)) = fresh () in
    let (NextUVar(tau2, fresh)) = fresh () in
    (match gather_subst
             (ins_env gamma x (monoTy2polyTy tau1)) t tau2 fresh with
      None, fresh -> None, fresh
    | Some sigma, fresh ->
      (match unify [(app_monoTy sigma tau,
                     app_monoTy sigma (TyArrow(tau1, tau2)))] with
        None -> None, fresh
      | Some sigma' -> Some (subst_compose sigma' sigma), fresh))
  | TmApp (t1, t2) ->
    let (NextUVar(tau1, fresh)) = fresh () in
    (match gather_subst gamma t1 (TyArrow(tau1, tau)) fresh with
       None, fresh -> None, fresh
     | Some sigma, fresh ->
       (match gather_subst
                (app_env sigma gamma)
                t2
                (app_monoTy sigma tau1) fresh with
         None, fresh -> None, fresh
       | Some sigma', fresh ->
         Some (subst_compose sigma' sigma), fresh))
  | TmLet (x, t1, t2) ->
    let (NextUVar(tau1, fresh)) = fresh () in
    (match gather_subst
             (ins_env gamma x (monoTy2polyTy tau1)) t1 tau1 fresh with
      None, fresh -> None, fresh
    | Some sigma1, fresh ->
      let sigma1_gamma = app_env sigma1 gamma in
      let sigma1_tau1 = app_monoTy sigma1 tau1 in
      let sigma1_tau = app_monoTy sigma1 tau in
      (match gather_subst
               (ins_env sigma1_gamma
                  x (gen sigma1_gamma sigma1_tau1)) t2
               sigma1_tau fresh with
        None, fresh -> None, fresh
      | Some sigma2, fresh -> Some (subst_compose sigma2 sigma1), fresh))
  | _ -> None, fresh

let infer (gamma : type_env) (t : term) =
  let (NextUVar(ty, fresh)) = uvargen () in
  let result =
    match gather_subst gamma t ty fresh  with
      None, _ -> None
    | Some sigma, _ -> match ty with
        TyVar n -> Some (subst_fun sigma)
      | _ -> None in
  result





let rec gather_exp_ty_substitution gamma exp tau =
  let judgment = TermJudgment(gamma, exp, tau) in
  (*
      let _ = print_string ("Trying to type "^ string_of_judgment judgment^"\n") in
  *)
  let result =
    match exp
    with TmConst c ->
      let tau' = const_signature c in
      (match unify [(tau, freshInstance tau')]
       with None       -> None
          | Some sigma -> Some(Proof([],judgment), sigma))
       | TmVar x ->
         (match lookup_env gamma x with None -> None
                                      | Some gamma_x ->
                                        (match unify [(tau, freshInstance gamma_x)]
                                         with None       -> None
                                            | Some sigma -> Some(Proof([],judgment), sigma)))
       | TmBinOp (binop, e1,e2) ->
         let tau' = binop_signature binop in
         let tau1 = fresh() in
         let tau2 = fresh() in
         (match gather_exp_ty_substitution gamma e1 tau1
          with None -> None
             | Some(pf1, sigma1) ->
               (match gather_exp_ty_substitution (app_env sigma1 gamma) e2 tau2
                with None -> None
                   | Some (pf2, sigma2) ->
                     let sigma21 = subst_compose sigma2 sigma1 in
                     (match unify[(app_monoTy sigma21
                                     (TyArrow(tau1, (TyArrow(tau2, tau)))),
                                   freshInstance tau')]
                      with None -> None
                         | Some sigma3 ->
                           Some(Proof([pf1;pf2], judgment),subst_compose sigma3 sigma21))))
       | TmMonOp (monop, e1) ->
         let tau' = monop_signature monop in
         let tau1 = fresh() in
         (match gather_exp_ty_substitution gamma e1 tau1
          with None -> None
             | Some(pf, sigma) ->
               (match unify[(app_monoTy sigma (TyArrow(tau1, tau)),
                             freshInstance tau')]
                with None -> None
                   | Some subst ->
                     Some(Proof([pf], judgment),
                          subst_compose subst sigma)))
       | Tmif(e1,e2,e3) ->
         (match gather_exp_ty_substitution gamma e1 TyBool
          with None -> None
             | Some(pf1, sigma1) ->
               (match gather_exp_ty_substitution
                        (app_env sigma1 gamma) e2 (app_monoTy sigma1 tau)
                with None -> None
                   | Some (pf2, sigma2) ->
                     let sigma21 = subst_compose sigma2 sigma1 in
                     (match gather_exp_ty_substitution
                              (app_env sigma21 gamma) e3
                              (app_monoTy sigma21 tau)
                      with  None -> None
                          | Some(pf3, sigma3) ->
                            Some(Proof([pf1;pf2;pf3], judgment), subst_compose sigma3 sigma21))))
       | TmAbs(x,e) ->
         let tau1 = fresh() in
         let tau2 = fresh() in
         (match gather_exp_ty_substitution
                  (ins_env gamma x (monoTy2polyTy tau1)) e tau2
          with None -> None
             | Some (pf, sigma) ->
               (match unify [(app_monoTy sigma tau,
                              app_monoTy sigma (TyArrow(tau1, tau2)))]
                with None -> None
                   | Some sigma1 ->
                     Some(Proof([pf],judgment), subst_compose sigma1 sigma)))
       | TmApp(e1,e2) ->
         let tau1 = fresh() in
         (match gather_exp_ty_substitution gamma e1 (TyArrow(tau1, tau))
          with None -> None
             | Some(pf1, sigma1) ->
               (match gather_exp_ty_substitution (app_env sigma1 gamma) e2
                        (app_monoTy sigma1 tau1)
                with None -> None
                   | Some (pf2, sigma2) ->
                     Some(Proof([pf1;pf2], judgment), subst_compose sigma2 sigma1)))
       | TmLet (x, t1, t2) ->
         let tau1 = fresh () in
         (match gather_exp_ty_substitution
                  (ins_env gamma x (monoTy2polyTy tau1)) t1 tau1 with
           None -> None
         | Some (pf1, sigma1) ->
           let sigma1_gamma = app_env sigma1 gamma in
           let sigma1_tau1 = app_monoTy sigma1 tau1 in
           let sigma1_tau = app_monoTy sigma1 tau in
           (match gather_exp_ty_substitution
                    (ins_env sigma1_gamma
                       x (gen sigma1_gamma sigma1_tau1)) t2
                    sigma1_tau with
             None -> None
           | Some (pf2, sigma2) -> Some(Proof([pf1;pf2], judgment), subst_compose sigma2 sigma1)))
       | _ -> None
  in result

let niceInfer_exp gather_exp (gamma:type_env) exp =
  let ty = fresh()
  in
  let result =
    match gather_exp gamma exp ty with
      None ->
      (print_string("Failure: No type for expression: "^
                    string_of_term exp^ "\n"^
                    "in the environment: "^
                    string_of_env string_of_polyTy gamma^ "\n");
       raise (Failure ""))
    | Some (p,s) ->
      (string_of_proof p^
       ("Unifying substitution: "^
        string_of_substitution s^
        "Substituting...\n"^
        let new_p = proof_lift_subst s p in
        string_of_proof new_p)) in
  let _ = reset() in
  result;;
