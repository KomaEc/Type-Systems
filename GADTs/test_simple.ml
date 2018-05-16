open Syntax_simple
open Core_simple

let judge env t =
  match gather_subst env t with
    Some s -> s
  | None -> failwith "Not typable"

let test_Unit = TmUnit

let test_Var_env = make_env "f" ([0], TyArrow(TyUnit, TyVar 0))
let test_Var = TmVar "f"

let test_abs = TmAbs("c", TmAbs("n", TmAbs("f", TmApp(TmApp(TmVar "f", TmVar "c"), TmVar "n"))))

let label_Nil_ty =
  let alpha = TyVar 0 in
  [0], TyArrow(TyUnit, mk_list_ty alpha)

let label_Nil_ty =
  let alpha = TyVar 0 in
  [0], TyArrow(TyCstr("tuple", [TyUnit]), mk_list_ty alpha)

let singleton_ty =
  let alpha = TyVar 0 in
  [0], TyArrow(alpha, TyCstr("tuple", [alpha]))

let primitive_env : type_env =
  [("Nil", label_Nil_ty);
   ("Cons", label_Cons_ty);
   ("make_pair", pair_ty);
   ("make", singleton_ty)]

let test_make_pair = TmVar "make_pair"
let test_Nil = TmVar "Nil"
let test_Cons = TmVar "Cons"

let test_nil = TmApp(TmVar "Nil", TmUnit)

let test_nil = TmApp(TmVar "Nil", TmApp(TmVar "make", TmUnit))

let test_partial_pair = TmApp(TmVar "make_pair", TmUnit)
let test_pair = TmApp(test_partial_pair, test_nil)
let test_list =
  TmApp(TmVar "Cons", TmApp(TmApp(TmVar "make_pair", TmUnit),
                            test_nil))

let test_flVar = TmFlatMatchWith(TmUnit, [(FlPVar "x"), (TmVar "x")])

let test_flVar1 = TmFlatMatchWith(test_list, [(FlPVar "x"), (TmVar "x")])

let test_flVar2 = TmFlatMatchWith(TmUnit, [FlPVar "x", TmApp(TmVar "Cons", TmApp(TmApp(TmVar "make_pair", TmVar "x"),
                                                                                 test_nil))])
let test_flCstr = TmFlatMatchWith(test_nil, [(FlPCstr("Nil", ["x"]), TmVar "x")])

let test_flCstr1 = TmFlatMatchWith(test_list, [(FlPCstr("Cons", ["x"; "xs"]), TmVar "xs")])
let test_flCstr2 = TmFlatMatchWith(test_list, [(FlPCstr("Cons", ["x"; "xs"]), TmVar "x")])
let test_flCstr3 = TmFlatMatchWith(test_list, [(FlPCstr("Nil", ["x"]), test_nil);
                                               (FlPCstr("Cons", ["x"; "xs"]), TmVar "xs")])
