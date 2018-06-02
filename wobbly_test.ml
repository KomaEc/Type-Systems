open Wobbly_syntax
open Wobbly_core


let test_Unit = TmConst(TmUnit)


let test_abs = TmAbs("c", TmAbs("n", TmAbs("f", TmApp(TmApp(TmVar "f", TmVar "c"), TmVar "n"))))


let label_Nil_ty =
  let alpha = TyVar 0 in
  [0], TyArrow(TyUnit, mk_list_ty alpha)

let primitive_env : env =
  [VarBind("Nil", R, label_Nil_ty);
   VarBind("Cons", R, label_Cons_ty)]



let test_Nil = TmCstr "Nil"
let test_Cons = TmCstr "Cons"

let test_nil = TmFold(TmCstr "Nil", TmConst(TmUnit))


let test_list =
  TmFold(TmCstr "Cons", TmTuple [TmConst(TmUnit); test_nil])

let test_int_list =
  TmFold(TmCstr "Cons", TmTuple [TmConst(TmInt 3); test_nil])

let test_flVar = TmFlatMatchWith(TmConst(TmUnit), [(FlPVar "x"), (TmVar "x")])

let test_flVar1 = TmFlatMatchWith(test_list, [(FlPVar "x"), (TmVar "x")])

let test_flVar2 = TmFlatMatchWith(TmConst(TmUnit), [FlPVar "x", TmFold(TmCstr "Cons", TmTuple [TmVar "x"; test_nil])])
let test_flCstr = TmFlatMatchWith(test_nil, [(FlPCstr("Nil", ["x"]), TmVar "x")])

let test_flCstr1 = TmFlatMatchWith(test_list, [(FlPCstr("Cons", ["x"; "xs"]), TmVar "xs")])
let test_flCstr2 = TmFlatMatchWith(test_list, [(FlPCstr("Cons", ["x"; "xs"]), TmVar "x")])
let test_flCstr3 = TmFlatMatchWith(test_list, [(FlPCstr("Nil", ["x"]), test_nil);
                                               (FlPCstr("Cons", ["x"; "xs"]), TmVar "xs")])

let test_let = TmLet("test_nil", test_nil, TmFlatMatchWith(test_list, [(FlPCstr("Nil", ["x"]), TmVar "test_nil");
                                                                       (FlPCstr("Cons", ["x"; "xs"]), TmVar "xs")]))

let test_tuple = TmTuple [TmConst(TmUnit); TmConst(TmUnit)]
let test_tuple1 = TmTuple [TmConst(TmUnit); test_abs; test_Cons]

let test_zero = TmAbs("s", TmAbs("z", TmVar "z"))
let test_one = TmAbs("s", TmAbs("z", TmApp(TmVar "s", TmVar "z")))
let test_two = TmAbs("s", TmAbs("z", TmApp(TmVar "s", TmApp(TmVar "s", TmVar "z"))))
let test_three = TmAbs("s", TmAbs("z", TmApp(TmVar "s", TmApp(TmVar "s", TmApp(TmVar "s", TmVar "z")))))
let test_succ = TmAbs("n", TmAbs("s", TmAbs("z",
                                            TmApp(TmVar "s", TmApp(TmApp(TmVar "n", TmVar "s"), TmVar "z")))))
let test_one_alt = TmApp(test_succ, test_zero)
let test_true = TmAbs("t", TmAbs("f", TmVar "t"))
let test_false = TmAbs("t", TmAbs("f", TmVar "f"))
let test_iszero = TmAbs("n",
                        TmApp(TmApp(TmVar "n", TmAbs("m", test_false)), test_true))
let test_n n =
  let rec aux t n =
    if n = 0 then TmAbs("s", TmAbs("z", t))
    else aux (TmApp(TmVar "s", t)) (n - 1) in
  aux (TmVar "z") n

let test_plus =
  TmAbs("n", TmAbs("m",
                   TmAbs("s", TmAbs("z",
                                    TmApp(TmApp(TmVar "n", TmVar "s"),
                                          TmApp(TmApp(TmVar "m", TmVar "s"), TmVar "z"))))))

let test_mul =
  TmAbs("n", TmAbs("m",
                   TmAbs("s", TmAbs("z",
                                    TmApp(TmApp(TmVar "n", TmApp(test_plus, TmVar "m")), TmVar "z")))))

let test_pred =
  TmAbs("n",
        TmAbs("s",
              TmAbs("z",
                    TmApp(TmApp(TmApp(TmVar "n", TmAbs("g", TmAbs("h", TmApp(TmVar "h", TmApp(TmVar "g", TmVar "s"))))), TmAbs("u", TmVar "z")), TmAbs("u", TmVar "u")))))

let test_and =
  TmAbs("b",
        TmAbs("c", TmApp(TmApp(TmVar "b", TmVar "c"), test_false)))

let test_equal =
  TmAbs("n",
        TmAbs("m",
              TmApp(TmApp(test_and, TmApp(test_iszero, TmApp(TmApp(TmVar "n", test_pred), TmVar "m"))),
                    TmApp(test_iszero, TmApp(TmApp(TmVar "m", test_pred), TmVar "m")))))

let test_equal_six =
  TmApp(TmApp(test_equal,
              TmApp(TmApp(test_mul, test_two), test_three)), test_n 6)

let test_ctx =
  [("plus", test_plus)]


let test_subst =
  TmApp(TmAbs("f", TmAbs("y", TmApp(TmVar "f", TmVar "y"))), TmAbs("x", TmApp(TmApp(TmVar "plus", TmVar "x"), TmVar "y")))


let test_ITE =
  TmAbs("b", TmAbs("x", TmAbs("y", TmApp(TmApp(TmVar "b", TmVar "x"), TmVar "y"))))

let test_frac =
  TmLet("frac",
        TmAbs("n",
              TmApp(TmApp(TmApp(test_ITE,
                                TmApp(test_iszero, TmVar "n")), test_one),
                    TmApp(TmApp(test_mul, TmVar "n"),
                          TmApp(TmVar "frac", TmApp(test_pred, TmVar "n"))))), TmApp(TmVar "frac", test_three))



let test_binop1 =
  TmBinOp(PlusOp, TmConst(TmInt 3), TmConst(TmInt 5))

let test_rec =
  TmLet("plus",
        TmAbs("n",
              TmAbs("m", TmIf(TmBinOp(EqOp, TmVar "n", TmConst(TmInt 0)),
                              TmVar "m",
                              TmApp(TmApp(TmVar "plus",
                                          TmBinOp(MinusOp, TmVar "n", TmConst(TmInt 1))),
                                    TmBinOp(PlusOp, TmVar "m", TmConst(TmInt 1)))))),
        TmApp(TmApp(TmVar "plus", TmConst(TmInt 3)), TmConst(TmInt 5)))

let test_frac =
  TmLet("frac",
        TmAbs("n",
              TmIf(TmBinOp(EqOp, TmVar "n", TmConst(TmInt 0)),
                   TmConst(TmInt 1),
                   TmBinOp(TimesOp,
                           TmVar "n", TmApp(TmVar "frac", TmBinOp(MinusOp, TmVar "n", TmConst(TmInt 1)))))),
        TmApp(TmVar "frac", TmConst(TmInt 3)))


let test_rec1 =
  TmApp
    (TmAbs ("n",
            TmIf (TmBinOp (EqOp, TmVar "n", TmConst (TmInt 0)), TmConst (TmInt 1),
                  TmBinOp (TimesOp, TmVar "n",
                           TmApp
                             (TmFix
                                (TmAbs ("frac",
                                        TmAbs ("n",
                                               TmIf (TmBinOp (EqOp, TmVar "n", TmConst (TmInt 0)),
                                                     TmConst (TmInt 1),
                                                     TmBinOp (TimesOp, TmVar "n",
                                                              TmApp (TmVar "frac",
                                                                     TmBinOp (MinusOp, TmVar "n", TmConst (TmInt 1)))))))),
                              TmBinOp (MinusOp, TmVar "n", TmConst (TmInt 1)))))),
     TmConst (TmInt 3))

let test_pat =
  TmFlatMatchWith(test_int_list,
                  [FlPCstr("Nil", ["_"]), TmConst(TmTrue);
                   FlPCstr("Cons", ["x"; "xs"]), TmConst(TmFalse)])
let test_list_n n =
  let rec aux n =
    if n = 0 then TmFold(TmCstr "Nil", TmConst(TmUnit))
    else TmFold(TmCstr "Cons", TmTuple ([TmConst(TmInt n); aux (n - 1)])) in
  aux n

let test_pat_rec =
  TmLet("length",
        TmAbs("l",
              TmFlatMatchWith(TmVar "l",
                              [FlPCstr("Nil", ["nil"]), TmConst(TmInt 0);
                               FlPCstr("Cons", ["x";"xs"]),
                               TmBinOp(PlusOp, TmConst(TmInt 1), TmApp(TmVar "length", TmVar "xs"))])),
        TmApp(TmVar "length", test_list_n 5))
let test_tl =
  TmAbs("l",
        TmFlatMatchWith(TmVar "l",
                        [FlPCstr("Cons", ["_"; "xs"]), TmVar "xs"]))

let test_wrong =
  TmLet("length",
        TmAbs("l",
              TmFlatMatchWith(TmVar "l",
                              [FlPCstr("Nil", ["nil"]), TmConst(TmInt 0)])),
        TmApp(TmVar "length", test_nil))
