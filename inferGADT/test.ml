open Format
open Syntax
open Core

let judge env t =
  match infer env t with
    Some s -> s
  | None -> failwith "Not typable"

let test_Const = TmConst(TmTrue)

let test_Var_env = make_env "f" ([0], TyArrow(TyBool, TyVar 0))
let test_Var = TmVar "f"

let test_Bin = TmBinOp(ConsOp, TmConst(TmInt 62), TmConst(TmNil))

let test_Mon = TmMonOp(IntNegOp, TmConst(TmInt 42))

let test_if = Tmif(TmConst(TmTrue), TmConst(TmInt 62), TmConst(TmInt 252))

let test_abs = TmAbs("x", TmBinOp(IntPlusOp, TmVar("x"), TmVar("x")))

let test_app = TmApp(TmAbs("x", TmBinOp(IntPlusOp, TmVar("x"), TmVar("x"))),
                     TmConst(TmInt 62))

let test_let = TmLet("length", TmAbs("list", Tmif(TmBinOp(EqOp, TmVar "list", TmConst(TmNil)),
                                                  TmConst(TmInt 0),
                                                  TmBinOp(IntPlusOp, TmConst(TmInt 1), TmApp(TmVar "length", TmMonOp(TlOp, TmVar "list"))))),
                     TmApp(TmVar "length", TmBinOp(ConsOp, TmConst(TmInt 2), TmConst(TmNil))))

let test_aux = TmAbs ("list",
                           Tmif (TmBinOp (EqOp, TmVar "list", TmConst TmNil), TmConst (TmInt 0),
                                 TmBinOp (IntPlusOp, TmConst (TmInt 1),
                                          TmApp (TmVar "length", TmMonOp (TlOp, TmVar "list")))))

let test_1 = TmAbs ("list",
                    Tmif (TmBinOp (EqOp, TmVar "list", TmConst TmNil), TmConst (TmInt 0),
                          TmBinOp (IntPlusOp, TmConst (TmInt 1),
                                   TmConst(TmInt (-1)))))
let test_2 = TmBinOp (EqOp, TmVar "list", TmConst TmNil)
let test_3 = TmAbs ("list",
                    Tmif (TmConst(TmTrue), TmConst (TmInt 0),
                          TmBinOp (IntPlusOp, TmConst (TmInt 1),
                                   TmConst(TmInt (-1)))))

let test_4 = TmBinOp (EqOp, TmConst TmNil, TmConst TmNil)
