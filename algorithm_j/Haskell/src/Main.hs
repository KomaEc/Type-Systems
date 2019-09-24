
module Main where

import Types
import Syntax
import Infer
import Utils

example1 :: Expr
example1 = Lam "x" $
            Let "y" (Var "x") $
                Var "y"

example2 :: Expr
example2 = Lam "x" $
            Let "y" (Lam "z" (Var "x"))
                (Var "y")

main :: IO ()
main = do
    Right ty <- runInfer $ typeOf example2
    str <- showIO ty
    putStrLn str
