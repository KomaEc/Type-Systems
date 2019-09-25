
module Main where

import Types
import Syntax
import Infer
import Utils
import Lexer
import Parser

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
    let exp = parse (scanTokens "lambda x -> let y = lambda z -> x in y")
    print exp
    putStrLn "type :"
    mty <- runInfer $ typeOf exp
    case mty of
        Right ty -> do
            str <- showIO ty
            putStrLn str
        Left e   -> 
            putStrLn "Not Typeable"