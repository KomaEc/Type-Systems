

module ParserTest where

import Syntax
import Lexer
import Parser

prog1 :: String
prog1 = "id : Π A : U . A → A = λ A . λ x . x"

prog2 :: String
prog2 = "bool : U = Sum (True 1 | False 1)\n elimBool : Π c : bool → U . c (False 0) → c (True 0) → Π b : bool . C b\n = λ c . λ h0 . λ h1 . fun (True → h0 | False → h1)"

testParser :: IO ()
testParser = do
    let exp1 = parseExpr prog1
        exp2 = parseExpr prog2
    print $ show exp1
    print $ show exp2