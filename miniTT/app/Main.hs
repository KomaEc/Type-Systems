module Main where

import Lib
import System.Environment ( getArgs )
import Parser
import Syntax
import Semantics

main :: IO ()
main = do
    args <- getArgs
    result <- case args of
              []  -> fmap (parseExpr "<stdin>") getContents
              [f] -> fmap (parseExpr f) (readFile f)
              _   -> error "expected at most one <FILE>"
    case result of 
        Right prog  -> case runTC (check prog VUnit) of
                        Right  _ -> putStrLn "type-checking succeeded"
                        Left err -> print err
        Left errMsg -> putStrLn errMsg