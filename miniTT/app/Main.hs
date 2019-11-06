module Main where

import Lib
import System.Environment ( getArgs )
import Parser
import Syntax

main :: IO ()
main = do
    args <- getArgs
    result <- case args of
              []  -> fmap (parseExpr "<stdin>") getContents
              [f] -> fmap (parseExpr f) (readFile f)
              _   -> error "expected at most one <FILE>"
    either putStrLn (print . show) result