module Main where

import Lib
import System.Environment
import Lexer
import Parser
import Syntax

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1 then
        putStrLn "minitt expects one [FILE]"
    else do
        contents <- readFile (head args)
        let exp = parse (scanTokens contents)
        print exp