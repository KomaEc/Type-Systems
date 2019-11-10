{
module Lexer
    ( Token(..)
    , AlexPosn(..)
    , TokenClass(..)
    , Alex(..)
    , runAlex'
    , alexMonadScan'
    , alexError'
    ) where

import Prelude hiding ( lex )
import Control.Monad ( liftM )
}

%wrapper "monadUserState"

$digit = 0-9
$alpha = [a-zA-Z]
$alphaL = [a-z]
$alphaU = [A-Z]

tokens :-
    $white+                             ;
    "--".*                              ;
    "rec₁"                              { lex' TokenRecUnit }
    "rec1"                              { lex' TokenRecUnit }
    "lambda"                            { lex' TokenLam }
    "rec"                               { lex' TokenRec }
    "λ"                                 { lex' TokenLam }
    "U"                                 { lex' TokenU }
    "fun"                               { lex' TokenFun }
    "Sum"                               { lex' TokenSum }
    "Pi"                                { lex' TokenPi }
    "∀"                                 { lex' TokenPi }
    "Π"                                 { lex' TokenPi }
    "Sigma"                             { lex' TokenSigma }
    "Σ"                                 { lex' TokenSigma }
    $alphaL [$alpha $digit \_ \']*      { lex TokenVar }
    $alphaU [$alpha $digit \_ \']*      { lex TokenConstr }
    \=                                  { lex' TokenEq }
    "->"                                { lex' TokenArrow }
    "→"                                 { lex' TokenArrow }
    \(                                  { lex' TokenLParen }
    \)                                  { lex' TokenRParen }
    \.                                  { lex' TokenDot }
    \:                                  { lex' TokenColon }
    \;                                  { lex' TokenSemiColon }
    \_                                  { lex' TokenDummy }
    \,                                  { lex' TokenComma }
    \|                                  { lex' TokenVBar }
    "0"                                 { lex' TokenZero }
    "1"                                 { lex' TokenOne }
    "2"                                 { lex' TokenTwo }
    "*"                                 { lex' TokenTimes }
    "×"                                 { lex' TokenTimes }


{

{-
-- in the monad wrapper
data AlexPosn = AlexPn !Int  -- absolute character offset
                       !Int  -- line number
                       !Int  -- column number

type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  [Byte],       -- rest of the bytes for the current char
                  String)       -- current input string

data AlexState = AlexState {
        alex_pos :: !AlexPosn,  -- position at current input location
        alex_inp :: String,     -- the current input
        alex_chr :: !Char,      -- the character before the input
        alex_bytes :: [Byte],   -- rest of the bytes for the current char
        alex_scd :: !Int,       -- the current startcode
        alex_ust :: AlexUserState -- AlexUserState will be defined in the user program
    }

type AlexAction result = AlexInput -> Int -> Alex result

{ ... }  :: AlexAction result
-}

-- To improve error messages, We keep the path of the file we are
-- lexing in our own state.
data AlexUserState = AlexUserState { filePath :: FilePath }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>"

getFilePath :: Alex FilePath
getFilePath = liftM filePath alexGetUserState

setFilePath :: FilePath -> Alex ()
setFilePath = alexSetUserState . AlexUserState

-- The token type, consisting of the source code position and a token class.
data Token = Token AlexPosn TokenClass
  deriving ( Show )

data TokenClass 
    = TokenLam
    | TokenRecUnit              -- "rec₁"
    | TokenRec
    | TokenArrow                -- "→"
    | TokenVar String
    | TokenConstr String
    | TokenEq
    | TokenLParen
    | TokenRParen
    | TokenDot                  -- "."
    | TokenColon                -- ":"
    | TokenSemiColon            -- ";"
    | TokenPi                   -- "Π"
    | TokenSigma                -- "Σ"
    | TokenComma                -- ","
    | TokenFun                  -- "fun"
    | TokenSum                  -- "Sum"
    | TokenVBar                 -- "|"
    | TokenZero
    | TokenOne
    | TokenTwo
    | TokenTimes                -- "×"
    | TokenU                    -- "U"
    | TokenDummy                -- '_'
    | TokenEOF
    deriving ( Show )

alexEOF :: Alex Token
alexEOF = do
    (p,_,_,_) <- alexGetInput
    return $ Token p TokenEOF

-- Unfortunately, we have to extract the matching bit of string
-- ourselves...
lex :: (String -> TokenClass) -> AlexAction Token
lex f = \(p,_,_,s) i -> return $ Token p (f (take i s))

-- For constructing tokens that do not depend on
-- the input
lex' :: TokenClass -> AlexAction Token
lex' = lex . const

-- We rewrite alexMonadScan' to delegate to alexError' when lexing fails
-- (the default implementation just returns an error message).
alexMonadScan' :: Alex Token
alexMonadScan' = do
    inp <- alexGetInput
    sc <- alexGetStartCode
    case alexScan inp sc of
        AlexEOF -> alexEOF
        AlexError (p, _, _, s) ->
            alexError' p ("lexical error at character '" ++ take 1 s ++ "'")
        AlexSkip  inp' len -> do
            alexSetInput inp'
            alexMonadScan'
        AlexToken inp' len action -> do -- here the action is the actions you write for each token
            alexSetInput inp'
            action (ignorePendingBytes inp) len

-- Signal an error, including a commonly accepted source code position.
alexError' :: AlexPosn -> String -> Alex a
alexError' (AlexPn _ l c) msg = do
  fp <- getFilePath
  alexError (fp ++ ":" ++ show l ++ ":" ++ show c ++ ": " ++ msg)

-- A variant of runAlex, keeping track of the path of the file we are lexing.
runAlex' :: Alex a -> FilePath -> String -> Either String a
runAlex' a fp input = runAlex input (setFilePath fp >> a)

}