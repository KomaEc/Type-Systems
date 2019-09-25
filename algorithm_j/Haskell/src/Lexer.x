{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
    $white+                             ;
    "--".*                              ;
    let                                 { \s -> TokenLet }
    in                                  { \s -> TokenIn }
    lambda                              { \s -> TokenLam }
    $alpha [$alpha $digit \_ \']*       { \s -> TokenVar s }
    \=                                  { \s -> TokenEq }
    "->"                                { \s -> TokenArrow }
    \(                                  { \s -> TokenLParen }
    \)                                  { \s -> TokenRParen }



{

data TokenClass 
    = TokenLet
    | TokenIn
    | TokenLam
    | TokenArrow
    | TokenVar String
    | TokenEq
    | TokenLParen
    | TokenRParen
    deriving ( Show )

scanTokens = alexScanTokens
}