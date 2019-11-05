{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$alphaL = [a-z]
$alphaU = [A-Z]

tokens :-
    $white+                             ;
    "--".*                              ;
    "lambda"                            { \s -> TokenLam }
    "rec"                               { \s -> TokenRec }
    "λ"                                 { \s -> TokenLam }
    "U"                                 { \s -> TokenU }
    "fun"                               { \s -> TokenFun }
    "Sum"                               { \s -> TokenSum }
    $alphaL [$alpha $digit \_ \']*      { \s -> TokenVar s }
    $alphaU [$alpha $digit \_ \']*      { \s -> TokenConstr s}
    \=                                  { \s -> TokenEq }
    "->"                                { \s -> TokenArrow }
    "→"                                 { \s -> TokenArrow }
    \(                                  { \s -> TokenLParen }
    \)                                  { \s -> TokenRParen }
    \.                                  { \s -> TokenDot }
    \:                                  { \s -> TokenColon }
    \;                                  { \s -> TokenSemiColon }
    \_                                  { \s -> TokenDummy }
    "Π"                                 { \s -> TokenPi }
    "Σ"                                 { \s -> TokenSigma }
    \,                                  { \s -> TokenComma }
    \|                                  { \s -> TokenVBar }
    "0"                                 { \s -> TokenZero }
    "1"                                 { \s -> TokenOne }
    "2"                                 { \s -> TokenTwo }
    "*"                                 { \s -> TokenTimes }
    "×"                                 { \s -> TokenTimes }


{

data TokenClass 
    = TokenLam
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

scanTokens = alexScanTokens
}