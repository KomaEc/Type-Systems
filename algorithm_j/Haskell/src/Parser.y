{
module Parser where
import Lexer
import Syntax
}

%name parse
%tokentype { TokenClass }
%error { parseError }

%token
    let         { TokenLet }
    in          { TokenIn }
    lambda      { TokenLam }
    arrows      { TokenArrow }
    var         { TokenVar $$ }
    '='         { TokenEq }
    '('         { TokenLParen }
    ')'         { TokenRParen }

%right in

%%

Exp : let var '=' Exp in Exp        { Let $2 $4 $6 }
    | Exp Exp                       { App $1 $2 }
    | lambda var arrows Exp         { Lam $2 $4 }
    | var                           { Var $1 }

{

parseError :: [TokenClass] -> a
parseError _ = error "Parser Error" 
}