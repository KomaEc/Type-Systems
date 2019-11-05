{
module Parser where
import Lexer
import Syntax
import Control.Monad.Except
}

%name parse
%tokentype { TokenClass }
%monad { Except String } { (>>=) } { return }
%error { parseError }


%token
    lambda                      { TokenLam }
    rec                         { TokenRec }
    '→'                         { TokenArrow }
    str                         { TokenVar $$ }
    constr                      { TokenConstr $$ }
    '='                         { TokenEq }
    '('                         { TokenLParen }
    ')'                         { TokenRParen }
    '.'                         { TokenDot }
    ':'                         { TokenColon }
    ';'                         { TokenSemiColon }
    'Π'                         { TokenPi }
    'Σ'                         { TokenSigma }
    ','                         { TokenComma }
    fun                         { TokenFun }
    sum                         { TokenSum }
    '|'                         { TokenVBar }
    '_'                         { TokenDummy }
    zero                        { TokenZero }
    one                         { TokenOne }
    two                         { TokenTwo }
    '×'                         { TokenTimes }
    'U'                         { TokenU }

%nonassoc DOT_GUARD
%nonassoc str lambda rec constr 'Π' 'Σ' fun sum zero one 'U' '('
%left '|'
%right '→'
%nonassoc '='
%left '.'
%left ',' '×'
%left APP

%%

Prog :                                              { ExprZero }
     | Decl ';' Prog                                { ExprDecl $1 $3 }                             

Expr : lambda Pat '.' Expr                          { ExprLam $2 $4 }
     | str                                          { ExprName $1 }
     | Expr Expr %prec APP                          { ExprApp $1 $2 }
     | constr Expr %prec APP                        { ExprConstr $1 $2 }
     | 'Π' Pat ColonExprDot Expr %prec DOT_GUARD    { ExprPi $2 $3 $4 }
     | 'Σ' Pat ColonExprDot Expr %prec DOT_GUARD    { ExprSigma $2 $3 $4 }
     | 'U'                                          { ExprU }
     | Expr '.' one                                 { ExprPrj1 $1 }
     | Expr '.' two                                 { ExprPrj2 $1 }
     | zero                                         { ExprZero }
     | one                                          { ExprUnit }
     | Expr ',' Expr                                { ExprProduct $1 $3 }
     | fun Choices                                  { ExprCaseFun $2 }
     | sum Choices                                  { ExprSum $2 }
     | Expr '→' Expr                                { ExprPi PatDummy $1 $3 }
     | Expr '×' Expr                                { ExprSigma PatDummy $1 $3 }
     | '(' Expr ')'                                 { $2 }

ColonExprDot : ':' Expr '.'                         { $2 }


Pat : str                           { PatName $1 }
    | Pat ',' Pat                   { PatProduct $1 $3 }
    | '_'                           { PatDummy }
    | '(' Pat ')'                   { $2 }


Choices : '(' Choices_ ')'           { $2 }

Choices_ :                           { [] }
         | ChoisesWhite              { $1 }
         | ChoisesArrow              { $1 }

ChoisesWhite : str Expr                            { [ ($1 , $2) ] }
             | str Expr '|' ChoisesWhite           { ($1 , $2) : $4 }

ChoisesArrow : str '→' Expr                            { [ ($1 , $3) ] }
             | str '→' Expr '|' ChoisesArrow           { ($1 , $3) : $5 }

Decl : Pat ':' Expr '=' Expr         { DeclRegular $1 $3 $5 }
     | rec Pat ':' Expr '=' Expr     { DeclRec $2 $4 $6 }

{
parseError :: [TokenClass] -> a
parseError _ = error "Parser Error" 
}