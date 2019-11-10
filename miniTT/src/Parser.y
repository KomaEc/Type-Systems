{
module Parser ( parseExpr ) where
import Lexer
import Syntax
import Control.Monad.Except
}

%name parse
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ TokenEOF }
-- Without this we get a type error
%error { happyError }


%token
    lambda                      { Token _ TokenLam }
    recUnit                     { Token _ TokenRecUnit }
    rec                         { Token _ TokenRec }
    '→'                         { Token _ TokenArrow }
    str                         { Token _ (TokenVar $$) }
    constr                      { Token _ (TokenConstr $$) }
    '='                         { Token _ TokenEq }
    '('                         { Token _ TokenLParen }
    ')'                         { Token _ TokenRParen }
    '.'                         { Token _ TokenDot }
    ':'                         { Token _ TokenColon }
    ';'                         { Token _ TokenSemiColon }
    'Π'                         { Token _ TokenPi }
    'Σ'                         { Token _ TokenSigma }
    ','                         { Token _ TokenComma }
    fun                         { Token _ TokenFun }
    sum                         { Token _ TokenSum }
    '|'                         { Token _ TokenVBar }
    '_'                         { Token _ TokenDummy }
    zero                        { Token _ TokenZero }
    one                         { Token _ TokenOne }
    two                         { Token _ TokenTwo }
    '×'                         { Token _ TokenTimes }
    'U'                         { Token _ TokenU }

%nonassoc DOT_GUARD
%nonassoc SINGLE_CONSTR
%left '|'
%left '.'
%right '→'
%nonassoc str lambda rec recUnit constr 'Π' 'Σ' fun sum zero one 'U'
%nonassoc '='
%left ',' '×'
%nonassoc '('
%left APP

%%

Prog :                                              { ExprZero }
     | Decl ';' Prog                                { ExprDecl $1 $3 }                             

Expr : lambda Pat '.' Expr                          { ExprLam $2 $4 }
     | str                                          { ExprName $1 }
     | Expr Expr %prec APP                          { ExprApp $1 $2 }
     | constr  %prec SINGLE_CONSTR                  { ExprConstr $1 ExprZero }
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
     | recUnit Expr                                 { ExprRecUnit $2 }

ColonExprDot : ':' Expr '.'                         { $2 }


Pat : str                           { PatName $1 }
    | Pat ',' Pat                   { PatProduct $1 $3 }
    | '_'                           { PatDummy }
    | '(' Pat ')'                   { $2 }


Choices : '(' Choices_ ')'           { $2 }

Choices_ :                           { [] }
         | ChoisesWhite              { $1 }
         | ChoisesArrow              { $1 }

ChoisesWhite : constr Expr                            { [ ($1 , $2) ] }
             | constr                                 { [ ($1 , ExprUnit) ] }
             | constr Expr '|' ChoisesWhite           { ($1 , $2) : $4 }
             | constr '|' ChoisesWhite                { ($1 , ExprUnit) : $3 }

ChoisesArrow : constr Pat '→' Expr                            { [ ($1 , ExprLam $2 $4) ] }
             | constr '→' Expr                                { [ ($1 , ExprRecUnit $3) ] }
             | constr Pat '→' Expr '|' ChoisesArrow           { ($1 , ExprLam $2 $4) : $6 }
             | constr '→' Expr '|' ChoisesArrow               { ($1 , ExprRecUnit $3) : $5 }

Decl : Pat ':' Expr '=' Expr         { DeclRegular $1 $3 $5 }
     | rec Pat ':' Expr '=' Expr     { DeclRec $2 $4 $6 }

{

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError (Token p t) =
    alexError' p ("parse error at token '" ++ show t ++ "'")

parseExpr:: FilePath -> String -> Either String Expr
parseExpr = runAlex' parse
}