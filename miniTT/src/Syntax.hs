{-# LANGUAGE GADTSyntax #-}

module Syntax where

type Name = String

data Pattern where
    PatName         :: Name                -> Pattern
    PatProduct      :: Pattern -> Pattern  -> Pattern
    PatDummy        ::                        Pattern
    deriving ( Eq
             , Show )

data Expr where
    ExprLam         :: Pattern -> Expr ->         Expr
    ExprName        :: Name ->                    Expr
    ExprApp         :: Expr -> Expr ->            Expr
    ExprPi          :: Pattern -> Expr -> Expr -> Expr
    ExprU           ::                            Expr
    ExprProduct     :: Expr -> Expr ->            Expr
    ExprPrj1        :: Expr ->                    Expr
    ExprPrj2        :: Expr ->                    Expr
    ExprSigma       :: Pattern -> Expr -> Expr -> Expr
    ExprZero        ::                            Expr
    ExprUnit        ::                            Expr
    ExprConstr      :: Name -> Expr ->            Expr
    ExprCaseFun     :: Choices ->                 Expr
    ExprSum         :: Choices ->                 Expr
    ExprDecl        :: Decl -> Expr ->            Expr
    deriving ( Eq
             , Show )

type Choices = [ (Name , Expr) ]

data Decl where
    DeclRegular     :: Pattern -> Expr -> Expr -> Decl
    DeclRec         :: Pattern -> Expr -> Expr -> Decl
    deriving ( Eq
             , Show )
