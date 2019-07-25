{-# LANGUAGE GADTs #-}

module Syntax (Name, Expr(..)) where

    type Name = String

    data Expr where 
        Var :: Name -> Expr
        App :: Expr -> Expr -> Expr
        Lam :: String -> Expr -> Expr
        Let :: String -> Expr -> Expr
        deriving (Show, Eq)

    