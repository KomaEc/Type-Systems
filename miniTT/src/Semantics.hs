{-# LANGUAGE GADTSyntax #-}

module Semantices where

import Syntax
import Control.Monad.Reader

-- a value represents an open expression in weak head normal form.
-- Neutral value : expression whose computation stopped because of an attempt to compute a variable
-- Conoical value : the form off which makes clear the head counstruction of an exapression


data Value where
    VNeutral    :: Neutral             -> Value
    VLam        :: FunCls              -> Value
    VPi         :: Value -> FunCls     -> Value
    VSigma      :: Value -> FunCls     -> Value
    VU          ::                        Value
    VProduct    :: Value -> Value      -> Value
    VZero       ::                        Value
    VUnit       ::                        Value
    VConstr     :: Name -> Value       -> Value
    VCaseFun    :: CCls                -> Value
    VSum        :: CCls                -> Value
    deriving ( Show
             , Eq )

data Neutral where
    NGeneric :: Int             -> Neutral
    NApp     :: Neutral -> Val  -> Neutral
    NFst     :: Neutral         -> Neutral
    NSnd     :: Neutral         -> Neutral
    NFun     :: CCls -> Neutral -> Neutral
    deriving ( Show
             , Eq )

data FunCls where
    Cl    :: Pattern -> Expr -> Rho -> Funcls
    ClCmp :: FunCls -> Name         -> FunCls -- closure composing a constructor

type CCls = ( Choices , Rho ) -- Choice Closure

data Rho where -- ρ = [] | ρ , p : V | ρ , p : A = M
    RNil ::                          Rho
    RVar :: Rho -> Pattern -> Val -> Rho
    RDec :: Rho -> Decl           -> Rho

eval :: MonadReader Rho m => Expr -> m Value
eval = undefined

-- the general idea of bidirectional inference : 
-- 1. Constructor terms should always be typed by innheritance.
-- 2. The constructor subterms in a destructor terms are always considered to be given enough information in the program, and therefore should be typed by synthesis.
-- 3. For each deconstructor, if the synthesized type information of the constructor subterm is adequate, then the whole term should be synthesized. Otherwise inhereted.