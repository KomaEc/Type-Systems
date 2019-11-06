{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE LambdaCase #-}

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

data Rho where -- ρ = [] | ρ , p = V | ρ , p : A = M
    RNil ::                          Rho
    RVar :: Rho -> Pattern -> Val -> Rho
    RDec :: Rho -> Decl           -> Rho

data Errors where
    VarNotInPattern :: Errors

-- obtain the value of x from the pattern p that contains x and its value.
proj :: MonadError Errors m => Pattern -> Value -> Name -> m Value
proj (PatName y) val x
    | x == y    = return val
    | otherwise = throwError VarNotInPattern
proj (PatProduct pat1 pat2) val x =
    proj pat1 (vfst val) x `catchError` \case 
        VarNotInPattern -> proj pat2 (vsnd val) x
proj PatDummy _ _ = throwError VarNotInPattern


-- v ~> v.1
vfst :: Monad m => Value -> m Value
vfst (VProduct v1 _) = return v1
vfst (VNeutral n) = return $ VNeutral (NFst n)

-- v ~> v.2
vsnd :: Monad m => Value -> m Value
vsnd (VProduct _ v2) = return v2
vsnd (VNeutral n) = return $ VNeutral (NSnd n)

class Evaluatable a where
    eval :: (MonadReader Rho m, MonadError Errors m) => a -> m Value

instance Evaluatable Expr where
    eval (ExprLam pat exp) = do
        rho <- ask
        return . VLam $ Cl pat exp rho
    eval (ExprName x) = eval x
    eval (ExprApp exp1 exp2) = do
        val1 <- eval exp1
        val2 <- eval exp2
        return $ app val1 val2
    eval (ExprPi pat exp1 exp2) = do
        val1 <- eval exp1
        rho <- ask
        return $ VPi val1 (Cl pat exp2 rho)
    eval ExprU = return VU
    eval (ExprDecl d exp) = local (`RDec` d) (eval exp)
    eval (ExprProduct exp1 exp2) = do
        val1 <- eval exp1
        val2 <- eval exp2
        return $ VProduct val1 val2
    eval ExprZero = return VZero
    eval (ExprPrj1 exp) = do
        val <- eval exp
        return $ case val of
                    VProduct v1 _ -> v1
                    VNeutral n    -> VNeutral $ NFst n
                    -- impossible to go here!!!!!
    eval (ExprPrj2 exp) = do
        val <- eval exp
        return $ case val of 
                    VProduct _ v2 -> v2
                    VNeutral n    -> VNeutral $ NSnd n
                    -- impossible to go here!!!!!
    eval (ExprSigma pat exp1 exp2) = do
        val1 <- eval exp1
        rho <- ask
        return $ VSigma val1 (Cl pat exp2 rho)
    eval ExprUnit = return VUnit
    eval (ExprConstr c exp) = do
        val <- eval exp
        return $ VConstr c val
    eval (ExprCaseFun choices) = undefined
    eval (ExprSum choices) = undefined

instance Evaluatable Name where
    eval x = do
        rho <- ask
        case rho of
            RVar rho' pat val                     -> proj pat val x
                                                        `catchError` \case
                                                            VarNotInPattern -> local (const rho') eval x
            RDec rho' (DeclRegular pat exp1 exp2) -> do
                val <- eval exp2 -- ??? shouldn't evaluate it first
                proj pat val x
                undefined



-- the general idea of bidirectional inference : 
-- 1. Constructor terms should always be typed by innheritance.
-- 2. The constructor subterms in a destructor terms are always considered to be given enough information in the program, and therefore should be typed by synthesis.
-- 3. For each deconstructor, if the synthesized type information of the constructor subterm is adequate, then the whole term should be synthesized. Otherwise inhereted.