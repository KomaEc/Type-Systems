{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TupleSections #-}

module Semantics where

import Syntax
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State

-- a value represents an open expression in weak head normal form.
-- Neutral value : expression whose computation stopped because of an attempt to compute a variable
-- Conoical value : the form off which makes clear the head counstruction of an exapression


data Value where
    VNeutral    :: Neutral Value            -> Value
    VLam        :: FunCls                   -> Value
    VPi         :: Value -> FunCls          -> Value
    VSigma      :: Value -> FunCls          -> Value
    VU          ::                             Value
    VProduct    :: Value -> Value           -> Value
    VZero       ::                             Value
    VUnit       ::                             Value
    VConstr     :: Name -> Value            -> Value
    VCaseFun    :: CCls Value               -> Value
    VSum        :: CCls Value               -> Value
    deriving ( Show
             , Eq )

-- parameterized neutral terms
-- value can be Value or NExpr
data Neutral value where
    NeuGeneric :: Int                             -> Neutral value
    NeuApp     :: Neutral value -> value          -> Neutral value
    NeuFst     :: Neutral value                   -> Neutral value
    NeuSnd     :: Neutral value                   -> Neutral value
    NeuFun     :: CCls value -> Neutral value     -> Neutral value
    deriving ( Show
             , Eq
             , Functor
             , Foldable )

instance Traversable Neutral where
    sequenceA (NeuGeneric i) = pure $ NeuGeneric i
    
    sequenceA (NeuApp neuA vA) = NeuApp
        <$> sequenceA neuA
        <*> vA

    sequenceA (NeuFst neuA) = NeuFst
        <$> sequenceA neuA

    sequenceA (NeuSnd neuA) = NeuSnd
        <$> sequenceA neuA

    sequenceA (NeuFun (choices, rhoA) neuA) = NeuFun
        <$> ((choices, ) <$> sequenceA rhoA)
        <*> sequenceA neuA

-- function closure
data FunCls where
    Cl    :: Pattern -> Expr -> Rho Value -> FunCls -- should it be Rho value  ?????? 
    ClCmp :: FunCls -> Name               -> FunCls -- closure composing a constructor
    deriving ( Show
             , Eq )

-- choice (case tree) closure
type CCls value = ( Choices , Rho value )

-- environment
data Rho value where -- ρ = [] | ρ , p = V | ρ , p : A = M
    RNil ::                                  Rho value
    RVar :: Rho value -> Pattern -> value -> Rho value
    RDec :: Rho value -> Decl             -> Rho value
    deriving ( Show
             , Eq
             , Functor
             , Foldable )

instance Traversable Rho where
    sequenceA RNil = pure RNil

    sequenceA (RVar rhoA pat vA) = RVar
        <$> sequenceA rhoA
        <*> pure pat
        <*> vA

    sequenceA  (RDec rhoA decl) = RDec
        <$> sequenceA rhoA
        <*> pure decl

-- all the errors that handled in the semantics analysis phase.
data Errors where
    VarNotInPattern   :: Errors -- an auxilary exception
    ValueNotPair      :: Errors
    UndefinedVariable :: Errors
    LookupGamma       :: Errors
    InsertGamma       :: Errors
    deriving Show

-- obtain the value of x from the pattern p that contains x and its value.
proj :: MonadError Errors m => Pattern -> Value -> Name -> m Value
proj (PatName y) val x
    | x == y    = return val
    | otherwise = throwError VarNotInPattern
proj (PatProduct pat1 pat2) val x = do
        val1 <- vfst val
        proj pat1 val1 x 
    `catchError` \case 
            VarNotInPattern -> do
                val2 <- vsnd val
                proj pat2 val2 x
proj PatDummy _ _                 = throwError VarNotInPattern

assertVarInPattern :: MonadError Errors m => Pattern -> Name -> m ()
assertVarInPattern (PatName y) x 
    | x == y    = return ()
    | otherwise = throwError VarNotInPattern
assertVarInPattern (PatProduct pat1 pat2) x = 
    assertVarInPattern pat1 x `catchError` \case
        VarNotInPattern -> assertVarInPattern pat2 x
assertVarInPattern PatDummy _               = throwError VarNotInPattern


-- v ~> v.1
vfst :: MonadError Errors m => Value -> m Value
vfst (VProduct v1 _) = return v1
vfst (VNeutral n)    = return $ VNeutral (NeuFst n)
vfst _               = throwError ValueNotPair

-- v ~> v.2
vsnd :: MonadError Errors m => Value -> m Value
vsnd (VProduct _ v2) = return v2
vsnd (VNeutral n)    = return $ VNeutral (NeuSnd n)
vsnd _               = throwError ValueNotPair

-- application between values
app :: MonadError Errors m => Value -> Value -> m Value
app (VLam fcls) v                           = inst fcls v
app (VCaseFun (choices, rho)) (VConstr c v) = -- construct a reader monad Rho -> m a
    let exp = fst . head $ filter (\ (x, _) -> x == c) choices
    in  do
            val <- runReaderT (eval exp) rho
            app val v
app (VCaseFun ccls) (VNeutral n)            = return . VNeutral $ NeuFun ccls n
app (VNeutral n) v                          = return . VNeutral $ NeuApp n v

-- instantiates a function closure to a value
inst :: MonadError Errors m => FunCls -> Value -> m Value
inst (Cl pat exp rho) v = runReaderT (eval exp) (RVar rho pat v)
inst (ClCmp fcls c) v   = inst fcls (VConstr c v)

-- operational semantics of miniTT
class Eval a where
    eval :: (MonadReader (Rho Value) m, MonadError Errors m) => a -> m Value

instance Eval Expr where
    eval (ExprLam pat exp)          = do
        rho <- ask
        return . VLam $ Cl pat exp rho

    eval (ExprName x)               = eval x

    eval (ExprApp exp1 exp2)        = do
        val1 <- eval exp1
        val2 <- eval exp2
        app val1 val2

    eval (ExprPi pat exp1 exp2)     = do
        val1 <- eval exp1
        rho <- ask
        return $ VPi val1 (Cl pat exp2 rho)

    eval ExprU                      = return VU

    eval (ExprDecl d exp)           = local (`RDec` d) (eval exp)

    eval (ExprProduct exp1 exp2)    = do
        val1 <- eval exp1
        val2 <- eval exp2
        return $ VProduct val1 val2

    eval ExprZero                   = return VZero

    eval (ExprPrj1 exp)             = do
        val <- eval exp
        val' <- vfst val
        return val'

    eval (ExprPrj2 exp)             = do
        val <- eval exp
        val' <- vsnd val
        return val'

    eval (ExprSigma pat exp1 exp2)  = do
        val1 <- eval exp1
        rho <- ask
        return $ VSigma val1 (Cl pat exp2 rho)

    eval ExprUnit                   = return VUnit

    eval (ExprConstr c exp)         = do
        val <- eval exp
        return $ VConstr c val

    eval (ExprCaseFun choices)      = do
        rho <- ask
        return $ VCaseFun (choices, rho)

    eval (ExprSum choices)          = do
        rho <- ask
        return $ VSum (choices, rho)

instance Eval Name where
    eval x = do
        rho <- ask
        case rho of
            RVar rho' pat val                     -> 
                proj pat val x
                    `catchError` \case
                            VarNotInPattern -> local (const rho') (eval x)
            RDec rho' (DeclRegular pat exp1 exp2) -> do
                    assertVarInPattern pat x
                    val <- local (const rho') (eval exp2)
                    proj pat val x
                `catchError`
                    \case
                        VarNotInPattern -> local (const rho') (eval x)
            RDec rho' (DeclRec pat exp1 exp2)     -> do
                    assertVarInPattern pat x
                    val <- eval exp2 -- use the original ρ, which contains the definition of pat
                    proj pat val x
                `catchError`
                    \case
                        VarNotInPattern -> local (const rho') (eval x)
            RNil                                  -> 
                throwError UndefinedVariable

-- normal expressions
data NExpr where
    NLam     :: Int -> NExpr          -> NExpr
    NPi      :: Int -> NExpr -> NExpr -> NExpr
    NU       ::                          NExpr
    NNeutral :: Neutral NExpr         -> NExpr
    NProduct :: NExpr -> NExpr        -> NExpr
    NZero    ::                          NExpr
    NSigma   :: Int -> NExpr -> NExpr -> NExpr
    NUnit    ::                          NExpr
    NConstr  :: Name -> NExpr         -> NExpr
    NCaseFun :: CCls NExpr            -> NExpr
    NSum     :: CCls NExpr            -> NExpr
    deriving ( Show
             , Eq )

type Nat = Int

-- read back functions.
-- the other two read back functions are (traverse readBack)
readBack :: (MonadState Nat m, MonadError Errors m) => Value -> m NExpr
readBack (VLam fcls) = do
    i <- get
    modify (+1)
    v <- inst fcls (VNeutral $ NeuGeneric i)
    nexpr <- readBack v
    return $ NLam i nexpr

readBack  (VProduct u v) = do
    n1 <- readBack u
    n2 <- readBack v
    return $ NProduct n1 n2

readBack VZero = return NZero

readBack (VConstr c v) = do
    n <- readBack v
    return $ NConstr c n

readBack (VCaseFun (choices, rho)) = do
    rho' <- traverse readBack rho
    return $ NCaseFun (choices, rho') 

readBack (VSum (choices, rho)) = do
    rho' <- traverse readBack rho
    return $ NSum (choices, rho')

readBack VU = return NU

readBack VUnit = return NUnit

readBack (VPi v fcls) = do
    i <- get
    n1 <- readBack v
    modify (+1)
    u <- inst fcls (VNeutral $ NeuGeneric i) -- inst g[xᵢ]
    n2 <- readBack u
    return $ NPi i n1 n2

readBack (VSigma v fcls) = do
    i <- get
    n1 <- readBack v
    modify (+1)
    u <- inst fcls (VNeutral $ NeuGeneric i) -- inst g[xᵢ]
    n2 <- readBack u
    return $ NSigma i n1 n2

readBack (VNeutral ne) = do
    n <- traverse readBack ne
    return $ NNeutral n

{-}
class ReadBack a b where
    readBack :: (MonadState Nat m, MonadError Errors m) => a -> m b

instance ReadBack Value NExpr where

    readBack (VLam fcls) = do
        i <- get
        put (i + 1)
        v <- inst fcls (VNeutral $ NeuGeneric i)
        nexpr <- readBack v
        return $ NLam i nexpr

    readBack  (VProduct u v) = do
        n1 <- readBack u
        n2 <- readBack v
        return $ NProduct n1 n2

    readBack VZero = return NZero

    readBack (VConstr c v) = do
        n <- readBack v
        return $ NConstr c n

    readBack (VCaseFun (choices, rho)) = do
        rho' <- readBack rho
        return $ NCaseFun (choices, rho') 

    readBack (VSum (choices, rho)) = do
        rho' <- readBack rho
        return $ NSum (choices, rho')

    readBack VU = return NU

    readBack VUnit = return NUnit

    readBack (VPi v fcls) = do
        i <- get
        n1 <- readBack v
        put (i + 1)
        u <- inst fcls (VNeutral $ NeuGeneric i) -- inst g[xᵢ]
        n2 <- readBack u
        return $ NPi i n1 n2

    readBack (VSigma v fcls) = do
        i <- get
        n1 <- readBack v
        put (i + 1)
        u <- inst fcls (VNeutral $ NeuGeneric i) -- inst g[xᵢ]
        n2 <- readBack u
        return $ NSigma i n1 n2

    readBack (VNeutral ne) = do
        n <- readBack ne
        return $ NNeutral n

instance ReadBack (Neutral Value) (Neutral NExpr) where

    readBack (NeuGeneric i) = return (NeuGeneric i)

    readBack (NeuApp n v) = do
        neuval <- readBack n
        nexpr  <- readBack v
        return $ NeuApp neuval nexpr

    readBack (NeuFst n) = do
        neuval <- readBack n
        return $ NeuFst neuval

    readBack (NeuSnd n) = do
        neuval <- readBack n
        return $ NeuSnd neuval    

    readBack (NeuFun (choices, rho) k) = do
        rho' <- readBack rho
        k'   <- readBack k
        return $ NeuFun (choices, rho') k'

instance ReadBack (Rho Value) (Rho NExpr) where
    
    readBack (RVar rho pat v) = do
        rho' <- readBack rho
        n    <- readBack v
        return $ RVar rho' pat n

    readBack (RDec rho d) = do
        rho' <- readBack rho
        return $ RDec rho' d

    readBack RNil = return RNil
-}

-- typing environment Γ
type Gamma = [ ( Name, Value ) ]

-- Γ(x) → t
lookUp :: MonadError Errors m => Gamma -> Name -> m Value
lookUp []                _  = throwError LookupGamma
lookUp ((y, t) : gamma') x
    | x == y    = return t
    | otherwise = lookUp gamma' x

-- directly update a binding p : t = v to Γ
insert :: MonadError Errors m => Pattern -> Value -> Value -> Gamma -> m Gamma
insert (PatName x)        t             _ gamma = return $ (x, t) : gamma
insert PatDummy           _             _ gamma = return gamma
insert (PatProduct p1 p2) (VSigma t1 g) v gamma = do
    v1     <- vfst v
    gamma1 <- insert p1 t1 v1 gamma
    t2     <- inst g v1
    v2     <- vsnd v
    insert p2 t2 v2 gamma1
insert _                  _             _ _     =  throwError InsertGamma

-- monad and related structure for type checking
data TCState = TCState {
      dIndex :: Int
    , venv   :: Rho Value
    , tenv   :: Gamma
}   deriving ( Show
             , Eq )

initTCState :: TCState
initTCState = TCState 0 RNil []

type TC = ReaderT TCState (
          Except Errors)

runTC :: TC a -> Either Errors a
runTC m = runExcept $ runReaderT m initTCState

eval' :: (MonadError Errors m, MonadReader TCState m, Eval a) => a -> m Value
eval' x = do
    rho <- asks venv
    runReaderT (eval x) rho

-- 4 forms of judgement

-- checking a definition is correct, and extends typing environment
checkD :: (MonadError Errors m, MonadReader TCState m) => Decl -> m Gamma

-- checking an expression is a correct type expression
checkT :: (MonadError Errors m, MonadReader TCState m) => Expr -> m ()

-- checking an expression is a correct on of a given type
check :: (MonadError Errors m, MonadReader TCState m) => Expr -> Value -> m ()

-- checking an expressionn is a correct one, and infer its type
infer :: (MonadError Errors m, MonadReader TCState m) => Expr -> Value -> m Value

-- ρ, Γ ⊢ p : A = M ⟹ Γ₁
checkD (DeclRegular pat expr1 expr2) = do
    checkT expr1    -- ρ, Γ ⊢ A
    t <- eval' expr1 -- t = eval A
    check expr1 t   -- ρ, Γ ⊢ M ⟸ A
    v <- eval' expr2 -- v = eval M
    gamma <- asks tenv
    insert pat t v gamma

-- ρ, Γ ⊢ rec p : A = M ⟹ Γ₂
checkD (DeclRec pat expr1 expr2) = do
    checkT expr1    -- ρ, Γ ⊢ A
    t <- eval' expr1 -- t = eval A
    undefined

checkT = undefined

check = undefined

infer = undefined

-- the general idea of bidirectional inference (maybe?) : 
-- 1. Constructor terms should always be typed by innheritance. (Weak head normal form)
-- 2. The constructor subterms in a destructor terms are always considered to be given enough information in the program, and therefore should be typed by synthesis.
-- 3. For each deconstructor, if the synthesized type information of the constructor subterm is adequate, then the whole term should be synthesized. Otherwise inhereted.