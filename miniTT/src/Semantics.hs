{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Semantics where

import Syntax
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Control.Lens
import Control.Lens.TH
import Data.Function ((&))

-- a value represents an open expression in weak head normal form.
-- Neutral value : expression whose computation stopped because of an attempt to compute a variable
-- Conoical value : the form of which makes clear the head counstruction of an exapression


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
    VRecUnit    :: Value                    -> Value 
    -- rec₁ shouldn't be put into neutral value. The consumption of unit type occurs in application
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
    VarNotInPattern   ::                            Errors -- an auxilary exception
    ValueNotPair      ::                            Errors
    UndefinedVariable ::                            Errors
    LookupGamma       :: Name                    -> Errors
    InsertGamma       ::                            Errors
    WrongConstructor  ::                            Errors
    TypeMismatch      ::                            Errors
    NonInferable      :: Expr                    -> Errors
    InferNeqCheck     :: Expr -> NExpr -> NExpr  -> Errors
    -- for debugging purposes
    Debug             :: String                  -> Errors
    DebugV            :: Value                   -> Errors
    DebugI            :: Int                     -> Errors
    DebugF            :: FunCls                  -> Errors
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
app (VRecUnit v) VUnit                      = return v  -- defining equation for rec₁

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
        vfst val

    eval (ExprPrj2 exp)             = do
        val <- eval exp
        vsnd val

    eval (ExprSigma pat exp1 exp2)  = do
        val1 <- eval exp1
        rho <- ask
        return $ VSigma val1 (Cl pat exp2 rho)

    eval ExprUnit                   = return VUnit

    eval (ExprConstr c exp)         = VConstr c
        <$> eval exp

    eval (ExprCaseFun choices)      = do
        rho <- ask
        return $ VCaseFun (choices, rho)

    eval (ExprSum choices)          = do
        rho <- ask
        return $ VSum (choices, rho)

    eval (ExprRecUnit m)            = VRecUnit
        <$> eval m

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
    NRecUnit :: NExpr                 -> NExpr
    deriving ( Show
             , Eq )

-- read back functions.
-- the other two read back functions are (traverse readBack)
readBack :: (MonadState Int m, MonadError Errors m) => Value -> m NExpr
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

readBack (VConstr c v) = NConstr c
    <$> readBack v

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

readBack (VNeutral ne) = NNeutral 
    <$> traverse readBack ne

readBack (VRecUnit v) = NRecUnit
    <$> readBack v

-- typing environment Γ
type Gamma = [ ( Name, Value ) ]

-- Γ(x) → t
lookupG' :: MonadError Errors m => Gamma -> Name -> m Value
lookupG' []                x  = throwError $ LookupGamma x
lookupG' ((y, t) : gamma') x
    | x == y    = return t
    | otherwise = lookupG' gamma' x

-- directly update a binding p : t = v to Γ
insertG' :: MonadError Errors m => Pattern -> Value -> Value -> Gamma -> m Gamma
insertG' (PatName x)        t             _ gamma = return $ (x, t) : gamma
insertG' PatDummy           _             _ gamma = return gamma
insertG' (PatProduct p1 p2) (VSigma t1 g) v gamma = do
    v1     <- vfst v
    gamma1 <- insertG' p1 t1 v1 gamma
    t2     <- inst g v1
    v2     <- vsnd v
    insertG' p2 t2 v2 gamma1
insertG' _                  _             _ _     =  throwError InsertGamma

-- monad and related structure for type checking
data TCState = TCState {
      _freeCnt :: Int
    , _venv    :: Rho Value
    , _tenv    :: Gamma
}   deriving ( Show
             , Eq )
makeLenses ''TCState

initTCState :: TCState
initTCState = TCState 0 RNil []

type TC = ReaderT TCState (
          Except Errors)

type MonadTC m = (MonadError Errors m, MonadReader TCState m)

runTC :: TC a -> Either Errors a
runTC m = runExcept $ runReaderT m initTCState

eval' :: (MonadError Errors m, MonadReader TCState m, Eval a) => a -> m Value
eval' x = do
    rho <- view venv
    runReaderT (eval x) rho

readBack' :: (MonadError Errors m, MonadReader TCState m) => Value -> m NExpr
readBack' v = do
    l <- view freeCnt
    evalStateT (readBack v) l

insertG :: MonadTC m => Pattern -> Value -> Value -> m Gamma
insertG pat t v = do
    gamma <- view tenv
    insertG' pat t v gamma

lookupG :: MonadTC m => Name -> m Value
lookupG x = do
    gamma <- view tenv
    lookupG' gamma x

newGenericVal :: MonadTC m => m Value
newGenericVal = VNeutral . NeuGeneric
    <$> view freeCnt

-- lookup a name in a choice
lookupC :: MonadTC m => Name -> Choices -> m Expr
lookupC x c = 
    case lookup x c of
        Just expr -> return expr
        Nothing   -> throwError WrongConstructor

--  check that constructors are matched
checkConstrMatch :: MonadTC m => Choices -> Choices -> m ()
checkConstrMatch ((c, _) : choices) ((c', _) : choices') 
    | c == c'   = checkConstrMatch choices choices'
    | otherwise = throwError WrongConstructor
checkConstrMatch []                 [] = return ()
checkConstrMatch _                  _  = throwError WrongConstructor


-- 4 forms of judgement

-- checking a definition is correct, and extends typing environment
checkD :: MonadTC m => Decl -> m Gamma

-- checking an expression is a correct type expression
checkT :: MonadTC m => Expr -> m ()

-- checking an expression is a correct on of a given type
check :: MonadTC m => Expr -> Value -> m ()

-- checking an expressionn is a correct one, and infer its type
infer :: MonadTC m => Expr -> m Value

-- ρ, Γ ⊢ p : A = M ⟹ Γ₁
checkD (DeclRegular pat expr1 expr2) = do
    checkT expr1    -- ρ, Γ ⊢ A
    t <- eval' expr1 -- t = eval A
    check expr2 t   -- ρ, Γ ⊢ M ⟸ A
    v <- eval' expr2 -- v = eval M
    insertG pat t v

-- ρ, Γ ⊢ rec p : A = M ⟹ Γ₂
checkD d@(DeclRec pat expr1 expr2) = do
    checkT expr1    -- ρ, Γ ⊢ A
    t <- eval' expr1 -- t = eval A
    xl <- newGenericVal
    gamma1 <- insertG pat t xl -- Γ ⊢ p : t = [x_l] ⟹ Γ₁
    local (over venv (\rho -> RVar rho pat xl) . 
           over tenv (const gamma1) .
           over freeCnt (+1)) (check expr2 t)
    -- ((ρ, p = xl), Γ₁ ⊢_(l+1) M ⟸ t
    -- recursively defined ifentifiers are treated as fresh connstants about which we assume nothing but their typing
    v <- local (over venv (`RDec` d)) (eval' expr2)
    insertG pat t v

-- ρ, Γ ⊢ U
checkT ExprU =  return () 

-- ρ, Γ ⊢ Π p : A . B
checkT (ExprPi pat expr1 expr2) = do
    checkT expr1 -- ρ, Γ ⊢ A
    t <- eval' expr1 --  t = eval A
    xl <- newGenericVal
    gamma1 <- insertG pat t xl
    local (over venv (\rho -> RVar rho pat xl) . 
           over tenv (const gamma1) .
           over freeCnt (+1)) (checkT expr2) 
    -- ((ρ, p = xl), Γ₁ ⊢_(l+1)  B

-- ρ, Γ ⊢ Σ ρ : A . B
checkT (ExprSigma pat expr1 expr2) = do
    checkT expr1 -- ρ, Γ ⊢ A
    t <- eval' expr1 --  t = eval A
    xl <- newGenericVal
    gamma1 <- insertG pat t xl
    local (over venv (\rho -> RVar rho pat xl) . 
           over tenv (const gamma1) .
           over freeCnt (+1)) (checkT expr2) 
    -- ((ρ, p = xl), Γ₁ ⊢_(l+1)  B

-- otherwise, check that A is of type U
checkT expr = check expr VU

-- ρ, Γ ⊢ λ p. M ⟸ Π t g
check (ExprLam pat m) (VPi t g) = do
    xl <- newGenericVal
    gamma1 <- insertG pat t xl -- Γ ⊢ p : t = [xl] ⟹ Γ₁
    g' <- inst g xl -- g' = inst g [xl]
    local (over venv (\rho -> RVar rho pat xl) . 
           over tenv (const gamma1) .
           over freeCnt (+1)) $ check m g'
    -- (ρ \, p = [xl]), Γ₁ ⊢ M ⟸ inst g [xl]

-- ρ, Γ ⊢ (M, N) ⟸ Σ t g
check (ExprProduct m n) (VSigma t g) = do
    check m t  -- ρ, Γ ⊢ M ⟸ t
    mv <- eval' m
    g' <- inst g mv
    check n g' -- ρ, Γ ⊢ N ⟸ inst g (eval M)

-- ρ, Γ ⊢ cᵢ M ⟸ Sum <..., ν>
check (ExprConstr c m) (VSum (choices, nu)) = do
    a <- lookupC c choices
    av <- local (over venv (const nu)) (eval' a)
    check m av -- ρ, Γ ⊢ M ⟸ eval Aᵢ

-- ρ, Γ ⊢ fun (...) ⟸ Π (Sum <..., ν>) g
check (ExprCaseFun choices) (VPi (VSum (choices', nu)) g) = do
    checkConstrMatch choices choices'
    zipWithM_ (\(c, m) (_, a) -> do
        a' <- local (over venv (const nu)) (eval' a)
        check m (VPi a' (ClCmp g c))) choices choices'
    -- ρ, Γ ⊢ Mᵢ ⟸ Π (eval Aᵢ) (g ∘ cᵢ)
    
-- ρ, Γ ⊢ D; M ⟸ t
check (ExprDecl decl m) t = do
    gamma1 <- checkD decl -- ρ, Γ ⊢ D ⟹ Γ₁
    local (over venv (`RDec` decl) . 
           over tenv (const gamma1)) $ check m t
    -- (ρ, D), Γ₁ ⊢ M ⟸ t

-- ρ, Γ ⊢ 0 ⟸ 1
check ExprZero VUnit = return ()

-- ρ, Γ ⊢ 1 ⟸ U
check ExprUnit VU = return ()

-- ρ, Γ ⊢ Π p : A . B ⟸ U
check (ExprPi pat a b) VU = do
    check a VU -- ρ, Γ ⊢ A ⟸ U
    t <- eval' a --  t = eval A
    xl <- newGenericVal
    gamma1 <- insertG pat t xl
    local (over venv (\rho -> RVar rho pat xl) . 
           over tenv (const gamma1) .
           over freeCnt (+1)) (check b VU) 
    -- ((ρ, p = xl), Γ₁ ⊢_(l+1)  B ⟸ U

-- ρ, Γ ⊢ Σ p : A . B ⟸ U
check (ExprSigma pat a b) VU = do
    check a VU -- ρ, Γ ⊢ A ⟸ U
    t <- eval' a --  t = eval A
    xl <- newGenericVal
    gamma1 <- insertG pat t xl
    local (over venv (\rho -> RVar rho pat xl) . 
           over tenv (const gamma1) .
           over freeCnt (+1)) (check b VU) 
    -- ((ρ, p = xl), Γ₁ ⊢_(l+1)  B ⟸ U

-- ρ, Γ ⊢ Sum (...) ⟸ U
check (ExprSum choices) VU = 
    forM_ choices 
        (\(_, a) -> check a VU)

-- ρ, Γ ⊢ rec₁ M ⟸ 1 → A
check (ExprRecUnit m) (VPi VUnit f) = do
    a <- inst f VZero -- TODO : inst 0 ???
    check m a -- ρ, Γ ⊢ M ⟸ A

-- otherwise we must infer its type
check m t = do
    t' <- infer m
    n <- readBack' t
    n' <- readBack' t'
    if n == n' 
        then return ()
        else throwError $ InferNeqCheck m n n'

-- ρ, Γ ⊢ x ⟹ t
infer (ExprName x) = lookupG x 

-- ρ, Γ ⊢ M N  ⟹ inst g (eval N)
infer (ExprApp m n) = do
    mt <- infer m
    case mt of
        VPi t g -> do
            check n t
            v <- eval' n
            inst g v
        _       -> throwError TypeMismatch

-- ρ, Γ ⊢ M.1 ⟹ t
infer (ExprPrj1 m) = do
    t' <- infer m
    case t' of
        VSigma t _ -> return t
        _          -> throwError TypeMismatch

-- ρ, Γ ⊢ M.2 ⟹ inst g ((eval M).1)
infer (ExprPrj2 m) = do
    t <- infer m
    case t of
        VSigma _ g -> do
            v <- eval' m
            v' <- vfst v
            inst g v'
        _          -> throwError TypeMismatch

infer expr = throwError $ NonInferable expr


-- the general idea of bidirectional inference (maybe?) : 
-- 1. Constructor terms should always be typed by innheritance. (Weak head normal form)
-- 2. The constructor subterms in a destructor terms are always considered to be given enough information in the program, and therefore should be typed by synthesis.
-- 3. For each deconstructor, if the synthesized type information of the constructor subterm is adequate, then the whole term should be synthesized. Otherwise inhereted.