{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

import Data.IORef
import Syntax
import Utils

type Level = Int

data TVar where
    Unbound :: Name -> Level -> TVar
    Link    :: Type          -> TVar

data Type where
    TVar   :: IORef TVar   -> Type   -- normal type variable
    QVar   :: Name         -> Type   -- quantified variable a, forall a . ...
    TArrow :: Type -> Type -> Type   -- arrow type a -> b

instance Show (IORef TVar) where
    show _ = "ref"

stringOfType :: Type -> IO String
stringOfType (TVar ref) = do
    tv <- readIORef ref
    stringOfTVar tv
stringOfType (QVar x) = return x
stringOfType (TArrow ty1 ty2) = do
    s1 <- stringOfType ty1
    s2 <- stringOfType ty2
    return $ s1 ++ " -> " ++ s2

stringOfTVar :: TVar -> IO String
stringOfTVar (Unbound x l) = return x
stringOfTVar (Link ty) = stringOfType ty

instance ShowIO TVar where
    showIO = stringOfTVar

instance ShowIO Type where
    showIO = stringOfType
