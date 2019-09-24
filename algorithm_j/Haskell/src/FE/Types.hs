{-# LANGUAGE GADTs #-}

module FE.Types where

    import Data.IORef
    import Syntax
    import Control.Effect

    type Level = Int

    data Type where
        TConst :: Name -> Type
        TApp :: Type -> Type -> Type
        TArrow :: Type -> Type -> Type
        TVar :: IORef TVar -> Type
        deriving Eq

    data TVar where
        Unbound :: Level -> String -> TVar
        Link :: Type -> TVar
        deriving Eq

    