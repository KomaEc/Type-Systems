{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Types where

    import Data.STRef
    import Control.Monad.ST
    import Syntax

    type Level = Int

    instance Show (STRef s (Type s)) where
        show _ = "some ref"

    data Type s where -- phantom type s stands for state thread
        TConst :: Name -> Type s -- type constant: `int` or `bool`
        TApp :: Type s -> Type s -> Type s -- type application: `list[int]`
        TArrow :: Type s -> Type s -> Type s -- function type: `(int, int) -> int`
        TVar :: STRef s (Type s) -> Type s
        deriving (Show, Eq)

    data TVar s where
        Unbound :: Level -> String -> TVar s
        Link :: Type s -> TVar s
        deriving (Show, Eq)

    repr :: Type s -> ST s (Type s)
    repr (TVar ref) = do
        ty <- readSTRef ref
        repr ty
    repr ty = return ty


    
    
