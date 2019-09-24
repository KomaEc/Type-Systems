{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module MTL.Types' where

    import Data.STRef
    import Control.Monad.ST
    import Syntax

    type Level = Int

    instance Show (STRef s (TVar s)) where
        show _ = "some ref"

    data Type s where -- phantom type s stands for state thread
        TConst :: Name -> Type s -- type constant: `int` or `bool`
        TApp :: Type s -> Type s -> Type s -- type application: `list[int]`
        TArrow :: Type s -> Type s -> Type s -- function type: `(int, int) -> int`
        TVar :: STRef s (TVar s) -> Type s
        deriving (Show, Eq)

    data TVar s where
        Unbound :: Level -> String -> TVar s
        Link :: Type s -> TVar s
        deriving (Show, Eq)

    repr :: Type s -> ST s (Type s)
    repr ty@(TVar ref) = do
        tvar <- readSTRef ref
        case tvar of
            Unbound _ _ -> return ty
            Link ty' -> repr ty'
    repr ty = return ty