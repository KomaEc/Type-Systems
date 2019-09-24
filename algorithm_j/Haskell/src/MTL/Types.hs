{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module MTL.Types where

    import Data.IORef
    import Syntax
    import Control.Monad.IO.Class

    type Level = Int

    data Type where
        TConst :: Name -> Type                  -- type constructor, such as "int", "bool"
        TApp :: Type -> Type -> Type            -- type level application
        TArrow :: Type -> Type -> Type
        TVar :: IORef TVar -> Type
        deriving Eq

    data TVar where
        Unbound :: Level -> String -> TVar
        Link :: Type -> TVar
        deriving Eq

    repr :: (MonadIO m) => Type -> m Type
    repr ty@(TVar ref) = do
        tvar <- liftIO $ readIORef ref
        case tvar of 
            Unbound _ _ -> return ty
            Link ty' -> repr ty'
    repr ty = return ty