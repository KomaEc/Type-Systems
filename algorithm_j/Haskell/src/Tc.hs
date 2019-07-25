{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Tc where

    import Syntax
    import Types
    import Control.Monad.State
    import Control.Monad.Reader
    import Control.Monad.Except
    import Control.Monad.ST
    import Data.STRef
    import Control.Lens hiding(Level)
    import Control.Lens.TH
    import Data.Map
    import Data.Function ((&))

    type Env a = Map Name a

    data TypeError s
        = OccurCheck (TVar s) (Type s)
        | PlayGround

    data TcState = TcState {
        _typeLevel :: Level
    ,   _newNames :: [Name]
    } deriving (Show)
    makeLenses ''TcState

    type Infer s = ReaderT (Env (Type s)) 
                   (StateT TcState
                   (ExceptT (TypeError s)
                   (ST s)))

    runInferST :: Env (Type s) -> Infer s a -> ST s (Either (TypeError s) a)
    runInferST r m = runReaderT m r -- `|>` in OCaml
                   & flip evalStateT tcState0
                   & runExceptT

    genericLevel :: Level
    genericLevel = 10000000000

    enterLevel :: Infer s ()
    enterLevel = lift $
                    modify (over typeLevel (+1))
        
    leaveLevel :: Infer s ()
    leaveLevel = lift $
                    modify (over typeLevel $ \x -> x - 1)

    initLevel :: Level
    initLevel = 1

    getNewName :: Infer s Name
    getNewName = lift $ do
        names <- use newNames
        modify $ over newNames tail
        return (head names)

    defaultSupply :: [Name]
    defaultSupply = [1..] >>= flip replicateM ['a'..'z']

    tcState0 :: TcState
    tcState0 = TcState initLevel defaultSupply


    unify :: Type s -> Type s -> Infer s ()
    unify ty1 ty2 = lift . lift . ExceptT $ do
                    ty1' <- repr ty1
                    ty2' <- repr ty2
                    unify' ty1' ty2'
        where
            unify' :: Type s -> Type s -> ST s (Either (TypeError s) ())
            unify' ty1@(TVar ref1) ty2@(TVar ref2) 
                | ref1 == ref2 = return $ Right ()
                | otherwise    = do
                    Unbound l1 x <- readSTRef ref1
                    Unbound l2 y <- readSTRef ref2
                    if l1 < l2 then -- unify to the lower level
                        Right <$> writeSTRef ref2 (Link ty1)
                    else
                        Right <$> writeSTRef ref1 (Link ty2)
            unify' ty (TVar ref) = do
                Unbound l ty' <- readSTRef ref
                Right <$> writeSTRef ref (Link ty)
            unify' ref@(TVar _) ty = unify' ty ref
            
    

