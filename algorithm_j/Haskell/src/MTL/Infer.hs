{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module MTL.Infer where

    import Syntax
    import MTL.Types
    import Control.Monad.State
    import Control.Monad.Reader
    import Control.Monad.Except
    import Control.Monad
    import Data.IORef
    import Control.Lens hiding (Level)
    import Control.Lens.TH
    import Data.Map
    import Data.Function ((&))

    type Env a = Map Name a

    data TypeError =
        OccurCheck TVar Type
      | UnificationError Type Type
      | UnboundVariable Name
      | PlayGround

    data TcState = TcState {
        _typeLevel :: Level
    ,   _newNames :: [Name]
    } deriving (Show)
    makeLenses ''TcState

    type Infer = ReaderT (Env Type) (
                 StateT TcState (
                 ExceptT TypeError
                 IO
                 )
            )

    runInfer :: Infer a -> IO (Either TypeError a)
    runInfer m = runReaderT m empty
               & flip evalStateT tcState0
               & runExceptT

    type MonadInfer m = (
        MonadError TypeError m
      , MonadIO m
      , MonadReader (Env Type) m
      , MonadState TcState m
      )

    genericLevel :: Monad m => m Level
    genericLevel = return 10000000000

    enterLevel :: MonadState TcState m => m ()
    enterLevel = modify (over typeLevel (+1))

    leaveLevel :: MonadState TcState m => m ()
    leaveLevel = modify (over typeLevel $ \x -> x - 1)

    currentLevel :: MonadState TcState m => m Level
    currentLevel = use typeLevel

    initLevel :: Monad m => m Level
    initLevel = return 1

    getNewName :: MonadState TcState m => m Name
    getNewName = do
        names <- use newNames
        modify $ over newNames tail
        return (head names)

    newTypeVar :: (MonadIO m, MonadState TcState m) => m Type
    newTypeVar = do
        l <- currentLevel
        x <- getNewName
        fmap TVar . newRef $ Unbound l x

    defaultSupply :: [Name]
    defaultSupply = [1..] >>= flip replicateM ['a'..'z']

    tcState0 :: TcState
    tcState0 = TcState (runIdentity initLevel) defaultSupply

    readRef :: MonadIO m => IORef a -> m a
    readRef = liftIO . readIORef

    writeRef :: MonadIO m => IORef a -> a -> m ()
    writeRef ref = liftIO . writeIORef ref

    newRef :: MonadIO m => a -> m (IORef a)
    newRef = liftIO . newIORef

    unify :: (MonadError TypeError m, MonadIO m) => Type -> Type -> m ()
    unify ty1 ty2 = do
        ty1' <- repr ty1
        ty2' <- repr ty2
        unify' ty1' ty2' 
        where
            unify' :: (MonadError TypeError m, MonadIO m) => Type -> Type -> m ()
            unify' ty1@(TVar ref1) ty2@(TVar ref2)
                | ref1 == ref2 = return ()
                | otherwise    = do
                    Unbound l1 x <- readRef ref1
                    Unbound l2 y <- readRef ref2
                    if l1 < l2 then
                        writeRef ref2 (Link ty1)
                    else
                        writeRef ref1 (Link ty2)
            unify' ty1 ty2@(TVar ref) = do
                occurs ref ty1
                writeRef ref (Link ty1)
            unify' ty1@(TVar _) ty2 = unify' ty2 ty1
            unify' (TArrow tyl1 tyl2) (TArrow tyr1 tyr2) = do
                unify tyl1 tyr1
                unify tyl2 tyr2
            unify' ty1 ty2 = throwError $ UnificationError ty1 ty2

            occurs, occurs' :: (MonadError TypeError m, MonadIO m) => IORef TVar -> Type -> m ()
            occurs ref ty = do
                ty' <- repr ty
                occurs' ref ty'

            occurs' ref ty@(TVar ref') 
                | ref == ref' = do
                    tv <- readRef ref
                    throwError $ OccurCheck tv ty
                | otherwise   = do
                    Unbound l1 _ <- readRef ref
                    Unbound l2 y <- readRef ref
                    writeRef ref' $ Unbound (l1 `min` l2) y
            occurs' ref (TArrow ty1 ty2) = do
                occurs ref ty1
                occurs ref ty2

    unify' :: (MonadError TypeError m, MonadIO m) => Type -> Type -> m ()
    unify' tv1@(TVar ref1) tv2@(TVar ref2) | ref1 == ref2 = return ()
    unify' (TVar ref1) ty2 = do
        tv <- readRef ref1
        case tv of
            Unbound l x -> do 
                occursCheck ref1 ty2
                writeRef ref1 (Link ty2)
            Link ty1    -> unify' ty1 ty2
    unify' ty1 ty2@(TVar _) = unify' ty2 ty1
    unify' (TArrow tyl1 tyl2) (TArrow tyr1 tyr2) = do
        unify' tyl1 tyr1
        unify' tyl2 tyr2

        where
            occursCheck :: IORef TVar -> Type -> m ()
            occursCheck ref ty@(TVar ref') 
                | ref == ref' = do
                    tv <- readRef ref
                    throwError (OccurCheck tv ty)
                | otherwise   = do
                    Unbound l x <- readRef ref
                    tv' <- readRef ref'
                    case tv' of
                        Unbound l' y -> writeRef ref' (Unbound (l `min` l') y)
                        Link ty      -> occursCheck ref ty
            occursCheck ref (TArrow ty1 ty2) = do
                occursCheck ref ty1
                occursCheck ref ty2





    generalize :: (MonadIO m, MonadState TcState m) => Type -> m Type
    generalize ty@(TVar ref) = do
        tv <- readRef ref
        l <- currentLevel
        case tv of
            Unbound l' x
                | l < l'    -> do
                    gl <- genericLevel
                    writeRef ref $ Unbound gl x
                    return ty
                | otherwise -> return ty
            Link ty'        -> generalize ty'
    generalize (TArrow ty1 ty2) = do
        ty1' <- generalize ty1
        ty2' <- generalize ty2
        return $ TArrow ty1' ty2'

    typeOf :: MonadInfer m => Expr -> m Type
    typeOf (Var x) = do
        r <- ask
        case Data.Map.lookup x r of
            Just ty -> instantiate ty
            Nothing -> throwError $ UnboundVariable x
    typeOf (Lam x expr) = do
        alpha <- newTypeVar
        local (insert x alpha) $ typeOf expr
    typeOf (App expr1 expr2) = do
        ty1 <- typeOf expr1
        ty2 <- typeOf expr2
        alpha <- newTypeVar
        unify ty1 $ ty2 `TArrow` alpha
        return alpha
    typeOf (Let x expr1 expr2) = do
        enterLevel
        ty <- typeOf expr1
        leaveLevel
        ty' <- generalize ty
        local (insert x ty') $ typeOf expr2

    instantiate :: (MonadError TypeError m, MonadIO m) => Type -> m Type
    instantiate = undefined

    typeOf' :: Expr -> Infer Type
    typeOf' = typeOf