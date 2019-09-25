{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}


module Infer where


    import Syntax
    import Types
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
                 IO))

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

    -- genericLevel :: Monad m => m Level
    -- genericLevel = return 10000000000

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
        fmap TVar . newRef $ Unbound x l

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
    unify (TVar ref1) (TVar ref2) | ref1 == ref2 = return ()
    unify (TVar ref) ty2 = do
        tv <- readRef ref
        case tv of 
            Unbound x l -> do
                occursCheck ref ty2
                writeRef ref (Link ty2)
            Link ty1    -> unify ty1 ty2
    unify ty1 ty2@(TVar _) = unify ty2 ty1
    unify (TArrow tyl1 tyl2) (TArrow tyr1 tyr2) = do
        unify tyl1 tyr1
        unify tyl2 tyr2

    occursCheck :: (MonadError TypeError m, MonadIO m) => IORef TVar -> Type -> m ()
    occursCheck ref ty@(TVar ref') 
            | ref == ref' = do
                tv <- readRef ref
                throwError (OccurCheck tv ty)
            | otherwise   = do
                Unbound x l <- readRef ref
                tv <- readRef ref'
                case tv of
                    Unbound y l' -> writeRef ref' (Unbound y (l `min` l'))
                    Link ty      -> occursCheck ref ty
    occursCheck ref (TArrow ty1 ty2) = do
        occursCheck ref ty1
        occursCheck ref ty2
    occursCheck ref (QVar _) = return () -- ??

    generalize :: (MonadState TcState m, MonadIO m) => Type -> m Type
    generalize ty@(TVar ref) = do
        tv <- readRef ref
        case tv of
            Unbound x l -> do
                cl <- currentLevel
                if l > cl then
                    return (QVar x)
                else
                    return ty
            Link ty' -> generalize ty'
    generalize (TArrow ty1 ty2) = do
        ty1' <- generalize ty1
        ty2' <- generalize ty2
        return $ TArrow ty1' ty2'
    generalize ty@(QVar _) = return ty

    typeOf :: MonadInfer m => Expr -> m Type
    typeOf (Var x) = do
        env <- ask
        case Data.Map.lookup x env of
            Just ty -> instantiate ty
            Nothing -> throwError (UnboundVariable x)
    typeOf (App t1 t2) = do
        ty1 <- typeOf t1
        ty2 <- typeOf t2
        tyRes <- newTypeVar
        unify ty1 (TArrow ty2 tyRes)
        return tyRes
    typeOf (Lam x t) = do
        alpha <- newTypeVar
        ty <- local (insert x alpha) $ typeOf t
        return $ TArrow alpha ty
    typeOf (Let x t t') = do
        enterLevel
        ty <- typeOf t
        leaveLevel
        ty' <- generalize ty
        local (insert x ty') $ typeOf t'

    instantiate :: (MonadState TcState m, MonadIO m) => Type -> m Type
    instantiate ty = fst <$> loop [] ty
        where
            loop subst (QVar x) = case Prelude.lookup x subst of
                                    Just ty -> return (ty, subst)
                                    Nothing -> do
                                        ty <- newTypeVar
                                        return (ty, (x, ty):subst)
            loop subst ty@(TVar ref) = do
                tv <- readRef ref
                case tv of
                    Unbound _ _ -> return (ty, subst)
                    Link ty'    -> loop subst ty'
            loop subst (TArrow ty1 ty2) = do
                (ty1', subst) <- loop subst ty1
                (ty2', subst) <- loop subst ty2
                return (TArrow ty1' ty2', subst)
