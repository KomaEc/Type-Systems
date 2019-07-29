{-# LANGUAGE TemplateHaskell #-}

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
        | UnificationError (Type s) (Type s)
        | UnboundVariable Name
        | PlayGround
        deriving Show

    data TcState = TcState {
        _typeLevel :: Level
    ,   _newNames :: [Name]
    } deriving (Show)
    makeLenses ''TcState

    type Infer s = ReaderT (Env (Type s)) 
                   (StateT TcState
                   (ExceptT (TypeError s)
                   (ST s)))

    runInferST :: Infer s a -> ST s (Either (TypeError s) a)
    runInferST m = runReaderT m empty -- `|>` in OCaml
                   & flip evalStateT tcState0
                   & runExceptT

{-
    runInfer :: Infer s a -> Either String a
    runInfer m = runReaderT m empty
               & flip evalStateT tcState0
               & runExceptT
               & fmap (either (Left . show) Right)
               & runST
-}

    genericLevel :: Level
    genericLevel = 10000000000

    enterLevel :: Infer s ()
    enterLevel = lift $
                    modify (over typeLevel (+1))
        
    leaveLevel :: Infer s ()
    leaveLevel = lift $
                    modify (over typeLevel $ \x -> x - 1)

    currentLevel :: Infer s Level
    currentLevel = lift $
                    use typeLevel

    initLevel :: Level
    initLevel = 1

    getNewName :: Infer s Name
    getNewName = lift $ do
        names <- use newNames
        modify $ over newNames tail
        return (head names)

    newTypeVar :: Infer s (Type s)
    newTypeVar = do
        l <- currentLevel
        x <- getNewName
        lift . lift . lift . fmap TVar . newSTRef $ Unbound l x

    defaultSupply :: [Name]
    defaultSupply = [1..] >>= flip replicateM ['a'..'z']

    tcState0 :: TcState
    tcState0 = TcState initLevel defaultSupply

    unify :: Type s -> Type s -> Infer s ()
    unify ty1 ty2 = lift . lift $ unify' ty1 ty2
        where
            unify', unify'' :: Type s -> Type s -> ExceptT (TypeError s) (ST s) ()
            unify' ty1 ty2 = do
                ty1' <- lift $ repr ty1
                ty2' <- lift $ repr ty2
                unify'' ty1' ty2'

            unify'' ty1@(TVar ref1) ty2@(TVar ref2)
                | ref1 == ref2 = return ()
                | otherwise    = lift $ do
                    Unbound l1 x <- readSTRef ref1
                    Unbound l2 y <- readSTRef ref2
                    if l1 < l2 then
                        writeSTRef ref2 (Link ty1)
                    else
                        writeSTRef ref1 (Link ty2)
            unify'' ty1 ty2@(TVar ref) = do -- note, here ref must be Unbound
                occurs ref ty1
                lift $ writeSTRef ref (Link ty1)
            unify'' ty1@(TVar _) ty2 = unify'' ty2 ty1
            unify'' (TArrow tyl1 tyl2) (TArrow tyr1 tyr2) = do
                unify' tyl1 tyr1 -- handle recursive cases
                unify' tyl2 tyr2
            unify'' ty1 ty2 = throwError $ UnificationError ty1 ty2

            occurs, occurs' :: STRef s (TVar s) -> Type s -> ExceptT (TypeError s) (ST s) () -- invariant :: ref must be Unbound
            occurs ref ty = do
                ty' <- lift $ repr ty
                occurs' ref ty'

            occurs' ref ty@(TVar ref')
                | ref == ref' = do
                    tv <- lift $ readSTRef ref
                    throwError $ OccurCheck tv ty
                | otherwise   = lift $ do
                    Unbound l1 _ <- readSTRef ref
                    Unbound l2 y <- readSTRef ref'
                    writeSTRef ref' $ Unbound (l1 `min` l2) y -- adjust level!
            occurs' ref (TArrow ty1 ty2) = do
                occurs ref ty1 -- apply the same trick
                occurs ref ty2


    generalize :: Type s -> Infer s (Type s)
    generalize ty@(TVar ref) = do
        tv <- lift . lift . lift $ readSTRef ref
        l <- currentLevel
        case tv of
            Unbound l' x 
                | l < l'    -> do 
                    lift . lift . lift . writeSTRef ref $ Unbound genericLevel x
                    return ty
                | otherwise -> return ty
            Link ty'        -> generalize ty'
    generalize (TArrow ty1 ty2) = do
        ty1' <- generalize ty1
        ty2' <- generalize ty2
        return $ TArrow ty1' ty2'
    generalize ty = return ty

    typeOf :: Expr -> Infer s (Type s)
    typeOf (Var x) = do
        r <- ask
        case Data.Map.lookup x r of
            Just ty -> instantiate ty
            Nothing -> lift . lift . throwError $ UnboundVariable x
    typeOf (Lam x expr) = do
        tyVar <- newTypeVar
        local (insert x tyVar) $ typeOf expr
    typeOf (App expr1 expr2) = do
        ty1 <- typeOf expr1
        ty2 <- typeOf expr2
        tyVar <- newTypeVar
        unify ty1 $ ty2 `TArrow` tyVar
        return tyVar
    typeOf (Let x expr1 expr2) = do
        enterLevel
        ty <- typeOf expr1
        leaveLevel
        ty' <- generalize ty
        local (insert x ty') $ typeOf expr2

    instantiate :: Type s -> Infer s (Type s)
    instantiate = undefined
