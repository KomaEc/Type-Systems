{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils where


class ShowIO a where
    showIO :: a -> IO String