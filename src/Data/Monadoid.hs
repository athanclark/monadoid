{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , DeriveFunctor
  , DeriveTraversable
  , DeriveGeneric
  , DeriveDataTypeable
  #-}

{-|

Module      : Data.Monadoid
Copyright   : (c) 2017 Athan Clark
License     : BSD-3
Maintainer  : athan.clark@gmail.com
Stability   : experimental
Portability : GHC

 -}

module Data.Monadoid where

import GHC.Generics (Generic)
import Data.Data (Data, Typeable)


newtype Monadoid m a = Monadoid {runMonadoid :: m a}
  deriving (Show, Eq, Ord, Functor, Applicative, Monad, Foldable, Traversable, Generic, Data, Typeable)


instance (Monad m, Monoid a) => Monoid (Monadoid m a) where
  mappend x y = do
    x' <- x
    y' <- y
    pure (x' `mappend` y')
  mempty = pure mempty
