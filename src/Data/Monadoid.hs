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

import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Writer.Class (MonadWriter)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Cont.Class (MonadCont)
import Control.Monad.RWS.Class (MonadRWS)
import Control.Monad.IO.Class (MonadIO)



newtype Monadoid m a = Monadoid {runMonadoid :: m a}
  deriving ( Show, Eq, Ord, Functor, Applicative, Monad, Foldable, Traversable, Generic, Data, Typeable
           , MonadReader r, MonadWriter w, MonadState s, MonadRWS r w s, MonadError e, MonadCont
           )


instance MonadTrans Monadoid where
  lift = Monadoid


-- | The only important instance
instance (Monad m, Monoid a) => Monoid (Monadoid m a) where
  mappend x y = do
    x' <- x
    y' <- y
    pure (x' `mappend` y')
  mempty = pure mempty
