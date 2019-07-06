{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Supply.Class
  ( MonadSupply(..)
  )where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.RWS

class Monad m => MonadSupply s m | m -> s where
    supply :: m s
    isExhausted :: m Bool

instance MonadSupply s m => MonadSupply s (ExceptT e m) where
    supply = lift supply
    isExhausted = lift isExhausted

instance MonadSupply s m => MonadSupply s (ReaderT r m) where
    supply = lift supply
    isExhausted = lift isExhausted

instance (Monoid w, MonadSupply s m) => MonadSupply s (RWST r w st m) where
    supply = lift supply
    isExhausted = lift isExhausted

instance MonadSupply s m => MonadSupply s (StateT st m) where
    supply = lift supply
    isExhausted = lift isExhausted

instance (Monoid w, MonadSupply s m) => MonadSupply s (WriterT w m) where
    supply = lift supply
    isExhausted = lift isExhausted
