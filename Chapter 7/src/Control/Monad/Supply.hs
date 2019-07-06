{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Supply
    ( module Control.Monad.Supply.Class
    , SupplyT
    , runSupplyT
    , runSupply
    , defaultStringSupply
    , defaultNameSupply
    , defaultNumSupply
    ) where

import Control.Monad.State
import Control.Monad.Supply.Class
import Data.Functor.Identity

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer

newtype SupplyT s m a = SupplyT (StateT [s] m a)
  deriving ( Functor, Applicative, Monad
           , MonadTrans, MonadIO, MonadFix)

runSupplyT :: Monad m => SupplyT s m a -> [s] -> m a
runSupplyT (SupplyT m) init = evalStateT m init

type Supply s = SupplyT s Identity

runSupply :: Supply s a -> [s] -> a
runSupply s = runIdentity . runSupplyT s

getSupply :: Monad m => SupplyT s m [s]
getSupply = SupplyT get

withRemaining :: Monad m => SupplyT s m a -> SupplyT s m (a, [s])
withRemaining m = do
    a <- m
    s <- getSupply
    return $ (a, s)

instance Monad m => MonadSupply s (SupplyT s m) where
    supply = supplyST
    isExhausted = isExhaustedST

supplyST :: Monad m => SupplyT s m s
supplyST = SupplyT $ state $ \s -> (head s, tail s)

isExhaustedST :: Monad m => SupplyT s m Bool
isExhaustedST = SupplyT $ gets null

defaultStringSupply :: [String]
defaultStringSupply = let legalChars = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']
                      in [1..] >>= flip replicateM legalChars

defaultNameSupply :: [String]
defaultNameSupply = [1..] >>= flip replicateM ['a'..'z']

defaultNumSupply :: Num a => [a]
defaultNumSupply = map fromIntegral [1..]

instance MonadError e m => MonadError e (SupplyT s m) where
    throwError e = SupplyT $ StateT $ \s -> do
        a <- throwError e
        return (a, s)
    -- Note: the state will be reverted to before action 'm' was run
    -- before running 'f' 'e'
    catchError m f = SupplyT $ StateT $ \s ->
        runSupplyT (withRemaining m) s `catchError` (\e -> let SupplyT sm = f e
                                                           in  runStateT sm s)
