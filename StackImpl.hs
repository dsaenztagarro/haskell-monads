{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module StackImpl where

import Control.Applicative (Applicative(..))
import Control.Monad.State
import StackClass

newtype FIFO s a = FIFO (State [s] a) deriving (Monad, Functor, Applicative)

nextFIFO :: FIFO s (Maybe s)
nextFIFO = FIFO $ do
    stack <- get
    case stack of
         [] -> return Nothing
         (x:xs) -> do put xs
                      return (Just x)

runFIFO :: FIFO t a -> [t] -> (a, [t])
runFIFO (FIFO s) xs = runState s xs

instance MonadStack s (FIFO s) where
    next = nextFIFO


newtype LIFO s a = LIFO (State [s] a) deriving (Monad, Functor, Applicative)

nextLIFO :: LIFO s (Maybe s)
nextLIFO = LIFO $ do
    stack <- get
    case stack of
         [] -> return Nothing
         xs -> do put stack'
                  return (Just element)
               where stack' = reverse $ tail $ reverse xs
                     element = last xs

runLIFO :: LIFO t a -> [t] -> (a, [t])
runLIFO (LIFO s) xs = runState s xs

instance MonadStack s (LIFO s) where
    next = nextLIFO