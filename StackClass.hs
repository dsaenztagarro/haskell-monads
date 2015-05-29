{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module StackClass where

class (Monad m) => MonadStack s m | m -> s where
    next :: m (Maybe s)

showTwo :: (Show s, Monad m, MonadStack s m) => m String
showTwo = do
    a <- next
    b <- next
    return $ "a: " ++ show a ++ " b: " ++ show b
