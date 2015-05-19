module Main where

import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)

-- Monad State implementation

newtype State s a = State { runState :: s -> (a, s) }

returnSt :: a -> State s a
returnSt a = State $ \s -> (a, s)

evalState :: State s a -> (s -> a)
evalState m = \s -> fst $ runState m s

execState :: State s a -> (s -> s)
execState m = \s -> snd $ runState m s

bindSt :: State s a -> (a -> State s b) -> State s b
bindSt m k = State $ \s -> let (a, s') = runState m s
                           in runState (k a) s'

getSt :: State s s
getSt = State $ \s -> (s, s)

putSt :: s -> State s ()
putSt s = State $ \_ -> ((), s)

instance Monad (State s) where
    return = returnSt
    (>>=) = bindSt

instance Functor (State s) where
    fmap = liftM

instance Applicative (State s) where
    pure = returnSt
    (<*>) = ap

-- GameState Monad

data Action = StandardAction
            | MoveAction
            deriving (Show)

data Game = Game
    { players :: [String] }
    deriving (Show)

type GameState = State Game

playGame :: GameState Action
playGame = do
    return StandardAction

loadGame :: Game
loadGame = Game { players = ["David", "Javier", "Mario"] }

main :: IO ()
main = do
    let (action, gameState) = runState playGame $ loadGame
    putStrLn $ "Action: " ++ show action
    putStrLn $ "GameState: " ++ show gameState

-- Example

type Stack = [Int]

checkStack :: State Stack ()
checkStack = do
    stack <- getSt
    if length stack > 3
    then return()
    else putSt $ [2]