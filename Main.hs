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

-- Random number generation

data CountRandom = CountRandom {
      crGen :: StdGen
    , crCount :: Int
    } deriving (Show)

type RandomState = State CountRandom

getRandom :: Random a => RandomState a
getRandom = do
    st <- getSt
    let (val, gen') = random $ crGen st
    putSt CountRandom { crGen = gen', crCount = crCount st + 1 }
    return val

threeRandoms :: RandomState (Int, Int, Int)
threeRandoms = do
    a <- getRandom
    b <- getRandom
    c <- getRandom
    return (a, b, c)

-- Reader Monad

newtype Reader e a = R { runReader :: e -> a }

returnR :: a -> Reader e a
returnR a = R $ \_ -> a

bindR :: Reader e a -> (a -> Reader e b) -> Reader e b
bindR m k = R $ \e -> (runReader $ k (runReader m $ e)) e

ask :: Reader e e
ask = R id

instance Monad (Reader e) where
    return = returnR
    (>>=) = bindR

instance Functor (Reader s) where
    fmap = liftM

instance Applicative (Reader s) where
    pure = returnR
    (<*>) = ap
