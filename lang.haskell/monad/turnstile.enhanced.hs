-- 좋은 예제, 하지만 아직 state의 본질을 내가 아직 이해하지 못하고 있다. (2023 / 11 / 6)
--
import Control.Monad              -- for filterM
import Control.Monad.State

data TurnstileState = Locked | Unlocked
  deriving (Eq, Show)

data TurnstileInput = Coin | Push
  deriving (Eq, Show)

data TurnstileOutput = Thank | Open | Tut
  deriving (Eq, Show)

turnS :: TurnstileInput -> State TurnstileState TurnstileOutput
turnS = state . turn where
  turn Coin _        = (Thank, Unlocked)
  turn Push Unlocked = (Open,  Locked)
  turn Push Locked   = (Tut,   Locked)

getsThroughS :: TurnstileInput -> State TurnstileState Bool
getsThroughS input = do
  output <- turnS input
  return $ output == Open

countOpens :: [TurnstileInput] -> State TurnstileState Int
countOpens = foldM incIfOpens 0 where
  incIfOpens :: Int -> TurnstileInput -> State TurnstileState Int
  incIfOpens n i = do
    g <- getsThroughS i
    if g then return (n+1) else return n

main :: IO ()
main = do 
  print $ runState (turnS Coin) Locked
  print $ evalState (mapM turnS [Coin, Push, Push, Coin, Push]) Locked
  print $ evalState (filterM getsThroughS [Push, Coin, Coin, Push, Push, Coin, Push]) Locked
  print $ evalState (countOpens [Coin, Push, Coin, Push, Push, Coin, Push]) Locked
