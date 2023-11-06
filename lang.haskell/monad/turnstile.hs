-- 좋은 예제, 하지만 아직 state의 본질을 내가 아직 이해하지 못하고 있다. (2023 / 11 / 6)
--
import Control.Monad.State

data TurnstileState = Locked | Unlocked
  deriving (Eq, Show)

data TurnstileOutput = Thank | Open | Tut
  deriving (Eq, Show)

coin, push :: TurnstileState -> (TurnstileOutput, TurnstileState)

coin _ = (Thank, Unlocked)

push Locked   = (Tut , Locked)
push Unlocked = (Open, Locked)

monday :: TurnstileState -> ([TurnstileOutput], TurnstileState)
monday s0 =
  let (a1, s1) = coin s0
      (a2, s2) = push s1
      (a3, s3) = push s2
      (a4, s4) = coin s3
      (a5, s5) = push s4
  in ([a1, a2, a3, a4, a5], s5)

-------------------------------------------------
coinS, pushS :: State TurnstileState TurnstileOutput
coinS = state coin  -- why state instead of State ? 이해는 안감.
pushS = state push

mondayS :: State TurnstileState [TurnstileOutput]
mondayS = do
  a1 <- coinS
  a2 <- pushS
  a3 <- pushS
  a4 <- coinS
  a5 <- pushS
  return [a1, a2, a3, a4, a5]

-- mondayS2 :: State TurnstileState [TurnstileOutput]
-- mondayS2 = sequence [ coinS, pushS, pushS, coinS, pushS ]

testTurnstile :: State TurnstileState Bool
testTurnstile = do
  put Locked
  check1 <- pushS
  put Unlocked 
  check2 <- pushS
  put Locked
  return (check1 == Tut && check2 == Open)

main :: IO ()
main = do 
  print "hi"
  print $ runState mondayS Locked
  -- print $ runState mondayS2 Locked
  print $ evalState mondayS Locked
  print $ execState mondayS Locked
  print $ runState testTurnstile Locked
  print $ runState testTurnstile Unlocked
