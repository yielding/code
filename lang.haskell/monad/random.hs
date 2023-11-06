import Control.Monad
import Control.Monad.Trans.State
import System.Random

type GeneratorState = State StdGen

rollDie :: GeneratorState Int
rollDie = do generator <- get
             let (value, newGenerator) = randomR(1, 6) generator
             put newGenerator
             return value

rollDiceIO :: IO (Int, Int)
rollDiceIO = liftM2 (,) (randomRIO (1, 6)) (randomRIO(1,6))

clumsyRollDice :: (Int, Int)
clumsyRollDice = (n, m)
  where
    (n, g) = randomR (1, 6) (mkStdGen 0)
    (m, _) = randomR (1, 6) g

main :: IO ()
main = do
  let generator = mkStdGen 0
  let (randInt, generator2) = random generator :: (Int, StdGen)
  print clumsyRollDice
  print $ evalState rollDie (mkStdGen 0)