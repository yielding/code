import Data.Monoid
import Control.Monad.Writer.Lazy

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got no: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
  a <- logNumber 3
  b <- logNumber 5
  return (a * b)

main :: IO ()
main = do 
 -- print $ runWriter (Just (Sum 3) `mappend` (Sum 4))
 print $ runWriter (return 3 :: Writer String Int)
 print $ runWriter (return 3 :: Writer (Sum Int) Int)
 print $ runWriter (return 3 :: Writer (Product Int) Int)
 print $ runWriter multWithLog