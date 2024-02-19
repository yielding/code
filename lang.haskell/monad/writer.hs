-- NOTICE: confer from
-- https://www.slideshare.net/namhyeonuk90/haskell-study-13
--
import Control.Monad.Writer

-- newtype Writer extra a = Writer { runWriter :: (a, extra) }
-- 
-- instance Monoid extra => Monoid (Writer extra) where
--   return a = Writer (a, mempty)
--   Writer (a, e) >>= f =
--     let (b, e') = runWriter (f a) in Writer (b, e `mappend` e')
-- 

logNumber :: Int -> Writer [String] Int
logNumber x = writer(x, ["Got Number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
  a <- logNumber 3
  b <- logNumber 5
  return (a * b)

-- tell :: a -> Writer a ()
-- tell e = writer((), e)

gcdWithLog :: Int -> Int -> Writer [String] Int
gcdWithLog a b
  | b == 0 = do
    tell ["Finished with " ++ show a]
    return a
  | otherwise = do
    tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
    gcdWithLog b (a `mod` b)


main :: IO ()
main = do
  print $ runWriter multWithLog
  print $ runWriter $ gcdWithLog 10 20
  print $ fst $ runWriter $ gcdWithLog 10 20
  print $ snd $ runWriter $ gcdWithLog 10 20