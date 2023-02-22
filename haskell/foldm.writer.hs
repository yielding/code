import Control.Monad
import Control.Monad.Writer
import Data.Maybe 

binSmalls :: Int -> Int -> Maybe Int
binSmalls acc x
  | x > 9 = Nothing
  | otherwise = Just (acc + x)

binLog :: Int -> Int -> Writer [String] Int
binLog acc x = writer (acc + x, ["add " ++ show x ++ " to " ++ show acc])

main = do
  let r0 = foldM binSmalls 0 [2, 8, 3]
  putStrLn ("res r0 : " ++ show(r0))

  let r1 = foldM binLog 0 [2, 8, 3]
  putStrLn ("res r1 : " ++ show(r1))

  let r2 = runWriter (foldM binLog 0 [2, 8, 3])
  putStrLn ("res r2 : " ++ show(r2))

  let r3 = runWriter $ foldM binLog 0 [2, 8, 3]
  putStrLn ("res r3 : " ++ show(r3))