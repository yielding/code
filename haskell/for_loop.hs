import Control.Monad

main :: IO ()
main = forM_ [1..10] $ \i -> do
  print i
