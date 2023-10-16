import Control.Applicative

main :: IO ()
main = do
  -- When you use famp on a function, you're just doing function composition
  let foo = fmap (+3) (+2)
  print $ foo 10

  print $ Just (+3) <*> Just 2

  print $ [(*2), (+3)] <*> [1..3]

  print $ Just (*5) <*> (Just 3)
  print $ (*) <$> Just 5 <*> Just 3
  print $ liftA2 (*) (Just 5) (Just 3)