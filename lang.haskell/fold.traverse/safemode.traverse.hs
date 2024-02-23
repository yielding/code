safeMode :: Integral a => a -> a -> Maybe a
safeMode _ 0 = Nothing
safeMode numerator divisor = Just $ mod numerator divisor

main = do 
  print $ map (safeMode 3) [1, 2, 0, 3]      -- [Just 0, Just 1, Nothing, Just 1]
  print $ traverse (safeMode 3) [1, 2, 0, 3] -- Nothing
  print $ traverse (safeMode 3) [1, 2, 3]    -- Just [0, 1, 0]
  print $ sequenceA [Just 0, Just 1, Nothing] -- Nothing
  print $ sequenceA [Just 0, Just 1, Just 2]  -- Just [0, 1, 2]