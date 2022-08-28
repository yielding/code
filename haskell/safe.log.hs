safeLog :: (Floating a, Ord a) => a -> Maybe a
safeLog x
  | x >= 0    = Just (log x)
  | otherwise = Nothing

