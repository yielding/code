import Text.Read

interactiveDoubling = do
  putStrLn "Choose a number:"
  s <- getLine
  let mx = readMaybe s :: Maybe Double
  case mx of
    Just x -> putStrLn ("The double of your number is " ++ show (2 *x))
    Nothing -> do 
      putStrLn "This is not a valid number. Retrying..."
      interactiveDoubling

