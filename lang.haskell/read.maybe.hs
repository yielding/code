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

interactiveDoubleing2 = do 
  putStrLn "Choose a number:"
  s <- getLine
  let mx = readMaybe s :: Maybe Double
  case fmap (2*) mx of
    Just d -> putStrLn("The double of your number is " ++ show d)
    Nothing -> do
      putStrLn "This is not a valid number. Retrying..."
      interactiveDoubleing2

--
--
--

interactiveSumming = do
  putStrLn "Choose two numbers: "
  sx <- getLine
  sy <- getLine
  let mx = readMaybe sx :: Maybe Double 
      my = readMaybe sy
  case mx of
      Just x ->  case my of
        Just y -> putStrLn ("The sum of your no. is " ++ show (x + y))
        Nothing -> retry 
      Nothing -> retry
  where
    retry = do
      putStrLn "Invalid number. Retrying.."
      interactiveSumming

interactiveSumming2 = do
  putStrLn "Choose two numbers: "
  sx <- getLine
  sy <- getLine
  let mx = readMaybe sx :: Maybe Double 
      my = readMaybe sy
  case (+) <$> mx <*> my of
      Just z  -> putStrLn ("The sum of your no. is " ++ show z)
      Nothing -> do
        putStrLn "Invalid number. Retrying.."
        interactiveSumming2

interactiveSumming3 = do
  putStrLn "Choose two numbers:"
  mx <- readMaybe <$> getLine
  my <- readMaybe <$> getLine
  case (+) <$> mx <*> my of
    Just z -> putStrLn("The sum of your no. is " ++ show z)
    Nothing -> do
      putStrLn "Invalid number. Retrying ..."
      interactiveSumming3

interactiveContetenating :: IO ()
interactiveContetenating = do
  putStrLn "Choose two strings:"
  sx <- getLine
  sy <- getLine
  putStrLn "Let's concatenate them:"
  putStrLn (sx ++ sy)

interactiveContetenating2 :: IO ()
interactiveContetenating2 = do
  putStrLn "Choose two strings:"
  sz <- (++) <$> getLine <*> (take 3 <$> getLine)
  putStrLn "Let's concatenate them:"
  putStrLn sz

main :: IO ()
main = do
  interactiveSumming2
  -- case readMaybe "3" :: Maybe Integer of
  --   Just x -> print x
  --   _      -> print "error"


