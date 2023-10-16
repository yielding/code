import Data.Maybe

zeroAsDefault :: Maybe Int -> Int
zeroAsDefault mx 
  = case mx of
      Nothing -> 0
      Just x  -> x

zeroAsDefaultStd :: Maybe Int -> Int
zeroAsDefaultStd mx = fromMaybe 0 mx

displayResult :: Maybe Int -> String
displayResult mx 
  = maybe "There was no result" (("The result was " ++) . show) mx

main :: IO ()
main = do
  print $ zeroAsDefault $ Just 10
  print $ zeroAsDefaultStd $ Just 10
  print $ displayResult $ Nothing
