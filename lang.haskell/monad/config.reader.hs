import Control.Monad.Reader

data Config 
  = Config { level :: Int
           , name  :: String 
           }

job :: Config -> String
job = do
  l <- levelJob
  n <- nameJob
  return $ "level is " ++ l ++ n

levelJob :: Config -> String
levelJob = do
  l <- level
  return $ if l > 5
           then "high"
           else "low"

nameJob :: Config -> String
nameJob = do
  n <- name
  return $ ", name is " ++ n

main :: IO ()
main = do 
  print $ job $ Config 3 "abc"
  print $ job $ Config 6 "def"