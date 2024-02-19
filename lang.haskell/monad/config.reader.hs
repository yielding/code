-- NOTICE : confer from
-- https://www.slideshare.net/namhyeonuk90/haskell-study-13
--
import Control.Monad.Reader

data Config = Config { level :: Int , name  :: String }

job :: Config -> String
job = do                           -- no config param
  l <- levelJob
  n <- nameJob
  return $ "level is " ++ l ++ n

levelJob :: Config -> String
levelJob = do                      -- no config param
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