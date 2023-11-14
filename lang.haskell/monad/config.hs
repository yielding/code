
data Config = Config { level :: Int, name :: String }

job :: Config -> String
job config = "level is " ++ l ++ n
  where l = levelJob config
        n = nameJob config

levelJob :: Config -> String
levelJob config = if l > 5 then "high" else "low"
  where l = level config

nameJob :: Config -> String
nameJob config = ", name is " ++ name config

main :: IO ()
main = do 
  print $ job $ Config 3 "abc"
  print $ job $ Config 6 "def"