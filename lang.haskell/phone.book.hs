-
-- 언어 공부에 있어서 절대적인 시간이 투입되어야 코드가 눈에 익는다.
--
phonebook :: [(String, String)]
phonebook = [ ("Bob",   "01788 665242"), 
              ("Fred",  "01624 556442"), 
              ("Alice", "01889 985333") ]

printNo :: String -> IO()
printNo name = msg $ lookup name phonebook
  where
    msg (Just number) = print number
    msg (Nothing)     = print $ name ++ " not found in db"

main :: IO()
main = do
  print $ lookup "Fred" phonebook
  printNo "Bob"          -- "01788 665242"
  printNo "yielding"     -- yielding not found in db
