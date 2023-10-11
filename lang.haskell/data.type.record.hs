-- 기명 필드 (record 구문)
--
import System.Directory

data Configuration
  = Configuration { username      :: String 
                  , localhost     :: String
                  , remotehost    :: String
                  , isguest       :: String
                  , issuperuser   :: String
                  , currentdir    :: String
                  , homedir       :: String
                  , timeconnected :: Integer
                  }
  deriving (Show, Eq)

changeDir :: Configuration -> String -> IO()
changeDir cfg newDir 
  = do ok <- doesDirectoryExist newDir
       if ok
          then print "ok"
          else print "fail"

postWorkingDir :: Configuration -> String
postWorkingDir cfg = currentdir cfg        -- getter
 
-- cfgFoo = Configuration { username = "Foo" }
-- cfgBar = Configuration { username = "Bar", remotehost = "Baz" }
-- cfgNon = Configuration { }

data Color
  = Color { redC     :: Int
          , greenC   :: Int
          , blueC    :: Int
          , opacityC :: Int
          }
  deriving (Show)

red :: Color
red = Color {redC = 255, opacityC = 255, blueC = 0, greenC = 0}

greenComponent :: Color -> Int
greenComponent Color {greenC = green} = green

main = do 
  -- print $ username cfgFoo
  print $ red
  print $ greenComponent red
