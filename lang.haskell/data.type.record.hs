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


changeDir :: Configuration -> String -> Configuration
changeDir cfg newDir =
  if doesDirNameExist newDir
     then cfg{currentdir = newDir}         -- setter
     else error "dir does not exist"

postWorkingDir :: Configuration -> String
postWorkingDir cfg = currentdir cfg        -- getter

cfgFoo = Configuration { username = "Foo" }
cfgBar = Configuration { username = "Bar", remotehost = "Baz" }
cfgNon = Configuration { }