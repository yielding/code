import System.Directory(copyFile)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

copyFile2 :: FilePath -> FilePath -> IO()
  contents <- L.readFile source
  L.writeFile dest contents

main :: IO ()
main = do
  print $ L.pack [99, 97, 110]
  copyFile2 "/tmp/test.py" "/tmp/test2.py"