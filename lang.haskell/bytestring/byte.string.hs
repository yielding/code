import System.Directory(copyFile)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

copyFile2 :: FilePath -> FilePath -> IO()
copyFile2 source dest = do
  contents <- S.readFile source
  S.writeFile dest contents

main :: IO ()
main = do
  print $ L.pack [99, 97, 110]
  copyFile2 "/tmp/test.py" "/tmp/test2.py"