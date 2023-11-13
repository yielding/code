import Data.Monoid
import Control.Monad.Writer.Lazy

main :: IO ()
main = do 
 -- 아래 라인을 어떻게 출력하는지 모르겠음.
 -- print $ runWriter (Just (Sum 3) `mappend` (Sum 4))
 print $ runWriter (return 3 :: Writer String Int)
 print $ runWriter (return 3 :: Writer (Sum Int) Int)
 print $ runWriter (return 3 :: Writer (Product Int) Int)