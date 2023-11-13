import Data.Monoid

newtype Writer extra a = Writer { runWriter :: (a, extra) }

instance Monoid extra => Monoid (Writer extra) where
  return a = Writer (a, mempty)
  Writer (a, e) >>= f =
    let (b, e') = runWriter (f a) in Writer (b, e `mappend` e')

main :: IO ()
main = print "hi"