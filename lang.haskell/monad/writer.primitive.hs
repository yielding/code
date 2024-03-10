import Data.Char(toUpper)

-- import Control.Monad

-- https://stackoverflow.com/questions/33881822/understanding-example-on-writer-monad
newtype Writer w a = Writer { runWriter :: (a, w) }

instance Functor (Writer w) where
  fmap f (Writer (a, w)) = Writer (f a, w)

instance Monoid w => Applicative (Writer w) where
  pure a = Writer (a, mempty)
  Writer (f, w) <*> Writer (a, w') = Writer (f a, w <> w')

instance Monoid w => Monad (Writer w) where
  return = pure
  Writer (a, w) >>= f = let (b, w') = runWriter (f a)
                        in Writer (b, w <> w')

tell :: w -> Writer w ()
tell w = Writer ((), w)

-- newtype Writer w a = Writer (a, w)
-- 
-- instance Functor (Writer w) where
--   fmap f (Writer (a, w)) = Writer (f a, w)
--   
-- class Monad m where
--   (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)
--   return :: a -> m a
-- 
-- instance Monoid w => Monad (Writer w) where
--   f >=> g = \a -> 
--     let Writer (b, s)  = f a
--         Writer (c, s') = g b
--     in Writer (c, s `mappend` s')
--   return a = Writer (a, mempty)
-- 
-- tell :: w -> Writer w ()
-- tell s = Writer ((), s)


upCase :: String -> Writer String
upCase s = (map toUpper s, "upCase ")
-- 
-- toWords :: String -> Writer [String]
-- toWords s = (words s, "toWords ")
-- 
-- process :: String -> Writer [String]
-- process = upCase >=> toWords

main :: IO()
main = print $ "hi"