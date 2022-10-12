{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

-- code from haskell school 다믜 
-- for studying

module Main where
import GHC.Types
import Control.Monad

type Coffee = Integer
type Work = Integer
type Solution = Integer
data Result = Solved Solution | YakShaving Work Work
type Code = [Solution]

data DevF :: Type -> Type where
  PopWork :: DevF Work
  PushWork :: Work -> DevF ()
  Solve :: Work -> DevF Result
  Report :: Solution -> DevF ()

data Dev :: Type -> Type where
  Vis :: DevF e -> (e -> Dev a) -> Dev a
  Ret :: a -> Dev a

popWork :: Dev Work
popWork = Vis PopWork Ret

pushWork :: Work -> Dev ()
pushWork w = Vis (PushWork w) Ret

solve :: Work -> Dev Result
solve w = Vis (Solve w) Ret

report :: Solution -> Dev ()
report s = Vis (Report s) Ret

instance Functor Dev where
  fmap f (Vis e k) = Vis e (fmap f . k)
  fmap f (Ret a) = Ret (f a)

instance Applicative Dev where
  pure = Ret
  (<*>) = ap
 
instance Monad Dev where
  (Vis e k1) >>= k2 = Vis e (k1 >=> k2)
  (Ret x) >>= k = k x

burn :: Coffee -> Dev a -> IO Code
burn = \fuel m -> go fuel [1] [] m
  where
    sol :: Work -> Result
    sol w
      | w `mod` 2 /= 0 = YakShaving (w + 2) (w + 1)
      | otherwise      = Solved w

    go :: Coffee -> [Work] -> Code -> Dev a -> IO Code
    go fuel ws c m | fuel <= 0 = pure c
    go fuel ws c (Vis e k) = case (e, ws) of
      (PopWork   , []  ) -> pure c
      (PopWork   , w:ws) -> go (fuel-1) ws        c     (k w)
      (PushWork w, _   ) -> go (fuel-1) (w:ws)    c     (k ())
      (Solve w   , _   ) -> go (fuel-5) ws        c     (k (sol w))
      (Report s  , _   ) -> go (fuel-1) ws        (s:c) (k ())
    go fuel ws c (Ret r) = pure c

develop :: Coffee -> IO Code
develop fuel = burn fuel . forever $ do
  w <- popWork
  s <- solve w
  case s of
    Solved s -> report s
    YakShaving w1 w2 -> do
      pushWork w1
      pushWork w2

main :: IO ()
main = do
  code <- develop 100
  print code
