
    class Monad where
      fmap   :: (a -> b) -> m a -> m b
      (>>=)  :: m a -> (a -> m b) -> m b
      return :: a -> m a