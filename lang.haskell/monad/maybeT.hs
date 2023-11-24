
-- in Control.Monad.Trans.Maybe
newtype MaybeT m a = MaybeT { runMaybeT m (Maybe a) }

instance Monad m => Monad (MaybeT m) where
    return  = MaybeT . return . Just  -- return . Just == m (Maybe a)

    x >>= f = MaybeT $ do
        maybe_value <- runMaybeT x
        case maybe_value of
            Nothing    -> return Nothing
            Just value -> runMaybeT $ f value

instance Monad m => Applicative (MaybeT m) where
    pure  = return
    (<*>) = ap

instance Monad m => Functor (MaybeT m) where
    fmap = liftM

instance Monad m => Alternative (MaybeT m) where
    empty   = MaybeT $ return Nothing
    x <|> y = MaybeT $ do maybe_value <- runMaybeT x
                          case maybe_value of
                              Nothing -> runMaybeT y
                              Just _  -> return maybe_value

instance Monad m => MoandPlus (MaybeT m) where
    mzero = empty
    mplus = (<|>)

instance MonadTrans MaybeT where
    lift = MaybeT . (liftM Just)
