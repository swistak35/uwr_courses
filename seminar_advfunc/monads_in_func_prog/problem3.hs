newtype MaybeT m a = MaybeT { runMaybeT :: Maybe (m a) }

instance (Monad m) => Monad (MaybeT m) where
     return = MaybeT . Just . return
     x >>= f = MaybeT $ do case (runMaybeT x) of
                              Nothing -> return Nothing
                              Just a -> return a
-- some_val <- runMaybeT x
                           -- return x
                           -- runMaybeT $ f some_val
