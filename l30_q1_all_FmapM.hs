allFmapM :: Monad m => (a -> b) -> m a -> m b
allFmapM fn val = val >>= (\v -> return (fn v))


app' :: Monad m => m (a -> b) -> m a -> m b

bind :: Monad m => m a -> (a -> m b) -> m b
fmap':: Monam m => (a -> b) -> m a -> m b
