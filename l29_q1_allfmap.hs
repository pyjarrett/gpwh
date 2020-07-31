allFmap :: Applicative f => (a -> b) -> f a -> f b
allFmap fn val = pure fn <*> val
