myReplicateM :: Monad m => Int -> m a -> m [a]
myReplicateM n action = mapM (\_ -> action) list
    where list = [1 .. n]

main :: IO ()
main = do
    myReplicateM 5 (print "hi")
    return ()
