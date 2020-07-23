
main :: IO ()
main = do
    values <- mapM (\_ -> getLine) [1..3]
    mapM_ putStrLn values
