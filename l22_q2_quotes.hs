quotes :: [String]
quotes = [
    "quote1",
    "quote2",
    "quote3",
    "quote4",
    "quote5"
    ]

lookupQuote :: [String] -> [String]
lookupQuote ("n":xs) = []
lookupQuote (x:xs) = (quotes !! index) : lookupQuote xs
    where index = read x - 1

main :: IO ()
main = do
    mapM_ print quotes
    input <- getContents
    let choices = lines input -- [String]
    mapM_ putStrLn (lookupQuote choices)

