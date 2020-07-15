fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n-2)

main :: IO ()
main = do
    putStrLn "Which fibonacci number do you want?"
    number <- getLine
    let fibNumber = fib (read number)
    putStrLn (show fibNumber)
    return ()