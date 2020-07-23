toInts :: String -> [Int]
toInts = map read . lines

sumOfSquares :: [Int] -> Int
sumOfSquares values = sum (map (^2) values)

main :: IO ()
main = do
    userInput <- getContents
    let numbers = toInts userInput
    print (sumOfSquares numbers)
    