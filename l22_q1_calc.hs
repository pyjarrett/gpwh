import Data.List

removeSpaces :: String -> String
removeSpaces str = filter (/= ' ') str

calc :: [String] -> Int
calc (left:"*":right:rest) = (read left) * (read right)
calc (left:"+":right:rest) = (read left) + (read right)


main :: IO ()
main = do
    putStrLn "Add or multiple numbers together"
    userInput <- getContents
    let input = lines (removeSpaces userInput)
    print (calc input)
    --print (calc input)