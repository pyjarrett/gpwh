import Control.Monad
import Data.Char

powersOfTwo :: Int -> [Int]
powersOfTwo n = do
    value <- [1 .. n]
    return (2^value)

powersOfTwoLC :: Int -> [Int]
powersOfTwoLC n = [2^x | x <- [1 .. n]]

powersOfTwoAndThree :: Int -> [(Int, Int)]
powersOfTwoAndThree n = do
    value <- [1 .. n]
    return (2^value, 3^value)

powersOfTwoAndThreeLC :: Int -> [(Int, Int)]
powersOfTwoAndThreeLC n = [(2^x, 3^x) | x <- [1 .. n]]

allEvenOdds :: Int -> [(Int, Int)]
allEvenOdds n = do
    evens <- [2, 4 .. n]
    odds <- [1, 3 .. n]
    return (evens, odds)

evensGuard :: Int -> [Int]
evensGuard n = do
    value <- [1 .. n]
    guard (even value)
    return value

evensGuardLC :: Int -> [Int]
evensGuardLC n = [x | x <- [1 .. n], even x]

colors :: [String]
colors = ["Mr. " ++ name | val <- ["brown", "blue", "pink", "orange"],
    let name = (\(x:xs) -> Data.Char.toUpper x:xs) val]