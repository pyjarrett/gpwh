import System.Environment
import System.IO

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

tryCopyFile :: [String] -> IO ()
tryCopyFile [] = putStrLn "No args provided."
tryCopyFile (x:xs) = if length xs == 0
                     then putStrLn "Not enough args provided"
                     else if length xs /= 1
                        then putStrLn "Too many arguments provided."
                        else doCopy x (head xs)

doCopy :: String -> String -> IO ()
doCopy from to = do
    putStrLn (mconcat ["Copying!", from, " -> ", to])
    fileFrom <- openFile from ReadMode
    fileTo <- openFile to WriteMode
    contents <- TIO.hGetContents fileFrom
    TIO.hPutStr fileTo contents
    hClose fileFrom
    hClose fileTo



main :: IO ()
main = do
    args <- getArgs
    tryCopyFile args
