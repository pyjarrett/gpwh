import System.Environment
import System.IO

getCounts :: String -> (Int, Int, Int)
getCounts str = (charCount, wordCount, lineCount)
    where charCount = length str
          wordCount = (length . words) str
          lineCount = (length . lines) str

printCounts :: (Int, Int, Int) -> String
printCounts (charCount, wordCount, lineCount) = unwords [
    "Characters: ", show charCount,
    "Words: ", show wordCount,
    "Lines: ", show lineCount
    ]

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    file <- openFile fileName ReadMode
    input <- hGetContents file
    let summary = (printCounts . getCounts) input
    putStrLn summary
    hClose file
    appendFile "stats.dat" (mconcat [fileName, " ", summary, "\n"])
