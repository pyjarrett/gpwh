{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

getCounts :: T.Text -> (Int, Int, Int)
getCounts str = (charCount, wordCount, lineCount)
    where charCount = T.length str
          wordCount = (length . T.words) str
          lineCount = (length . T.lines) str

printCounts :: (Int, Int, Int) -> T.Text
printCounts (charCount, wordCount, lineCount) = (T.pack . unwords) [
    "Characters: ", show charCount,
    "Words: ", show wordCount,
    "Lines: ", show lineCount
    ]

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    file <- openFile fileName ReadMode
    input <- TIO.hGetContents file
    let summary = (printCounts . getCounts) input
    TIO.putStrLn summary
    hClose file
    TIO.appendFile "stats.dat" (mconcat [T.pack fileName, " ", summary, "\n"])
