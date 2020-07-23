{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import System.IO

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    fileInput <- TIO.readFile fileName
    let capitalized = T.toUpper fileInput
    let outputFileName = mconcat [ fileName, "capitalized" ]
    TIO.writeFile outputFileName capitalized
