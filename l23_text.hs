{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

steveJobsQuote :: T.Text
steveJobsQuote = "The only way to do great work is to love what you do. If you haven't found it yet, keep looking. Don't settle."

highlight :: T.Text -> T.Text -> T.Text
highlight query fullText = T.intercalate highlighted pieces
    where highlighted = mconcat["{", query, "}"]
          pieces = T.splitOn query fullText

main = do
    TIO.putStrLn (highlight "love" steveJobsQuote)
    
