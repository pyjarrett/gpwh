module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Palindrome

main :: IO ()
main = do
    print "Enter a word to check to see if it is a Palindrome."
    text <- TIO.getLine
    if Palindrome.isPalindrome text
    then putStrLn "Is a palindrome!"
    else putStrLn "Not a palindrome."
