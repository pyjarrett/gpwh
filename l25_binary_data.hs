{-# LANGUAGE OverloadedStrings #-}

--
-- NOTE: Requires stack to build since System.Random isn't
-- provided on my system.
--
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import qualified System.Environment as Sys
import qualified System.Random as Random

import Control.Monad

import qualified Data.List as L

bcInt :: BC.ByteString
bcInt = "6"

toInt :: BC.ByteString -> Int
toInt = read . BC.unpack

safeIntToChar :: Int -> Char
safeIntToChar i = toEnum boundedInt
    where boundedInt = i `mod` 255

intToByteString:: Int -> BC.ByteString
intToByteString i = BC.pack [safeIntToChar i]

replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte location value bytes = BC.concat [before, newValue, newAfter]
    where (before, after) = BC.splitAt location bytes
          newValue = intToByteString value
          newAfter = BC.drop 1 after

randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
    let bytesLength = BC.length bytes
    location <- Random.randomRIO (1, bytesLength)
    value <- Random.randomRIO (0, 255)
    return (replaceByte location value bytes)

sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size bytes = mconcat [before, changed, after]
    where (before, rest) = BC.splitAt start bytes
          (target, after) = BC.splitAt size rest
          changed = (BC.reverse . BC.sort) target

reverseSection :: Int -> Int -> BC.ByteString -> BC.ByteString
reverseSection start size bytes = mconcat [before, changed, after]
    where (before, rest) = BC.splitAt start bytes
          (target, after) = BC.splitAt size rest
          changed = BC.reverse target

randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
    let selectionSize = 25
    let bytesLength = BC.length bytes
    start <- Random.randomRIO (0, bytesLength - selectionSize)
    return (sortSection start selectionSize bytes)

randomReverseBytes :: BC.ByteString -> IO BC.ByteString
randomReverseBytes bytes = do
    let selectionSize = 25
    let bytesLength = BC.length bytes
    start <- Random.randomRIO (0, bytesLength - selectionSize)
    return (reverseSection start selectionSize bytes)


main :: IO ()
main = do
    args <- Sys.getArgs
    let fileName = head args
    imageFile <- BC.readFile fileName
    glitched <- foldM (\bytes fn -> fn bytes) imageFile [
        randomReplaceByte,
        randomSortSection,
        randomReplaceByte,
        randomSortSection,
        randomReplaceByte,
        randomSortSection]
    let glitchedFileName = mconcat [fileName, "glitched"]
    BC.writeFile glitchedFileName glitched
    print "Wrote glitched file."

mySort :: Ord a => Int -> Int -> [a] -> [a]
mySort start size values = mconcat [before, changed, after]
    where (before, rest) = splitAt start values
          (target, after) = splitAt size rest
          changed = (L.reverse . L.sort) target