import Data.Char

compact str = map toLower (filter isSpace str)
    where isSpace = (/=) ' ' 

myPalindrome str = (reverse compacted) == compacted
    where compacted = compact str
