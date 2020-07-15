data ThreeLetterAlphabet = Alpha | Beta | Kappa deriving (Show, Enum, Bounded)
data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)

rotNEncoder :: (Bounded a, Enum a) => Int -> a -> a
rotNEncoder n character = toEnum rotated
    where characterAsNum = fromEnum character
          half = n `div` 2
          rotated = (half + characterAsNum) `mod` n

rotNDecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNDecoder n character = toEnum rotated
    where half = n `div` 2
          added = half + fromEnum character
          rotated = if even n
                    then added `mod` n
                    else (1 + added) `mod` n

encode :: (Bounded a, Enum a) => Int -> [a] -> [a]
encode alphabetSize letters = map encoder letters
    where encoder = rotNEncoder alphabetSize

decode :: (Bounded a, Enum a) => Int -> [a] -> [a]
decode alphabetSize letters = map decoder letters
    where decoder = rotNDecoder alphabetSize
