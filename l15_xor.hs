-- Create a type alias for bits, Haskell already has one, but use our own.
type Bits = [Bool]

myXor :: Bool -> Bool -> Bool
myXor a b = (a || b) && (not (a && b))

myXorPair :: (Bool, Bool) -> Bool
myXorPair (a, b) = myXor a b

myXorList :: Bits -> Bits -> Bits
myXorList list1 list2 = map myXorPair (zip list1 list2)

maxBits :: Int
maxBits = length (intToBits' maxBound)

-- Helper function for intToBits'
intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' i = bit : intToBits' (i `div` 2)
    where bit = i `mod` 2 == 1

intToBits :: Int -> Bits
intToBits i = reverse leadingFalse ++ foundBits
    where foundBits = reverse (intToBits' i)
          leadingFalse = take leadingFalseBits (cycle [False])
          leadingFalseBits = maxBits - length foundBits

charToBits :: Char -> Bits
charToBits c = intToBits (fromEnum c)

bitsToInt :: Bits -> Int
bitsToInt bits = sum values
    where values = map (\(_,index) -> 2^index) trueBits
          trueBits = filter (\(bit,_) -> bit == True) bitPairs
          bitPairs = zip bits indices
          indices = [maxBits-1, maxBits-2 .. 0]

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)

stringToBits :: String -> [Bits]
stringToBits str = map charToBits str

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

rotEncoder :: String -> String
rotEncoder text = map rotChar text
    where alphaSize = 1 + fromEnum (maxBound :: Char)
          rotChar = rotNEncoder alphaSize

rotDecoder :: String -> String
rotDecoder text = map rotChar text
    where alphaSize = 1 + fromEnum (maxBound :: Char)
          rotChar = rotNDecoder alphaSize

applyOneTimePad' :: String -> String -> [Bits]
applyOneTimePad' pad plainText = map (\(a, b) -> myXorList a b) pairedBits
    where plainTextBits = map charToBits plainText
          padBits = map charToBits pad
          pairedBits = zip plainTextBits padBits

applyOneTimePad :: String -> String -> String
applyOneTimePad pad plainText = map bitsToChar paddedBits
    where paddedBits = applyOneTimePad' plainText pad

encoderDecoder :: String -> (String -> String)
encoderDecoder pad = applyOneTimePad pad

class Cipher a where
    encode :: a -> String -> String
    decode :: a -> String -> String

data Rot = Rot

instance Cipher Rot where
    encode Rot text = rotEncoder text
    decode Rot text = rotDecoder text

data OneTimePad = OneTimePad String

instance Cipher OneTimePad where
    encode (OneTimePad pad) text = encoderDecoder pad text
    decode (OneTimePad pad) text = encoderDecoder pad text

myOneTimePad :: OneTimePad
myOneTimePad = OneTimePad (cycle [minBound .. maxBound])

linearCongurentialPRNG :: Int -> Int -> Int -> Int -> Int
linearCongurentialPRNG a b maxNumber seed = (a*seed + b) `mod` maxNumber

data StreamCipher = StreamCipher Int Int Int Int deriving (Show)
streamValue (StreamCipher a b maxValue seed) 0 = linearCongurentialPRNG a b maxValue seed
streamValue (StreamCipher a b maxValue seed) n = linearCongurentialPRNG a b maxValue previousValue
    where previousValue = streamValue (StreamCipher a b maxValue seed) (n-1)

instance Cipher StreamCipher where
    encode (StreamCipher a b maxValue seed) text = encoderDecoder padString text
        where padString = map bitsToChar padBits
              textLength = length text
              padBits = map intToBits pad
              pad = map generator [0 .. textLength]
              generator = streamValue (StreamCipher a b maxValue seed)
    decode = encode
