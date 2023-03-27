import Distribution.Simple (Bound)
import Language.Haskell.TH (fromE)
--L15-1
data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)

--L15-2
rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
  where
    halfAlphabet = alphabetSize `div` 2
    offset = fromEnum c + halfAlphabet
    rotation = offset `mod` alphabetSize

--L15-3
largestCharNumber :: Int
largestCharNumber = fromEnum (maxBound :: Char)

smallestCharNumber :: Int
smallestCharNumber = fromEnum (minBound :: Char)

--L15-4
rotChar :: Char -> Char
rotChar charToEncrypt = rotN sizeOfAlphabet charToEncrypt
  where
    sizeOfAlphabet = 1 + fromEnum (maxBound :: Char)

--L15-5
fourLetterMessage :: [FourLetterAlphabet]
fourLetterMessage = [L1, L3, L4, L1, L1, L2]

--L15-6
fourLetterEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterEncoder vals = map rot4l vals
  where
    alphaSize = fromEnum (maxBound :: FourLetterAlphabet) + 1
    rot4l = rotN alphaSize

--L15-7
data ThreeLetterAlphabet =
  Alpha | Beta | Kappa
  deriving (Show, Enum, Bounded)

threeLetterMessage :: [ThreeLetterAlphabet]
threeLetterMessage = [Alpha, Alpha, Beta, Alpha, Kappa]

threeLetterEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterEncoder vals = map rot3l vals
  where
    alphaSize = fromEnum (maxBound :: ThreeLetterAlphabet) + 1
    rot3l  = rotN alphaSize

--L15-8
rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation
  where
    halfN = n `div` 2
    offset =
      if even n
      then fromEnum c + halfN
      else fromEnum c + halfN + 1
    rotation = offset `mod` n

--L15-9
threeLetterDecoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterDecoder vals = map rot3ldecoder vals
  where
    alphaSize = fromEnum (maxBound :: ThreeLetterAlphabet) + 1
    rot3ldecoder  = rotNdecoder alphaSize

--L15-10
rotEncoder :: String -> String
rotEncoder s = map rotChar s
  where
    alphaSize = fromEnum (maxBound :: Char) + 1
    rotChar = rotN alphaSize

rotDecoder :: String -> String
rotDecoder s = map rotCharDecoder s
  where
    alphaSize = fromEnum (maxBound :: Char) + 1
    rotCharDecoder = rotNdecoder alphaSize

--L15-11
xorBool :: Bool -> Bool -> Bool
xorBool value1 value2 = (value1 || value2) && (not (value1 && value2))

--L15-12
xorPair :: (Bool, Bool) -> Bool
xorPair (b1, b2) = xorBool b1 b2

--L15-13
xor :: [Bool] -> [Bool] -> [Bool]
xor bs1 bs2 = map xorPair (zip bs1 bs2)

--L15-14
type Bits = [Bool]

--L15-15
intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n =
  if (remainder == 0)
  then False : intToBits' nextVal
  else True : intToBits' nextVal
  where
    remainder = n `mod` 2
    nextVal = n `div` 2

--L15-16
maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
  where
    reversedBits = reverse (intToBits' n)
    missingBits = maxBits - (length reversedBits)
    leadingFalses = take missingBits (cycle [False])

--L15-17
charToBits :: Char -> Bits
charToBits c = intToBits (fromEnum c)

--L15-18
bitsToInt :: Bits -> Int
bitsToInt bits = sum(map (\ x -> 2^(snd x)) trueLocations)
  where
    size = length bits
    indices = [size - 1, size - 2 .. 0]
    trueLocations = filter fst (zip bits indices)

--L15-19
bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)

--L15-20
myPad :: String
myPad = "Shhhhhh"

--L15-21
myPlainText :: String
myPlainText = "Haskell"

--L15-22
applyOTP' :: String -> String -> [Bits]
applyOTP' pad plainText =
  map
    (\ pair -> (fst pair) `xor` (snd pair))
    (zip padBits plainTextBits)
  where
    padBits = map charToBits pad
    plainTextBits = map charToBits plainText

--L15-23
applyOTP :: String -> String -> String
applyOTP pad plainText = map bitsToChar bitList
  where
    bitList = applyOTP' pad plainText

--L15-24
encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad

--L15-25
class Cipher a where
  encode :: a -> String -> String
  decode :: a -> String -> String

--L15-26 ROT13のための単純な型
data Rot = Rot

--L15-27 ROT13で暗号化するためRotをCipherのインスタンスにする
instance Cipher Rot where
  encode Rot = rotEncoder
  decode Rot = rotDecoder

--L15-28 ワンタイムパッドの型
data OneTimePad = OTP String

--L15-29 ワンタイムパッドで暗号化するためOneTimePadをCipherのインスタンスにする
instance Cipher OneTimePad where
  encode (OTP pad) text = applyOTP pad text
  decode (OTP pad) text = applyOTP pad text

--L15-30
myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])

--L15-31
prng :: Int -> Int -> Int -> Int -> Int
prng a b maxNum seed = (a * seed + b) `mod` maxNum

examplePRNG :: Int -> Int
examplePRNG = prng 1337 7 100
