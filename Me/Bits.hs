
import Data.Bits

instance Bits [Bool] where
  (.&.) :: [Bool] -> [Bool] -> [Bool]
  (.&.) = zipWith (&&)
  (.|.) :: [Bool] -> [Bool] -> [Bool]
  (.|.) = zipWith (||)
  xor :: [Bool] -> [Bool] -> [Bool]
  xor = zipWith (/=)
  complement :: [Bool] -> [Bool]
  complement = map not
  shiftL :: [Bool] -> Int -> [Bool]
  shiftL = flip drop
  shiftR :: [Bool] -> Int -> [Bool]
  shiftR = (. flip replicate False) . flip (++)
  rotateL :: [Bool] -> Int -> [Bool]
  rotateL x n = drop n x ++ take n x
  rotateR :: [Bool] -> Int -> [Bool]
  rotateR x n = drop (n - length x) x ++ take (n - length x) x
  bitSize :: [Bool] -> Int
  bitSize = length
  bitSizeMaybe :: [Bool] -> Maybe Int
  bitSizeMaybe = Just . length
  isSigned :: [Bool] -> Bool
  isSigned = const False
  testBit :: [Bool] -> Int -> Bool
  testBit = (!!)
  bit :: Int -> [Bool]
  bit = (True:) . flip replicate False -- or (++[True])?
  popCount :: [Bool] -> Int
  popCount = length . filter id
instance FiniteBits [Bool] where
  finiteBitSize :: [Bool] -> Int
  finiteBitSize = length

