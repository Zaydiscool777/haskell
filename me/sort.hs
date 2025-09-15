module Main where
import Text.Read

input = [7,1,8,2,6,9,4,2,5,8,0,2,6,8,2,3,6,3,5,8]

sort :: Ord a => [a] -> [a] -- divide
sort [] = []
sort [x] = [x]
sort x = sort2 (sort a) (sort b)
  where
    (a, b) = split x

split :: Ord a => [a] -> ([a], [a])
split [] = ([], [])
split [x] = ([x], [])
split (a:b:s) = (a:sa, b:sb)
  where
    (sa, sb) = split s

sort2 :: Ord a => [a] -> [a] -> [a] -- conquer
sort2 x [] = x
sort2 [] y = y
sort2 (a:sa) (b:sb)
  | a < b = a:(sort2 sa (b:sb))
  | otherwise = b:(sort2 (a:sa) sb)

-- main = putStrLn $ show $ sort input
readInts :: IO [Int] = do
  x <- getLine
  let y = readMaybe x :: Maybe Int
  case y of
    Nothing -> return []
    Just n -> readInts >>= return . (n:)

main = readInts >>= (putStrLn . show . sort)
