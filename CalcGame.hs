
import Data.Maybe
import Text.Read

searchCalcs :: forall a. (Num a, Eq a) => [a -> Maybe a] -> Int -> a -> a -> Maybe [a]
searchCalcs _ _ a b | a == b = Just []
searchCalcs _ 0 _ _ = Nothing
searchCalcs m l s g = b
  where
    mIx :: [Int]
    mIx = [0..pred (length m)]
    next :: Int -> Maybe [a]
    next n = (fromIntegral n:) <$> ((m !! n) s >>= flip (searchCalcs m (pred l)) g)
    a :: [Maybe [a]]
    a = map next mIx
    b :: Maybe [a]
    b = listToMaybe (catMaybes a)

idv :: Int -> Int -> Maybe Int
idv y x = if mod x y == 0 then Just $ div x y else Nothing

j :: Int -> Maybe Int -- j. 
j = Just

(<?>) :: (Int -> Int -> Int) -> Int -> Int -> Maybe Int
(f <?> y) x = Just (f y x)

sub :: Int -> Int -> Int
sub = subtract

rev :: Int -> Int
rev = unsign (read . reverse . show)
  where unsign f x = f (abs x) * signum x

sl :: Int -> Int
sl = fromMaybe 9999 . readMaybe . init . show

apd :: Int -> Int -> Int
apd x = (+x) . (*10)

sr :: Char -> [Char] -> Int -> Int
sr a b = read . sr' . show
  where
    sr' [] = []
    sr' (x:r) | x == a = b ++ sr' r
    sr' (x:r) = x:sr' r

main :: IO ()
main = do
  let x = searchCalcs (map (Just .) [apd 0, (*2), sr '2' "10", rev, sr '0' "1"]) 5 100 101
  print x
