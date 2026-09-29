
import Data.Maybe
import Data.List
import Data.Functor
import Text.Read

searchCalcs :: [Int -> Maybe Int] -> Int -> Int -> Int -> Maybe [Int]
searchCalcs _ _ a b | a == b = Just []
searchCalcs _ 0 _ _ = Nothing
searchCalcs m l s g = c
  where
    mIx :: [Int]
    mIx = [0..pred (length m)]
    next :: Int -> Maybe [Int]
    next n = if isNothing (m !! n $ s) then Nothing else (n:) <$> searchCalcs m (pred l) (fromJust (m !! n $ s)) g
    a :: [Maybe [Int]]
    a = map next mIx
    b :: [Maybe [Int]] -- [Just [Int]]
    b = filter isJust a
    c :: Maybe [Int]
    c = if null b then Nothing else head b

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
  let x = searchCalcs [j.apd 0, j.(*2), j.sr '2' "10", j.rev, j.sr '0' "1"] 5 100 101
  print x
