{-# LANGUAGE BlockArguments, PatternSynonyms #-}
module Parser (
  item, option, sat, char, string,
  sepBy, sepBy1, chainl, chainl1, chainr, chainr1,
  bracket, ops, force, manyf, somef, most, (<<|>)) where
-- https://people.cs.nott.ac.uk/pszgmh/monparsing.pdf
import Data.Maybe (maybeToList, maybe) -- item, int
import Data.List (uncons) -- item
import Control.Monad (guard, join) -- sat
import Control.Applicative
import Control.Monad.Trans.State
type Parser a = StateT String [] a; pattern Parser p = StateT p
item :: Parser Char
item = Parser $ maybeToList . uncons
option x p = p <|> pure x
sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x else empty
char :: Char -> Parser Char
char = sat . (==)
string :: String -> Parser String
string "" = return ""
string (x:xs) = liftA2 (:) (char x) $ string xs
sepBy1 :: Parser a -> Parser b -> Parser [a]
p `sepBy1` sep = liftA2 (:) p $ many (sep >> p)
bracket :: Parser a -> Parser c -> Parser b -> Parser b
(open `bracket` close) p = do
  open
  x <- p
  close
  return x
sepBy :: Parser a -> Parser b -> Parser [a]
p `sepBy` sep = option [] (p `sepBy1` sep)
chainl1 :: Parser (a -> a -> a) -> Parser a -> Parser a
chainl1 op p = p >>= rest where
  {rest x = option x do
    f <- op
    y <- p
    rest (f x y)}
ops :: [(Parser a, b)] -> Parser b
ops x = foldr1 (<|>) [f <$ p | (p, f) <- x]
chainr1 :: Parser (a -> a -> a) -> Parser a -> Parser a
chainr1 op p = do
  x <- p
  option x do
    f <- op
    f x <$> chainr1 op p
chainl :: Parser (a -> a -> a) -> Parser a -> a -> Parser a
chainl op p v = option v $ chainl1 op p
chainr :: Parser (a -> a -> a) -> Parser a -> a -> Parser a
chainr op p v = option v $ chainr1 op p
force :: Parser a -> Parser a
force (Parser p) = Parser \i -> case p i of x@((_,_):_) -> x -- ?
manyf :: Parser a -> Parser [a]
manyf = option [] . somef
somef :: Parser a -> Parser [a]
somef p = force $ liftA2 (:) p $ manyf p
most :: Parser a -> Parser a
most (Parser p) = Parser $ take 1 . p -- uh
infixl 3 <<|>
(<<|>) :: Parser a -> Parser a -> Parser a
p <<|> q = most (p <|> q)
