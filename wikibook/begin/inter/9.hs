{-
The deriving keyword can only be used with:
Eq
Equality operators == and /=
Ord
Comparison operators < <= > >=; min, max, and compare.
Enum
For enumerations only. Allows the use of list syntax such as [Blue .. Green].
Bounded
Also for enumerations, but can also be used on types that have only one constructor. Provides minBound and maxBound as the lowest and highest values that the type can take.
Show
Defines the function show, which converts a value into a string, and other related functions.
Read
Defines the function read, which parses a string into a value of the type, and other related functions.
https://en.m.wikibooks.org/wiki/File:Base-classes.svg
-}

-- Location, in two dimensions.
class Located a where
    getLocation :: a -> (Int, Int)

class (Located a) => Movable a where
    setLocation :: (Int, Int) -> a -> a

-- An example type, with accompanying instances.
data NamedPoint = NamedPoint
    { pointName :: String
    , pointX    :: Int
    , pointY    :: Int
    } deriving (Show)

instance Located NamedPoint where
    getLocation p = (pointX p, pointY p)

instance Movable NamedPoint where
    setLocation (x, y) p = p { pointX = x, pointY = y }

-- Moves a value of a Movable type by the specified displacement.
-- This works for any movable, including NamedPoint.
move :: (Movable a) => (Int, Int) -> a -> a
move (dx, dy) p = setLocation (x + dx, y + dy) p
    where
    (x, y) = getLocation p