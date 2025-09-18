-- The Golden Rule Of Indentation!
{-
    Code which is part of some expression should be 
        indented further in than the beginning of that expression
    (even if the expression is not the leftmost element of the line).
-}
z = x + y
    where x = 1
          y = 2 -- that's stupid
-- Indentation isn't actually needed:
{-
    1. If you see one of the layout keywords, (let, where, of, do),
    insert an open curly brace (right before the stuff that follows it)
    2. If you see something indented to the SAME level, insert a semicolon
    3. If you see something indented LESS, insert a closing curly brace
    4. If you see something unexpected in a list, like where,
    insert a closing brace before instead of a semicolon.
-}
foo :: Double -> Double; foo x = let { s = sin x; c = cos x } in 2 * s * c