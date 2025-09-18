g x y = (if x == 0 then 1 else sin x / x) * y
-- && does not evaluate the second arg if the first is false
lastNonZero a = go a (length a-1)
  where
    go a l
        | l >= 0 && a !! l == 0 = go a (l-1)
        | l < 0  = Nothing
        | otherwise = Just (a !! l)
-- pattern matching : case :: guards : if-then-else
f x =
    case x of
       0 -> 18
       1 -> 15
       2 -> 12
       _ -> 12 - x
describeString :: String -> String
describeString str =
    case str of
        (x:xs) -> "The first character of the string is: " ++ [x] ++ "; and " ++
            "there are " ++ show (length xs) ++ " more characters in it."
        [] -> "This is an empty string."
-- you can't do this with pattern matching:
data Colour = Black | White | RGB Int Int Int
describeBlackOrWhite c =
    "This colour is"
    ++ case c of
        Black           -> " black"
        White           -> " white"
        RGB 0 0 0       -> " black"
        RGB 255 255 255 -> " white"
        _               -> "... uh... something else"
    ++ "."
