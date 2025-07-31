f x =
    if x > 0 -- let is a clause like if
        then (let lsq = log x ^ 2 in tan lsq) * sin x
        else 0 -- where is like a guard
data Color = Black | White | RGB Int Int Int
describeColour c = 
   "This colour "
   ++ case c of
          Black -> "is black"
          White -> "is white"
          RGB red green blue -> " has an average of the components of " ++ show av
             where av = div (red + green + blue) 3
   ++ ", yeah?"