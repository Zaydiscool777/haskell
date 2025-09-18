heron a b c = sqrt ( s * (s - a) * (s - b) * (s - c) )
    where
    s = (a + b + c) / 2
heron2 a b c = let s = (a + b + c) / 2
    in sqrt ( s * (s - a) * (s - b) * (s - c) )