factorialR n = go n 1
    where
    go n res
        | n > 1     = go (n - 1) (res * n)
        | otherwise = res
-- or...
factorial n = product [1..n]