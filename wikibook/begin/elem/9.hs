triangleNums x = scanl (+) 0 [1..x]
retainEven = filter even
retainLargeEvenMinusOne es =
    [n - 1| n <- es, even n, n > 100]
doubleOfFirstForEvenSeconds ps =
    [2 * x | (x, y) <- ps, even y]
-- using <- makes it not a test
allPairs = [(x, y) | x <- [1..4], y <- [5..8]]