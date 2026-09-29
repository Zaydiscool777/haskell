main = do
    putStrLn "The base?"
    base <- getLine
    putStrLn "The height?"
    height <- getLine
    let aRea = (read base * read height) / 2
    putStrLn ("The area of the triangle is " ++ show aRea)
-- fun fact: you can't use <- on the last action, for a complicated reason