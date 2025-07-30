main = do
    putStrLn "What is your name?"
    name <- getLine
    if name == "Simon" || name == "John" || name == "Phil"
    then putStrLn "Haskell is a great programming language!"
    else if name == "Koen"
        then putStrLn "debugging Haskell is fun!"
        else putStrLn "I don't know who you are."