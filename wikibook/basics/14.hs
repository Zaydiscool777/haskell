doGuessing num = do
    putStrLn "Enter your guess:"
    guess <- getLine
    if read guess < num
        then do
            putStrLn "Too low!"
            doGuessing num
        else
            if read guess > num
                then do
                    putStrLn "Too high!"
                    doGuessing num
                else putStrLn "You Win!"

-- do is used because of sequencing