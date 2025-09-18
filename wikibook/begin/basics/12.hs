-- putStrLn :: String -> IO ();
-- IO is an action, and () is a tuple.
-- getLine :: IO String
main = do
    putStrLn "Please enter your name:"
    name <- getLine
    putStrLn ("Hello, " ++ name ++ ", how are you?")