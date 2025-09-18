{-
putStrLn :: String -> IO ()
getLine  :: IO String
since String isn't IO String, we have to
use the IO action to return
a string using <-.
-}
main =
 do putStrLn "What is your name? "
    name <- getLine
    putStrLn ("Hello " ++ name)