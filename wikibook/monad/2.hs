import Text.Read

{- In Haskell, expressions are _referentially transparent_.
This means you can substitute an x = 5 with a normal 5,
and have your code be and work the same.
Now, suppose we had: main = putStrLn (addExclamation getLine)
If getLine was replaced by _any_ string, then we can't get input from the user.
Thus, an IO String is not a String; the result of getLine is called opaque. -}

readByFmap = do
    a <- getLine
    let b = readMaybe a :: Maybe Int
    let c = fmap (2*) b
    print c

-- Even though b is unknown, we can still apply fmap onto it; IO, like Maybe, is a functor.

interactiveConcatenating :: IO ()
interactiveConcatenating = do
    putStrLn "Choose two strings:"
    sz <- (++) <$> getLine <*> getLine -- fmap (getLine <*> getLine)
    putStrLn "Let's concatenate them:"
    putStrLn sz

-- one-liner to sequence things
-- (\_y -> y forgets a value and becomes id)
printSeq = (\_ y -> y) <$> putStrLn "First!" <*> putStrLn "Second!"
-- This is done so much there is an operator just for that: putStrLn "First!" *> putStrLn "Second!"

interactiveConcatenating2 :: IO () = do
    sz <- putStrLn "Choose two strings:" *> ((++) <$> getLine <*> getLine)
    putStrLn "Let's concatenate them:" *> putStrLn sz