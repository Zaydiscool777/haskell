import Text.Read

interactiveTripling = do
    putStrLn "Give me a number!"
    s <- getLine
    let mx = readMaybe s :: Maybe Double
    case mx of
        Just x -> putStrLn (show x ++ "*3=" ++ show (x * 3))
        Nothing -> do
            putStrLn "Invalid number"
            interactiveTripling

interactiveAdding = do
    putStrLn "Give me TWO numbers!"
    s <- getLine
    t <- getLine
    let mx = readMaybe s :: Maybe Double
    let my = readMaybe t :: Maybe Double
    case mx of
        Just x ->
            case my of 
                Just y ->
                    putStrLn (show x ++ " + " ++ show y ++ " = " ++ show (x + y))
                Nothing -> retry
        Nothing -> retry
    where
        retry = do
            putStrLn "Invalid number"
            interactiveAdding
-- pretty long, right?

-- <*> is a way to apply functions inside of functors
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- <$> is the infix synonym of fmap
just7 = (+) <$> Just 3 <*> Just 4 -- Just 7
notJust7 = (+) <$> Nothing <*> Just 7 -- Nothing
-- pure is a way of trivially wrapping a value in a functor
pure7 = (+) <$> pure 3 <*> pure 4 :: Num a => Maybe a -- pure 7 / Just 7

interactiveSumming = do
    putStrLn "Choose two numbers:"
    sx <- getLine
    sy <- getLine
    let mx = readMaybe sx :: Maybe Double
        my = readMaybe sy
    case (+) <$> mx <*> my of
        Just z -> putStrLn ("The sum of your numbers is " ++ show z)
        Nothing -> do
            putStrLn "Invalid number. Retrying..."
            interactiveSumming