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

getNum :: IO () -> IO Double
getNum i = do
    s <- getLine
    let mx = readMaybe s :: Maybe Double
    case mx of
        Nothing -> do
            putStrLn "Invalid Number"
            i
        Just x -> return x

interactiveAdding = do
    putStrLn "Give me TWO numbers!"
    s <- getNum interactiveAdding
    t <- getNum interactiveAdding
    putStrLn (
        show s
        ++ " + "
        ++ show t
        ++ " = "
        ++ show (s + t))