main =
 do x <- getX
    putStrLn x
getX :: IO String
getX =
 do return "My Shangri-La"
    return "beneath"
    return "the summer moon"
    return "I will"
    return "return"
    return "again"