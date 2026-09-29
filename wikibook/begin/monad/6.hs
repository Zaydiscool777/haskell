-- Even a list is a monad!?
-- return x :: a -> [] a = [x]
-- xs >>= f :: [a] -> (a -> [b]) -> [b] = concat (map f xs)

type Board = String -- unimportant
nextConfigs :: Board -> [Board]
nextConfigs bd = undefined -- unimportant
bd :: Board = "unimportant"

nextTurn = nextConfigs bd >>= nextConfigs

threeTurns bd = do
    bd1 <- nextConfigs bd
    bd2 <- nextConfigs bd1
    bd3 <- nextConfigs bd2
    return bd3 -- this looks a bit like a list comprehension...

