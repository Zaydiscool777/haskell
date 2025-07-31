for :: a -> (a -> Bool) -> (a -> a) -> (a -> IO ()) -> IO ()
for i p f job =
    if p i
    then do
        job i
        for (f i) p f job
    else
        return ()
myInits :: [a] -> [[a]] -- inits [1,2,3] = [[],[1],[1,2],[1,2,3]]
myInits xs = map reverse . scanl (flip (:)) [] $ xs -- $ has low precedence
dollarSignExample = map ($ 2) [(2*), (4*), (8*)]
addPair = uncurry (+) -- curry DOES exist, but all functions are already curried
applyPair = uncurry ($)
stillHello = (const . id) "Hello" "World"