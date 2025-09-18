data Foo = Bar | Baz {age :: Int, name :: String}
h :: Foo -> Int
h Baz {name = s} = length s
h Bar {} = 0
x = Baz 1 "Haskell"
y = Baz {name = "Curry", age = 2}
g :: Foo -> Bool
g Bar {} = True
g Baz {} = False
-- run: h x, h y, g x, g y