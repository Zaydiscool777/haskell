data Foo = Bar | Baz Int -- bar and baz are constructors for type foo
f :: Foo -> Int -- f Bar or f (Baz x)
f Bar = 1
f (Baz x) = x - 1
weirdtuple = (,,,) "George" "Paul" "John" "Ringo"