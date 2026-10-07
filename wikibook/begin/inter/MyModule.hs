module MyModule where
    removeE xs = [x | x <- xs, x /= 'e']
    coolFunction = id