module MyModule where
    remove_e xs = [x | x <- xs, x /= 'e']
    coolFunction = id