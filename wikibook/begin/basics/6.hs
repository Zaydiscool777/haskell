xor :: Bool -> Bool -> Bool -- signature
xor p q = (p || q) && not (p && q)