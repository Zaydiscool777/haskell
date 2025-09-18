nums = [1, 2, 3, 4] -- no mixed types
conned = 0 : nums
lists = [[1,2],[3,4,5],[6]]
bools = True : False : [] -- can't do True : False
tup = (True, "Hello", 1, 'a', [()])
dec = 4 / fromIntegral (length lists) -- Int isn't Number
-- try: tail lists, head lists, fst (1, 2), snd ('f', 4)