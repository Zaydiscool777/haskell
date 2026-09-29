inf = [1..] -- if done in ghci, remember that
-- you can press ctrl-c to stop evaluation...
intsFrom n = n : intsFrom (n + 1)
-- inf cannot be sorted or printed
evens = map (2 *) inf
