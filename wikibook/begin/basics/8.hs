mySignum x =
if x < 0 
    then -1 -- then clause is required
    else if x > 0 -- so is else clause
        then 1
        else 0