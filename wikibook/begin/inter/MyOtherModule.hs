module MyOtherModule where
    removeE xs = [x | x <- xs, x /= 'e', x /= 'E']
    data CoolType = forall x. ConstructCoolType x
    lawfulEvil = reverse [1..]