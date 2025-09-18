module MyOtherModule where
    remove_e xs = [x | x <- xs, x /= 'e', x /= 'E']
    data CoolType = forall x. ConstructCoolType x
    lawfulEvil = reverse [1..]