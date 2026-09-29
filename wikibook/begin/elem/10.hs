data Anniversary =
    Birthday
        String Date
    | Wedding
        String String Date
johnSmith :: Anniversary
johnSmith = Birthday "John Smith" (Date 1968 7 3)
smithWedding = Wedding "John Smith" "Jane Smith" (Date 1987 3 4)
type AnniversaryBook = [Anniversary]
anniversariesOfJohnSmith :: AnniversaryBook
anniversariesOfJohnSmith = [johnSmith, smithWedding]

data Date = Date Int Int Int
showDate (Date y m d) =
    show y ++ "-" ++ show m ++ "-" ++ show d
showAnniversary :: Anniversary -> String
showAnniversary (Birthday name date) =
   name ++ " born " ++ showDate date
showAnniversary (Wedding name1 name2 date) =
   name1 ++ " married " ++ name2 ++ " on " ++ showDate date
