data TurnstileState = Locked | Unlocked
    deriving (Eq, Show)

data TurnstileOutput = Thank | Open | Tut
    deriving (Eq, Show)

coin, push :: TurnstileState -> (TurnstileOutput, TurnstileState)

coin _ = (Thank, Unlocked)

push Locked   = (Tut , Locked)
push Unlocked = (Open, Locked)

monday :: TurnstileState -> ([TurnstileOutput], TurnstileState)
monday s0 =
    let (a1, s1) = coin s0
        (a2, s2) = push s1
        (a3, s3) = push s2
        (a4, s4) = coin s3
        (a5, s5) = push s4
    in ([a1, a2, a3, a4, a5], s5)

regularPerson, distractedPerson, hastyPerson :: TurnstileState -> ([TurnstileOutput], TurnstileState)

regularPerson s0 =
    let (a1, s1) = coin s0
        (a2, s2) = push s1
    in ([a1, a2], s2)

distractedPerson s0 =
    let (a1, s1) = coin s0
    in ([a1], s1)

hastyPerson s0 =
    (a1 : ac, sf)
    where
        (a1, s1) = push s0
        (a2, s2) = coin s1
        (a3, s3) = push s2
        ac
            | s1 == Unlocked = []
            | otherwise = [a2, a3]
        sf
            | s1 == Unlocked = s1
            | otherwise = s3

tuesday :: TurnstileState -> ([TurnstileOutput], TurnstileState)
tuesday s0 =
    let (aa1, s1) = regularPerson s0
        (aa2, s2) = hastyPerson s1
        (aa3, s3) = distractedPerson s2
        (aa4, s4) = hastyPerson s3
    in (aa1++aa2++aa3++aa4, s4)

luckyPair :: Bool -> TurnstileState -> (Bool, TurnstileState)
luckyPair d s0 =
    let p0 = if d then distractedPerson else regularPerson
        p1 = push
        (a1, s1) = p0 s0
        (a2, s2) = p1 s1
    in (d, s2)