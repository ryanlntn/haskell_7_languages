module DayOne where
    allEven :: [Integer] -> [Integer]
    allEven [] = []
    allEven (h:t) = if even h then h:allEven t else allEven t

    allEvenListComp :: [Integer] -> [Integer]
    allEvenListComp l = [x | x <- l, x `mod` 2 == 0]

    reversed :: [a] -> [a]
    reversed [] = []
    reversed (h:t) = reversed(t) ++ [h]

    colors = ["black","white","blue","yellow","red"]
    colorCombos = [(a,b) | a <- colors, b <- colors, a /= b, a < b]

    multiplicationTable = [(a,b,a*b) | a <- [1..12], b <- [1..12]]
