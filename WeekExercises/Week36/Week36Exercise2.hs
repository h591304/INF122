module Week36Exercise2 where

--2a)

firstHalf :: String -> String
firstHalf s = take (length s `div` 2) s

secondHalf :: String -> String
secondHalf s = drop ((length s + 1) `div` 2) s

semiRepetitive :: String -> Maybe String
semiRepetitive s = 
    if firstHalf s == secondHalf s then
        Just $ firstHalf s
        else 
            Nothing

--2b)

getMidChar :: String -> Maybe Char
getMidChar c =
    let len = length c
        midChar = (len - 1) `div` 2
    in if odd len 
        then Just (c !! midChar)
        else Nothing

decomposeSemiRepetitive :: String -> Maybe (String, Maybe Char)
decomposeSemiRepetitive s =
    if even (length s) then 
        Just (firstHalf s, Nothing)
        else
            Just (firstHalf s, getMidChar s)

--2c)

createSemiRepetitive :: String -> Maybe Char -> String 
createSemiRepetitive s Nothing = s ++ s
createSemiRepetitive s (Just c) = s ++ [c] ++ s

