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

decomposeSemiRepetitive :: String -> Maybe (String, Maybe Char)


--2c)

--CreateSemiRepetitive :: String -> Maybe Char -> String

