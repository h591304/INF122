module Week40Exercise0 where

type SemiRepetitive = (String, Maybe Char)

-- a) implement semiRepetitive
semiRepetitive :: String -> Maybe SemiRepetitive
semiRepetitive str =
    let first = firstHalf str
        second = secondHalf str
        middleChar = findChar str
    in if first == second then Just (first, middleChar) else Nothing

findChar :: String -> Maybe Char
findChar c
    | odd (length c) = Just $ c !! max 0 (length (firstHalf c)) -- !! starts at 0
    | otherwise = Nothing

firstHalf :: String -> String
firstHalf str = take (length str `div` 2) str

secondHalf :: String -> String
secondHalf str = drop ((length str + 1) `div` 2) str

-- b) implement toString
toString :: SemiRepetitive -> String
toString (a, b) = case b of
    Nothing -> a ++ a
    Just b -> a ++ [b] ++ a
    
