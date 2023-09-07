module Week37Exercise0 where

str :: (String, String, String) -> String
str (x, y, z) = x ++ " is studying at " ++ y ++ " department and started in " ++ z

information :: [String] -> [String] -> [Integer] -> [String]
information name insName year =
    let contained = zip3 name insName (map show year)
        filtered = filter (\(_, _, year) -> read year >= 2022) contained
    in map str filtered 

