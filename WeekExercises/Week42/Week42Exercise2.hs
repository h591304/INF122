module Week42Exercise2 where

-- a) implement ifFiveMultiples point-free
isFiveMultiples :: [Integer] -> Bool
isFiveMultiples = all (\x -> x `mod` 5 == 0) 

-- b) implement factorial
factorial :: Integer -> Integer
factorial = product . enumFromTo 1