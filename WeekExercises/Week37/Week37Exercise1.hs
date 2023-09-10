module Week37Exercise1 where

f :: Integer -> Integer -> Integer -> Integer -> Integer
f a b c m = a^m + b^m - c^(m-1)

isFZero :: Integer -> Integer -> Integer -> Integer -> Bool
isFZero a b c m = f a b c m == 0

semiFermat :: Integer -> Integer -> [(Integer, Integer, Integer)]
semiFermat n m = [(a, b, c) | a <- [1..n], b <- [a..n], c <- [b..n], isFZero a b c m]
