module Week38Exercise1 where

combinations :: Integer -> [Char] -> [String]
combinations 0 _ = [""]
combinations n alphabet = [x : xs | x <- alphabet, xs <- combinations (n-1) alphabet] 

