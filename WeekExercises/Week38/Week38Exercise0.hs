module Week38Exercise0 where

runningSum :: [Integer] -> [Integer]
runningSum xs = runningSumHelper 0 xs
  where
    runningSumHelper _ [] = []
    runningSumHelper sum (x:xs) = (x + sum) : runningSumHelper (x + sum) xs
