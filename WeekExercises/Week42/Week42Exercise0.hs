module Week42Exercise0 where
import Data.Void (vacuous)

applyFunctions :: [a -> b] -> [b -> c] -> [a] -> [c]
applyFunctions [] _ _ = []
applyFunctions _ [] _ = []
applyFunctions _ _ [] = []
applyFunctions (f:fs) (g:gs) (x:xs) = g (f x) : applyFunctions fs gs xs