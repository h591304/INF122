module Week41Exercise1 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Graph n = Map n (Set n)

--a) implement disjoint
disjoint :: (Ord a) => Set a -> Set a -> Bool
disjoint s1 s2 = Set.null $ s1 `Set.intersection` s2

--b) implement hasCycle
hasCycle :: (Ord n) => Graph n -> n -> Bool
hasCycle g n
    | Map.notMember n g = False
    | otherwise = hasCycleFromNode n g Set.empty

hasCycleFromNode :: (Ord n) => n -> Graph n -> Set n -> Bool
hasCycleFromNode n g visited
  | Set.member n visited = True
  | otherwise =
    let neighbors = Map.findWithDefault Set.empty n g
    in any (\neighbor -> hasCycleFromNode neighbor g (Set.insert n visited)) (Set.toList neighbors)