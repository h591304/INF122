module Week41Exercise0 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Maybe
import Data.Graph (graphFromEdges)

type Graph n = Map n (Set n)

-- a) implement bridge
bridge :: (Ord n) => n -> n -> Graph n -> Graph n
bridge n1 n2 g =
    let hasPath = path g n1 n2 
    in if isJust hasPath then g 
    else Map.insertWith Set.union n1 (Set.singleton n2) 
        $ Map.insertWith Set.union n2 Set.empty g

path :: (Ord node) => Graph node -> node -> node -> Maybe [node]
path g start end = path' g start end Set.empty

path' :: (Ord node) => Graph node -> node -> node -> Set node -> Maybe [node]
path' g start end visited
  | Set.member start visited = Nothing
  | start == end = Just []
  | otherwise = do
      let visited' = Set.insert start visited
      nexts <- Map.lookup start g
      listToMaybe $ mapMaybe (\next -> do
        pathCont <- path' g next end visited'
        Just (next:pathCont)) (Set.toList nexts)



        