module Week40Exercise2 where
import Data.List (sort)

data BinSearchTree a
  = Empty
  | Branch (BinSearchTree a) a (BinSearchTree a)
  deriving (Eq, Show)

  -- implement toBinarySearchTree
toBinarySearchTree :: Ord a => [a] -> BinSearchTree a
toBinarySearchTree [] = Empty
toBinarySearchTree list =
  let sortedList = sort list
      rootIndex = length sortedList `div` 2
      lt = take rootIndex sortedList
      rt = drop (rootIndex + 1) sortedList
      root = sortedList !! rootIndex
  in Branch (toBinarySearchTree lt) root (toBinarySearchTree rt)
