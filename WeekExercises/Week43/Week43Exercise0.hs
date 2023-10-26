module Week43Exercise0 where
import Data.Foldable (Foldable(toList))

data BinSearchTree a
  = Leaf a
  | LeftRightChildBranch (BinSearchTree a) a (BinSearchTree a)
  | LeftChildBranch (BinSearchTree a) a
  | RightChildBranch a (BinSearchTree a)
  deriving (Eq, Show)

-- a) implement instance Foldable for BinSearchTree
instance Foldable BinSearchTree where
  foldr f a (Leaf x) = f x a
  foldr f a (LeftRightChildBranch left x right) = foldr f (f x (foldr f a right)) left
  foldr f a (LeftChildBranch left x) = foldr f (f x a) left
  foldr f a (RightChildBranch x right) = f x (foldr f a right)

-- b) implement function toList
toList :: BinSearchTree a -> [a]
toList = foldr (:) []