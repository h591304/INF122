module Week43Exercise1 where

data RoseTree a = Branch a [RoseTree a]
  deriving (Eq, Show)

instance Functor RoseTree where
  fmap f (Branch x tree) = Branch (f x) (map (fmap f) tree)

productNodes :: (Num a) =>  RoseTree [a] -> RoseTree a
productNodes = fmap product

