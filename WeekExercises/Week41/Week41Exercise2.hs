module Week41Exercise2 where

import Data.Map (Map)
import qualified Data.Map as Map

data Expr variable
  = Var variable
  | Lit Bool
  | And (Expr variable) (Expr variable)
  | Or (Expr variable) (Expr variable)
  deriving (Eq, Show)

eval :: (Ord variable) => Expr variable -> Map variable Bool -> Maybe Bool
eval (Var v) vMap = Map.lookup v vMap
eval (Lit b) _ = Just b
eval (And e1 e2) vMap
    | Just v1 <- eval e1 vMap, Just v2 <- eval e2 vMap = Just (v1 && v2)
    | otherwise = Nothing 

eval (Or e1 e2) vMap
    | Just v1 <- eval e1 vMap, Just v2 <- eval e2 vMap = Just (v1 || v2)
    | otherwise = Nothing 