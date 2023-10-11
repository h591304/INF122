module Week41Exercise2 where

import Data.Map (Map)
import qualified Data.Map as Map

data Expr variable
  = Var variable
  | Lit Bool
  | And (Expr variable) (Expr variable)
  | Or (Expr variable) (Expr variable)
  deriving (Eq, Show)
