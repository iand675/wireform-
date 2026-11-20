-- | Exact predicates over Delta's logical Arrow columns. Column paths use
-- individual display names so a name containing a dot remains unambiguous.
module Delta.Predicate (
  Predicate (..),
  ColumnTest (..),
  Comparison (..),
  PValue (..),
) where

import Columnar.Predicate (PValue (..))
import Data.Text (Text)


data Predicate
  = Column !Text ![Text] !ColumnTest
  | And !Predicate !Predicate
  | Or !Predicate !Predicate
  | Not !Predicate
  | Always !Bool
  deriving stock (Show, Eq)


data ColumnTest
  = Compare !Comparison !PValue
  | NullSafeEqual !(Maybe PValue)
  | In ![Maybe PValue]
  | IsNull
  | Like !Text -- ^ A SQL pattern. Integer and boolean fields use their text representation.
  | Size !Comparison !Int -- ^ Number of elements in an array or entries in a map.
  | StringLength !Comparison !Int -- ^ Number of Unicode code points in a string.
  deriving stock (Show, Eq)


data Comparison = Equal | NotEqual | Less | LessEqual | Greater | GreaterEqual
  deriving stock (Show, Eq)
