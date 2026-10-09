{- | Vectors in Arrow layout behind the @vector@ package API.

@'Vector' a@ is a data family (like @Data.Vector.Unboxed.Vector@) with
an instance per element type, each storing its elements the way an
Arrow column does:

* fixed-width types ('Data.Int.Int8' to 'Data.Int.Int64',
  'Data.Word.Word8' to 'Data.Word.Word64', 'Float', 'Double', 'Int',
  'Word' and the Arrow element types 'Arrow.Column.Float16',
  'Arrow.Column.IntervalDayTime', 'Arrow.Column.IntervalMonthDayNano',
  'Arrow.Column.Decimal128', 'Arrow.Column.Decimal256'): a storable
  vector;
* 'Bool': a bitmap;
* @'Maybe' a@: a validity bitmap plus a @'Vector' a@ (a @Vector (Maybe
  Int64)@ costs 8 bytes and one bit per row, with no heap object per
  row);
* 'Data.Text.Text', 'Data.ByteString.ByteString' and @'Vector' a@
  (list rows): start and end offsets into one store, so a row is an
  O(1) slice.

Instances nest: @Vector (Maybe (Vector (Maybe Int32)))@ is a list
column. Use the generic API with element type @Maybe a@ as usual:

> import Data.Vector.Generic qualified as G
>
> total :: Vector (Maybe Int64) -> Int64
> total = G.foldl' (\acc m -> maybe acc (+ acc) m) 0

Fused consumers like this one compile to a loop over the bitmap and the
values that allocates nothing per row. 'Data.Vector.Generic.convert' turns a vector into
a boxed "Data.Vector" (or any other generic vector) and back.

The conversions in "Arrow.Column" ('Arrow.Column.toMaybeVector' and
friends) alias the column's buffers, so the result keeps the decoded
input alive like the column does; 'Data.Vector.Generic.force' copies into fresh buffers.

Mutable 'Data.Text.Text', 'Data.ByteString.ByteString' and list
vectors keep their rows in an append-only store shared by every slice:
writing a row appends its bytes (O(row size)), and 'Data.Vector.Generic.freeze' /
'Data.Vector.Generic.unsafeFreeze' keep every byte ever written. 'Data.Vector.Generic.force' (or
'Data.Vector.Generic.freeze' of a fresh vector) copies only what the rows reference.
-}
module Arrow.Vector (
  Vector,
  MVector,
  IOVector,
  STVector,
  Element,
  FixedWidth (..),
  maybeValues,
) where

import Arrow.Vector.Internal
import Control.Monad.ST (RealWorld)


type IOVector = MVector RealWorld


type STVector s = MVector s
