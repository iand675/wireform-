{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
-- | Internal: tight builder for Thrift @Struct@ field vectors.
--
-- The Parquet footer encodes ~10 records (FileMetadata, RowGroup,
-- ColumnChunk, ColumnMetadata, Statistics, …) each of which has a
-- handful of mandatory fields and many optional ones. The classic
-- list-based shape:
--
-- @
-- TV.Struct $ V.fromList $
--      [ (1, ...mandatory...) ]
--   ++ maybe [] (\\v -> [(2, ...)]) optionalA
--   ++ maybe [] (\\v -> [(3, ...)]) optionalB
--   ++ ...
-- @
--
-- allocates a closure per @maybe@, a singleton list per present
-- optional, a cons cell per element in the concatenation, walks the
-- list spine for @++@, and finally walks it /again/ to length-then-copy
-- it into a 'V.Vector'. For a fully-populated @ColumnMetadata@ that's
-- ~75–100 allocations per record before the Thrift encoder even runs.
--
-- This module replaces the chain with a pre-sized mutable buffer:
--
-- @
-- TV.Struct (buildFields 15 $ \\fb -> do
--   pushField  fb 1 (TV.I32 ...)
--   pushFieldMb fb 9 statisticsToThrift (cmStatistics cm)
--   ...)
-- @
--
-- 'pushField' and 'pushFieldMb' do exactly one 'MV.unsafeWrite' on a
-- present field, no allocation on an absent field, and 'buildFields'
-- finishes by 'V.unsafeFreeze'-ing the populated prefix. Net cost per
-- record drops to ~1 allocation for the buffer plus one write per
-- field that was actually present.
module Parquet.Footer.Build
  ( FieldsBuilder
  , buildFields
  , pushField
  , pushFieldMb
  , pushFieldWhen
  ) where

import Control.Monad.ST (ST, runST)
import Data.Int (Int16)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)

import qualified Thrift.Value as TV

-- | A scoped builder backed by a mutable vector + a write-cursor.
--
-- The buffer is allocated once per record at the maximum field count;
-- 'pushField' writes at the cursor, 'buildFields' freezes the
-- populated prefix.
data FieldsBuilder s = FieldsBuilder
  { _fbBuf :: !(MV.MVector s (Int16, TV.Value))
  , _fbPos :: {-# UNPACK #-} !(STRef s Int)
  }

-- | @buildFields cap k@ allocates a buffer with capacity @cap@,
-- runs @k@ to populate it, then freezes a 'V.Vector' of the prefix
-- that was actually written. /The caller must size @cap@ to the
-- maximum number of fields the action might write./
{-# INLINE buildFields #-}
buildFields
  :: Int                                                  -- ^ maximum field count
  -> (forall s. FieldsBuilder s -> ST s ())                -- ^ writer
  -> V.Vector (Int16, TV.Value)
buildFields cap k = runST $ do
  mv  <- MV.unsafeNew cap
  pos <- newSTRef 0
  k (FieldsBuilder mv pos)
  n   <- readSTRef pos
  V.unsafeFreeze (MV.unsafeTake n mv)

{-# INLINE pushField #-}
pushField :: FieldsBuilder s -> Int16 -> TV.Value -> ST s ()
pushField (FieldsBuilder mv ref) !fid !val = do
  i <- readSTRef ref
  MV.unsafeWrite mv i (fid, val)
  writeSTRef ref (i + 1)

-- | Push a field iff the 'Maybe' is 'Just'; the projection runs only
-- in the @Just@ branch so the @Nothing@ branch is a no-op (no closure
-- allocation, no singleton list, no concat).
{-# INLINE pushFieldMb #-}
pushFieldMb
  :: FieldsBuilder s
  -> Int16
  -> (a -> TV.Value)
  -> Maybe a
  -> ST s ()
pushFieldMb _  _   _    Nothing   = pure ()
pushFieldMb fb fid proj (Just !x) = pushField fb fid (proj x)

-- | Push a field iff the 'Bool' is 'True'.
{-# INLINE pushFieldWhen #-}
pushFieldWhen
  :: FieldsBuilder s
  -> Int16
  -> Bool
  -> TV.Value
  -> ST s ()
pushFieldWhen fb fid cond val
  | cond      = pushField fb fid val
  | otherwise = pure ()

-- 'modifySTRef'' is brought in for clients that want to add bulk
-- writes; not used by the current call sites but kept here so the
-- module is self-contained.
_unused :: STRef s Int -> Int -> ST s ()
_unused ref delta = modifySTRef' ref (+ delta)
{-# NOINLINE _unused #-}
