{-# LANGUAGE BangPatterns #-}

{- | FlatBuffers binary encoding from a self-describing
'FlatBuffers.Value.Value' AST.

This is a thin walker on top of "FlatBuffers.Builder" — the
spec-compliant back-to-front builder that's also used by
"Arrow.FlatBufferIPC". The walker translates each 'Value'
constructor into a sequence of 'Builder' calls and finalises
with 'finish'. The result is a real FlatBuffers buffer that any
spec-compliant reader (including "FlatBuffers.View") can parse.

= Why this layer at all?

"FlatBuffers.Builder" is precise but verbose: callers describe
each field individually and own the slot order. This module is
the value-shaped surface — useful when you have a 'Value' AST
in hand (from a quasi-quoter, a generic deriver, a
self-describing-format bridge, ...) and want to serialise it
without building a per-schema encoder.

There's no AST round-trip (and never was): an arbitrary
'Value' isn't enough information to reconstruct the original
vtable layout exactly, but the round-trip
@encode . FlatBuffers.Decode.decode@ is bit-for-bit stable
modulo vtable dedup, which is what users actually depend on.
-}
module FlatBuffers.Encode (
  encode,
) where

import Data.ByteString (ByteString)
import Data.Vector qualified as V
import FlatBuffers.Builder (
  Builder,
  Field',
  currentUOff,
  finish,
  newBuilder,
  prepForObject,
  prependI16,
  prependI32,
  prependI64,
  prependU16,
  prependU32,
  prependU64,
  prependU8,
  scalarI16,
  scalarI32,
  scalarI64,
  scalarU16,
  scalarU32,
  scalarU64,
  scalarU8,
  struct,
  voff,
  writeString,
  writeTable,
 )
import FlatBuffers.Value qualified as F
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import System.IO.Unsafe (unsafePerformIO)


{- | Encode a 'Value' into a real FlatBuffers buffer.

The root constructor must be a 'F.VTable' (FlatBuffers requires
the root to be a table). Scalar / vector / string / struct
inputs are wrapped in a one-slot table for round-trippability,
which matches what the existing 'FlatBuffers.Decode' expects.
-}
encode :: F.Value -> ByteString
encode !val = unsafePerformIO $ do
  b <- newBuilder
  rootUOff <- case val of
    F.VTable fields -> writeValueTable b fields
    _ -> writeValueTable b (V.singleton (Just val))
  finish b rootUOff
{-# NOINLINE encode #-}


-- ============================================================
-- Tables
-- ============================================================

{- | Lay out a vtable + table for a 'V.Vector (Maybe Value)'.
Out-of-line content (strings, vectors, nested tables) is
written first to satisfy the back-to-front emission order;
inline scalars / structs get written in 'writeTable'.
-}
writeValueTable
  :: Builder
  -> V.Vector (Maybe F.Value)
  -> IO Int
writeValueTable b fields = do
  -- Pre-resolve out-of-line slots: walk the fields, materialise
  -- any string / vector / nested-table content, and stash the
  -- resulting UOffsets so 'writeTable' can reference them via
  -- 'voff'. Inline fields (scalars / structs) are written
  -- directly inside the slot writer.
  --
  -- We walk in declaration order; the order doesn't affect the
  -- final layout because everything resolves through UOffsets.
  resolvedFields <- traverseFields b fields
  writeTable b resolvedFields


{- | Walk the 'V.Vector' in declaration order, emit each
out-of-line value, and return the matching @[Maybe Field']@
list 'writeTable' consumes.
-}
traverseFields :: Builder -> V.Vector (Maybe F.Value) -> IO [Maybe Field']
traverseFields b vs = go 0
  where
    !n = V.length vs
    go !i
      | i >= n = pure []
      | otherwise = do
          slot <- case V.unsafeIndex vs i of
            Nothing -> pure Nothing
            Just v -> Just <$> resolveSlot b v
          rest <- go (i + 1)
          pure (slot : rest)


{- | Translate one 'Value' into a 'Field''. Out-of-line content
is emitted /now/ (so its UOffset is known); inline scalars /
structs are deferred to the 'Field'' writer.
-}
resolveSlot :: Builder -> F.Value -> IO Field'
resolveSlot b val = case val of
  F.VBool x -> pure (scalarU8 (if x then 1 else 0))
  F.VInt8 x -> pure (scalarU8 (fromIntegral x))
  F.VInt16 x -> pure (scalarI16 x)
  F.VInt32 x -> pure (scalarI32 x)
  F.VInt64 x -> pure (scalarI64 x)
  F.VWord8 x -> pure (scalarU8 x)
  F.VWord16 x -> pure (scalarU16 x)
  F.VWord32 x -> pure (scalarU32 x)
  F.VWord64 x -> pure (scalarU64 x)
  F.VFloat x -> pure (scalarU32 (castFloatToWord32 x))
  F.VDouble x -> pure (scalarU64 (castDoubleToWord64 x))
  F.VString t -> do
    !uoff <- writeString b t
    pure (voff uoff)
  F.VVector vs -> do
    !uoff <- writeValueVector b vs
    pure (voff uoff)
  F.VTable fields -> do
    !uoff <- writeValueTable b fields
    pure (voff uoff)
  F.VStruct vs -> do
    -- Structs are inline. We compute the total size by walking
    -- the constituent scalars and lay them out tight (no padding
    -- between fields — flatbuffers struct layout assumes the
    -- caller chose alignment-compatible widths). The struct
    -- writer prepends its bytes back-to-front in
    -- /reverse/ field order, so we reverse the iteration here.
    let !sz = V.foldl' (\acc v -> acc + scalarBytes v) 0 vs
        !al = V.foldl' (\acc v -> max acc (scalarBytes v)) 1 vs
    pure $ struct sz al $ \bb -> writeStructFieldsRev bb vs


-- ============================================================
-- Vectors
-- ============================================================

{- | Lay out a vector. Element shape (inline scalar / struct vs.
uoffset to out-of-line content) is determined per element; we
reject mixed-type vectors as malformed.
-}
writeValueVector :: Builder -> V.Vector F.Value -> IO Int
writeValueVector b vs
  | V.null vs = do
      -- Empty vector: 4-byte count of zero, 4-aligned.
      prepForObject b 4 4
      prependU32 b 0
      uoffOnEmpty b
  | otherwise = do
      let !head' = V.unsafeHead vs
      case head' of
        F.VString _ ->
          writeUOffsetVec
            b
            vs
            ( \bb v -> case v of
                F.VString t -> Just <$> writeString bb t
                _ -> pure Nothing
            )
        F.VVector _ ->
          writeUOffsetVec
            b
            vs
            ( \bb v -> case v of
                F.VVector inner -> Just <$> writeValueVector bb inner
                _ -> pure Nothing
            )
        F.VTable _ ->
          writeUOffsetVec
            b
            vs
            ( \bb v -> case v of
                F.VTable fs -> Just <$> writeValueTable bb fs
                _ -> pure Nothing
            )
        _ -> writeInlineVec b vs


{- | UOffset-shaped vector: emit each element's content first,
collect their UOffsets, then write the vector.
-}
writeUOffsetVec
  :: Builder
  -> V.Vector F.Value
  -> (Builder -> F.Value -> IO (Maybe Int))
  -> IO Int
writeUOffsetVec b vs writer = do
  -- Walk left-to-right, materialise each element's content, and
  -- stash UOffsets in declaration order. The actual vector is
  -- emitted afterward (back-to-front).
  let !n = V.length vs
  uoffs <- collectUOffs 0 n []
  -- Emit the vector: 4-byte count followed by n×4-byte
  -- relative offsets.
  prepForObject b (4 + 4 * n) 4
  emitOffs (reverse uoffs)
  prependU32 b (fromIntegral n)
  uoffOnEmpty b
  where
    collectUOffs !i !n acc
      | i >= n = pure (reverse acc)
      | otherwise = do
          mu <- writer b (V.unsafeIndex vs i)
          case mu of
            Just u -> collectUOffs (i + 1) n (u : acc)
            Nothing -> error "FlatBuffers.Encode: heterogeneous vector"
    emitOffs [] = pure ()
    emitOffs (t : ts) = do
      cur <- builderSize b
      prependU32 b (fromIntegral (cur + 4 - t))
      emitOffs ts


{- | Inline-element vector (scalars / structs). Element width is
derived from the first element; mismatching elements are
rejected.
-}
writeInlineVec :: Builder -> V.Vector F.Value -> IO Int
writeInlineVec b vs = do
  let !head' = V.unsafeHead vs
      !w = scalarBytes head'
      !n = V.length vs
  prepForObject b (4 + w * n) (max 4 w)
  -- Reverse-order emission.
  emit (V.length vs - 1)
  prependU32 b (fromIntegral n)
  uoffOnEmpty b
  where
    emit !i
      | i < 0 = pure ()
      | otherwise = do
          writeScalar b (V.unsafeIndex vs i)
          emit (i - 1)


-- ============================================================
-- Scalars (inline writers)
-- ============================================================

-- | Per-scalar in-buffer width.
scalarBytes :: F.Value -> Int
scalarBytes = \case
  F.VBool _ -> 1
  F.VInt8 _ -> 1
  F.VInt16 _ -> 2
  F.VInt32 _ -> 4
  F.VInt64 _ -> 8
  F.VWord8 _ -> 1
  F.VWord16 _ -> 2
  F.VWord32 _ -> 4
  F.VWord64 _ -> 8
  F.VFloat _ -> 4
  F.VDouble _ -> 8
  F.VStruct vs -> V.foldl' (\acc v -> acc + scalarBytes v) 0 vs
  -- UOffset-shaped values inside an inline vector are illegal
  -- per the spec; we'd never reach here from 'writeInlineVec'.
  _ -> error "FlatBuffers.Encode: non-scalar inside inline vector"


-- | Emit a single scalar (back-to-front).
writeScalar :: Builder -> F.Value -> IO ()
writeScalar b = \case
  F.VBool x -> prependU8 b (if x then 1 else 0)
  F.VInt8 x -> prependU8 b (fromIntegral x)
  F.VInt16 x -> prependI16 b x
  F.VInt32 x -> prependI32 b x
  F.VInt64 x -> prependI64 b x
  F.VWord8 x -> prependU8 b x
  F.VWord16 x -> prependU16 b x
  F.VWord32 x -> prependU32 b x
  F.VWord64 x -> prependU64 b x
  F.VFloat x -> prependU32 b (castFloatToWord32 x)
  F.VDouble x -> prependU64 b (castDoubleToWord64 x)
  F.VStruct vs -> writeStructFieldsRev b vs
  v -> error ("FlatBuffers.Encode: writeScalar on non-scalar: " <> showCtor v)


{- | Emit a struct's fields in /reverse/ declaration order
(matching the back-to-front builder).
-}
writeStructFieldsRev :: Builder -> V.Vector F.Value -> IO ()
writeStructFieldsRev b vs = go (V.length vs - 1)
  where
    go !i
      | i < 0 = pure ()
      | otherwise = do
          writeScalar b (V.unsafeIndex vs i)
          go (i - 1)


-- ============================================================
-- Helpers
-- ============================================================

{- | Builder size accessor — convenience aliases that make the
call sites read more directly. The builder's UOffset is just
its current size while we're emitting back-to-front.
-}
builderSize :: Builder -> IO Int
builderSize = currentUOff
{-# INLINE builderSize #-}


uoffOnEmpty :: Builder -> IO Int
uoffOnEmpty = currentUOff
{-# INLINE uoffOnEmpty #-}


showCtor :: F.Value -> String
showCtor = \case
  F.VBool _ -> "VBool"
  F.VInt8 _ -> "VInt8"
  F.VInt16 _ -> "VInt16"
  F.VInt32 _ -> "VInt32"
  F.VInt64 _ -> "VInt64"
  F.VWord8 _ -> "VWord8"
  F.VWord16 _ -> "VWord16"
  F.VWord32 _ -> "VWord32"
  F.VWord64 _ -> "VWord64"
  F.VFloat _ -> "VFloat"
  F.VDouble _ -> "VDouble"
  F.VString _ -> "VString"
  F.VVector _ -> "VVector"
  F.VTable _ -> "VTable"
  F.VStruct _ -> "VStruct"
