{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Kafka.Protocol.Wire.Primitives
Description : 'Wire' instances for Kafka-protocol primitive types

Layered on "Kafka.Protocol.Wire" — the primitive byte / int helpers
live there. This module wraps the Kafka-specific length-prefixed
encodings (strings, bytes, arrays, UUIDs, tagged fields, varints in
their newtype wrappers) into 'Wire' instances /and/ exposes a small
set of @poke*@ / @peek*@ helpers the code generator drops directly
into the per-message encoders (skipping a typeclass dispatch).

The @C@ in @CompactString@ / @CompactBytes@ / @CompactArray@ marks
the flexible-version encoding: lengths are @UVarInt@ rather than
fixed-width and a length of 0 indicates null (so non-null lengths
are encoded as @len + 1@). The 'Wire' instances dispatch on the
newtype constructor; the codegen picks compact vs. non-compact
based on the API version.
-}
module Kafka.Protocol.Wire.Primitives (
  -- * String / bytes / array helpers
  pokeKafkaString,
  peekKafkaString,
  pokeCompactString,
  peekCompactString,
  pokeNullableKafkaString,
  peekNullableKafkaString,
  pokeNullableCompactString,
  peekNullableCompactString,

  -- * Bytes
  pokeKafkaBytes,
  peekKafkaBytes,
  pokeCompactBytes,
  peekCompactBytes,
  pokeNullableKafkaBytes,
  peekNullableKafkaBytes,
  pokeNullableCompactBytes,
  peekNullableCompactBytes,

  -- * UUID
  pokeKafkaUuid,
  peekKafkaUuid,

  -- * Arrays (length prefix only — element pokes supplied by caller)
  pokeKafkaArrayLen,
  peekKafkaArrayLen,
  pokeCompactArrayLen,
  peekCompactArrayLen,
  pokeNullableArrayLen,
  peekNullableArrayLen,
  pokeNullableCompactArrayLen,
  peekNullableCompactArrayLen,

  -- * Full-array helpers (length prefix + element loop)
  pokeKafkaArray,
  peekKafkaArray,
  pokeCompactArray,
  peekCompactArray,
  pokeNullableKafkaArray,
  peekNullableKafkaArray,
  pokeNullableCompactArray,
  peekNullableCompactArray,

  -- * Versioned arrays — switches between the compact and the

  -- non-compact codec at the supplied flexible-version threshold
  -- (mirrors @E.encodeVersionedArray@'s shape).
  pokeVersionedArray,
  peekVersionedArray,
  pokeVersionedNullableArray,
  peekVersionedNullableArray,

  -- * Tagged-field entries (KIP-866-style payloads)
  pokeTaggedFieldEntries,

  -- * Tagged fields
  pokeEmptyTaggedFields,
  peekTaggedFieldsCount,
  skipTaggedFieldsBody,
  peekAndSkipTaggedFields,
  peekTaggedFieldsMap,

  -- * Estimators (called by 'wireMaxSize' implementations)
  kafkaStringMaxSize,
  kafkaBytesMaxSize,
  compactStringMaxSize,
  compactBytesMaxSize,
  compactArrayHeaderMaxSize,

  -- * Dual-format estimators (compact-or-plain upper bound)
  dualStringMaxSize,
  dualBytesMaxSize,
) where

import Control.Exception (throwIO)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int32)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.UUID qualified as UUID
import Data.Vector qualified as Vector
import Data.Vector.Mutable qualified as VM
import Data.Word (Word32, Word8)
import Foreign.Marshal.Utils qualified
import Foreign.Ptr (Ptr, minusPtr, plusPtr)
import Kafka.Protocol.Primitives qualified as P
import Kafka.Protocol.Wire


----------------------------------------------------------------------
-- Strings (non-compact)
----------------------------------------------------------------------

{- | Upper bound on the bytes a 'P.KafkaString' will occupy: 2 byte
length + UTF-8 length of the contained text.
-}
{-# INLINE kafkaStringMaxSize #-}
kafkaStringMaxSize :: P.KafkaString -> Int
kafkaStringMaxSize (P.KafkaString P.Null) = 2
kafkaStringMaxSize (P.KafkaString (P.NotNull t)) =
  -- UTF-8 worst-case is 4 bytes per code point; over-estimating is
  -- the right call for a buffer-sizing hint.
  2 + 4 * 1 * fromIntegral (sizeOfText t)


sizeOfText :: Text -> Int
sizeOfText t = BS.length (TE.encodeUtf8 t)


{-# INLINE pokeKafkaString #-}
pokeKafkaString :: Ptr Word8 -> P.KafkaString -> IO (Ptr Word8)
pokeKafkaString p (P.KafkaString P.Null) = pokeInt16BE p (-1)
pokeKafkaString p (P.KafkaString (P.NotNull t)) = do
  let !bs = TE.encodeUtf8 t
      !n = BS.length bs
  p1 <- pokeInt16BE p (fromIntegral n)
  pokeByteString p1 bs


{-# INLINE peekKafkaString #-}
peekKafkaString :: Ptr Word8 -> Ptr Word8 -> IO (P.KafkaString, Ptr Word8)
peekKafkaString p endPtr = do
  (len, p1) <- peekInt16BE p endPtr
  if len < 0
    then pure (P.KafkaString P.Null, p1)
    else do
      (bs, p2) <- peekByteString p1 endPtr (fromIntegral len)
      let !t = TE.decodeUtf8 bs
      pure (P.mkKafkaString t, p2)


{- | Always-non-null version (length is encoded as a non-negative
Int16). Used by fields that are explicitly required.
-}
pokeNullableKafkaString :: Ptr Word8 -> P.KafkaString -> IO (Ptr Word8)
pokeNullableKafkaString = pokeKafkaString


peekNullableKafkaString :: Ptr Word8 -> Ptr Word8 -> IO (P.KafkaString, Ptr Word8)
peekNullableKafkaString = peekKafkaString


----------------------------------------------------------------------
-- Compact strings
----------------------------------------------------------------------

{-# INLINE compactStringMaxSize #-}
compactStringMaxSize :: P.CompactString -> Int
compactStringMaxSize cs = case P.unCompactString cs of
  P.Null -> 1
  P.NotNull t -> 5 + sizeOfText t -- 5-byte UVarInt worst case + UTF-8 length


{-# INLINE pokeCompactString #-}
pokeCompactString :: Ptr Word8 -> P.CompactString -> IO (Ptr Word8)
pokeCompactString p cs = case P.unCompactString cs of
  P.Null -> pokeUVarInt p 0
  P.NotNull t -> do
    let !bs = TE.encodeUtf8 t
        !n = BS.length bs
    p1 <- pokeUVarInt p (fromIntegral n + 1)
    pokeByteString p1 bs


{-# INLINE peekCompactString #-}
peekCompactString :: Ptr Word8 -> Ptr Word8 -> IO (P.CompactString, Ptr Word8)
peekCompactString p endPtr = do
  (len, p1) <- peekUVarInt p endPtr
  if len == 0
    then pure (compactStringNull, p1)
    else do
      (bs, p2) <- peekByteString p1 endPtr (fromIntegral len - 1)
      let !t = TE.decodeUtf8 bs
      pure (P.mkCompactString t, p2)


{- | A compact string carrying 'P.Null'. Defined here because the
'P.CompactString' constructor is intentionally hidden by
"Kafka.Protocol.Primitives"; we use the public @mk@ helpers in
the @NotNull@ branch and round-trip through the
'P.fromCompactString' / 'P.toCompactString' coercion for the
null case so we don't reach into the constructor.
-}
compactStringNull :: P.CompactString
compactStringNull = P.toCompactString (P.KafkaString P.Null)


pokeNullableCompactString
  :: Ptr Word8 -> P.CompactString -> IO (Ptr Word8)
pokeNullableCompactString = pokeCompactString


peekNullableCompactString
  :: Ptr Word8 -> Ptr Word8 -> IO (P.CompactString, Ptr Word8)
peekNullableCompactString = peekCompactString


----------------------------------------------------------------------
-- Bytes (non-compact)
----------------------------------------------------------------------

{-# INLINE kafkaBytesMaxSize #-}
kafkaBytesMaxSize :: P.KafkaBytes -> Int
kafkaBytesMaxSize (P.KafkaBytes P.Null) = 4
kafkaBytesMaxSize (P.KafkaBytes (P.NotNull bs)) = 4 + BS.length bs


{-# INLINE pokeKafkaBytes #-}
pokeKafkaBytes :: Ptr Word8 -> P.KafkaBytes -> IO (Ptr Word8)
pokeKafkaBytes p (P.KafkaBytes P.Null) = pokeInt32BE p (-1)
pokeKafkaBytes p (P.KafkaBytes (P.NotNull bs)) = do
  let !n = BS.length bs
  p1 <- pokeInt32BE p (fromIntegral n)
  pokeByteString p1 bs


{-# INLINE peekKafkaBytes #-}
peekKafkaBytes :: Ptr Word8 -> Ptr Word8 -> IO (P.KafkaBytes, Ptr Word8)
peekKafkaBytes p endPtr = do
  (len, p1) <- peekInt32BE p endPtr
  if len < 0
    then pure (P.KafkaBytes P.Null, p1)
    else do
      (bs, p2) <- peekByteString p1 endPtr (fromIntegral len)
      pure (P.mkKafkaBytes bs, p2)


pokeNullableKafkaBytes :: Ptr Word8 -> P.KafkaBytes -> IO (Ptr Word8)
pokeNullableKafkaBytes = pokeKafkaBytes


peekNullableKafkaBytes :: Ptr Word8 -> Ptr Word8 -> IO (P.KafkaBytes, Ptr Word8)
peekNullableKafkaBytes = peekKafkaBytes


----------------------------------------------------------------------
-- Compact bytes
----------------------------------------------------------------------

{-# INLINE compactBytesMaxSize #-}
compactBytesMaxSize :: P.CompactBytes -> Int
compactBytesMaxSize cb = case P.unCompactBytes cb of
  P.Null -> 1
  P.NotNull bs -> 5 + BS.length bs


{- | Worst-case wire size of a 'P.KafkaBytes' field that may be
encoded either as compact bytes (varint length prefix, on
flexible versions) or plain bytes (Int32 length prefix, on
non-flexible versions). The codegen uses this for fields
that straddle the flexible-version threshold so the per-
request buffer is sized for the worst case regardless of
which protocol version we end up at runtime.

  * non-null payload: @max (4 + n) (5 + n) = 5 + n@
  * null payload:     @max 4 1 = 4@
-}
{-# INLINE dualBytesMaxSize #-}
dualBytesMaxSize :: P.KafkaBytes -> Int
dualBytesMaxSize (P.KafkaBytes P.Null) = 4
dualBytesMaxSize (P.KafkaBytes (P.NotNull bs)) = 5 + BS.length bs


{- | Worst-case wire size of a 'P.KafkaString' field that may be
encoded either as compact string (flexible) or plain string
(non-flexible). Symmetric to 'dualBytesMaxSize'.

  * non-null payload: @max (2 + n) (5 + n) = 5 + n@
  * null payload:     @max 2 1 = 2@
-}
{-# INLINE dualStringMaxSize #-}
dualStringMaxSize :: P.KafkaString -> Int
dualStringMaxSize ks = case P.unKafkaString ks of
  P.Null -> 2
  P.NotNull t -> 5 + BS.length (TE.encodeUtf8 t)


{-# INLINE pokeCompactBytes #-}
pokeCompactBytes :: Ptr Word8 -> P.CompactBytes -> IO (Ptr Word8)
pokeCompactBytes p cb = case P.unCompactBytes cb of
  P.Null -> pokeUVarInt p 0
  P.NotNull bs -> do
    let !n = BS.length bs
    p1 <- pokeUVarInt p (fromIntegral n + 1)
    pokeByteString p1 bs


{-# INLINE peekCompactBytes #-}
peekCompactBytes :: Ptr Word8 -> Ptr Word8 -> IO (P.CompactBytes, Ptr Word8)
peekCompactBytes p endPtr = do
  (len, p1) <- peekUVarInt p endPtr
  if len == 0
    then pure (compactBytesNull, p1)
    else do
      (bs, p2) <- peekByteString p1 endPtr (fromIntegral len - 1)
      pure (P.mkCompactBytes bs, p2)


compactBytesNull :: P.CompactBytes
compactBytesNull = P.toCompactBytes (P.KafkaBytes P.Null)


pokeNullableCompactBytes
  :: Ptr Word8 -> P.CompactBytes -> IO (Ptr Word8)
pokeNullableCompactBytes = pokeCompactBytes


peekNullableCompactBytes
  :: Ptr Word8 -> Ptr Word8 -> IO (P.CompactBytes, Ptr Word8)
peekNullableCompactBytes = peekCompactBytes


----------------------------------------------------------------------
-- UUID
----------------------------------------------------------------------

{-# INLINE pokeKafkaUuid #-}
pokeKafkaUuid :: Ptr Word8 -> P.KafkaUuid -> IO (Ptr Word8)
pokeKafkaUuid p uuid = do
  let !bs = LBS.toStrict (UUID.toByteString (P.unKafkaUuid uuid))
  pokeByteString p bs


{-# INLINE peekKafkaUuid #-}
peekKafkaUuid :: Ptr Word8 -> Ptr Word8 -> IO (P.KafkaUuid, Ptr Word8)
peekKafkaUuid p endPtr = do
  (bs, p1) <- peekByteString p endPtr 16
  case UUID.fromByteString (LBS.fromStrict bs) of
    Just u -> pure (P.mkKafkaUuid u, p1)
    Nothing -> throwIO (WireInvalid "invalid 16-byte UUID payload")


----------------------------------------------------------------------
-- Array length prefixes
----------------------------------------------------------------------

{- | Write a non-compact array's Int32 length prefix. The caller is
responsible for poking the elements that follow.
-}
{-# INLINE pokeKafkaArrayLen #-}
pokeKafkaArrayLen :: Ptr Word8 -> Int32 -> IO (Ptr Word8)
pokeKafkaArrayLen = pokeInt32BE


{-# INLINE peekKafkaArrayLen #-}
peekKafkaArrayLen :: Ptr Word8 -> Ptr Word8 -> IO (Int32, Ptr Word8)
peekKafkaArrayLen = peekInt32BE


{- | Compact array length prefix: @len + 1@ (encoded as 'UVarInt'),
with @0@ meaning null.
-}
{-# INLINE pokeCompactArrayLen #-}
pokeCompactArrayLen :: Ptr Word8 -> Int -> IO (Ptr Word8)
pokeCompactArrayLen p n = pokeUVarInt p (fromIntegral n + 1)


{-# INLINE peekCompactArrayLen #-}
peekCompactArrayLen :: Ptr Word8 -> Ptr Word8 -> IO (Int, Ptr Word8)
peekCompactArrayLen p endPtr = do
  (w, p') <- peekUVarInt p endPtr
  pure (fromIntegral w - 1, p')


-- | Non-compact nullable array: @-1@ for null, @n@ for length.
{-# INLINE pokeNullableArrayLen #-}
pokeNullableArrayLen :: Ptr Word8 -> Maybe Int -> IO (Ptr Word8)
pokeNullableArrayLen p Nothing = pokeInt32BE p (-1)
pokeNullableArrayLen p (Just n) = pokeInt32BE p (fromIntegral n)


{-# INLINE peekNullableArrayLen #-}
peekNullableArrayLen
  :: Ptr Word8 -> Ptr Word8 -> IO (Maybe Int, Ptr Word8)
peekNullableArrayLen p endPtr = do
  (n, p') <- peekInt32BE p endPtr
  pure (if n < 0 then Nothing else Just (fromIntegral n), p')


{-# INLINE pokeNullableCompactArrayLen #-}
pokeNullableCompactArrayLen :: Ptr Word8 -> Maybe Int -> IO (Ptr Word8)
pokeNullableCompactArrayLen p Nothing = pokeUVarInt p 0
pokeNullableCompactArrayLen p (Just n) = pokeUVarInt p (fromIntegral n + 1)


{-# INLINE peekNullableCompactArrayLen #-}
peekNullableCompactArrayLen
  :: Ptr Word8 -> Ptr Word8 -> IO (Maybe Int, Ptr Word8)
peekNullableCompactArrayLen p endPtr = do
  (w, p') <- peekUVarInt p endPtr
  pure (if w == 0 then Nothing else Just (fromIntegral w - 1), p')


{- | Worst-case bytes a compact array's length prefix consumes. Used
by 'wireMaxSize' implementations to add 5 bytes per array header.
-}
compactArrayHeaderMaxSize :: Int
compactArrayHeaderMaxSize = 5


----------------------------------------------------------------------
-- Full-array helpers
--
-- The codegen emits these for every array field. Splitting the
-- length prefix from the element loop (the @poke*Len@ helpers above)
-- is still useful for hand-written hot paths that already have the
-- element bytes in hand; the per-array helpers below are what the
-- generated code uses.
----------------------------------------------------------------------

{- | Encode a non-nullable, non-compact 'P.KafkaArray' by writing the
4-byte length prefix and then each element via the supplied
per-element poke. Treats @P.KafkaArray P.Null@ as the empty array
(mirrors the legacy @E.encodeVersionedArray@'s default case).
-}
{-# INLINE pokeKafkaArray #-}
pokeKafkaArray
  :: (Ptr Word8 -> a -> IO (Ptr Word8))
  -> Ptr Word8
  -> P.KafkaArray a
  -> IO (Ptr Word8)
pokeKafkaArray pokeElt p arr = do
  let !v = case P.unKafkaArray arr of
        P.NotNull v' -> v'
        P.Null -> Vector.empty
      !n = Vector.length v
  p1 <- pokeInt32BE p (fromIntegral n)
  Vector.foldM' (\cur x -> pokeElt cur x) p1 v


{- | Decode a non-nullable, non-compact 'P.KafkaArray'. Reads the
4-byte length prefix, then runs the element peek that many times.
-}
peekKafkaArray
  :: (Ptr Word8 -> Ptr Word8 -> IO (a, Ptr Word8))
  -> Ptr Word8
  -> Ptr Word8
  -> IO (P.KafkaArray a, Ptr Word8)
peekKafkaArray peekElt p endPtr = do
  (n, p1) <- peekInt32BE p endPtr
  if n < 0
    then pure (P.mkKafkaArray Vector.empty, p1)
    else do
      (v, p2) <- replicateMVec (fromIntegral n) peekElt p1 endPtr
      pure (P.mkKafkaArray v, p2)


{- | Encode a non-nullable compact 'P.KafkaArray' (UVarInt @len + 1@
prefix). Treats 'P.Null' as length 0 (which the broker reads as
"absent"; non-nullable arrays should never carry 'P.Null' in
practice but we mirror the legacy generator's permissive behaviour).
-}
{-# INLINE pokeCompactArray #-}
pokeCompactArray
  :: (Ptr Word8 -> a -> IO (Ptr Word8))
  -> Ptr Word8
  -> P.KafkaArray a
  -> IO (Ptr Word8)
pokeCompactArray pokeElt p arr = do
  let !v = case P.unKafkaArray arr of
        P.NotNull v' -> v'
        P.Null -> Vector.empty
      !n = Vector.length v
  p1 <- pokeUVarInt p (fromIntegral (n + 1))
  Vector.foldM' (\cur x -> pokeElt cur x) p1 v


peekCompactArray
  :: (Ptr Word8 -> Ptr Word8 -> IO (a, Ptr Word8))
  -> Ptr Word8
  -> Ptr Word8
  -> IO (P.KafkaArray a, Ptr Word8)
peekCompactArray peekElt p endPtr = do
  (lenPlus1, p1) <- peekUVarInt p endPtr
  if lenPlus1 == 0
    then pure (P.mkKafkaArray Vector.empty, p1)
    else do
      let !n = fromIntegral lenPlus1 - 1
      (v, p2) <- replicateMVec n peekElt p1 endPtr
      pure (P.mkKafkaArray v, p2)


{- | Encode a nullable, non-compact 'P.KafkaArray'. 'P.Null' becomes
@-1@ for the length; otherwise the same shape as 'pokeKafkaArray'.
-}
{-# INLINE pokeNullableKafkaArray #-}
pokeNullableKafkaArray
  :: (Ptr Word8 -> a -> IO (Ptr Word8))
  -> Ptr Word8
  -> P.KafkaArray a
  -> IO (Ptr Word8)
pokeNullableKafkaArray pokeElt p arr = case P.unKafkaArray arr of
  P.Null -> pokeInt32BE p (-1)
  P.NotNull v -> do
    let !n = Vector.length v
    p1 <- pokeInt32BE p (fromIntegral n)
    Vector.foldM' (\cur x -> pokeElt cur x) p1 v


peekNullableKafkaArray
  :: (Ptr Word8 -> Ptr Word8 -> IO (a, Ptr Word8))
  -> Ptr Word8
  -> Ptr Word8
  -> IO (P.KafkaArray a, Ptr Word8)
peekNullableKafkaArray peekElt p endPtr = do
  (n, p1) <- peekInt32BE p endPtr
  if n < 0
    then pure (P.KafkaArray P.Null, p1)
    else do
      (v, p2) <- replicateMVec (fromIntegral n) peekElt p1 endPtr
      pure (P.mkKafkaArray v, p2)


{- | Encode a nullable compact 'P.KafkaArray'. 'P.Null' becomes 0;
otherwise the same shape as 'pokeCompactArray'.
-}
{-# INLINE pokeNullableCompactArray #-}
pokeNullableCompactArray
  :: (Ptr Word8 -> a -> IO (Ptr Word8))
  -> Ptr Word8
  -> P.KafkaArray a
  -> IO (Ptr Word8)
pokeNullableCompactArray pokeElt p arr = case P.unKafkaArray arr of
  P.Null -> pokeUVarInt p 0
  P.NotNull v -> do
    let !n = Vector.length v
    p1 <- pokeUVarInt p (fromIntegral (n + 1))
    Vector.foldM' (\cur x -> pokeElt cur x) p1 v


peekNullableCompactArray
  :: (Ptr Word8 -> Ptr Word8 -> IO (a, Ptr Word8))
  -> Ptr Word8
  -> Ptr Word8
  -> IO (P.KafkaArray a, Ptr Word8)
peekNullableCompactArray peekElt p endPtr = do
  (lenPlus1, p1) <- peekUVarInt p endPtr
  if lenPlus1 == 0
    then pure (P.KafkaArray P.Null, p1)
    else do
      let !n = fromIntegral lenPlus1 - 1
      (v, p2) <- replicateMVec n peekElt p1 endPtr
      pure (P.mkKafkaArray v, p2)


----------------------------------------------------------------------
-- Versioned arrays
--
-- Switches between the compact and the non-compact codec at the
-- supplied flexible-version threshold; mirrors the legacy
-- 'Kafka.Protocol.Encoding.encodeVersionedArray' shape. The codegen
-- emits @pokeVersionedArray version threshold elementPoke (accessor msg)@
-- per array field.
----------------------------------------------------------------------

{-# INLINE pokeVersionedArray #-}
pokeVersionedArray
  :: Int
  -- ^ message version
  -> Int
  -- ^ flexible-version threshold
  -> (Ptr Word8 -> a -> IO (Ptr Word8))
  -> Ptr Word8
  -> P.KafkaArray a
  -> IO (Ptr Word8)
pokeVersionedArray version threshold pokeElt p arr
  | version >= threshold = pokeCompactArray pokeElt p arr
  | otherwise = pokeKafkaArray pokeElt p arr


peekVersionedArray
  :: Int
  -> Int
  -> (Ptr Word8 -> Ptr Word8 -> IO (a, Ptr Word8))
  -> Ptr Word8
  -> Ptr Word8
  -> IO (P.KafkaArray a, Ptr Word8)
peekVersionedArray version threshold peekElt p endPtr
  | version >= threshold = peekCompactArray peekElt p endPtr
  | otherwise = peekKafkaArray peekElt p endPtr


{-# INLINE pokeVersionedNullableArray #-}
pokeVersionedNullableArray
  :: Int
  -> Int
  -> (Ptr Word8 -> a -> IO (Ptr Word8))
  -> Ptr Word8
  -> P.KafkaArray a
  -> IO (Ptr Word8)
pokeVersionedNullableArray version threshold pokeElt p arr
  | version >= threshold = pokeNullableCompactArray pokeElt p arr
  | otherwise = pokeNullableKafkaArray pokeElt p arr


peekVersionedNullableArray
  :: Int
  -> Int
  -> (Ptr Word8 -> Ptr Word8 -> IO (a, Ptr Word8))
  -> Ptr Word8
  -> Ptr Word8
  -> IO (P.KafkaArray a, Ptr Word8)
peekVersionedNullableArray version threshold peekElt p endPtr
  | version >= threshold = peekNullableCompactArray peekElt p endPtr
  | otherwise = peekNullableKafkaArray peekElt p endPtr


{- | Run @peekElt@ exactly @n@ times. Builds a strict 'V.Vector' from
the results without going through an intermediate list (which
would force the spine + thunk every cell).
-}
replicateMVec
  :: Int
  -> (Ptr Word8 -> Ptr Word8 -> IO (a, Ptr Word8))
  -> Ptr Word8
  -> Ptr Word8
  -> IO (Vector.Vector a, Ptr Word8)
replicateMVec n peekElt p endPtr
  -- Every element takes at least one byte, so a count beyond the
  -- bytes left is corrupt. Refuse it before allocating: a vector
  -- of a garbage length (up to 2^31 boxed slots) is one
  -- uninterruptible primop that stalls every thread at the next GC.
  | n > endPtr `minusPtr` p = throwIO (WireTruncated ("array of " <> show n <> " elements in " <> show (endPtr `minusPtr` p) <> " bytes"))
  | otherwise = do
  -- Pre-allocate a mutable vector to avoid the @[a] -> Vector a@
  -- copy that the legacy 'Vector.replicateM' would have done. The
  -- threading of the cursor is sequential so we walk in-place.
  mv <- VM.new n
  finalCur <- loop 0 p mv
  v <- Vector.unsafeFreeze mv
  pure (v, finalCur)
  where
    loop !i !cur !mv
      | i == n = pure cur
      | otherwise = do
          (x, cur') <- peekElt cur endPtr
          VM.write mv i x
          loop (i + 1) cur' mv


----------------------------------------------------------------------
-- Tagged-field entries (KIP-866-style payloads)
----------------------------------------------------------------------

{- | Write a list of @(tag, payloadBytes)@ entries in the wire format
the @TaggedFields@ envelope expects: a UVarInt count followed by
per-entry @UVarInt tag, UVarInt size, bytes@.

Used by codegen-emitted Wire pokes for messages that carry tagged
fields with payloads (e.g. KIP-866 @NodeEndpoints@ on
@ProduceResponse v10+@).
-}
pokeTaggedFieldEntries
  :: Ptr Word8
  -> [(Word32, BS.ByteString)]
  -> IO (Ptr Word8)
pokeTaggedFieldEntries p entries = do
  let !n = length entries
  p1 <- pokeUVarInt p (fromIntegral n)
  go p1 entries
  where
    go !cur [] = pure cur
    go !cur ((tag, payload) : rs) = do
      cur1 <- pokeUVarInt cur tag
      cur2 <- pokeUVarInt cur1 (fromIntegral (BS.length payload))
      cur3 <- pokeByteString cur2 payload
      go cur3 rs


----------------------------------------------------------------------
-- Tagged fields
----------------------------------------------------------------------

{- | Write the empty-tagged-fields marker (a single 0 byte). The
code generator emits this at every flexible message boundary.
-}
{-# INLINE pokeEmptyTaggedFields #-}
pokeEmptyTaggedFields :: Ptr Word8 -> IO (Ptr Word8)
pokeEmptyTaggedFields p = pokeUVarInt p 0


{- | Read a tagged-fields header and return the per-tag count. The
code generator's per-message decoder either skips that many
(tag, size, bytes) triples or routes them into the message's
'TaggedFields' map; this helper just gives the count.
-}
{-# INLINE peekTaggedFieldsCount #-}
peekTaggedFieldsCount :: Ptr Word8 -> Ptr Word8 -> IO (Int, Ptr Word8)
peekTaggedFieldsCount p endPtr = do
  (n, p') <- peekUVarInt p endPtr
  pure (fromIntegral n, p')


{- | Skip a single @(tag, size, bytes)@ tagged-field triple. Reads
the tag UVarInt, the size UVarInt, then advances the cursor past
@size@ bytes. Used by 'skipTaggedFieldsBody'.
-}
{-# INLINE skipOneTaggedField #-}
skipOneTaggedField :: Ptr Word8 -> Ptr Word8 -> IO (Ptr Word8)
skipOneTaggedField p endPtr = do
  (_tag, p1) <- peekUVarInt p endPtr
  (sz, p2) <- peekUVarInt p1 endPtr
  let !n = fromIntegral sz
  ensureBytes p2 endPtr n "tagged-field payload"
  pure (p2 `plusPtr` n)


{- | Skip @n@ tagged-field entries. The caller is responsible for
having already consumed the leading UVarInt count; this just
walks the per-entry triples.
-}
skipTaggedFieldsBody :: Int -> Ptr Word8 -> Ptr Word8 -> IO (Ptr Word8)
skipTaggedFieldsBody = go
  where
    go !n !p !endPtr
      | n <= 0 = pure p
      | otherwise = do
          p' <- skipOneTaggedField p endPtr
          go (n - 1) p' endPtr


{- | Combined: read the count + skip every entry, returning just the
cursor past the last byte consumed. The codegen-emitted Wire peek
uses this for messages that don't surface tagged fields to the
caller (i.e. discards the body, like the legacy
@_ <- (deserialize :: m TaggedFields)@ pattern).
-}
{-# INLINE peekAndSkipTaggedFields #-}
peekAndSkipTaggedFields :: Ptr Word8 -> Ptr Word8 -> IO (Ptr Word8)
peekAndSkipTaggedFields p endPtr = do
  (n, p') <- peekTaggedFieldsCount p endPtr
  skipTaggedFieldsBody n p' endPtr


{- | Read a tagged-fields envelope into a @Map Word32 ByteString@
so the codegen-emitted Wire peek can dispatch each known tag to
a per-field decoder. Used by messages that surface tagged fields
to the caller (KIP-866-style payloads). The payload bytes are
copied (not sliced) so the caller doesn't need to keep the source
'ForeignPtr' alive for the lifetime of the returned map.
-}
peekTaggedFieldsMap
  :: Ptr Word8
  -> Ptr Word8
  -> IO (Map.Map Word32 BS.ByteString, Ptr Word8)
peekTaggedFieldsMap p endPtr = do
  (n, p') <- peekTaggedFieldsCount p endPtr
  go n p' Map.empty
  where
    go !k !cur !acc
      | k <= 0 = pure (acc, cur)
      | otherwise = do
          (tag, cur1) <- peekUVarInt cur endPtr
          (sz, cur2) <- peekUVarInt cur1 endPtr
          let !nbytes = fromIntegral sz
          ensureBytes cur2 endPtr nbytes "tagged-field payload"
          payload <- BSI.create nbytes $ \dst ->
            Foreign.Marshal.Utils.copyBytes dst cur2 nbytes
          go (k - 1) (cur2 `plusPtr` nbytes) (Map.insert tag payload acc)


----------------------------------------------------------------------
-- Wire instances
----------------------------------------------------------------------

instance Wire P.KafkaString where
  wireMaxSize = kafkaStringMaxSize
  {-# INLINE wireMaxSize #-}
  wirePoke = pokeKafkaString
  {-# INLINE wirePoke #-}
  wirePeek = peekKafkaString
  {-# INLINE wirePeek #-}


instance Wire P.CompactString where
  wireMaxSize = compactStringMaxSize
  {-# INLINE wireMaxSize #-}
  wirePoke = pokeCompactString
  {-# INLINE wirePoke #-}
  wirePeek = peekCompactString
  {-# INLINE wirePeek #-}


instance Wire P.KafkaBytes where
  wireMaxSize = kafkaBytesMaxSize
  {-# INLINE wireMaxSize #-}
  wirePoke = pokeKafkaBytes
  {-# INLINE wirePoke #-}
  wirePeek = peekKafkaBytes
  {-# INLINE wirePeek #-}


instance Wire P.CompactBytes where
  wireMaxSize = compactBytesMaxSize
  {-# INLINE wireMaxSize #-}
  wirePoke = pokeCompactBytes
  {-# INLINE wirePoke #-}
  wirePeek = peekCompactBytes
  {-# INLINE wirePeek #-}


instance Wire P.KafkaUuid where
  wireMaxSize _ = 16
  {-# INLINE wireMaxSize #-}
  wirePoke = pokeKafkaUuid
  {-# INLINE wirePoke #-}
  wirePeek = peekKafkaUuid
  {-# INLINE wirePeek #-}


instance Wire P.VarInt where
  wireMaxSize _ = 5
  {-# INLINE wireMaxSize #-}
  wirePoke p (P.VarInt i) = pokeVarInt p i
  {-# INLINE wirePoke #-}
  wirePeek p endPtr = do
    (i, p') <- peekVarInt p endPtr
    pure (P.VarInt i, p')
  {-# INLINE wirePeek #-}


instance Wire P.VarLong where
  wireMaxSize _ = 10
  {-# INLINE wireMaxSize #-}
  wirePoke p (P.VarLong i) = pokeVarLong p i
  {-# INLINE wirePoke #-}
  wirePeek p endPtr = do
    (i, p') <- peekVarLong p endPtr
    pure (P.VarLong i, p')
  {-# INLINE wirePeek #-}


instance Wire P.UVarInt where
  wireMaxSize _ = 5
  {-# INLINE wireMaxSize #-}
  wirePoke p (P.UVarInt w) = pokeUVarInt p w
  {-# INLINE wirePoke #-}
  wirePeek p endPtr = do
    (w, p') <- peekUVarInt p endPtr
    pure (P.UVarInt w, p')
  {-# INLINE wirePeek #-}
