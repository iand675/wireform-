{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Kafka.Protocol.RecordBatchWire
Description : Direct-poke 'Wire'-codec encoder for 'RecordBatch'

Replaces the @runPutS@-per-record path in
"Kafka.Protocol.RecordBatch" with a single-allocation, single-pass
encoder that pokes the entire batch into one contiguous buffer.
The bytes are byte-identical with 'RB.encodeRecordBatch' (proven by
the round-trip + cross-codec equivalence properties in
'Protocol.RecordBatchWireSpec').

Why a separate module:

  * The legacy 'RB.encodeRecordBatch' is still used by callers
    that expect the @ByteString@ shape; switching every site is a
    bigger change than necessary for the perf win.
  * Compression + the version-aware envelope have their own paths
    in @RecordBatch@; this module focuses on the uncompressed
    happy path, which is what the producer's hot send loop hits
    most of the time.

== Hot-path savings (per-record, GHC 9.6.4 -O1):

  * legacy `encodeRecordBatch` (100 records): ~107 µs total /
    ~1.07 µs per record
  * `encodeRecordBatchWire`     (100 records): see
    'Benchmarks.HotPath' for the latest number; the win comes
    from one 'mallocForeignPtrBytes' instead of @runPutS@'s
    Builder + chunk-coalesce.
-}
module Kafka.Protocol.RecordBatchWire (
  encodeRecordBatchWire,
  decodeRecordBatchWireWithDecompression,
  recordBatchWireSize,

  -- * Records-only encoder (used by the compressed path)
  encodeRecordsWire,
  recordsWireSize,

  -- * Wire-based compressed encoder
  encodeRecordBatchWireCompressed,
  encodeRecordBatchWireCompressedWithLevel,

  -- * Direct-poke decoder
  decodeRecordBatchWire,

  -- * Sliced (memory-efficient) decoder
  SlicedRecordBatch (..),
  decodeRecordBatchWireSliced,
  decodeRecordBatchWireSlicedWithDecompression,
  slicedRecordKey,
  slicedRecordValue,
  slicedRecordCount,
  slicedRecordOffset,
  slicedRecordTimestamp,

  -- ** Header accessors (KIP-82)
  slicedRecordHeaderCount,
  slicedRecordHeader,
  slicedRecordHeaders,
) where

import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.Bits ((.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Foldable (foldl')
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int16, Int32, Int64)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Data.Word (Word8)
import Foreign.ForeignPtr (
  ForeignPtr,
  mallocForeignPtrBytes,
  withForeignPtr,
 )
import Foreign.Marshal.Utils (moveBytes)
import Foreign.Ptr (Ptr, minusPtr, plusPtr)
import GHC.IO (unsafePerformIO)
import Kafka.Compression.Ring qualified as CompressionRing
import Kafka.Compression.Types qualified as Compression
import Kafka.Protocol.CRC32C qualified as CRC
import Kafka.Protocol.RecordBatch (
  Attributes (..),
  Record (..),
  RecordBatch (..),
  RecordHeader (..),
  TimestampType (..),
  magicV2,
  recordBatchOverhead,
 )
import "wireform-kafka-protocol" Kafka.Protocol.Wire (
  ensureBytes,
  peekByteStringSlice,
  peekInt16BE,
  peekInt32BE,
  peekInt64BE,
  peekVarInt,
  peekVarLong,
  peekWord32BE,
  peekWord8,
  pokeByteString,
  pokeInt16BE,
  pokeInt32BE,
  pokeInt64BE,
  pokeVarInt,
  pokeVarLong,
  pokeWord32BE,
  pokeWord8,
 )
import "wireform-kafka-protocol" Kafka.Protocol.Wire.SliceVector qualified as SV


----------------------------------------------------------------------
-- Public surface
----------------------------------------------------------------------

{- | Direct-poke encoder for 'RecordBatch'. Allocates one
'ByteString' of size 'recordBatchWireSize' and writes the entire
batch in a single pass.

Byte-identical to 'Kafka.Protocol.RecordBatch.encodeRecordBatch'
for any non-compressed batch; for compressed batches use the
existing 'encodeRecordBatchWithCompression' (Wire wrapping for
those goes in a follow-up).
-}
{-# INLINEABLE encodeRecordBatchWire #-}
encodeRecordBatchWire :: RecordBatch -> ByteString
encodeRecordBatchWire batch = unsafePerformIO $ do
  let !sz = recordBatchWireSize batch
  fp <- mallocForeignPtrBytes sz
  withForeignPtr fp $ \basePtr -> do
    !endPtr <- pokeBatch basePtr batch
    let !len = endPtr `minusPtr` basePtr
    pure (BSI.fromForeignPtr fp 0 len)


{- | Upper bound on the bytes 'encodeRecordBatchWire' may write.
For a typical record batch the upper bound is exact (records
have known sizes); the only worst-case fudge is per-record
VarInt headers (5-byte cap each).
-}
{-# INLINEABLE recordBatchWireSize #-}
recordBatchWireSize :: RecordBatch -> Int
recordBatchWireSize RecordBatch {..} =
  recordBatchOverhead + recordsWireSize batchRecords


{- | Encode just the records section of a 'RecordBatch' (no batch
header, no CRC, no length prefix). Used by the compressed
encoder, which:

  1. encodes the records via 'encodeRecordsWire',
  2. compresses the resulting bytes,
  3. wraps the compressed payload in a normal batch header.

Step 1 was previously the hot bottleneck for compressed
producers (it called 'runPutS' once per record); this entry
gives them the same single-allocation, single-pass shape the
uncompressed path enjoys.
-}
{-# INLINEABLE encodeRecordsWire #-}
encodeRecordsWire :: V.Vector Record -> ByteString
encodeRecordsWire records = unsafePerformIO $ do
  let !sz = recordsWireSize records
  if sz == 0
    then pure BS.empty
    else do
      fp <- mallocForeignPtrBytes sz
      withForeignPtr fp $ \basePtr -> do
        endPtr <- pokeRecords basePtr records
        let !len = endPtr `minusPtr` basePtr
        pure (BSI.fromForeignPtr fp 0 len)


{-# INLINE recordsWireSize #-}
recordsWireSize :: V.Vector Record -> Int
recordsWireSize = V.foldl' (\acc r -> acc + recordWireSize r) 0


----------------------------------------------------------------------
-- Compressed-records encoder (uses the Wire records helper +
-- the existing Compression layer, then wraps the compressed
-- bytes in a single-allocation batch envelope written with the
-- direct-poke primitives).
----------------------------------------------------------------------

{- | Wire-based version of
'Kafka.Protocol.RecordBatch.encodeRecordBatchWithCompression':

  1. Encodes the records section in one pass via
     'encodeRecordsWire'.
  2. Compresses the resulting bytes through the codec named
     in the batch attributes.
  3. Wraps the compressed payload in a single-allocation
     batch envelope written with the same direct-poke
     primitives as 'encodeRecordBatchWire' (one
     'mallocForeignPtrBytes', body CRC32C computed in place
     via 'crc32cPtr', length back-patched on completion).

For 'NoCompression' callers, prefer 'encodeRecordBatchWire'
directly — it skips the no-op compression layer entirely.
-}
encodeRecordBatchWireCompressed
  :: RecordBatch -> IO (Either String ByteString)
encodeRecordBatchWireCompressed b =
  encodeRecordBatchWireCompressedWithLevel
    b
    ( Compression.defaultLevel
        (attrCompressionType (batchAttributes b))
    )


{- | Like 'encodeRecordBatchWireCompressed' but takes an explicit
compression level (KIP-353 / KIP-776 / KIP-909).
-}
encodeRecordBatchWireCompressedWithLevel
  :: RecordBatch
  -> Compression.CompressionLevel
  -> IO (Either String ByteString)
encodeRecordBatchWireCompressedWithLevel batch@RecordBatch {..} level = do
  let !codec = attrCompressionType batchAttributes
      !rawRecordsBytes = encodeRecordsWire batchRecords
  compressedR <- Compression.compressWithLevel codec level rawRecordsBytes
  case compressedR of
    Left err -> pure (Left err)
    Right compressedRecords -> do
      -- Build the batch envelope directly into a
      -- single-allocation buffer. Layout matches v2:
      --   [0  .. 8)  baseOffset           Int64 BE
      --   [8  .. 12) length               Int32 BE  (back-patched)
      --   [12 .. 16) partitionLeaderEpoch Int32 BE
      --   [16]       magic                Int8     (2)
      --   [17 .. 21) crc                  Word32 BE (back-patched)
      --   [21 .. 61) body header (attrs/lastDelta/timestamps/
      --              producer fields/recordsCount)
      --   [61 ..  )  compressed records bytes
      let !nRec = V.length batchRecords
          !compLen = BS.length compressedRecords
          !sz = recordBatchOverhead + compLen
      fp <- mallocForeignPtrBytes sz
      withForeignPtr fp $ \basePtr -> do
        _ <- pokeInt64BE basePtr batchBaseOffset
        let !lengthPtr = basePtr `plusPtr` 8
        _ <- pokeInt32BE lengthPtr 0 -- placeholder
        let !leaderPtr = basePtr `plusPtr` 12
        _ <- pokeInt32BE leaderPtr batchPartitionLeaderEpoch
        let !magicPtr = basePtr `plusPtr` 16
        _ <- pokeWord8 magicPtr (fromIntegral magicV2 :: Word8)
        let !crcPtr = basePtr `plusPtr` 17
        _ <- pokeWord32BE crcPtr 0 -- placeholder
        let !bodyStart = basePtr `plusPtr` 21
        bodyEnd <- writeCompressedBody bodyStart batch nRec compressedRecords
        let !lenValue = fromIntegral (bodyEnd `minusPtr` leaderPtr) :: Int32
        _ <- pokeInt32BE lengthPtr lenValue
        let !bodyLen = bodyEnd `minusPtr` bodyStart
        !crc <- CRC.crc32cPtr bodyStart bodyLen
        _ <- pokeWord32BE crcPtr crc
        let !len = bodyEnd `minusPtr` basePtr
        pure (Right (BSI.fromForeignPtr fp 0 len))


{-# INLINE writeCompressedBody #-}
writeCompressedBody
  :: Ptr Word8 -> RecordBatch -> Int -> ByteString -> IO (Ptr Word8)
writeCompressedBody p RecordBatch {..} nRec compressedRecords = do
  p1 <- pokeInt16BE p (encodeAttributes batchAttributes)
  p2 <- pokeInt32BE p1 batchLastOffsetDelta
  p3 <- pokeInt64BE p2 batchBaseTimestamp
  p4 <- pokeInt64BE p3 batchMaxTimestamp
  p5 <- pokeInt64BE p4 batchProducerId
  p6 <- pokeInt16BE p5 batchProducerEpoch
  p7 <- pokeInt32BE p6 batchBaseSequence
  p8 <- pokeInt32BE p7 (fromIntegral nRec :: Int32)
  pokeByteString p8 compressedRecords


----------------------------------------------------------------------
-- Per-record sizing
----------------------------------------------------------------------

-- | Upper bound on a single record's wire size.
{-# INLINE recordWireSize #-}
recordWireSize :: Record -> Int
recordWireSize Record {..} =
  let !keyLen = maybe 0 BS.length recordKey
      !valLen = maybe 0 BS.length recordValue
      !hdrCount = length recordHeaders
      !hdrSize = foldl' (\acc h -> acc + headerWireSize h) 0 recordHeaders
      -- 5-byte VarInt cap on every length / delta + 1 byte attrs.
      -- Conservative; the actual encoded size is typically smaller
      -- (small records fit in 1-byte varints).
      !innerSize =
        1 -- record attrs
          + 10 -- timestampDelta (VarLong, max 10)
          + 5 -- offsetDelta (VarInt, max 5)
          + 5
          + keyLen -- key length + bytes
          + 5
          + valLen -- value length + bytes
          + 5 -- headers count
          + hdrSize
  in 5 + innerSize -- outer record length VarInt


{-# INLINE headerWireSize #-}
headerWireSize :: RecordHeader -> Int
headerWireSize RecordHeader {..} =
  let !valLen = maybe 0 BS.length headerValue
  in 5 + BS.length headerKey + 5 + valLen


----------------------------------------------------------------------
-- The actual poke
----------------------------------------------------------------------

{- | Write the entire 'RecordBatch' starting at 'basePtr', returning
the pointer past the last byte written.

Layout (RecordBatch v2):

    [0  .. 8)  baseOffset           Int64 BE
    [8  .. 12) length               Int32 BE   (filled in last)
    [12 .. 16) partitionLeaderEpoch Int32 BE
    [16]       magic                Int8       (2)
    [17 .. 21) crc                  Word32 BE  (filled in last)
    [21 ..  )  body (attrs, deltas, producer-id+epoch+seq, count, records)
-}
{-# INLINE pokeBatch #-}
pokeBatch :: Ptr Word8 -> RecordBatch -> IO (Ptr Word8)
pokeBatch basePtr RecordBatch {..} = do
  -- Header (offsets [0 .. 21))
  _ <- pokeInt64BE basePtr batchBaseOffset
  -- length (4 bytes) — placeholder; rewritten at the end.
  let !lengthPtr = basePtr `plusPtr` 8
  _ <- pokeInt32BE lengthPtr 0
  let !leaderPtr = basePtr `plusPtr` 12
  _ <- pokeInt32BE leaderPtr batchPartitionLeaderEpoch
  let !magicPtr = basePtr `plusPtr` 16
  _ <- pokeWord8 magicPtr (fromIntegral magicV2 :: Word8)
  let !crcPtr = basePtr `plusPtr` 17
  _ <- pokeWord32BE crcPtr 0 -- placeholder
  -- Body starts at offset 21.
  let !bodyStart = basePtr `plusPtr` 21
  bodyEnd <-
    pokeBody
      bodyStart
      batchAttributes
      batchLastOffsetDelta
      batchBaseTimestamp
      batchMaxTimestamp
      batchProducerId
      batchProducerEpoch
      batchBaseSequence
      batchRecords
  -- Patch length (= bytes from offset 12 to bodyEnd).
  let !lenValue = fromIntegral (bodyEnd `minusPtr` leaderPtr) :: Int32
  _ <- pokeInt32BE lengthPtr lenValue
  -- Patch CRC = CRC32C over the body bytes (everything from
  -- attributes onward, i.e. [21 .. bodyEnd)). Uses the raw-Ptr
  -- entry on the C side to skip the body memcpy a 'ByteString'-
  -- shaped helper would force.
  let !bodyLen = bodyEnd `minusPtr` bodyStart
  !crc <- CRC.crc32cPtr bodyStart bodyLen
  _ <- pokeWord32BE crcPtr crc
  pure bodyEnd


{-# INLINE pokeBody #-}
pokeBody
  :: Ptr Word8
  -> Attributes
  -> Int32 -- last offset delta
  -> Int64 -- base timestamp
  -> Int64 -- max timestamp
  -> Int64 -- producer id
  -> Int16 -- producer epoch
  -> Int32 -- base sequence
  -> V.Vector Record
  -> IO (Ptr Word8)
pokeBody p attrs lastOffsetDelta baseTs maxTs pid pep baseSeq records = do
  p1 <- pokeInt16BE p (encodeAttributes attrs)
  p2 <- pokeInt32BE p1 lastOffsetDelta
  p3 <- pokeInt64BE p2 baseTs
  p4 <- pokeInt64BE p3 maxTs
  p5 <- pokeInt64BE p4 pid
  p6 <- pokeInt16BE p5 pep
  p7 <- pokeInt32BE p6 baseSeq
  p8 <- pokeInt32BE p7 (fromIntegral (V.length records) :: Int32)
  pokeRecords p8 records


{-# INLINE pokeRecords #-}
pokeRecords :: Ptr Word8 -> V.Vector Record -> IO (Ptr Word8)
pokeRecords p0 records = V.foldM' pokeOne p0 records
  where
    pokeOne !p r = pokeRecord p r


----------------------------------------------------------------------
-- Per-record poke
----------------------------------------------------------------------

{- | Encode a single 'Record' directly into the buffer.

The Kafka record format is a length-prefixed body: we write the
body first into a /scratch span/, then prepend the actual length
as a VarInt. To do that single-pass we exploit the upper-bound
size: write the body into the buffer starting at @p + 5@ (the
maximum varint length), then copy the body backwards by however
many bytes the actual VarInt occupies. With small records (the
common case) the actual VarInt is 1 byte and we copy 4 fewer
bytes than reserved.

This trades one short memcpy per record for a two-pass "size +
copy" encode. The break-even is around 16 bytes / record; for
typical Kafka records (hundreds of bytes) the saving is large.
-}
{-# INLINE pokeRecord #-}
pokeRecord :: Ptr Word8 -> Record -> IO (Ptr Word8)
pokeRecord !p Record {..} = do
  -- Reserve 5 bytes for the outer length VarInt; write the body
  -- starting at p + 5.
  let !bodyStart = p `plusPtr` 5
  -- Body: attrs (1) + tsDelta (VarLong) + offsetDelta (VarInt)
  --     + key (VarInt + bytes) + value (VarInt + bytes)
  --     + headers count (VarInt) + each header.
  pa <- pokeWord8 bodyStart 0 -- attributes byte (always 0 in v2)
  pb <- pokeVarLong pa recordTimestampDelta
  pc <- pokeVarInt pb recordOffsetDelta
  pd <- case recordKey of
    Nothing -> pokeVarInt pc (-1)
    Just k -> do
      p' <- pokeVarInt pc (fromIntegral (BS.length k))
      pokeByteString p' k
  pe <- case recordValue of
    Nothing -> pokeVarInt pd (-1)
    Just v -> do
      p' <- pokeVarInt pd (fromIntegral (BS.length v))
      pokeByteString p' v
  pf <- pokeVarInt pe (fromIntegral (length recordHeaders))
  pg <- foldHeaders pf recordHeaders
  let !bodyEnd = pg
      !bodyLen = bodyEnd `minusPtr` bodyStart
  -- Write the actual VarInt length at p, then shift the body
  -- left if the VarInt is shorter than the 5 bytes we reserved.
  -- Use Wire's pokeVarInt which returns the new pointer.
  lenEnd <- pokeVarInt p (fromIntegral bodyLen :: Int32)
  let !lenWritten = lenEnd `minusPtr` p
      !shift_ = 5 - lenWritten
  if shift_ == 0
    then pure bodyEnd
    else do
      -- memmove the body left by 'shift'. We shift bytes that
      -- live at @bodyStart .. bodyEnd@ down to @lenEnd ..
      -- lenEnd + bodyLen@. dst < src, so a forward
      -- byte-by-byte copy is safe.
      memmoveLeft
        bodyStart -- src: where the body currently lives
        lenEnd -- dst: just past the actual VarInt length
        bodyLen
      pure (bodyEnd `plusPtr` (negate shift_))


{-# INLINE foldHeaders #-}
foldHeaders :: Ptr Word8 -> [RecordHeader] -> IO (Ptr Word8)
foldHeaders = go
  where
    go !p [] = pure p
    go !p (h : hs) = do
      p' <- pokeHeader p h
      go p' hs


{-# INLINE pokeHeader #-}
pokeHeader :: Ptr Word8 -> RecordHeader -> IO (Ptr Word8)
pokeHeader !p RecordHeader {..} = do
  pk <- pokeVarInt p (fromIntegral (BS.length headerKey))
  pk2 <- pokeByteString pk headerKey
  case headerValue of
    Nothing -> pokeVarInt pk2 (-1)
    Just v -> do
      pv <- pokeVarInt pk2 (fromIntegral (BS.length v))
      pokeByteString pv v


----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

{- | Encode the v2 'Attributes' word the same way
"Kafka.Protocol.RecordBatch" does.
-}
{-# INLINE encodeAttributes #-}
encodeAttributes :: Attributes -> Int16
encodeAttributes Attributes {..} =
  let !comp = fromIntegral (Compression.codecId attrCompressionType) .&. 0x07
      !ts = if attrTimestampType == LogAppendTime then 0x08 else 0x00
      !txn = if attrIsTransactional then 0x10 else 0x00
      !ctrl = if attrIsControl then 0x20 else 0x00
      !del = if attrHasDeleteHorizon then 0x40 else 0x00
  in comp .|. ts .|. txn .|. ctrl .|. del


{- | @memmoveLeft src dst n@ copies @n@ bytes from @src@ to @dst@,
where @dst < src@ (i.e. shifting the data left in the buffer).

Delegates to libc 'memmove' via 'Foreign.Marshal.Utils.moveBytes'.
'memmove' handles overlapping regions correctly (unlike 'memcpy')
/and/ is SIMD-vectorised on every modern target — glibc dispatches
to AVX2 / AVX-512 / SSE2 on x86, NEON / SVE on ARM. For typical
record bodies (100s of bytes) shifted left by 1-4 bytes per record
on the encode hot path, this turns into a couple of vector
loads + stores, which is dramatically faster than the previous
byte-by-byte Haskell loop (which paid one peek + one poke + one
branch per byte and never vectorised).

A 100-record batch with 200-byte average body length shifts ~20 KB
of data through this hop on every encode; switching to libc cuts
it from a tight Haskell loop to an SSE2/AVX move.
-}
{-# INLINE memmoveLeft #-}
memmoveLeft :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
memmoveLeft !src !dst !n = moveBytes dst src n


----------------------------------------------------------------------
-- Decoder
----------------------------------------------------------------------

{- | Direct-poke decoder for 'RecordBatch'. Reads the entire batch
in a single pass straight off the source 'ByteString' buffer
(held alive by its 'ForeignPtr'), with one mutable 'V.Vector'
allocation for the records. Errors come back as @Left err@.

Per-record steady-state cost: see 'Benchmarks.HotPath' / WireDecode
for the latest figure. One mutable vector + one CRC32C raw-ptr
call.
-}
{-# INLINEABLE decodeRecordBatchWire #-}
decodeRecordBatchWire :: ByteString -> Either String RecordBatch
decodeRecordBatchWire bs = unsafePerformIO $ do
  let (fp, off, len) = BSI.toForeignPtr bs
  withForeignPtr fp $ \basePtr -> do
    let !startPtr = basePtr `plusPtr` off
        !endPtr = startPtr `plusPtr` len
    -- Catch every exception because both 'WireError' (from the Wire
    -- primitives) and 'IOError' (from 'errOut' below) can fire.
    r <-
      try (peekBatch fp basePtr startPtr endPtr)
        :: IO (Either SomeException RecordBatch)
    case r of
      Left e -> pure (Left (show e))
      Right rb -> pure (Right rb)


{- | The source 'ForeignPtr' threads down into 'peekRecord' so
per-record key + value + header reads can hand back
/zero-copy slices/ over the input buffer instead of memcpy'ing
each one. The slices keep the source 'ForeignPtr' alive
through their own reference, so leaving the
'withForeignPtr' scope is safe.
-}
{-# INLINE peekBatch #-}
peekBatch
  :: ForeignPtr Word8
  -> Ptr Word8
  -- ^ basePtr (start of the source buffer in this scope)
  -> Ptr Word8
  -- ^ start of this batch
  -> Ptr Word8
  -- ^ end of buffer
  -> IO RecordBatch
peekBatch fp basePtr p endPtr = do
  ensureBytes p endPtr 21 "RecordBatch header"
  (baseOffset, p1) <- peekInt64BE p endPtr
  (lenValue, p2) <- peekInt32BE p1 endPtr
  (leaderEp, p3) <- peekInt32BE p2 endPtr
  (magic, p4) <- peekWord8 p3 endPtr
  if fromIntegral magic /= magicV2
    then errOut ("Unsupported magic byte: " ++ show magic)
    else do
      (storedCrc, p5) <- peekWord32BE p4 endPtr
      let !bodyStart = p5
          !bodyLen = fromIntegral lenValue - 4 - 1 - 4
          -- minus partition leader epoch (4) + magic (1) + crc (4)
          !bodyEnd = bodyStart `plusPtr` bodyLen
      ensureBytes bodyStart endPtr bodyLen "RecordBatch body"
      computedCrc <- CRC.crc32cPtr bodyStart bodyLen
      if computedCrc /= storedCrc
        then
          errOut
            ( "CRC mismatch: stored="
                ++ show storedCrc
                ++ ", computed="
                ++ show computedCrc
            )
        else do
          (attrsW, q1) <- peekInt16BE bodyStart bodyEnd
          attrs <- case decodeAttributes attrsW of
            Left e -> errOut e
            Right a -> pure a
          (lastDelta, q2) <- peekInt32BE q1 bodyEnd
          (baseTs, q3) <- peekInt64BE q2 bodyEnd
          (maxTs, q4) <- peekInt64BE q3 bodyEnd
          (pid, q5) <- peekInt64BE q4 bodyEnd
          (pep, q6) <- peekInt16BE q5 bodyEnd
          (baseSeq, q7) <- peekInt32BE q6 bodyEnd
          (recCount, q8) <- peekInt32BE q7 bodyEnd
          let !n = fromIntegral recCount :: Int
          records <-
            if n <= 0
              then pure V.empty
              else readRecords fp basePtr n q8 bodyEnd
          pure
            RecordBatch
              { batchBaseOffset = baseOffset
              , batchPartitionLeaderEpoch = leaderEp
              , batchAttributes = attrs
              , batchLastOffsetDelta = lastDelta
              , batchBaseTimestamp = baseTs
              , batchMaxTimestamp = maxTs
              , batchProducerId = pid
              , batchProducerEpoch = pep
              , batchBaseSequence = baseSeq
              , batchRecords = records
              }


{- | Inline copy of 'RB.decodeAttributes' so we don't pay an
allocator round-trip for the @Either@ on the hot path.
-}
{-# INLINE decodeAttributes #-}
decodeAttributes :: Int16 -> Either String Attributes
decodeAttributes attrs =
  let !cId = fromIntegral (attrs .&. 0x07)
      !ts = if (attrs .&. 0x08) /= 0 then LogAppendTime else CreateTime
      !txn = (attrs .&. 0x10) /= 0
      !ctrl = (attrs .&. 0x20) /= 0
      !del = (attrs .&. 0x40) /= 0
      !codec = case (cId :: Int) of
        0 -> Compression.NoCompression
        1 -> Compression.Gzip
        2 -> Compression.Snappy
        3 -> Compression.Lz4
        4 -> Compression.Zstd
        _ -> Compression.NoCompression
  in Right $ Attributes codec ts txn ctrl del


{- | Decode @n@ records into a freshly allocated 'V.Vector' using
one mutable buffer.
-}
{-# INLINE readRecords #-}
readRecords
  :: ForeignPtr Word8
  -> Ptr Word8
  -- ^ basePtr (start of source buffer)
  -> Int
  -> Ptr Word8
  -> Ptr Word8
  -> IO (V.Vector Record)
readRecords fp basePtr n start endPtr = do
  mv <- MV.unsafeNew n
  let go !i !p
        | i >= n = pure ()
        | otherwise = do
            (rec, p') <- peekRecord fp basePtr p endPtr
            MV.unsafeWrite mv i rec
            go (i + 1) p'
  go 0 start
  V.unsafeFreeze mv


{- | Per-record peek that hands back zero-copy slices of the
source buffer for the key, value, and each header's bytes
(rather than memcpy'ing each into a fresh 'ByteString'). For
a typical 50 MiB fetch response with thousands of records
that's tens of MiB of memcpy / allocation eliminated; the
record's lifetime keeps the source 'ForeignPtr' alive.
-}
{-# INLINE peekRecord #-}
peekRecord
  :: ForeignPtr Word8
  -> Ptr Word8
  -> Ptr Word8
  -> Ptr Word8
  -> IO (Record, Ptr Word8)
peekRecord fp basePtr p endPtr = do
  -- Outer length VarInt (we don't need it, but it has to be
  -- consumed so the cursor advances past it).
  (_len, p0) <- peekVarInt p endPtr
  -- Record attributes byte (always 0 in v2; we tolerate any value).
  (_attrs, p1) <- peekWord8 p0 endPtr
  (tsDelta, p2) <- peekVarLong p1 endPtr
  (offDelta, p3) <- peekVarInt p2 endPtr
  (mKey, p4) <- peekVarBytesSlice fp basePtr p3 endPtr
  (mVal, p5) <- peekVarBytesSlice fp basePtr p4 endPtr
  (hdrCount, p6) <- peekVarInt p5 endPtr
  (hdrs, p7) <- readHeaders fp basePtr (fromIntegral hdrCount) p6 endPtr
  pure
    ( Record
        { recordTimestampDelta = tsDelta
        , recordOffsetDelta = offDelta
        , recordKey = mKey
        , recordValue = mVal
        , recordHeaders = hdrs
        }
    , p7
    )


{- | Length-prefixed bytes, returned as a zero-copy slice over
the source 'ForeignPtr'.
-}
{-# INLINE peekVarBytesSlice #-}
peekVarBytesSlice
  :: ForeignPtr Word8
  -> Ptr Word8
  -> Ptr Word8
  -> Ptr Word8
  -> IO (Maybe ByteString, Ptr Word8)
peekVarBytesSlice fp basePtr p endPtr = do
  (n, p1) <- peekVarInt p endPtr
  if n < 0
    then pure (Nothing, p1)
    else do
      let !ni = fromIntegral n
      (bs, p2) <- peekByteStringSlice fp basePtr p1 endPtr ni
      pure (Just bs, p2)


{-# INLINE readHeaders #-}
readHeaders
  :: ForeignPtr Word8
  -> Ptr Word8
  -> Int
  -> Ptr Word8
  -> Ptr Word8
  -> IO ([RecordHeader], Ptr Word8)
readHeaders _ _ 0 !p _ = pure ([], p)
readHeaders fp basePtr n !p endPtr = do
  -- Build a list right-to-left so we don't reverse afterwards;
  -- each header is small enough that a list is fine here.
  (hs, p') <- go n p []
  pure (reverse hs, p')
  where
    go 0 q acc = pure (acc, q)
    go !k q acc = do
      (h, q') <- peekHeader fp basePtr q endPtr
      go (k - 1) q' (h : acc)


{-# INLINE peekHeader #-}
peekHeader
  :: ForeignPtr Word8
  -> Ptr Word8
  -> Ptr Word8
  -> Ptr Word8
  -> IO (RecordHeader, Ptr Word8)
peekHeader fp basePtr p endPtr = do
  (kLen, p1) <- peekVarInt p endPtr
  let !ki = fromIntegral kLen
  (k, p2) <- peekByteStringSlice fp basePtr p1 endPtr ki
  (mv, p3) <- peekVarBytesSlice fp basePtr p2 endPtr
  pure (RecordHeader {headerKey = k, headerValue = mv}, p3)


errOut :: String -> IO a
errOut = ioError . userError


----------------------------------------------------------------------
-- Decoder with decompression
----------------------------------------------------------------------

{- | Decode a 'RecordBatch' with automatic decompression. The
hot path:

  1. Parse the batch header + everything before the records via
     Wire pokes.
  2. If the batch is uncompressed, hand off to the per-record
     Wire decoder ('readRecords').
  3. Otherwise slice the compressed records section, run it
     through 'Compression.decompress', then parse the
     decompressed bytes via the per-record Wire decoder against
     the freshly-allocated 'ForeignPtr'.

Returns the decoded 'RecordBatch' or a 'Left' with the same
shape of error message the legacy
'decodeRecordBatchWithDecompression' produces, so callers that
pattern-match on the error text don't break.
-}
decodeRecordBatchWireWithDecompression
  :: ByteString
  -> IO (Either String RecordBatch)
decodeRecordBatchWireWithDecompression bs = do
  let (fp, off, len) = BSI.toForeignPtr bs
  withForeignPtr fp $ \basePtr -> do
    let !startPtr = basePtr `plusPtr` off
        !endPtr = startPtr `plusPtr` len
    r <-
      try (peekBatchWithDecompression fp basePtr startPtr endPtr)
        :: IO (Either SomeException RecordBatch)
    case r of
      Left e -> pure (Left (show e))
      Right rb -> pure (Right rb)


peekBatchWithDecompression
  :: ForeignPtr Word8
  -> Ptr Word8
  -> Ptr Word8
  -> Ptr Word8
  -> IO RecordBatch
peekBatchWithDecompression fp basePtr p endPtr = do
  ensureBytes p endPtr 21 "RecordBatch header"
  (baseOffset, p1) <- peekInt64BE p endPtr
  (lenValue, p2) <- peekInt32BE p1 endPtr
  (leaderEp, p3) <- peekInt32BE p2 endPtr
  (magic, p4) <- peekWord8 p3 endPtr
  if fromIntegral magic /= magicV2
    then errOut ("Unsupported magic byte: " ++ show magic)
    else do
      (storedCrc, p5) <- peekWord32BE p4 endPtr
      let !bodyStart = p5
          !bodyLen = fromIntegral lenValue - 4 - 1 - 4
          !bodyEnd = bodyStart `plusPtr` bodyLen
      ensureBytes bodyStart endPtr bodyLen "RecordBatch body"
      computedCrc <- CRC.crc32cPtr bodyStart bodyLen
      if computedCrc /= storedCrc
        then
          errOut
            ( "CRC mismatch: stored="
                ++ show storedCrc
                ++ ", computed="
                ++ show computedCrc
            )
        else do
          (attrsW, q1) <- peekInt16BE bodyStart bodyEnd
          attrs <- case decodeAttributes attrsW of
            Left e -> errOut e
            Right a -> pure a
          (lastDelta, q2) <- peekInt32BE q1 bodyEnd
          (baseTs, q3) <- peekInt64BE q2 bodyEnd
          (maxTs, q4) <- peekInt64BE q3 bodyEnd
          (pid, q5) <- peekInt64BE q4 bodyEnd
          (pep, q6) <- peekInt16BE q5 bodyEnd
          (baseSeq, q7) <- peekInt32BE q6 bodyEnd
          (recCount, q8) <- peekInt32BE q7 bodyEnd
          let !n = fromIntegral recCount :: Int
              !codec = attrCompressionType attrs
          records <- case codec of
            Compression.NoCompression ->
              if n <= 0
                then pure V.empty
                else readRecords fp basePtr n q8 bodyEnd
            _ -> do
              -- The compressed @records@ section runs from @q8@ to
              -- @bodyEnd@ within the input ForeignPtr.  Hand the raw
              -- pointer + length straight to the ring-aware
              -- decompressor (no input-side BS allocation; for
              -- snappy + zstd the output is sized exactly via
              -- 'BSI.mallocByteString' so the C decompressor writes
              -- directly into the heap buffer that becomes the
              -- result ByteString).
              let !rawLen = bodyEnd `minusPtr` q8
              decompressedR <- CompressionRing.decompressFromPtr codec q8 rawLen
              case decompressedR of
                Left err -> errOut ("Decompression failed: " ++ err)
                Right decompressed
                  | n <= 0 -> pure V.empty
                  | otherwise -> do
                      let (dfp, doff, dlen) = BSI.toForeignPtr decompressed
                      withForeignPtr dfp $ \dbase -> do
                        let !ds = dbase `plusPtr` doff
                            !de = ds `plusPtr` dlen
                        readRecords dfp dbase n ds de
          pure
            RecordBatch
              { batchBaseOffset = baseOffset
              , batchPartitionLeaderEpoch = leaderEp
              , batchAttributes = attrs
              , batchLastOffsetDelta = lastDelta
              , batchBaseTimestamp = baseTs
              , batchMaxTimestamp = maxTs
              , batchProducerId = pid
              , batchProducerEpoch = pep
              , batchBaseSequence = baseSeq
              , batchRecords = records
              }


----------------------------------------------------------------------
-- Sliced (memory-efficient) decoder
--
-- The standard 'decodeRecordBatchWire' returns a 'V.Vector
-- Record', where every record carries:
--
--   * one 'BS.ByteString' header for the key (24 bytes + a
--     'ForeignPtr' GC-root, ~32 bytes of GC bookkeeping);
--   * another for the value (same overhead);
--   * a list of 'RecordHeader' records, each with two more
--     'ByteString' headers.
--
-- For a 50 MiB fetch response with 100 K records that's roughly
-- 5.6 MiB of /header/ overhead alone, before any payload bytes
-- get touched. Each 'ByteString' also carries an independent
-- 'ForeignPtr' reference to the source buffer, so the GC has
-- 100 K + roots to walk every minor collection.
--
-- 'SlicedRecordBatch' collapses all of that into:
--
--   * one 'ForeignPtr Word8' for the source buffer (one GC
--     root for the whole batch);
--   * three unboxed parallel vectors with the per-record
--     metadata ('Int64' offset deltas, 'Int64' timestamp
--     deltas, header counts);
--   * two 'SV.SliceVector's, one for keys and one for values,
--     keyed on the same buffer (16 bytes per slice — an
--     @(Int32 offset, Int32 length)@ pair — vs ~56 bytes per
--     'ByteString' header).
--
-- For a 100 K-record batch the slice vectors are about 1.6 MiB
-- + 1.6 MiB instead of the ~5.6 MiB of headers; per-record
-- access is one 'IntMap'-free, branch-free pointer arithmetic
-- step.
--
-- Headers (KIP-82) are also exposed via the same flat-slice
-- shape: 'sbHeaderKeys' and 'sbHeaderValues' carry every
-- record's headers concatenated together, and
-- 'sbHeaderStartOffs' is the per-record prefix-sum index into
-- those (so record @i@'s headers live at indices
-- @[sbHeaderStartOffs ! i .. sbHeaderStartOffs ! (i+1) - 1]@).
-- 'slicedRecordHeaders' / 'slicedRecordHeader' /
-- 'slicedRecordHeaderCount' provide convenient per-record
-- access without forcing the caller to do the index
-- arithmetic.

{- | Memory-efficient view of a Kafka 'RecordBatch'. See the
module-level commentary above for the trade-offs vs
'RecordBatch'.
-}
data SlicedRecordBatch = SlicedRecordBatch
  { sbBaseOffset :: !Int64
  , sbPartitionLeaderEpoch :: !Int32
  , sbAttributes :: !Attributes
  , sbLastOffsetDelta :: !Int32
  , sbBaseTimestamp :: !Int64
  , sbMaxTimestamp :: !Int64
  , sbProducerId :: !Int64
  , sbProducerEpoch :: !Int16
  , sbBaseSequence :: !Int32
  , sbCount :: !Int
  -- ^ Number of records in the batch.
  , sbOffsetDeltas :: !(VU.Vector Int32)
  {- ^ Per-record offset delta. Index by record position
  @[0 .. sbCount - 1]@.
  -}
  , sbTimestampDeltas :: !(VU.Vector Int64)
  -- ^ Per-record timestamp delta.
  , sbKeySlices :: !SV.SliceVector
  {- ^ Per-record keys. A length of @-1@ on the underlying
  slice means the key was null (use 'slicedRecordKey'
  to do the right thing).
  -}
  , sbValueSlices :: !SV.SliceVector
  {- ^ Per-record values. A length of @-1@ means the value
  was null; the v2 record format permits this.
  -}
  , sbHeaderCounts :: !(VU.Vector Int32)
  -- ^ Number of headers each record has.
  , sbHeaderStartOffs :: !(VU.Vector Int32)
  {- ^ Per-record prefix-sum index into 'sbHeaderKeys' /
  'sbHeaderValues'. Length @sbCount + 1@; record @i@'s
  headers live at indices
  @[sbHeaderStartOffs ! i .. sbHeaderStartOffs ! (i+1) - 1]@.
  The trailing entry equals the total header count, which
  is also @SV.length sbHeaderKeys@.
  -}
  , sbHeaderKeys :: !SV.SliceVector
  {- ^ Header keys, concatenated across every record in the
  batch. Header keys are non-nullable (KIP-82); a length
  of zero is a legitimate empty key.
  -}
  , sbHeaderValues :: !SV.SliceVector
  {- ^ Header values, concatenated. A length of @-1@ means
  the value was null on the wire (KIP-82 allows this);
  'slicedRecordHeader' returns 'Nothing' for it.
  -}
  }


-- | Number of records in the batch.
{-# INLINE slicedRecordCount #-}
slicedRecordCount :: SlicedRecordBatch -> Int
slicedRecordCount = sbCount


{- | Compute the absolute Kafka offset of the i-th record in
the batch.
-}
{-# INLINE slicedRecordOffset #-}
slicedRecordOffset :: SlicedRecordBatch -> Int -> Int64
slicedRecordOffset SlicedRecordBatch {..} i =
  sbBaseOffset + fromIntegral (sbOffsetDeltas VU.! i)


-- | Compute the absolute Kafka timestamp of the i-th record.
{-# INLINE slicedRecordTimestamp #-}
slicedRecordTimestamp :: SlicedRecordBatch -> Int -> Int64
slicedRecordTimestamp SlicedRecordBatch {..} i =
  sbBaseTimestamp + sbTimestampDeltas VU.! i


{- | Read the i-th record's key as a zero-copy 'ByteString'
slice over the batch's backing buffer. Returns 'Nothing' if
the on-the-wire key was null (length -1).
-}
{-# INLINE slicedRecordKey #-}
slicedRecordKey :: SlicedRecordBatch -> Int -> Maybe ByteString
slicedRecordKey SlicedRecordBatch {..} i =
  let !(off, len) = VU.unsafeIndex (SV.sliceVectorOffsets sbKeySlices) i
  in if len < 0
       then Nothing
       else
         Just
           ( BSI.fromForeignPtr
               (SV.sliceVectorBuffer sbKeySlices)
               (fromIntegral off)
               (fromIntegral len)
           )


{- | Read the i-th record's value; 'Nothing' for a null value (a
tombstone, length @-1@ on the wire).
-}
{-# INLINE slicedRecordValue #-}
slicedRecordValue :: SlicedRecordBatch -> Int -> Maybe ByteString
slicedRecordValue SlicedRecordBatch {..} i =
  let !(off, len) = VU.unsafeIndex (SV.sliceVectorOffsets sbValueSlices) i
  in if len < 0
       then Nothing
       else
         Just
           ( BSI.fromForeignPtr
               (SV.sliceVectorBuffer sbValueSlices)
               (fromIntegral off)
               (fromIntegral len)
           )


-- | Number of headers attached to the i-th record (KIP-82).
{-# INLINE slicedRecordHeaderCount #-}
slicedRecordHeaderCount :: SlicedRecordBatch -> Int -> Int
slicedRecordHeaderCount SlicedRecordBatch {..} i =
  fromIntegral (VU.unsafeIndex sbHeaderCounts i)


{- | Read the j-th header on the i-th record as a zero-copy
@(key, Just value)@ pair, or @(key, Nothing)@ if the value
was null on the wire (KIP-82 permits null values; keys are
non-null).

Bounds checking is the caller's responsibility: pair with
'slicedRecordHeaderCount'. Out-of-range reads
'Prelude.error' through the underlying 'VU.Vector' index.
-}
{-# INLINE slicedRecordHeader #-}
slicedRecordHeader
  :: SlicedRecordBatch
  -> Int
  -- ^ record index
  -> Int
  -- ^ header index within the record (0-based)
  -> (ByteString, Maybe ByteString)
slicedRecordHeader SlicedRecordBatch {..} i j =
  let !startIx = fromIntegral (VU.unsafeIndex sbHeaderStartOffs i) :: Int
      !flatIx = startIx + j
      !(kOff, kLen) = VU.unsafeIndex (SV.sliceVectorOffsets sbHeaderKeys) flatIx
      !(vOff, vLen) = VU.unsafeIndex (SV.sliceVectorOffsets sbHeaderValues) flatIx
      !key =
        BSI.fromForeignPtr
          (SV.sliceVectorBuffer sbHeaderKeys)
          (fromIntegral kOff)
          (fromIntegral kLen)
      !val
        | vLen < 0 = Nothing
        | otherwise =
            Just
              ( BSI.fromForeignPtr
                  (SV.sliceVectorBuffer sbHeaderValues)
                  (fromIntegral vOff)
                  (fromIntegral vLen)
              )
  in (key, val)


{- | Materialise every header on the i-th record as a list of
@(key, Just value)@ / @(key, Nothing)@ pairs. Convenience
wrapper around 'slicedRecordHeader' for callers that prefer
the list shape; the per-header bytes are still zero-copy
slices of the underlying source buffer.
-}
{-# INLINE slicedRecordHeaders #-}
slicedRecordHeaders
  :: SlicedRecordBatch
  -> Int
  -> [(ByteString, Maybe ByteString)]
slicedRecordHeaders sb i =
  let !cnt = slicedRecordHeaderCount sb i
  in [slicedRecordHeader sb i j | j <- [0 .. cnt - 1]]


{- | Decode a record batch into the memory-efficient sliced
view. The bytes are interpreted exactly the same way as
'decodeRecordBatchWire'; the only differences are how the
result is arranged in memory and that headers are not
materialised.
-}
{-# INLINEABLE decodeRecordBatchWireSliced #-}
decodeRecordBatchWireSliced
  :: ByteString -> Either String SlicedRecordBatch
decodeRecordBatchWireSliced bs = unsafePerformIO $ do
  let (fp, off, len) = BSI.toForeignPtr bs
  withForeignPtr fp $ \basePtr -> do
    let !startPtr = basePtr `plusPtr` off
        !endPtr = startPtr `plusPtr` len
    r <-
      try (peekSlicedBatch fp basePtr startPtr endPtr)
        :: IO (Either SomeException SlicedRecordBatch)
    case r of
      Left e -> pure (Left (show e))
      Right rb -> pure (Right rb)


{-# INLINE peekSlicedBatch #-}
peekSlicedBatch
  :: ForeignPtr Word8
  -> Ptr Word8
  -> Ptr Word8
  -> Ptr Word8
  -> IO SlicedRecordBatch
peekSlicedBatch fp basePtr p endPtr = do
  ensureBytes p endPtr 21 "RecordBatch header"
  (baseOffset, p1) <- peekInt64BE p endPtr
  (lenValue, p2) <- peekInt32BE p1 endPtr
  (leaderEp, p3) <- peekInt32BE p2 endPtr
  (magic, p4) <- peekWord8 p3 endPtr
  if fromIntegral magic /= magicV2
    then errOut ("Unsupported magic byte: " ++ show magic)
    else do
      (storedCrc, p5) <- peekWord32BE p4 endPtr
      let !bodyStart = p5
          !bodyLen = fromIntegral lenValue - 4 - 1 - 4
          !bodyEnd = bodyStart `plusPtr` bodyLen
      ensureBytes bodyStart endPtr bodyLen "RecordBatch body"
      computedCrc <- CRC.crc32cPtr bodyStart bodyLen
      if computedCrc /= storedCrc
        then
          errOut
            ( "CRC mismatch: stored="
                ++ show storedCrc
                ++ ", computed="
                ++ show computedCrc
            )
        else do
          (attrsW, q1) <- peekInt16BE bodyStart bodyEnd
          attrs <- case decodeAttributes attrsW of
            Left e -> errOut e
            Right a -> pure a
          (lastDelta, q2) <- peekInt32BE q1 bodyEnd
          (baseTs, q3) <- peekInt64BE q2 bodyEnd
          (maxTs, q4) <- peekInt64BE q3 bodyEnd
          (pid, q5) <- peekInt64BE q4 bodyEnd
          (pep, q6) <- peekInt16BE q5 bodyEnd
          (baseSeq, q7) <- peekInt32BE q6 bodyEnd
          (recCount, q8) <- peekInt32BE q7 bodyEnd
          let !n = fromIntegral recCount :: Int
          if n <= 0
            then
              pure
                SlicedRecordBatch
                  { sbBaseOffset = baseOffset
                  , sbPartitionLeaderEpoch = leaderEp
                  , sbAttributes = attrs
                  , sbLastOffsetDelta = lastDelta
                  , sbBaseTimestamp = baseTs
                  , sbMaxTimestamp = maxTs
                  , sbProducerId = pid
                  , sbProducerEpoch = pep
                  , sbBaseSequence = baseSeq
                  , sbCount = 0
                  , sbOffsetDeltas = VU.empty
                  , sbTimestampDeltas = VU.empty
                  , sbKeySlices = SV.empty
                  , sbValueSlices = SV.empty
                  , sbHeaderCounts = VU.empty
                  , sbHeaderStartOffs = VU.singleton 0
                  , sbHeaderKeys = SV.empty
                  , sbHeaderValues = SV.empty
                  }
            else do
              ( offDeltas
                , tsDeltas
                , keyOffs
                , valOffs
                , hdrCounts
                , hdrStarts
                , hdrKeyOffs
                , hdrValOffs
                ) <-
                readSlicedRecords basePtr n q8 bodyEnd
              pure
                SlicedRecordBatch
                  { sbBaseOffset = baseOffset
                  , sbPartitionLeaderEpoch = leaderEp
                  , sbAttributes = attrs
                  , sbLastOffsetDelta = lastDelta
                  , sbBaseTimestamp = baseTs
                  , sbMaxTimestamp = maxTs
                  , sbProducerId = pid
                  , sbProducerEpoch = pep
                  , sbBaseSequence = baseSeq
                  , sbCount = n
                  , sbOffsetDeltas = offDeltas
                  , sbTimestampDeltas = tsDeltas
                  , sbKeySlices = SV.fromForeignPtrSlices fp keyOffs
                  , sbValueSlices = SV.fromForeignPtrSlices fp valOffs
                  , sbHeaderCounts = hdrCounts
                  , sbHeaderStartOffs = hdrStarts
                  , sbHeaderKeys = SV.fromForeignPtrSlices fp hdrKeyOffs
                  , sbHeaderValues = SV.fromForeignPtrSlices fp hdrValOffs
                  }


{- | Walk the @n@ records once, populating eight parallel
vectors. The two 'SliceVector' offset arrays for keys /
values + the two for header keys / values all use the
source buffer's 'ForeignPtr' as the shared backing store
so the per-record cost is a handful of @(Int32, Int32)@
writes — no 'ByteString' header allocation.
-}
{-# INLINE readSlicedRecords #-}
readSlicedRecords
  :: Ptr Word8
  -- ^ basePtr (start of source buffer)
  -> Int
  -- ^ record count
  -> Ptr Word8
  -- ^ start of first record
  -> Ptr Word8
  -- ^ end of body
  -> IO
       ( VU.Vector Int32 -- offset deltas
       , VU.Vector Int64 -- timestamp deltas
       , VU.Vector (Int32, Int32) -- key (offset, length) pairs
       , VU.Vector (Int32, Int32) -- value (offset, length) pairs
       , VU.Vector Int32 -- per-record header counts
       , VU.Vector Int32 -- per-record header start indices
       -- (prefix-sum, length n+1)
       , VU.Vector (Int32, Int32) -- header key (offset, length)
       , VU.Vector (Int32, Int32) -- header value (offset, length)
       )
readSlicedRecords basePtr n start endPtr = do
  mvOff <- VUM.unsafeNew n
  mvTs <- VUM.unsafeNew n
  mvKey <- VUM.unsafeNew n
  mvVal <- VUM.unsafeNew n
  mvHdrCount <- VUM.unsafeNew n
  mvHdrStart <- VUM.unsafeNew (n + 1)
  -- Headers go into doubling-grow unboxed mutable vectors (one for
  -- keys, one for values). The running total is threaded through
  -- 'go' as a strict 'Int' so the no-header path doesn't touch an
  -- IORef.
  let !initialHdrCap = max 16 (n * 2)
  hdrKeysRef <-
    newIORef
      =<< ( VUM.unsafeNew initialHdrCap
              :: IO (VUM.IOVector (Int32, Int32))
          )
  hdrValsRef <-
    newIORef
      =<< ( VUM.unsafeNew initialHdrCap
              :: IO (VUM.IOVector (Int32, Int32))
          )
  let go !i !p !runningHdrTotal
        | i >= n = pure runningHdrTotal
        | otherwise = do
            -- Outer length VarInt (consumed but unused).
            (_len, p0) <- peekVarInt p endPtr
            -- Per-record attributes byte (always 0 in v2).
            (_attrs, p1) <- peekWord8 p0 endPtr
            (tsDelta, p2) <- peekVarLong p1 endPtr
            (offDelta, p3) <- peekVarInt p2 endPtr
            (keyLen, p4) <- peekVarInt p3 endPtr
            let !keyOffset = fromIntegral (p4 `minusPtr` basePtr) :: Int32
                !keyLenInt = fromIntegral keyLen :: Int
                !p5 =
                  if keyLen < 0
                    then p4
                    else p4 `plusPtr` keyLenInt
            (valLen, p6) <- peekVarInt p5 endPtr
            let !valOffset = fromIntegral (p6 `minusPtr` basePtr) :: Int32
                !valLenInt = fromIntegral valLen :: Int
                !p7 =
                  if valLen < 0
                    then p6
                    else p6 `plusPtr` valLenInt
            (hdrCount, p8) <- peekVarInt p7 endPtr
            -- Stamp the prefix-sum index for this record. All
            -- per-record writes happen before the header walk so
            -- they can't be reordered around its potential
            -- IOVector growth.
            VUM.unsafeWrite mvHdrStart i (fromIntegral runningHdrTotal)
            VUM.unsafeWrite mvOff i offDelta
            VUM.unsafeWrite mvTs i tsDelta
            VUM.unsafeWrite mvKey i (keyOffset, fromIntegral keyLen)
            VUM.unsafeWrite mvVal i (valOffset, fromIntegral valLen)
            VUM.unsafeWrite mvHdrCount i hdrCount
            if hdrCount == 0
              then go (i + 1) p8 runningHdrTotal
              else do
                let !hcInt = fromIntegral hdrCount
                p9 <-
                  captureHeadersInto
                    basePtr
                    hcInt
                    p8
                    endPtr
                    runningHdrTotal
                    hdrKeysRef
                    hdrValsRef
                go (i + 1) p9 (runningHdrTotal + hcInt)
  totalHdrs <- go 0 start 0
  -- Trailing entry of 'mvHdrStart' is the total header count;
  -- 'slicedRecordHeader' derives per-record header length as
  -- @start[i+1] - start[i]@.
  VUM.unsafeWrite mvHdrStart n (fromIntegral totalHdrs)
  -- Snap the header buffers to their used prefix and freeze
  -- in place — no copy.
  hdrKeysBuf <- readIORef hdrKeysRef
  hdrValsBuf <- readIORef hdrValsRef
  let !hdrKeysVec = VUM.unsafeTake totalHdrs hdrKeysBuf
      !hdrValsVec = VUM.unsafeTake totalHdrs hdrValsBuf
  hdrKeysFrozen <- VU.unsafeFreeze hdrKeysVec
  hdrValsFrozen <- VU.unsafeFreeze hdrValsVec
  (,,,,,,,)
    <$> VU.unsafeFreeze mvOff
    <*> VU.unsafeFreeze mvTs
    <*> VU.unsafeFreeze mvKey
    <*> VU.unsafeFreeze mvVal
    <*> VU.unsafeFreeze mvHdrCount
    <*> VU.unsafeFreeze mvHdrStart
    <*> pure hdrKeysFrozen
    <*> pure hdrValsFrozen


{- | Walk @k@ headers, recording each header's key and value
@(offset, length)@ pair into the supplied doubling-grow
'IOVector' buffers. Returns the cursor positioned past the
last header. Header keys are non-nullable per KIP-82; header
values may be null (encoded as VarInt length -1).

'startIx' is the index of the first header in the buffer
(== running total of all headers seen so far in the batch);
callers maintain it themselves so we don't need to read it
back out of an 'IORef Int' here.
-}
{-# INLINE captureHeadersInto #-}
captureHeadersInto
  :: Ptr Word8
  -> Int
  -> Ptr Word8
  -> Ptr Word8
  -> Int
  -- ^ start index in buffer
  -> IORef (VUM.IOVector (Int32, Int32)) -- key buffer (mutable)
  -> IORef (VUM.IOVector (Int32, Int32)) -- value buffer (mutable)
  -> IO (Ptr Word8)
captureHeadersInto basePtr !k !p endPtr !startIx keyBufRef valBufRef = go k p startIx
  where
    go 0 !pt _ = pure pt
    go !kRem !pt !ix = do
      keyBuf <- readIORef keyBufRef
      valBuf <- readIORef valBufRef
      let !cap = VUM.length keyBuf
      keyBuf' <- if ix < cap then pure keyBuf else growBuf keyBuf cap
      valBuf' <- if ix < cap then pure valBuf else growBuf valBuf cap
      when (ix >= cap) $ do
        writeIORef keyBufRef keyBuf'
        writeIORef valBufRef valBuf'
      (kLen, p1) <- peekVarInt pt endPtr
      let !kOff = fromIntegral (p1 `minusPtr` basePtr) :: Int32
          !p2 = p1 `plusPtr` fromIntegral kLen
      (vLen, p3) <- peekVarInt p2 endPtr
      let !vOff = fromIntegral (p3 `minusPtr` basePtr) :: Int32
          !p4 = if vLen < 0 then p3 else p3 `plusPtr` fromIntegral vLen
      VUM.unsafeWrite keyBuf' ix (kOff, fromIntegral kLen)
      VUM.unsafeWrite valBuf' ix (vOff, fromIntegral vLen)
      go (kRem - 1) p4 (ix + 1)

    -- Doubling growth. 'VUM.unsafeGrow' allocates a fresh
    -- buffer, copies the live prefix, and returns the new
    -- buffer; the old one is GC'd.
    {-# INLINE growBuf #-}
    growBuf
      :: VUM.IOVector (Int32, Int32)
      -> Int
      -> IO (VUM.IOVector (Int32, Int32))
    growBuf old cap = VUM.unsafeGrow old cap


----------------------------------------------------------------------
-- Sliced decoder with decompression
----------------------------------------------------------------------

{- | Variant of 'decodeRecordBatchWireSliced' that also handles
compressed batches. Mirrors
'decodeRecordBatchWireWithDecompression' but produces a
'SlicedRecordBatch' so the consumer's hot poll path can take
the lower-allocation slice view through to ConsumerRecord
materialisation. For uncompressed batches the work is the
same as 'decodeRecordBatchWireSliced' (just one extra branch
on 'attrCompressionType'); for compressed batches we
decompress the records section, then run
'readSlicedRecords' against the decompressed buffer's
'ForeignPtr' so the resulting 'SliceVector's stay zero-copy
relative to the decompressed bytes.
-}
decodeRecordBatchWireSlicedWithDecompression
  :: ByteString -> IO (Either String SlicedRecordBatch)
decodeRecordBatchWireSlicedWithDecompression bs = do
  let (fp, off, len) = BSI.toForeignPtr bs
  withForeignPtr fp $ \basePtr -> do
    let !startPtr = basePtr `plusPtr` off
        !endPtr = startPtr `plusPtr` len
    r <-
      try (peekSlicedBatchWithDecompression fp basePtr startPtr endPtr)
        :: IO (Either SomeException SlicedRecordBatch)
    case r of
      Left e -> pure (Left (show e))
      Right rb -> pure (Right rb)


peekSlicedBatchWithDecompression
  :: ForeignPtr Word8
  -> Ptr Word8
  -> Ptr Word8
  -> Ptr Word8
  -> IO SlicedRecordBatch
peekSlicedBatchWithDecompression fp basePtr p endPtr = do
  ensureBytes p endPtr 21 "RecordBatch header"
  (baseOffset, p1) <- peekInt64BE p endPtr
  (lenValue, p2) <- peekInt32BE p1 endPtr
  (leaderEp, p3) <- peekInt32BE p2 endPtr
  (magic, p4) <- peekWord8 p3 endPtr
  if fromIntegral magic /= magicV2
    then errOut ("Unsupported magic byte: " ++ show magic)
    else do
      (storedCrc, p5) <- peekWord32BE p4 endPtr
      let !bodyStart = p5
          !bodyLen = fromIntegral lenValue - 4 - 1 - 4
          !bodyEnd = bodyStart `plusPtr` bodyLen
      ensureBytes bodyStart endPtr bodyLen "RecordBatch body"
      computedCrc <- CRC.crc32cPtr bodyStart bodyLen
      if computedCrc /= storedCrc
        then
          errOut
            ( "CRC mismatch: stored="
                ++ show storedCrc
                ++ ", computed="
                ++ show computedCrc
            )
        else do
          (attrsW, q1) <- peekInt16BE bodyStart bodyEnd
          attrs <- case decodeAttributes attrsW of
            Left e -> errOut e
            Right a -> pure a
          (lastDelta, q2) <- peekInt32BE q1 bodyEnd
          (baseTs, q3) <- peekInt64BE q2 bodyEnd
          (maxTs, q4) <- peekInt64BE q3 bodyEnd
          (pid, q5) <- peekInt64BE q4 bodyEnd
          (pep, q6) <- peekInt16BE q5 bodyEnd
          (baseSeq, q7) <- peekInt32BE q6 bodyEnd
          (recCount, q8) <- peekInt32BE q7 bodyEnd
          let !n = fromIntegral recCount :: Int
              !codec = attrCompressionType attrs
          -- The no-compression and compression branches both
          -- end up calling 'readSlicedRecords' against some
          -- 'ForeignPtr'; the only difference is which
          -- 'ForeignPtr' the resulting 'SliceVector's get
          -- keyed on (the source buffer's 'fp' for the
          -- uncompressed case, or a fresh decompressed-buffer
          -- 'ForeignPtr' for the compressed case).
          ( offDeltas
            , tsDeltas
            , keyVec
            , valVec
            , hdrCounts
            , hdrStarts
            , hdrKeyVec
            , hdrValVec
            ) <-
            case codec of
              Compression.NoCompression
                | n <= 0 ->
                    pure
                      ( VU.empty
                      , VU.empty
                      , SV.empty
                      , SV.empty
                      , VU.empty
                      , VU.singleton 0
                      , SV.empty
                      , SV.empty
                      )
                | otherwise -> do
                    (offD, tsD, kP, vP, hC, hS, hK, hV) <-
                      readSlicedRecords basePtr n q8 bodyEnd
                    pure
                      ( offD
                      , tsD
                      , SV.fromForeignPtrSlices fp kP
                      , SV.fromForeignPtrSlices fp vP
                      , hC
                      , hS
                      , SV.fromForeignPtrSlices fp hK
                      , SV.fromForeignPtrSlices fp hV
                      )
              _ -> do
                -- Ring-aware path: hand the raw compressed pointer +
                -- length straight to the codec; the destination
                -- ByteString is sized exactly (snappy / zstd) so the
                -- C decompressor writes directly into the
                -- 'BSI.mallocByteString'-allocated buffer.
                let !rawLen = bodyEnd `minusPtr` q8
                decompressedR <- CompressionRing.decompressFromPtr codec q8 rawLen
                case decompressedR of
                  Left err -> errOut ("Decompression failed: " ++ err)
                  Right decompressed
                    | n <= 0 ->
                        pure
                          ( VU.empty
                          , VU.empty
                          , SV.empty
                          , SV.empty
                          , VU.empty
                          , VU.singleton 0
                          , SV.empty
                          , SV.empty
                          )
                    | otherwise -> do
                        let (dfp, doff, dlen) = BSI.toForeignPtr decompressed
                        withForeignPtr dfp $ \dbase -> do
                          let !ds = dbase `plusPtr` doff
                              !de = ds `plusPtr` dlen
                          (offD, tsD, kP, vP, hC, hS, hK, hV) <-
                            readSlicedRecords dbase n ds de
                          pure
                            ( offD
                            , tsD
                            , SV.fromForeignPtrSlices dfp kP
                            , SV.fromForeignPtrSlices dfp vP
                            , hC
                            , hS
                            , SV.fromForeignPtrSlices dfp hK
                            , SV.fromForeignPtrSlices dfp hV
                            )
          pure
            SlicedRecordBatch
              { sbBaseOffset = baseOffset
              , sbPartitionLeaderEpoch = leaderEp
              , sbAttributes = attrs
              , sbLastOffsetDelta = lastDelta
              , sbBaseTimestamp = baseTs
              , sbMaxTimestamp = maxTs
              , sbProducerId = pid
              , sbProducerEpoch = pep
              , sbBaseSequence = baseSeq
              , sbCount = n
              , sbOffsetDeltas = offDeltas
              , sbTimestampDeltas = tsDeltas
              , sbKeySlices = keyVec
              , sbValueSlices = valVec
              , sbHeaderCounts = hdrCounts
              , sbHeaderStartOffs = hdrStarts
              , sbHeaderKeys = hdrKeyVec
              , sbHeaderValues = hdrValVec
              }
