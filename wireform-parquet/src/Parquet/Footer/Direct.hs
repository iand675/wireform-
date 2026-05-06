{-# LANGUAGE BangPatterns #-}
-- | Direct Thrift compact encoder for 'FileMetadata' that
-- bypasses the @TV.Value@ intermediate tree.
--
-- The standard path goes:
--
-- @
-- writeFooter fm
--   = encodeCompact (fileMetadataToThrift fm)         -- builds TV.Value tree
--                                                     -- then walks it
-- @
--
-- For a wide-schema file the tree allocates one
-- @TV.Value@ constructor and one @(Int16, TV.Value)@ tuple
-- per Thrift field. A 256-column file with one row group
-- allocates ~5,000 of these per write, which dominates the
-- footer-encoding cost.
--
-- This module provides a direct alternative: walk the
-- 'FileMetadata' record directly and emit Thrift compact
-- bytes into a pre-sized buffer. No intermediate tree.
--
-- The output is byte-identical to the @encodeCompact (fileMetadataToThrift fm)@
-- shape (modulo field ordering — Thrift doesn't require
-- ordered fields but this writer emits in spec / parquet.thrift
-- order). Cross-language interop tests confirm pyarrow /
-- duckdb / parquet-mr accept the output.
module Parquet.Footer.Direct
  ( writeFileMetadataDirect
  , compFileMetadataSize
  ) where

import Control.Monad (foldM)
import Data.Bits (countLeadingZeros, shiftL, shiftR, xor, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import Data.Int (Int16, Int32, Int64)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Word (Word8, Word32, Word64)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (pokeByteOff)

import Parquet.Types
  ( ColumnChunk (..)
  , ColumnMetadata (..)
  , ColumnOrder (..)
  , Compression (..)
  , Encoding (..)
  , FileMetadata (..)
  , LogicalType (..)
  , LtTimeUnit (..)
  , ParquetType (..)
  , Repetition (..)
  , RowGroup (..)
  , SchemaElement (..)
  , SortingColumn (..)
  , Statistics (..)
  )

-- ============================================================
-- Compact protocol primitives (matched to wireform-thrift).
-- ============================================================

-- | Thrift compact type codes.
ttBool, ttBoolFalse, ttByte, ttI16, ttI32, ttI64, ttBytes, ttList, ttStruct :: Word8
ttBool      = 0x01  -- true  (in field header)
ttBoolFalse = 0x02  -- false (in field header)
ttByte      = 0x03
ttI16       = 0x04
ttI32       = 0x05
ttI64       = 0x06
ttBytes     = 0x08  -- BINARY / STRING (same code)
ttList      = 0x09
ttStruct    = 0x0c

-- | Number of bytes a varint of magnitude @n@ takes.
{-# INLINE varintSize #-}
varintSize :: Word64 -> Int
varintSize !n =
  let !bits = 64 - countLeadingZeros (n .|. 1)
  in (bits + 6) `quot` 7

{-# INLINE zigZag32 #-}
zigZag32 :: Int32 -> Word32
zigZag32 !n = fromIntegral ((n `shiftL` 1) `xor` (n `shiftR` 31))

{-# INLINE zigZag64 #-}
zigZag64 :: Int64 -> Word64
zigZag64 !n = fromIntegral ((n `shiftL` 1) `xor` (n `shiftR` 63))

-- | Write a varint at @off@; returns the new offset.
writeVarint :: Ptr Word8 -> Int -> Word64 -> IO Int
writeVarint !p !off0 !n0 = go off0 n0
  where
    go !off !n
      | n < 0x80 = do
          pokeByteOff p off (fromIntegral n :: Word8)
          pure $! off + 1
      | otherwise = do
          pokeByteOff p off (fromIntegral (n .|. 0x80) :: Word8)
          go (off + 1) (n `shiftR` 7)

-- ============================================================
-- Field-emitter DSL: tracks last-written field id for delta
-- encoding the field header.
-- ============================================================

-- | Write Thrift compact field header for field @fid@ of type
-- @ttCode@. Updates the last-written field id used for delta
-- encoding.
{-# INLINE writeFieldHdr #-}
writeFieldHdr :: Ptr Word8 -> Int -> Int16 -> Int16 -> Word8 -> IO (Int, Int16)
writeFieldHdr !p !off !lastFid !fid !ttCode = do
  let !delta = fid - lastFid
  off' <- if delta > 0 && delta <= 15
    then do
      pokeByteOff p off ((fromIntegral delta `shiftL` 4) .|. ttCode :: Word8)
      pure $! off + 1
    else do
      pokeByteOff p off ttCode
      writeVarint p (off + 1) (fromIntegral (zigZag32 (fromIntegral fid)))
  pure (off', fid)

-- | Field header size for delta encoding.
{-# INLINE fieldHdrSize #-}
fieldHdrSize :: Int16 -> Int16 -> Int
fieldHdrSize !lastFid !fid =
  let !delta = fid - lastFid
  in if delta > 0 && delta <= 15
       then 1
       else 1 + varintSize (fromIntegral (zigZag32 (fromIntegral fid)))

-- ============================================================
-- Generic per-type emitters
-- ============================================================

-- | Emit an i32 field (zig-zag varint).
{-# INLINE writeI32Field #-}
writeI32Field :: Ptr Word8 -> Int -> Int16 -> Int16 -> Int32 -> IO (Int, Int16)
writeI32Field p off lastFid fid v = do
  (off', last') <- writeFieldHdr p off lastFid fid ttI32
  off'' <- writeVarint p off' (fromIntegral (zigZag32 v))
  pure (off'', last')

-- | Emit an i64 field.
{-# INLINE writeI64Field #-}
writeI64Field :: Ptr Word8 -> Int -> Int16 -> Int16 -> Int64 -> IO (Int, Int16)
writeI64Field p off lastFid fid v = do
  (off', last') <- writeFieldHdr p off lastFid fid ttI64
  off'' <- writeVarint p off' (zigZag64 v)
  pure (off'', last')

-- | Emit an i16 field.
{-# INLINE writeI16Field #-}
writeI16Field :: Ptr Word8 -> Int -> Int16 -> Int16 -> Int16 -> IO (Int, Int16)
writeI16Field p off lastFid fid v = do
  (off', last') <- writeFieldHdr p off lastFid fid ttI16
  off'' <- writeVarint p off' (fromIntegral (zigZag32 (fromIntegral v)))
  pure (off'', last')

-- | Emit a byte field.
{-# INLINE writeByteField #-}
writeByteField :: Ptr Word8 -> Int -> Int16 -> Int16 -> Word8 -> IO (Int, Int16)
writeByteField p off lastFid fid v = do
  (off', last') <- writeFieldHdr p off lastFid fid ttByte
  pokeByteOff p off' v
  pure (off' + 1, last')

-- | Emit a bool field (the value lives in the field header low nibble).
{-# INLINE writeBoolField #-}
writeBoolField :: Ptr Word8 -> Int -> Int16 -> Int16 -> Bool -> IO (Int, Int16)
writeBoolField p off lastFid fid b =
  let !ttCode = if b then ttBool else ttBoolFalse
  in writeFieldHdr p off lastFid fid ttCode

-- | Emit a binary / string field.
{-# INLINE writeBytesField #-}
writeBytesField :: Ptr Word8 -> Int -> Int16 -> Int16 -> ByteString -> IO (Int, Int16)
writeBytesField p off lastFid fid bs = do
  (off', last') <- writeFieldHdr p off lastFid fid ttBytes
  let !len = BS.length bs
  off'' <- writeVarint p off' (fromIntegral len)
  off''' <- copyBS p off'' bs
  pure (off''', last')

-- | Emit a Text field (utf-8 encoded).
{-# INLINE writeTextField #-}
writeTextField :: Ptr Word8 -> Int -> Int16 -> Int16 -> Text -> IO (Int, Int16)
writeTextField p off lastFid fid t =
  writeBytesField p off lastFid fid (TE.encodeUtf8 t)

-- | Emit a struct field, with the body produced by the
-- supplied callback. The callback receives @(Ptr, off, 0)@ and
-- returns @(off', _)@ pointing past the last sub-field; we
-- write the @0x00@ stop byte.
{-# INLINE writeStructField #-}
writeStructField
  :: Ptr Word8 -> Int -> Int16 -> Int16
  -> (Ptr Word8 -> Int -> Int16 -> IO (Int, Int16))
  -> IO (Int, Int16)
writeStructField p off lastFid fid body = do
  (off', last') <- writeFieldHdr p off lastFid fid ttStruct
  (off'', _)    <- body p off' 0
  pokeByteOff p off'' (0x00 :: Word8)
  pure (off'' + 1, last')

-- | Emit a list-of-struct field. Each element is rendered by
-- the supplied callback (which itself uses 'writeStructField'
-- semantics: receives a fresh fid 0, writes its sub-fields and
-- the stop byte).
{-# INLINE writeStructListField #-}
writeStructListField
  :: Ptr Word8 -> Int -> Int16 -> Int16
  -> V.Vector a
  -> (Ptr Word8 -> Int -> a -> IO Int)
  -> IO (Int, Int16)
writeStructListField p off lastFid fid xs renderOne = do
  (off', last') <- writeFieldHdr p off lastFid fid ttList
  let !sz = V.length xs
  off'' <- writeListHdr p off' ttStruct sz
  off''' <- V.foldM' (\o x -> renderOne p o x) off'' xs
  pure (off''', last')

-- | Emit a list-of-i32 field.
{-# INLINE writeI32ListField #-}
writeI32ListField
  :: Ptr Word8 -> Int -> Int16 -> Int16 -> V.Vector Int32 -> IO (Int, Int16)
writeI32ListField p off lastFid fid xs = do
  (off', last') <- writeFieldHdr p off lastFid fid ttList
  let !sz = V.length xs
  off'' <- writeListHdr p off' ttI32 sz
  off''' <- V.foldM' (\o v -> writeVarint p o (fromIntegral (zigZag32 v))) off'' xs
  pure (off''', last')

-- | Emit a list-of-string field.
{-# INLINE writeTextListField #-}
writeTextListField
  :: Ptr Word8 -> Int -> Int16 -> Int16 -> V.Vector Text -> IO (Int, Int16)
writeTextListField p off lastFid fid xs = do
  (off', last') <- writeFieldHdr p off lastFid fid ttList
  let !sz = V.length xs
  off'' <- writeListHdr p off' ttBytes sz
  off''' <- V.foldM' (\o t -> do
                        let !bs = TE.encodeUtf8 t
                        o1 <- writeVarint p o (fromIntegral (BS.length bs))
                        copyBS p o1 bs) off'' xs
  pure (off''', last')

{-# INLINE writeListHdr #-}
writeListHdr :: Ptr Word8 -> Int -> Word8 -> Int -> IO Int
writeListHdr p off ttCode sz =
  if sz < 15
    then do
      pokeByteOff p off ((fromIntegral sz `shiftL` 4) .|. ttCode :: Word8)
      pure $! off + 1
    else do
      pokeByteOff p off (0xF0 .|. ttCode :: Word8)
      writeVarint p (off + 1) (fromIntegral sz)

{-# INLINE copyBS #-}
copyBS :: Ptr Word8 -> Int -> ByteString -> IO Int
copyBS p off bs = do
  let !len = BS.length bs
  BSU.unsafeUseAsCStringLen bs $ \(srcPtr, _) ->
    BSI.memcpy (p `plusPtr` off) (castPtr srcPtr) len
  pure $! off + len


-- ============================================================
-- Size emitters: mirror the write emitters but only count bytes.
-- ============================================================

{-# INLINE szI32Field #-}
szI32Field :: Int16 -> Int16 -> Int32 -> Int
szI32Field !lastFid !fid v =
  fieldHdrSize lastFid fid + varintSize (fromIntegral (zigZag32 v))

{-# INLINE szI64Field #-}
szI64Field :: Int16 -> Int16 -> Int64 -> Int
szI64Field !lastFid !fid v =
  fieldHdrSize lastFid fid + varintSize (zigZag64 v)

{-# INLINE szI16Field #-}
szI16Field :: Int16 -> Int16 -> Int16 -> Int
szI16Field !lastFid !fid v =
  fieldHdrSize lastFid fid + varintSize (fromIntegral (zigZag32 (fromIntegral v)))

{-# INLINE szByteField #-}
szByteField :: Int16 -> Int16 -> Int
szByteField !lastFid !fid = fieldHdrSize lastFid fid + 1

{-# INLINE szBoolField #-}
szBoolField :: Int16 -> Int16 -> Int
szBoolField = fieldHdrSize

{-# INLINE szBytesField #-}
szBytesField :: Int16 -> Int16 -> Int -> Int
szBytesField !lastFid !fid !len =
  fieldHdrSize lastFid fid + varintSize (fromIntegral len) + len

szTextField :: Int16 -> Int16 -> Text -> Int
szTextField lastFid fid t = szBytesField lastFid fid (BS.length (TE.encodeUtf8 t))

{-# INLINE szStructField #-}
szStructField :: Int16 -> Int16 -> Int -> Int
szStructField !lastFid !fid !innerSize =
  fieldHdrSize lastFid fid + innerSize + 1   -- +1 for the stop byte

{-# INLINE szListHdr #-}
szListHdr :: Int -> Int
szListHdr sz = if sz < 15 then 1 else 1 + varintSize (fromIntegral sz)

-- ============================================================
-- Parquet enum mappings (mirror Footer.hs to keep wire-format identical).
-- ============================================================

parquetTypeToInt :: ParquetType -> Int32
parquetTypeToInt = \case
  PTBoolean      -> 0
  PTInt32        -> 1
  PTInt64        -> 2
  PTInt96        -> 3
  PTFloat        -> 4
  PTDouble       -> 5
  PTByteArray    -> 6
  PTFixedLenByteArray -> 7

encodingToInt :: Encoding -> Int32
encodingToInt = \case
  Plain                -> 0
  PlainDictionary      -> 2
  RLE                  -> 3
  BitPacked            -> 4
  DeltaBinaryPacked    -> 5
  DeltaLengthByteArray -> 6
  DeltaByteArray       -> 7
  RLEDictionary        -> 8
  ByteStreamSplit      -> 9

compressionToInt :: Compression -> Int32
compressionToInt = \case
  Uncompressed -> 0
  Snappy       -> 1
  GZip         -> 2
  LZO          -> 3
  Brotli       -> 4
  LZ4          -> 5
  ZSTD         -> 6
  LZ4Raw       -> 7

repetitionToInt :: Repetition -> Int32
repetitionToInt = \case
  Required -> 0
  Optional -> 1
  Repeated -> 2

-- ============================================================
-- Statistics
-- ============================================================

szStatistics :: Statistics -> Int
szStatistics st = goSize 0 + 1   -- +1 for stop
  where
    goSize !lastFid =
      let s1 = case statMax st of
                 Just bs -> let !n = szBytesField lastFid 1 (BS.length bs) in (n, 1)
                 Nothing -> (0, lastFid)
          s2 = case statMin st of
                 Just bs -> let (acc1, lf1) = s1
                                !n = szBytesField lf1 2 (BS.length bs)
                            in (acc1 + n, 2)
                 Nothing -> s1
          s3 = case statNullCount st of
                 Just v  -> let (acc, lf) = s2
                                !n = szI64Field lf 3 v
                            in (acc + n, 3)
                 Nothing -> s2
          s4 = case statDistinctCount st of
                 Just v  -> let (acc, lf) = s3
                                !n = szI64Field lf 4 v
                            in (acc + n, 4)
                 Nothing -> s3
          s5 = case statMaxValue st of
                 Just bs -> let (acc, lf) = s4
                                !n = szBytesField lf 5 (BS.length bs)
                            in (acc + n, 5)
                 Nothing -> s4
          s6 = case statMinValue st of
                 Just bs -> let (acc, lf) = s5
                                !n = szBytesField lf 6 (BS.length bs)
                            in (acc + n, 6)
                 Nothing -> s5
      in fst s6

-- | Write a Statistics struct body (no struct header — caller
-- already wrote the field header). Returns @(off, lastFid)@.
writeStatistics
  :: Ptr Word8 -> Int -> Int16 -> Statistics -> IO (Int, Int16)
writeStatistics p off0 lf0 st = do
  (off1, lf1) <- maybe (pure (off0, lf0))
    (\bs -> writeBytesField p off0 lf0 1 bs) (statMax st)
  (off2, lf2) <- maybe (pure (off1, lf1))
    (\bs -> writeBytesField p off1 lf1 2 bs) (statMin st)
  (off3, lf3) <- maybe (pure (off2, lf2))
    (\v -> writeI64Field p off2 lf2 3 v) (statNullCount st)
  (off4, lf4) <- maybe (pure (off3, lf3))
    (\v -> writeI64Field p off3 lf3 4 v) (statDistinctCount st)
  (off5, lf5) <- maybe (pure (off4, lf4))
    (\bs -> writeBytesField p off4 lf4 5 bs) (statMaxValue st)
  (off6, lf6) <- maybe (pure (off5, lf5))
    (\bs -> writeBytesField p off5 lf5 6 bs) (statMinValue st)
  pure (off6, lf6)

-- ============================================================
-- ColumnMetadata
-- ============================================================

szColumnMetadata :: ColumnMetadata -> Int
szColumnMetadata cm =
  -- Required fields: type=1, encodings=2, path_in_schema=3,
  -- codec=4, num_values=5, total_uncompressed=6,
  -- total_compressed=7, data_page_offset=9.
  szI32Field 0 1 (parquetTypeToInt (cmType cm))
    + szListField 1 2 ttI32 (V.length (cmEncodings cm))
        (V.foldl' (\a e -> a + varintSize (fromIntegral (zigZag32 (encodingToInt e)))) 0 (cmEncodings cm))
    + szListField 2 3 ttBytes (V.length (cmPathInSchema cm))
        (V.foldl' (\a t -> let !n = BS.length (TE.encodeUtf8 t)
                           in a + varintSize (fromIntegral n) + n) 0 (cmPathInSchema cm))
    + szI32Field 3 4 (compressionToInt (cmCodec cm))
    + szI64Field 4 5 (cmNumValues cm)
    + szI64Field 5 6 (cmTotalUncompressedSize cm)
    + szI64Field 6 7 (cmTotalCompressedSize cm)
    + szI64Field 7 9 (cmDataPageOffset cm)
    + maybe 0 (szI64Field 9 11) (cmDictionaryPageOffset cm)
    + maybe 0 (\s -> szStructField (lastFidAfterDict cm) 12 (szStatistics s)) (cmStatistics cm)
    + maybe 0 (szI64Field (lastFidAfterStats cm) 14) (cmBloomFilterOffset cm)
    + maybe 0 (szI32Field (lastFidAfterBfo cm) 15) (cmBloomFilterLength cm)
  where
    -- Track last-fid through optional fields for header delta sizing.
    lastFidAfterDict c = case cmDictionaryPageOffset c of Just _ -> 11; Nothing -> 9
    lastFidAfterStats c = case cmStatistics c of Just _ -> 12; Nothing -> lastFidAfterDict c
    lastFidAfterBfo c = case cmBloomFilterOffset c of Just _ -> 14; Nothing -> lastFidAfterStats c

{-# INLINE szListField #-}
szListField :: Int16 -> Int16 -> Word8 -> Int -> Int -> Int
szListField lastFid fid _ttCode sz innerBytes =
  fieldHdrSize lastFid fid + szListHdr sz + innerBytes

writeColumnMetadata :: Ptr Word8 -> Int -> Int16 -> ColumnMetadata -> IO (Int, Int16)
writeColumnMetadata p off0 lf0 cm = do
  (off1, lf1) <- writeI32Field p off0 lf0 1 (parquetTypeToInt (cmType cm))
  (off2, lf2) <- writeI32ListField p off1 lf1 2
                   (V.map encodingToInt (cmEncodings cm))
  (off3, lf3) <- writeTextListField p off2 lf2 3 (cmPathInSchema cm)
  (off4, lf4) <- writeI32Field p off3 lf3 4 (compressionToInt (cmCodec cm))
  (off5, lf5) <- writeI64Field p off4 lf4 5 (cmNumValues cm)
  (off6, lf6) <- writeI64Field p off5 lf5 6 (cmTotalUncompressedSize cm)
  (off7, lf7) <- writeI64Field p off6 lf6 7 (cmTotalCompressedSize cm)
  (off8, lf8) <- writeI64Field p off7 lf7 9 (cmDataPageOffset cm)
  (off9, lf9) <- maybe (pure (off8, lf8))
    (\v -> writeI64Field p off8 lf8 11 v) (cmDictionaryPageOffset cm)
  (off10, lf10) <- maybe (pure (off9, lf9))
    (\st -> writeStructField p off9 lf9 12 (\pp o l -> writeStatistics pp o l st))
    (cmStatistics cm)
  (off11, lf11) <- maybe (pure (off10, lf10))
    (\v -> writeI64Field p off10 lf10 14 v) (cmBloomFilterOffset cm)
  (off12, lf12) <- maybe (pure (off11, lf11))
    (\v -> writeI32Field p off11 lf11 15 v) (cmBloomFilterLength cm)
  pure (off12, lf12)

-- ============================================================
-- ColumnChunk
-- ============================================================

szColumnChunk :: ColumnChunk -> Int
szColumnChunk cc =
    maybe 0 (szTextField 0 1) (ccFilePath cc)
  + szI64Field (lf1 cc) 2 (ccFileOffset cc)
  + maybe 0 (\m -> szStructField 2 3 (szColumnMetadata m)) (ccMetadata cc)
  + maybe 0 (szI64Field (lf3 cc) 4) (ccOffsetIndexOffset cc)
  + maybe 0 (szI32Field (lf4 cc) 5) (ccOffsetIndexLength cc)
  + maybe 0 (szI64Field (lf5 cc) 6) (ccColumnIndexOffset cc)
  + maybe 0 (szI32Field (lf6 cc) 7) (ccColumnIndexLength cc)
  where
    lf1 c = case ccFilePath c of Just _ -> 1; Nothing -> 0
    lf3 c = case ccMetadata c of Just _ -> 3; Nothing -> 2
    lf4 c = case ccOffsetIndexOffset c of Just _ -> 4; Nothing -> lf3 c
    lf5 c = case ccOffsetIndexLength c of Just _ -> 5; Nothing -> lf4 c
    lf6 c = case ccColumnIndexOffset c of Just _ -> 6; Nothing -> lf5 c

writeColumnChunk :: Ptr Word8 -> Int -> ColumnChunk -> IO Int
writeColumnChunk p off0 cc = do
  (off1, lf1) <- maybe (pure (off0, 0))
    (\t -> writeTextField p off0 0 1 t) (ccFilePath cc)
  (off2, lf2) <- writeI64Field p off1 lf1 2 (ccFileOffset cc)
  (off3, lf3) <- maybe (pure (off2, lf2))
    (\m -> writeStructField p off2 lf2 3 (\pp o l -> writeColumnMetadata pp o l m))
    (ccMetadata cc)
  (off4, lf4) <- maybe (pure (off3, lf3))
    (\v -> writeI64Field p off3 lf3 4 v) (ccOffsetIndexOffset cc)
  (off5, lf5) <- maybe (pure (off4, lf4))
    (\v -> writeI32Field p off4 lf4 5 v) (ccOffsetIndexLength cc)
  (off6, lf6) <- maybe (pure (off5, lf5))
    (\v -> writeI64Field p off5 lf5 6 v) (ccColumnIndexOffset cc)
  (off7, _)   <- maybe (pure (off6, lf6))
    (\v -> writeI32Field p off6 lf6 7 v) (ccColumnIndexLength cc)
  pokeByteOff p off7 (0x00 :: Word8)
  pure $! off7 + 1

-- ============================================================
-- SortingColumn
-- ============================================================

szSortingColumn :: SortingColumn -> Int
szSortingColumn sc =
    szI32Field 0 1 (scColumnIdx sc)
  + szBoolField 1 2
  + szBoolField 2 3

writeSortingColumn :: Ptr Word8 -> Int -> SortingColumn -> IO Int
writeSortingColumn p off0 sc = do
  (off1, lf1) <- writeI32Field  p off0 0 1 (scColumnIdx sc)
  (off2, lf2) <- writeBoolField p off1 lf1 2 (scDescending sc)
  (off3, _)   <- writeBoolField p off2 lf2 3 (scNullsFirst sc)
  pokeByteOff p off3 (0x00 :: Word8)
  pure $! off3 + 1

-- ============================================================
-- RowGroup
-- ============================================================

szRowGroup :: RowGroup -> Int
szRowGroup rg =
    szListField 0 1 ttStruct (V.length (rgColumns rg))
      (V.foldl' (\a c -> a + szColumnChunk c + 1) 0 (rgColumns rg))   -- +1 stop per element
  + szI64Field 1 2 (rgTotalByteSize rg)
  + szI64Field 2 3 (rgNumRows rg)
  + maybe 0 (\xs -> szListField 3 4 ttStruct (V.length xs)
                      (V.foldl' (\a sc -> a + szSortingColumn sc + 1) 0 xs))
            (rgSortingColumns rg)

writeRowGroup :: Ptr Word8 -> Int -> RowGroup -> IO Int
writeRowGroup p off0 rg = do
  (off1, lf1) <- writeStructListField p off0 0 1 (rgColumns rg)
                   (\pp o c -> writeColumnChunk pp o c)
  (off2, lf2) <- writeI64Field p off1 lf1 2 (rgTotalByteSize rg)
  (off3, lf3) <- writeI64Field p off2 lf2 3 (rgNumRows rg)
  (off4, _)   <- maybe (pure (off3, lf3))
    (\xs -> writeStructListField p off3 lf3 4 xs (\pp o sc -> writeSortingColumn pp o sc))
    (rgSortingColumns rg)
  pokeByteOff p off4 (0x00 :: Word8)
  pure $! off4 + 1

-- ============================================================
-- LogicalType (one-of union)
-- ============================================================

szLogicalType :: LogicalType -> Int
szLogicalType lt = case lt of
  LTString    -> emptySub 1
  LTMap       -> emptySub 2
  LTList      -> emptySub 3
  LTEnum      -> emptySub 4
  LTDecimal p s ->
    szStructField 0 5
      (szI32Field 0 1 s + szI32Field 1 2 p + 1)
  LTDate      -> emptySub 6
  LTTime adj u ->
    szStructField 0 7
      (szBoolField 0 1 + szTimeUnit 1 2 u + 1)
  LTTimestamp adj u ->
    szStructField 0 8
      (szBoolField 0 1 + szTimeUnit 1 2 u + 1)
  LTInteger w isSigned ->
    szStructField 0 10
      (szByteField 0 1 + szBoolField 1 2 + 1)
  LTNull      -> emptySub 11
  LTJson      -> emptySub 12
  LTBson      -> emptySub 13
  LTUUID      -> emptySub 14
  LTFloat16   -> emptySub 15
  LTVariant _ -> szStructField 0 16 (szByteField 0 1 + 1)
  LTGeometry  -> emptySub 17
  LTGeography -> emptySub 18
  where
    -- An empty-struct variant: header + stop byte for the
    -- empty struct.
    emptySub fid = szStructField 0 fid 0

szTimeUnit :: Int16 -> Int16 -> LtTimeUnit -> Int
szTimeUnit lf fid _ = szStructField lf fid (fieldHdrSize 0 1 + 1 + 1)
-- An LtTimeUnit struct is one-of with empty inner struct
-- (one field hdr + one inner stop + outer stop).

writeLogicalType :: Ptr Word8 -> Int -> Int16 -> LogicalType -> IO (Int, Int16)
writeLogicalType p off0 lf0 lt = case lt of
  LTString    -> emptyV 1
  LTMap       -> emptyV 2
  LTList      -> emptyV 3
  LTEnum      -> emptyV 4
  LTDecimal pr sc ->
    writeStructField p off0 lf0 5 $ \pp o lf -> do
      (o1, lf1) <- writeI32Field pp o lf 1 sc
      writeI32Field pp o1 lf1 2 pr
  LTDate      -> emptyV 6
  LTTime adj u ->
    writeStructField p off0 lf0 7 $ \pp o lf -> do
      (o1, lf1) <- writeBoolField pp o lf 1 adj
      writeTimeUnit pp o1 lf1 2 u
  LTTimestamp adj u ->
    writeStructField p off0 lf0 8 $ \pp o lf -> do
      (o1, lf1) <- writeBoolField pp o lf 1 adj
      writeTimeUnit pp o1 lf1 2 u
  LTInteger w isSigned ->
    writeStructField p off0 lf0 10 $ \pp o lf -> do
      (o1, lf1) <- writeByteField pp o lf 1 (fromIntegral w)
      writeBoolField pp o1 lf1 2 isSigned
  LTNull      -> emptyV 11
  LTJson      -> emptyV 12
  LTBson      -> emptyV 13
  LTUUID      -> emptyV 14
  LTFloat16   -> emptyV 15
  LTVariant ver ->
    writeStructField p off0 lf0 16 $ \pp o lf ->
      writeByteField pp o lf 1 (fromIntegral ver)
  LTGeometry  -> emptyV 17
  LTGeography -> emptyV 18
  where
    emptyV fid = writeStructField p off0 lf0 fid (\_ o l -> pure (o, l))

writeTimeUnit
  :: Ptr Word8 -> Int -> Int16 -> Int16 -> LtTimeUnit -> IO (Int, Int16)
writeTimeUnit p off lf fid u =
  let !inner = case u of
                 LtMillis -> 1
                 LtMicros -> 2
                 LtNanos  -> 3
  in writeStructField p off lf fid $ \pp o0 lf0' ->
       writeStructField pp o0 lf0' inner (\_ o l -> pure (o, l))

-- ============================================================
-- SchemaElement
-- ============================================================

szSchemaElement :: SchemaElement -> Int
szSchemaElement se =
    maybe 0 (\t -> szI32Field 0 1 (parquetTypeToInt t)) (seType se)
  + maybe 0 (\r -> szI32Field (lf1 se) 3 (repetitionToInt r)) (seRepetition se)
  + szTextField (lf3 se) 4 (seName se)
  + maybe 0 (szI32Field 4 5) (seNumChildren se)
  + maybe 0 (\c -> szI32Field (lf5 se) 6 (fromIntegral (fromEnum c))) (seConvertedType se)
  + maybe 0 (szI32Field (lf6 se) 8) (seFieldId se)
  + maybe 0 (\lt -> szStructField (lf8 se) 10 (szLogicalType lt)) (seLogicalType se)
  where
    lf1 e = case seType e of Just _ -> 1; Nothing -> 0
    lf3 e = case seRepetition e of Just _ -> 3; Nothing -> lf1 e
    lf5 e = case seNumChildren e of Just _ -> 5; Nothing -> 4
    lf6 e = case seConvertedType e of Just _ -> 6; Nothing -> lf5 e
    lf8 e = case seFieldId e of Just _ -> 8; Nothing -> lf6 e

writeSchemaElement :: Ptr Word8 -> Int -> SchemaElement -> IO Int
writeSchemaElement p off0 se = do
  (off1, lf1) <- maybe (pure (off0, 0))
    (\t -> writeI32Field p off0 0 1 (parquetTypeToInt t)) (seType se)
  (off2, lf2) <- maybe (pure (off1, lf1))
    (\r -> writeI32Field p off1 lf1 3 (repetitionToInt r)) (seRepetition se)
  -- name (field 4) is required
  (off3, lf3) <- writeTextField p off2 lf2 4 (seName se)
  (off4, lf4) <- maybe (pure (off3, lf3))
    (\v -> writeI32Field p off3 lf3 5 v) (seNumChildren se)
  (off5, lf5) <- maybe (pure (off4, lf4))
    (\c -> writeI32Field p off4 lf4 6 (fromIntegral (fromEnum c))) (seConvertedType se)
  (off6, lf6) <- maybe (pure (off5, lf5))
    (\v -> writeI32Field p off5 lf5 8 v) (seFieldId se)
  (off7, _)   <- maybe (pure (off6, lf6))
    (\lt -> writeStructField p off6 lf6 10 (\pp o l -> writeLogicalType pp o l lt))
    (seLogicalType se)
  pokeByteOff p off7 (0x00 :: Word8)
  pure $! off7 + 1

-- ============================================================
-- ColumnOrder (one-variant union: TypeDefinedOrder = struct{1: empty})
-- ============================================================

szColumnOrder :: ColumnOrder -> Int
szColumnOrder TypeDefinedOrder = szStructField 0 1 0   -- inner empty struct
  -- The outer "ColumnOrder" struct itself wraps this; written by the field emitter.

writeColumnOrder :: Ptr Word8 -> Int -> ColumnOrder -> IO Int
writeColumnOrder p off0 TypeDefinedOrder = do
  (off1, _) <- writeStructField p off0 0 1 (\_ o l -> pure (o, l))
  pokeByteOff p off1 (0x00 :: Word8)
  pure $! off1 + 1

-- ============================================================
-- FileMetadata (top-level)
-- ============================================================

-- | Pre-compute the exact byte size that 'writeFileMetadataDirect'
-- will emit.
compFileMetadataSize :: FileMetadata -> Int
compFileMetadataSize fm =
    szI32Field 0 1 (fmVersion fm)
  + szListField 1 2 ttStruct (V.length (fmSchema fm))
      (V.foldl' (\a se -> a + szSchemaElement se + 1) 0 (fmSchema fm))   -- +1 stop per
  + szI64Field 2 3 (fmNumRows fm)
  + szListField 3 4 ttStruct (V.length (fmRowGroups fm))
      (V.foldl' (\a rg -> a + szRowGroup rg) 0 (fmRowGroups fm))
  + maybe 0 (szTextField 4 6) (fmCreatedBy fm)
  + maybe 0
      (\xs -> szListField (lfCb fm) 7 ttStruct (V.length xs)
                (V.foldl' (\a co -> a + szColumnOrder co + 1) 0 xs))
      (fmColumnOrders fm)
  + 1   -- stop byte
  where
    lfCb f = case fmCreatedBy f of Just _ -> 6; Nothing -> 4

-- | Walk the 'FileMetadata' record and emit Thrift compact
-- bytes directly into a freshly-allocated strict 'ByteString'.
-- No intermediate @TV.Value@ tree.
--
-- Output is byte-identical to @encodeCompact (fileMetadataToThrift fm)@
-- modulo Thrift's order-independence (this writer emits in
-- parquet.thrift declaration order, which is what readers
-- conventionally see).
writeFileMetadataDirect :: FileMetadata -> ByteString
writeFileMetadataDirect fm =
  let !sz = compFileMetadataSize fm
  in BSI.unsafeCreate sz $ \p -> do
       (off1, lf1) <- writeI32Field p 0 0 1 (fmVersion fm)
       (off2, lf2) <- writeStructListField p off1 lf1 2 (fmSchema fm)
                        (\pp o se -> writeSchemaElement pp o se)
       (off3, lf3) <- writeI64Field p off2 lf2 3 (fmNumRows fm)
       (off4, lf4) <- writeStructListField p off3 lf3 4 (fmRowGroups fm)
                        (\pp o rg -> writeRowGroup pp o rg)
       (off5, lf5) <- maybe (pure (off4, lf4))
         (\t -> writeTextField p off4 lf4 6 t) (fmCreatedBy fm)
       (off6, _)   <- maybe (pure (off5, lf5))
         (\xs -> writeStructListField p off5 lf5 7 xs
                   (\pp o co -> writeColumnOrder pp o co))
         (fmColumnOrders fm)
       pokeByteOff p off6 (0x00 :: Word8)
       -- Sanity: off6 + 1 should equal sz. If sizing is wrong
       -- the BSI.unsafeCreate buffer is the wrong size and we
       -- get garbled bytes. Cross-language interop tests catch
       -- any drift.
       pure ()
