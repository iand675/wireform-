{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
-- | Arrow IPC encoder: builds the @Schema@, @Field@, @Type@,
-- @RecordBatch@, @DictionaryBatch@, @Tensor@ and @SparseTensor@
-- flatbuffer tables, the encapsulated-message framing, the stream
-- and file layouts, and body compression. See "Arrow.FlatBufferIPC"
-- for the format notes.
--
-- Record batch and dictionary batch headers are written front to
-- back straight into their final bytes (no FlatBuffers builder): the
-- field nodes and buffers are 'Storable' vectors whose memory layout
-- is the wire layout, so each is one memcpy. Streams and files are
-- laid out from 'Frame's: 'renderStream' / 'renderFile' make one
-- allocation and copy every body byte once, 'renderStreamLazy' /
-- 'renderFileLazy' alias the body pieces as lazy chunks.
module Arrow.FlatBufferIPC.Write
  ( -- * Top-level builders
    buildSchemaMessage
  , buildRecordBatchMessage
    -- * Encapsulated-message framing
  , encapsulateMessage
    -- * Frames and layout
  , Frame (..)
  , EncodedBatch (..)
  , encodeBatch
  , schemaFrame
  , batchFrame
  , dictionaryFrame
  , renderStream
  , renderStreamLazy
  , renderFile
  , renderFileLazy
    -- * Stream / file writers
  , writeArrowStreamFB
  , writeArrowFileFB
  , writeArrowFileFBWithDicts
    -- * Column-based convenience writer
  , buildRecordBatchBytes
  , buildRecordBatchBytesWith
  , writeArrowStreamFBFromColumns
    -- * Body compression helpers
  , compressBody
  , compressBufferEither
  , bodyCompressionAvailable
    -- * Dictionary support
  , buildDictionaryBatchMessage
  , writeArrowStreamFBWithDicts
    -- * Tensor / SparseTensor
  , buildTensorMessage
  , encodeTensorFrame
  , buildSparseTensorMessageCOO
  , encodeSparseTensorFrame
    -- * Single-message frames
  , encodeMessageFrame
  ) where

#include "MachDeps.h"
#if defined(WORDS_BIGENDIAN)
#error "Arrow.FlatBufferIPC.Write copies Storable FieldNode/Buffer vectors as little-endian wire structs"
#endif

import Control.Monad (foldM, when)
import Data.Bits ((.&.), complement)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BSU
import Data.Int (Int16, Int32, Int64)
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Data.Word (Word16, Word32, Word8)
import Foreign.Marshal.Utils (copyBytes, fillBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (pokeByteOff)
import System.IO.Unsafe (unsafePerformIO)

import Arrow.Column (ColumnArray)
import Arrow.FlatBufferIPC.Common
import Arrow.Types
import Arrow.Write.Columns (BatchPlan (..), planBatch)
import FlatBuffers.Builder
  ( Builder
  , alignUp
  , finish
  , newBuilder
  , prependBS
  , prependI16
  , prependI32
  , prependI64
  , prependU8
  , scalar
  , struct
  , voff
  , writeString
  , writeTable
  , writeVectorInt32
  , writeVectorInt64
  , writeVectorOfOffsets
  , writeVectorOfStructs
  )

#ifdef HAVE_ZSTD
import qualified Codec.Compression.Zstd as Zstd
#endif

#ifdef HAVE_LZ4
import qualified Codec.Lz4 as Lz4
#endif
-- ============================================================
-- Arrow-specific: Type tables
-- ============================================================

-- | Returns (union_tag, UOffset of the type table).
writeType :: Builder -> ArrowType -> IO (Word8, Int)
writeType b ty = case ty of
  ANull             -> emptyT 1
  AInt bits signed  -> do
    -- Arrow's Int.fbs declares is_signed with no default, but
    -- arrow-cpp's generated reader defaults absent slots to
    -- @true@. The writer used to omit the slot when
    -- @signed = False@, which silently coerced unsigned columns
    -- back to signed on round-trip. Emit the slot explicitly
    -- whenever @signed = False@ so both paths survive.
    u <- writeTable b
           [ Just (scalar 4 (\bb -> prependI32 bb (fromIntegral bits)))
           , Just (scalar 1 (\bb -> prependU8 bb (if signed then 1 else 0)))
           ]
    pure (2, u)
  AFloatingPoint p  -> do
    u <- writeTable b [ Just (scalar 2 (\bb -> prependI16 bb (fromIntegral (precisionTag p)))) ]
    pure (3, u)
  ABinary           -> emptyT 4
  AUtf8             -> emptyT 5
  ABool             -> emptyT 6
  ADecimal p s      -> do
    u <- writeTable b
           [ Just (scalar 4 (\bb -> prependI32 bb (fromIntegral p)))
           , Just (scalar 4 (\bb -> prependI32 bb (fromIntegral s)))
           , Nothing   -- bitWidth default 128
           ]
    pure (7, u)
  ADecimal256 p s   -> do
    u <- writeTable b
           [ Just (scalar 4 (\bb -> prependI32 bb (fromIntegral p)))
           , Just (scalar 4 (\bb -> prependI32 bb (fromIntegral s)))
           , Just (scalar 4 (\bb -> prependI32 bb 256))
           ]
    pure (7, u)
  ADate u' -> do
    u <- writeTable b [ Just (scalar 2 (\bb -> prependI16 bb (fromIntegral (dateUnitTag u')))) ]
    pure (8, u)
  ATime u' bits -> do
    u <- writeTable b
           [ Just (scalar 2 (\bb -> prependI16 bb (fromIntegral (timeUnitTag u'))))
           , Just (scalar 4 (\bb -> prependI32 bb (fromIntegral bits)))
           ]
    pure (9, u)
  ATimestamp u' tz -> do
    tzOff <- case tz of
      Nothing -> pure Nothing
      Just t  -> Just <$> writeString b t
    u <- writeTable b
           [ Just (scalar 2 (\bb -> prependI16 bb (fromIntegral (timeUnitTag u'))))
           , case tzOff of
               Nothing  -> Nothing
               Just uo  -> Just (voff uo)
           ]
    pure (10, u)
  AInterval u' -> do
    u <- writeTable b [ Just (scalar 2 (\bb -> prependI16 bb (fromIntegral (intervalUnitTag u')))) ]
    pure (11, u)
  AList  -> emptyT 12
  AStruct -> emptyT 13
  AUnion mode typeIds -> do
    idsOff <- if V.null typeIds
                then pure Nothing
                else Just <$> writeVectorInt32 b (V.toList typeIds)
    u <- writeTable b
           [ Just (scalar 2 (\bb -> prependI16 bb (fromIntegral (unionModeTag mode))))
           , case idsOff of { Nothing -> Nothing; Just uo -> Just (voff uo) }
           ]
    pure (14, u)
  AFixedSizeBinary n -> do
    u <- writeTable b [ Just (scalar 4 (\bb -> prependI32 bb (fromIntegral n))) ]
    pure (15, u)
  AFixedSizeList n -> do
    u <- writeTable b [ Just (scalar 4 (\bb -> prependI32 bb (fromIntegral n))) ]
    pure (16, u)
  AMap sorted -> do
    u <- writeTable b
           [ if sorted then Just (scalar 1 (\bb -> prependU8 bb 1)) else Nothing ]
    pure (17, u)
  ADuration u' -> do
    u <- writeTable b [ Just (scalar 2 (\bb -> prependI16 bb (fromIntegral (timeUnitTag u')))) ]
    pure (18, u)
  ALargeBinary    -> emptyT 19
  ALargeUtf8      -> emptyT 20
  ALargeList      -> emptyT 21
  ARunEndEncoded  -> emptyT 22
  ABinaryView     -> emptyT 23
  AUtf8View       -> emptyT 24
  AListView       -> emptyT 25
  ALargeListView  -> emptyT 26
  where
    emptyT !tag = do
      u <- writeTable b []
      pure (tag, u)

precisionTag :: Precision -> Int
precisionTag Half            = 0
precisionTag Single          = 1
precisionTag DoublePrecision = 2

dateUnitTag :: DateUnit -> Int
dateUnitTag DateDay         = 0
dateUnitTag DateMillisecond = 1

timeUnitTag :: TimeUnit -> Int
timeUnitTag Second      = 0
timeUnitTag Millisecond = 1
timeUnitTag Microsecond = 2
timeUnitTag Nanosecond  = 3

intervalUnitTag :: IntervalUnit -> Int
intervalUnitTag YearMonth    = 0
intervalUnitTag DayTime      = 1
intervalUnitTag MonthDayNano = 2

unionModeTag :: UnionMode -> Int
unionModeTag Sparse = 0
unionModeTag Dense  = 1

-- ============================================================
-- Field + Schema tables
-- ============================================================

-- | @
-- table Field {
--   name            : string;           // 0
--   nullable        : bool;             // 1
--   type_type       : ubyte;            // 2
--   type            : Type;             // 3
--   dictionary      : DictionaryEncoding; // 4
--   children        : [Field];          // 5
--   custom_metadata : [KeyValue];       // 6
-- }
-- @
writeField :: Builder -> Field -> IO Int
writeField b fld = do
  childrenVec <- if V.null (fieldChildren fld)
                   then pure Nothing
                   else do
                     childUOffs <- mapM (writeField b) (V.toList (fieldChildren fld))
                     Just <$> writeVectorOfOffsets b childUOffs
  (tyTag, tyUOff) <- writeType b (fieldType fld)
  dictOff <- case fieldDictionary fld of
    Nothing -> pure Nothing
    Just de -> Just <$> writeDictionaryEncoding b de
  nameOff <- if T.null (fieldName fld)
               then pure Nothing
               else Just <$> writeString b (fieldName fld)
  customMd <- writeKeyValueVector b (fieldMetadata fld)
  writeTable b
    [ case nameOff of { Nothing -> Nothing; Just uo -> Just (voff uo) }
    , if fieldNullable fld then Just (scalar 1 (\bb -> prependU8 bb 1)) else Nothing
    , Just (scalar 1 (\bb -> prependU8 bb tyTag))
    , Just (voff tyUOff)
    , case dictOff of { Nothing -> Nothing; Just uo -> Just (voff uo) }
    , case childrenVec of { Nothing -> Nothing; Just uo -> Just (voff uo) }
    , case customMd  of { Nothing -> Nothing; Just uo -> Just (voff uo) }
    ]

-- | Build a @[KeyValue]@ vector for @custom_metadata@ slots
-- (used by both 'Field' and 'Schema'). Returns 'Nothing' for an
-- empty metadata vector so the caller can omit the slot
-- entirely (FlatBuffers' 'Nothing' = default).
writeKeyValueVector :: Builder -> V.Vector (T.Text, T.Text) -> IO (Maybe Int)
writeKeyValueVector b kvs
  | V.null kvs = pure Nothing
  | otherwise = do
      kvUOffs <- mapM (writeKeyValueTable b) (V.toList kvs)
      Just <$> writeVectorOfOffsets b kvUOffs

writeKeyValueTable :: Builder -> (T.Text, T.Text) -> IO Int
writeKeyValueTable b (k, v) = do
  kOff <- writeString b k
  vOff <- writeString b v
  writeTable b [ Just (voff kOff), Just (voff vOff) ]

-- | Build a 'DictionaryEncoding' table:
--
-- @
-- table DictionaryEncoding {
--   id: long;
--   indexType: Int;
--   isOrdered: bool;
--   dictionaryKind: DictionaryKind;
-- }
-- @
writeDictionaryEncoding :: Builder -> DictionaryEncoding -> IO Int
writeDictionaryEncoding b (DictionaryEncoding did indexTy ordered) = do
  -- The indexType is always an Int table; build via 'writeType' to
  -- reuse the layout, but we must always emit the table even if
  -- indexTy is the default Int32-signed.
  (_, intUOff) <- writeType b indexTy
  writeTable b
    [ Just (scalar 8 (\bb -> prependI64 bb did))
    , Just (voff intUOff)
    , if ordered then Just (scalar 1 (\bb -> prependU8 bb 1)) else Nothing
    -- dictionaryKind defaults to DenseArray (0); omit.
    ]

-- | @
-- table Schema {
--   endianness     : Endianness = Little;
--   fields         : [Field];
--   custom_metadata: [KeyValue];
--   features       : [long];
-- }
-- @
writeSchema :: Builder -> Schema -> IO Int
writeSchema b sch = do
  fieldUOffs <- mapM (writeField b) (V.toList (arrowFields sch))
  fieldsVec  <- writeVectorOfOffsets b fieldUOffs
  customMd   <- writeKeyValueVector b (arrowMetadata sch)
  writeTable b
    [ case arrowEndianness sch of
        Little -> Nothing
        Big    -> Just (scalar 2 (\bb -> prependI16 bb 1))
    , Just (voff fieldsVec)
    , case customMd of { Nothing -> Nothing; Just uo -> Just (voff uo) }
    , Nothing -- features
    ]

-- ============================================================
-- RecordBatch / DictionaryBatch messages (direct, front to back)
-- ============================================================

-- | The @DictionaryBatch@ wrapper fields: id and isDelta.
data DictHeader = DictHeader !Int64 !Bool


align8 :: Int -> Int
align8 n = (n + 7) .&. complement 7
{-# INLINE align8 #-}


{- | A record batch @Message@ (or, with a 'DictHeader', a dictionary
batch @Message@ wrapping it), written front to back into one
allocation. With @encapsulated@ the result is the whole frame header:
continuation marker, metadata length, the metadata, and zero padding
to an 8-byte multiple; otherwise just the metadata.

Layout (offsets relative to the metadata start, which the framing
keeps 8-aligned): root offset; @Message@ vtable at 4 and table at 16
(version, header type, header, bodyLength at 32); for a dictionary
batch its vtable at 40 and table at 56 (id, data, isDelta); then the
@RecordBatch@ vtable and table (length, nodes, buffers, compression,
variadicBufferCounts), the nodes and buffers vectors (16-byte structs,
one memcpy each from the 'Storable' vectors, 8-aligned data), the
@BodyCompression@ vtable and table, and the variadic counts vector.
Every offset points forward and every vtable precedes its table.
-}
batchMessage :: Bool -> Maybe DictHeader -> RecordBatchDef -> Int64 -> ByteString
batchMessage encapsulated mDict rb bodyLen = BSI.unsafeCreate total $ \p0 -> do
  fillBytes p0 0 total
  when encapsulated $ do
    pokeByteOff p0 0 (0xFFFFFFFF :: Word32)
    pokeByteOff p0 4 (fromIntegral metaPadded :: Int32)
  let !p = p0 `plusPtr` pre
      w8 :: Int -> Int -> IO ()
      w8 o x = pokeByteOff p o (fromIntegral x :: Word8)
      w16 :: Int -> Int -> IO ()
      w16 o x = pokeByteOff p o (fromIntegral x :: Word16)
      w32 :: Int -> Int -> IO ()
      w32 o x = pokeByteOff p o (fromIntegral x :: Word32)
      w64 :: Int -> Int64 -> IO ()
      w64 o x = pokeByteOff p o x
      uoff at target = w32 at (target - at)
  -- Message
  w32 0 16
  w16 4 12 >> w16 6 24 >> w16 8 4 >> w16 10 6 >> w16 12 8 >> w16 14 16
  w32 16 12
  pokeByteOff p 20 metadataVersionV5
  case mDict of
    Nothing -> do
      w8 22 3
      uoff 24 rbTable
    Just (DictHeader did isDelta) -> do
      w8 22 2
      uoff 24 56
      w16 40 10 >> w16 42 24 >> w16 44 8 >> w16 46 16 >> w16 48 (if isDelta then 20 else 0)
      w32 56 16
      w64 64 did
      uoff 72 rbTable
      when isDelta (w8 76 1)
  w64 32 bodyLen
  -- RecordBatch
  w16 rbVt rbVtSize
  w16 (rbVt + 2) rbTableSize
  w16 (rbVt + 4) 8 >> w16 (rbVt + 6) 16 >> w16 (rbVt + 8) 20
  when (rbFields >= 4) (w16 (rbVt + 10) (if hasComp then 24 else 0))
  when (rbFields >= 5) (w16 (rbVt + 12) 28)
  w32 rbTable (rbTable - rbVt)
  w64 (rbTable + 8) (rbLength rb)
  uoff (rbTable + 16) nodesCount
  uoff (rbTable + 20) bufsCount
  w32 nodesCount nN
  VS.unsafeWith (rbNodes rb) $ \src -> copyBytes (p `plusPtr` (nodesCount + 4)) (castPtr src) (16 * nN)
  w32 bufsCount nB
  VS.unsafeWith (rbBuffers rb) $ \src -> copyBytes (p `plusPtr` (bufsCount + 4)) (castPtr src) (16 * nB)
  case rbBodyCompression rb of
    Nothing -> pure ()
    Just codec -> do
      uoff (rbTable + 24) compTable
      w16 afterBufs 6 >> w16 (afterBufs + 2) 8 >> w16 (afterBufs + 4) 4
      w32 compTable (compTable - afterBufs)
      w8 (compTable + 4) (codecTag codec)
  when hasVar $ do
    uoff (rbTable + 28) varCount
    w32 varCount nV
    V.imapM_ (\i x -> w64 (varCount + 4 + 8 * i) x) (rbVariadicBufferCounts rb)
  where
    !pre = if encapsulated then 8 else 0
    !nN = VS.length (rbNodes rb)
    !nB = VS.length (rbBuffers rb)
    !nV = V.length (rbVariadicBufferCounts rb)
    !hasComp = isJust (rbBodyCompression rb)
    !hasVar = nV > 0
    !rbFields = if hasVar then 5 else if hasComp then 4 else 3 :: Int
    !rbVt = case mDict of
      Nothing -> 40
      Just _ -> 80
    !rbVtSize = 4 + 2 * rbFields
    !rbTable = align8 (rbVt + rbVtSize)
    !rbTableSize = if rbFields > 3 then 32 else 24
    !nodesCount = align8 (rbTable + rbTableSize + 4) - 4
    !bufsCount = align8 (nodesCount + 4 + 16 * nN + 4) - 4
    !afterBufs = bufsCount + 4 + 16 * nB
    !compTable = afterBufs + 8
    !afterComp = if hasComp then compTable + 8 else afterBufs
    !varCount = align8 (afterComp + 4) - 4
    !metaSize = if hasVar then varCount + 4 + 8 * nV else afterComp
    !metaPadded = align8 metaSize
    !total = if encapsulated then 8 + metaPadded else metaSize


codecTag :: BodyCompressionCodec -> Int
codecTag = \case
  LZ4Frame -> 0
  BodyZstd -> 1

-- ============================================================
-- Message envelope
-- ============================================================

-- | @
-- table Message {
--   version        : MetadataVersion;  // short, V5 = 4
--   header_type    : MessageHeader;    // ubyte (1=Schema, 3=RecordBatch)
--   header         : MessageHeader;    // union payload
--   bodyLength     : long;
--   custom_metadata: [KeyValue];
-- }
-- @
--
-- File-identifier is /not/ emitted for Arrow Messages; the
-- encapsulating stream framing distinguishes message boundaries.
buildSchemaMessage :: Schema -> ByteString
buildSchemaMessage sch = unsafePerformIO $ do
  b <- newBuilder
  schUOff <- writeSchema b sch
  msgUOff <- writeTable b
    [ Just (scalar 2 (\bb -> prependI16 bb metadataVersionV5))
    , Just (scalar 1 (\bb -> prependU8 bb 1))   -- header type: Schema
    , Just (voff schUOff)
    , Just (scalar 8 (\bb -> prependI64 bb 0))  -- bodyLength
    , Nothing
    ]
  finish b msgUOff
{-# NOINLINE buildSchemaMessage #-}

-- | The @Message@ flatbuffer of a record batch whose body is @bodyLen@ bytes.
buildRecordBatchMessage :: RecordBatchDef -> Int64 -> ByteString
buildRecordBatchMessage = batchMessage False Nothing

-- | Build a @Message@ flatbuffer wrapping a @DictionaryBatch@:
--
-- @
-- table DictionaryBatch {
--   id      : long;          // 0
--   data    : RecordBatch;   // 1
--   isDelta : bool = false;  // 2
-- }
-- @
--
-- The @bodyLength@ is the batch body's length padded to 8 bytes.
buildDictionaryBatchMessage :: DictBatch -> ByteString
buildDictionaryBatchMessage (DictBatch did isDelta rb body) =
  batchMessage False (Just (DictHeader did isDelta)) rb (fromIntegral (align8 (BS.length body)))

metadataVersionV5 :: Int16
metadataVersionV5 = 4

-- | Build a @Message@ flatbuffer wrapping a @Tensor@:
--
-- @
-- table Tensor {
--   type_type: Type;       // slot 0 (union tag)
--   type:      Type;       // slot 1
--   shape:    [TensorDim]; // slot 2
--   strides:  [long];      // slot 3
--   data:      Buffer;     // slot 4 (struct)
-- }
-- @
buildTensorMessage :: Tensor -> ByteString
buildTensorMessage t = unsafePerformIO $ do
  b <- newBuilder
  (tyTag, tyUOff) <- writeType b (tensorType t)
  -- Each TensorDim is a table (not a struct) because @name@ is a
  -- variable-length string.
  dimUOffs <- mapM (writeTensorDim b) (V.toList (tensorShape t))
  shapeVec <- writeVectorOfOffsets b dimUOffs
  stridesVec <- if V.null (tensorStrides t)
                  then pure Nothing
                  else Just <$> writeVectorInt64 b (V.toList (tensorStrides t))
  -- Data Buffer struct: i64 offset (always 0, we emit body
  -- right after the metadata) + i64 length.
  let !bodyLen = fromIntegral (BS.length (tensorBody t)) :: Int64
  tensorUOff <- writeTable b
    [ Just (scalar 1 (\bb -> prependU8 bb tyTag))
    , Just (voff tyUOff)
    , Just (voff shapeVec)
    , fmap voff stridesVec
    , Just (struct 16 8 (\bb -> do
        prependI64 bb bodyLen
        prependI64 bb 0))
    ]
  -- Now wrap in a Message table: header_type = 4 (Tensor).
  msgUOff <- writeTable b
    [ Just (scalar 2 (\bb -> prependI16 bb metadataVersionV5))
    , Just (scalar 1 (\bb -> prependU8 bb 4))
    , Just (voff tensorUOff)
    , Just (scalar 8 (\bb -> prependI64 bb bodyLen))
    , Nothing
    ]
  finish b msgUOff
{-# NOINLINE buildTensorMessage #-}

-- | Encode one @TensorDim@ as a flatbuffer table.
writeTensorDim :: Builder -> TensorDim -> IO Int
writeTensorDim b (TensorDim size name) = do
  nameOff <- if T.null name
               then pure Nothing
               else Just <$> writeString b name
  writeTable b
    [ Just (scalar 8 (\bb -> prependI64 bb size))
    , fmap voff nameOff
    ]

-- | Build a SparseTensor @Message@ flatbuffer (COO index format).
-- The body of the encapsulated frame carries indices followed by
-- values, concatenated; callers must handle the per-buffer
-- offsets on the encoding side.
buildSparseTensorMessageCOO :: SparseTensor -> ByteString
buildSparseTensorMessageCOO st = unsafePerformIO $ do
  b <- newBuilder
  (tyTag, tyUOff) <- writeType b (sparseTensorType st)
  dimUOffs <- mapM (writeTensorDim b) (V.toList (sparseTensorShape st))
  shapeVec <- writeVectorOfOffsets b dimUOffs
  -- Indices buffer struct (offset = 0, length)
  let !indicesLen = fromIntegral (BS.length (sparseIndicesBody st)) :: Int64
      !valuesLen  = fromIntegral (BS.length (sparseTensorBody  st)) :: Int64
      !indicesOffset = 0 :: Int64
      !valuesOffset  = alignUp (BS.length (sparseIndicesBody st)) 8
  (_, idxTypeUOff) <- writeType b (sparseIndicesType st)
  cooUOff <- writeTable b
    [ Just (voff idxTypeUOff)
    , Nothing                   -- indicesStrides (empty)
    , Just (struct 16 8 (\bb -> do
        prependI64 bb indicesLen
        prependI64 bb indicesOffset))
    , if sparseIndicesCanonical st
        then Just (scalar 1 (\bb -> prependU8 bb 1)) else Nothing
    ]
  -- SparseTensor table
  stUOff <- writeTable b
    [ Just (scalar 1 (\bb -> prependU8 bb tyTag))
    , Just (voff tyUOff)
    , Just (voff shapeVec)
    , Just (scalar 8 (\bb -> prependI64 bb (sparseNonZeroLength st)))
    , Just (scalar 1 (\bb -> prependU8 bb 1))   -- SparseTensorIndex = COO
    , Just (voff cooUOff)
    , Just (struct 16 8 (\bb -> do
        prependI64 bb valuesLen
        prependI64 bb (fromIntegral valuesOffset)))
    ]
  msgUOff <- writeTable b
    [ Just (scalar 2 (\bb -> prependI16 bb metadataVersionV5))
    , Just (scalar 1 (\bb -> prependU8 bb 5))   -- header type: SparseTensor
    , Just (voff stUOff)
    , Just (scalar 8 (\bb -> prependI64 bb (indicesLen + fromIntegral valuesOffset - indicesLen + valuesLen)))
    , Nothing
    ]
  finish b msgUOff
{-# NOINLINE buildSparseTensorMessageCOO #-}

-- | Encode a sparse tensor as a standalone Arrow IPC frame.
-- The encapsulated body is @indicesBody <pad8> tensorBody@, so
-- the body offsets recorded in the message match the slice
-- layout.
encodeSparseTensorFrame :: SparseTensor -> ByteString
encodeSparseTensorFrame st =
  let !iLen   = BS.length (sparseIndicesBody st)
      !iPad   = alignUp iLen 8 - iLen
      !body   = BS.concat
                  [ sparseIndicesBody st
                  , BS.replicate iPad 0
                  , sparseTensorBody  st
                  ]
  in  encapsulateMessage (buildSparseTensorMessageCOO st) body

-- | Encode a 'Tensor' as a standalone Arrow IPC frame:
-- continuation + metadata length + padded flatbuffer + body
-- + body padding. Suitable as a file payload or as one element
-- of an application-level container; Arrow's stream framing
-- itself accepts Tensor messages interleaved with any other
-- Message.
encodeTensorFrame :: Tensor -> ByteString
encodeTensorFrame t = encapsulateMessage (buildTensorMessage t) (tensorBody t)

-- ============================================================
-- Encapsulated framing
-- ============================================================

-- | Wrap a raw flatbuffer @Message@ in the encapsulated IPC frame
-- (one allocation):
--
-- @
-- <continuation 0xFFFFFFFF : u32-LE>
-- <metadata_length : i32-LE, padded so body starts aligned to 8>
-- <flatbuffer bytes, padded>
-- <body bytes, padded to 8-byte alignment>
-- @
encapsulateMessage :: ByteString -> ByteString -> ByteString
encapsulateMessage meta body =
  let !metaLen = BS.length meta
      !padded = align8 metaLen
      !bodyLen = BS.length body
      !total = 8 + padded + align8 bodyLen
  in BSI.unsafeCreate total $ \p -> do
       pokeByteOff p 0 (0xFFFFFFFF :: Word32)
       pokeByteOff p 4 (fromIntegral padded :: Int32)
       copyInto (p `plusPtr` 8) meta
       fillBytes (p `plusPtr` (8 + metaLen)) 0 (padded - metaLen)
       copyInto (p `plusPtr` (8 + padded)) body
       fillBytes (p `plusPtr` (8 + padded + bodyLen)) 0 (total - 8 - padded - bodyLen)


copyInto :: Ptr Word8 -> ByteString -> IO ()
copyInto dst bs = BSU.unsafeUseAsCStringLen bs $ \(src, n) -> copyBytes dst (castPtr src) n
{-# INLINE copyInto #-}


-- ============================================================
-- Frames and layout
-- ============================================================

{- | One encapsulated message ready to be laid out: its header
(continuation marker, metadata length, metadata, zero padding; a
multiple of 8 bytes) and its body pieces. On output every piece is
followed by zero padding to a multiple of 8, and 'frameBodyLength' is
the padded total (the @bodyLength@ the header declares).
-}
data Frame = Frame
  { frameHeader :: !ByteString
  , framePieces :: !(V.Vector ByteString)
  , frameBodyLength :: !Int
  }


frameSize :: Frame -> Int
frameSize fr = BS.length (frameHeader fr) + frameBodyLength fr


-- | The schema message frame (no body).
schemaFrame :: Schema -> Frame
schemaFrame sch = Frame (encapsulateMessage (buildSchemaMessage sch) BS.empty) V.empty 0


{- | A record batch laid out for the wire: the header definition (buffer
offsets relative to the body start, 8-byte aligned) and the body pieces
(one per buffer, compressed when the definition says so).
-}
data EncodedBatch = EncodedBatch
  { ebDef :: !RecordBatchDef
  , ebPieces :: !(V.Vector ByteString)
  , ebBodyLength :: !Int
  }


{- | Plan a batch (see 'Arrow.Write.Columns.planBatch') and compress its
pieces when a codec is requested. A codec that was not compiled in
(see 'bodyCompressionAvailable') is ignored, so the batch is always a
valid uncompressed one.
-}
encodeBatch :: Maybe BodyCompressionCodec -> V.Vector Field -> V.Vector ColumnArray -> Either String EncodedBatch
encodeBatch mCodec0 fields cols = do
  plan <- planBatch fields cols
  let !mCodec = case mCodec0 of
        Just codec | bodyCompressionAvailable codec -> Just codec
        _ -> Nothing
  pieces <- case mCodec of
    Nothing -> Right (bpPieces plan)
    Just codec -> V.mapM (compressPiece codec) (bpPieces plan)
  let !(bufs, bodyLen) = layoutBuffers pieces
  Right
    EncodedBatch
      { ebDef =
          RecordBatchDef
            { rbLength = bpLength plan
            , rbNodes = bpNodes plan
            , rbBuffers = bufs
            , rbVariadicBufferCounts = bpVariadic plan
            , rbBodyCompression = mCodec
            }
      , ebPieces = pieces
      , ebBodyLength = bodyLen
      }


-- | Buffer descriptors for pieces laid out back to back, each padded to 8 bytes, and the padded total.
layoutBuffers :: V.Vector ByteString -> (VS.Vector Buffer, Int)
layoutBuffers pieces =
  let !lens = VS.generate (V.length pieces) (BS.length . V.unsafeIndex pieces)
      !offs = VS.prescanl' (\o l -> o + align8 l) 0 lens
      !bufs = VS.zipWith (\o l -> Buffer (fromIntegral o) (fromIntegral l)) offs lens
  in (bufs, VS.foldl' (\t l -> t + align8 l) 0 lens)


-- | The frame of an encoded record batch.
batchFrame :: EncodedBatch -> Frame
batchFrame eb =
  Frame (batchMessage True Nothing (ebDef eb) (fromIntegral (ebBodyLength eb))) (ebPieces eb) (ebBodyLength eb)


-- | The frame of an encoded dictionary batch (id, isDelta, the values batch).
dictionaryFrame :: Int64 -> Bool -> EncodedBatch -> Frame
dictionaryFrame did isDelta eb =
  Frame
    (batchMessage True (Just (DictHeader did isDelta)) (ebDef eb) (fromIntegral (ebBodyLength eb)))
    (ebPieces eb)
    (ebBodyLength eb)


-- | A frame for a definition and a pre-built body (one piece).
rawBatchFrame :: Maybe DictHeader -> RecordBatchDef -> ByteString -> Frame
rawBatchFrame mDict rb body =
  let !bodyLen = align8 (BS.length body)
  in Frame (batchMessage True mDict rb (fromIntegral bodyLen)) (V.singleton body) bodyLen


-- | Write a frame at @off@; returns the offset after it.
pokeFrame :: Ptr Word8 -> Int -> Frame -> IO Int
pokeFrame p off fr = do
  let !h = frameHeader fr
  copyInto (p `plusPtr` off) h
  V.foldM'
    ( \o pc -> do
        let !l = BS.length pc
            !padded = align8 l
        copyInto (p `plusPtr` o) pc
        fillBytes (p `plusPtr` (o + l)) 0 (padded - l)
        pure (o + padded)
    )
    (off + BS.length h)
    (framePieces fr)


-- | The end-of-stream marker: continuation, zero metadata length.
eosBytes :: ByteString
eosBytes = BS.pack [0xFF, 0xFF, 0xFF, 0xFF, 0, 0, 0, 0]
{-# NOINLINE eosBytes #-}


-- | Zero bytes the lazy layouts slice their padding from.
zeroPad :: ByteString
zeroPad = BS.replicate 8 0
{-# NOINLINE zeroPad #-}


{- | A stream (the frames in order, then the end-of-stream marker) in
one allocation; every body byte is copied once.
-}
renderStream :: [Frame] -> ByteString
renderStream frames =
  let !total = foldl (\t fr -> t + frameSize fr) (BS.length eosBytes) frames
  in BSI.unsafeCreate total $ \p -> do
       end <- foldM (pokeFrame p) 0 frames
       copyInto (p `plusPtr` end) eosBytes


-- | Lazy chunks of one frame: the header, then each piece (aliased) and its padding.
frameChunks :: Frame -> [ByteString]
frameChunks fr = frameHeader fr : V.foldr (\pc rest -> pc : padChunk (BS.length pc) rest) [] (framePieces fr)
  where
    padChunk l rest =
      let !k = align8 l - l
      in if k == 0 then rest else BS.take k zeroPad : rest


{- | 'renderStream' as a lazy 'BL.ByteString' whose chunks are the frame
headers, the body pieces themselves (no copy), and shared padding.
-}
renderStreamLazy :: [Frame] -> BL.ByteString
renderStreamLazy frames = BL.fromChunks (concatMap frameChunks frames ++ [eosBytes])


-- | The leading and trailing file magic (@ARROW1@ plus two padding bytes).
fileMagic :: ByteString
fileMagic = "ARROW1\0\0"
{-# NOINLINE fileMagic #-}


{- | The file layout around the stream: the footer (indexing the dictionary
and record batch frames) and its trailer (footer length, @ARROW1@).
-}
fileFooter :: Schema -> [Frame] -> [Frame] -> ByteString
fileFooter sch dictFrames batchFrames =
  let !schemaSize = frameSize (schemaFrame sch)
      blocks off0 frs = reverse (snd (foldl (\(!o, acc) fr -> (o + frameSize fr, block o fr : acc)) (off0, []) frs))
      block o fr =
        ArrowBlock
          { abOffset = fromIntegral o
          , abMetaLen = fromIntegral (BS.length (frameHeader fr))
          , abBodyLen = fromIntegral (frameBodyLength fr)
          }
      !dictStart = BS.length fileMagic + schemaSize
      !batchStart = foldl (\t fr -> t + frameSize fr) dictStart dictFrames
      !footer = buildFileFooter sch (blocks dictStart dictFrames) (blocks batchStart batchFrames)
      !trailer = BSI.unsafeCreate 10 $ \p -> do
        pokeByteOff p 0 (fromIntegral (BS.length footer) :: Int32)
        copyInto (p `plusPtr` 4) (BS.take 6 fileMagic)
  in footer <> trailer


{- | An Arrow IPC /file/ (per @format/File.fbs@) in one allocation:

@
'ARROW1'\\0\\0
<schema message> <dictionary batches> <record batches> <EOS>
<Footer flatbuffer> <i32 footer length> 'ARROW1'
@

Each footer 'Block' points at the continuation marker of its message,
with the header length (marker, length, metadata, padding) and the
padded body length. The EOS marker is kept so the payload also parses
as a stream.
-}
renderFile :: Schema -> [Frame] -> [Frame] -> ByteString
renderFile sch dictFrames batchFrames =
  let !frames = schemaFrame sch : dictFrames ++ batchFrames
      !footer = fileFooter sch dictFrames batchFrames
      !total = foldl (\t fr -> t + frameSize fr) (BS.length fileMagic + BS.length eosBytes + BS.length footer) frames
  in BSI.unsafeCreate total $ \p -> do
       copyInto p fileMagic
       end <- foldM (pokeFrame p) (BS.length fileMagic) frames
       copyInto (p `plusPtr` end) eosBytes
       copyInto (p `plusPtr` (end + BS.length eosBytes)) footer


-- | 'renderFile' as lazy chunks aliasing the body pieces (see 'renderStreamLazy').
renderFileLazy :: Schema -> [Frame] -> [Frame] -> BL.ByteString
renderFileLazy sch dictFrames batchFrames =
  BL.fromChunks
    ( fileMagic
        : concatMap frameChunks (schemaFrame sch : dictFrames ++ batchFrames)
        ++ [eosBytes, fileFooter sch dictFrames batchFrames]
    )


-- ============================================================
-- Low-level writers over pre-built bodies
-- ============================================================

-- | Emit a complete Arrow IPC stream (schema + batches + EOS).
writeArrowStreamFB :: Schema -> [(RecordBatchDef, ByteString)] -> ByteString
writeArrowStreamFB sch = writeArrowStreamFBWithDicts sch []


-- | Emit a stream with the dictionary batches (in the order given)
-- ahead of the record batches. Each batch carries an @id@ that must
-- match a @DictionaryEncoding.id@ of the schema.
writeArrowStreamFBWithDicts :: Schema -> [DictBatch] -> [(RecordBatchDef, ByteString)] -> ByteString
writeArrowStreamFBWithDicts sch dicts batches =
  renderStream (schemaFrame sch : map dictBatchFrame dicts ++ map (\(rb, body) -> rawBatchFrame Nothing rb body) batches)


dictBatchFrame :: DictBatch -> Frame
dictBatchFrame (DictBatch did isDelta rb body) = rawBatchFrame (Just (DictHeader did isDelta)) rb body


-- | Arrow IPC file with the given record batches (see 'renderFile').
writeArrowFileFB :: Schema -> [(RecordBatchDef, ByteString)] -> ByteString
writeArrowFileFB sch = writeArrowFileFBWithDicts sch []


-- | Arrow IPC file with dictionary batches indexed in the footer.
writeArrowFileFBWithDicts :: Schema -> [DictBatch] -> [(RecordBatchDef, ByteString)] -> ByteString
writeArrowFileFBWithDicts sch dicts batches =
  renderFile sch (map dictBatchFrame dicts) (map (\(rb, body) -> rawBatchFrame Nothing rb body) batches)


-- | The header and a contiguous body (one allocation) for a batch; see 'buildRecordBatchBytesWith'.
buildRecordBatchBytes :: Schema -> V.Vector ColumnArray -> Either String (RecordBatchDef, ByteString)
buildRecordBatchBytes = buildRecordBatchBytesWith Nothing


{- | Lay out a batch (spec buffer layout, 8-byte aligned buffers) and
copy its pieces into one contiguous body. With a codec each buffer is
compressed independently (Arrow's @BodyCompression@ = @BUFFER@) and
'rbBodyCompression' is set; a codec that was not compiled in is ignored.
The columns are not checked against the schema beyond what the layout
needs; see 'Arrow.Write.Columns.validateColumns'.
-}
buildRecordBatchBytesWith
  :: Maybe BodyCompressionCodec
  -> Schema
  -> V.Vector ColumnArray
  -> Either String (RecordBatchDef, ByteString)
buildRecordBatchBytesWith mCodec sch cols = do
  eb <- encodeBatch mCodec (arrowFields sch) cols
  Right (ebDef eb, concatPieces (ebBodyLength eb) (ebPieces eb))


concatPieces :: Int -> V.Vector ByteString -> ByteString
concatPieces total pieces = BSI.unsafeCreate total $ \p -> do
  _ <- pokeFrame p 0 (Frame BS.empty pieces total)
  pure ()


-- | A stream from column batches (no schema checks; see "Arrow.Stream" for the checked writer).
writeArrowStreamFBFromColumns :: Schema -> V.Vector (V.Vector ColumnArray) -> Either String ByteString
writeArrowStreamFBFromColumns sch batches = do
  ebs <- traverse (encodeBatch Nothing (arrowFields sch)) (V.toList batches)
  Right (renderStream (schemaFrame sch : map batchFrame ebs))


-- ============================================================
-- Body compression
-- ============================================================

{- | Apply Arrow's @BodyCompression = BUFFER@ scheme to a body: every
buffer slice is replaced by its envelope (see 'compressPiece') and the
buffers are laid out again, 8-aligned.
-}
compressBody
  :: BodyCompressionCodec
  -> VS.Vector Buffer
  -> ByteString
  -> Either String (VS.Vector Buffer, ByteString)
compressBody codec bufs body = do
  let slice (Buffer o l) = BS.take (fromIntegral l) (BS.drop (fromIntegral o) body)
  pieces <- V.mapM (compressPiece codec . slice) (V.convert bufs)
  let !(bufs', total) = layoutBuffers pieces
  Right (bufs', concatPieces total pieces)


{- | One buffer per Arrow's @BUFFER@ method: an 8-byte little-endian
uncompressed length followed by the compressed bytes, or length @-1@
and the raw bytes when compression does not shrink them (the spec's
escape hatch). Empty buffers stay empty.
-}
compressPiece :: BodyCompressionCodec -> ByteString -> Either String ByteString
compressPiece codec raw
  | BS.null raw = Right raw
  | otherwise = do
      compressed <- compressBufferEither codec raw
      let !rawLen = BS.length raw
          !(prefix, payload)
            | BS.length compressed >= rawLen = (-1, raw)
            | otherwise = (fromIntegral rawLen, compressed)
      Right $ BSI.unsafeCreate (8 + BS.length payload) $ \p -> do
        pokeByteOff p 0 (prefix :: Int64)
        copyInto (p `plusPtr` 8) payload


-- | Compress a single buffer's bytes. Routes to the right
-- codec backend; @-fzstd@ / @-flz4@ Cabal flags select
-- availability.
--
-- Returns 'Left' when the requested codec wasn't compiled in.
compressBufferEither
  :: BodyCompressionCodec -> ByteString
  -> Either String ByteString
compressBufferEither codec bs = case codec of
#ifdef HAVE_ZSTD
  BodyZstd ->
    Right (Zstd.compress 3 bs)   -- level 3 matches arrow-cpp's default
#else
  BodyZstd -> Left "Arrow.FlatBufferIPC: ZSTD body compression requires building wireform-arrow with -fzstd"
#endif
#ifdef HAVE_LZ4
  LZ4Frame ->
    -- lz4-hs's Codec.Lz4.compress produces the official
    -- LZ4_Frame format (magic 0x184D2204 + frame descriptor +
    -- one-or-more blocks), which is what arrow-cpp / pyarrow
    -- consume for BodyCompression codec=0 (LZ4_FRAME). The API
    -- works on lazy ByteStrings under the hood.
    Right (BL.toStrict (Lz4.compress (BL.fromStrict bs)))
#else
  LZ4Frame -> Left "Arrow.FlatBufferIPC: LZ4 body compression requires building wireform-arrow with -flz4"
#endif

-- | Predicate the caller can use to fail-fast before invoking
-- the writer on a codec that isn't compiled in.
bodyCompressionAvailable :: BodyCompressionCodec -> Bool
bodyCompressionAvailable = \case
#ifdef HAVE_ZSTD
  BodyZstd -> True
#else
  BodyZstd -> False
#endif
#ifdef HAVE_LZ4
  LZ4Frame -> True
#else
  LZ4Frame -> False
#endif

-- | A 'Block' struct as emitted in the @Footer.recordBatches@
-- vector. Inline fixed-size struct (24 bytes total): offset i64,
-- metaDataLength i32, bodyLength i64.
data ArrowBlock = ArrowBlock
  { abOffset  :: !Int64
  , abMetaLen :: !Int32
  , abBodyLen :: !Int64
  }

-- | Build the @Footer@ flatbuffer:
--
-- @
-- table Footer {
--   version       : MetadataVersion;   // i16
--   schema        : Schema;            // table uoffset
--   dictionaries  : [Block];           // vector of structs (struct size = 24 with padding)
--   recordBatches : [Block];
--   custom_metadata : [KeyValue];
-- }
-- @
--
-- @Block@ is a struct with layout
-- @offset: i64; metaDataLength: i32; bodyLength: i64;@. The
-- @metaDataLength@ field is 4 bytes wide but the struct is
-- 8-aligned, so each Block occupies 24 bytes (8 + 4 + 4 padding +
-- 8). FlatBuffers structs are compiler-generated, but we hand-roll
-- the same layout here.
buildFileFooter :: Schema -> [ArrowBlock] -> [ArrowBlock] -> ByteString
buildFileFooter sch dictBlocks rbBlocks = unsafePerformIO $ do
  b <- newBuilder
  schUOff <- writeSchema b sch
  rbVec   <- writeVectorOfStructs b 24 8 (map writeBlockStruct rbBlocks)
  dictVec <- if null dictBlocks
               then pure Nothing
               else Just <$> writeVectorOfStructs b 24 8
                                (map writeBlockStruct dictBlocks)
  msgUOff <- writeTable b
    [ Just (scalar 2 (\bb -> prependI16 bb metadataVersionV5))
    , Just (voff schUOff)
    , case dictVec of { Nothing -> Nothing; Just uo -> Just (voff uo) }
    , Just (voff rbVec)
    , Nothing                        -- custom_metadata
    ]
  finish b msgUOff
{-# NOINLINE buildFileFooter #-}

writeBlockStruct :: ArrowBlock -> Builder -> IO ()
writeBlockStruct (ArrowBlock o ml bl) bb = do
  -- Reverse order: bodyLength (i64), 4-byte pad, metaDataLength
  -- (i32), offset (i64).
  prependI64 bb bl
  prependBS  bb (BS.replicate 4 0)
  prependI32 bb ml
  prependI64 bb o


-- | Encapsulated frame (continuation marker, length, FlatBuffers
-- @Message@ metadata, padding) for one 'Message'. The body is not
-- included: the metadata's @bodyLength@ is the 8-aligned extent of
-- the batch's buffers, and the caller appends that many body bytes.
encodeMessageFrame :: Message -> ByteString
encodeMessageFrame = \case
  SchemaMessage sch -> encapsulateMessage (buildSchemaMessage sch) BS.empty
  RecordBatch rb -> batchMessage True Nothing rb (bodyExtent rb)
  DictionaryBatch did isDelta rb -> batchMessage True (Just (DictHeader did isDelta)) rb (bodyExtent rb)
  where
    bodyExtent rb =
      let !end = VS.foldl' (\m buf -> max m (bufOffset buf + bufLength buf)) 0 (rbBuffers rb)
      in (end + 7) .&. complement 7
