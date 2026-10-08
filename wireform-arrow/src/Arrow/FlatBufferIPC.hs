{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
-- | Real Apache Arrow IPC framing — binary-compatible with the
-- reference implementations (arrow-cpp, arrow-rs, pyarrow).
--
-- "Arrow.IPC" uses a simplified flatbuffer-shaped encoding that
-- only self-round-trips; this module constructs Arrow's metadata
-- tables as actual FlatBuffers (per @format/Schema.fbs@ and
-- @format/Message.fbs@) and emits the encapsulated-message framing
-- so pyarrow / arrow-rs / arrow-cpp can consume the output.
--
-- The /generic/ flatbuffer primitives (back-to-front 'Builder',
-- vtable dedup, soffset chains, reader peek + table resolution)
-- live in "FlatBuffers.Builder" and "FlatBuffers.Reader" inside
-- @wireform-flatbuffers@. This module only owns the
-- Arrow-specific layout — the @Schema@, @Field@, @Type@,
-- @RecordBatch@, @Message@, @Tensor@, @SparseTensor@ tables, plus
-- the encapsulated-frame / file-format glue. That split lets
-- "FlatBuffers.Encode" / "FlatBuffers.Decode" stay focused on
-- value-shaped use cases while the spec-precise encoder is
-- shared rather than reimplemented per call site.
--
-- The encoder is standards-compliant:
--
--   * Buffer is built back-to-front.
--   * Tables carry a signed int32 soffset to their vtable at offset 0.
--   * Vtables share when structurally identical (via a deduplication map).
--   * Scalars are aligned to their width; vectors/strings/tables
--     are 4-aligned.
--   * The root offset at byte 0 is an unsigned uoffset_t pointing to
--     the root table.
module Arrow.FlatBufferIPC
  ( -- * Top-level builders
    buildSchemaMessage
  , buildRecordBatchMessage
    -- * Encapsulated-message framing
  , encapsulateMessage
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
  , decompressBody
    -- * Reader (parses pyarrow / arrow-cpp output)
  , readArrowStreamFB
  , readArrowFileFB
  , readArrowFileFBWithDicts
  , decodeSchemaMessage
  , decodeRecordBatchMessage
  , decodeDictionaryBatchMessage
  , denormaliseBuffers
  , materializeRecordBatchFB
    -- * Dictionary support
  , DictBatch (..)
  , readArrowStreamFBWithDicts
  , readArrowStreamFBInterleaved
  , StreamFrame (..)
  , buildDictionaryBatchMessage
    -- * Tensor / SparseTensor
  , Tensor (..)
  , TensorDim (..)
  , buildTensorMessage
  , decodeTensorMessage
  , encodeTensorFrame
  , decodeTensorFrame
  , SparseTensor (..)
  , buildSparseTensorMessageCOO
  , encodeSparseTensorFrame
  , decodeSparseTensorFrame
  , writeArrowStreamFBWithDicts
    -- * Single-message frames
  , encodeMessageFrame
  , decodeMessageFrame
  , readFrameHeader
  ) where

import Data.Bits ((.&.), (.|.), complement, shiftL)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Wireform.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int16, Int32, Int64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Word (Word8)
import System.IO.Unsafe (unsafePerformIO)

import Arrow.Column (ColumnArray (..), columnLength, isNullableColumn, materializeRecordBatch)
import Arrow.Types
import qualified Arrow.Write.Columns as W
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
import FlatBuffers.Reader
  ( Pos
  , followUOffset
  , peekI16
  , peekI32
  , peekI64
  , peekU8
  , peekU32
  , readString
  , readStringSlice
  , readVectorInt64
  , readVectorOfOffsets
  , readVectorOfStructs
  , resolveTable
  , vectorLength
  )

#ifdef HAVE_ZSTD
import qualified Codec.Compression.Zstd as Zstd
#endif

#ifdef HAVE_LZ4
import qualified Codec.Lz4 as Lz4
import qualified Control.Exception as Exc
#endif


-- | Mini-helper: run @e@ when the condition holds, else 'Right' '()'.
-- (This is shaped like 'Control.Monad.when' but specialised to
-- 'Either String' and lifted to a top-level binding so all the
-- decoders below can share one definition.)
when' :: Bool -> Either String () -> Either String ()
when' True  e = e
when' False _ = Right ()

-- | Slice @len@ bytes at @off@ out of @bs@, where both come off the
-- wire: negative or out-of-range values are a 'Left', never a
-- silently clamped slice.
sliceWire :: String -> Int64 -> Int64 -> ByteString -> Either String ByteString
sliceWire what off len bs
  | off < 0 || len < 0 || len > n || off > n - len =
      Left
        ( "Arrow.FlatBufferIPC: "
            ++ what
            ++ " [offset "
            ++ show off
            ++ ", length "
            ++ show len
            ++ "] lies outside the "
            ++ show n
            ++ " available bytes"
        )
  | otherwise = Right $! BS.take (fromIntegral len) (BS.drop (fromIntegral off) bs)
  where
    n = fromIntegral (BS.length bs) :: Int64

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
-- RecordBatch
-- ============================================================

writeFieldNodeStruct :: FieldNode -> Builder -> IO ()
writeFieldNodeStruct fn b = do
  -- Struct layout: length (i64), null_count (i64). We're writing
  -- back-to-front so write null_count first, then length.
  prependI64 b (fnNullCount fn)
  prependI64 b (fnLength fn)

writeBufferStruct :: Buffer -> Builder -> IO ()
writeBufferStruct bf b = do
  prependI64 b (bufLength bf)
  prependI64 b (bufOffset bf)

writeRecordBatch :: Builder -> RecordBatchDef -> IO Int
writeRecordBatch b rb = do
  variadicVec <- if V.null (rbVariadicBufferCounts rb)
    then pure Nothing
    else do
      uo <- writeVectorInt64 b (V.toList (rbVariadicBufferCounts rb))
      pure (Just uo)
  buffersVec <- writeVectorOfStructs b 16 8
                  [ writeBufferStruct buf | buf <- V.toList (rbBuffers rb) ]
  nodesVec   <- writeVectorOfStructs b 16 8
                  [ writeFieldNodeStruct fn | fn <- V.toList (rbNodes rb) ]
  -- BodyCompression (slot 3): table { codec: i8 (default 0), method: i8 (default 0) }.
  bodyCompVec <- case rbBodyCompression rb of
    Nothing -> pure Nothing
    Just codec -> do
      uo <- writeTable b
              [ Just (scalar 1 (\bb -> prependU8 bb (case codec of
                                                       LZ4Frame -> 0
                                                       BodyZstd -> 1)))
              , Nothing  -- method = BUFFER (default)
              ]
      pure (Just uo)
  writeTable b
    [ Just (scalar 8 (\bb -> prependI64 bb (rbLength rb)))
    , Just (voff nodesVec)
    , Just (voff buffersVec)
    , case bodyCompVec of { Nothing -> Nothing; Just uo -> Just (voff uo) }
    , case variadicVec of { Nothing -> Nothing; Just uo -> Just (voff uo) }
    ]

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

buildRecordBatchMessage :: RecordBatchDef -> Int64 -> ByteString
buildRecordBatchMessage rb bodyLen = unsafePerformIO $ do
  b <- newBuilder
  rbUOff <- writeRecordBatch b rb
  msgUOff <- writeTable b
    [ Just (scalar 2 (\bb -> prependI16 bb metadataVersionV5))
    , Just (scalar 1 (\bb -> prependU8 bb 3))   -- header type: RecordBatch
    , Just (voff rbUOff)
    , Just (scalar 8 (\bb -> prependI64 bb bodyLen))
    , Nothing
    ]
  finish b msgUOff
{-# NOINLINE buildRecordBatchMessage #-}

-- | Build a @Message@ flatbuffer wrapping a @DictionaryBatch@:
--
-- @
-- table DictionaryBatch {
--   id      : long;          // 0
--   data    : RecordBatch;   // 1
--   isDelta : bool = false;  // 2
-- }
-- @
buildDictionaryBatchMessage :: DictBatch -> ByteString
buildDictionaryBatchMessage (DictBatch did isDelta rb body) =
  buildDictionaryBatchMessageLen did isDelta rb (fromIntegral (BS.length body))


-- | 'buildDictionaryBatchMessage' given the body length instead of the body.
buildDictionaryBatchMessageLen :: Int64 -> Bool -> RecordBatchDef -> Int64 -> ByteString
buildDictionaryBatchMessageLen did isDelta rb bodyLen = unsafePerformIO $ do
  b <- newBuilder
  rbUOff  <- writeRecordBatch b rb
  dbUOff  <- writeTable b
    [ Just (scalar 8 (\bb -> prependI64 bb did))
    , Just (voff rbUOff)
    , if isDelta then Just (scalar 1 (\bb -> prependU8 bb 1)) else Nothing
    ]
  msgUOff <- writeTable b
    [ Just (scalar 2 (\bb -> prependI16 bb metadataVersionV5))
    , Just (scalar 1 (\bb -> prependU8 bb 2))   -- header type: DictionaryBatch
    , Just (voff dbUOff)
    , Just (scalar 8 (\bb -> prependI64 bb bodyLen))
    , Nothing
    ]
  finish b msgUOff
{-# NOINLINE buildDictionaryBatchMessageLen #-}

metadataVersionV5 :: Int16
metadataVersionV5 = 4

-- ============================================================
-- Tensor / SparseTensor messages
-- ============================================================

-- | Dense tensor metadata. Mirrors @table Tensor@ from Arrow's
-- @format/Tensor.fbs@. @tensorBody@ is the raw buffer carrying
-- @product(shape) * bytesPerElement(tensorType)@ little-endian
-- elements in row-major order (unless 'tensorStrides' is set).
data Tensor = Tensor
  { tensorType    :: !ArrowType
  , tensorShape   :: !(V.Vector TensorDim)
  , tensorStrides :: !(V.Vector Int64)
  , tensorBody    :: !ByteString
  } deriving (Show, Eq)

-- | One entry of a tensor's shape vector. Name is optional
-- (empty string by default).
data TensorDim = TensorDim
  { tdSize :: !Int64
  , tdName :: !T.Text
  } deriving (Show, Eq)

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

-- | Parse a @Tensor@ message header (flatbuffer metadata only;
-- the body bytes are the caller's concern — they sit in the
-- encapsulated frame's body section).
decodeTensorMessage
  :: ByteString -> Either String (Tensor, Int64)
decodeTensorMessage meta = do
  msgPos <- fromIntegral <$> peekU32 meta 0
  mSlot  <- resolveTable meta msgPos
  ht <- case mSlot 1 of
    Nothing -> Right (0 :: Int)
    Just b  -> fromIntegral <$> peekU8 meta b
  when' (ht /= 4) $
    Left ("Arrow.FlatBufferIPC.decodeTensorMessage: expected header_type=4, got "
          ++ show ht)
  headerPos <- case mSlot 2 of
    Nothing  -> Left "Tensor message missing header slot"
    Just off -> Right off
  tensorPos <- followUOffset meta headerPos
  tSlot     <- resolveTable meta tensorPos
  tyTag <- case tSlot 0 of
    Nothing -> Left "Tensor missing type_type"
    Just b  -> fromIntegral <$> peekU8 meta b
  tyFieldPos <- case tSlot 1 of
    Nothing  -> Left "Tensor missing type"
    Just off -> Right off
  tyPos <- followUOffset meta tyFieldPos
  arrowTy <- readType meta tyTag (Just tyPos)
  shape <- case tSlot 2 of
    Nothing  -> Right V.empty
    Just off -> do
      shapePos <- followUOffset meta off
      dimOffs  <- readVectorOfOffsets meta shapePos
      V.mapM (\dimPos -> readTensorDim meta dimPos) dimOffs
  strides <- case tSlot 3 of
    Nothing  -> Right V.empty
    Just off -> do
      sPos <- followUOffset meta off
      V.fromList <$> readVectorInt64 meta sPos
  bodyLen <- case mSlot 3 of
    Nothing -> Right 0
    Just b  -> peekI64 meta b
  Right (Tensor arrowTy shape strides BS.empty, bodyLen)

-- ============================================================
-- SparseTensor
-- ============================================================
--
-- Arrow's SparseTensor union covers four index formats (COO,
-- CSR, CSC, CSF). We model the common case — SparseTensorIndexCOO
-- — explicitly; callers with other formats can drop down to the
-- raw flatbuffer builder. A SparseTensor message (header_type=5)
-- wraps:
--
-- @
-- table SparseTensor {
--   type_type: Type;           // slot 0
--   type:      Type;           // slot 1
--   shape:    [TensorDim];     // slot 2
--   non_zero_length: long;     // slot 3
--   sparseIndex_type: SparseTensorIndex;  // slot 4 (union tag)
--   sparseIndex: SparseTensorIndex;       // slot 5
--   data: Buffer;              // slot 6
-- }
-- @
--
-- For COO:
--
-- @
-- table SparseTensorIndexCOO {
--   indicesType: Int;       // slot 0
--   indicesStrides: [long]; // slot 1
--   indicesBuffer: Buffer;  // slot 2
--   isCanonical: bool;      // slot 3
-- }
-- @

-- | A sparse tensor in coordinate (COO) layout. @indicesType@
-- is the integer width of each coordinate; @indicesBuffer@
-- carries @non_zero_length * ndim@ integers; @tensorBody@
-- carries @non_zero_length@ values of @tensorType@.
data SparseTensor = SparseTensor
  { sparseTensorType    :: !ArrowType
  , sparseTensorShape   :: !(V.Vector TensorDim)
  , sparseNonZeroLength :: !Int64
  , sparseIndicesType   :: !ArrowType
    -- ^ typically @AInt 64 True@ or @AInt 32 True@
  , sparseIndicesBody   :: !ByteString
  , sparseIndicesCanonical :: !Bool
  , sparseTensorBody    :: !ByteString
  } deriving (Show, Eq)

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

-- | Parse a SparseTensor message (header_type=5) carrying a COO
-- index. Returns the decoded sparse tensor with both its
-- @sparseIndicesBody@ and @sparseTensorBody@ sliced out of the
-- frame's body bytes, and the remainder of the input.
decodeSparseTensorFrame
  :: ByteString -> Either String (SparseTensor, ByteString)
decodeSparseTensorFrame bs = do
  (mlen, meta, rest1) <- readFrameHeader bs
  when' (mlen <= 0) $ Left "decodeSparseTensorFrame: unexpected EOS"
  msgPos <- fromIntegral <$> peekU32 meta 0
  mSlot  <- resolveTable meta msgPos
  ht <- case mSlot 1 of
    Nothing -> Right (0 :: Int)
    Just b  -> fromIntegral <$> peekU8 meta b
  when' (ht /= 5) $
    Left ("decodeSparseTensorFrame: expected header_type=5, got " ++ show ht)
  hdrPos <- case mSlot 2 of
    Nothing -> Left "SparseTensor missing header"
    Just p  -> Right p
  stPos  <- followUOffset meta hdrPos
  sSlot  <- resolveTable meta stPos
  tyTag  <- case sSlot 0 of
    Nothing -> Left "SparseTensor missing type_type"
    Just b  -> fromIntegral <$> peekU8 meta b
  tyFieldPos <- case sSlot 1 of
    Nothing -> Left "SparseTensor missing type"
    Just p  -> Right p
  tyPos <- followUOffset meta tyFieldPos
  arrowTy <- readType meta tyTag (Just tyPos)
  shape <- case sSlot 2 of
    Nothing  -> Right V.empty
    Just off -> do
      shapePos <- followUOffset meta off
      dimOffs  <- readVectorOfOffsets meta shapePos
      V.mapM (\dimPos -> readTensorDim meta dimPos) dimOffs
  nnz <- case sSlot 3 of
    Nothing -> Right 0
    Just p  -> peekI64 meta p
  idxTag <- case sSlot 4 of
    Nothing -> Left "SparseTensor missing sparseIndex_type"
    Just p  -> fromIntegral <$> peekU8 meta p
  when' (idxTag /= 1) $
    Left ("SparseTensor: only COO index format supported by this decoder, got tag=" ++ show (idxTag :: Int))
  idxFieldPos <- case sSlot 5 of
    Nothing -> Left "SparseTensor missing sparseIndex"
    Just p  -> Right p
  cooPos <- followUOffset meta idxFieldPos
  cSlot  <- resolveTable meta cooPos
  iTyFieldPos <- case cSlot 0 of
    Nothing -> Left "SparseTensorIndexCOO missing indicesType"
    Just p  -> Right p
  iTyPos <- followUOffset meta iTyFieldPos
  idxArrowTy <- readType meta 2 (Just iTyPos)  -- always an Int table
  (idxOffset, idxLen) <- case cSlot 2 of
    Nothing -> Left "SparseTensorIndexCOO missing indicesBuffer"
    Just p  -> do
      lo  <- peekI64 meta p
      ln' <- peekI64 meta (p + 8)
      Right (lo, ln')
  canonical <- case cSlot 3 of
    Nothing -> Right False
    Just p  -> (/= 0) <$> peekU8 meta p
  (valOffset, valLen) <- case sSlot 6 of
    Nothing -> Left "SparseTensor missing data buffer"
    Just p  -> do
      lo <- peekI64 meta p
      ln <- peekI64 meta (p + 8)
      Right (lo, ln)
  iSlice <- sliceWire "sparse tensor indices buffer" idxOffset idxLen rest1
  vSlice <- sliceWire "sparse tensor data buffer" valOffset valLen rest1
  -- Both slices were range-checked above, so this sum cannot overflow.
  let !bodyLen = fromIntegral valOffset + fromIntegral valLen :: Int
      rest2   = BS.drop (alignUp bodyLen 8) rest1
  Right ( SparseTensor
            { sparseTensorType       = arrowTy
            , sparseTensorShape      = shape
            , sparseNonZeroLength    = nnz
            , sparseIndicesType      = idxArrowTy
            , sparseIndicesBody      = iSlice
            , sparseIndicesCanonical = canonical
            , sparseTensorBody       = vSlice
            }
        , rest2
        )

-- | Encode a 'Tensor' as a standalone Arrow IPC frame:
-- continuation + metadata length + padded flatbuffer + body
-- + body padding. Suitable as a file payload or as one element
-- of an application-level container; Arrow's stream framing
-- itself accepts Tensor messages interleaved with any other
-- Message.
encodeTensorFrame :: Tensor -> ByteString
encodeTensorFrame t = encapsulateMessage (buildTensorMessage t) (tensorBody t)

-- | Parse an Arrow IPC frame that carries a 'Tensor'. Returns
-- the decoded tensor (with 'tensorBody' filled in from the
-- frame's body bytes) and the remaining bytes. Fails if the
-- frame's header_type isn't 4 (Tensor).
decodeTensorFrame
  :: ByteString -> Either String (Tensor, ByteString)
decodeTensorFrame bs = do
  (mlen, meta, rest1) <- readFrameHeader bs
  when' (mlen <= 0) $ Left "decodeTensorFrame: unexpected EOS"
  (t, bodyLen) <- decodeTensorMessage meta
  (body, rest2) <- takeFrameBody "tensor body" bodyLen rest1
  Right (t { tensorBody = body }, rest2)

-- | Split a frame's @bodyLen@-byte body (a wire value) off the bytes
-- that follow its metadata, also dropping the body's padding to the
-- next 8-byte boundary.
takeFrameBody :: String -> Int64 -> ByteString -> Either String (ByteString, ByteString)
takeFrameBody what bodyLen rest = do
  body <- sliceWire what 0 bodyLen rest
  Right (body, BS.drop (alignUp (BS.length body) 8) rest)

-- | Parse a @TensorDim@ table at the given position.
readTensorDim :: ByteString -> Int -> Either String TensorDim
readTensorDim meta pos = do
  slot <- resolveTable meta pos
  size <- case slot 0 of
    Nothing -> Right 0
    Just b  -> peekI64 meta b
  name <- case slot 1 of
    Nothing  -> Right T.empty
    Just off -> do
      sPos <- followUOffset meta off
      readString meta sPos
  Right (TensorDim size name)

-- ============================================================
-- Encapsulated framing + stream / file writers
-- ============================================================

-- | Wrap a raw flatbuffer @Message@ in the encapsulated IPC frame:
--
-- @
-- <continuation 0xFFFFFFFF : u32-LE>
-- <metadata_length : i32-LE, padded so body starts aligned to 8>
-- <flatbuffer bytes, padded>
-- <body bytes, padded to 8-byte alignment>
-- @
encapsulateMessage :: ByteString -> ByteString -> ByteString
encapsulateMessage meta body =
  let !metaLen  = BS.length meta
      !padded   = alignUp8 (8 + metaLen) - 8 -- meta+8-byte-prefix must end on 8B
      !metaPad  = padded - metaLen
      !bodyLen  = BS.length body
      !bodyPad  = alignUp8 bodyLen - bodyLen
  in BL.toStrict $ B.toLazyByteString $
        B.word32LE 0xFFFFFFFF
        <> B.int32LE (fromIntegral padded)
        <> B.byteString meta
        <> B.byteString (BS.replicate metaPad 0)
        <> B.byteString body
        <> B.byteString (BS.replicate bodyPad 0)
  where
    alignUp8 n = (n + 7) .&. complement 7

-- | Emit a complete Arrow IPC stream (schema + batches + EOS).
writeArrowStreamFB :: Schema -> [(RecordBatchDef, ByteString)] -> ByteString
writeArrowStreamFB sch batches = writeArrowStreamFBWithDicts sch [] batches

-- | Emit a stream with dictionary batches preceding the record
-- batches. Dictionary batches are placed in the order given; each
-- carries an @id@ that must match a @DictionaryEncoding.id@
-- referenced by the schema. Most consumers expect dictionary
-- batches before any record batch that references them — that's
-- the order we emit.
writeArrowStreamFBWithDicts
  :: Schema
  -> [DictBatch]
  -> [(RecordBatchDef, ByteString)]
  -> ByteString
writeArrowStreamFBWithDicts sch dicts batches =
  let !schemaMsg = encapsulateMessage (buildSchemaMessage sch) BS.empty
      !dictBytes = BS.concat
        [ encapsulateMessage
            (buildDictionaryBatchMessage db)
            (dbBody db)
        | db <- dicts
        ]
      !batchBytes = BS.concat
        [ encapsulateMessage
            (buildRecordBatchMessage rb (fromIntegral (BS.length body)))
            body
        | (rb, body) <- batches
        ]
      !eos = BL.toStrict $ B.toLazyByteString $
               B.word32LE 0xFFFFFFFF <> B.int32LE 0
  in schemaMsg <> dictBytes <> batchBytes <> eos

-- | Build @(RecordBatchDef, body bytes)@ from a 'Schema' + columns.
-- Delegates to 'Arrow.Write.encodeColumns' for the physical body
-- layout (validity bitmaps, typed buffers, 8-byte aligned), then
-- normalises the 'Buffer' list so every top-level non-nullable
-- /primitive/ field has the Arrow-spec buffer count — which means
-- prepending a zero-length validity buffer (the simplified internal
-- encoder in "Arrow.Write" omits the slot for non-nullable
-- primitives because its own reader does too; the spec requires
-- the slot to exist even when empty). Nested / view / REE columns
-- are emitted with their canonical buffer layout by 'encodeCol'
-- already, and pass through unchanged.
buildRecordBatchBytes
  :: Schema
  -> V.Vector ColumnArray
  -> (RecordBatchDef, ByteString)
buildRecordBatchBytes = buildRecordBatchBytesWith Nothing

-- | 'buildRecordBatchBytes' with an optional 'BodyCompressionCodec'.
-- When 'Just', each buffer in the body is compressed independently
-- (per Arrow's @BodyCompression@ semantics) and the buffer offsets
-- in the returned 'RecordBatchDef' point into the /compressed/
-- body. The corresponding 'rbBodyCompression' field is populated
-- so readers can dispatch decompression.
--
-- A codec that was not compiled in (see 'bodyCompressionAvailable';
-- the @zstd@ / @lz4@ cabal flags) is ignored: the batch is written
-- uncompressed with 'rbBodyCompression' = 'Nothing', so the output
-- is always a valid Arrow batch.
buildRecordBatchBytesWith
  :: Maybe BodyCompressionCodec
  -> Schema
  -> V.Vector ColumnArray
  -> (RecordBatchDef, ByteString)
buildRecordBatchBytesWith mCodec0 sch cols =
  let !acc = W.encodeColumns (arrowFields sch) cols W.emptyBuildAcc
      !rawNodes = V.fromList (reverse (W.baNodes acc))
      !rawBufs  = V.fromList (reverse (W.baBufs acc))
      !rawVar          = V.fromList (reverse (W.baVariadic acc))
      !(!nodes, !bufs0) = normaliseBuffers (arrowFields sch) cols rawNodes rawBufs rawVar
      !rawBody = BL.toStrict (B.toLazyByteString (W.baBody acc))
      !numRows = if V.null cols then 0 else columnLength (V.head cols)
      !mCodec = case mCodec0 of
        Just codec | bodyCompressionAvailable codec -> Just codec
        _ -> Nothing
      !(!bufs, !body) = case mCodec of
        Nothing    -> (bufs0, rawBody)
        Just codec -> compressBody codec bufs0 rawBody
      !rb = RecordBatchDef
              { rbLength  = fromIntegral numRows
              , rbNodes   = nodes
              , rbBuffers = bufs
              , rbVariadicBufferCounts = rawVar
              , rbBodyCompression = mCodec
              }
  in (rb, body)

-- | Apply Arrow's @BodyCompression = BUFFER@ scheme: each
-- @Buffer { offset, length }@ slice in the body is replaced with
-- @<i64 LE uncompressedLength><compressedBytes>@, with the
-- buffer offsets rewritten to point at the new layout (8-aligned
-- between buffers, like the uncompressed body). Returns the new
-- buffer list + the rewritten body.
compressBody
  :: BodyCompressionCodec
  -> V.Vector Buffer
  -> ByteString
  -> (V.Vector Buffer, ByteString)
compressBody codec bufs body0 =
  let !chunks = V.imap rewrite bufs
      rewrite _ buf =
        let !off  = fromIntegral (bufOffset buf) :: Int
            !len  = fromIntegral (bufLength buf) :: Int
            !raw  = BS.take len (BS.drop off body0)
            !envelope = compressBufferEnvelope codec raw
        in  envelope
      step (!off, !revBufs, !revChunks) chunk =
        let !len    = BS.length chunk
            !padded = alignUp len 8
            !pad    = padded - len
            !newBuf = Buffer { bufOffset = fromIntegral off
                             , bufLength = fromIntegral len
                             }
        in  ( off + padded
            , newBuf : revBufs
            , (chunk <> BS.replicate pad 0) : revChunks
            )
      (_, revBufsOut, revChunksOut) =
        V.foldl' step (0 :: Int, [], []) chunks
  in  ( V.fromList (reverse revBufsOut)
      , BS.concat (reverse revChunksOut)
      )

-- | Wrap a single buffer's bytes per Arrow's @BUFFER@ method: an
-- 8-byte little-endian uncompressed length followed by the
-- compressed payload. When compression doesn't shrink the bytes
-- (rare but possible for tiny / already-random inputs) we set
-- @uncompressedLength = -1@ and emit the raw bytes inline, which
-- is the spec-mandated escape hatch.
compressBufferEnvelope :: BodyCompressionCodec -> ByteString -> ByteString
compressBufferEnvelope codec raw
  | BS.null raw = raw
  | otherwise =
      let !compressed = compressBuffer codec raw
          !rawLen     = BS.length raw
          !compLen    = BS.length compressed
      in  if compLen >= rawLen
            then  -- spec escape hatch: -1 length, raw bytes inline
              encodeBodyLen (-1 :: Int64) <> raw
            else encodeBodyLen (fromIntegral rawLen) <> compressed

encodeBodyLen :: Int64 -> ByteString
encodeBodyLen n = BL.toStrict $ B.toLazyByteString $
  B.int64LE n

-- | Decompress one buffer envelope per the @BUFFER@ method.
--
-- The 8-byte uncompressed length is a wire claim. It must be @-1@
-- (stored uncompressed) or a size the payload could actually expand
-- to (see 'maxCompressionRatio'), and the decompressed bytes must
-- match it exactly; a lying header can never size an allocation.
decompressBufferEnvelope
  :: BodyCompressionCodec -> ByteString -> Either String ByteString
decompressBufferEnvelope codec env
  | BS.null env = Right env
  | BS.length env < 8 =
      Left "Arrow.FlatBufferIPC: buffer envelope shorter than 8 bytes"
  | rawLen == -1 = Right payload   -- spec escape: stored uncompressed
  | rawLen < 0 =
      Left ("Arrow.FlatBufferIPC: negative uncompressed buffer length " ++ show rawLen)
  | rawLen > fromIntegral (BS.length payload) * maxCompressionRatio =
      Left
        ( "Arrow.FlatBufferIPC: buffer claims "
            ++ show rawLen
            ++ " uncompressed bytes from a "
            ++ show (BS.length payload)
            ++ "-byte payload"
        )
  | otherwise = do
      out <- decompressBuffer codec (fromIntegral rawLen) payload
      if BS.length out /= fromIntegral rawLen
        then
          Left
            ( "Arrow.FlatBufferIPC: buffer decompressed to "
                ++ show (BS.length out)
                ++ " bytes, header says "
                ++ show rawLen
            )
        else Right out
  where
    -- Lazy on purpose: only forced once the guards above have checked
    -- that the envelope holds the 8-byte length.
    rawLen = decodeBodyLen env
    payload = BS.drop 8 env

-- | No LZ4 frame or ZSTD frame expands by more than this factor (a
-- ZSTD RLE block turns 4 bytes into at most 128 KiB; LZ4 tops out
-- near 255x), so a larger claimed size is a lie, not data.
maxCompressionRatio :: Int64
maxCompressionRatio = 32768

decodeBodyLen :: ByteString -> Int64
decodeBodyLen bs =
  let !b0 = fromIntegral (BS.index bs 0) :: Int64
      !b1 = fromIntegral (BS.index bs 1) :: Int64
      !b2 = fromIntegral (BS.index bs 2) :: Int64
      !b3 = fromIntegral (BS.index bs 3) :: Int64
      !b4 = fromIntegral (BS.index bs 4) :: Int64
      !b5 = fromIntegral (BS.index bs 5) :: Int64
      !b6 = fromIntegral (BS.index bs 6) :: Int64
      !b7 = fromIntegral (BS.index bs 7) :: Int64
  in   b0
    .|. (b1 `shiftL` 8)
    .|. (b2 `shiftL` 16)
    .|. (b3 `shiftL` 24)
    .|. (b4 `shiftL` 32)
    .|. (b5 `shiftL` 40)
    .|. (b6 `shiftL` 48)
    .|. (b7 `shiftL` 56)

-- | Compress a single buffer's bytes. Routes to the right
-- codec backend; @-fzstd@ / @-flz4@ Cabal flags select
-- availability.
--
-- Returns 'Left' when the requested codec wasn't compiled in.
-- Callers that don't want to handle the error can ignore the
-- 'Left' branch (the historical 'compressBuffer' that fired
-- 'error' is preserved as 'compressBuffer'); new code should
-- prefer 'compressBufferEither'.
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

-- | Back-compat wrapper that fires 'error' on the unavailable
-- codec; new code should reach for 'compressBufferEither'.
compressBuffer :: BodyCompressionCodec -> ByteString -> ByteString
compressBuffer codec bs = case compressBufferEither codec bs of
  Right out -> out
  Left e    -> error e

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

-- | Decompress a payload that must expand to exactly @rawLen@ bytes
-- ('decompressBufferEnvelope' has already bounded @rawLen@). Neither
-- backend may allocate more than @rawLen + 1@ output bytes.
decompressBuffer
  :: BodyCompressionCodec -> Int -> ByteString -> Either String ByteString
decompressBuffer codec rawLen comp = case codec of
#ifdef HAVE_ZSTD
  BodyZstd ->
    -- Zstd.decompress sizes its output from the frame header, so the
    -- header must agree with the (bounded) envelope length first.
    case Zstd.decompressedSize comp of
      Just n | n == rawLen ->
        case Zstd.decompress comp of
          Zstd.Decompress out -> Right out
          Zstd.Skip           -> Left "Arrow.FlatBufferIPC: ZSTD decompress: skipped frame"
          Zstd.Error msg      -> Left ("Arrow.FlatBufferIPC: ZSTD decompress: " ++ msg)
      other ->
        Left
          ( "Arrow.FlatBufferIPC: ZSTD frame content size "
              ++ show other
              ++ " does not match the buffer's uncompressed length "
              ++ show rawLen
          )
#else
  BodyZstd -> Left "Arrow.FlatBufferIPC: ZSTD body compression requires building wireform-arrow with -fzstd"
#endif
#ifdef HAVE_LZ4
  LZ4Frame ->
    -- lz4-hs's decompress is lazy and throws on malformed input;
    -- take at most one byte past the expected size (so an overlong
    -- frame is detected without being fully inflated) and catch the
    -- exception to keep the Either-shaped contract.
    case unsafePerformIO $
           Exc.try @Exc.SomeException
             (Exc.evaluate
                (BL.toStrict (BL.take (fromIntegral rawLen + 1) (Lz4.decompress (BL.fromStrict comp))))) of
      Right out -> Right out
      Left e    -> Left ("Arrow.FlatBufferIPC: LZ4_FRAME decompress: " ++ show e)
#else
  LZ4Frame -> Left "Arrow.FlatBufferIPC: LZ4 body compression requires building wireform-arrow with -flz4"
#endif

-- | Decode a body that was written with body compression. Walks
-- each buffer in the supplied list (with offsets pointing into
-- the compressed body), unwraps the per-buffer envelope, and
-- returns @(newBuffers, decompressedBody)@ where the new
-- buffers' offsets point into the uncompressed body — suitable
-- for the standard 'materializeRecordBatch' path.
decompressBody
  :: BodyCompressionCodec
  -> V.Vector Buffer
  -> ByteString
  -> Either String (V.Vector Buffer, ByteString)
decompressBody codec bufs body0 = do
  payloads <- V.mapM
    (\buf -> do
        env <- sliceWire "compressed buffer" (bufOffset buf) (bufLength buf) body0
        decompressBufferEnvelope codec env)
    bufs
  let step (!off, !accBufs, !accChunks) decoded =
        let !len    = BS.length decoded
            !padded = alignUp len 8
            !pad    = padded - len
            !newBuf = Buffer { bufOffset = fromIntegral off
                             , bufLength = fromIntegral len
                             }
        in  ( off + padded
            , newBuf : accBufs
            , (decoded <> BS.replicate pad 0) : accChunks
            )
      (_, revBufs, revChunks) =
        V.foldl' step (0 :: Int, [], []) payloads
  Right ( V.fromList (reverse revBufs)
        , BS.concat (reverse revChunks)
        )

-- | Walk every TOP-LEVEL column in the batch and inject an empty
-- 'Buffer' (offset=0, length=0) for the validity slot of any
-- non-nullable column whose layout has one. The Arrow spec
-- requires the slot at every level; pyarrow, arrow-cpp, and
-- arrow-rs all happily accept a "missing" empty slot for nested
-- non-nullable children, so we only fix this at the top level
-- where the readers are stricter.
--
-- For columns whose layout has /no/ validity slot (Union, REE,
-- FixedSizeBinary's data buffer, struct's children, ...) we pass
-- through unchanged.
--
-- 'FieldNode' counts pass through unchanged — the spec says one
-- @FieldNode@ per field in pre-order, which 'encodeCol' already
-- produces.
normaliseBuffers
  :: V.Vector Field
  -> V.Vector ColumnArray
  -> V.Vector FieldNode
  -> V.Vector Buffer
  -> V.Vector Int64    -- ^ baVariadic: per-view-column variadic count
                       --   in DFS pre-order, as recorded by the
                       --   writer; consumed left-to-right.
  -> (V.Vector FieldNode, V.Vector Buffer)
normaliseBuffers _fields cols nodes rawBufs varCounts =
  let (_, _, !revBufs) = V.foldl' step (0 :: Int, 0 :: Int, []) cols
      step (!bIdx, !vIdx, acc) col =
        let (!consumed, !varConsumed, !emitted) =
              injectColumn col rawBufs bIdx varCounts vIdx
        in  ( bIdx + consumed
            , vIdx + varConsumed
            , reverse emitted ++ acc
            )
  in  (nodes, V.fromList (reverse revBufs))

-- | Recursively inject empty validity buffers at every layout
-- position that has a validity slot but where the writer omitted
-- it (non-nullable column).  Returns @(#source buffers consumed,
-- #variadic-count entries consumed, spec-compliant output buffers
-- in emission order)@.
injectColumn
  :: ColumnArray
  -> V.Vector Buffer
  -> Int
  -> V.Vector Int64
  -> Int
  -> (Int, Int, [Buffer])
injectColumn col bufs bIdx0 varCounts vIdx0 = case col of
  _ | isFlatPrim col ->
      let (vBuf, bIdx1) = takeValidity (isNullable col) bufs bIdx0
          dataBuf       = bufs V.! bIdx1
      in  (bIdx1 + 1 - bIdx0, 0, [vBuf, dataBuf])
  _ | isVarLen col ->
      let (vBuf, bIdx1) = takeValidity (isNullable col) bufs bIdx0
          offsetsBuf    = bufs V.! bIdx1
          dataBuf       = bufs V.! (bIdx1 + 1)
      in  (bIdx1 + 2 - bIdx0, 0, [vBuf, offsetsBuf, dataBuf])

  ColStruct _ children        -> goStruct False (V.toList (V.map snd children)) bIdx0 vIdx0
  ColStructMaybe _ children   -> goStruct True  (V.toList (V.map snd children)) bIdx0 vIdx0

  ColList _ child             -> goList False child bIdx0 vIdx0
  ColListMaybe _ _ child      -> goList True  child bIdx0 vIdx0
  ColLargeList _ child        -> goList False child bIdx0 vIdx0
  ColLargeListMaybe _ _ child -> goList True  child bIdx0 vIdx0

  ColFixedSizeList _ _ child      -> goFixedSizeList False child bIdx0 vIdx0
  ColFixedSizeListMaybe _ _ child -> goFixedSizeList True  child bIdx0 vIdx0

  ColMap _ k v                -> goMap False k v bIdx0 vIdx0
  ColMapMaybe _ _ k v         -> goMap True  k v bIdx0 vIdx0

  ColDenseUnion _ _ children ->
      let typeIds = bufs V.! bIdx0
          offsets = bufs V.! (bIdx0 + 1)
          (cc, cv, cb) = goSiblings (V.toList children) (bIdx0 + 2) vIdx0
      in  (cc + 2, cv, typeIds : offsets : cb)
  ColSparseUnion _ children ->
      let typeIds = bufs V.! bIdx0
          (cc, cv, cb) = goSiblings (V.toList children) (bIdx0 + 1) vIdx0
      in  (cc + 1, cv, typeIds : cb)

  ColRunEndEncoded re vals ->
      let (cre, crv, bre) = injectColumn re bufs bIdx0 varCounts vIdx0
          (cv,  cvv, bv)  = injectColumn vals bufs (bIdx0 + cre) varCounts (vIdx0 + crv)
      in  (cre + cv, crv + cvv, bre ++ bv)

  ColListView _ _ child            -> goListView False child bIdx0 vIdx0
  ColListViewMaybe _ _ _ child     -> goListView True  child bIdx0 vIdx0
  ColLargeListView _ _ child       -> goListView False child bIdx0 vIdx0
  ColLargeListViewMaybe _ _ _ child -> goListView True  child bIdx0 vIdx0

  ColUtf8View {}        -> goView bIdx0
  ColUtf8ViewMaybe {}   -> goView bIdx0
  ColBinaryView {}      -> goView bIdx0
  ColBinaryViewMaybe {} -> goView bIdx0

  ColDictionary {} -> dictIndices
  ColDictionaryMaybe {} -> dictIndices

  _ -> (0, 0, [])
  where
    dictIndices =
      let (vBuf, bIdx1) = takeValidity (isNullable col) bufs bIdx0
          indices       = bufs V.! bIdx1
      in  (bIdx1 + 1 - bIdx0, 0, [vBuf, indices])
    goStruct nullable children bIdx vIdx =
      let (vBuf, bIdx1) = takeValidity nullable bufs bIdx
          (cc, cv, cb) = goSiblings children bIdx1 vIdx
      in  (bIdx1 + cc - bIdx, cv, vBuf : cb)
    goList nullable child bIdx vIdx =
      let (vBuf, bIdx1) = takeValidity nullable bufs bIdx
          offsetsBuf    = bufs V.! bIdx1
          (cc, cv, cb)  = injectColumn child bufs (bIdx1 + 1) varCounts vIdx
      in  (bIdx1 + 1 + cc - bIdx, cv, vBuf : offsetsBuf : cb)
    goFixedSizeList nullable child bIdx vIdx =
      let (vBuf, bIdx1) = takeValidity nullable bufs bIdx
          (cc, cv, cb)  = injectColumn child bufs bIdx1 varCounts vIdx
      in  (bIdx1 + cc - bIdx, cv, vBuf : cb)
    goMap nullable k v bIdx vIdx =
      let (vBuf, bIdx1) = takeValidity nullable bufs bIdx
          offsetsBuf    = bufs V.! bIdx1
          (ck, ckv, kb) = injectColumn k bufs (bIdx1 + 1) varCounts vIdx
          (cv, cvv, kv) = injectColumn v bufs (bIdx1 + 1 + ck) varCounts (vIdx + ckv)
      in  ( bIdx1 + 1 + ck + cv - bIdx
          , ckv + cvv
          , vBuf : offsetsBuf : emptyValidityBuffer : kb ++ kv
          )
    goListView nullable child bIdx vIdx =
      let (vBuf, bIdx1) = takeValidity nullable bufs bIdx
          offsetsBuf    = bufs V.! bIdx1
          sizesBuf      = bufs V.! (bIdx1 + 1)
          (cc, cv, cb)  = injectColumn child bufs (bIdx1 + 2) varCounts vIdx
      in  (bIdx1 + 2 + cc - bIdx, cv, vBuf : offsetsBuf : sizesBuf : cb)
    goView bIdx =
      -- Variadic count comes from the writer's per-view tally
      -- (recorded in baVariadic and passed through here as
      -- @varCounts@). Far cheaper than re-encoding the column.
      let (vBuf, bIdx1) = takeValidity (isNullable col) bufs bIdx
          viewBuf       = bufs V.! bIdx1
          !varCount = case varCounts V.!? vIdx0 of
            Just c  -> fromIntegral c :: Int
            Nothing -> 0
          variadics = [ bufs V.! (bIdx1 + 1 + i) | i <- [0 .. varCount - 1] ]
      in  ( bIdx1 + 1 + varCount - bIdx
          , 1
          , vBuf : viewBuf : variadics
          )
    goSiblings :: [ColumnArray] -> Int -> Int -> (Int, Int, [Buffer])
    goSiblings []     _    _   = (0, 0, [])
    goSiblings (c:cs) bIdx vIdx =
      let (cc, cv, cb) = injectColumn c bufs bIdx varCounts vIdx
          (rc, rv, rb) = goSiblings cs (bIdx + cc) (vIdx + cv)
      in  (cc + rc, cv + rv, cb ++ rb)

-- | Take a validity buffer (or substitute an empty one) and step
-- the source-buffer cursor accordingly.
takeValidity :: Bool -> V.Vector Buffer -> Int -> (Buffer, Int)
takeValidity True  bufs bIdx = (V.unsafeIndex bufs bIdx, bIdx + 1)
takeValidity False _    bIdx = (emptyValidityBuffer, bIdx)

isFlatPrim :: ColumnArray -> Bool
isFlatPrim = \case
  ColInt8 {} -> True; ColInt16 {} -> True; ColInt32 {} -> True; ColInt64 {} -> True
  ColUInt8 {} -> True; ColUInt16 {} -> True; ColUInt32 {} -> True; ColUInt64 {} -> True
  ColFloat16 {} -> True; ColFloat {} -> True; ColDouble {} -> True
  ColBool {} -> True
  ColDate32 {} -> True; ColDate64 {} -> True
  ColTime32 {} -> True; ColTime64 {} -> True
  ColTimestamp {} -> True; ColDuration {} -> True
  ColDecimal128 {} -> True; ColDecimal256 {} -> True
  ColFixedSizeBinary {} -> True
  ColIntervalYearMonth {} -> True
  ColIntervalDayTime {} -> True
  ColIntervalMonthDayNano {} -> True
  ColInt8Maybe {} -> True; ColInt16Maybe {} -> True
  ColInt32Maybe {} -> True; ColInt64Maybe {} -> True
  ColUInt8Maybe {} -> True; ColUInt16Maybe {} -> True
  ColUInt32Maybe {} -> True; ColUInt64Maybe {} -> True
  ColFloat16Maybe {} -> True
  ColFloatMaybe {} -> True; ColDoubleMaybe {} -> True
  ColBoolMaybe {} -> True
  ColDate32Maybe {} -> True; ColDate64Maybe {} -> True
  ColTime32Maybe {} -> True; ColTime64Maybe {} -> True
  ColTimestampMaybe {} -> True; ColDurationMaybe {} -> True
  ColFixedSizeBinaryMaybe {} -> True
  ColDecimal128Maybe {} -> True; ColDecimal256Maybe {} -> True
  ColIntervalYearMonthMaybe {} -> True
  ColIntervalDayTimeMaybe {} -> True
  ColIntervalMonthDayNanoMaybe {} -> True
  _ -> False

isVarLen :: ColumnArray -> Bool
isVarLen = \case
  ColUtf8 {} -> True; ColBinary {} -> True
  ColLargeUtf8 {} -> True; ColLargeBinary {} -> True
  ColUtf8Maybe {} -> True; ColBinaryMaybe {} -> True
  ColLargeUtf8Maybe {} -> True; ColLargeBinaryMaybe {} -> True
  _ -> False

isNullable :: ColumnArray -> Bool
isNullable = isNullableColumn

-- ============================================================
-- Inverse: spec-format → simplified-format
-- ============================================================

-- | Strip the empty validity slots that the spec mandates at every
-- layout position from a 'RecordBatchDef' produced by an
-- arrow-cpp / arrow-rs / pyarrow writer (or our own
-- 'normaliseBuffers'). The returned batch's buffer list matches
-- what 'Arrow.Write.encodeColumns' would have emitted for the same
-- schema, so 'Arrow.Column.materializeRecordBatch' can consume it
-- directly.
--
-- The body bytes are unchanged; only the buffer /index/ list is
-- rewritten. A spec-format empty-validity slot has
-- @offset == 0 && length == 0@ and points nowhere, so dropping it
-- doesn't disturb the body offsets the surviving buffers carry.
--
-- The buffer list comes off the wire: a batch with fewer buffers
-- (or variadic counts) than the schema's layout needs is a 'Left'.
denormaliseBuffers
  :: Schema -> RecordBatchDef -> Either String RecordBatchDef
denormaliseBuffers sch rb = do
  (_, _, revOut) <- stripFields (V.toList (arrowFields sch)) (0, 0, [])
  Right rb { rbBuffers = V.fromList (reverse revOut) }
  where
    !bufs = rbBuffers rb
    !varCounts = rbVariadicBufferCounts rb

    stripFields :: [Field] -> StripState -> Either String StripState
    stripFields fs st = foldl (\acc f -> acc >>= stripField f) (Right st) fs

    at :: Int -> Either String Buffer
    at i = case bufs V.!? i of
      Just b -> Right b
      Nothing ->
        Left
          ( "Arrow.FlatBufferIPC: record batch has "
              ++ show (V.length bufs)
              ++ " buffers, schema layout needs buffer #"
              ++ show i
          )

    -- Keep the validity slot only for nullable fields.
    keepValidity f (bi, vi, acc) = do
      v <- at bi
      Right (bi + 1, vi, if fieldNullable f then v : acc else acc)

    -- Copy the next @n@ buffers through unchanged.
    keep :: Int -> StripState -> Either String StripState
    keep n st0 = go n st0
      where
        go 0 st = Right st
        go k (bi, vi, acc) = do
          b <- at bi
          go (k - 1) (bi + 1, vi, b : acc)

    skip n (bi, vi, acc) = at (bi + n - 1) >> Right (bi + n, vi, acc)

    onlyChild what f = case V.toList (fieldChildren f) of
      [c] -> Right c
      _ -> Left ("Arrow.FlatBufferIPC: " ++ what ++ " field must have exactly one child")

    stripField :: Field -> StripState -> Either String StripState
    stripField f st
      -- Dictionary-encoded fields carry the index column on the wire,
      -- not the value column: a flat int layout (validity + data).
      | Just _ <- fieldDictionary f = keepValidity f st >>= keep 1
      | otherwise = case fieldType f of
          ANull -> Right st
          AInt _ _ -> flatPrim
          ABool -> flatPrim
          AFloatingPoint _ -> flatPrim
          AFixedSizeBinary _ -> flatPrim
          ADate _ -> flatPrim
          ATime _ _ -> flatPrim
          ATimestamp _ _ -> flatPrim
          ADuration _ -> flatPrim
          ADecimal _ _ -> flatPrim
          ADecimal256 _ _ -> flatPrim
          AInterval _ -> flatPrim
          AUtf8 -> varLen
          ABinary -> varLen
          ALargeUtf8 -> varLen
          ALargeBinary -> varLen
          AStruct -> keepValidity f st >>= stripFields (V.toList (fieldChildren f))
          AList -> listLike "list"
          ALargeList -> listLike "large list"
          AFixedSizeList _ -> do
            c <- onlyChild "fixed-size list" f
            keepValidity f st >>= stripField c
          AMap _ ->
            -- Spec layout: [validity, offsets, entries-struct validity,
            -- key bufs..., value bufs...]. The simplified layout has no
            -- entries-struct buffer.
            case V.toList (fieldChildren f) of
              [entries] -> case V.toList (fieldChildren entries) of
                [keyField, valField] ->
                  keepValidity f st >>= keep 1 >>= skip 1 >>= stripField keyField >>= stripField valField
                _ -> Left "Arrow.FlatBufferIPC: map entries must be a struct of (key, value)"
              _ -> Left "Arrow.FlatBufferIPC: map field must have exactly one child"
          AUnion Dense _ -> keep 2 st >>= stripFields (V.toList (fieldChildren f))
          AUnion Sparse _ -> keep 1 st >>= stripFields (V.toList (fieldChildren f))
          ARunEndEncoded -> case V.toList (fieldChildren f) of
            [reField, valField] -> stripField reField st >>= stripField valField
            _ -> Left "Arrow.FlatBufferIPC: run-end encoded field must have two children"
          AListView -> listViewLike "list-view"
          ALargeListView -> listViewLike "large list-view"
          AUtf8View -> viewLayout
          ABinaryView -> viewLayout
      where
        flatPrim = keepValidity f st >>= keep 1
        varLen = keepValidity f st >>= keep 2
        listLike what = do
          c <- onlyChild what f
          keepValidity f st >>= keep 1 >>= stripField c
        listViewLike what = do
          c <- onlyChild what f
          keepValidity f st >>= keep 2 >>= stripField c
        -- Spec: [validity, views, ...variadic data buffers]; the
        -- variadic count is this view column's entry in
        -- rbVariadicBufferCounts (preorder over view columns).
        viewLayout = do
          st1@(bi, vi, _) <- keepValidity f st
          varCount <- case varCounts V.!? vi of
            Just c
              | c >= 0 && c < fromIntegral (V.length bufs - bi) -> Right (fromIntegral c)
              | otherwise -> Left ("Arrow.FlatBufferIPC: invalid variadic buffer count " ++ show c)
            Nothing -> Left "Arrow.FlatBufferIPC: view column has no variadicBufferCounts entry"
          (bi', _, acc') <- keep (1 + varCount) st1
          Right (bi', vi + 1, acc')


-- | @(next source buffer, next variadic-count entry, kept buffers reversed)@.
type StripState = (Int, Int, [Buffer])

-- | Convenience: parse + materialise. Pyarrow / arrow-cpp output
-- becomes a 'V.Vector ColumnArray' per batch in one call.
materializeRecordBatchFB
  :: Schema -> RecordBatchDef -> ByteString
  -> Either String (V.Vector ColumnArray)
materializeRecordBatchFB sch rb body = do
  rb' <- denormaliseBuffers sch rb
  materializeRecordBatch sch rb' body

-- | Zero-length validity buffer used for fields whose validity
-- doesn't appear in the source 'Buffer' vector — namely:
--
--   * The validity slot of a non-nullable field (Arrow's wire
--     layout still requires a buffer entry; readers see
--     length=0 and treat every value as valid).
--   * The keys-validity slot of a Map column. Map keys are
--     non-nullable per the Arrow spec; the only nullability
--     in a Map is at the entry level (Map's own validity) and
--     at the value level (per the value field's own
--     'fieldNullable').
--
-- Despite the historical \"placeholder\" comment this is the
-- normal, spec-conforming way to encode the slot — pyarrow
-- and arrow-cpp both emit Buffer 0 0 here on round-trip.
emptyValidityBuffer :: Buffer
emptyValidityBuffer = Buffer 0 0

-- | Convenience: pyarrow-compatible Arrow IPC stream from columnar
-- data. Calls 'buildRecordBatchBytes' per batch and wraps each in
-- the encapsulated framing.
writeArrowStreamFBFromColumns
  :: Schema
  -> V.Vector (V.Vector ColumnArray)
  -> ByteString
writeArrowStreamFBFromColumns sch batches =
  writeArrowStreamFB sch
    (V.toList (V.map (buildRecordBatchBytes sch) batches))

-- | Arrow IPC /file/ format (per @format/File.fbs@):
--
-- @
-- 'ARROW1'\\0\\0
-- <encapsulated schema message>
-- <encapsulated record batch 1>
-- ...
-- <encapsulated record batch N>
-- <Footer flatbuffer>             // table Footer { schema, recordBatches: [Block], ... }
-- <i32 footer length>
-- 'ARROW1'
-- @
--
-- Each 'Block' references one record batch by
-- @(offset, metaDataLength, bodyLength)@ — @offset@ pointing at the
-- continuation marker that begins the encapsulated message. We emit
-- the EOS marker too so the file simultaneously parses as a stream.
writeArrowFileFB :: Schema -> [(RecordBatchDef, ByteString)] -> ByteString
writeArrowFileFB sch batches = writeArrowFileFBWithDicts sch [] batches

-- | File format with dictionary batches indexed in the 'Footer'.
-- Identical layout to 'writeArrowFileFB' except dict batches are
-- emitted between the schema and the first record batch, and the
-- footer's @dictionaries: [Block]@ slot points to them.
writeArrowFileFBWithDicts
  :: Schema
  -> [DictBatch]
  -> [(RecordBatchDef, ByteString)]
  -> ByteString
writeArrowFileFBWithDicts sch dicts batches =
  let !magic       = "ARROW1"
      !magicPad    = BS.pack [0, 0]
      !headerLen   = BS.length magic + BS.length magicPad   -- 8 bytes
      !schemaMsg   = encapsulateMessage (buildSchemaMessage sch) BS.empty

      -- Walk a list of frames, computing one Block per frame and
      -- accumulating the encapsulated bytes alongside.
      stepDict (revBlocks, !off, accBytes) db =
        let !msgBytes = encapsulateMessage
              (buildDictionaryBatchMessage db) (dbBody db)
            !msgLen     = BS.length msgBytes
            !bodyLen    = BS.length (dbBody db)
            !paddedBody = alignUp8FB bodyLen
            !metaLen    = msgLen - paddedBody
            !blk = ArrowBlock
                     { abOffset  = fromIntegral off
                     , abMetaLen = fromIntegral metaLen
                     , abBodyLen = fromIntegral paddedBody
                     }
        in  (blk : revBlocks, off + msgLen, accBytes ++ [msgBytes])
      stepBatch (revBlocks, !off, accBytes) (rb, body) =
        let !msgBytes = encapsulateMessage
              (buildRecordBatchMessage rb (fromIntegral (BS.length body)))
              body
            !msgLen   = BS.length msgBytes
            !bodyLen  = BS.length body
            !paddedBody = alignUp8FB bodyLen
            !metaLen  = msgLen - paddedBody
            !blk = ArrowBlock
                     { abOffset    = fromIntegral off
                     , abMetaLen   = fromIntegral metaLen
                     , abBodyLen   = fromIntegral paddedBody
                     }
        in  (blk : revBlocks, off + msgLen, accBytes ++ [msgBytes])

      (revDictBlocks, !afterDicts, dictBs) =
        foldl stepDict
              ([], headerLen + BS.length schemaMsg, [])
              dicts
      (revRbBlocks, _eosOff, msgBs) =
        foldl stepBatch ([], afterDicts, []) batches

      !dictBlocks = reverse revDictBlocks
      !rbBlocks   = reverse revRbBlocks
      !eos = BL.toStrict $ B.toLazyByteString $
               B.word32LE 0xFFFFFFFF <> B.int32LE 0
      !streamBytes = BS.concat (schemaMsg : dictBs ++ msgBs ++ [eos])
      !footer = buildFileFooter sch dictBlocks rbBlocks
      !footerPad =
        let raw = BS.length footer
        in  BS.replicate (alignUp8FB raw - raw) 0
      !footerLenLE = BL.toStrict $ B.toLazyByteString $
                       B.int32LE (fromIntegral (BS.length footer))
  in BS.concat
       [ magic, magicPad
       , streamBytes
       , footer, footerPad
       , footerLenLE
       , magic
       ]
  where
    alignUp8FB n = (n + 7) .&. complement (7 :: Int)

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


-- | Decode a complete @Schema@ table at the given position.
readSchemaTable :: ByteString -> Pos -> Either String Schema
readSchemaTable bs schPos = do
  slot <- resolveTable bs schPos
  endian <- case slot 0 of
    Nothing -> Right Little
    Just p  -> do
      v <- peekI16 bs p
      case v of
        0 -> Right Little
        1 -> Right Big
        _ -> Left ("Arrow.FlatBufferIPC: unknown endianness " ++ show v)
  (fieldsVec, budget0) <- case slot 1 of
    Nothing -> Right (V.empty, schemaBudget bs)
    Just p  -> readChargedOffsets bs (schemaBudget bs) p
  (fields, budget1) <- readFields bs 0 budget0 fieldsVec
  (customMd, _) <- readKeyValues bs budget1 (slot 2)
  Right Schema
    { arrowFields     = fields
    , arrowEndianness = endian
    , arrowMetadata   = customMd
    , arrowFeatures   = V.empty  -- features round-trip on the wire is parsed by callers via getFeaturesIfPresent
    }

-- | Nesting limit for decoded schemas (arrow-cpp's IPC reader uses
-- the same bound).
maxSchemaDepth :: Int
maxSchemaDepth = 64

-- | FlatBuffers offsets may alias, so a few hundred bytes can
-- describe an exponentially large DAG of fields or reuse one long
-- string for every name. Every decoded field, string and type id is
-- charged against this budget; an honest (unaliased) buffer spends
-- at most its own length.
schemaBudget :: ByteString -> Int
schemaBudget bs = 4 * BS.length bs + 4096

-- | Charge @cost@ against the remaining schema budget.
charge :: Int -> Int -> Either String Int
charge cost budget
  | cost > budget =
      Left "Arrow.FlatBufferIPC: schema decodes to more fields and strings than its metadata holds (aliased offsets)"
  | otherwise = Right (budget - cost)

-- | Decode the string at the uoffset in @fieldPos@, charging its length first.
readChargedString :: ByteString -> Int -> Pos -> Either String (T.Text, Int)
readChargedString bs budget fieldPos = do
  strPos <- followUOffset bs fieldPos
  raw <- readStringSlice bs strPos
  budget' <- charge (4 + BS.length raw) budget
  case TE.decodeUtf8' raw of
    Left _ -> Left "Arrow.FlatBufferIPC: invalid UTF-8 in schema string"
    Right t -> Right (t, budget')

-- | Read the @[offset]@ vector at the uoffset in @fieldPos@, charging
-- four units per element before the position vector is allocated.
readChargedOffsets :: ByteString -> Int -> Pos -> Either String (V.Vector Pos, Int)
readChargedOffsets bs budget fieldPos = do
  vecPos <- followUOffset bs fieldPos
  n <- vectorLength bs vecPos
  budget' <- charge (4 * n) budget
  ps <- readVectorOfOffsets bs vecPos
  Right (ps, budget')

-- | 'readType' with its variable-size parts (union type ids, the
-- timestamp zone string) charged before they are decoded.
readTypeCharged :: ByteString -> Int -> Int -> Maybe Pos -> Either String (ArrowType, Int)
readTypeCharged bs budget tag mpos = do
  cost <- case (tag, mpos) of
    (10, Just p) -> slotCost p (\vp -> (\s -> 4 + BS.length s) <$> readStringSlice bs vp)
    (14, Just p) -> slotCost p (\vp -> (* 4) <$> vectorLength bs vp)
    _ -> Right 0
  budget' <- charge cost budget
  ty <- readType bs tag mpos
  Right (ty, budget')
  where
    -- Both variable parts live behind slot 1 of their type table.
    slotCost p measure = do
      s <- resolveTable bs p
      case s 1 of
        Nothing -> Right 0
        Just fp -> followUOffset bs fp >>= measure

readFields :: ByteString -> Int -> Int -> V.Vector Pos -> Either String (V.Vector Field, Int)
readFields bs depth budget0 ps = go 0 budget0 []
  where
    !n = V.length ps
    go !i !budget acc
      | i >= n = Right (V.fromListN n (reverse acc), budget)
      | otherwise = do
          (f, budget') <- readField bs depth budget (V.unsafeIndex ps i)
          go (i + 1) budget' (f : acc)

-- | Decode one @Field@ table (and its children), returning the
-- remaining schema budget.
readField :: ByteString -> Int -> Int -> Pos -> Either String (Field, Int)
readField bs depth budget0 fldPos = do
  when' (depth >= maxSchemaDepth) $
    Left ("Arrow.FlatBufferIPC: schema nests deeper than " ++ show maxSchemaDepth ++ " levels")
  budget1 <- charge 8 budget0
  slot <- resolveTable bs fldPos
  (name, budget2) <- case slot 0 of
    Nothing -> Right ("", budget1)
    Just p  -> readChargedString bs budget1 p
  nullable <- case slot 1 of
    Nothing -> Right False
    Just p  -> do
      v <- peekU8 bs p
      Right (v /= 0)
  tyTag <- case slot 2 of
    Nothing -> Right 0
    Just p  -> peekU8 bs p
  (ty, budget3) <- case slot 3 of
    Nothing  -> readTypeCharged bs budget2 (fromIntegral tyTag) Nothing
    Just p   -> do
      tyPos <- followUOffset bs p
      readTypeCharged bs budget2 (fromIntegral tyTag) (Just tyPos)
  dictionary <- case slot 4 of
    Nothing -> Right Nothing
    Just p  -> do
      dePos <- followUOffset bs p
      Just <$> readDictionaryEncodingTable bs dePos
  (customMd, budget4) <- readKeyValues bs budget3 (slot 6)
  (children, budget5) <- case slot 5 of
    Nothing -> Right (V.empty, budget4)
    Just p  -> do
      (childPositions, budget4') <- readChargedOffsets bs budget4 p
      readFields bs (depth + 1) budget4' childPositions
  Right
    ( Field
        { fieldName     = name
        , fieldNullable = nullable
        , fieldType     = ty
        , fieldChildren = children
        , fieldDictionary = dictionary
        , fieldMetadata = customMd
        }
    , budget5
    )

-- | Decode an optional @[KeyValue]@ vector slot, charging each entry.
readKeyValues :: ByteString -> Int -> Maybe Pos -> Either String (V.Vector (T.Text, T.Text), Int)
readKeyValues _ budget Nothing = Right (V.empty, budget)
readKeyValues bs budget00 (Just p) = do
  (kvPositions, budget0) <- readChargedOffsets bs budget00 p
  let !n = V.length kvPositions
      go !i !budget acc
        | i >= n = Right (V.fromListN n (reverse acc), budget)
        | otherwise = do
            (kv, budget') <- readKeyValue bs budget (V.unsafeIndex kvPositions i)
            go (i + 1) budget' (kv : acc)
  go 0 budget0 []

-- | Decode one @KeyValue@ table (per @format/Schema.fbs@):
--
-- @
-- table KeyValue {
--   key   : string;   // 0
--   value : string;   // 1
-- }
-- @
readKeyValue :: ByteString -> Int -> Pos -> Either String ((T.Text, T.Text), Int)
readKeyValue bs budget0 kvPos = do
  budget1 <- charge 8 budget0
  slot <- resolveTable bs kvPos
  (k, budget2) <- case slot 0 of
    Nothing -> Right ("", budget1)
    Just p  -> readChargedString bs budget1 p
  (v, budget3) <- case slot 1 of
    Nothing -> Right ("", budget2)
    Just p  -> readChargedString bs budget2 p
  Right ((k, v), budget3)

-- | Decode a 'DictionaryEncoding' table:
--
-- @
-- table DictionaryEncoding {
--   id: long;
--   indexType: Int;
--   isOrdered: bool;
--   dictionaryKind: DictionaryKind;
-- }
-- @
readDictionaryEncodingTable :: ByteString -> Pos -> Either String DictionaryEncoding
readDictionaryEncodingTable bs dePos = do
  s <- resolveTable bs dePos
  did <- case s 0 of
    Nothing -> Right 0
    Just b  -> peekI64 bs b
  idxTy <- case s 1 of
    Nothing -> Right (AInt 32 True)   -- spec default
    Just b  -> do
      tyPos <- followUOffset bs b
      readType bs 2 (Just tyPos)
  ordered <- case s 2 of
    Nothing -> Right False
    Just b  -> do
      v <- peekU8 bs b
      Right (v /= 0)
  Right (DictionaryEncoding did idxTy ordered)

-- | Decode a @Type@ union variant. The discriminator (@type_type@)
-- selects which sub-table layout to read at @typePos@.
readType :: ByteString -> Int -> Maybe Pos -> Either String ArrowType
readType _  0 _ = Right ANull   -- "None" / Null
readType _  1 _ = Right ANull
readType bs 2 (Just p) = do
  -- Int { bitWidth: i32, is_signed: bool }. Schema.fbs gives
  -- is_signed no explicit default, so an absent slot is the bool
  -- default (false); pyarrow omits it for unsigned columns.
  s <- resolveTable bs p
  bits <- case s 0 of
    Nothing -> Right 32
    Just b  -> peekI32 bs b
  signed <- case s 1 of
    Nothing -> Right False
    Just b  -> do
      v <- peekU8 bs b
      Right (v /= 0)
  Right (AInt (fromIntegral bits) signed)
readType bs 3 (Just p) = do
  -- FloatingPoint { precision: Precision }; an absent slot is the
  -- enum's first value, HALF (pyarrow omits it for float16).
  s <- resolveTable bs p
  prec <- case s 0 of
    Nothing -> Right 0
    Just b  -> peekI16 bs b
  case prec of
    0 -> Right (AFloatingPoint Half)
    1 -> Right (AFloatingPoint Single)
    2 -> Right (AFloatingPoint DoublePrecision)
    n -> Left $ "Arrow.FlatBufferIPC: unknown precision " ++ show n
readType _  4 _ = Right ABinary
readType _  5 _ = Right AUtf8
readType _  6 _ = Right ABool
readType bs 7 (Just p) = do
  s <- resolveTable bs p
  prec  <- case s 0 of { Nothing -> Right 0; Just b -> peekI32 bs b }
  scale <- case s 1 of { Nothing -> Right 0; Just b -> peekI32 bs b }
  bw    <- case s 2 of { Nothing -> Right 128; Just b -> peekI32 bs b }
  case bw of
    128 -> Right (ADecimal (fromIntegral prec) (fromIntegral scale))
    256 -> Right (ADecimal256 (fromIntegral prec) (fromIntegral scale))
    n   -> Left $ "Arrow.FlatBufferIPC: unsupported decimal bitWidth " ++ show n
readType bs 8 (Just p) = do
  s <- resolveTable bs p
  u <- case s 0 of { Nothing -> Right 1; Just b -> peekI16 bs b }
  case u of
    0 -> Right (ADate DateDay)
    1 -> Right (ADate DateMillisecond)
    n -> Left $ "Arrow.FlatBufferIPC: unknown date unit " ++ show n
readType bs 9 (Just p) = do
  s <- resolveTable bs p
  u  <- case s 0 of { Nothing -> Right 1;  Just b -> peekI16 bs b }
  bw <- case s 1 of { Nothing -> Right 32; Just b -> peekI32 bs b }
  unit <- timeUnitFromTag (fromIntegral u)
  Right (ATime unit (fromIntegral bw))
readType bs 10 (Just p) = do
  s <- resolveTable bs p
  u  <- case s 0 of { Nothing -> Right 0; Just b -> peekI16 bs b }
  tz <- case s 1 of
    Nothing -> Right Nothing
    Just b  -> do
      strPos <- followUOffset bs b
      Just <$> readString bs strPos
  unit <- timeUnitFromTag (fromIntegral u)
  Right (ATimestamp unit tz)
readType bs 11 (Just p) = do
  s <- resolveTable bs p
  u <- case s 0 of { Nothing -> Right 0; Just b -> peekI16 bs b }
  iu <- case u of
    0 -> Right YearMonth
    1 -> Right DayTime
    2 -> Right MonthDayNano
    n -> Left $ "Arrow.FlatBufferIPC: unknown interval unit " ++ show n
  Right (AInterval iu)
readType _  12 _ = Right AList
readType _  13 _ = Right AStruct
readType bs 14 (Just p) = do
  s <- resolveTable bs p
  m <- case s 0 of { Nothing -> Right 0; Just b -> peekI16 bs b }
  mode <- case m of
    0 -> Right Sparse
    1 -> Right Dense
    n -> Left $ "Arrow.FlatBufferIPC: unknown union mode " ++ show n
  ids <- case s 1 of
    Nothing -> Right V.empty
    Just b  -> do
      vecPos <- followUOffset bs b
      (_, elems) <- readVectorOfStructs bs vecPos 4
      V.mapM (peekI32 bs) elems
  Right (AUnion mode ids)
readType bs 15 (Just p) = do
  s <- resolveTable bs p
  bw <- case s 0 of { Nothing -> Right 0; Just b -> peekI32 bs b }
  Right (AFixedSizeBinary (fromIntegral bw))
readType bs 16 (Just p) = do
  s <- resolveTable bs p
  ls <- case s 0 of { Nothing -> Right 0; Just b -> peekI32 bs b }
  Right (AFixedSizeList (fromIntegral ls))
readType bs 17 (Just p) = do
  s <- resolveTable bs p
  sorted <- case s 0 of
    Nothing -> Right False
    Just b  -> do
      v <- peekU8 bs b
      Right (v /= 0)
  Right (AMap sorted)
readType bs 18 (Just p) = do
  s <- resolveTable bs p
  u <- case s 0 of { Nothing -> Right 1; Just b -> peekI16 bs b }
  unit <- timeUnitFromTag (fromIntegral u)
  Right (ADuration unit)
readType _  19 _ = Right ALargeBinary
readType _  20 _ = Right ALargeUtf8
readType _  21 _ = Right ALargeList
readType _  22 _ = Right ARunEndEncoded
readType _  23 _ = Right ABinaryView
readType _  24 _ = Right AUtf8View
readType _  25 _ = Right AListView
readType _  26 _ = Right ALargeListView
readType _  n  _ = Left $ "Arrow.FlatBufferIPC: unsupported Type discriminator " ++ show n

timeUnitFromTag :: Int -> Either String TimeUnit
timeUnitFromTag 0 = Right Second
timeUnitFromTag 1 = Right Millisecond
timeUnitFromTag 2 = Right Microsecond
timeUnitFromTag 3 = Right Nanosecond
timeUnitFromTag n = Left $ "Arrow.FlatBufferIPC: unknown time unit " ++ show n

-- | Decode a @RecordBatch@ table.
readRecordBatchTable :: ByteString -> Pos -> Either String RecordBatchDef
readRecordBatchTable bs rbPos = do
  s <- resolveTable bs rbPos
  len <- case s 0 of
    Nothing -> Right 0
    Just b  -> peekI64 bs b
  nodes <- case s 1 of
    Nothing -> Right V.empty
    Just b  -> do
      vecPos <- followUOffset bs b
      (_, elems) <- readVectorOfStructs bs vecPos 16
      V.mapM (\ep -> do
                l <- peekI64 bs ep
                nc <- peekI64 bs (ep + 8)
                Right (FieldNode l nc)) elems
  bufs <- case s 2 of
    Nothing -> Right V.empty
    Just b  -> do
      vecPos <- followUOffset bs b
      (_, elems) <- readVectorOfStructs bs vecPos 16
      V.mapM (\ep -> do
                o <- peekI64 bs ep
                l <- peekI64 bs (ep + 8)
                Right (Buffer o l)) elems
  variadic <- case s 4 of
    Nothing -> Right V.empty
    Just b  -> do
      vecPos <- followUOffset bs b
      V.fromList <$> readVectorInt64 bs vecPos
  -- Slot 3 is BodyCompression (a table). When present we read
  -- the codec discriminator and translate to our enum.
  bodyComp <- case s 3 of
    Nothing -> Right Nothing
    Just b  -> do
      bcPos <- followUOffset bs b
      bcSlot <- resolveTable bs bcPos
      codec <- case bcSlot 0 of
        Nothing -> Right (0 :: Int)
        Just p  -> do
          v <- peekU8 bs p
          Right (fromIntegral v)
      case codec of
        0 -> Right (Just LZ4Frame)
        1 -> Right (Just BodyZstd)
        n -> Left $ "Arrow.FlatBufferIPC: unknown BodyCompression codec " ++ show n
  Right RecordBatchDef
    { rbLength  = len
    , rbNodes   = nodes
    , rbBuffers = bufs
    , rbVariadicBufferCounts = variadic
    , rbBodyCompression = bodyComp
    }

-- | Decode a Schema-typed @Message@ flatbuffer (just the metadata
-- bytes; the caller has already stripped the encapsulated framing).
decodeSchemaMessage :: ByteString -> Either String Schema
decodeSchemaMessage meta = do
  msgPos <- fromIntegral <$> peekU32 meta 0
  s <- resolveTable meta msgPos
  ht <- case s 1 of
    Nothing -> Left "Arrow.FlatBufferIPC: Message header_type missing"
    Just b  -> peekU8 meta b
  when' (ht /= 1) $
    Left ("Arrow.FlatBufferIPC: expected Schema header (1), got " ++ show ht)
  case s 2 of
    Nothing -> Left "Arrow.FlatBufferIPC: Message header (Schema) missing"
    Just b  -> do
      schPos <- followUOffset meta b
      readSchemaTable meta schPos

-- | Decode a RecordBatch-typed @Message@ flatbuffer to
-- @(RecordBatchDef, bodyLength)@.
decodeRecordBatchMessage :: ByteString -> Either String (RecordBatchDef, Int64)
decodeRecordBatchMessage meta = do
  msgPos <- fromIntegral <$> peekU32 meta 0
  s <- resolveTable meta msgPos
  ht <- case s 1 of
    Nothing -> Left "Arrow.FlatBufferIPC: Message header_type missing"
    Just b  -> peekU8 meta b
  when' (ht /= 3) $
    Left ("Arrow.FlatBufferIPC: expected RecordBatch header (3), got " ++ show ht)
  rb <- case s 2 of
    Nothing -> Left "Arrow.FlatBufferIPC: Message header (RecordBatch) missing"
    Just b  -> do
      rbPos <- followUOffset meta b
      readRecordBatchTable meta rbPos
  bodyLen <- case s 3 of
    Nothing -> Right 0
    Just b  -> peekI64 meta b
  Right (rb, bodyLen)

-- | One decoded dictionary batch — the raw payload that defines a
-- dictionary's index → value mapping. The @data@ field is the
-- inner @RecordBatch@ (a single column whose values are the
-- dictionary values, in index order).
data DictBatch = DictBatch
  { dbId      :: !Int64
    -- ^ Dictionary id; matches @DictionaryEncoding.id@ in the
    -- schema.
  , dbIsDelta :: !Bool
    -- ^ When @True@, the values append to the existing dictionary
    -- with this id; otherwise they replace it.
  , dbData    :: !RecordBatchDef
  , dbBody    :: !ByteString
  } deriving stock (Show, Eq)

-- | Decode a DictionaryBatch-typed @Message@ flatbuffer to
-- @(id, isDelta, RecordBatchDef, bodyLength)@.
decodeDictionaryBatchMessage
  :: ByteString -> Either String (Int64, Bool, RecordBatchDef, Int64)
decodeDictionaryBatchMessage meta = do
  msgPos <- fromIntegral <$> peekU32 meta 0
  s <- resolveTable meta msgPos
  ht <- case s 1 of
    Nothing -> Left "Arrow.FlatBufferIPC: Message header_type missing"
    Just b  -> peekU8 meta b
  when' (ht /= 2) $
    Left ("Arrow.FlatBufferIPC: expected DictionaryBatch header (2), got " ++ show ht)
  dbTblPos <- case s 2 of
    Nothing -> Left "Arrow.FlatBufferIPC: Message header (DictionaryBatch) missing"
    Just b  -> followUOffset meta b
  ds <- resolveTable meta dbTblPos
  did <- case ds 0 of
    Nothing -> Right 0
    Just b  -> peekI64 meta b
  rb <- case ds 1 of
    Nothing -> Left "Arrow.FlatBufferIPC: DictionaryBatch.data missing"
    Just b  -> do
      rbPos <- followUOffset meta b
      readRecordBatchTable meta rbPos
  isDelta <- case ds 2 of
    Nothing -> Right False
    Just b  -> do
      v <- peekU8 meta b
      Right (v /= 0)
  bodyLen <- case s 3 of
    Nothing -> Right 0
    Just b  -> peekI64 meta b
  Right (did, isDelta, rb, bodyLen)

-- | Parse an Arrow IPC stream produced by any spec-compliant
-- writer (pyarrow / arrow-cpp / arrow-rs) into wireform's
-- 'Schema' + a list of @(RecordBatchDef, body bytes)@ pairs.
--
-- Recognises both the post-0.15.0 framing (continuation marker +
-- length) and the legacy framing (positive length first, no
-- continuation), per the @ConsumeInitial@ logic in arrow-cpp's
-- @message.cc@.
readArrowStreamFB
  :: ByteString
  -> Either String (Schema, [(RecordBatchDef, ByteString)])
readArrowStreamFB bs0 = do
  (sch, _, batches) <- readArrowStreamFBWithDicts bs0
  Right (sch, batches)

-- | Frame variant for 'readArrowStreamFBInterleaved'. Preserves
-- the stream-order sequence of dict + record messages so a
-- downstream reader can honour replacement / delta dictionary
-- semantics.
data StreamFrame
  = SFDict  !DictBatch
  | SFBatch !RecordBatchDef !ByteString
  deriving (Show, Eq)

-- | Like 'readArrowStreamFBWithDicts' but returns the dict /
-- record batches /interleaved/ in stream order. Use this when
-- a writer may emit @isDelta=false@ replacement dict batches
-- between record batches: the flat list gives each record batch
-- the opportunity to resolve against the most-recently-seen dict
-- for that id.
readArrowStreamFBInterleaved
  :: ByteString
  -> Either String (Schema, [StreamFrame])
readArrowStreamFBInterleaved bs0 = do
  (schema, after) <- consumeSchema bs0
  frames <- goFrames after []
  Right (schema, frames)
  where
    consumeSchema bs = do
      (mlen, meta, rest) <- readFrameHeader bs
      when' (mlen <= 0) $
        Left "Arrow.FlatBufferIPC: unexpected EOS while reading schema"
      sch <- decodeSchemaMessage meta
      Right (sch, rest)

    goFrames bs acc
      | BS.length bs < 4 = Right (reverse acc)
      | otherwise = do
          (mlen, meta, rest1) <- readFrameHeader bs
          if mlen == 0
            then Right (reverse acc)
            else do
              ht <- peekHeaderType meta
              case ht of
                3 -> do
                  (rb, bodyLen) <- decodeRecordBatchMessage meta
                  (body, rest2) <- takeFrameBody "record batch body" bodyLen rest1
                  goFrames rest2 (SFBatch rb body : acc)
                2 -> do
                  (did, isDelta, rb, bodyLen) <-
                    decodeDictionaryBatchMessage meta
                  (body, rest2) <- takeFrameBody "dictionary batch body" bodyLen rest1
                  let !db = DictBatch { dbId = did, dbIsDelta = isDelta
                                      , dbData = rb, dbBody = body }
                  goFrames rest2 (SFDict db : acc)
                _ ->
                  Left ("Arrow.FlatBufferIPC: unsupported message header_type "
                        ++ show ht)

-- | Like 'readArrowStreamFB' but also returns any 'DictBatch'
-- frames encountered (in stream order). Most pyarrow / arrow-cpp
-- streams emit dictionary batches before the first record batch
-- whose schema references their @id@.
readArrowStreamFBWithDicts
  :: ByteString
  -> Either String (Schema, [DictBatch], [(RecordBatchDef, ByteString)])
readArrowStreamFBWithDicts bs0 = do
  (schema, frames) <- readArrowStreamFBInterleaved bs0
  let split (SFDict db) (ds, bs) = (db : ds, bs)
      split (SFBatch rb body) (ds, bs) = (ds, (rb, body) : bs)
      (dicts, batches) = foldr split ([], []) frames
  Right (schema, dicts, batches)

-- | Look up the @header_type@ ubyte from a Message flatbuffer.
peekHeaderType :: ByteString -> Either String Int
peekHeaderType meta = do
  msgPos <- fromIntegral <$> peekU32 meta 0
  s <- resolveTable meta msgPos
  case s 1 of
    Nothing -> Right 0
    Just b  -> fromIntegral <$> peekU8 meta b

-- | Encapsulated frame (continuation marker, length, FlatBuffers
-- @Message@ metadata, padding) for one 'Message'. The body is not
-- included: the metadata's @bodyLength@ is the 8-aligned extent of
-- the batch's buffers, and the caller appends that many body bytes.
encodeMessageFrame :: Message -> ByteString
encodeMessageFrame = \case
  SchemaMessage sch -> encapsulateMessage (buildSchemaMessage sch) BS.empty
  RecordBatch rb -> encapsulateMessage (buildRecordBatchMessage rb (bodyExtent rb)) BS.empty
  DictionaryBatch did isDelta rb ->
    encapsulateMessage (buildDictionaryBatchMessageLen did isDelta rb (bodyExtent rb)) BS.empty
  where
    bodyExtent rb =
      let !end = V.foldl' (\m buf -> max m (bufOffset buf + bufLength buf)) 0 (rbBuffers rb)
      in (end + 7) .&. complement 7

-- | Parse one encapsulated frame (with or without the continuation
-- marker) into its 'Message' and the @bodyLength@ it declares. The
-- end-of-stream marker and unsupported header types are 'Left'.
decodeMessageFrame :: ByteString -> Either String (Message, Int64)
decodeMessageFrame bs = do
  (mlen, meta, _) <- readFrameHeader bs
  when' (mlen == 0) $
    Left "Arrow.FlatBufferIPC: end-of-stream marker, not a message"
  when' (BS.length meta < mlen) $
    Left "Arrow.FlatBufferIPC: truncated message metadata"
  ht <- peekHeaderType meta
  case ht of
    1 -> (\s -> (SchemaMessage s, 0)) <$> decodeSchemaMessage meta
    2 -> (\(did, isDelta, rb, n) -> (DictionaryBatch did isDelta rb, n)) <$> decodeDictionaryBatchMessage meta
    3 -> (\(rb, n) -> (RecordBatch rb, n)) <$> decodeRecordBatchMessage meta
    _ -> Left ("Arrow.FlatBufferIPC: unsupported message header_type " ++ show ht)

-- | Parse an Arrow IPC /file/ (per @format/File.fbs@), accepting
-- either the legacy stream-shaped output of 'writeArrowFileFB' or
-- the canonical pyarrow / arrow-cpp file with a trailing 'Footer'.
-- The strategy: skip the 8-byte @ARROW1\\0\\0@ header and parse the
-- contents as a stream. The trailing @Footer + length + ARROW1@
-- comes after the EOS marker so 'readArrowStreamFB' stops there.
readArrowFileFB
  :: ByteString
  -> Either String (Schema, [(RecordBatchDef, ByteString)])
readArrowFileFB bs = do
  (sch, _, batches) <- readArrowFileFBWithDicts bs
  Right (sch, batches)

-- | Like 'readArrowFileFB' but also returns any 'DictBatch'
-- frames the file contains.
readArrowFileFBWithDicts
  :: ByteString
  -> Either String (Schema, [DictBatch], [(RecordBatchDef, ByteString)])
readArrowFileFBWithDicts bs = do
  when' (BS.length bs < 14) $
    Left "Arrow.FlatBufferIPC: input too small to be an Arrow file"
  when' (BS.take 6 bs /= "ARROW1") $
    Left "Arrow.FlatBufferIPC: missing leading ARROW1 magic"
  when' (BS.takeEnd 6 bs /= "ARROW1") $
    Left "Arrow.FlatBufferIPC: missing trailing ARROW1 magic"
  readArrowStreamFBWithDicts (BS.drop 8 bs)

-- | Strip one encapsulated-message frame:
--
--   * 4 bytes continuation (0xFFFFFFFF) — optional in legacy mode
--   * 4 bytes metadata_length (i32 LE)
--   * @metadata_length@ metadata bytes (already padded)
--
-- Returns @(mlen, metadata bytes, rest of stream after metadata)@.
-- @mlen == 0@ signals the EOS marker.
readFrameHeader
  :: ByteString
  -> Either String (Int, ByteString, ByteString)
readFrameHeader bs = do
  when' (BS.length bs < 4) $
    Left "Arrow.FlatBufferIPC: truncated frame header"
  first4 <- peekU32 bs 0
  if first4 == 0xFFFFFFFF
    then do
      when' (BS.length bs < 8) $
        Left "Arrow.FlatBufferIPC: truncated frame after continuation"
      mlen <- peekI32 bs 4
      metadataAt 8 (fromIntegral mlen)
    else
      -- Legacy: first 4 bytes are the metadata length itself.
      if first4 == 0
        then Right (0, BS.empty, BS.drop 4 bs)
        else metadataAt 4 (fromIntegral first4)
  where
    metadataAt :: Int -> Int -> Either String (Int, ByteString, ByteString)
    metadataAt start mlenI
      | mlenI < 0 = Left "Arrow.FlatBufferIPC: negative metadata length"
      | mlenI > BS.length bs - start =
          Left
            ( "Arrow.FlatBufferIPC: frame claims "
                ++ show mlenI
                ++ " metadata bytes, "
                ++ show (BS.length bs - start)
                ++ " remain"
            )
      | otherwise =
          Right ( mlenI
                , BS.take mlenI (BS.drop start bs)
                , BS.drop (start + mlenI) bs
                )
