{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
-- | Arrow IPC decoder: parses the @Schema@, @Field@, @Type@,
-- @RecordBatch@, @DictionaryBatch@, @Tensor@ and @SparseTensor@
-- flatbuffer tables emitted by pyarrow / arrow-rs / arrow-cpp,
-- walks the encapsulated-message framing of streams and files,
-- and undoes body compression. See "Arrow.FlatBufferIPC" for the
-- format notes.
module Arrow.FlatBufferIPC.Read
  ( -- * Body compression
    decompressBody
    -- * Reader (parses pyarrow / arrow-cpp output)
  , readArrowStreamFB
  , readArrowFileFB
  , readArrowFileFBWithDicts
  , decodeSchemaMessage
  , decodeRecordBatchMessage
  , decodeDictionaryBatchMessage
  , decodeRecordBatch
  , decodeDictionaryBatch
    -- * Dictionary support
  , readArrowStreamFBWithDicts
  , readArrowStreamFBInterleaved
  , StreamFrame (..)
    -- * Tensor / SparseTensor
  , decodeTensorMessage
  , decodeTensorFrame
  , decodeSparseTensorFrame
    -- * Single-message frames
  , decodeMessageFrame
  , readFrameHeader
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Foreign.Storable (Storable)

import Arrow.Column.Internal (bytesToStorable)
import Arrow.FlatBufferIPC.Common
import Arrow.Read.Columns (decodeDictionaryBatch, decodeRecordBatch, decompressBody)
import Arrow.Types
import FlatBuffers.Builder (alignUp)
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

-- | The vector of 16-byte structs (@FieldNode@ or @Buffer@) at the
-- uoffset in @slotPos@: one length check, then an O(1) alias of the
-- payload (copied once only when it is not 8-aligned in memory).
structVector :: Storable a => ByteString -> Pos -> Either String (VS.Vector a)
structVector bs slotPos = do
  vecPos <- followUOffset bs slotPos
  n <- vectorLength bs vecPos
  -- vectorLength succeeded, so 0 <= vecPos <= length - 4.
  if n > (BS.length bs - vecPos - 4) `quot` 16
    then Left ("Arrow.FlatBufferIPC: record batch vector of " ++ show n ++ " structs overruns the message")
    else Right $! bytesToStorable (BS.take (n * 16) (BS.drop (vecPos + 4) bs))

-- | Decode a @RecordBatch@ table.
readRecordBatchTable :: ByteString -> Pos -> Either String RecordBatchDef
readRecordBatchTable bs rbPos = do
  s <- resolveTable bs rbPos
  len <- case s 0 of
    Nothing -> Right 0
    Just b  -> peekI64 bs b
  nodes <- maybe (Right VS.empty) (structVector bs) (s 1)
  bufs <- maybe (Right VS.empty) (structVector bs) (s 2)
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
