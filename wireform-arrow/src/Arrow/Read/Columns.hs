{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | Decode Arrow IPC record batch and dictionary batch bodies into
'ColumnArray's without copying.

Every buffer of the result aliases the message body (or, for a
compressed body, the freshly decompressed buffer). A buffer is copied
only when its address is not aligned for the element type (arrow-rs
@align_buffers@) or when the schema declares big-endian data, which is
byte-swapped into a fresh buffer.

Decoding validates everything arrow-rs validates by default, in O(1)
Haskell work per array plus C kernels: buffer bounds, buffer sizes
against array lengths, null counts against the validity bitmap
(popcount), offsets (monotonic, in range), UTF-8 and character
boundaries, list views, union type ids and dense offsets, run ends,
views. Dictionary keys are checked against their values when the
dictionary is resolved ('Arrow.Column.resolveDictionaryColumn'). A
corrupt or hostile batch is a 'Left', never an out-of-bounds read.

The body must use the IPC specification's buffer layout: a validity
slot for every array except null, union and run-end-encoded arrays
(an empty slot when the array has no nulls).
-}
module Arrow.Read.Columns (
  decodeRecordBatch,
  decodeDictionaryBatch,

  -- * Body compression
  decompressBody,

  -- * Buffer bounds
  validateRecordBatchBuffers,
) where

import Arrow.Column (columnLength, placeholderColumn)
import Arrow.Column.Internal (
  ColumnArray,
  IntegralPrim (..),
  Offset,
  PrimType (..),
  SomePrimType (..),
  integralPrim,
  primTypeFor,
  primWidth,
  withPrim,
 )
import Arrow.Column.Internal qualified as I
import Arrow.FlatBufferIPC.Common (DictBatch (..))
import Arrow.Types (
  ArrowType (..),
  BodyCompressionCodec (..),
  Buffer (..),
  DictionaryEncoding (..),
  Endianness (..),
  Field (..),
  FieldNode (..),
  RecordBatchDef (..),
  Schema (..),
  UnionMode (..),
 )
import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Int (Int32, Int64, Int8)
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Data.Word (Word8)
import Foreign.Marshal.Utils (copyBytes, fillBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable (..), peekByteOff, pokeByteOff)
import System.IO.Unsafe (unsafeDupablePerformIO)
import Wireform.FFI (validateArrowBuffers)

#ifdef HAVE_ZSTD
import qualified Codec.Compression.Zstd as Zstd
#endif

#ifdef HAVE_LZ4
import qualified Codec.Lz4 as Lz4
import qualified Control.Exception as Exc
import qualified Data.ByteString.Lazy as BL
#endif


-- ============================================================
-- Entry points
-- ============================================================

{- | Decode one record batch: the schema's top-level fields, in order,
from the batch header and its body. Body compression is undone first
(per buffer). Every top-level column has exactly 'rbLength' rows.

Dictionary-encoded columns come back as @ColDictionary id keys
placeholder@: the keys at their wire width with the row validity, and
an empty placeholder for the values; resolve them with
'Arrow.Column.resolveDictionaryColumn'.
-}
decodeRecordBatch :: Schema -> RecordBatchDef -> ByteString -> Either String (V.Vector ColumnArray)
decodeRecordBatch schema rb body = do
  src <- bodySource rb body
  let !ctx = Ctx (arrowEndianness schema == Big) rb src
  Outs cols _ <- fieldsN ctx (arrowFields schema) (Cur 0 0 0 0)
  case V.findIndex (\c -> fromIntegral (columnLength c) /= rbLength rb) cols of
    Nothing -> Right cols
    Just k ->
      Left
        ( "Arrow.Read.Columns: column #"
            ++ show k
            ++ " has "
            ++ show (columnLength (V.unsafeIndex cols k))
            ++ " rows, the record batch declares "
            ++ show (rbLength rb)
        )


{- | Decode the values of a dictionary batch. The values field is the
schema field (at any depth) whose dictionary encoding carries the
batch's id, with the encoding removed. Returns the id and the values
column; dictionaries nested inside the values stay unresolved
placeholders.
-}
decodeDictionaryBatch :: Schema -> DictBatch -> Either String (Int64, ColumnArray)
decodeDictionaryBatch sch db = do
  f <- case findDictField (dbId db) (arrowFields sch) of
    Just f -> Right f
    Nothing ->
      Left
        ( "Arrow.Read.Columns: dictionary batch with id "
            ++ show (dbId db)
            ++ " doesn't match any field in the schema"
        )
  let !valuesField = f {fieldDictionary = Nothing}
      !inner = sch {arrowFields = V.singleton valuesField, arrowMetadata = V.empty, arrowFeatures = V.empty}
  cols <- decodeRecordBatch inner (dbData db) (dbBody db)
  if V.length cols == 1
    then Right (dbId db, V.unsafeHead cols)
    else Left "Arrow.Read.Columns: dictionary batch must hold exactly one column"


-- | The field whose dictionary encoding carries the id (depth first).
findDictField :: Int64 -> V.Vector Field -> Maybe Field
findDictField did = go
  where
    go fs = V.foldr (\f rest -> pick f rest) Nothing fs
    pick f rest = case fieldDictionary f of
      Just de | deId de == did -> Just f
      _ -> case go (fieldChildren f) of
        Just g -> Just g
        Nothing -> rest


{- | Validate that all buffer offset/length pairs in a 'RecordBatchDef' are
non-negative, within the given body length, and non-overlapping.
SIMD-accelerated; reads the Storable buffer vector in place.
-}
validateRecordBatchBuffers :: RecordBatchDef -> Int64 -> Bool
validateRecordBatchBuffers rb bodyLen
  | n == 0 = True
  | otherwise = unsafeDupablePerformIO $ VS.unsafeWith bufs $ \p ->
      pure $! validateArrowBuffers (castPtr p) n bodyLen
  where
    !bufs = rbBuffers rb
    !n = VS.length bufs


-- ============================================================
-- Body sources
-- ============================================================

-- | Where buffer @i@ lives.
data Source
  = -- | Uncompressed body; buffer descriptors already bounds-checked.
    Plain !ByteString
  | -- | One decompressed buffer per descriptor.
    Unpacked !(V.Vector ByteString)


bodySource :: RecordBatchDef -> ByteString -> Either String Source
bodySource rb body = case rbBodyCompression rb of
  Nothing
    | validateRecordBatchBuffers rb (fromIntegral (BS.length body)) -> Right (Plain body)
    | otherwise -> Left "Arrow.Read.Columns: invalid buffer bounds in RecordBatchDef"
  Just codec -> Unpacked <$> decompressBuffers codec (rbBuffers rb) body


-- ============================================================
-- Walk state
-- ============================================================

data Ctx = Ctx
  { ctxSwap :: !Bool
  -- ^ Big-endian body: multi-byte values are byte-swapped into fresh buffers.
  , ctxRb :: !RecordBatchDef
  , ctxSrc :: !Source
  }


-- | Next field node, next buffer, next variadic-count entry, unbacked rows so far.
data Cur = Cur
  { curNode :: {-# UNPACK #-} !Int
  , curBuf :: {-# UNPACK #-} !Int
  , curVar :: {-# UNPACK #-} !Int
  , curUnbacked :: {-# UNPACK #-} !Int
  }


data Out = Out !ColumnArray {-# UNPACK #-} !Cur


data Outs = Outs !(V.Vector ColumnArray) {-# UNPACK #-} !Cur


-- | Advance past @nodes@ field nodes and @bufs@ buffers.
adv :: Int -> Int -> Cur -> Cur
adv nodes bufs (Cur n b v u) = Cur (n + nodes) (b + bufs) v u
{-# INLINE adv #-}


{- | Upper bound on any array length taken from a field node. Far above
any batch that fits in memory, and small enough that @len * 32@ and
@(len + 1) * 8@ cannot overflow 'Int'.
-}
maxArrayLength :: Int
maxArrayLength = 1 `shiftL` 40


{- | Rows a batch may claim without any body bytes backing them (nullable
structs or fixed-size lists over null-typed or run-end-encoded
children, zero-width fixed-size binary). Generous for real data, small
enough that a hostile length cannot make every later per-row pass over
the column run for hours.
-}
maxUnbackedRows :: Int
maxUnbackedRows = 1 `shiftL` 20


-- | Field node @i@ with its length checked to lie in @[0, 'maxArrayLength']@.
nodeAt :: Ctx -> Int -> Either String FieldNode
nodeAt ctx i = case rbNodes (ctxRb ctx) VS.!? i of
  Nothing ->
    Left
      ( "Arrow.Read.Columns: record batch has "
          ++ show (VS.length (rbNodes (ctxRb ctx)))
          ++ " field nodes, schema needs node #"
          ++ show i
      )
  Just fn
    | fnLength fn < 0 || fnLength fn > fromIntegral maxArrayLength ->
        Left ("Arrow.Read.Columns: invalid field node length " ++ show (fnLength fn))
    | otherwise -> Right fn
{-# INLINE nodeAt #-}


-- | Bytes of buffer @i@.
bufferAt :: Ctx -> Int -> Either String ByteString
bufferAt ctx i
  | i < 0 || i >= VS.length bufs =
      Left
        ( "Arrow.Read.Columns: record batch has "
            ++ show (VS.length bufs)
            ++ " buffers, schema layout needs buffer #"
            ++ show i
        )
  | otherwise = case ctxSrc ctx of
      Unpacked v -> Right (V.unsafeIndex v i)
      Plain body ->
        let Buffer o l = VS.unsafeIndex bufs i
        in if o < 0 || l < 0 || o > fromIntegral (BS.length body) - l
             then Left "Arrow.Read.Columns: buffer outside the body"
             else Right $! BSU.unsafeTake (fromIntegral l) (BSU.unsafeDrop (fromIntegral o) body)
  where
    !bufs = rbBuffers (ctxRb ctx)
{-# INLINE bufferAt #-}


-- ============================================================
-- Schema walk
-- ============================================================

fieldsN :: Ctx -> V.Vector Field -> Cur -> Either String Outs
fieldsN ctx fields cur0 = go 0 cur0 []
  where
    !n = V.length fields
    go !i !cur acc
      | i >= n = Right (Outs (V.fromListN n (reverse acc)) cur)
      | otherwise = do
          Out c cur' <- field ctx (V.unsafeIndex fields i) cur
          go (i + 1) cur' (c : acc)


field :: Ctx -> Field -> Cur -> Either String Out
field ctx f cur = do
  fn <- nodeAt ctx (curNode cur)
  let !len = fromIntegral (fnLength fn) :: Int
      !ub = if allocatesUnbacked f then curUnbacked cur + len else curUnbacked cur
  if ub > maxUnbackedRows
    then
      Left
        ( "Arrow.Read.Columns: record batch claims "
            ++ show ub
            ++ " rows with no buffer backing them (limit "
            ++ show maxUnbackedRows
            ++ ")"
        )
    else fieldAt ctx f fn len cur {curUnbacked = ub}


fieldAt :: Ctx -> Field -> FieldNode -> Int -> Cur -> Either String Out
fieldAt ctx f fn len cur = case fieldDictionary f of
  Just de -> dictKeys ctx f de fn len cur
  Nothing -> case fieldType f of
    ANull -> Right (Out (I.ColNull len) (adv 1 0 cur))
    ABool -> do
      v <- validityAt ctx "bool" len fn bi
      bs <- bufferAt ctx (bi + 1)
      bits <- case I.mkBitmap bs 0 len of
        Right b -> Right b
        Left _ -> Left "Arrow.Read.Columns: bool data buffer too small"
      Right (Out (I.ColBool v bits) (adv 1 2 cur))
    AUtf8 -> varLen @Int32 ctx "utf8" True I.ColUtf8 fn len cur
    ABinary -> varLen @Int32 ctx "binary" False I.ColBinary fn len cur
    ALargeUtf8 -> varLen @Int64 ctx "large utf8" True I.ColLargeUtf8 fn len cur
    ALargeBinary -> varLen @Int64 ctx "large binary" False I.ColLargeBinary fn len cur
    AFixedSizeBinary w -> fixedSizeBinary ctx w fn len cur
    AUtf8View -> views ctx True fn len cur
    ABinaryView -> views ctx False fn len cur
    AStruct -> struct ctx f fn len cur
    AList -> list @Int32 ctx "list" I.ColList f fn len cur
    ALargeList -> list @Int64 ctx "large list" I.ColLargeList f fn len cur
    AListView -> listView @Int32 ctx "list view" I.ColListView f fn len cur
    ALargeListView -> listView @Int64 ctx "large list view" I.ColLargeListView f fn len cur
    AFixedSizeList size -> fixedSizeList ctx size f fn len cur
    AMap _ -> mapCol ctx f fn len cur
    AUnion mode ids -> union ctx mode ids f len cur
    ARunEndEncoded -> runEndEncoded ctx f len cur
    ty -> case primTypeFor ty of
      Just (SomePrimType t) -> do
        c <- prim ctx t fn len bi
        Right (Out c (adv 1 2 cur))
      Nothing -> Left ("Arrow.Read.Columns: unsupported type: " ++ show ty)
  where
    !bi = curBuf cur


-- | Does this array claim rows that no buffer backs?
allocatesUnbacked :: Field -> Bool
allocatesUnbacked f
  | Just _ <- fieldDictionary f = False
  | otherwise = case fieldType f of
      AStruct -> fieldNullable f && not (lengthBacked f)
      AFixedSizeList _ -> fieldNullable f && not (lengthBacked f)
      AFixedSizeBinary 0 -> True
      _ -> False


{- | Is the length of an array of this field paid for by bytes in some
buffer of the array or its descendants? Null arrays have no buffers,
and a run-end-encoded array's logical length can be any value its last
run end names, so neither backs a parent.
-}
lengthBacked :: Field -> Bool
lengthBacked f
  | Just _ <- fieldDictionary f = True
  | otherwise = case fieldType f of
      ANull -> False
      ARunEndEncoded -> False
      AStruct -> V.any lengthBacked (fieldChildren f)
      AFixedSizeList n -> n > 0 && V.any lengthBacked (fieldChildren f)
      AFixedSizeBinary w -> w > 0
      _ -> True


-- ============================================================
-- Buffers
-- ============================================================

{- | The validity of an array of @len@ rows from its slot. No nulls is
'Nothing' (whatever the slot holds, as in arrow-rs); otherwise the
bitmap must cover @len@ bits and its clear bits must equal the node's
null count (C popcount).
-}
validityAt :: Ctx -> String -> Int -> FieldNode -> Int -> Either String (Maybe I.Validity)
validityAt ctx what len fn bi
  | nc == 0 = bufferAt ctx bi >> Right Nothing
  | nc < 0 || nc > len = Left ("Arrow.Read.Columns: " ++ what ++ ": null count " ++ show nc ++ " for " ++ show len ++ " rows")
  | otherwise = do
      bs <- bufferAt ctx bi
      let !nbytes = (len + 7) `quot` 8
      if BS.length bs < nbytes
        then Left ("Arrow.Read.Columns: " ++ what ++ ": validity bitmap too small for " ++ show len ++ " rows")
        else do
          let !b = I.Bitmap (BSU.unsafeTake nbytes bs) 0 len
              !nulls = len - I.bitmapSetCount b
          if nulls /= nc
            then
              Left
                ( "Arrow.Read.Columns: "
                    ++ what
                    ++ ": null count "
                    ++ show nc
                    ++ " does not match the validity bitmap ("
                    ++ show nulls
                    ++ ")"
                )
            else Right (Just (I.Validity b nc))
  where
    !nc = fromIntegral (fnNullCount fn) :: Int


-- | @n@ elements of @w@ bytes from buffer @i@, aligned and in native byte order.
storableAt :: forall a. Storable a => Ctx -> String -> [Int] -> Int -> Int -> Either String (VS.Vector a)
storableAt ctx what swapPat n i = do
  bs <- bufferAt ctx i
  let !w = sizeOf (undefined :: a)
      !need = n * w
  if BS.length bs < need
    then Left ("Arrow.Read.Columns: " ++ what ++ " buffer too small for " ++ show n ++ " values")
    else
      let !exact = BSU.unsafeTake need bs
      in Right $!
           if ctxSwap ctx && not (null swapPat)
             then I.unsafeBytesToStorable (swapCopy swapPat w n exact)
             else I.bytesToStorable exact
{-# INLINE storableAt #-}


{- | @rows + 1@ offsets from buffer @i@. A zero-row array may omit the
offsets buffer (it then has the single offset 0).
-}
offsetsAt :: forall o. Offset o => Ctx -> String -> Int -> Int -> Either String (VS.Vector o)
offsetsAt ctx what rows i = do
  bs <- bufferAt ctx i
  if rows == 0 && BS.length bs < sizeOf (undefined :: o)
    then Right (VS.singleton 0)
    else storableAt ctx (what ++ " offsets") [sizeOf (undefined :: o)] (rows + 1) i
{-# INLINE offsetsAt #-}


-- | Byte-swap groups within each element (big-endian bodies).
swapPattern :: PrimType a -> [Int]
swapPattern = \case
  PInt8 -> []
  PUInt8 -> []
  PIntervalDayTime -> [4, 4]
  PIntervalMonthDayNano -> [4, 4, 8]
  t -> [primWidth t]


{- | Copy @n@ elements of @w@ bytes, reversing the byte order of each
group of the pattern within every element (big-endian input only).
-}
swapCopy :: [Int] -> Int -> Int -> ByteString -> ByteString
swapCopy pat w n src = I.createAligned (n * w) $ \dst -> I.withBytesPtr src $ \s ->
  let elemLoop !e
        | e >= n = pure ()
        | otherwise = groups (e * w) pat >> elemLoop (e + 1)
      groups !_ [] = pure ()
      groups !base (g : gs) = swapGroup (s `plusPtr` base) (dst `plusPtr` base) g >> groups (base + g) gs
  in elemLoop 0


swapGroup :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
swapGroup s d g = go 0
  where
    go !k
      | k >= g = pure ()
      | otherwise = do
          b <- peekByteOff s (g - 1 - k) :: IO Word8
          pokeByteOff d k b
          go (k + 1)


-- ============================================================
-- Leaf arrays
-- ============================================================

-- | A fixed-width array: validity at @bi@, values at @bi + 1@.
prim :: Ctx -> PrimType a -> FieldNode -> Int -> Int -> Either String ColumnArray
prim ctx t fn len bi = withPrim t $ do
  v <- validityAt ctx (I.primTypeName t) len fn bi
  xs <- storableAt ctx (I.primTypeName t) (swapPattern t) len (bi + 1)
  Right (I.ColPrim t v xs)


varLen
  :: forall o
   . Offset o
  => Ctx
  -> String
  -> Bool
  -> (Maybe I.Validity -> VS.Vector o -> ByteString -> ColumnArray)
  -> FieldNode
  -> Int
  -> Cur
  -> Either String Out
varLen ctx what utf8 con fn len cur = do
  let !bi = curBuf cur
  v <- validityAt ctx what len fn bi
  offs <- offsetsAt @o ctx what len (bi + 1)
  dat <- bufferAt ctx (bi + 2)
  I.validateOffsets what (BS.length dat) offs
  if utf8 then I.validateUtf8 what offs dat else Right ()
  Right (Out (con v offs dat) (adv 1 3 cur))
{-# INLINE varLen #-}


fixedSizeBinary :: Ctx -> Int -> FieldNode -> Int -> Cur -> Either String Out
fixedSizeBinary ctx w fn len cur
  | w < 0 = Left ("Arrow.Read.Columns: negative fixed-size binary width " ++ show w)
  | w > 0 && len > maxArrayLength `quot` w =
      Left ("Arrow.Read.Columns: fixed-size binary of width " ++ show w ++ " cannot have " ++ show len ++ " rows")
  | otherwise = do
      let !bi = curBuf cur
      v <- validityAt ctx "fixed-size binary" len fn bi
      dat <- bufferAt ctx (bi + 1)
      if BS.length dat < w * len
        then Left "Arrow.Read.Columns: fixed-size binary data buffer too small"
        else Right (Out (I.ColFixedSizeBinary w len v (BSU.unsafeTake (w * len) dat)) (adv 1 2 cur))


{- | Utf8View / BinaryView: validity, the 16-byte views, then this
column's variadic data buffers (their count is the next
'rbVariadicBufferCounts' entry, in preorder over view columns).
-}
views :: Ctx -> Bool -> FieldNode -> Int -> Cur -> Either String Out
views ctx utf8 fn len cur = do
  let !rb = ctxRb ctx
      !bi = curBuf cur
      !what = if utf8 then "utf8 view" else "binary view"
  nVar <- case rbVariadicBufferCounts rb V.!? curVar cur of
    Nothing -> Left "Arrow.Read.Columns: view column has no variadicBufferCounts entry"
    Just c
      | c < 0 || c > fromIntegral (VS.length (rbBuffers rb)) ->
          Left ("Arrow.Read.Columns: invalid variadic buffer count " ++ show c)
      | otherwise -> Right (fromIntegral c :: Int)
  v <- validityAt ctx what len fn bi
  vs <- bufferAt ctx (bi + 1)
  if BS.length vs < len * 16
    then Left ("Arrow.Read.Columns: " ++ what ++ " buffer too small")
    else Right ()
  dataBufs <- V.generateM nVar (\k -> bufferAt ctx (bi + 2 + k))
  let !viewBytes = BSU.unsafeTake (len * 16) vs
  I.validateViews what utf8 len v viewBytes dataBufs
  let !col = if utf8 then I.ColUtf8View v viewBytes dataBufs else I.ColBinaryView v viewBytes dataBufs
      Cur n b x u = cur
  Right (Out col (Cur (n + 1) (b + 2 + nVar) (x + 1) u))


-- ============================================================
-- Nested arrays
-- ============================================================

singleChild :: String -> Field -> Either String Field
singleChild what f
  | V.length (fieldChildren f) == 1 = Right (V.unsafeHead (fieldChildren f))
  | otherwise =
      Left ("Arrow.Read.Columns: " ++ what ++ " field must have exactly one child, has " ++ show (V.length (fieldChildren f)))


-- | Every child of a struct-like parent must cover the parent's rows.
checkChildLengths :: String -> Int -> V.Vector ColumnArray -> Either String ()
checkChildLengths what len cols = case V.findIndex (\c -> columnLength c < len) cols of
  Nothing -> Right ()
  Just k ->
    Left
      ( "Arrow.Read.Columns: "
          ++ what
          ++ " child #"
          ++ show k
          ++ " has "
          ++ show (columnLength (V.unsafeIndex cols k))
          ++ " rows, parent has "
          ++ show len
      )


struct :: Ctx -> Field -> FieldNode -> Int -> Cur -> Either String Out
struct ctx f fn len cur = do
  v <- validityAt ctx "struct" len fn (curBuf cur)
  Outs cols cur' <- fieldsN ctx (fieldChildren f) (adv 1 1 cur)
  checkChildLengths "struct" len cols
  let named = V.zipWith (\c col -> (fieldName c, col)) (fieldChildren f) cols :: V.Vector (Text, ColumnArray)
  Right (Out (I.ColStruct len v named) cur')


list
  :: forall o
   . Offset o
  => Ctx
  -> String
  -> (Maybe I.Validity -> VS.Vector o -> ColumnArray -> ColumnArray)
  -> Field
  -> FieldNode
  -> Int
  -> Cur
  -> Either String Out
list ctx what con f fn len cur = do
  let !bi = curBuf cur
  childField <- singleChild what f
  v <- validityAt ctx what len fn bi
  offs <- offsetsAt @o ctx what len (bi + 1)
  Out child cur' <- field ctx childField (adv 1 2 cur)
  I.validateOffsets what (columnLength child) offs
  Right (Out (con v offs child) cur')


listView
  :: forall o
   . Offset o
  => Ctx
  -> String
  -> (Maybe I.Validity -> VS.Vector o -> VS.Vector o -> ColumnArray -> ColumnArray)
  -> Field
  -> FieldNode
  -> Int
  -> Cur
  -> Either String Out
listView ctx what con f fn len cur = do
  let !bi = curBuf cur
      !w = sizeOf (undefined :: o)
  childField <- singleChild what f
  v <- validityAt ctx what len fn bi
  offs <- storableAt @o ctx (what ++ " offsets") [w] len (bi + 1)
  sizes <- storableAt @o ctx (what ++ " sizes") [w] len (bi + 2)
  Out child cur' <- field ctx childField (adv 1 3 cur)
  I.validateListView what v offs sizes (columnLength child)
  Right (Out (con v offs sizes child) cur')


fixedSizeList :: Ctx -> Int -> Field -> FieldNode -> Int -> Cur -> Either String Out
fixedSizeList ctx size f fn len cur
  | size < 0 || (size > 0 && len > maxArrayLength `quot` size) =
      Left ("Arrow.Read.Columns: fixed-size list of size " ++ show size ++ " cannot have " ++ show len ++ " rows")
  | otherwise = do
      childField <- singleChild "fixed-size list" f
      v <- validityAt ctx "fixed-size list" len fn (curBuf cur)
      Out child cur' <- field ctx childField (adv 1 1 cur)
      if columnLength child < len * size
        then
          Left
            ( "Arrow.Read.Columns: fixed-size list child has "
                ++ show (columnLength child)
                ++ " rows, needs "
                ++ show (len * size)
            )
        else Right (Out (I.ColFixedSizeList size len v child) cur')


{- | Map: validity, offsets, then the entries struct (key, value),
which may hold no nulls of its own (arrow-rs @MapArray@).
-}
mapCol :: Ctx -> Field -> FieldNode -> Int -> Cur -> Either String Out
mapCol ctx f fn len cur = do
  let !bi = curBuf cur
  entries <- singleChild "map" f
  v <- validityAt ctx "map" len fn bi
  offs <- offsetsAt @Int32 ctx "map" len (bi + 1)
  Out ent cur' <- field ctx entries (adv 1 2 cur)
  case ent of
    I.ColStruct n Nothing kv
      | V.length kv == 2 -> do
          I.validateOffsets "map" n offs
          Right (Out (I.ColMap v offs (snd (V.unsafeIndex kv 0)) (snd (V.unsafeIndex kv 1))) cur')
    _ -> Left "Arrow.Read.Columns: map entries must be a struct of exactly (key, value) without nulls"


{- | Unions carry no validity. Wire type ids become child positions
through the declared @typeIds@ (aliased as is when they are
positional, the common case; otherwise one table-lookup pass).
-}
union :: Ctx -> UnionMode -> V.Vector Int32 -> Field -> Int -> Cur -> Either String Out
union ctx mode declared f len cur = do
  let !bi = curBuf cur
      !k = V.length (fieldChildren f)
      !what = "union"
  table <- unionChildTable k declared
  wire <- storableAt @Int8 ctx "union type ids" [] len bi
  let !types = case table of
        Nothing -> wire
        Just tbl -> VS.map (\t -> if t < 0 then -1 else VS.unsafeIndex tbl (fromIntegral t)) wire
  case mode of
    Dense -> do
      offs <- storableAt @Int32 ctx "dense union offsets" [4] len (bi + 1)
      Outs children cur' <- fieldsN ctx (fieldChildren f) (adv 1 2 cur)
      let !lens = VS.generate k (\j -> fromIntegral (columnLength (V.unsafeIndex children j))) :: VS.Vector Int64
      I.validateDenseUnion what types offs lens
      Right (Out (I.ColDenseUnion types offs children) cur')
    Sparse -> do
      Outs children cur' <- fieldsN ctx (fieldChildren f) (adv 1 1 cur)
      checkChildLengths "sparse union" len children
      I.validateSparseUnionTypes what types k
      Right (Out (I.ColSparseUnion types children) cur')


{- | Lookup table from wire type id (0..127) to child position (@-1@ when
undeclared), or 'Nothing' when the ids are positional (child @k@ has
id @k@), so the wire buffer already holds child positions.
-}
unionChildTable :: Int -> V.Vector Int32 -> Either String (Maybe (VS.Vector Int8))
unionChildTable nChildren declared
  | nChildren > 128 = Left ("Arrow.Read.Columns: union has " ++ show nChildren ++ " children, at most 128 allowed")
  | V.null declared = Right Nothing
  | V.length declared /= nChildren =
      Left
        ( "Arrow.Read.Columns: union declares "
            ++ show (V.length declared)
            ++ " type ids for "
            ++ show nChildren
            ++ " children"
        )
  | V.any (\t -> t < 0 || t > 127) declared = Left "Arrow.Read.Columns: union type ids must be in 0..127"
  | V.and (V.imap (\j t -> fromIntegral t == j) declared) = Right Nothing
  | otherwise =
      let table = VS.generate 128 (\t -> maybe (-1) fromIntegral (V.elemIndex (fromIntegral t) declared)) :: VS.Vector Int8
      in if VS.length (VS.filter (>= 0) table) /= nChildren
           then Left "Arrow.Read.Columns: union declares a type id twice"
           else Right (Just table)


{- | Run-end encoded: no buffers of its own, children run ends (non-null
int16/32/64, positive, strictly increasing, last >= length) and values
(at least one per run).
-}
runEndEncoded :: Ctx -> Field -> Int -> Cur -> Either String Out
runEndEncoded ctx f len cur
  | V.length (fieldChildren f) /= 2 =
      Left "Arrow.Read.Columns: RunEndEncoded must have exactly two children (run_ends, values)"
  | otherwise = do
      Out re cur1 <- field ctx (V.unsafeIndex (fieldChildren f) 0) (adv 1 0 cur)
      Out vals cur2 <- field ctx (V.unsafeIndex (fieldChildren f) 1) cur1
      I.validateRunEnds "run-end encoded" re len
      let !runs = columnLength re
      if columnLength vals < runs
        then
          Left
            ( "Arrow.Read.Columns: run-end encoded array has "
                ++ show runs
                ++ " runs but "
                ++ show (columnLength vals)
                ++ " values"
            )
        else Right (Out (I.ColRunEndEncoded 0 len re vals) cur2)


{- | The wire payload of a dictionary-encoded field: the keys (an
integer array at the index type's width, carrying the row validity)
and an empty placeholder for the values.
-}
dictKeys :: Ctx -> Field -> DictionaryEncoding -> FieldNode -> Int -> Cur -> Either String Out
dictKeys ctx f de fn len cur = case primTypeFor (deIndexType de) of
  Just (SomePrimType t)
    | Just IntegralPrim <- integralPrim t -> do
        keys <- prim ctx t fn len (curBuf cur)
        placeholder <- placeholderColumn f
        Right (Out (I.ColDictionary (deId de) keys placeholder) (adv 1 2 cur))
  _ -> Left ("Arrow.Read.Columns: dictionary index type must be an 8/16/32/64-bit integer, got " ++ show (deIndexType de))


-- ============================================================
-- Body compression
-- ============================================================

{- | Decompress every buffer of a compressed body (one envelope per
buffer descriptor). Each result is a fresh buffer.
-}
decompressBuffers :: BodyCompressionCodec -> VS.Vector Buffer -> ByteString -> Either String (V.Vector ByteString)
decompressBuffers codec bufs body =
  V.generateM (VS.length bufs) $ \i -> do
    let Buffer o l = VS.unsafeIndex bufs i
    env <- sliceWire "compressed buffer" o l body
    decompressBufferEnvelope codec env


{- | Decode a body written with body compression into an uncompressed
body plus buffer descriptors that point into it (each buffer padded to
8 bytes). 'decodeRecordBatch' does this itself, per buffer and without
the concatenation; this is for callers that need the uncompressed
message body.
-}
decompressBody :: BodyCompressionCodec -> VS.Vector Buffer -> ByteString -> Either String (VS.Vector Buffer, ByteString)
decompressBody codec bufs body = do
  payloads <- decompressBuffers codec bufs body
  let padded b = (BS.length b + 7) `quot` 8 * 8
      offsets = V.prescanl' (+) 0 (V.map padded payloads)
      !total = V.sum (V.map padded payloads)
      newBufs = VS.generate (V.length payloads) $ \i ->
        Buffer (fromIntegral (V.unsafeIndex offsets i)) (fromIntegral (BS.length (V.unsafeIndex payloads i)))
      out = I.createAligned total $ \dst -> do
        let fill !i
              | i >= V.length payloads = pure ()
              | otherwise = do
                  let !p = V.unsafeIndex payloads i
                      !off = V.unsafeIndex offsets i
                      !pl = padded p
                  I.withBytesPtr p $ \src -> copyBytes (dst `plusPtr` off) src (BS.length p)
                  fillBytes (dst `plusPtr` (off + BS.length p)) 0 (pl - BS.length p)
                  fill (i + 1)
        fill 0
  Right (newBufs, out)


-- | Slice @len@ bytes at @off@ out of @bs@ (both wire values).
sliceWire :: String -> Int64 -> Int64 -> ByteString -> Either String ByteString
sliceWire what off len bs
  | off < 0 || len < 0 = Left ("Arrow.Read.Columns: " ++ what ++ ": negative offset or length")
  | off > n || len > n - off =
      Left
        ( "Arrow.Read.Columns: "
            ++ what
            ++ " ["
            ++ show off
            ++ ", +"
            ++ show len
            ++ ") runs past the "
            ++ show n
            ++ "-byte input"
        )
  | otherwise = Right $! BSU.unsafeTake (fromIntegral len) (BSU.unsafeDrop (fromIntegral off) bs)
  where
    n = fromIntegral (BS.length bs) :: Int64


{- | Decompress one buffer envelope per the @BUFFER@ method.

The 8-byte uncompressed length is a wire claim. It must be @-1@
(stored uncompressed) or a size the payload could actually expand to
(see 'maxCompressionRatio'), and the decompressed bytes must match it
exactly; a lying header can never size an allocation.
-}
decompressBufferEnvelope :: BodyCompressionCodec -> ByteString -> Either String ByteString
decompressBufferEnvelope codec env
  | BS.null env = Right env
  | BS.length env < 8 = Left "Arrow.Read.Columns: buffer envelope shorter than 8 bytes"
  | rawLen == -1 = Right payload
  | rawLen < 0 = Left ("Arrow.Read.Columns: negative uncompressed buffer length " ++ show rawLen)
  | rawLen > fromIntegral (BS.length payload) * maxCompressionRatio =
      Left
        ( "Arrow.Read.Columns: buffer claims "
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
            ( "Arrow.Read.Columns: buffer decompressed to "
                ++ show (BS.length out)
                ++ " bytes, header says "
                ++ show rawLen
            )
        else Right out
  where
    -- Lazy on purpose: only forced once the guards above have checked
    -- that the envelope holds the 8-byte length.
    rawLen = leInt64 env
    payload = BSU.unsafeDrop 8 env


{- | No LZ4 frame or ZSTD frame expands by more than this factor (a
ZSTD RLE block turns 4 bytes into at most 128 KiB; LZ4 tops out near
255x), so a larger claimed size is a lie, not data.
-}
maxCompressionRatio :: Int64
maxCompressionRatio = 32768


-- | Little-endian 'Int64' in the first 8 bytes (the caller checked the length).
leInt64 :: ByteString -> Int64
leInt64 bs = go 7 0
  where
    go :: Int -> Int64 -> Int64
    go !k !acc
      | k < 0 = acc
      | otherwise = go (k - 1) ((acc `shiftL` 8) .|. fromIntegral (BSU.unsafeIndex bs k))


{- | Decompress a payload that must expand to exactly @rawLen@ bytes
('decompressBufferEnvelope' has already bounded @rawLen@). Neither
backend may allocate more than @rawLen + 1@ output bytes.
-}
decompressBuffer :: BodyCompressionCodec -> Int -> ByteString -> Either String ByteString
#if defined(HAVE_ZSTD) || defined(HAVE_LZ4)
decompressBuffer codec rawLen comp = case codec of
#else
decompressBuffer codec _ _ = case codec of
#endif
#ifdef HAVE_ZSTD
  BodyZstd ->
    -- Zstd.decompress sizes its output from the frame header, so the
    -- header must agree with the (bounded) envelope length first.
    case Zstd.decompressedSize comp of
      Just n | n == rawLen ->
        case Zstd.decompress comp of
          Zstd.Decompress out -> Right out
          Zstd.Skip -> Left "Arrow.Read.Columns: ZSTD decompress: skipped frame"
          Zstd.Error msg -> Left ("Arrow.Read.Columns: ZSTD decompress: " ++ msg)
      other ->
        Left
          ( "Arrow.Read.Columns: ZSTD frame content size "
              ++ show other
              ++ " does not match the buffer's uncompressed length "
              ++ show rawLen
          )
#else
  BodyZstd -> Left "Arrow.Read.Columns: ZSTD body compression requires building wireform-arrow with -fzstd"
#endif
#ifdef HAVE_LZ4
  LZ4Frame ->
    -- lz4-hs's decompress is lazy and throws on malformed input; take at
    -- most one byte past the expected size (so an overlong frame is
    -- detected without being fully inflated) and catch the exception to
    -- keep the Either-shaped contract.
    case unsafeDupablePerformIO $
      Exc.try @Exc.SomeException
        (Exc.evaluate (BL.toStrict (BL.take (fromIntegral rawLen + 1) (Lz4.decompress (BL.fromStrict comp))))) of
      Right out -> Right out
      Left e -> Left ("Arrow.Read.Columns: LZ4_FRAME decompress: " ++ show e)
#else
  LZ4Frame -> Left "Arrow.Read.Columns: LZ4 body compression requires building wireform-arrow with -flz4"
#endif
