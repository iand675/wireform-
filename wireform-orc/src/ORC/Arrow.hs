{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Arrow ↔ ORC column-data bridge.

Lets callers keep a single in-memory representation
('Arrow.Column.ColumnArray') and serialise it as ORC instead.
Mirrors "Parquet.Arrow" for the same flat-primitive subset
ORC's writer natively encodes today.

@
-- Arrow → ORC
let !(types, stripes) = 'arrowToORC' arrowSchema arrowBatches
    bytes             = 'ORC.encodeORC'
                           'ORC.defaultWriteOptions'
                           types stripes

-- ORC → Arrow (one stripe at a time)
footer <- 'ORC.decodeORC' bytes
batch  <- 'orcStripeToArrow' arrowSchema bytes footer 0
@

Coverage today: flat-primitive Arrow columns only — 'AInt'
(8/16/32/64), 'ABool', 'AFloatingPoint' (Single + Double),
'AUtf8', 'ABinary', and their nullable variants. Nested types
(struct / list / map / union / dictionary / view / REE)
aren't yet routed through ORC's nested writer; they fall
through to a clean 'Left' at translation time.
-}
module ORC.Arrow (
  -- * Arrow → ORC
  arrowToORC,
  columnArrayToORCStreams,

  -- * ORC → Arrow
  orcStripeToArrow,
  orcStripeToArrowProjected,

  -- * Streaming reader (one stripe at a time)
  streamStripes,
  streamStripesIter,
  streamStripesProjectedIter,
  streamStripesFilteredIter,
  streamStripesProjectedFilteredIter,
  numStripes,
) where

import Arrow.Column qualified as AC
import Arrow.Column.Internal (bytesToStorable)
import Arrow.Types qualified as AT
import Columnar.Stream qualified as IS
import Control.Monad.ST (runST)
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int32, Int64)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Mutable qualified as VM
import Data.Vector.Primitive qualified as VP
import Data.Vector.Primitive.Mutable qualified as VPM
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.Mutable qualified as VSM
import Data.Word (Word32, Word64)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Storable (Storable, sizeOf)
import ORC.Read qualified as OR
import ORC.Statistics qualified as OStats
import ORC.Stripe qualified as OSt
import ORC.Types qualified as OT
import ORC.Write qualified as OW


-- ============================================================
-- Stream-kind constants (per ORC's Stream.proto)
-- ============================================================

-- | @Kind = PRESENT@.
streamPresent :: Word64
streamPresent = 0


-- | @Kind = DATA@.
streamData :: Word64
streamData = 1


-- | @Kind = LENGTH@.
streamLength :: Word64
streamLength = 2


{- | @Kind = SECONDARY@ (id 5) — used for ORC timestamp
nanoseconds and decimal scale. The previous value here
was 7, which is actually @BLOOM_FILTER@; that mistake
silently mis-tagged every SECONDARY stream we wrote so
pyarrow's reader couldn't find them.
-}
streamSecondary :: Word64
streamSecondary = 5


{- | The ORC timestamp epoch is 2015-01-01 00:00:00 UTC, /not/
the Unix epoch. Per spec
(https://orc.apache.org/specification/ORCv1/#timestamp-data)
the DATA stream stores seconds relative to ORC's epoch; we
shift between the two when round-tripping with anything that
speaks Unix time. The constant below is
@1_420_070_400 = (2015 - 1970) * 365.25 * 86400@ rounded down
to the second (the exact value Java / C++ / Rust ORC use).
-}
orcEpochSecondsFromUnix :: Int64
orcEpochSecondsFromUnix = 1_420_070_400


{- | Convert an 'OR.ORCTimestamp' (seconds-since-ORC-epoch +
decoded-nanos) back to whole nanoseconds since the Unix epoch.
-}
timestampToUnixNanos :: OR.ORCTimestamp -> Int64
timestampToUnixNanos (OR.ORCTimestamp s n) =
  (s + orcEpochSecondsFromUnix) * 1_000_000_000 + n


-- ============================================================
-- Arrow → ORC
-- ============================================================

{- | Lower an Arrow schema + a sequence of column-major batches
to the inputs 'ORC.encodeORC' expects.

Each Arrow batch becomes one ORC stripe. The output pairs
each stripe's stream tuples with its row count (derived from
the first top-level column's length) so
'ORC.encodeORC' can stamp @siNumberOfRows@ directly
into the stripe information.

Supports both flat and nested types:

  * flat primitives + nullable variants (see 'columnArrayToORCStreams')
  * temporal (Date, Time, Timestamp, Duration) + Decimal
  * @ColStruct@ → ORC 'TKStruct' with child column ids
  * @ColList@ / @ColLargeList@ → ORC 'TKList' with a LENGTH
    stream on the parent (per-row child counts)
  * @ColMap@ → ORC 'TKMap' with a LENGTH stream on the parent +
    key / value child columns
-}
arrowToORC
  :: AT.Schema
  -> [V.Vector AC.ColumnArray]
  -> Either
       String
       ( V.Vector OT.ORCType
       , [(V.Vector (Word64, Word64, ByteString), Word64)]
       )
arrowToORC sch batches = do
  -- Step 1: walk the schema depth-first, assigning an ORC
  -- column id per node (root=0, top-level leaves 1..N, then
  -- their descendants). Returns the flat ORCType vector.
  let !topFields = AT.arrowFields sch
  (!types, !topIds) <- buildSchemaTree topFields
  -- Step 2: per stripe, emit streams for each top-level column
  -- using the same id layout.
  stripes <-
    mapM
      ( \cols -> do
          streams <- encodeStripe topIds cols
          let !rowCount =
                if V.null cols
                  then 0
                  else fromIntegral (AC.columnLength (V.head cols))
          Right (streams, rowCount)
      )
      batches
  Right (types, stripes)


{- | Build ORC's flat 'ORCType' vector for an Arrow schema.
Returns the type vector plus a parallel list of the /top-level/
column ids (the ids assigned to each direct child of the root
struct). Encoders use the top-level ids to emit streams in
declaration order.
-}
buildSchemaTree
  :: V.Vector AT.Field
  -> Either String (V.Vector OT.ORCType, V.Vector Word32)
buildSchemaTree topFields = do
  -- Reserve column 0 for the synthetic root struct.
  let !startCid = 1 :: Word32
  (childTypes, !topIds, !_nextCid) <-
    foldM
      ( \(!acc, !ids, !cid) fld -> do
          (!subTypes, !next) <- assignIds fld cid
          Right
            ( acc V.++ subTypes
            , V.snoc ids cid
            , next
            )
      )
      (V.empty, V.empty, startCid)
      (V.toList topFields)
  let !rootType =
        OT.ORCType
          { OT.otKind = OT.TKStruct
          , OT.otSubtypes = topIds
          , OT.otFieldNames = V.map AT.fieldName topFields
          }
  Right (V.cons rootType childTypes, topIds)
  where
    foldM f z0 = go z0
      where
        go z [] = Right z
        go z (x : xs) = f z x >>= \z' -> go z' xs


{- | Assign a depth-first column id to a field + every
descendant. Returns the types laid out in id order + the
next free column id.
-}
assignIds
  :: AT.Field
  -> Word32
  -- ^ cid allocated to this field
  -> Either String (V.Vector OT.ORCType, Word32)
assignIds fld cid = case AT.fieldType fld of
  AT.AStruct -> do
    -- Struct's subtypes are its children, laid out immediately
    -- after the struct itself.
    let !children = AT.fieldChildren fld
    (childTypes, !topIds, !nextCid) <-
      layoutChildren children (cid + 1)
    let !selfType =
          OT.ORCType
            { OT.otKind = OT.TKStruct
            , OT.otSubtypes = topIds
            , OT.otFieldNames = V.map AT.fieldName children
            }
    Right (V.cons selfType childTypes, nextCid)
  AT.AList -> listLike (cid + 1)
  AT.ALargeList -> listLike (cid + 1)
  AT.AMap _sorted ->
    case V.toList (AT.fieldChildren fld) of
      -- Arrow maps use an intermediate entries struct whose
      -- children are (key, value); ORC's TKMap wants the key
      -- and value subtypes directly. Descend through the
      -- entries struct when present; otherwise treat the
      -- children as the (key, value) pair directly.
      [entry]
        | AT.fieldType entry == AT.AStruct
        , V.length (AT.fieldChildren entry) == 2 ->
            let !kf = V.unsafeIndex (AT.fieldChildren entry) 0
                !vf = V.unsafeIndex (AT.fieldChildren entry) 1
            in mapLike kf vf (cid + 1)
      [kf, vf] -> mapLike kf vf (cid + 1)
      _ -> Left "ORC.Arrow: AMap must have key + value children"
  _ -> do
    -- Leaf field: no subtypes, just the ORC kind.
    kind <- arrowLeafKindFor (AT.fieldType fld)
    let !selfType =
          OT.ORCType
            { OT.otKind = kind
            , OT.otSubtypes = V.empty
            , OT.otFieldNames = V.empty
            }
    Right (V.singleton selfType, cid + 1)
  where
    -- List / LargeList: single child immediately after the list.
    listLike childCid =
      case V.toList (AT.fieldChildren fld) of
        [child] -> do
          (childTypes, !nextCid) <- assignIds child childCid
          let !selfType =
                OT.ORCType
                  { OT.otKind = OT.TKList
                  , OT.otSubtypes = V.singleton childCid
                  , OT.otFieldNames = V.empty
                  }
          Right (V.cons selfType childTypes, nextCid)
        _ -> Left "ORC.Arrow: AList / ALargeList must have exactly one child"

    mapLike kf vf kCid = do
      (kTypes, !vCid) <- assignIds kf kCid
      (vTypes, !nextCid) <- assignIds vf vCid
      let !selfType =
            OT.ORCType
              { OT.otKind = OT.TKMap
              , OT.otSubtypes = V.fromList [kCid, vCid]
              , OT.otFieldNames = V.empty
              }
      Right (V.cons selfType (kTypes V.++ vTypes), nextCid)

    -- Shared helper for struct field lists.
    layoutChildren
      :: V.Vector AT.Field
      -> Word32
      -> Either String (V.Vector OT.ORCType, V.Vector Word32, Word32)
    layoutChildren children startCid =
      let go !_ !acc !ids !n [] = Right (acc, ids, n)
          go cur !acc !ids !_n (c : rest) = do
            (ts, next) <- assignIds c cur
            go next (acc V.++ ts) (V.snoc ids cur) next rest
      in go startCid V.empty V.empty startCid (V.toList children)


{- | Leaf-kind mapping used by 'assignIds'. Nested kinds go
through their own branches above; this table covers the
primitives + temporals only.
-}
arrowLeafKindFor :: AT.ArrowType -> Either String OT.TypeKind
arrowLeafKindFor ty = case ty of
  AT.AInt 8 _ -> Right OT.TKByte
  AT.AInt 16 _ -> Right OT.TKShort
  AT.AInt 32 _ -> Right OT.TKInt
  AT.AInt 64 _ -> Right OT.TKLong
  AT.ABool -> Right OT.TKBoolean
  AT.AFloatingPoint AT.Single -> Right OT.TKFloat
  AT.AFloatingPoint AT.DoublePrecision -> Right OT.TKDouble
  AT.AUtf8 -> Right OT.TKString
  AT.ABinary -> Right OT.TKBinary
  AT.ALargeUtf8 -> Right OT.TKString
  AT.ALargeBinary -> Right OT.TKBinary
  AT.ADate _ -> Right OT.TKDate
  -- Arrow Timestamp(_, Just tz) maps to ORC's TIMESTAMP_INSTANT
  -- (UTC-anchored); without tz it maps to local-time TIMESTAMP.
  AT.ATimestamp _ (Just _) -> Right OT.TKTimestampInstant
  AT.ATimestamp _ Nothing -> Right OT.TKTimestamp
  AT.ADuration _ -> Right OT.TKLong
  AT.ATime _ _ -> Right OT.TKLong
  AT.ADecimal _ _ -> Right OT.TKDecimal
  other ->
    Left $
      "ORC.Arrow: Arrow type "
        ++ show other
        ++ " has no flat ORC equivalent"


{- | Build one stripe by walking the top-level columns with their
allocated ORC column ids. Lists + structs + maps recurse into
'columnArrayToORCStreamsNested' which tracks its own id cursor.
-}
encodeStripe
  :: V.Vector Word32
  -> V.Vector AC.ColumnArray
  -> Either String (V.Vector (Word64, Word64, ByteString))
encodeStripe topIds cols
  | V.length topIds /= V.length cols =
      Left $
        "ORC.Arrow.arrowToORC: schema has "
          ++ show (V.length topIds)
          ++ " top-level fields but batch has "
          ++ show (V.length cols)
          ++ " columns"
  | otherwise = do
      streamLists <-
        V.zipWithM
          (\cid col -> columnArrayToORCStreamsNested (fromIntegral cid) col)
          topIds
          cols
      Right (V.concat (V.toList streamLists))


{- | Dispatch between nested + flat stream-encoding. Nested
shapes know how to recurse; everything else falls through to
'columnArrayToORCStreams'.
-}
columnArrayToORCStreamsNested
  :: Word64
  -> AC.ColumnArray
  -> Either String (V.Vector (Word64, Word64, ByteString))
columnArrayToORCStreamsNested cid col = case col of
  AC.ColStruct n _ namedChildren -> do
    -- Structs in ORC have no streams of their own when the
    -- struct is non-nullable; the children carry the data. A
    -- proper nullable-struct implementation would emit a
    -- PRESENT stream at @cid@ (deferred until a concrete
    -- generator stresses it). Children may hold more rows than
    -- the struct, so each is cut to the struct's rows.
    let !children = V.map (AC.sliceColumnArray 0 n . snd) namedChildren
        !childCids = V.prescanl' (+) (cid + 1) (V.map columnArraySpan children)
    childStreams <- V.zipWithM columnArrayToORCStreamsNested childCids children
    Right (V.concat (V.toList childStreams))
  AC.ColList _ offs child -> encodeList (offsetLengths offs) (childWindow offs child)
  AC.ColLargeList _ offs child -> encodeList (offsetLengths offs) (childWindow offs child)
  AC.ColMap _ offs keys values -> do
    -- LENGTH on the parent, then the key and value children.
    let !keyCid = cid + 1
        !keys' = childWindow offs keys
    keyStreams <- columnArrayToORCStreamsNested keyCid keys'
    valStreams <-
      columnArrayToORCStreamsNested
        (keyCid + columnArraySpan keys')
        (childWindow offs values)
    Right (lengthStream (offsetLengths offs) <> keyStreams <> valStreams)
  _ -> columnArrayToORCStreams cid col
  where
    -- Encode an Arrow list column into ORC streams: LENGTH on
    -- the parent (per-row child counts) + recursive child
    -- streams.
    encodeList lengths child = do
      childStreams <- columnArrayToORCStreamsNested (cid + 1) child
      Right (lengthStream lengths <> childStreams)

    lengthStream lengths =
      V.singleton (streamLength, cid, OW.encodeIntColumn lengths False)


{- | Per-row child counts of an offsets buffer (rows + 1 entries):
@[o1 - o0, o2 - o1, ..]@.
-}
offsetLengths :: (Storable o, Integral o) => VS.Vector o -> VP.Vector Int64
offsetLengths offs =
  VP.generate
    (max 0 (VS.length offs - 1))
    (\i -> fromIntegral (VS.unsafeIndex offs (i + 1) - VS.unsafeIndex offs i))
{-# INLINE offsetLengths #-}


{- | The child rows an offsets buffer references. Arrow offsets
need not start at zero (slices keep the parent's offsets), while
ORC's child stream holds exactly the referenced rows.
-}
childWindow :: (Storable o, Integral o) => VS.Vector o -> AC.ColumnArray -> AC.ColumnArray
childWindow offs child
  | VS.null offs = AC.sliceColumnArray 0 0 child
  | otherwise =
      let !start = fromIntegral (VS.head offs)
      in AC.sliceColumnArray start (fromIntegral (VS.last offs) - start) child
{-# INLINE childWindow #-}


{- | How many ORC column-id slots does this 'ColumnArray'
occupy? Mirrors 'assignIds' on the field side; used to
advance the cid cursor when encoding struct children.
-}
columnArraySpan :: AC.ColumnArray -> Word64
columnArraySpan col = case col of
  AC.ColStruct _ _ kids -> 1 + V.foldl' (\acc (_, k) -> acc + columnArraySpan k) 0 kids
  AC.ColList _ _ inner -> 1 + columnArraySpan inner
  AC.ColLargeList _ _ inner -> 1 + columnArraySpan inner
  AC.ColMap _ _ keys values -> 1 + columnArraySpan keys + columnArraySpan values
  _ -> 1


{- | Encode one Arrow column at the given ORC column id into its
ORC stream tuples. Returns @[(streamKind, columnId, payload)]@
in emission order. A column with nulls emits a PRESENT stream
first, then present-only values, so round-trips recover the nulls
at the right positions.
-}
columnArrayToORCStreams
  :: Word64
  -> AC.ColumnArray
  -> Either String (V.Vector (Word64, Word64, ByteString))
columnArrayToORCStreams !cid col = case col of
  AC.ColInt8 mv xs -> Right (intStreams (presentPrim fromIntegral mv xs))
  AC.ColInt16 mv xs -> Right (intStreams (presentPrim fromIntegral mv xs))
  AC.ColInt32 mv xs -> Right (intStreams (presentPrim fromIntegral mv xs))
  AC.ColInt64 mv xs -> Right (intStreams (presentPrim id mv xs))
  AC.ColUInt8 mv xs -> Right (intStreams (presentPrim fromIntegral mv xs))
  AC.ColUInt16 mv xs -> Right (intStreams (presentPrim fromIntegral mv xs))
  AC.ColUInt32 mv xs -> Right (intStreams (presentPrim fromIntegral mv xs))
  AC.ColUInt64 mv xs -> Right (intStreams (presentPrim fromIntegral mv xs))
  AC.ColFloat mv xs ->
    Right (withPresent (V.singleton (streamData, cid, OW.encodeFloatColumn (presentPrim id mv xs))))
  AC.ColDouble mv xs ->
    Right (withPresent (V.singleton (streamData, cid, OW.encodeDoubleColumn (presentPrim id mv xs))))
  -- Temporal types: map to an integer stream at the natural
  -- width. Date = days-since-epoch, Time/Duration use the
  -- Int32/Int64 payload as-is.
  AC.ColDate32 mv xs -> Right (intStreams (presentPrim fromIntegral mv xs))
  AC.ColDate64 mv xs -> Right (intStreams (presentPrim id mv xs))
  AC.ColTime32 mv xs -> Right (intStreams (presentPrim fromIntegral mv xs))
  AC.ColTime64 mv xs -> Right (intStreams (presentPrim id mv xs))
  AC.ColDuration mv xs -> Right (intStreams (presentPrim id mv xs))
  AC.ColTimestamp mv xs -> Right (timestampStreams (presentPrim id mv xs))
  _
    | Just ba <- AC.asBool col ->
        let !present = presentBoxed (AC.columnLength col) (AC.nullCount col) (AC.boolArrayAt ba)
        in Right (withPresent (V.singleton (streamData, cid, OW.encodeBooleanRLE present)))
    | Just ba <- AC.asBinary col -> Right (stringStreams ba)
    | Just ba <- AC.asLargeBinary col -> Right (stringStreams ba)
    | otherwise ->
        Left $
          "ORC.Arrow: column shape "
            ++ AC.columnTag col
            ++ " not supported by the bridge yet "
            ++ "(nested types, dictionary, view, REE)"
  where
    !mValidity = AC.validity col

    withPresent streams = case mValidity of
      Nothing -> streams
      Just _ ->
        let !bits = V.generate (AC.columnLength col) (AC.isValidAt mValidity)
        in V.cons (streamPresent, cid, OW.encodeBooleanRLE bits) streams

    -- ORC's RLE-v2 integer encoders take (Int64 vector, signed?).
    intStreams xs = withPresent (V.singleton (streamData, cid, OW.encodeIntColumn xs True))

    -- ORC timestamps need both a DATA stream (signed seconds
    -- with the SPEC-defined epoch of 2015-01-01 GMT, NOT
    -- 1970-01-01, the famous ORC epoch gotcha) and a
    -- SECONDARY stream (nanoseconds with the 3-bit
    -- trailing-zero encoding ORC defines). The Arrow
    -- 'ColTimestamp' payload is whole nanoseconds since
    -- 1970-01-01; convert to ORC's epoch by subtracting
    -- 'orcEpochSecondsFromUnix' from the seconds part. Negative
    -- timestamps are fine since the seconds field is signed.
    timestampStreams (nsVec :: VP.Vector Int64) =
      let !secs = VP.map (\ns -> ns `quot` 1_000_000_000 - orcEpochSecondsFromUnix) nsVec
          !nanos = VP.map (\ns -> ns `rem` 1_000_000_000) nsVec
          !(secBs, nanoBs) = OW.encodeTimestampColumn secs nanos
      in withPresent (V.fromList [(streamData, cid, secBs), (streamSecondary, cid, nanoBs)])

    -- DIRECT_V2 strings: DATA is the present values' bytes back
    -- to back, LENGTH their byte lengths. Both come straight from
    -- the Arrow offsets; when the null rows reference no bytes
    -- (the usual layout) DATA is a zero-copy slice of the column.
    stringStreams :: (Storable o, Integral o) => AC.BytesArray o -> V.Vector (Word64, Word64, ByteString)
    stringStreams ba@(AC.BytesArray mv offs dat) =
      let !lens = presentPrim id mv (offsetLengths offs)
          !start = fromIntegral (VS.head offs)
          !span' = fromIntegral (VS.last offs) - start
          !total = fromIntegral (VP.sum lens)
          !dataBs
            | total == span' = BS.take span' (BS.drop start dat)
            | otherwise =
                BS.concat (V.toList (presentBoxed (VS.length offs - 1) (AC.nullCount col) (AC.unsafeBytesAt ba)))
      in withPresent
           ( V.fromList
               [ (streamData, cid, dataBs)
               , (streamLength, cid, OW.encodeIntColumn lens False)
               ]
           )


{- | Present values of a column's slots, cast for ORC's encoders,
in one pass (one compaction pass when the column has nulls).
Works over any per-row vector that shares the column's validity.
-}
presentPrim
  :: (VG.Vector v a, VP.Prim b)
  => (a -> b) -> Maybe AC.Validity -> v a -> VP.Vector b
presentPrim f mv xs = case mv of
  Nothing -> VP.generate n (\i -> f (VG.unsafeIndex xs i))
  Just v -> runST $ do
    out <- VPM.unsafeNew (n - AC.validityNullCount v)
    let go !i !j
          | i >= n = pure ()
          | AC.isValidAt mv i = VPM.unsafeWrite out j (f (VG.unsafeIndex xs i)) *> go (i + 1) (j + 1)
          | otherwise = go (i + 1) j
    go 0 0
    VP.unsafeFreeze out
  where
    !n = VG.length xs
{-# INLINE presentPrim #-}


{- | Present values of a boxed-row view (bytes): @n@ rows, @nulls@
of them null, read through @at@.
-}
presentBoxed :: Int -> Int -> (Int -> Maybe a) -> V.Vector a
presentBoxed n nulls at = runST $ do
  out <- VM.unsafeNew (n - nulls)
  let go !i !j
        | i >= n = pure ()
        | otherwise = case at i of
            Just x -> VM.unsafeWrite out j x *> go (i + 1) (j + 1)
            Nothing -> go (i + 1) j
  go 0 0
  V.unsafeFreeze out
{-# INLINE presentBoxed #-}


-- ============================================================
-- ORC → Arrow
-- ============================================================

{- | Compute the starting ORC column id for each top-level
Arrow field, given the schema's field order. Mirrors
'buildSchemaTree' on the write side.
-}
fieldStartIds :: V.Vector AT.Field -> V.Vector Word64
fieldStartIds fields =
  V.fromList (go 1 (V.toList fields))
  where
    go !_ [] = []
    go !cur (f : rest) = cur : go (cur + fieldSpan f) rest


{- | How many ORC column-id slots does an Arrow field consume?
Matches 'assignIds' on the write side.
-}
fieldSpan :: AT.Field -> Word64
fieldSpan fld = case AT.fieldType fld of
  AT.AStruct -> 1 + sum (map fieldSpan (V.toList (AT.fieldChildren fld)))
  AT.AList ->
    1 + case V.toList (AT.fieldChildren fld) of
      [c] -> fieldSpan c
      _ -> 0
  AT.ALargeList ->
    1 + case V.toList (AT.fieldChildren fld) of
      [c] -> fieldSpan c
      _ -> 0
  AT.AMap _ ->
    case V.toList (AT.fieldChildren fld) of
      [entry]
        | AT.fieldType entry == AT.AStruct
        , V.length (AT.fieldChildren entry) == 2 ->
            let !kf = V.unsafeIndex (AT.fieldChildren entry) 0
                !vf = V.unsafeIndex (AT.fieldChildren entry) 1
            in 1 + fieldSpan kf + fieldSpan vf
      [kf, vf] -> 1 + fieldSpan kf + fieldSpan vf
      _ -> 1
  _ -> 1


{- | Recursive reader: dispatches to the nested decoder for
struct / list / map, and to the leaf decoder for everything
else.
-}
decodeColumnNested
  :: Word64
  -> AT.Field
  -> Int
  -> ByteString
  -> V.Vector OSt.Stream
  -> Either String AC.ColumnArray
decodeColumnNested cid fld numRows stripeBs streams =
  case AT.fieldType fld of
    AT.AStruct -> do
      let !kids = AT.fieldChildren fld
          !kidCids = V.prescanl' (+) (cid + 1) (V.map fieldSpan kids)
      childCols <-
        V.zipWithM
          (\kidCid kidFld -> decodeColumnNested kidCid kidFld numRows stripeBs streams)
          kidCids
          kids
      AC.mkStruct numRows Nothing (V.zipWith (\k c -> (AT.fieldName k, c)) kids childCols)
    AT.AList -> decodeListLike AC.mkList cid fld numRows stripeBs streams
    AT.ALargeList -> decodeListLike AC.mkLargeList cid fld numRows stripeBs streams
    AT.AMap _ -> decodeMap cid fld numRows stripeBs streams
    _ -> decodeOneColumn cid fld numRows stripeBs streams


{- | Shared list decoder for 'AList' (Int32 offsets) and
'ALargeList' (Int64 offsets). The parent's LENGTH stream gives
per-row child counts; the child column holds exactly the rows
they add up to.
-}
decodeListLike
  :: (Storable o, Integral o, Bounded o)
  => (Maybe AC.Validity -> VS.Vector o -> AC.ColumnArray -> Either String AC.ColumnArray)
  -> Word64
  -> AT.Field
  -> Int
  -> ByteString
  -> V.Vector OSt.Stream
  -> Either String AC.ColumnArray
decodeListLike mk cid fld numRows stripeBs streams =
  case V.toList (AT.fieldChildren fld) of
    [childFld] -> do
      offsets <- readOffsets cid numRows stripeBs streams
      childCol <- decodeColumnNested (cid + 1) childFld (fromIntegral (VS.last offsets)) stripeBs streams
      mk Nothing offsets childCol
    _ -> Left "ORC.Arrow: AList / ALargeList must have exactly one child"


decodeMap
  :: Word64
  -> AT.Field
  -> Int
  -> ByteString
  -> V.Vector OSt.Stream
  -> Either String AC.ColumnArray
decodeMap cid fld numRows stripeBs streams = do
  -- Peel through the intermediate entries-struct if present.
  (kf, vf) <- case V.toList (AT.fieldChildren fld) of
    [entry]
      | AT.fieldType entry == AT.AStruct
      , V.length (AT.fieldChildren entry) == 2 ->
          Right
            ( V.unsafeIndex (AT.fieldChildren entry) 0
            , V.unsafeIndex (AT.fieldChildren entry) 1
            )
    [k, v] -> Right (k, v)
    _ -> Left "ORC.Arrow: AMap must carry key + value children"
  offsets <- readOffsets cid numRows stripeBs streams
  let !childCount = fromIntegral (VS.last offsets :: Int32)
      !kCid = cid + 1
  keys <- decodeColumnNested kCid kf childCount stripeBs streams
  vals <- decodeColumnNested (kCid + fieldSpan kf) vf childCount stripeBs streams
  AC.mkMap Nothing offsets keys vals


{- | Load the LENGTH stream for @cid@ (unsigned RLE v2, one entry
per row) and turn it into Arrow offsets starting at zero.
-}
readOffsets
  :: (Storable o, Integral o, Bounded o)
  => Word64
  -> Int
  -> ByteString
  -> V.Vector OSt.Stream
  -> Either String (VS.Vector o)
readOffsets cid n stripeBs streams =
  case sliceForCid cid streamLength stripeBs streams of
    Nothing -> Left $ "ORC.Arrow: column " ++ show cid ++ " missing LENGTH stream"
    Just bs -> do
      lens <- OR.decodeRLEv2Int False n bs
      lengthsToOffsets n Nothing lens


{- | Offsets (rows + 1 entries, from zero) for @n@ rows whose valid
rows take their lengths, in order, from @lens@; null rows are
empty. 'Left' when a length is negative, @lens@ is short, or the
total overflows the offset type.
-}
lengthsToOffsets
  :: forall o
   . (Storable o, Integral o, Bounded o)
  => Int -> Maybe AC.Validity -> VP.Vector Int64 -> Either String (VS.Vector o)
lengthsToOffsets n mv lens
  | VP.length lens < present =
      Left ("ORC.Arrow: LENGTH stream holds " ++ show (VP.length lens) ++ " entries, expected " ++ show present)
  | VP.any (< 0) used = Left "ORC.Arrow: negative length in LENGTH stream"
  | VP.sum used > fromIntegral (maxBound :: o) =
      Left "ORC.Arrow: LENGTH stream total overflows the Arrow offset type"
  | otherwise = Right $ VS.create $ do
      out <- VSM.unsafeNew (n + 1)
      VSM.unsafeWrite out 0 0
      let go !i !j !acc
            | i >= n = pure ()
            | AC.isValidAt mv i = do
                let !acc' = acc + fromIntegral (VP.unsafeIndex used j)
                VSM.unsafeWrite out (i + 1) acc'
                go (i + 1) (j + 1) acc'
            | otherwise = VSM.unsafeWrite out (i + 1) acc *> go (i + 1) j acc
      go 0 0 0
      pure out
  where
    !present = n - maybe 0 AC.validityNullCount mv
    !used = VP.take present lens


{- | Shared helper: find the byte-slice for @(cid, kind)@ in the
stripe's declared-stream layout.
-}
sliceForCid
  :: Word64
  -> Word64
  -> ByteString
  -> V.Vector OSt.Stream
  -> Maybe ByteString
sliceForCid cid kind stripeBs streams =
  case V.foldl'
    ( \(off, found) s ->
        case found of
          Just _ -> (off, found)
          Nothing
            | OSt.stColumn s == cid && OSt.stKind s == kind ->
                (off, Just (off, OSt.stLength s))
            | otherwise ->
                (off + OSt.stLength s, Nothing)
    )
    (0 :: Word64, Nothing)
    streams of
    (_, Just (off, len)) ->
      Just (BS.take (fromIntegral len) (BS.drop (fromIntegral off) stripeBs))
    _ -> Nothing


{- | Read a single stripe from an ORC file and lift each leaf
column to its Arrow shape. Requires both the parsed footer
(from 'ORC.decodeORC') and the original file bytes
so we can slice the stripe payload.

The Arrow schema is consulted to resolve the per-column
target nullability (UTF-8 vs raw binary likewise).
-}
orcStripeToArrow
  :: AT.Schema
  -> ByteString
  -- ^ the full ORC file bytes
  -> OT.ORCFooter
  -- ^ pre-parsed footer (from 'ORC.decodeORC')
  -> Int
  -- ^ stripe index
  -> Either String (V.Vector AC.ColumnArray)
orcStripeToArrow sch fileBs footer stripeIdx = do
  ofile <- OR.loadORCFile fileBs
  let !si = OT.orcStripes (OR.ofFooter ofile) V.! stripeIdx
  stripeBytes <- OR.stripeSlice ofile stripeIdx
  stFooter <- OR.loadStripeFooter ofile stripeIdx
  let !numRows = fromIntegral (OT.siNumberOfRows si) :: Int
      !streams = OSt.sfStreams stFooter
      _ = footer
      !topFields = AT.arrowFields sch
  -- Top-level column ids are allocated exactly as on the write
  -- side (see 'buildSchemaTree'): 1, 1+span_0, 1+span_0+span_1, ...
  V.zipWithM
    (\cid fld -> decodeColumnNested cid fld numRows stripeBytes streams)
    (fieldStartIds topFields)
    topFields


{- | Decode one ORC column from its (kind, columnId, length)
stream descriptors plus the stripe data section. Walks the
stream descriptors to locate the @DATA@ (and @LENGTH@) byte
ranges for this column id, then dispatches to the appropriate
decoder in "ORC.Read".
-}
decodeOneColumn
  :: Word64
  -- ^ column id
  -> AT.Field
  -- ^ Arrow target field
  -> Int
  -- ^ stripe row count
  -> ByteString
  -- ^ stripe bytes
  -> V.Vector OSt.Stream
  -- ^ stream descriptors
  -> Either String AC.ColumnArray
decodeOneColumn cid fld numRows stripeBs streams = do
  -- The PRESENT stream (absent when the column has no nulls)
  -- becomes the Arrow validity; every value stream then holds
  -- only the present rows.
  mv <- case stream streamPresent of
    Nothing -> Right Nothing
    Just presentBs -> AC.validityFromBools <$> OR.decodePresentStream numRows presentBs
  let !present = numRows - maybe 0 AC.validityNullCount mv
  case AT.fieldType fld of
    AT.AInt _ signed -> do
      vals <- OR.decodeRLEv2Int signed present =<< required streamData
      intColumn (AT.fieldType fld) numRows mv vals
    AT.ABool -> do
      vals <- OR.decodeBooleanRLE present =<< required streamData
      Right (boolColumn numRows mv vals)
    AT.AFloatingPoint AT.Single -> do
      vals <- fixedWidthValues present =<< required streamData
      scatterPrim AC.PFloat numRows mv (VS.unsafeIndex vals)
    AT.AFloatingPoint AT.DoublePrecision -> do
      vals <- fixedWidthValues present =<< required streamData
      scatterPrim AC.PDouble numRows mv (VS.unsafeIndex vals)
    AT.AUtf8 -> stringColumn AC.mkUtf8 mv present
    AT.ALargeUtf8 -> stringColumn AC.mkLargeUtf8 mv present
    AT.ABinary -> stringColumn AC.mkBinary mv present
    AT.ALargeBinary -> stringColumn AC.mkLargeBinary mv present
    -- Temporal types: recover the int stream at the right Arrow
    -- flavour. Date32 uses i32 days, Date64 i64, Time i32/i64,
    -- Duration i64.
    AT.ADate _ -> temporal mv present
    AT.ATime _ _ -> temporal mv present
    AT.ADuration _ -> temporal mv present
    AT.ATimestamp _ _ -> do
      -- ORC timestamps are encoded as DATA (signed seconds
      -- since 2015-01-01 GMT, the ORC epoch, NOT 1970) +
      -- SECONDARY (per-row nano-of-second with the 3-bit
      -- trailing-zero scale). Reconstruct nanoseconds since
      -- 1970-01-01 from both streams so callers see the same
      -- semantics as Arrow's ColTimestamp.
      secs <- OR.decodeRLEv2Int True present =<< required streamData
      nanos <- OR.decodeRLEv2Int False present =<< required streamSecondary
      scatterPrim AC.PTimestamp numRows mv $ \j ->
        timestampToUnixNanos
          (OR.ORCTimestamp (VP.unsafeIndex secs j) (decodeORCNanos (VP.unsafeIndex nanos j)))
    other ->
      Left $
        "ORC.Arrow: column type "
          ++ show other
          ++ " not yet supported by the read bridge"
  where
    stream k = sliceForCid cid k stripeBs streams
    required k = case stream k of
      Just bs -> Right bs
      Nothing -> Left ("ORC.Arrow: column " ++ show cid ++ " missing stream kind " ++ show k)

    temporal mv present = do
      vals <- OR.decodeRLEv2Int True present =<< required streamData
      intColumn (AT.fieldType fld) numRows mv vals

    -- DIRECT_V2: LENGTH holds the present values' byte lengths,
    -- DATA their bytes back to back. The offsets come from the
    -- lengths and the data buffer aliases the stripe bytes; the
    -- validating constructor checks offsets (and UTF-8 for text).
    stringColumn
      :: (Storable o, Integral o, Bounded o)
      => (Maybe AC.Validity -> VS.Vector o -> ByteString -> Either String AC.ColumnArray)
      -> Maybe AC.Validity
      -> Int
      -> Either String AC.ColumnArray
    stringColumn mk mv present = do
      dataBs <- required streamData
      lens <- OR.decodeRLEv2Int False present =<< required streamLength
      offsets <- lengthsToOffsets numRows mv lens
      mk mv offsets dataBs


{- | ORC's SECONDARY timestamp encoding as this package writes it
('ORC.Write.encodeORCNano'): the low 3 bits give the number of
trailing decimal zeros dropped, the upper bits the remaining
value. Same arithmetic as the reader in "ORC.Read", which does not
export it.
-}
decodeORCNanos :: Int64 -> Int64
decodeORCNanos raw =
  let !encoded = fromIntegral raw :: Word64
      !zeros = fromIntegral (encoded .&. 0x7) :: Int
      !base = fromIntegral (encoded `shiftR` 3) :: Int64
  in base * 10 ^ zeros
{-# INLINE decodeORCNanos #-}


{- | Lift a present-only ORC integer stream to the Arrow column the
field asks for, narrowing to its width. Covers integers and the
integer-backed temporal types.
-}
intColumn
  :: AT.ArrowType -> Int -> Maybe AC.Validity -> VP.Vector Int64 -> Either String AC.ColumnArray
intColumn ty n mv vals = case ty of
  AT.AInt 8 True -> narrow AC.PInt8
  AT.AInt 16 True -> narrow AC.PInt16
  AT.AInt 32 True -> narrow AC.PInt32
  AT.AInt 64 True -> narrow AC.PInt64
  AT.AInt 8 False -> narrow AC.PUInt8
  AT.AInt 16 False -> narrow AC.PUInt16
  AT.AInt 32 False -> narrow AC.PUInt32
  AT.AInt 64 False -> narrow AC.PUInt64
  AT.ADate AT.DateDay -> narrow AC.PDate32
  AT.ADate AT.DateMillisecond -> narrow AC.PDate64
  AT.ATime _ 32 -> narrow AC.PTime32
  AT.ATime _ 64 -> narrow AC.PTime64
  AT.ADuration _ -> narrow AC.PDuration
  _ -> Left ("ORC.Arrow: no integer column for " ++ show ty)
  where
    narrow :: (Storable a, Num a) => AC.PrimType a -> Either String AC.ColumnArray
    narrow t = scatterPrim t n mv (\j -> fromIntegral (VP.unsafeIndex vals j))
    {-# INLINE narrow #-}


{- | Build an @n@-row fixed-width column whose valid rows take
present value @j@ (in order) from @at@. One pass into a pinned
buffer; null slots are zero.
-}
scatterPrim
  :: Storable a
  => AC.PrimType a -> Int -> Maybe AC.Validity -> (Int -> a) -> Either String AC.ColumnArray
scatterPrim t n mv at = case mv of
  Nothing -> Right (AC.primColumn t (VS.generate n at))
  Just _ ->
    AC.mkPrim t mv $ VS.create $ do
      out <- VSM.unsafeNew n
      let (fp, _) = VSM.unsafeToForeignPtr0 out
      unsafeIOToST (withForeignPtr fp (\p -> fillBytes p 0 (n * sizeOf (at 0))))
      let go !i !j
            | i >= n = pure ()
            | AC.isValidAt mv i = VSM.unsafeWrite out i (at j) *> go (i + 1) (j + 1)
            | otherwise = go (i + 1) j
      go 0 0
      pure out
{-# INLINE scatterPrim #-}


-- | ORC boolean values (present-only) scattered over @n@ rows.
boolColumn :: Int -> Maybe AC.Validity -> V.Vector Bool -> AC.ColumnArray
boolColumn n mv vals = case mv of
  Nothing -> AC.fromBools vals
  Just _ -> runST $ do
    b <- AC.newBoolBuilder n
    let go !i !j
          | i >= n = pure ()
          | AC.isValidAt mv i = AC.appendBool b (V.unsafeIndex vals j) *> go (i + 1) (j + 1)
          | otherwise = AC.appendNull b *> go (i + 1) j
    go 0 0
    AC.freezeBuilder b


{- | The first @k@ little-endian values of a fixed-width stream.
Aliases the stream bytes when they are suitably aligned (one
aligned copy otherwise).
-}
fixedWidthValues :: forall a. Storable a => Int -> ByteString -> Either String (VS.Vector a)
fixedWidthValues k bs
  | BS.length bs < need = Left ("ORC.Arrow: DATA stream too short: " ++ show (BS.length bs) ++ " < " ++ show need)
  | otherwise = Right (bytesToStorable (BS.take need bs))
  where
    !need = k * sizeOf (undefined :: a)


-- ============================================================
-- Streaming reader (one stripe at a time)
-- ============================================================

{- | Number of stripes in an ORC file's footer. Useful as a loop
bound for 'orcStripeToArrow' / 'streamStripesIter'.
-}
numStripes :: OT.ORCFooter -> Int
numStripes = V.length . OT.orcStripes


{- | Eager list of @Either String batch@: one slot per stripe.
Mirrors 'Parquet.Arrow.streamRowGroups' shape so callers can
pick whichever format they're targeting and use the same
driver. Prefer 'streamStripesIter' for new code.
-}
streamStripes
  :: AT.Schema
  -> ByteString
  -> OT.ORCFooter
  -> [Either String (V.Vector AC.ColumnArray)]
streamStripes sch fileBs footer =
  [ orcStripeToArrow sch fileBs footer i
  | i <- [0 .. numStripes footer - 1]
  ]


{- | Iterator over stripes. Each step decodes one stripe to an
Arrow batch on demand. Errors halt the iterator at the failing
stripe (rather than being threaded through a list).
-}
streamStripesIter
  :: AT.Schema
  -> ByteString
  -> OT.ORCFooter
  -> IS.Iter (V.Vector AC.ColumnArray)
streamStripesIter sch fileBs footer =
  IS.iterFromIndexed (numStripes footer) $ \i ->
    orcStripeToArrow sch fileBs footer i


{- | Like 'streamStripesIter' but only decodes the named columns
of each stripe. Names absent from the source schema cause every
iterator step to fail with the same error.

Equivalent to @'streamStripesIter' (projectFields names sch)@
but with an explicit error path so the caller doesn't have to
pre-project the schema.
-}
streamStripesProjectedIter
  :: AT.Schema
  -> [Text]
  -> ByteString
  -> OT.ORCFooter
  -> IS.Iter (V.Vector AC.ColumnArray)
streamStripesProjectedIter sch names fileBs footer =
  case projectFields names sch of
    Left e -> IS.iterUnfold () (\_ -> Left e)
    Right narrow -> streamStripesIter narrow fileBs footer


-- | Decode a single stripe with column projection.
orcStripeToArrowProjected
  :: AT.Schema
  -> [Text]
  -> ByteString
  -> OT.ORCFooter
  -> Int
  -> Either String (V.Vector AC.ColumnArray)
orcStripeToArrowProjected sch names fileBs footer stripeIdx = do
  narrow <- projectFields names sch
  orcStripeToArrow narrow fileBs footer stripeIdx


{- | Build a sub-schema by name, preserving the order of @names@.
Names not present in the source schema produce an error.
-}
projectFields :: [Text] -> AT.Schema -> Either String AT.Schema
projectFields names sch =
  let !fields = AT.arrowFields sch
      !byName =
        Map.fromList
          [(AT.fieldName f, f) | f <- V.toList fields]
      pickOne nm = case Map.lookup nm byName of
        Just f -> Right f
        Nothing ->
          Left $
            "ORC.Arrow: projected column "
              ++ show nm
              ++ " not present in target schema"
  in do
       fs <- traverse pickOne names
       Right sch {AT.arrowFields = V.fromList fs}


-- ============================================================
-- Stripe-level predicate pushdown
-- ============================================================

{- | Iterator over stripes that drops any stripe whose
file-footer 'ColumnStatistics' prove the predicate matches
no rows. ORC stores per-file column statistics in the footer
(one entry per leaf column, /not/ per-stripe); the read API
has to reconstruct per-stripe stats from the protobuf
@StripeStatistics@ payloads in @Metadata@. For now this
shape uses the file-level stats — accurate when the file is
a single stripe (the common Iceberg case) and conservatively
safe (PMaybeKeep) for multi-stripe files where per-stripe
stats would be tighter.

Returns @(totalStripes, droppedStripes, iter)@ so callers
can log the skip ratio.
-}
streamStripesFilteredIter
  :: AT.Schema
  -> OStats.Predicate
  -> ByteString
  -> OT.ORCFooter
  -> (Int, Int, IS.Iter (V.Vector AC.ColumnArray))
streamStripesFilteredIter sch predicate fileBs footer =
  let !leafNames = leafColumnNames sch
      !stats = OT.orcStatistics footer
      !allDecide =
        -- File-level decision applies to every stripe (we
        -- don't yet read the per-stripe Metadata payload).
        OStats.evalStripe leafNames stats predicate
      !nStripes = numStripes footer
      keep _ = allDecide == OStats.PMaybeKeep
      !kept = V.filter keep (V.enumFromN 0 nStripes)
      !nKept = V.length kept
      !nSkip = nStripes - nKept
      step k =
        let !i = V.unsafeIndex kept k
        in orcStripeToArrow sch fileBs footer i
  in (nStripes, nSkip, IS.iterFromIndexed nKept step)


{- | Combination of 'streamStripesProjectedIter' and
'streamStripesFilteredIter': only decodes the named columns
of stripes that survive the predicate.
-}
streamStripesProjectedFilteredIter
  :: AT.Schema
  -> [Text]
  -> OStats.Predicate
  -> ByteString
  -> OT.ORCFooter
  -> Either String (Int, Int, IS.Iter (V.Vector AC.ColumnArray))
streamStripesProjectedFilteredIter sch names predicate fileBs footer = do
  narrow <- projectFields names sch
  let (nStripes, nSkip, it) =
        streamStripesFilteredIter narrow predicate fileBs footer
  Right (nStripes, nSkip, it)


{- | Leaf column names for an Arrow schema (used as the column
name vector by the predicate evaluator).
-}
leafColumnNames :: AT.Schema -> V.Vector Text
leafColumnNames sch =
  V.map AT.fieldName (AT.arrowFields sch)
