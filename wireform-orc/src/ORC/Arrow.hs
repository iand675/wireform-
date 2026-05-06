{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Arrow ↔ ORC column-data bridge.
--
-- Lets callers keep a single in-memory representation
-- ('Arrow.Column.ColumnArray') and serialise it as ORC instead.
-- Mirrors "Parquet.Arrow" for the same flat-primitive subset
-- ORC's writer natively encodes today.
--
-- @
-- -- Arrow → ORC
-- let !(types, stripes) = 'arrowToORC' arrowSchema arrowBatches
--     bytes             = 'ORC.encodeORC'
--                            'ORC.defaultWriteOptions'
--                            types stripes
--
-- -- ORC → Arrow (one stripe at a time)
-- footer <- 'ORC.decodeORC' bytes
-- batch  <- 'orcStripeToArrow' arrowSchema bytes footer 0
-- @
--
-- Coverage today: flat-primitive Arrow columns only — 'AInt'
-- (8/16/32/64), 'ABool', 'AFloatingPoint' (Single + Double),
-- 'AUtf8', 'ABinary', and their nullable variants. Nested types
-- (struct / list / map / union / dictionary / view / REE)
-- aren't yet routed through ORC's nested writer; they fall
-- through to a clean 'Left' at translation time.
module ORC.Arrow
  ( -- * Arrow → ORC
    arrowToORC
  , columnArrayToORCStreams
    -- * ORC → Arrow
  , orcStripeToArrow
  , orcStripeToArrowProjected
    -- * Streaming reader (one stripe at a time)
  , streamStripes
  , streamStripesIter
  , streamStripesProjectedIter
  , streamStripesFilteredIter
  , streamStripesProjectedFilteredIter
  , numStripes
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int8, Int16, Int32, Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import Data.Word (Word8, Word16, Word32, Word64)

import qualified Arrow.Column as AC
import qualified Arrow.Types as AT

import qualified Columnar.NullableBuild as NB
import qualified Columnar.Stream as IS

import qualified ORC.Read   as OR
import qualified ORC.Statistics as OStats
import qualified ORC.Stripe as OSt
import qualified ORC.Types  as OT
import qualified ORC.Write  as OW

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

-- | @Kind = SECONDARY@ (id 5) — used for ORC timestamp
-- nanoseconds and decimal scale. The previous value here
-- was 7, which is actually @BLOOM_FILTER@; that mistake
-- silently mis-tagged every SECONDARY stream we wrote so
-- pyarrow's reader couldn't find them.
streamSecondary :: Word64
streamSecondary = 5

-- | The ORC timestamp epoch is 2015-01-01 00:00:00 UTC, /not/
-- the Unix epoch. Per spec
-- (https://orc.apache.org/specification/ORCv1/#timestamp-data)
-- the DATA stream stores seconds relative to ORC's epoch; we
-- shift between the two when round-tripping with anything that
-- speaks Unix time. The constant below is
-- @1_420_070_400 = (2015 - 1970) * 365.25 * 86400@ rounded down
-- to the second (the exact value Java / C++ / Rust ORC use).
orcEpochSecondsFromUnix :: Int64
orcEpochSecondsFromUnix = 1_420_070_400

-- | Convert an 'OR.ORCTimestamp' (seconds-since-ORC-epoch +
-- decoded-nanos) back to whole nanoseconds since the Unix epoch.
timestampToUnixNanos :: OR.ORCTimestamp -> Int64
timestampToUnixNanos (OR.ORCTimestamp s n) =
  (s + orcEpochSecondsFromUnix) * 1_000_000_000 + n

-- ============================================================
-- Arrow → ORC
-- ============================================================

-- | Lower an Arrow schema + a sequence of column-major batches
-- to the inputs 'ORC.encodeORC' expects.
--
-- Each Arrow batch becomes one ORC stripe. The output pairs
-- each stripe's stream tuples with its row count (derived from
-- the first top-level column's length) so
-- 'ORC.encodeORC' can stamp @siNumberOfRows@ directly
-- into the stripe information.
--
-- Supports both flat and nested types:
--
--   * flat primitives + nullable variants (see 'columnArrayToORCStreams')
--   * temporal (Date, Time, Timestamp, Duration) + Decimal
--   * @ColStruct@ → ORC 'TKStruct' with child column ids
--   * @ColList@ / @ColLargeList@ → ORC 'TKList' with a LENGTH
--     stream on the parent (per-row child counts)
--   * @ColMap@ → ORC 'TKMap' with a LENGTH stream on the parent +
--     key / value child columns
arrowToORC
  :: AT.Schema
  -> [V.Vector AC.ColumnArray]
  -> Either String ( V.Vector OT.ORCType
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
  stripes <- mapM
    (\cols -> do
        streams <- encodeStripe topIds cols
        let !rowCount = if V.null cols
                           then 0
                           else fromIntegral (AC.columnLength (V.head cols))
        Right (streams, rowCount))
    batches
  Right (types, stripes)

-- | Build ORC's flat 'ORCType' vector for an Arrow schema.
-- Returns the type vector plus a parallel list of the /top-level/
-- column ids (the ids assigned to each direct child of the root
-- struct). Encoders use the top-level ids to emit streams in
-- declaration order.
buildSchemaTree
  :: V.Vector AT.Field
  -> Either String (V.Vector OT.ORCType, V.Vector Word32)
buildSchemaTree topFields = do
  -- Reserve column 0 for the synthetic root struct.
  let !startCid = 1 :: Word32
  (childTypes, !topIds, !_nextCid) <-
    foldM
      (\(!acc, !ids, !cid) fld -> do
          (!subTypes, !next) <- assignIds fld cid
          Right ( acc V.++ subTypes
                , V.snoc ids cid
                , next
                ))
      (V.empty, V.empty, startCid)
      (V.toList topFields)
  let !rootType = OT.ORCType
        { OT.otKind       = OT.TKStruct
        , OT.otSubtypes   = topIds
        , OT.otFieldNames = V.map AT.fieldName topFields
        }
  Right (V.cons rootType childTypes, topIds)
  where
    foldM f z0 = go z0
      where
        go z []       = Right z
        go z (x:xs)   = f z x >>= \z' -> go z' xs

-- | Assign a depth-first column id to a field + every
-- descendant. Returns the types laid out in id order + the
-- next free column id.
assignIds
  :: AT.Field
  -> Word32                        -- ^ cid allocated to this field
  -> Either String (V.Vector OT.ORCType, Word32)
assignIds fld cid = case AT.fieldType fld of
  AT.AStruct -> do
    -- Struct's subtypes are its children, laid out immediately
    -- after the struct itself.
    let !children = AT.fieldChildren fld
    (childTypes, !topIds, !nextCid) <-
      layoutChildren children (cid + 1)
    let !selfType = OT.ORCType
          { OT.otKind       = OT.TKStruct
          , OT.otSubtypes   = topIds
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
            in  mapLike kf vf (cid + 1)
      [kf, vf] -> mapLike kf vf (cid + 1)
      _ -> Left "ORC.Arrow: AMap must have key + value children"

  _ -> do
    -- Leaf field: no subtypes, just the ORC kind.
    kind <- arrowLeafKindFor (AT.fieldType fld)
    let !selfType = OT.ORCType
          { OT.otKind       = kind
          , OT.otSubtypes   = V.empty
          , OT.otFieldNames = V.empty
          }
    Right (V.singleton selfType, cid + 1)
  where
    -- List / LargeList: single child immediately after the list.
    listLike childCid =
      case V.toList (AT.fieldChildren fld) of
        [child] -> do
          (childTypes, !nextCid) <- assignIds child childCid
          let !selfType = OT.ORCType
                { OT.otKind       = OT.TKList
                , OT.otSubtypes   = V.singleton childCid
                , OT.otFieldNames = V.empty
                }
          Right (V.cons selfType childTypes, nextCid)
        _ -> Left "ORC.Arrow: AList / ALargeList must have exactly one child"

    mapLike kf vf kCid = do
      (kTypes, !vCid)  <- assignIds kf kCid
      (vTypes, !nextCid) <- assignIds vf vCid
      let !selfType = OT.ORCType
            { OT.otKind       = OT.TKMap
            , OT.otSubtypes   = V.fromList [kCid, vCid]
            , OT.otFieldNames = V.empty
            }
      Right (V.cons selfType (kTypes V.++ vTypes), nextCid)

    -- Shared helper for struct field lists.
    layoutChildren
      :: V.Vector AT.Field
      -> Word32
      -> Either String (V.Vector OT.ORCType, V.Vector Word32, Word32)
    layoutChildren children startCid =
      let go !_ !acc !ids !n []             = Right (acc, ids, n)
          go cur !acc !ids !n (c : rest)    = do
            (ts, next) <- assignIds c cur
            go next (acc V.++ ts) (V.snoc ids cur) next rest
      in go startCid V.empty V.empty startCid (V.toList children)

-- | Leaf-kind mapping used by 'assignIds'. Nested kinds go
-- through their own branches above; this table covers the
-- primitives + temporals only.
arrowLeafKindFor :: AT.ArrowType -> Either String OT.TypeKind
arrowLeafKindFor ty = case ty of
  AT.AInt 8  _                 -> Right OT.TKByte
  AT.AInt 16 _                 -> Right OT.TKShort
  AT.AInt 32 _                 -> Right OT.TKInt
  AT.AInt 64 _                 -> Right OT.TKLong
  AT.ABool                     -> Right OT.TKBoolean
  AT.AFloatingPoint AT.Single  -> Right OT.TKFloat
  AT.AFloatingPoint AT.DoublePrecision -> Right OT.TKDouble
  AT.AUtf8                     -> Right OT.TKString
  AT.ABinary                   -> Right OT.TKBinary
  AT.ALargeUtf8                -> Right OT.TKString
  AT.ALargeBinary              -> Right OT.TKBinary
  AT.ADate _                   -> Right OT.TKDate
  -- Arrow Timestamp(_, Just tz) maps to ORC's TIMESTAMP_INSTANT
  -- (UTC-anchored); without tz it maps to local-time TIMESTAMP.
  AT.ATimestamp _ (Just _)     -> Right OT.TKTimestampInstant
  AT.ATimestamp _ Nothing      -> Right OT.TKTimestamp
  AT.ADuration _               -> Right OT.TKLong
  AT.ATime _ _                 -> Right OT.TKLong
  AT.ADecimal _ _              -> Right OT.TKDecimal
  other ->
    Left $ "ORC.Arrow: Arrow type " ++ show other
            ++ " has no flat ORC equivalent"

-- | Build one stripe by walking the top-level columns with their
-- allocated ORC column ids. Lists + structs + maps recurse into
-- 'columnArrayToORCStreamsNested' which tracks its own id cursor.
encodeStripe
  :: V.Vector Word32
  -> V.Vector AC.ColumnArray
  -> Either String (V.Vector (Word64, Word64, ByteString))
encodeStripe topIds cols
  | V.length topIds /= V.length cols =
      Left $ "ORC.Arrow.arrowToORC: schema has "
              ++ show (V.length topIds)
              ++ " top-level fields but batch has "
              ++ show (V.length cols)
              ++ " columns"
  | otherwise = do
      streamLists <- V.zipWithM
        (\cid col -> columnArrayToORCStreamsNested (fromIntegral cid) col)
        topIds cols
      Right (V.concat (V.toList streamLists))

-- | Dispatch between nested + flat stream-encoding. Nested
-- shapes know how to recurse; everything else falls through to
-- 'columnArrayToORCStreams'.
columnArrayToORCStreamsNested
  :: Word64
  -> AC.ColumnArray
  -> Either String (V.Vector (Word64, Word64, ByteString))
columnArrayToORCStreamsNested cid col = case col of
  AC.ColStruct namedChildren -> do
    childStreams <- V.mapM (uncurry columnArrayToORCStreamsNested)
      (assignChildIds (cid + 1) (V.map snd namedChildren))
    -- Structs in ORC have no streams of their own when the
    -- struct is non-nullable; the children carry the data. A
    -- proper nullable-struct implementation would emit a
    -- PRESENT stream at @cid@ — deferred until a concrete
    -- generator stresses it.
    Right (V.concat (V.toList childStreams))

  AC.ColList offsets child -> encodeList cid (VP.toList offsets) child
  AC.ColLargeList offsets child -> encodeList cid (VP.toList offsets) child
  AC.ColMap offsets keys values -> encodeMap cid (VP.toList offsets) keys values
  _ -> columnArrayToORCStreams cid col
  where
    -- Given a starting cid and a vector of child columns, pair
    -- each child with its own depth-first-allocated cid. We
    -- only know the types at emission time (the schema was
    -- flattened in 'buildSchemaTree' using the same walk), so
    -- this has to match 'assignIds' exactly.
    assignChildIds :: Word64 -> V.Vector AC.ColumnArray
                   -> V.Vector (Word64, AC.ColumnArray)
    assignChildIds startCid children =
      let go !_ acc [] = acc
          go !cur acc (c:rest) =
            let !span' = columnArraySpan c
            in go (cur + span') (acc ++ [(cur, c)]) rest
      in V.fromList (go startCid [] (V.toList children))

    -- Encode an Arrow list column into ORC streams: LENGTH on
    -- the parent (per-row child counts) + recursive child
    -- streams.
    encodeList
      :: (Integral a, VP.Prim a)
      => Word64 -> [a] -> AC.ColumnArray
      -> Either String (V.Vector (Word64, Word64, ByteString))
    encodeList parentCid offs child = do
      let !lengths = offsetsToLengths offs
          !lenBs   = OW.encodeIntColumn (VP.fromList lengths) False
          !lenStream = V.singleton (streamLength, parentCid, lenBs)
      childStreams <- columnArrayToORCStreamsNested (parentCid + 1) child
      Right (lenStream <> childStreams)

    encodeMap
      :: (Integral a, VP.Prim a)
      => Word64 -> [a] -> AC.ColumnArray -> AC.ColumnArray
      -> Either String (V.Vector (Word64, Word64, ByteString))
    encodeMap parentCid offs keys values = do
      let !lengths = offsetsToLengths offs
          !lenBs   = OW.encodeIntColumn (VP.fromList lengths) False
          !lenStream = V.singleton (streamLength, parentCid, lenBs)
          !keyCid  = parentCid + 1
      keyStreams <- columnArrayToORCStreamsNested keyCid keys
      let !valCid = keyCid + columnArraySpan keys
      valStreams <- columnArrayToORCStreamsNested valCid values
      Right (lenStream <> keyStreams <> valStreams)

-- | How many ORC column-id slots does this 'ColumnArray'
-- occupy? Mirrors 'assignIds' on the field side; used to
-- advance the cid cursor when encoding struct children.
columnArraySpan :: AC.ColumnArray -> Word64
columnArraySpan col = case col of
  AC.ColStruct kids   -> 1 + sum (map (columnArraySpan . snd) (V.toList kids))
  AC.ColList _ inner  -> 1 + columnArraySpan inner
  AC.ColLargeList _ i -> 1 + columnArraySpan i
  _                   -> 1

-- | Offsets [o0, o1, .., oN] → lengths [o1-o0, o2-o1, ..,
-- oN - o_{N-1}] with a length vector of N elements.
offsetsToLengths :: Integral a => [a] -> [Int64]
offsetsToLengths []         = []
offsetsToLengths [_]        = []
offsetsToLengths (a:b:rest) = fromIntegral (b - a) : offsetsToLengths (b:rest)

-- | Encode one Arrow column at the given ORC column id into its
-- ORC stream tuples. Returns @[(streamKind, columnId, payload)]@
-- in emission order. Nullable columns now emit a PRESENT stream
-- so round-trips recover the nulls at the right positions.
columnArrayToORCStreams
  :: Word64
  -> AC.ColumnArray
  -> Either String (V.Vector (Word64, Word64, ByteString))
columnArrayToORCStreams !cid = go
  where
    go col = case col of
      AC.ColInt8  v -> Right (intStreams Nothing cid (signedI8 v))
      AC.ColInt16 v -> Right (intStreams Nothing cid (signedI16 v))
      AC.ColInt32 v -> Right (intStreams Nothing cid (signedI32 v))
      AC.ColInt64 v -> Right (intStreams Nothing cid v)
      AC.ColUInt8  v -> Right (intStreams Nothing cid (VP.map fromIntegral v))
      AC.ColUInt16 v -> Right (intStreams Nothing cid (VP.map fromIntegral v))
      AC.ColUInt32 v -> Right (intStreams Nothing cid (VP.map fromIntegral v))
      AC.ColUInt64 v -> Right (intStreams Nothing cid (VP.map fromIntegral v))

      AC.ColBool   v -> Right (boolStreams Nothing cid v)
      AC.ColFloat  v -> Right (floatStreams Nothing cid v)
      AC.ColDouble v -> Right (doubleStreams Nothing cid v)

      AC.ColUtf8 v -> Right (stringStreams Nothing cid (V.map TE.encodeUtf8 v))
      AC.ColLargeUtf8 v -> Right (stringStreams Nothing cid (V.map TE.encodeUtf8 v))
      AC.ColBinary v -> Right (stringStreams Nothing cid v)
      AC.ColLargeBinary v -> Right (stringStreams Nothing cid v)

      -- Temporal types: map to an integer stream at the natural
      -- width. Date = days-since-epoch, Time/Duration/Timestamp
      -- use the Int32/Int64 payload as-is.
      AC.ColDate32 v -> Right (intStreams Nothing cid (signedI32 v))
      AC.ColDate64 v -> Right (intStreams Nothing cid v)
      AC.ColTime32 v -> Right (intStreams Nothing cid (signedI32 v))
      AC.ColTime64 v -> Right (intStreams Nothing cid v)
      AC.ColTimestamp v -> Right (timestampStreams Nothing cid v)
      AC.ColDuration  v -> Right (intStreams Nothing cid v)

      -- Nullable variants: emit PRESENT + present-only data.
      AC.ColInt8Maybe   v -> Right (intMaybe v cid signedI8')
      AC.ColInt16Maybe  v -> Right (intMaybe v cid signedI16')
      AC.ColInt32Maybe  v -> Right (intMaybe v cid signedI32')
      AC.ColInt64Maybe  v -> Right (intMaybe v cid id)
      AC.ColUInt8Maybe  v -> Right (intMaybe v cid fromIntegral)
      AC.ColUInt16Maybe v -> Right (intMaybe v cid fromIntegral)
      AC.ColUInt32Maybe v -> Right (intMaybe v cid fromIntegral)
      AC.ColUInt64Maybe v -> Right (intMaybe v cid fromIntegral)

      AC.ColBoolMaybe   v ->
        let (pres, present) = presentBits v
        in Right (boolStreams (Just pres) cid present)
      AC.ColFloatMaybe  v ->
        let (pres, present) = presentFloat v
        in Right (floatStreams (Just pres) cid present)
      AC.ColDoubleMaybe v ->
        let (pres, present) = presentDouble v
        in Right (doubleStreams (Just pres) cid present)

      AC.ColUtf8Maybe   v ->
        let (pres, present) = presentBytes (V.map (fmap TE.encodeUtf8) v)
        in Right (stringStreams (Just pres) cid present)
      AC.ColLargeUtf8Maybe v ->
        let (pres, present) = presentBytes (V.map (fmap TE.encodeUtf8) v)
        in Right (stringStreams (Just pres) cid present)
      AC.ColBinaryMaybe v ->
        let (pres, present) = presentBytes v
        in Right (stringStreams (Just pres) cid present)
      AC.ColLargeBinaryMaybe v ->
        let (pres, present) = presentBytes v
        in Right (stringStreams (Just pres) cid present)

      -- Nullable temporals: reuse intMaybe with the matching
      -- width-preserving cast. Narrow Int32 payloads get
      -- signedI32'; native-Int64 Timestamps / Date64 / Time64 /
      -- Duration use id.
      AC.ColDate32Maybe    v -> Right (intMaybe v cid signedI32')
      AC.ColDate64Maybe    v -> Right (intMaybe v cid id)
      AC.ColTime32Maybe    v -> Right (intMaybe v cid signedI32')
      AC.ColTime64Maybe    v -> Right (intMaybe v cid id)
      AC.ColTimestampMaybe v -> Right (timestampMaybe v cid)
      AC.ColDurationMaybe  v -> Right (intMaybe v cid id)

      other -> Left $ "ORC.Arrow: column shape "
                       ++ show other
                       ++ " not supported by the bridge yet "
                       ++ "(nested types, dictionary, view, REE)"

    -- Build PRESENT stream + present-only Int64 payload for a
    -- nullable integer column. We materialise the entire payload
    -- at the requested signed width.
    intMaybe
      :: V.Vector (Maybe a)
      -> Word64
      -> (a -> Int64)
      -> V.Vector (Word64, Word64, ByteString)
    intMaybe vmb c cast =
      let !pres = OW.encodeBooleanRLE (V.map maybeToBool vmb)
          !xs   = NB.presentValuesPMap cast vmb
      in  intStreams (Just pres) c xs

    maybeToBool :: Maybe a -> Bool
    maybeToBool (Just _) = True
    maybeToBool Nothing  = False

    presentBits v =
      let !pres = OW.encodeBooleanRLE (V.map maybeToBool v)
          !vs   = NB.presentValuesV v
      in (pres, vs)
    presentFloat v =
      let !pres = OW.encodeBooleanRLE (V.map maybeToBool v)
          !vs   = NB.presentValuesP v
      in (pres, vs)
    presentDouble v =
      let !pres = OW.encodeBooleanRLE (V.map maybeToBool v)
          !vs   = NB.presentValuesP v
      in (pres, vs)
    presentBytes v =
      let !pres = OW.encodeBooleanRLE (V.map maybeToBool v)
          !vs   = NB.presentValuesV v
      in (pres, vs)

    -- ORC's RLE-v2 integer encoders take (Int64 vector, signed?).
    -- Each emitter optionally prepends a PRESENT stream.
    intStreams mPres !c xs =
      presentPrefix mPres c <>
        V.singleton (streamData, c, OW.encodeIntColumn xs True)

    -- ORC timestamps need both a DATA stream (signed seconds
    -- with the SPEC-defined epoch of 2015-01-01 GMT, NOT
    -- 1970-01-01 — the famous ORC epoch gotcha) and a
    -- SECONDARY stream (nanoseconds with the 3-bit
    -- trailing-zero encoding ORC defines). The Arrow
    -- 'ColTimestamp' payload is whole nanoseconds since
    -- 1970-01-01; convert to ORC's epoch by subtracting
    -- 'orcEpochSecondsFromUnix' from the seconds part. Negative
    -- timestamps are fine since the seconds field is signed.
    timestampStreams mPres !c (nsVec :: VP.Vector Int64) =
      let !secsUnix = VP.map (\ns -> ns `quot` 1_000_000_000) nsVec
          !secs     = VP.map (\s -> s - orcEpochSecondsFromUnix) secsUnix
          !nanos    = VP.map (\ns -> ns `rem`  1_000_000_000) nsVec
          !(secBs, nanoBs) = OW.encodeTimestampColumn secs nanos
      in  presentPrefix mPres c <>
            V.fromList
              [ (streamData,      c, secBs)
              , (streamSecondary, c, nanoBs)
              ]

    -- Nullable timestamp: PRESENT mask + per-present timestamp
    -- pair (DATA + SECONDARY).
    timestampMaybe v !c =
      let (!pres, !justs) = presentBytes v
          !nsVec    = VP.fromList (V.toList justs)
          !secsUnix = VP.map (\ns -> ns `quot` 1_000_000_000) nsVec
          !secs     = VP.map (\s -> s - orcEpochSecondsFromUnix) secsUnix
          !nanos    = VP.map (\ns -> ns `rem`  1_000_000_000) nsVec
          !(secBs, nanoBs) = OW.encodeTimestampColumn secs nanos
      in  V.fromList
            [ (streamPresent,   c, pres)
            , (streamData,      c, secBs)
            , (streamSecondary, c, nanoBs)
            ]
    boolStreams mPres !c xs =
      presentPrefix mPres c <>
        V.singleton (streamData, c, OW.encodeBooleanRLE xs)
    floatStreams mPres !c xs =
      presentPrefix mPres c <>
        V.singleton (streamData, c, OW.encodeFloatColumn xs)
    doubleStreams mPres !c xs =
      presentPrefix mPres c <>
        V.singleton (streamData, c, OW.encodeDoubleColumn xs)
    stringStreams mPres !c bytesVec =
      let !(dataBs, lengthBs) = OW.encodeStringDirectColumn (V.map decodeBytesAsText bytesVec)
      in presentPrefix mPres c <>
         V.fromList
           [ (streamData,   c, dataBs)
           , (streamLength, c, lengthBs)
           ]

    presentPrefix Nothing  _ = V.empty
    presentPrefix (Just p) c = V.singleton (streamPresent, c, p)

    -- Feed the writer's text-encoder by re-tagging the raw bytes
    -- as Text. ORC's DIRECT_V2 string writer treats the Text
    -- payload as a UTF-8 bytestring under the hood; decodeUtf8
    -- with replacement keeps non-text payloads valid for the
    -- binary case.
    decodeBytesAsText bs = case TE.decodeUtf8' bs of
      Right t -> t
      Left  _ -> T.pack (map (toEnum . fromIntegral) (BS.unpack bs))

-- Type-directed signed-cast helpers (for signed Arrow integers).
signedI8 :: VP.Vector Int8 -> VP.Vector Int64
signedI8 = VP.map fromIntegral
signedI16 :: VP.Vector Int16 -> VP.Vector Int64
signedI16 = VP.map fromIntegral
signedI32 :: VP.Vector Int32 -> VP.Vector Int64
signedI32 = VP.map fromIntegral
signedI8' :: Int8 -> Int64
signedI8' = fromIntegral
signedI16' :: Int16 -> Int64
signedI16' = fromIntegral
signedI32' :: Int32 -> Int64
signedI32' = fromIntegral

-- ============================================================
-- ORC → Arrow
-- ============================================================

-- | Compute the starting ORC column id for each top-level
-- Arrow field, given the schema's field order. Mirrors
-- 'buildSchemaTree' on the write side.
fieldStartIds :: V.Vector AT.Field -> V.Vector Word64
fieldStartIds fields =
  V.fromList (go 1 (V.toList fields))
  where
    go !_   []       = []
    go !cur (f:rest) = cur : go (cur + fieldSpan f) rest

-- | How many ORC column-id slots does an Arrow field consume?
-- Matches 'assignIds' on the write side.
fieldSpan :: AT.Field -> Word64
fieldSpan fld = case AT.fieldType fld of
  AT.AStruct      -> 1 + sum (map fieldSpan (V.toList (AT.fieldChildren fld)))
  AT.AList        -> 1 + case V.toList (AT.fieldChildren fld) of
                          [c] -> fieldSpan c
                          _   -> 0
  AT.ALargeList   -> 1 + case V.toList (AT.fieldChildren fld) of
                          [c] -> fieldSpan c
                          _   -> 0
  AT.AMap _ ->
    case V.toList (AT.fieldChildren fld) of
      [entry]
        | AT.fieldType entry == AT.AStruct
        , V.length (AT.fieldChildren entry) == 2 ->
            let !kf = V.unsafeIndex (AT.fieldChildren entry) 0
                !vf = V.unsafeIndex (AT.fieldChildren entry) 1
            in 1 + fieldSpan kf + fieldSpan vf
      [kf, vf] -> 1 + fieldSpan kf + fieldSpan vf
      _        -> 1
  _               -> 1

-- | Recursive reader: dispatches to the nested decoder for
-- struct / list / map, and to the leaf decoder for everything
-- else.
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
          !kidCids = V.fromList (go (cid + 1) (V.toList kids))
      childCols <- V.zipWithM
        (\kidCid kidFld -> decodeColumnNested kidCid kidFld numRows stripeBs streams)
        kidCids kids
      let !named = V.zipWith (\k c -> (AT.fieldName k, c)) kids childCols
      Right (AC.ColStruct named)

    AT.AList -> decodeListLike AC.ColList cid fld numRows stripeBs streams
    AT.ALargeList -> decodeListLikeLarge cid fld numRows stripeBs streams

    AT.AMap _ -> decodeMap cid fld numRows stripeBs streams

    _ -> decodeOneColumn cid fld numRows stripeBs streams
  where
    go !_   []       = []
    go !cur (f:rest) = cur : go (cur + fieldSpan f) rest

-- | Shared list-decoder for 'AList' (Int32 offsets).
-- The parent's LENGTH stream gives per-row child counts;
-- we materialise the full child column then build offsets.
decodeListLike
  :: (VP.Vector Int32 -> AC.ColumnArray -> AC.ColumnArray)
  -> Word64 -> AT.Field -> Int -> ByteString -> V.Vector OSt.Stream
  -> Either String AC.ColumnArray
decodeListLike wrap cid fld numRows stripeBs streams = do
  case V.toList (AT.fieldChildren fld) of
    [childFld] -> do
      lengths <- readLengthStream cid numRows stripeBs streams
      let !childCount = sum (map fromIntegral lengths) :: Int
      childCol <- decodeColumnNested (cid + 1) childFld childCount stripeBs streams
      let !offsets = VP.fromList (scanl (\a n -> a + fromIntegral n) (0 :: Int32) lengths)
      Right (wrap offsets childCol)
    _ -> Left "ORC.Arrow: AList must have exactly one child"

decodeListLikeLarge
  :: Word64 -> AT.Field -> Int -> ByteString -> V.Vector OSt.Stream
  -> Either String AC.ColumnArray
decodeListLikeLarge cid fld numRows stripeBs streams = do
  case V.toList (AT.fieldChildren fld) of
    [childFld] -> do
      lengths <- readLengthStream cid numRows stripeBs streams
      let !childCount = sum (map fromIntegral lengths) :: Int
      childCol <- decodeColumnNested (cid + 1) childFld childCount stripeBs streams
      let !offsets = VP.fromList (scanl (\a n -> a + fromIntegral n) (0 :: Int64) lengths)
      Right (AC.ColLargeList offsets childCol)
    _ -> Left "ORC.Arrow: ALargeList must have exactly one child"

decodeMap
  :: Word64 -> AT.Field -> Int -> ByteString -> V.Vector OSt.Stream
  -> Either String AC.ColumnArray
decodeMap cid fld numRows stripeBs streams = do
  -- Peel through the intermediate entries-struct if present.
  (kf, vf) <- case V.toList (AT.fieldChildren fld) of
    [entry]
      | AT.fieldType entry == AT.AStruct
      , V.length (AT.fieldChildren entry) == 2 ->
          Right ( V.unsafeIndex (AT.fieldChildren entry) 0
                , V.unsafeIndex (AT.fieldChildren entry) 1
                )
    [k, v]  -> Right (k, v)
    _       -> Left "ORC.Arrow: AMap must carry key + value children"
  lengths <- readLengthStream cid numRows stripeBs streams
  let !childCount = sum (map fromIntegral lengths) :: Int
      !kCid       = cid + 1
  keys <- decodeColumnNested kCid kf childCount stripeBs streams
  let !vCid = kCid + fieldSpan kf
  vals <- decodeColumnNested vCid vf childCount stripeBs streams
  let !offsets = VP.fromList (scanl (\a n -> a + fromIntegral n) (0 :: Int32) lengths)
  Right (AC.ColMap offsets keys vals)

-- | Load a LENGTH stream for @cid@ and decode @n@ per-row
-- lengths (unsigned int v2).
readLengthStream
  :: Word64 -> Int -> ByteString -> V.Vector OSt.Stream
  -> Either String [Word64]
readLengthStream cid n stripeBs streams =
  case sliceForCid cid streamLength stripeBs streams of
    Nothing -> Left $ "ORC.Arrow: column " ++ show cid ++ " missing LENGTH stream"
    Just bs -> do
      xs <- OR.decodeIntColumn False n bs Nothing
      Right [fromIntegral v | Just v <- V.toList xs]

-- | Shared helper: find the byte-slice for @(cid, kind)@ in the
-- stripe's declared-stream layout.
sliceForCid
  :: Word64
  -> Word64
  -> ByteString
  -> V.Vector OSt.Stream
  -> Maybe ByteString
sliceForCid cid kind stripeBs streams =
  case V.foldl'
         (\(off, found) s ->
             case found of
               Just _ -> (off, found)
               Nothing
                 | OSt.stColumn s == cid && OSt.stKind s == kind ->
                     (off, Just (off, OSt.stLength s))
                 | otherwise ->
                     (off + OSt.stLength s, Nothing))
         (0 :: Word64, Nothing) streams of
    (_, Just (off, len)) ->
      Just (BS.take (fromIntegral len) (BS.drop (fromIntegral off) stripeBs))
    _ -> Nothing

-- | Read a single stripe from an ORC file and lift each leaf
-- column to its Arrow shape. Requires both the parsed footer
-- (from 'ORC.decodeORC') and the original file bytes
-- so we can slice the stripe payload.
--
-- The Arrow schema is consulted to resolve the per-column
-- target nullability (UTF-8 vs raw binary likewise).
orcStripeToArrow
  :: AT.Schema
  -> ByteString             -- ^ the full ORC file bytes
  -> OT.ORCFooter           -- ^ pre-parsed footer (from 'ORC.decodeORC')
  -> Int                    -- ^ stripe index
  -> Either String (V.Vector AC.ColumnArray)
orcStripeToArrow sch fileBs footer stripeIdx = do
  ofile <- OR.loadORCFile fileBs
  let !si = OT.orcStripes (OR.ofFooter ofile) V.! stripeIdx
  stripeBytes <- OR.stripeSlice ofile stripeIdx
  stFooter    <- OR.loadStripeFooter ofile stripeIdx
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

-- | Decode one ORC column from its (kind, columnId, length)
-- stream descriptors plus the stripe data section. Walks the
-- stream descriptors to locate the @DATA@ (and @LENGTH@) byte
-- ranges for this column id, then dispatches to the appropriate
-- decoder in "ORC.Read".
decodeOneColumn
  :: Word64                      -- ^ column id
  -> AT.Field                    -- ^ Arrow target field
  -> Int                         -- ^ stripe row count
  -> ByteString                  -- ^ stripe bytes
  -> V.Vector OSt.Stream         -- ^ stream descriptors
  -> Either String AC.ColumnArray
decodeOneColumn cid fld numRows stripeBs streams = do
  -- Optionally pick up the PRESENT stream; pass to each decoder
  -- so nulls round-trip correctly.
  let mPresentBs = either (const Nothing) Just (sliceFor streamPresent)
  case AT.fieldType fld of
    AT.AInt _ True -> do
      dataBs <- sliceFor streamData
      xs <- OR.decodeIntColumn True numRows dataBs mPresentBs
      intToArrow (AT.fieldType fld) (AT.fieldNullable fld) xs
    AT.AInt _ False -> do
      dataBs <- sliceFor streamData
      xs <- OR.decodeIntColumn False numRows dataBs mPresentBs
      intToArrow (AT.fieldType fld) (AT.fieldNullable fld) xs
    AT.ABool -> do
      dataBs <- sliceFor streamData
      xs <- OR.decodeBoolColumn numRows dataBs mPresentBs
      if AT.fieldNullable fld
        then Right (AC.ColBoolMaybe xs)
        else Right (AC.ColBool (V.map (maybe False id) xs))
    AT.AFloatingPoint AT.Single -> do
      dataBs <- sliceFor streamData
      xs <- OR.decodeFloatColumn numRows dataBs mPresentBs
      if AT.fieldNullable fld
        then Right (AC.ColFloatMaybe xs)
        else Right (AC.ColFloat (VP.fromList (map (maybe 0 id) (V.toList xs))))
    AT.AFloatingPoint AT.DoublePrecision -> do
      dataBs <- sliceFor streamData
      xs <- OR.decodeDoubleColumn numRows dataBs mPresentBs
      if AT.fieldNullable fld
        then Right (AC.ColDoubleMaybe xs)
        else Right (AC.ColDouble (VP.fromList (map (maybe 0 id) (V.toList xs))))
    AT.AUtf8        -> stringColumn mPresentBs AT.AUtf8
    AT.ALargeUtf8   -> stringColumn mPresentBs AT.ALargeUtf8
    AT.ABinary      -> stringColumn mPresentBs AT.ABinary
    AT.ALargeBinary -> stringColumn mPresentBs AT.ALargeBinary
    -- Temporal types: recover the int stream at the right Arrow
    -- flavour. Date32 uses i32 days, Date64 i64, Time i32/i64,
    -- Timestamp / Duration i64.
    AT.ADate _ -> do
      dataBs <- sliceFor streamData
      xs <- OR.decodeIntColumn True numRows dataBs mPresentBs
      temporalToArrow (AT.fieldType fld) (AT.fieldNullable fld) xs
    AT.ATime _ _ -> do
      dataBs <- sliceFor streamData
      xs <- OR.decodeIntColumn True numRows dataBs mPresentBs
      temporalToArrow (AT.fieldType fld) (AT.fieldNullable fld) xs
    AT.ATimestamp _ _ -> do
      -- ORC timestamps are encoded as DATA (signed seconds
      -- since 2015-01-01 GMT, the ORC epoch — NOT 1970) +
      -- SECONDARY (per-row nano-of-second with the 3-bit
      -- trailing-zero scale). Reconstruct nanoseconds since
      -- 1970-01-01 from both streams so callers see the same
      -- semantics as Arrow's ColTimestamp.
      dataBs <- sliceFor streamData
      nanoBs <- sliceFor streamSecondary
      tss <- OR.decodeTimestampColumn numRows dataBs nanoBs mPresentBs
      let !nsVec = V.map (fmap timestampToUnixNanos) tss
      temporalToArrow (AT.fieldType fld) (AT.fieldNullable fld) nsVec
    AT.ADuration _ -> do
      dataBs <- sliceFor streamData
      xs <- OR.decodeIntColumn True numRows dataBs mPresentBs
      temporalToArrow (AT.fieldType fld) (AT.fieldNullable fld) xs
    other ->
      Left $ "ORC.Arrow: column type " ++ show other
              ++ " not yet supported by the read bridge"
  where
    -- Lazy stream-byte helper. Accumulates stream lengths in the
    -- declared stripe order and returns the first chunk that
    -- matches (column, kind).
    sliceFor k = case V.foldl'
                   (\(off, found) s ->
                       case found of
                         Just _ -> (off, found)
                         Nothing
                           | OSt.stColumn s == cid && OSt.stKind s == k ->
                               (off, Just (off, OSt.stLength s))
                           | otherwise ->
                               (off + OSt.stLength s, Nothing))
                   (0 :: Word64, Nothing) streams of
                   (_, Just (off, len)) ->
                     Right (BS.take (fromIntegral len)
                                    (BS.drop (fromIntegral off) stripeBs))
                   (_, Nothing) ->
                     Left $ "ORC.Arrow: column " ++ show cid
                              ++ " missing stream kind " ++ show k

    stringColumn mPresentBs ty = do
      dataBs   <- sliceFor streamData
      lengthBs <- sliceFor streamLength
      xs <- OR.decodeStringColumn numRows dataBs lengthBs BS.empty mPresentBs
      let !decoded = case ty of
            AT.ABinary       -> AC.ColBinary
                                  (V.map (maybe BS.empty TE.encodeUtf8) xs)
            AT.ALargeBinary  -> AC.ColLargeBinary
                                  (V.map (maybe BS.empty TE.encodeUtf8) xs)
            AT.ALargeUtf8    -> AC.ColLargeUtf8
                                  (V.map (maybe T.empty id) xs)
            _                -> AC.ColUtf8
                                  (V.map (maybe T.empty id) xs)
      if AT.fieldNullable fld
        then Right $ case ty of
               AT.ABinary       -> AC.ColBinaryMaybe (V.map (fmap TE.encodeUtf8) xs)
               AT.ALargeBinary  -> AC.ColLargeBinaryMaybe (V.map (fmap TE.encodeUtf8) xs)
               AT.ALargeUtf8    -> AC.ColLargeUtf8Maybe xs
               _                -> AC.ColUtf8Maybe xs
        else Right decoded

-- | Cast a @V.Vector (Maybe Int64)@ stream to the right Arrow
-- column flavour. ORC ints use a single Int64-backed RLE-v2
-- representation; we narrow back to the requested Arrow width
-- here.
intToArrow
  :: AT.ArrowType -> Bool -> V.Vector (Maybe Int64)
  -> Either String AC.ColumnArray
intToArrow ty nullable xs = case (ty, nullable) of
  (AT.AInt 8  True,  False) -> Right $! AC.ColInt8   (NB.presentValuesPMap narrow8  xs)
  (AT.AInt 16 True,  False) -> Right $! AC.ColInt16  (NB.presentValuesPMap narrow16 xs)
  (AT.AInt 32 True,  False) -> Right $! AC.ColInt32  (NB.presentValuesPMap narrow32 xs)
  (AT.AInt 64 True,  False) -> Right $! AC.ColInt64  (NB.presentValuesP xs)
  (AT.AInt 8  False, False) -> Right $! AC.ColUInt8  (NB.presentValuesPMap (fromIntegral :: Int64 -> Word8)  xs)
  (AT.AInt 16 False, False) -> Right $! AC.ColUInt16 (NB.presentValuesPMap (fromIntegral :: Int64 -> Word16) xs)
  (AT.AInt 32 False, False) -> Right $! AC.ColUInt32 (NB.presentValuesPMap (fromIntegral :: Int64 -> Word32) xs)
  (AT.AInt 64 False, False) -> Right $! AC.ColUInt64 (NB.presentValuesPMap (fromIntegral :: Int64 -> Word64) xs)
  (AT.AInt 8  True,  True)  -> Right $! AC.ColInt8Maybe   (V.map (fmap narrow8)  xs)
  (AT.AInt 16 True,  True)  -> Right $! AC.ColInt16Maybe  (V.map (fmap narrow16) xs)
  (AT.AInt 32 True,  True)  -> Right $! AC.ColInt32Maybe  (V.map (fmap narrow32) xs)
  (AT.AInt 64 True,  True)  -> Right $! AC.ColInt64Maybe  xs
  (AT.AInt 8  False, True)  -> Right $! AC.ColUInt8Maybe  (V.map (fmap fromIntegral) xs)
  (AT.AInt 16 False, True)  -> Right $! AC.ColUInt16Maybe (V.map (fmap fromIntegral) xs)
  (AT.AInt 32 False, True)  -> Right $! AC.ColUInt32Maybe (V.map (fmap fromIntegral) xs)
  (AT.AInt 64 False, True)  -> Right $! AC.ColUInt64Maybe (V.map (fmap fromIntegral) xs)
  _ -> Left $ "ORC.Arrow: unexpected type/null combo " ++ show (ty, nullable)
  where
    narrow8 :: Int64 -> Int8
    narrow8 = fromIntegral
    narrow16 :: Int64 -> Int16
    narrow16 = fromIntegral
    narrow32 :: Int64 -> Int32
    narrow32 = fromIntegral


-- | Lift a decoded ORC integer stream into one of Arrow's temporal
-- column shapes. Width narrowing happens here (Date32 / Time32
-- use Int32 under the hood).
temporalToArrow
  :: AT.ArrowType -> Bool -> V.Vector (Maybe Int64)
  -> Either String AC.ColumnArray
temporalToArrow ty nullable xs = case (ty, nullable) of
  (AT.ADate AT.DateDay, False) ->
    Right $! AC.ColDate32 (NB.presentValuesPMap narrow32 xs)
  (AT.ADate AT.DateMillisecond, False) ->
    Right $! AC.ColDate64 (NB.presentValuesP xs)
  (AT.ATime _ 32, False) ->
    Right $! AC.ColTime32 (NB.presentValuesPMap narrow32 xs)
  (AT.ATime _ 64, False) ->
    Right $! AC.ColTime64 (NB.presentValuesP xs)
  (AT.ATimestamp _ _, False) ->
    Right $! AC.ColTimestamp (NB.presentValuesP xs)
  (AT.ADuration _, False) ->
    Right $! AC.ColDuration (NB.presentValuesP xs)

  (AT.ADate AT.DateDay, True) ->
    Right $! AC.ColDate32Maybe (V.map (fmap narrow32) xs)
  (AT.ADate AT.DateMillisecond, True) ->
    Right $! AC.ColDate64Maybe xs
  (AT.ATime _ 32, True) ->
    Right $! AC.ColTime32Maybe (V.map (fmap narrow32) xs)
  (AT.ATime _ 64, True) ->
    Right $! AC.ColTime64Maybe xs
  (AT.ATimestamp _ _, True) ->
    Right $! AC.ColTimestampMaybe xs
  (AT.ADuration _, True) ->
    Right $! AC.ColDurationMaybe xs

  _ -> Left $ "ORC.Arrow.temporalToArrow: unexpected type/null combo "
                ++ show (ty, nullable)
  where
    narrow32 :: Int64 -> Int32
    narrow32 = fromIntegral

-- ============================================================
-- Streaming reader (one stripe at a time)
-- ============================================================

-- | Number of stripes in an ORC file's footer. Useful as a loop
-- bound for 'orcStripeToArrow' / 'streamStripesIter'.
numStripes :: OT.ORCFooter -> Int
numStripes = V.length . OT.orcStripes

-- | Eager list of @Either String batch@: one slot per stripe.
-- Mirrors 'Parquet.Arrow.streamRowGroups' shape so callers can
-- pick whichever format they're targeting and use the same
-- driver. Prefer 'streamStripesIter' for new code.
streamStripes
  :: AT.Schema
  -> ByteString
  -> OT.ORCFooter
  -> [Either String (V.Vector AC.ColumnArray)]
streamStripes sch fileBs footer =
  [ orcStripeToArrow sch fileBs footer i
  | i <- [0 .. numStripes footer - 1]
  ]

-- | Iterator over stripes. Each step decodes one stripe to an
-- Arrow batch on demand. Errors halt the iterator at the failing
-- stripe (rather than being threaded through a list).
streamStripesIter
  :: AT.Schema
  -> ByteString
  -> OT.ORCFooter
  -> IS.Iter (V.Vector AC.ColumnArray)
streamStripesIter sch fileBs footer =
  IS.iterFromIndexed (numStripes footer) $ \i ->
    orcStripeToArrow sch fileBs footer i

-- | Like 'streamStripesIter' but only decodes the named columns
-- of each stripe. Names absent from the source schema cause every
-- iterator step to fail with the same error.
--
-- Equivalent to @'streamStripesIter' (projectFields names sch)@
-- but with an explicit error path so the caller doesn't have to
-- pre-project the schema.
streamStripesProjectedIter
  :: AT.Schema
  -> [Text]
  -> ByteString
  -> OT.ORCFooter
  -> IS.Iter (V.Vector AC.ColumnArray)
streamStripesProjectedIter sch names fileBs footer =
  case projectFields names sch of
    Left e          -> IS.iterUnfold () (\_ -> Left e)
    Right narrow    -> streamStripesIter narrow fileBs footer

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

-- | Build a sub-schema by name, preserving the order of @names@.
-- Names not present in the source schema produce an error.
projectFields :: [Text] -> AT.Schema -> Either String AT.Schema
projectFields names sch =
  let !fields = AT.arrowFields sch
      !byName = Map.fromList
        [ (AT.fieldName f, f) | f <- V.toList fields ]
      pickOne nm = case Map.lookup nm byName of
        Just f  -> Right f
        Nothing -> Left $ "ORC.Arrow: projected column "
                          ++ show nm ++ " not present in target schema"
  in do
    fs <- traverse pickOne names
    Right sch { AT.arrowFields = V.fromList fs }

-- ============================================================
-- Stripe-level predicate pushdown
-- ============================================================

-- | Iterator over stripes that drops any stripe whose
-- file-footer 'ColumnStatistics' prove the predicate matches
-- no rows. ORC stores per-file column statistics in the footer
-- (one entry per leaf column, /not/ per-stripe); the read API
-- has to reconstruct per-stripe stats from the protobuf
-- @StripeStatistics@ payloads in @Metadata@. For now this
-- shape uses the file-level stats — accurate when the file is
-- a single stripe (the common Iceberg case) and conservatively
-- safe (PMaybeKeep) for multi-stripe files where per-stripe
-- stats would be tighter.
--
-- Returns @(totalStripes, droppedStripes, iter)@ so callers
-- can log the skip ratio.
streamStripesFilteredIter
  :: AT.Schema
  -> OStats.Predicate
  -> ByteString
  -> OT.ORCFooter
  -> (Int, Int, IS.Iter (V.Vector AC.ColumnArray))
streamStripesFilteredIter sch predicate fileBs footer =
  let !leafNames = leafColumnNames sch
      !stats     = OT.orcStatistics footer
      !allDecide =
        -- File-level decision applies to every stripe (we
        -- don't yet read the per-stripe Metadata payload).
        OStats.evalStripe leafNames stats predicate
      !nStripes  = numStripes footer
      keep _ = allDecide == OStats.PMaybeKeep
      !kept   = V.filter keep (V.enumFromN 0 nStripes)
      !nKept  = V.length kept
      !nSkip  = nStripes - nKept
      step k =
        let !i = V.unsafeIndex kept k
        in orcStripeToArrow sch fileBs footer i
  in (nStripes, nSkip, IS.iterFromIndexed nKept step)

-- | Combination of 'streamStripesProjectedIter' and
-- 'streamStripesFilteredIter': only decodes the named columns
-- of stripes that survive the predicate.
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

-- | Leaf column names for an Arrow schema (used as the column
-- name vector by the predicate evaluator).
leafColumnNames :: AT.Schema -> V.Vector Text
leafColumnNames sch =
  V.map AT.fieldName (AT.arrowFields sch)
