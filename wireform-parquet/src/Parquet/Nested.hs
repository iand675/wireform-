{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

{- | Writer-side support for arbitrary nested Parquet column trees:
structs, lists (3-level encoding), maps, and any composition of
those with primitive leaves. The writer walks a row-major tree and
emits Dremel-style @(repetition, definition, value)@ events per
leaf, ready to drop into a DATA_PAGE_V2.

The Iceberg spec uses the standard parquet-format encoding for
each compound type:

* @list<T>@ → @optional group <name> (LIST) { repeated group list
  { optional <T> element; } }@.
* @map<K, V>@ → @optional group <name> (MAP) { repeated group
  key_value { required <K> key; optional <V> value; } }@.
* @struct<...>@ → an unannotated group whose children are fields.

We model the row-major input as 'NestedRow' (a self-similar tree
of @Maybe@ / list / struct / map / leaf), build the schema with
'NestedSchema' / 'mkSchema', and run 'shred' to flatten one row at
a time. The result is one 'NestedLeaf' per primitive leaf in the
schema in left-to-right order, exactly matching what
DATA_PAGE_V2 expects.

The sibling 'encodeOptionalListOptionalI32' / friends shipped in
the previous PR are now thin wrappers that build a 'NestedSchema',
a 'NestedRow' vector, and call 'shred'.
-}
module Parquet.Nested (
  -- * Schema
  NestedSchema (..),
  LeafType (..),
  LeafDescriptor (..),

  -- * Row-major data
  NestedRow (..),
  LeafValue (..),

  -- * Shredding
  NestedLeaf (..),
  shred,

  -- * Schema flattening
  flattenSchema,
  nestedSchemaToFlatSchema,

  -- * Whole-file builder
  buildNestedFile,
  buildNestedFileWithAux,
  buildNestedFileWithBloomFilters,
  buildNestedFileWithBloomFiltersUsing,

  -- * Convenience for the optional-list-of-optional-primitive case

  --   (the previous PR's Iceberg-default-value shape)
  encodeOptionalListOptionalI32,
  encodeOptionalListOptionalI64,
  encodeOptionalListOptionalDouble,
  optionalListSchemaSegments,
  buildOptionalListFile,
  buildOptionalListFileEither,
) where

import Data.Bits (setBit, shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Unsafe qualified as BSU
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Text.Foreign qualified as TF
import Data.Vector qualified as V
import Data.Vector.Primitive qualified as VP
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Exts (Int (I#), Int#, Word64#, (+#), (<#), (==#), (>#))
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import GHC.Word (Word64 (W64#))
import Parquet.Compress qualified as Compress
import Parquet.Nested.Fast qualified as Fast
import Parquet.File qualified as F
import Parquet.Footer qualified as PF
import Parquet.LevelsEncode qualified as LE
import Parquet.Page qualified as PP
import Parquet.Types (
  ColumnChunk (..),
  ColumnMetadata (..),
  Compression (..),
  ConvertedType (..),
  Encoding (..),
  FileMetadata (..),
  LogicalType (..),
  LtTimeUnit (..),
  ParquetType (..),
  Repetition (..),
  RowGroup (..),
  SchemaElement (..),
  Statistics (..),
  ColumnOrder (..),
  hasCompatiblePhysicalType,
 )
import Parquet.Variant qualified as Variant
import Parquet.Write qualified as PW
import Parquet.Nested.Bloom qualified as NestedBloom
import Parquet.Nested.Statistics qualified as NestedStats
import Parquet.Nested.Types
import Parquet.Variant.Nested qualified as VariantNested
import Wireform.Builder qualified as B


-- ============================================================
-- Schema description
-- ============================================================

leafParquetType :: LeafType -> ParquetType
leafParquetType = \case
  LtInt32 -> PTInt32
  LtInt64 -> PTInt64
  LtFloat16 -> PTFixedLenByteArray
  LtFloat -> PTFloat
  LtDouble -> PTDouble
  LtBool -> PTBoolean
  LtString -> PTByteArray
  LtJson -> PTByteArray
  LtBinary -> PTByteArray
  LtInteger 64 _ -> PTInt64
  LtInteger _ _ -> PTInt32
  LtDate -> PTInt32
  LtTime _ LtMillis -> PTInt32
  LtTime _ _ -> PTInt64
  LtTimestamp {} -> PTInt64
  LtTimestamp96 {} -> PTFixedLenByteArray
  LtInt96 -> PTInt96
  LtDecimal precision _
    | precision <= 9 -> PTInt32
    | precision <= 18 -> PTInt64
    | otherwise -> PTFixedLenByteArray
  LtFixedBinary {} -> PTFixedLenByteArray
  LtUuid -> PTFixedLenByteArray


leafConvertedType :: LeafType -> Maybe ConvertedType
leafConvertedType = \case
  LtString -> Just CTUtf8
  LtJson -> Just CTJson
  LtDecimal {} -> Just CTDecimal
  LtInteger 8 True -> Just CTInt8
  LtInteger 16 True -> Just CTInt16
  LtInteger 32 True -> Just CTInt32
  LtInteger 64 True -> Just CTInt64
  LtInteger 8 False -> Just CTUInt8
  LtInteger 16 False -> Just CTUInt16
  LtInteger 32 False -> Just CTUInt32
  LtInteger 64 False -> Just CTUInt64
  LtDate -> Just CTDate
  LtTime _ LtMillis -> Just CTTimeMillis
  LtTime _ LtMicros -> Just CTTimeMicros
  LtTimestamp _ LtMillis -> Just CTTimestampMillis
  LtTimestamp _ LtMicros -> Just CTTimestampMicros
  _ -> Nothing


leafLogicalType :: LeafType -> Maybe LogicalType
leafLogicalType = \case
  LtFloat16 -> Just LTFloat16
  LtString -> Just LTString
  LtJson -> Just LTJson
  LtInteger width signed -> Just (LTInteger width signed)
  LtDate -> Just LTDate
  LtTime adjusted unit -> Just (LTTime adjusted unit)
  LtTimestamp adjusted unit -> Just (LTTimestamp adjusted unit)
  LtTimestamp96 adjusted unit -> Just (LTTimestamp adjusted unit)
  LtDecimal precision scale -> Just (LTDecimal precision scale)
  LtUuid -> Just LTUUID
  _ -> Nothing


leafSchemaElement :: Text -> Maybe Repetition -> LeafType -> SchemaElement
leafSchemaElement name repetition typ =
  SchemaElement
    { seTypeLength = case typ of
        LtFloat16 -> Just 2
        LtFixedBinary width -> Just width
        LtTimestamp96 {} -> Just 12
        LtUuid -> Just 16
        LtDecimal precision _ | precision > 18 -> Just (if precision <= 38 then 16 else 32)
        _ -> Nothing
    , seName = name
    , seRepetition = repetition
    , seType = Just (leafParquetType typ)
    , seNumChildren = Nothing
    , seConvertedType = leafConvertedType typ
    , seLogicalType = leafLogicalType typ
    , seFieldId = Nothing
    }


-- ============================================================
-- Shredded leaf
-- ============================================================

-- | One primitive leaf after Dremel shredding.
data NestedLeaf = NestedLeaf
  { nlPath :: !(V.Vector Text)
  , nlMaxDef :: !Int
  , nlMaxRep :: !Int
  , nlDefLevels :: !(VP.Vector Int32)
  , nlRepLevels :: !(VP.Vector Int32)
  , nlValueBytes :: !ByteString
  , nlValueCount :: !Int
  }
  deriving (Show, Eq)


-- ============================================================
-- Schema flattening + max levels
-- ============================================================

{- | Walk a 'NestedSchema' producing one entry per primitive leaf, in
the same order Parquet's flat schema expects. Each entry carries
the leaf's path, its physical type, and the Dremel max definition
and repetition levels at the leaf.
-}
flattenSchema :: Text -> NestedSchema -> V.Vector (LeafDescriptor)
flattenSchema rootName ns = V.fromList (go (V.singleton rootName) 0 0 ns)
  where
    -- Walk one node, accumulating 'curDef' / 'curRep' as we descend.
    go path !d !r = \case
      NSPrimitive lt ->
        [LeafDescriptor path lt d r]
      NSOptional inner ->
        go path (d + 1) r inner
      NSRequired inner ->
        go path d r inner
      NSList inner ->
        let path' = V.snoc (V.snoc path "list") "element"
        in -- 'list' is a repeated group, so descending into it
           -- bumps both d and r.
           go path' (d + 1) (r + 1) inner
      NSMap k v ->
        let !pkv = V.snoc path "key_value"
        in go (V.snoc pkv "key") (d + 1) (r + 1) k
             ++ go (V.snoc pkv "value") (d + 1) (r + 1) v
      NSStruct children ->
        concatMap
          (\(name, child) -> go (V.snoc path name) d r child)
          (V.toList children)
      NSFile fields ->
        V.foldr
          (\spec rest -> LeafDescriptor (V.snoc path (F.fileFieldName (F.fileField spec))) (fileLeafType (F.fileField spec)) (d + 1) r : rest)
          []
          (F.fileSchemaFields fields)
      NSVariant ->
        -- Variant is a 2-leaf binary group. Both leaves are
        -- @required binary@ so descending into them doesn't bump d
        -- or r; the group's own repetition is set by an enclosing
        -- NSOptional / NSRequired (typical Iceberg usage:
        -- @optional NSVariant@).
        [ LeafDescriptor (V.snoc path "metadata") LtBinary d r
        , LeafDescriptor (V.snoc path "value") LtBinary d r
        ]
      NSVariantWithLimits _ -> go path d r NSVariant
      NSVariantTyped storage -> go path d r (storageLayout storage)


{- | Per-leaf metadata used both by the shredder and by
'buildOptionalListFile' to populate the column chunks.
-}
data LeafDescriptor = LeafDescriptor
  { ldPath :: !(V.Vector Text)
  , ldType :: !LeafType
  , ldMaxDef :: !Int
  , ldMaxRep :: !Int
  }
  deriving (Show, Eq)




-- ============================================================
-- Shredding
-- ============================================================

{- | Shred a vector of row-major records into one 'NestedLeaf' per
primitive leaf in @schema@. Length of 'nlDefLevels' / 'nlRepLevels'
on each leaf is the same, equal to the total number of value
positions (one per "leaf slot" expanded across all rows).

Each row is one 'NestedRow' that must structurally match @schema@:

* 'NRLeaf' under 'NSPrimitive'.
* 'NRList' under 'NSList'.
* 'NRStruct' under 'NSStruct' with children in the same order.
* 'NRMapEntries' under 'NSMap'.
* 'NRNull' under any 'NSOptional' wrapper.

A type-mismatch yields 'Left'.
-}
shred
  :: NestedSchema
  -> V.Vector NestedRow
  -> Either String (V.Vector NestedLeaf)
shred schema rows = do
  planned <- planShred schema rows
  validateVariantRows schema rows
  pure (V.map materializeShreddedLeaf planned)


-- All leaf sizes and level streams are planned before any value builder is
-- run. Keeping the plan private also lets the whole-file writer reuse it.
data ShreddedLeafPlan = ShreddedLeafPlan
  { slpDescriptor :: !LeafDescriptor
  , slpDefinitions :: !(VP.Vector Int32)
  , slpRepetitions :: !(VP.Vector Int32)
  , slpValues :: !ShreddedValues
  , slpPage :: !NestedPagePlan
  }


-- Materialize a leaf only when an operation needs its PLAIN bytes. A bloom
-- filter and V2 compression share that buffer; ordinary uncompressed writes
-- retain the direct builder path.
data ShreddedValues
  = PlannedValues !B.Builder
  | MaterializedValues !ByteString
  | DeferredValues {-# UNPACK #-} !Int ~ByteString


shreddedValueBuilder :: ShreddedValues -> B.Builder
shreddedValueBuilder (PlannedValues builder) = builder
shreddedValueBuilder (MaterializedValues bytes) = B.byteString bytes
shreddedValueBuilder (DeferredValues _ bytes) = B.byteString bytes


shreddedValueBytes :: ShreddedValues -> ByteString
shreddedValueBytes (PlannedValues builder) = BL.toStrict (B.toLazyByteString builder)
shreddedValueBytes (MaterializedValues bytes) = bytes
shreddedValueBytes (DeferredValues _ bytes) = bytes


planShred :: NestedSchema -> V.Vector NestedRow -> Either String (V.Vector ShreddedLeafPlan)
planShred schema rows = do
  _ <- nestedPageCount "row count" (V.length rows)
  validateNestedSchema schema
  if V.null rows then Right () else validateObservableShape schema
  let !leaves = flattenSchema "" schema
      !nLeaves = V.length leaves
      seeds = V.replicate nLeaves emptyAcc
  V.mapM_
    ( \leaf ->
        if hasCompatiblePhysicalType (leafSchemaElement "" Nothing (ldType leaf))
          then Right ()
          else Left "Parquet.Nested: invalid logical leaf type"
    )
    leaves
  case Fast.shredFast schema rows of
    Just result -> result >>= V.imapM (\index leaf -> finalizeFast (V.unsafeIndex leaves index) leaf)
    Nothing -> do
      finalAcc <- V.foldM' (\acc row -> shredRow schema acc row 0 0) seeds rows
      V.imapM (\i ld -> finalize ld (V.unsafeIndex finalAcc i)) leaves
  where
    finalizeFast ld leaf = do
      let definitions = Fast.fastDefinitions leaf
          repetitions = Fast.fastRepetitions leaf
          payload = Fast.fastPayload leaf
          input = NestedPageInput (V.length rows) (ldMaxDef ld) (ldMaxRep ld) definitions repetitions (ShreddedCounts (Fast.fastValueCount leaf)) (Fast.fastPayloadSize leaf)
      page <- planNestedPageInput input
      Right (ShreddedLeafPlan ld definitions repetitions (DeferredValues (Fast.fastPayloadSize leaf) payload) page)
    finalize ld acc = do
      let definitions = VP.fromList (reverse (accDefs acc))
          repetitions = VP.fromList (reverse (accReps acc))
          input = NestedPageInput (V.length rows) (ldMaxDef ld) (ldMaxRep ld) definitions repetitions (CheckLevelCounts Nothing) (accPayloadSize acc)
      page <- planNestedPageInput input
      Right (ShreddedLeafPlan ld definitions repetitions (PlannedValues (finishValues acc)) page)


materializeShreddedLeaf :: ShreddedLeafPlan -> NestedLeaf
materializeShreddedLeaf prepared =
  let descriptor = slpDescriptor prepared
  in NestedLeaf
       { nlPath = ldPath descriptor
       , nlMaxDef = ldMaxDef descriptor
       , nlMaxRep = ldMaxRep descriptor
       , nlDefLevels = slpDefinitions prepared
       , nlRepLevels = slpRepetitions prepared
       , nlValueBytes = shreddedValueBytes (slpValues prepared)
       , nlValueCount = nppValueCount (slpPage prepared)
       }


-- Repetition wrappers describe one physical node, not additional nodes.
-- The schema emitter can store only one repetition value there; accepting
-- adjacent wrappers would let the shredder count levels absent from the file.
validateNestedSchema :: NestedSchema -> Either String ()
validateNestedSchema = go
  where
    go node = case node of
      NSPrimitive (LtDecimal precision scale)
        | precision < 1 || precision > 76 || scale < 0 || scale > precision -> Left "Parquet.Nested: DECIMAL requires precision1..76 and scale0..precision"
      NSPrimitive (LtFixedBinary width)
        | width <= 0 -> Left "Parquet.Nested: fixed-byte width must be positive"
      NSOptional child -> unwrapped child
      NSRequired child -> unwrapped child
      NSList child -> go child
      NSMap key value -> do
        case key of
          NSOptional _ -> Left "Parquet.Nested: MAP keys must be required"
          _ -> Right ()
        go key
        go value
      NSStruct fields
        | V.null fields -> Left "Parquet.Nested: empty structs have no physical leaf"
        | otherwise -> V.mapM_ (go . snd) fields
      NSVariantWithLimits limits
        | Variant.variantMaxDepth limits < 0 || Variant.variantMaxValues limits <= 0 || Variant.variantMaxDictionaryEntries limits < 0 ->
            Left "Parquet.Nested: invalid Variant resource limits"
      _ -> Right ()
    unwrapped child = case child of
      NSOptional _ -> adjacent
      NSRequired _ -> adjacent
      _ -> go child
    adjacent = Left "Parquet.Nested: adjacent Optional / Required wrappers describe the same physical node"


-- A zero-field FILE can only contain null references. It needs no levels of
-- its own, but other optional/repeated containers still need a physical leaf
-- to encode their presence and multiplicity. Reject unobservable shapes
-- instead of emitting a file that silently loses those distinctions.
validateObservableShape :: NestedSchema -> Either String ()
validateObservableShape schema = case schema of
  NSOptional child
    | leafCount child == 0 -> case child of
        NSFile _ -> Right ()
        _ -> missing
    | otherwise -> validateObservableShape child
  NSRequired child -> validateObservableShape child
  NSList child
    | leafCount child == 0 -> missing
    | otherwise -> validateObservableShape child
  NSMap key value
    | leafCount key + leafCount value == 0 -> missing
    | otherwise -> validateObservableShape key >> validateObservableShape value
  NSStruct fields -> V.mapM_ (validateObservableShape . snd) fields
  NSVariantTyped storage -> validateObservableShape (storageLayout storage)
  _ -> Right ()
  where
    missing = Left "Parquet.Nested: optional/repeated container has no physical leaf to encode its shape"


{- | Per-leaf accumulator. We push events as we walk each row
recursively; finalising reverses the def / rep lists into the
'VP.Vector's the writer expects.
-}
data LeafAcc
  = LeafAcc
      { accDefs :: ![Int32]
      -- ^ reversed
      , accReps :: ![Int32]
      -- ^ reversed
      , accValuesB :: !B.Builder
      , -- Low 32 bits: event count. High 32 bits: planned value bytes.
        -- Both are bounded by signed i32, so they fit without adding a field
        -- to every per-event accumulator. Non-null count comes from levels.
        accPageCounts :: {-# UNPACK #-} !Word64
      }
  | BooleanLeafAcc
      -- Only boolean leaves carry a partial byte plus its bit position.
      -- Nulls advance the event count but leave this state untouched.
      { accDefs :: ![Int32]
      , accReps :: ![Int32]
      , accValuesB :: !B.Builder
      , accPageCounts :: {-# UNPACK #-} !Word64
      , accBooleanState :: {-# UNPACK #-} !Word16
      }


emptyAcc :: LeafAcc
emptyAcc = LeafAcc [] [] mempty 0


finishValues :: LeafAcc -> B.Builder
finishValues acc = case acc of
  BooleanLeafAcc {accBooleanState = state} | state `shiftR` 8 /= 0 -> accValuesB acc <> B.word8 (fromIntegral state)
  _ -> accValuesB acc


accPayloadSize :: LeafAcc -> Int
accPayloadSize acc = fromIntegral (accPageCounts acc `shiftR` 32)
{-# INLINE accPayloadSize #-}


booleanState :: LeafAcc -> Word16
booleanState acc = case acc of
  BooleanLeafAcc {accBooleanState = state} -> state
  _ -> 0
{-# INLINE booleanState #-}


{- | Shred one row recursively. The arguments are the schema we're
still descending through, the per-leaf accumulators (one slot per
leaf in the *original* schema, indexed left-to-right), the row
itself, and the current (def, rep) levels.

Critical invariant: the order in which leaves get appended must
match the schema's flatten order, which 'descendThrough' preserves
by walking the same way 'flattenSchema' does.
-}
shredRow
  :: NestedSchema
  -> V.Vector LeafAcc
  -> NestedRow
  -> Int
  -- ^ current def
  -> Int
  -- ^ current rep
  -> Either String (V.Vector LeafAcc)
shredRow schema accs row d r = do
  (newAccs, _) <- descendThrough schema 0 accs row d r 0
  Right newAccs


{- | Descend one schema node into one row value.

Arguments:

  * @schema@: the (sub)schema to descend.
  * @cursor@: the leaf-index to write the next event to.
  * @accs@: per-leaf event accumulators.
  * @row@: the row value being shredded against @schema@.
  * @d@: current definition level.
  * @r@: rep level to emit for the very next leaf event the recursion
         produces (i.e. the rep level for the FIRST element of any
         repeated group inside @schema@). Subsequent elements of any
         repeated group within @schema@ use the absolute rep depth.
  * @absRep@: total number of repeated groups crossed so far, which
         determines the rep level for "subsequent siblings" inside
         a repeated group boundary.
-}
descendThrough
  :: NestedSchema
  -> Int
  -- ^ leafCursor
  -> V.Vector LeafAcc
  -> NestedRow
  -> Int
  -- ^ current def
  -> Int
  -- ^ rep level for the next leaf event
  -> Int
  -- ^ absolute rep depth
  -> Either String (V.Vector LeafAcc, Int)
descendThrough schema !cursor !accs row !d !r !absRep = case (schema, row) of
  -- An NSOptional node either consumes NRNull (which emits a null
  -- event at the *parent's* def for every leaf in the subtree -
  -- this Optional layer wasn't crossed) or unwraps the inner row,
  -- bumping def by 1 to reflect that the layer was crossed.
  (NSOptional inner, NRNull) ->
    case emitNullsAcrossSubtree inner cursor accs d r of
      (# message | #) -> Left message
      (# | next #) -> Right (next, cursor + leafCount inner)
  (NSOptional inner, other) ->
    descendThrough inner cursor accs other (d + 1) r absRep
  (NSRequired inner, other) ->
    descendThrough inner cursor accs other d r absRep
  -- NRNull at a required-or-list node is an error; the schema doesn't
  -- allow nulls there.
  (NSPrimitive _, NRNull) ->
    Left "Parquet.Nested: NRNull at required NSPrimitive"
  (NSList _, NRNull) ->
    Left "Parquet.Nested: NRNull at required NSList (wrap with NSOptional)"
  (NSMap _ _, NRNull) ->
    Left "Parquet.Nested: NRNull at required NSMap (wrap with NSOptional)"
  (NSStruct _, NRNull) ->
    Left "Parquet.Nested: NRNull at required NSStruct (wrap with NSOptional)"
  (NSPrimitive typ, NRLeaf v)
    | not (leafValueMatches typ v) -> Left "Parquet.Nested: leaf value violates its physical or logical type"
    | otherwise ->
        case pushLeafChecked# (V.unsafeIndex accs cursor) d r v of
          (# message | #) -> Left message
          (# | next #) -> Right (V.unsafeUpd accs [(cursor, next)], cursor + 1)
  (NSPrimitive _, _) ->
    Left "Parquet.Nested: row shape doesn't match NSPrimitive"
  -- NRList under NSList: empty -> emit one "empty list defined"
  -- event per leaf in the subtree (def stays at d, the LIST itself
  -- is defined; the contents aren't). Non-empty -> recurse, with
  -- the FIRST element using the parent's r and subsequent ones
  -- bumping r by 1 (we've crossed a repeated group).
  (NSList inner, NRList children)
    | 1# <- nestedCountExceeded# (V.length children) -> Left "Parquet.Nested: list length exceeds signed 32-bit page limit"
    | V.null children ->
        case emitNullsAcrossSubtree inner cursor accs d r of
          (# message | #) -> Left message
          (# | next #) -> Right (next, cursor + leafCount inner)
    | otherwise ->
        -- We're crossing this list's repeated group. The first
        -- element keeps the caller's rep level (we haven't
        -- "started a new sibling at this depth" - we're just
        -- descending into the first one). Subsequent elements use
        -- the absolute rep depth for /this/ list = absRep + 1.
        let !thisAbsRep = absRep + 1
            nKids = V.length children
            stepKid (accs', _cur) i =
              let !kid = V.unsafeIndex children i
                  !rEvent = if i == 0 then r else thisAbsRep
              in descendThrough inner cursor accs' kid (d + 1) rEvent thisAbsRep
        in case foldM stepKid (accs, cursor) [0 .. nKids - 1] of
             Right (accs', _) -> Right (accs', cursor + leafCount inner)
             Left e -> Left e
  (NSList _, _) -> Left "Parquet.Nested: row shape doesn't match NSList"
  (NSMap k v, NRMapEntries pairs)
    | 1# <- nestedCountExceeded# (V.length pairs) -> Left "Parquet.Nested: map length exceeds signed 32-bit page limit"
    | V.null pairs ->
        case emitNullsAcrossSubtree
          (NSStruct (V.fromList [("key", k), ("value", v)]))
          cursor
          accs
          d
          r of
          (# message | #) -> Left message
          (# | next #) -> Right (next, cursor + leafCount k + leafCount v)
    | otherwise ->
        let !thisAbsRep = absRep + 1
            nPairs = V.length pairs
            stepPair (accs', _cur) i =
              let (kRow, vRow) = V.unsafeIndex pairs i
                  !rEvent = if i == 0 then r else thisAbsRep
              in do
                   (a1, c1) <- descendThrough k cursor accs' kRow (d + 1) rEvent thisAbsRep
                   (a2, c2) <- descendThrough v c1 a1 vRow (d + 1) rEvent thisAbsRep
                   Right (a2, c2)
        in case foldM stepPair (accs, cursor) [0 .. nPairs - 1] of
             Right (accs', _) -> Right (accs', cursor + leafCount k + leafCount v)
             Left e -> Left e
  (NSMap _ _, _) -> Left "Parquet.Nested: row shape doesn't match NSMap"
  (NSVariantTyped storage, input) -> descendThrough (storageLayout storage) cursor accs (VariantNested.normalizeStorageRow storage input) d r absRep
  (NSFile fields, NRFile reference) -> do
    F.validateFileReferenceForSchema fields reference
    next <- V.ifoldM (pushFileField reference) accs (F.fileSchemaFields fields)
    Right (next, cursor + V.length (F.fileSchemaFields fields))
    where
      pushFileField reference' accumulators index spec =
        let !position = cursor + index
            !accumulator = V.unsafeIndex accumulators position
            value = case F.fileField spec of
              F.FileUri -> LvString <$> F.fileUri reference'
              F.FileOffset -> LvInt64 <$> F.fileOffset reference'
              F.FileSize -> LvInt64 <$> F.fileSize reference'
              F.FileContentType -> LvString <$> F.fileContentType reference'
              F.FileChecksumField -> LvString <$> F.fileChecksum reference'
              F.FileInline -> LvBinary <$> F.fileInline reference'
        in case value of
             Nothing -> case emitNullsAcrossSubtree (NSPrimitive (fileLeafType (F.fileField spec))) position accumulators d r of
               (# message | #) -> Left message
               (# | result #) -> Right result
             Just present -> case pushLeafChecked# accumulator (d + 1) r present of
               (# message | #) -> Left message
               (# | result #) -> Right (V.unsafeUpd accumulators [(position, result)])
  (NSFile _, _) -> Left "Parquet.Nested: row shape doesn't match NSFile (expected NRFile)"
  (variant, NRVariantBytes metaBs valBs) | Just _ <- variantLimits variant -> do
    -- Push two binary leaves: the metadata and value byte-strings.
    -- Both are 'required binary' so they emit at the current
    -- definition level. Calling code typically wraps NSVariant in
    -- NSOptional, in which case the row is either NRNull (handled
    -- above) or NRVariantBytes for a present Variant.
    -- Payload interpretation runs after every leaf's byte/count preflight,
    -- so rejected pages do not repeatedly decode large valid dictionaries.
    case pushLeafChecked# (V.unsafeIndex accs cursor) d r (LvBinary metaBs) of
      (# message | #) -> Left message
      (# | metadata #) ->
        case pushLeafChecked# (V.unsafeIndex accs (cursor + 1)) d r (LvBinary valBs) of
          (# message | #) -> Left message
          (# | value #) -> Right (V.unsafeUpd accs [(cursor, metadata), (cursor + 1, value)], cursor + 2)
  (NSVariant, _) ->
    Left "Parquet.Nested: row shape doesn't match NSVariant (expected NRVariantBytes or NRNull)"
  (NSVariantWithLimits _, _) ->
    Left "Parquet.Nested: row shape doesn't match NSVariantWithLimits (expected NRVariantBytes or NRNull)"
  (NSStruct fields, NRStruct values)
    | V.length fields /= V.length values ->
        Left "Parquet.Nested: NSStruct / NRStruct field count mismatch"
    | otherwise ->
        let nFs = V.length fields
            stepField (accs', cur) i =
              let (_, child) = V.unsafeIndex fields i
                  val = V.unsafeIndex values i
              in descendThrough child cur accs' val d r absRep
        in case foldM stepField (accs, cursor) [0 .. nFs - 1] of
             Right (accs', cur') -> Right (accs', cur')
             Left e -> Left e
  (NSStruct _, _) -> Left "Parquet.Nested: row shape doesn't match NSStruct"


variantLimits :: NestedSchema -> Maybe Variant.VariantLimits
variantLimits NSVariant = Just Variant.defaultVariantLimits
variantLimits (NSVariantWithLimits limits) = Just limits
variantLimits _ = Nothing
{-# INLINE variantLimits #-}


-- Compile only the Variant-bearing portions of a schema. No extra per-row
-- traversal is needed for ordinary columns; nested checks visit selected
-- children without creating intermediate row vectors.
data VariantCheck
  = CheckVariant !Variant.VariantLimits
  | CheckTypedVariant !VariantStorageSchema
  | CheckVariantStruct !(V.Vector VariantCheckChild)
  | CheckVariantList !VariantCheck
  | CheckVariantMap !(Maybe VariantCheck) !(Maybe VariantCheck)


data VariantCheckChild = VariantCheckChild
  { variantChildIndex :: {-# UNPACK #-} !Int
  , variantChildCheck :: !VariantCheck
  }


variantCheck :: NestedSchema -> Maybe VariantCheck
variantCheck schema = case schema of
  NSVariant -> Just (CheckVariant Variant.defaultVariantLimits)
  NSVariantWithLimits limits -> Just (CheckVariant limits)
  NSVariantTyped storage -> Just (CheckTypedVariant storage)
  NSOptional child -> variantCheck child
  NSRequired child -> variantCheck child
  NSList child -> CheckVariantList <$> variantCheck child
  NSMap key value -> case (variantCheck key, variantCheck value) of
    (Nothing, Nothing) -> Nothing
    (keyCheck, valueCheck) -> Just (CheckVariantMap keyCheck valueCheck)
  NSStruct fields ->
    let checks = V.imapMaybe (\index (_, child) -> VariantCheckChild index <$> variantCheck child) fields
    in if V.null checks then Nothing else Just (CheckVariantStruct checks)
  _ -> Nothing


validateVariantRows :: NestedSchema -> V.Vector NestedRow -> Either String ()
validateVariantRows schema rows = case variantCheck schema of
  Nothing -> Right ()
  Just plan -> V.mapM_ (validateVariantRow plan) rows


validateVariantRow :: VariantCheck -> NestedRow -> Either String ()
validateVariantRow _ NRNull = Right ()
validateVariantRow plan row = case (plan, row) of
  (CheckTypedVariant storage, value) -> VariantNested.validateStorageRow storage value
  (CheckVariant limits, NRVariantBytes metadata value) ->
    () <$ Variant.decodeVariantWithLimits limits metadata value
  (CheckVariantStruct children, NRStruct values) ->
    V.mapM_ (\child -> validateVariantRow (variantChildCheck child) (V.unsafeIndex values (variantChildIndex child))) children
  (CheckVariantList child, NRList values) -> V.mapM_ (validateVariantRow child) values
  (CheckVariantMap keyPlan valuePlan, NRMapEntries pairs) ->
    V.mapM_
      ( \(key, value) -> do
          maybe (Right ()) (`validateVariantRow` key) keyPlan
          maybe (Right ()) (`validateVariantRow` value) valuePlan
      )
      pairs
  _ -> Left "Parquet.Nested: Variant validation plan disagrees with preflight row shape"


-- | Number of primitive leaves under a schema node.
leafCount :: NestedSchema -> Int
leafCount = \case
  NSPrimitive _ -> 1
  NSOptional s -> leafCount s
  NSRequired s -> leafCount s
  NSList s -> leafCount s
  NSMap k v -> leafCount k + leafCount v
  NSStruct fields -> sum (map (leafCount . snd) (V.toList fields))
  NSFile fields -> V.length (F.fileSchemaFields fields)
  NSVariant -> 2
  NSVariantWithLimits _ -> 2
  NSVariantTyped storage -> leafCount (storageLayout storage)


fileLeafType :: F.FileField -> LeafType
fileLeafType = \case
  F.FileOffset -> LtInt64
  F.FileSize -> LtInt64
  F.FileInline -> LtBinary
  _ -> LtString
{-# INLINE fileLeafType #-}


{- | Push one (def, rep) event for /every/ leaf in a subtree, with
no value. Used for null-list and null-optional events.
-}
emitNullsAcrossSubtree
  :: NestedSchema
  -> Int
  -- ^ leafCursor
  -> V.Vector LeafAcc
  -> Int
  -> Int
  -- ^ def, rep
  -> (# String | V.Vector LeafAcc #)
emitNullsAcrossSubtree sch !cur !accs !d !r =
  let !nLeaves = leafCount sch
      indices = [cur .. cur + nLeaves - 1]
      updates =
        map
          ( \i ->
              let acc = V.unsafeIndex accs i
                  !acc' = pushNull acc d r
              in (i, acc')
          )
          indices
  in case cur of
       I# start -> case cur + nLeaves of
         I# end -> case nullRangeFits# start end accs of
           0# -> (# "Parquet.Nested: event count exceeds signed 32-bit page limit" | #)
           _ -> let !next = V.unsafeUpd accs updates in (# | next #)


nestedCountExceeded# :: Int -> Int#
nestedCountExceeded# (I# count) = case count <# 0# of
  1# -> 1#
  _ -> count ># 2147483647#
{-# INLINE nestedCountExceeded# #-}


nullRangeFits# :: Int# -> Int# -> V.Vector LeafAcc -> Int#
nullRangeFits# index end accs = case index ==# end of
  1# -> 1#
  _ -> case fromIntegral (accPageCounts (V.unsafeIndex accs (I# index)) .&. 0xffffffff) :: Int of
    I# count -> case count <# 2147483647# of
      1# -> nullRangeFits# (index +# 1#) end accs
      _ -> 0#


advancePageCounts# :: LeafAcc -> Int# -> (# String | Word64# #)
advancePageCounts# acc bytes# =
  let !events = fromIntegral (accPageCounts acc .&. 0xffffffff) :: Int
      !bytes = accPayloadSize acc
      !added = I# bytes#
  in if events == 2147483647
       then (# "Parquet.Nested: event count exceeds signed 32-bit page limit" | #)
       else
         if added < 0 || added > 2147483647 - bytes
           then (# "Parquet.Nested: value payload size exceeds signed 32-bit page limit" | #)
           else case (fromIntegral (bytes + added) `shiftL` 32) .|. fromIntegral (events + 1) :: Word64 of
             W64# counts -> (# | counts #)
{-# INLINE advancePageCounts# #-}


leafValueSize :: LeafAcc -> LeafValue -> Int
leafValueSize acc value = case value of
  LvInt32 {} -> 4
  LvInt64 {} -> 8
  LvFloat16 {} -> 2
  LvFloat {} -> 4
  LvDouble {} -> 8
  LvBool {} -> if booleanState acc `shiftR` 8 == 0 then 1 else 0
  LvString text -> prefixedSize (TF.lengthWord8 text)
  LvBinary bytes -> prefixedSize (BS.length bytes)
  LvFixedBinary bytes -> if BS.length bytes > 2147483647 then -1 else BS.length bytes
  where
    prefixedSize size
      | size < 0 || size > 2147483643 = -1
      | otherwise = size + 4
{-# INLINE leafValueSize #-}


pushLeafChecked# :: LeafAcc -> Int -> Int -> LeafValue -> (# String | LeafAcc #)
pushLeafChecked# acc d r value = case leafValueSize acc value of
  I# bytes -> case advancePageCounts# acc bytes of
    (# message | #) -> (# message | #)
    (# | counts #) -> let !next = pushLeaf acc counts d r value in (# | next #)
{-# INLINE pushLeafChecked# #-}


pushLeaf :: LeafAcc -> Word64# -> Int -> Int -> LeafValue -> LeafAcc
pushLeaf acc counts d r v =
  let !defs = fromIntegral d : accDefs acc
      !reps = fromIntegral r : accReps acc
  in case v of
       LvBool bit ->
         let !state = booleanState acc
             !position = fromIntegral (state `shiftR` 8)
             !pending = fromIntegral state :: Word8
             !byte = if bit then setBit pending position else pending
         in if position == 7
              then BooleanLeafAcc defs reps (accValuesB acc <> B.word8 byte) (W64# counts) 0
              else BooleanLeafAcc defs reps (accValuesB acc) (W64# counts) (fromIntegral byte .|. (fromIntegral (position + 1) `shiftL` 8))
       _ -> LeafAcc defs reps (accValuesB acc <> encodeLeafValue v) (W64# counts)


pushNull :: LeafAcc -> Int -> Int -> LeafAcc
pushNull acc d r =
  acc
    { accDefs = fromIntegral d : accDefs acc
    , accReps = fromIntegral r : accReps acc
    , accPageCounts = accPageCounts acc + 1
    }
{-# INLINE pushNull #-}


encodeLeafValue :: LeafValue -> B.Builder
encodeLeafValue = \case
  LvInt32 v -> B.int32LE v
  LvInt64 v -> B.int64LE v
  LvFloat16 v -> B.word16LE v
  LvFloat v -> B.word32LE (castFloatToWord32 v)
  LvDouble v -> B.word64LE (castDoubleToWord64 v)
  LvBool b -> B.word8 (if b then 1 else 0)
  -- Defer UTF-8 buffer creation until all page plans have passed. Size
  -- accounting uses text-2.x's O(1) UTF-8 byte length above.
  LvString t -> B.Builder $ \sink state ->
    let !bs = TE.encodeUtf8 t
    in B.unBuilder (B.word32LE (fromIntegral (BS.length bs)) <> B.byteString bs) sink state
  LvBinary bs ->
    B.word32LE (fromIntegral (BS.length bs)) <> B.byteString bs
  LvFixedBinary bs -> B.byteString bs


-- A Data.Foldable.foldM-equivalent specialised to lists; we don't
-- want to drag the Foldable typeclass machinery into the inner
-- shredder loop.
foldM :: (s -> a -> Either String s) -> s -> [a] -> Either String s
foldM _ !s [] = Right s
foldM f !s (x : xs) = case f s x of
  Right s' -> foldM f s' xs
  Left e -> Left e


-- ============================================================
-- Specialised entry points (kept from the previous PR)
-- ============================================================

{- | @optional list&lt;optional INT32&gt;@ shred wrapper (the Iceberg
V3 default-value-array shape). Internally builds a 'NestedSchema'
and a 'V.Vector NestedRow' and calls 'shred'.
-}
encodeOptionalListOptionalI32
  :: Text
  -> V.Vector (Maybe (V.Vector (Maybe Int32)))
  -> NestedLeaf
encodeOptionalListOptionalI32 outerName =
  optListOptPrim outerName LtInt32 (LvInt32 . id)


encodeOptionalListOptionalI64
  :: Text
  -> V.Vector (Maybe (V.Vector (Maybe Int64)))
  -> NestedLeaf
encodeOptionalListOptionalI64 outerName =
  optListOptPrim outerName LtInt64 LvInt64


encodeOptionalListOptionalDouble
  :: Text
  -> V.Vector (Maybe (V.Vector (Maybe Double)))
  -> NestedLeaf
encodeOptionalListOptionalDouble outerName =
  optListOptPrim outerName LtDouble LvDouble


optListOptPrim
  :: Text
  -> LeafType
  -> (a -> LeafValue)
  -> V.Vector (Maybe (V.Vector (Maybe a)))
  -> NestedLeaf
optListOptPrim outerName lt mkLeaf rows =
  let !schema = NSOptional (NSList (NSOptional (NSPrimitive lt)))
      mkRow Nothing = NRNull
      mkRow (Just inner) =
        NRList
          ( V.map
              ( \case
                  Nothing -> NRNull
                  Just v -> NRLeaf (mkLeaf v)
              )
              inner
          )
      !nrows = V.map mkRow rows
  in case shred schema nrows of
       Right ls
         | V.length ls == 1 ->
             let leaf = V.unsafeIndex ls 0
             in leaf {nlPath = V.fromList [outerName, "list", "element"]}
       Right _ -> error "Parquet.Nested: shred produced wrong leaf count"
       Left e -> error ("Parquet.Nested: shred failed: " ++ e)


-- ============================================================
-- Schema helpers (kept from the previous PR)
-- ============================================================

optionalListSchemaSegments
  :: Text
  -> ParquetType
  -> Maybe Int32
  -> Maybe Int32
  -> [SchemaElement]
optionalListSchemaSegments outerName leafType outerFid leafFid =
  [ SchemaElement
      { seTypeLength = Nothing
      , seName = outerName
      , seRepetition = Just Optional
      , seType = Nothing
      , seNumChildren = Just 1
      , seConvertedType = Just CTList
      , seLogicalType = Just LTList
      , seFieldId = outerFid
      }
  , SchemaElement
      { seTypeLength = Nothing
      , seName = "list"
      , seRepetition = Just Repeated
      , seType = Nothing
      , seNumChildren = Just 1
      , seConvertedType = Nothing
      , seLogicalType = Nothing
      , seFieldId = Nothing
      }
  , SchemaElement
      { seTypeLength = Nothing
      , seName = "element"
      , seRepetition = Just Optional
      , seType = Just leafType
      , seNumChildren = Nothing
      , seConvertedType = Nothing
      , seLogicalType = Nothing
      , seFieldId = leafFid
      }
  ]


buildOptionalListFile
  :: ParquetType
  -> NestedLeaf
  -> Int
  -> ByteString
buildOptionalListFile leafType leaf numRows =
  either error id (buildOptionalListFileEither leafType leaf numRows)


{- | Checked optional-list convenience writer. The leaf must describe the
standard optional list of optional primitives, with path @name/list/element@.
Counts, level ranges, row boundaries and the complete page size are checked
before encoding level streams or copying the supplied PLAIN value bytes.
'buildOptionalListFile' is the exception-raising convenience wrapper.
-}
buildOptionalListFileEither :: ParquetType -> NestedLeaf -> Int -> Either String ByteString
buildOptionalListFileEither leafType leaf numRows = do
  if nlMaxDef leaf /= 3 || nlMaxRep leaf /= 1
    then Left "Parquet.Nested: optional-list leaf needs maximum definition/repetition levels 3/1"
    else Right ()
  let path = nlPath leaf
  if V.length path /= 3 || path V.! 1 /= "list" || path V.! 2 /= "element"
    then Left "Parquet.Nested: optional-list leaf needs path name/list/element"
    else Right ()
  plan <- planNestedLeafPage numRows leaf
  validateOptionalListBoundaries leaf
  validateOptionalListPlainSize leafType leaf
  pageSize <- checkedNestedFileSize (plannedNestedPageSize plan)
  let
    !numEvents = VP.length (nlDefLevels leaf)
    !schema =
      V.fromList
        ( SchemaElement
            { seTypeLength = Nothing
            , seName = "schema"
            , seRepetition = Nothing
            , seType = Nothing
            , seNumChildren = Just 1
            , seConvertedType = Nothing
            , seLogicalType = Nothing
            , seFieldId = Nothing
            }
            : optionalListSchemaSegments
              (V.unsafeIndex (nlPath leaf) 0)
              leafType
              Nothing
              Nothing
        )

    !colChunk =
      ColumnChunk
        { ccFilePath = Nothing
        , ccFileOffset = 0
        , ccMetadata =
            Just
              ColumnMetadata
                { cmType = leafType
                , cmEncodings = V.fromList [Plain, RLE]
                , cmPathInSchema = nlPath leaf
                , cmCodec = Uncompressed
                , cmNumValues = fromIntegral numEvents
                , cmTotalUncompressedSize = fromIntegral pageSize
                , cmTotalCompressedSize = fromIntegral pageSize
                , cmDataPageOffset = 4
                , cmDictionaryPageOffset = Nothing
                , cmStatistics =
                    Just
                      Statistics
                        { statMinValueExact = Nothing
                        , statMaxValueExact = Nothing
                        , statNanCount = Nothing
                        , statMin = Nothing
                        , statMax = Nothing
                        , statNullCount = Just (fromIntegral (numEvents - nlValueCount leaf))
                        , statDistinctCount = Nothing
                        , statMinValue = Nothing
                        , statMaxValue = Nothing
                        }
                , cmBloomFilterOffset = Nothing
                , cmBloomFilterLength = Nothing
                }
        , ccOffsetIndexOffset = Nothing
        , ccOffsetIndexLength = Nothing
        , ccColumnIndexOffset = Nothing
        , ccColumnIndexLength = Nothing
        }
    !rg =
      RowGroup
        { rgColumns = V.singleton colChunk
        , rgTotalByteSize = fromIntegral pageSize
        , rgNumRows = fromIntegral numRows
        , rgSortingColumns = Nothing
        }
    !fm =
      FileMetadata
        { fmVersion = 2
        , fmSchema = schema
        , fmNumRows = fromIntegral numRows
        , fmRowGroups = V.singleton rg
        , fmCreatedBy = Just "wireform"
        , fmKeyValueMetadata = Nothing
        , fmColumnOrders = Nothing
        }
  assembleNestedOutput fm (toInteger pageSize) (plannedNestedPageBuilder plan (B.byteString (nlValueBytes leaf)))


-- ============================================================
-- Schema serialisation
-- ============================================================

{- | Walk a top-level @(name, NestedSchema)@ vector and produce the
flat 'V.Vector SchemaElement' Parquet's footer expects. The first
element is always the row-group root; the rest are the schema
nodes in pre-order DFS, with @num_children@ populated on every
internal node.
-}
nestedSchemaToFlatSchema
  :: V.Vector (Text, NestedSchema)
  -> V.Vector SchemaElement
nestedSchemaToFlatSchema columns =
  let !root =
        SchemaElement
          { seTypeLength = Nothing
          , seName = "schema"
          , seRepetition = Nothing
          , seType = Nothing
          , seNumChildren = Just (fromIntegral (V.length columns))
          , seConvertedType = Nothing
          , seLogicalType = Nothing
          , seFieldId = Nothing
          }
      !cols =
        concatMap
          (\(name, sch) -> emitColumn name sch)
          (V.toList columns)
  in V.fromList (root : cols)


{- | Emit a column's flat schema entries in pre-order DFS. The
top-level element gets a name and (for compound types) the
LogicalType / ConvertedType annotation.
-}
emitColumn :: Text -> NestedSchema -> [SchemaElement]
emitColumn outerName outerSchema = goTop outerName outerSchema
  where
    -- Top-level: peel the outer Optional / Required wrappers,
    -- recording the repetition. The actual /child/ that gets the
    -- name and annotations is what's underneath.
    goTop name (NSOptional inner) = goNamed name (Just Optional) inner
    goTop name (NSRequired inner) = goNamed name (Just Required) inner
    goTop name other = goNamed name (Just Required) other

    -- A "named" node: pick its annotation + group-or-leaf shape.
    goNamed name rep node = case node of
      NSPrimitive lt -> [leafSchemaElement name rep lt]
      NSOptional inner -> goNamed name (Just Optional) inner
      NSRequired inner -> goNamed name (Just Required) inner
      NSList inner ->
        let !groupHead =
              SchemaElement
                { seTypeLength = Nothing
                , seName = name
                , seRepetition = rep
                , seType = Nothing
                , seNumChildren = Just 1
                , seConvertedType = Just CTList
                , seLogicalType = Just LTList
                , seFieldId = Nothing
                }
            !listGroup =
              SchemaElement
                { seTypeLength = Nothing
                , seName = "list"
                , seRepetition = Just Repeated
                , seType = Nothing
                , seNumChildren = Just 1
                , seConvertedType = Nothing
                , seLogicalType = Nothing
                , seFieldId = Nothing
                }
        in groupHead : listGroup : goNamed "element" (Just Required) inner
      NSMap k v ->
        let !groupHead =
              SchemaElement
                { seTypeLength = Nothing
                , seName = name
                , seRepetition = rep
                , seType = Nothing
                , seNumChildren = Just 1
                , seConvertedType = Just CTMap
                , seLogicalType = Just LTMap
                , seFieldId = Nothing
                }
            !kvGroup =
              SchemaElement
                { seTypeLength = Nothing
                , seName = "key_value"
                , seRepetition = Just Repeated
                , seType = Nothing
                , seNumChildren = Just 2
                , seConvertedType = Nothing
                , seLogicalType = Nothing
                , seFieldId = Nothing
                }
        in groupHead
             : kvGroup
             : goNamed "key" (Just Required) k
             ++ goNamed "value" (Just Required) v
      NSStruct fields ->
        let !groupHead =
              SchemaElement
                { seTypeLength = Nothing
                , seName = name
                , seRepetition = rep
                , seType = Nothing
                , seNumChildren = Just (fromIntegral (V.length fields))
                , seConvertedType = Nothing
                , seLogicalType = Nothing
                , seFieldId = Nothing
                }
        in groupHead
             : concatMap
               (\(fname, fch) -> goNamed fname (Just Required) fch)
               (V.toList fields)
      NSFile fields ->
        let children = F.fileSchemaElements fields
            groupHead =
              SchemaElement
                { seTypeLength = Nothing
                , seName = name
                , seRepetition = rep
                , seType = Nothing
                , seNumChildren = Just (fromIntegral (V.length children))
                , seConvertedType = Nothing
                , seLogicalType = Just LTFile
                , seFieldId = F.fileSchemaFieldId fields
                }
        in groupHead : V.toList children
      NSVariant ->
        -- The Iceberg V3 / Spark Variant unshredded shape is:
        --   <rep> group <name> (VARIANT(1)) {
        --     required binary metadata;
        --     required binary value;
        --   }
        let !groupHead =
              SchemaElement
                { seTypeLength = Nothing
                , seName = name
                , seRepetition = rep
                , seType = Nothing
                , seNumChildren = Just 2
                , seConvertedType = Nothing
                , seLogicalType = Just (LTVariant 1)
                , seFieldId = Nothing
                }
            !metadataLeaf =
              SchemaElement
                { seTypeLength = Nothing
                , seName = "metadata"
                , seRepetition = Just Required
                , seType = Just PTByteArray
                , seNumChildren = Nothing
                , seConvertedType = Nothing
                , seLogicalType = Nothing
                , seFieldId = Nothing
                }
            !valueLeaf =
              SchemaElement
                { seTypeLength = Nothing
                , seName = "value"
                , seRepetition = Just Required
                , seType = Just PTByteArray
                , seNumChildren = Nothing
                , seConvertedType = Nothing
                , seLogicalType = Nothing
                , seFieldId = Nothing
                }
        in [groupHead, metadataLeaf, valueLeaf]
      NSVariantWithLimits _ -> goNamed name rep NSVariant
      NSVariantTyped storage -> case goNamed name rep (storageLayout storage) of
        group : children ->
          V.toList (V.imap (\index node -> node {seFieldId = maybe Nothing id (storageFieldIds storage V.!? index)}) (V.fromList (group {seLogicalType = Just (LTVariant 1)} : children)))
        [] -> []


-- ============================================================
-- Whole-file builder for arbitrary nested schemas
-- ============================================================

{- | Build a complete Parquet file from a vector of top-level
@(name, NestedSchema)@ columns plus a vector of row-major
'NestedRow' values, one per column. Row count = length of the
inner vector for any column (they must all match).

The file layout is one DATA_PAGE_V2 per primitive leaf, no
compression, no bloom filter / page index. Suitable for the
pyarrow round-trip test that ground-truths the shred against
arrow-cpp's reader.
-}
buildNestedFile
  :: V.Vector (Text, NestedSchema)
  -> V.Vector (V.Vector NestedRow)
  -- ^ outer index = column, inner = row
  -> Either String ByteString
buildNestedFile = buildNestedFileWithAux V.empty


{- | Nested writer with one auxiliary record per physical leaf, in schema
order. An empty vector selects the legacy uncompressed V2 layout. Supports
compression, V1/V2 pages, bloom filters and indexes. Automatic column indexes
are emitted when bounds or all-null statistics are available. Encryption is
rejected explicitly; callers must use an IO writer for random nonces.
-}
buildNestedFileWithAux
  :: V.Vector PW.ColumnAux
  -> V.Vector (Text, NestedSchema)
  -> V.Vector (V.Vector NestedRow)
  -> Either String ByteString
buildNestedFileWithAux = buildNestedFileWithBloomFilters []


{- | Like 'buildNestedFileWithAux', generating a bloom filter for every
physical leaf beneath each requested top-level field name. Unknown names
are rejected. Explicit per-leaf filters take precedence over generation.
-}
buildNestedFileWithBloomFilters
  :: [Text]
  -> V.Vector PW.ColumnAux
  -> V.Vector (Text, NestedSchema)
  -> V.Vector (V.Vector NestedRow)
  -> Either String ByteString
buildNestedFileWithBloomFilters = buildNestedFileWithBloomFiltersUsing assembleNestedOutputChecked


-- | Pass validated page builders and metadata to a custom file assembler.
-- The validation action must run before the builder is materialized.
buildNestedFileWithBloomFiltersUsing
  :: (FileMetadata -> Integer -> Either String () -> B.Builder -> Either String a)
  -> [Text]
  -> V.Vector PW.ColumnAux
  -> V.Vector (Text, NestedSchema)
  -> V.Vector (V.Vector NestedRow)
  -> Either String a
buildNestedFileWithBloomFiltersUsing finish bloomNames suppliedAux columns rowsPerColumn
  | V.length columns /= V.length rowsPerColumn =
      Left "Parquet.Nested.buildNestedFile: column count mismatch"
  | V.null columns = Left "Parquet.Nested.buildNestedFile: at least one column required"
  | otherwise = do
      mapM_ (\name -> if V.any ((== name) . fst) columns then Right () else Left "Parquet.Nested: requested bloom field does not exist") bloomNames
      V.mapM_ validateNestedAux suppliedAux
      let !numRows = V.length (V.unsafeIndex rowsPerColumn 0)
      _ <- nestedPageCount "row count" numRows
      -- Sanity: every column must have the same row count.
      let !rowCounts = V.map V.length rowsPerColumn
      if V.any (/= numRows) rowCounts
        then Left "Parquet.Nested.buildNestedFile: row counts differ between columns"
        else do
          shreds <-
            V.imapM
              ( \i (_name, sch) ->
                  let !rows = V.unsafeIndex rowsPerColumn i
                  in planShred sch rows
              )
              columns
          -- A null-only empty FILE contributes no physical leaf.
          let !flatSchema = nestedSchemaToFlatSchema columns
              -- Patch each leaf's path so that its first element is
              -- the column name; the shredder uses "" by default.
              !columnNames = V.toList (V.map fst columns)
              !leavesByCol = V.toList shreds
              !pathedPlans =
                concat
                  ( zipWith
                      ( \name leaves ->
                          map
                            (\prepared -> let descriptor = slpDescriptor prepared in prepared {slpDescriptor = descriptor {ldPath = V.cons name (V.tail (ldPath descriptor))}})
                            (V.toList leaves)
                      )
                      columnNames
                      leavesByCol
                  )
          let leafCount = length pathedPlans
              baseAuxes = if V.null suppliedAux then V.replicate leafCount PW.emptyColumnAux {PW.caPageVersion = PW.PageV2} else suppliedAux
          if V.length baseAuxes /= leafCount
            then Left "Parquet.Nested: auxiliary column count mismatch"
            else Right ()
          let leafFields = V.filter (maybe False (const True) . seType) flatSchema
              generateStats = not (V.null suppliedAux)
          if V.length leafFields /= leafCount
            then Left "Parquet.Nested: schema and shredded leaf counts disagree"
            else Right ()
          generated <- V.imapM (\index prepared -> prepareAuxiliary generateStats (leafFields V.! index) bloomNames (baseAuxes V.! index) prepared) (V.fromList pathedPlans)
          let auxes = V.map preparedAux generated
          encoded <- V.mapM (\entry -> let prepared = preparedLeaf entry in EncodedNestedLeaf prepared (preparedStatistics entry) <$> encodeNestedPlan numRows (preparedAux entry) prepared) generated
          let encodedPlans = V.toList encoded
          rowGroupSize <- checkedNestedFileSize (V.foldl' (\total leaf -> total + toInteger (nepSize (enlPage leaf))) 0 encoded)
          let !startOfData = 4 :: Int
          -- Build per-column metadata, threading the file-relative
          -- offset.
          let buildCol :: Int -> [EncodedNestedLeaf] -> [ColumnChunk]
              buildCol _ [] = []
              buildCol off (encodedLeaf : rest) =
                let prepared = enlPlan encodedLeaf
                    encodedPage = enlPage encodedLeaf
                    page = slpPage prepared
                    descriptor = slpDescriptor prepared
                    sz = nepSize encodedPage
                    ldType' = leafParquetType (ldType descriptor)
                    cm =
                      ColumnMetadata
                        { cmType = ldType'
                        , cmEncodings =
                            if ldMaxDef descriptor > 0 || ldMaxRep descriptor > 0
                              then V.fromList [Plain, RLE]
                              else V.singleton Plain
                        , cmPathInSchema = ldPath descriptor
                        , cmCodec = nepCodec encodedPage
                        , cmNumValues =
                            fromIntegral
                              (VP.length (slpDefinitions prepared))
                        , cmTotalUncompressedSize = fromIntegral (nepUncompressedSize encodedPage)
                        , cmTotalCompressedSize = fromIntegral sz
                        , cmDataPageOffset = fromIntegral off
                        , cmDictionaryPageOffset = Nothing
                        , cmStatistics = Just (enlStatistics encodedLeaf)
                        , cmBloomFilterOffset = Nothing
                        , cmBloomFilterLength = Nothing
                        }
                    cc =
                      ColumnChunk
                        { ccFilePath = Nothing
                        , ccFileOffset = 0
                        , ccMetadata = Just cm
                        , ccOffsetIndexOffset = Nothing
                        , ccOffsetIndexLength = Nothing
                        , ccColumnIndexOffset = Nothing
                        , ccColumnIndexLength = Nothing
                        }
                in cc : buildCol (off + sz) rest
              !columnChunks =
                buildCol
                  startOfData
                  encodedPlans
              !rg =
                RowGroup
                  { rgColumns = V.fromList columnChunks
                  , rgTotalByteSize = V.foldl' (\total leaf -> total + fromIntegral (nepUncompressedSize (enlPage leaf))) 0 encoded
                  , rgNumRows = fromIntegral numRows
                  , rgSortingColumns = Nothing
                  }
              !fm =
                FileMetadata
                  { fmVersion = 2
                  , fmSchema = flatSchema
                  , fmNumRows = fromIntegral numRows
                  , fmRowGroups = V.singleton rg
                  , fmCreatedBy = Just "wireform"
                  , fmKeyValueMetadata = Nothing
                  , fmColumnOrders = if generateStats || V.any (\aux -> PW.caGeneratePageIndex aux || maybe False (const True) (PW.caColumnIndex aux)) auxes then Just (V.map NestedStats.statisticsOrderFor leafFields) else Nothing
                  }
          layout <- PW.layoutColumnAuxiliaries (V.singleton rg) (V.singleton auxes) (4 + rowGroupSize)
          finish
            fm {fmRowGroups = PW.auxRowGroups layout}
            (toInteger (PW.auxEndOffset layout) - 4)
            (V.imapM_ (\i (_, schema) -> validateVariantRows schema (V.unsafeIndex rowsPerColumn i)) columns)
            (foldMap (nepBuilder . enlPage) encodedPlans <> foldMap B.byteString (PW.auxParts layout))


-- Keep the page plan and its per-leaf options together while generated
-- filters replace only missing explicit filters.
data PreparedNestedLeaf = PreparedNestedLeaf
  { preparedLeaf :: !ShreddedLeafPlan
  , preparedAux :: !PW.ColumnAux
  , preparedStatistics :: !Statistics
  }


prepareAuxiliary :: Bool -> SchemaElement -> [Text] -> PW.ColumnAux -> ShreddedLeafPlan -> Either String PreparedNestedLeaf
prepareAuxiliary generateStats field names aux prepared = do
  let descriptor = slpDescriptor prepared
      count = nppValueCount (slpPage prepared)
      nulls = VP.length (slpDefinitions prepared) - count
      nullStatistics = Statistics Nothing Nothing (Just (fromIntegral nulls)) Nothing Nothing Nothing Nothing Nothing Nothing
      requestBloom = case PW.caBloomFilter aux of
        Just _ -> False
        Nothing -> V.head (ldPath descriptor) `elem` names
  if not generateStats && not requestBloom
    then pure (PreparedNestedLeaf prepared aux nullStatistics)
    else do
      let bytes = shreddedValueBytes (slpValues prepared)
          cached = prepared {slpValues = MaterializedValues bytes}
      bloom <- if requestBloom then Just <$> NestedBloom.buildBloomFilter field count bytes else pure (PW.caBloomFilter aux)
      stats <- if generateStats then NestedStats.columnStatistics (NestedStats.statisticsOrderFor field) field nulls count bytes else pure nullStatistics
      pure (PreparedNestedLeaf cached aux {PW.caBloomFilter = bloom} stats)


data EncodedNestedPage = EncodedNestedPage
  { nepBuilder :: !B.Builder
  , nepSize :: {-# UNPACK #-} !Int
  , nepUncompressedSize :: {-# UNPACK #-} !Int
  , nepCodec :: !Compression
  }


data EncodedNestedLeaf = EncodedNestedLeaf
  { enlPlan :: !ShreddedLeafPlan
  , enlStatistics :: !Statistics
  , enlPage :: !EncodedNestedPage
  }


validateNestedAux :: PW.ColumnAux -> Either String ()
validateNestedAux aux = do
  PW.validateAuxIndexes aux
  Compress.validateCompression (PW.caCodec aux)
  case PW.caEncryption aux of
    Just _ -> Left "Parquet.Nested: encryption requires a nested IO writer"
    Nothing -> Right ()


encodeNestedPlan :: Int -> PW.ColumnAux -> ShreddedLeafPlan -> Either String EncodedNestedPage
encodeNestedPlan _ aux prepared
  | PW.caPageVersion aux == PW.PageV2 && PW.caCodec aux == Uncompressed =
      let plan = slpPage prepared
          size = fromInteger (plannedNestedPageSize plan)
      in Right (EncodedNestedPage (plannedNestedPageBuilder plan (shreddedValueBuilder (slpValues prepared))) size size Uncompressed)
encodeNestedPlan numRows aux prepared = do
  let plan = slpPage prepared
      descriptor = slpDescriptor prepared
      codec = PW.caCodec aux
      version = PW.caPageVersion aux
      levelBytes = nestedLevelSize (nppRepetition plan) + nestedLevelSize (nppDefinition plan)
      prefixes = case version of
        PW.PageV1 -> (if ldMaxRep descriptor > 0 then 4 else 0) + (if ldMaxDef descriptor > 0 then 4 else 0)
        PW.PageV2 -> 0
      plainSize = fromIntegral (nppPayloadSize plan) + prefixes
      values = shreddedValueBuilder (slpValues prepared)
      repetition = maybe BS.empty LE.encodeHybridPlan (nppRepetition plan)
      definition = maybe BS.empty LE.encodeHybridPlan (nppDefinition plan)
      levels = levelPart version (nppRepetition plan) repetition <> levelPart version (nppDefinition plan) definition
      plainBody = levels <> values
  uncompressed <- nestedPageCount "uncompressed page size" plainSize
  -- The uncompressed path retains the builder directly, avoiding an
  -- intermediate values buffer and an extra copy for existing callers.
  payload <- case codec of
    Uncompressed -> Right (NestedPayload plainBody plainSize)
    _ -> case version of
      PW.PageV1 -> do
        compressed <- Compress.compressPageBytes codec (BL.toStrict (B.toLazyByteString plainBody))
        Right (NestedPayload (B.byteString compressed) (BS.length compressed))
      PW.PageV2 -> do
        compressed <- Compress.compressPageBytes codec (shreddedValueBytes (slpValues prepared))
        Right (NestedPayload (levels <> B.byteString compressed) (levelBytes + BS.length compressed))
  compressed <- nestedPageCount "compressed page size" (npSize payload)
  let events = fromIntegral (VP.length (slpDefinitions prepared))
      rows = fromIntegral numRows
      kind = case version of
        PW.PageV1 -> PP.PtDataPage (PP.DataPageHeader events 0)
        PW.PageV2 -> PP.PtDataPageV2 (PP.DataPageHeaderV2 events (events - fromIntegral (nppValueCount plan)) rows 0 (fromIntegral (BS.length definition)) (fromIntegral (BS.length repetition)) (codec /= Uncompressed))
      header = PW.encodePageHeader (PP.PageHeader kind (Just uncompressed) (Just compressed))
      headerSize = BS.length header
  Right (EncodedNestedPage (B.byteString header <> npBuilder payload) (headerSize + npSize payload) (headerSize + plainSize) codec)
  where
    levelPart PW.PageV1 (Just _) bytes = B.word32LE (fromIntegral (BS.length bytes)) <> B.byteString bytes
    levelPart _ _ bytes = B.byteString bytes


data NestedPayload = NestedPayload
  { npBuilder :: !B.Builder
  , npSize :: {-# UNPACK #-} !Int
  }


-- Page-header counts and byte sizes are signed i32 in parquet.thrift.
-- Keep validation/planning separate from encoding so a rejected page never
-- allocates level payloads or copies an oversized supplied value payload.
data NestedPagePlan = NestedPagePlan
  { nppHeaderBytes :: !ByteString
  , nppDefinition :: !(Maybe LE.HybridPlan)
  , nppRepetition :: !(Maybe LE.HybridPlan)
  , nppPayloadSize :: {-# UNPACK #-} !Int32
  , nppValueCount :: {-# UNPACK #-} !Int
  }


-- Page metadata can be measured from levels and planned value length,
-- without constructing the potentially large encoded value buffer.
-- Caller-supplied levels need a full count/boundary check. The internal
-- shredder has already validated rows and counted each leaf; retain those
-- counts instead of rescanning levels it just constructed.
data NestedCountSource = CheckLevelCounts !(Maybe Int) | ShreddedCounts {-# UNPACK #-} !Int


data NestedPageInput = NestedPageInput
  { npiRows :: {-# UNPACK #-} !Int
  , npiMaxDef :: {-# UNPACK #-} !Int
  , npiMaxRep :: {-# UNPACK #-} !Int
  , npiDefinitions :: !(VP.Vector Int32)
  , npiRepetitions :: !(VP.Vector Int32)
  , npiCounts :: !NestedCountSource
  , npiValueBytes :: {-# UNPACK #-} !Int
  }


nestedPageCount :: String -> Int -> Either String Int32
nestedPageCount label count
  | count < 0 || toInteger count > toInteger (maxBound :: Int32) =
      Left ("Parquet.Nested: " ++ label ++ " exceeds signed 32-bit page limit")
  | otherwise = Right (fromIntegral count)


planNestedLeafPage :: Int -> NestedLeaf -> Either String NestedPagePlan
planNestedLeafPage numRows leaf =
  planNestedPageInput
    ( NestedPageInput
        numRows
        (nlMaxDef leaf)
        (nlMaxRep leaf)
        (nlDefLevels leaf)
        (nlRepLevels leaf)
        (CheckLevelCounts (Just (nlValueCount leaf)))
        (BS.length (nlValueBytes leaf))
    )


planNestedPageInput :: NestedPageInput -> Either String NestedPagePlan
planNestedPageInput input = do
  let !numRows = npiRows input
  rows <- nestedPageCount "row count" numRows
  let defs = npiDefinitions input
      reps = npiRepetitions input
      !numEvents = VP.length defs
      expectedValues = case npiCounts input of CheckLevelCounts expected -> expected; ShreddedCounts count -> Just count
      !valueBytes = npiValueBytes input
  events <- nestedPageCount "event count" numEvents
  case expectedValues of
    Just count -> do
      _ <- nestedPageCount "value count" count
      if count > numEvents then Left "Parquet.Nested: inconsistent leaf event/value/row counts or level maxima" else Right ()
    Nothing -> Right ()
  _ <- nestedPageCount "value payload size" valueBytes
  _ <- nestedPageCount "maximum definition level" (npiMaxDef input)
  _ <- nestedPageCount "maximum repetition level" (npiMaxRep input)
  if VP.length reps /= numEvents || numRows > numEvents || npiMaxRep input > npiMaxDef input
    then Left "Parquet.Nested: inconsistent leaf event/value/row counts or level maxima"
    else Right ()
  -- This loop returns one public error/result for the entire page. Strict
  -- counters become Int# workers; there is no per-event Either or tuple.
  let validate !index !observedRows !observedValues
        | index == numEvents =
            if observedRows == numRows && maybe True (== observedValues) expectedValues
              then Right observedValues
              else Left "Parquet.Nested: levels disagree with row or non-null value count"
        | otherwise =
            let !rep = VP.unsafeIndex reps index
                !def = VP.unsafeIndex defs index
            in if index == 0 && rep /= 0
                 then Left "Parquet.Nested: a V2 page must begin at a row boundary"
                 else validate (index + 1) (observedRows + if rep == 0 then 1 else 0) (observedValues + if fromIntegral def == npiMaxDef input then 1 else 0)
  numValues <- case npiCounts input of
    CheckLevelCounts _ -> validate 0 0 0
    ShreddedCounts count -> Right count
  definition <- levelPlan (npiMaxDef input) defs
  repetition <- levelPlan (npiMaxRep input) reps
  let !total = toInteger valueBytes + toInteger (nestedLevelSize definition) + toInteger (nestedLevelSize repetition)
  if total > toInteger (maxBound :: Int32)
    then Left "Parquet.Nested: complete page payload exceeds signed 32-bit limit"
    else
      let !payloadSize = fromInteger total
          !header =
            PP.PageHeader
              { PP.phType =
                  PP.PtDataPageV2
                    PP.DataPageHeaderV2
                      { PP.dph2NumValues = events
                      , PP.dph2NumNulls = fromIntegral (numEvents - numValues)
                      , PP.dph2NumRows = rows
                      , PP.dph2Encoding = 0
                      , PP.dph2DefLevelsLen = fromIntegral (nestedLevelSize definition)
                      , PP.dph2RepLevelsLen = fromIntegral (nestedLevelSize repetition)
                      , PP.dph2IsCompressed = False
                      }
              , PP.phUncompressedPageSize = Just payloadSize
              , PP.phCompressedPageSize = Just payloadSize
              }
      in Right (NestedPagePlan (PW.encodePageHeader header) definition repetition payloadSize numValues)
  where
    levelPlan 0 _ | ShreddedCounts _ <- npiCounts input = Right Nothing
    levelPlan maximumLevel levels = do
      plan <- LE.planRLEHybrid (LE.bitWidthFor maximumLevel) (fromIntegral maximumLevel) levels
      Right (if maximumLevel == 0 then Nothing else Just plan)


nestedLevelSize :: Maybe LE.HybridPlan -> Int
nestedLevelSize = maybe 0 LE.hybridEncodedSize


-- A continuation belongs to the same repeated element group. Both its
-- predecessor and current event must have crossed that group's definition
-- threshold (2 for optional-list<optional primitive>). A null or empty list
-- therefore cannot be followed by another event in the same row.
validateOptionalListBoundaries :: NestedLeaf -> Either String ()
validateOptionalListBoundaries leaf = go 0 0
  where
    defs = nlDefLevels leaf
    reps = nlRepLevels leaf
    go !index !previousDef
      | index == VP.length defs = Right ()
      | otherwise =
          let !def = VP.unsafeIndex defs index
              !rep = VP.unsafeIndex reps index
          in if rep /= 0 && (previousDef < 2 || def < 2)
               then Left "Parquet.Nested: repeated value continues a null or empty list"
               else go (index + 1) def


-- The convenience schema has no fixed-byte width parameter. Other fixed
-- physical types have an exact PLAIN size; BYTE_ARRAY carries its own lengths.
validateOptionalListPlainSize :: ParquetType -> NestedLeaf -> Either String ()
validateOptionalListPlainSize physical leaf = case physical of
  PTBoolean -> checkSize ((toInteger (nlValueCount leaf) + 7) `quot` 8)
  PTInt32 -> fixed 4
  PTInt64 -> fixed 8
  PTInt96 -> fixed 12
  PTFloat -> fixed 4
  PTDouble -> fixed 8
  PTByteArray -> validateNestedPlainByteArrays (nlValueCount leaf) (nlValueBytes leaf)
  PTFixedLenByteArray -> Left "Parquet.Nested: optional-list schema cannot specify a fixed-byte width"
  where
    fixed width = checkSize (width * toInteger (nlValueCount leaf))
    checkSize expected
      | expected == toInteger (BS.length (nlValueBytes leaf)) = Right ()
      | otherwise = Left "Parquet.Nested: PLAIN payload size differs from physical value count"


validateNestedPlainByteArrays :: Int -> ByteString -> Either String ()
validateNestedPlainByteArrays count bytes = go 0 0
  where
    !size = BS.length bytes
    go !index !offset
      | index == count =
          if offset == size then Right () else Left "Parquet.Nested: trailing PLAIN BYTE_ARRAY bytes"
      | size - offset < 4 = Left "Parquet.Nested: truncated PLAIN BYTE_ARRAY length"
      | otherwise =
          let !lengthWord =
                fromIntegral (BSU.unsafeIndex bytes offset)
                  + (fromIntegral (BSU.unsafeIndex bytes (offset + 1)) `shiftL` 8)
                  + (fromIntegral (BSU.unsafeIndex bytes (offset + 2)) `shiftL` 16)
                  + (fromIntegral (BSU.unsafeIndex bytes (offset + 3)) `shiftL` 24)
                  :: Word32
          in if lengthWord > fromIntegral (size - offset - 4)
               then Left "Parquet.Nested: truncated PLAIN BYTE_ARRAY value"
               else go (index + 1) (offset + 4 + fromIntegral lengthWord)


checkedNestedFileSize :: Integer -> Either String Int
checkedNestedFileSize size
  | size < 0 || size > toInteger (maxBound :: Int) - 4 = Left "Parquet.Nested: row-group bytes exceed addressable file offsets"
  | otherwise = Right (fromInteger size)


plannedNestedPageBuilder :: NestedPagePlan -> B.Builder -> B.Builder
plannedNestedPageBuilder plan values =
  let !defStream = maybe BS.empty LE.encodeHybridPlan (nppDefinition plan)
      !repStream = maybe BS.empty LE.encodeHybridPlan (nppRepetition plan)
  in B.byteString (nppHeaderBytes plan) <> B.byteString repStream <> B.byteString defStream <> values


assembleNestedOutput :: FileMetadata -> Integer -> B.Builder -> Either String ByteString
assembleNestedOutput metadata dataSize = assembleNestedOutputChecked metadata dataSize (Right ())


assembleNestedOutputChecked :: FileMetadata -> Integer -> Either String () -> B.Builder -> Either String ByteString
assembleNestedOutputChecked metadata dataSize validatePayloads body = do
  footer <- PF.writeFooterEither metadata
  let !total = dataSize + 4 + toInteger (BS.length footer)
  if dataSize < 0 || total > toInteger (maxBound :: Int)
    then Left "Parquet.Nested: complete file exceeds addressable byte size"
    else do
      validatePayloads
      Right (B.toStrictByteStringWith (fromInteger total) (B.byteString PF.parquetMagic <> body <> B.byteString footer))


plannedNestedPageSize :: NestedPagePlan -> Integer
plannedNestedPageSize plan = toInteger (nppPayloadSize plan) + toInteger (BS.length (nppHeaderBytes plan))


-- silence unused 'leafConvertedType' / 'leafLogicalType' / 'leafParquetType' warnings
_unusedHelpers :: (LeafType -> ParquetType, LeafType -> Maybe ConvertedType, LeafType -> Maybe LogicalType)
_unusedHelpers = (leafParquetType, leafConvertedType, leafLogicalType)
