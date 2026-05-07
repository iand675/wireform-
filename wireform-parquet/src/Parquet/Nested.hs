{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
-- | Writer-side support for arbitrary nested Parquet column trees:
-- structs, lists (3-level encoding), maps, and any composition of
-- those with primitive leaves. The writer walks a row-major tree and
-- emits Dremel-style @(repetition, definition, value)@ events per
-- leaf, ready to drop into a DATA_PAGE_V2.
--
-- The Iceberg spec uses the standard parquet-format encoding for
-- each compound type:
--
-- * @list<T>@ → @optional group <name> (LIST) { repeated group list
--   { optional <T> element; } }@.
-- * @map<K, V>@ → @optional group <name> (MAP) { repeated group
--   key_value { required <K> key; optional <V> value; } }@.
-- * @struct<...>@ → an unannotated group whose children are fields.
--
-- We model the row-major input as 'NestedRow' (a self-similar tree
-- of @Maybe@ / list / struct / map / leaf), build the schema with
-- 'NestedSchema' / 'mkSchema', and run 'shred' to flatten one row at
-- a time. The result is one 'NestedLeaf' per primitive leaf in the
-- schema in left-to-right order, exactly matching what
-- DATA_PAGE_V2 expects.
--
-- The sibling 'encodeOptionalListOptionalI32' / friends shipped in
-- the previous PR are now thin wrappers that build a 'NestedSchema',
-- a 'NestedRow' vector, and call 'shred'.
module Parquet.Nested
  ( -- * Schema
    NestedSchema (..)
  , LeafType (..)
  , LeafDescriptor (..)
    -- * Row-major data
  , NestedRow (..)
  , LeafValue (..)
    -- * Shredding
  , NestedLeaf (..)
  , shred
    -- * Schema flattening
  , flattenSchema
  , nestedSchemaToFlatSchema
    -- * Whole-file builder
  , buildNestedFile
    -- * Convenience for the optional-list-of-optional-primitive case
    --   (the previous PR's Iceberg-default-value shape)
  , encodeOptionalListOptionalI32
  , encodeOptionalListOptionalI64
  , encodeOptionalListOptionalDouble
  , optionalListSchemaSegments
  , buildOptionalListFile
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import GHC.Float (castDoubleToWord64, castFloatToWord32)

import qualified Parquet.LevelsEncode as LE
import qualified Parquet.Page as PP
import Parquet.Types
  ( ColumnChunk (..)
  , ColumnMetadata (..)
  , Compression (..)
  , ConvertedType (..)
  , Encoding (..)
  , FileMetadata (..)
  , LogicalType (..)
  , ParquetType (..)
  , Repetition (..)
  , RowGroup (..)
  , Statistics (..)
  , SchemaElement (..)
  )
import qualified Parquet.Write as PW

-- ============================================================
-- Schema description
-- ============================================================

-- | Description of one column tree, mirroring the user's logical
-- type. 'NSOptional' / 'NSRequired' wrap a child to set the leaf or
-- group's repetition (Optional / Required); 'NSList' / 'NSMap' /
-- 'NSStruct' name the compound types; 'NSVariant' emits the
-- Iceberg V3 / Spark Variant 2-leaf binary group
-- (@{required metadata: BINARY, required value: BINARY}@) annotated
-- with the @VARIANT(1)@ logical type.
data NestedSchema
  = NSPrimitive !LeafType
  | NSOptional  !NestedSchema
  | NSRequired  !NestedSchema
  | NSList      !NestedSchema
  | NSMap       !NestedSchema !NestedSchema   -- key, value
  | NSStruct    !(V.Vector (Text, NestedSchema))
  | NSVariant
  deriving (Show, Eq)

-- | Leaf primitive type and its Parquet physical encoding.
data LeafType
  = LtInt32
  | LtInt64
  | LtFloat
  | LtDouble
  | LtBool
  | LtString
  | LtBinary
  deriving (Show, Eq)

leafParquetType :: LeafType -> ParquetType
leafParquetType = \case
  LtInt32   -> PTInt32
  LtInt64   -> PTInt64
  LtFloat   -> PTFloat
  LtDouble  -> PTDouble
  LtBool    -> PTBoolean
  LtString  -> PTByteArray
  LtBinary  -> PTByteArray

leafConvertedType :: LeafType -> Maybe ConvertedType
leafConvertedType = \case
  LtString -> Just CTUtf8
  _        -> Nothing

leafLogicalType :: LeafType -> Maybe LogicalType
leafLogicalType = \case
  LtString -> Just LTString
  _        -> Nothing

-- ============================================================
-- Row-major data
-- ============================================================

-- | One value at any node of the tree. Mirrors 'NestedSchema'.
data NestedRow
  = NRLeaf !LeafValue                -- ^ a primitive value (matched against an NSPrimitive node)
  | NRNull                           -- ^ a Nothing at an NSOptional wrapper
  | NRList !(V.Vector NestedRow)     -- ^ a list (possibly empty)
  | NRStruct !(V.Vector NestedRow)   -- ^ struct fields, indexed in NSStruct order
  | NRMapEntries !(V.Vector (NestedRow, NestedRow))
    -- ^ a map, as a vector of (key, value) pairs
  | NRVariantBytes !ByteString !ByteString
    -- ^ Pre-encoded Variant @(metadataBytes, valueBytes)@, matching
    --   the two binary leaves an 'NSVariant' schema emits. Use
    --   'encodeVariant' from "Iceberg.Variant" to obtain the bytes.
  deriving (Show, Eq)

-- | Concrete value carried by an 'NRLeaf'.
data LeafValue
  = LvInt32  !Int32
  | LvInt64  !Int64
  | LvFloat  !Float
  | LvDouble !Double
  | LvBool   !Bool
  | LvString !Text
  | LvBinary !ByteString
  deriving (Show, Eq)

-- ============================================================
-- Shredded leaf
-- ============================================================

-- | One primitive leaf after Dremel shredding.
data NestedLeaf = NestedLeaf
  { nlPath        :: !(V.Vector Text)
  , nlMaxDef      :: !Int
  , nlMaxRep      :: !Int
  , nlDefLevels   :: !(VP.Vector Int32)
  , nlRepLevels   :: !(VP.Vector Int32)
  , nlValueBytes  :: !ByteString
  , nlValueCount  :: !Int
  } deriving (Show, Eq)

-- ============================================================
-- Schema flattening + max levels
-- ============================================================

-- | Walk a 'NestedSchema' producing one entry per primitive leaf, in
-- the same order Parquet's flat schema expects. Each entry carries
-- the leaf's path, its physical type, and the Dremel max definition
-- and repetition levels at the leaf.
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
            -- 'list' is a repeated group, so descending into it
            -- bumps both d and r.
         in go path' (d + 1) (r + 1) inner
      NSMap k v ->
        let !pkv = V.snoc path "key_value"
         in go (V.snoc pkv "key")   (d + 1) (r + 1) k
              ++ go (V.snoc pkv "value") (d + 1) (r + 1) v
      NSStruct children ->
        concatMap (\(name, child) -> go (V.snoc path name) d r child)
                  (V.toList children)
      NSVariant ->
        -- Variant is a 2-leaf binary group. Both leaves are
        -- @required binary@ so descending into them doesn't bump d
        -- or r; the group's own repetition is set by an enclosing
        -- NSOptional / NSRequired (typical Iceberg usage:
        -- @optional NSVariant@).
        [ LeafDescriptor (V.snoc path "metadata") LtBinary d r
        , LeafDescriptor (V.snoc path "value")    LtBinary d r
        ]

-- | Per-leaf metadata used both by the shredder and by
-- 'buildOptionalListFile' to populate the column chunks.
data LeafDescriptor = LeafDescriptor
  { ldPath    :: !(V.Vector Text)
  , ldType    :: !LeafType
  , ldMaxDef  :: !Int
  , ldMaxRep  :: !Int
  } deriving (Show, Eq)

-- ============================================================
-- Shredding
-- ============================================================

-- | Shred a vector of row-major records into one 'NestedLeaf' per
-- primitive leaf in @schema@. Length of 'nlDefLevels' / 'nlRepLevels'
-- on each leaf is the same, equal to the total number of value
-- positions (one per "leaf slot" expanded across all rows).
--
-- Each row is one 'NestedRow' that must structurally match @schema@:
--
-- * 'NRLeaf' under 'NSPrimitive'.
-- * 'NRList' under 'NSList'.
-- * 'NRStruct' under 'NSStruct' with children in the same order.
-- * 'NRMapEntries' under 'NSMap'.
-- * 'NRNull' under any 'NSOptional' wrapper.
--
-- A type-mismatch yields 'Left'.
shred
  :: NestedSchema
  -> V.Vector NestedRow
  -> Either String (V.Vector NestedLeaf)
shred schema rows = do
  let !leaves = flattenSchema "" schema
      !nLeaves = V.length leaves
      seeds = V.replicate nLeaves emptyAcc
  finalAcc <- V.foldM' (\acc row -> shredRow schema acc row 0 0) seeds rows
  Right (V.imap (\i ld -> finalize ld (V.unsafeIndex finalAcc i)) leaves)
  where
    -- def / rep are appended together (one pair per shred event)
    -- so a single 'accLevelCount' keeps both lengths in sync. Use
    -- 'VP.fromListN' with the exact count to skip 'VP.fromList'\'s
    -- size-doubling growth.
    finalize ld acc =
      let !lc = accLevelCount acc
      in NestedLeaf
        { nlPath        = ldPath ld
        , nlMaxDef      = ldMaxDef ld
        , nlMaxRep      = ldMaxRep ld
        , nlDefLevels   = VP.fromListN lc (reverse (accDefs acc))
        , nlRepLevels   = VP.fromListN lc (reverse (accReps acc))
        , nlValueBytes  = BL.toStrict (B.toLazyByteString (accValuesB acc))
        , nlValueCount  = accValueCount acc
        }

-- | Per-leaf accumulator. We push events as we walk each row
-- recursively; finalising reverses the def / rep lists into the
-- 'VP.Vector's the writer expects.
data LeafAcc = LeafAcc
  { accDefs       :: ![Int32]   -- ^ reversed
  , accReps       :: ![Int32]   -- ^ reversed
  , accLevelCount :: !Int
    -- ^ Tracked alongside @accDefs@ / @accReps@ so 'finalize'
    -- can pre-size the 'VP.Vector'. Always equal to
    -- @length accDefs == length accReps@ — the two are
    -- pushed in lock-step by the shredder.
  , accValuesB    :: !B.Builder
  , accValueCount :: !Int
  }

emptyAcc :: LeafAcc
emptyAcc = LeafAcc [] [] 0 mempty 0

-- | Shred one row recursively. The arguments are the schema we're
-- still descending through, the per-leaf accumulators (one slot per
-- leaf in the *original* schema, indexed left-to-right), the row
-- itself, and the current (def, rep) levels.
--
-- Critical invariant: the order in which leaves get appended must
-- match the schema's flatten order, which 'descendThrough' preserves
-- by walking the same way 'flattenSchema' does.
shredRow
  :: NestedSchema
  -> V.Vector LeafAcc
  -> NestedRow
  -> Int        -- ^ current def
  -> Int        -- ^ current rep
  -> Either String (V.Vector LeafAcc)
shredRow schema accs row d r = do
  (newAccs, _) <- descendThrough schema 0 accs row d r 0
  Right newAccs

-- | Descend one schema node into one row value.
--
-- Arguments:
--
--   * @schema@: the (sub)schema to descend.
--   * @cursor@: the leaf-index to write the next event to.
--   * @accs@: per-leaf event accumulators.
--   * @row@: the row value being shredded against @schema@.
--   * @d@: current definition level.
--   * @r@: rep level to emit for the very next leaf event the recursion
--          produces (i.e. the rep level for the FIRST element of any
--          repeated group inside @schema@). Subsequent elements of any
--          repeated group within @schema@ use the absolute rep depth.
--   * @absRep@: total number of repeated groups crossed so far, which
--          determines the rep level for "subsequent siblings" inside
--          a repeated group boundary.
descendThrough
  :: NestedSchema
  -> Int                  -- ^ leafCursor
  -> V.Vector LeafAcc
  -> NestedRow
  -> Int                  -- ^ current def
  -> Int                  -- ^ rep level for the next leaf event
  -> Int                  -- ^ absolute rep depth
  -> Either String (V.Vector LeafAcc, Int)
descendThrough schema !cursor !accs row !d !r !absRep = case (schema, row) of
  -- An NSOptional node either consumes NRNull (which emits a null
  -- event at the *parent's* def for every leaf in the subtree -
  -- this Optional layer wasn't crossed) or unwraps the inner row,
  -- bumping def by 1 to reflect that the layer was crossed.
  (NSOptional inner, NRNull) ->
    Right (emitNullsAcrossSubtree inner cursor accs d r, cursor + leafCount inner)
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

  (NSPrimitive _, NRLeaf v) ->
    let acc  = V.unsafeIndex accs cursor
        acc' = pushLeaf acc d r v
     in Right (V.unsafeUpd accs [(cursor, acc')], cursor + 1)
  (NSPrimitive _, _) ->
    Left "Parquet.Nested: row shape doesn't match NSPrimitive"

  -- NRList under NSList: empty -> emit one "empty list defined"
  -- event per leaf in the subtree (def stays at d, the LIST itself
  -- is defined; the contents aren't). Non-empty -> recurse, with
  -- the FIRST element using the parent's r and subsequent ones
  -- bumping r by 1 (we've crossed a repeated group).
  (NSList inner, NRList children)
    | V.null children ->
        Right ( emitNullsAcrossSubtree inner cursor accs d r
              , cursor + leafCount inner
              )
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
             Left  e          -> Left e
  (NSList _, _) -> Left "Parquet.Nested: row shape doesn't match NSList"

  (NSMap k v, NRMapEntries pairs)
    | V.null pairs ->
        Right
          ( emitNullsAcrossSubtree (NSStruct (V.fromList [("key", k), ("value", v)]))
              cursor accs d r
          , cursor + leafCount k + leafCount v
          )
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
             Left  e          -> Left e
  (NSMap _ _, _) -> Left "Parquet.Nested: row shape doesn't match NSMap"

  (NSVariant, NRVariantBytes metaBs valBs) ->
    -- Push two binary leaves: the metadata and value byte-strings.
    -- Both are 'required binary' so they emit at the current
    -- definition level. Calling code typically wraps NSVariant in
    -- NSOptional, in which case the row is either NRNull (handled
    -- above) or NRVariantBytes for a present Variant.
    let acc1  = V.unsafeIndex accs cursor
        acc1' = pushLeaf acc1 d r (LvBinary metaBs)
        accs1 = V.unsafeUpd accs [(cursor, acc1')]
        acc2  = V.unsafeIndex accs1 (cursor + 1)
        acc2' = pushLeaf acc2 d r (LvBinary valBs)
        accs2 = V.unsafeUpd accs1 [(cursor + 1, acc2')]
     in Right (accs2, cursor + 2)
  (NSVariant, _) ->
    Left "Parquet.Nested: row shape doesn't match NSVariant (expected NRVariantBytes or NRNull)"

  (NSStruct fields, NRStruct values)
    | V.length fields /= V.length values ->
        Left "Parquet.Nested: NSStruct / NRStruct field count mismatch"
    | otherwise ->
        let nFs = V.length fields
            stepField (accs', cur) i =
              let (_, child) = V.unsafeIndex fields i
                  val        = V.unsafeIndex values i
              in descendThrough child cur accs' val d r absRep
        in case foldM stepField (accs, cursor) [0 .. nFs - 1] of
             Right (accs', cur') -> Right (accs', cur')
             Left  e             -> Left e
  (NSStruct _, _) -> Left "Parquet.Nested: row shape doesn't match NSStruct"

-- | Number of primitive leaves under a schema node.
leafCount :: NestedSchema -> Int
leafCount = \case
  NSPrimitive _   -> 1
  NSOptional s    -> leafCount s
  NSRequired s    -> leafCount s
  NSList s        -> leafCount s
  NSMap k v       -> leafCount k + leafCount v
  NSStruct fields -> sum (map (leafCount . snd) (V.toList fields))
  NSVariant       -> 2

-- | Push one (def, rep) event for /every/ leaf in a subtree, with
-- no value. Used for null-list and null-optional events.
emitNullsAcrossSubtree
  :: NestedSchema
  -> Int                  -- ^ leafCursor
  -> V.Vector LeafAcc
  -> Int -> Int           -- ^ def, rep
  -> V.Vector LeafAcc
emitNullsAcrossSubtree sch !cur !accs !d !r =
  let !nLeaves = leafCount sch
      indices  = [cur .. cur + nLeaves - 1]
      updates  = map (\i ->
                        let acc  = V.unsafeIndex accs i
                            acc' = pushNull acc d r
                         in (i, acc'))
                     indices
   in V.unsafeUpd accs updates

pushLeaf :: LeafAcc -> Int -> Int -> LeafValue -> LeafAcc
pushLeaf acc d r v = LeafAcc
  { accDefs       = fromIntegral d : accDefs acc
  , accReps       = fromIntegral r : accReps acc
  , accLevelCount = accLevelCount acc + 1
  , accValuesB    = accValuesB acc <> encodeLeafValue v
  , accValueCount = accValueCount acc + 1
  }

pushNull :: LeafAcc -> Int -> Int -> LeafAcc
pushNull acc d r = LeafAcc
  { accDefs       = fromIntegral d : accDefs acc
  , accReps       = fromIntegral r : accReps acc
  , accLevelCount = accLevelCount acc + 1
  , accValuesB    = accValuesB acc
  , accValueCount = accValueCount acc
  }

encodeLeafValue :: LeafValue -> B.Builder
encodeLeafValue = \case
  LvInt32 v  -> B.int32LE v
  LvInt64 v  -> B.int64LE v
  LvFloat v  -> B.word32LE (castFloatToWord32 v)
  LvDouble v -> B.word64LE (castDoubleToWord64 v)
  LvBool b   -> B.word8 (if b then 1 else 0)
  LvString t ->
    let !bs = TE.encodeUtf8 t
     in B.word32LE (fromIntegral (BS.length bs)) <> B.byteString bs
  LvBinary bs ->
    B.word32LE (fromIntegral (BS.length bs)) <> B.byteString bs

-- A Data.Foldable.foldM-equivalent specialised to lists; we don't
-- want to drag the Foldable typeclass machinery into the inner
-- shredder loop.
foldM :: (s -> a -> Either String s) -> s -> [a] -> Either String s
foldM _ !s [] = Right s
foldM f !s (x : xs) = case f s x of
  Right s' -> foldM f s' xs
  Left  e  -> Left e

-- ============================================================
-- Specialised entry points (kept from the previous PR)
-- ============================================================

-- | @optional list&lt;optional INT32&gt;@ shred wrapper (the Iceberg
-- V3 default-value-array shape). Internally builds a 'NestedSchema'
-- and a 'V.Vector NestedRow' and calls 'shred'.
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
        NRList (V.map (\case
                          Nothing -> NRNull
                          Just v  -> NRLeaf (mkLeaf v))
                      inner)
      !nrows = V.map mkRow rows
   in case shred schema nrows of
        Right ls | V.length ls == 1 ->
          let leaf = V.unsafeIndex ls 0
           in leaf { nlPath = V.fromList [outerName, "list", "element"] }
        Right _ -> error "Parquet.Nested: shred produced wrong leaf count"
        Left e  -> error ("Parquet.Nested: shred failed: " ++ e)

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
      { seName          = outerName
      , seRepetition    = Just Optional
      , seType          = Nothing
      , seNumChildren   = Just 1
      , seConvertedType = Just CTList
      , seLogicalType   = Just LTList
      , seFieldId       = outerFid
      }
  , SchemaElement
      { seName          = "list"
      , seRepetition    = Just Repeated
      , seType          = Nothing
      , seNumChildren   = Just 1
      , seConvertedType = Nothing
      , seLogicalType   = Nothing
      , seFieldId       = Nothing
      }
  , SchemaElement
      { seName          = "element"
      , seRepetition    = Just Optional
      , seType          = Just leafType
      , seNumChildren   = Nothing
      , seConvertedType = Nothing
      , seLogicalType   = Nothing
      , seFieldId       = leafFid
      }
  ]

buildOptionalListFile
  :: ParquetType
  -> NestedLeaf
  -> Int
  -> ByteString
buildOptionalListFile leafType leaf numRows =
  let !defStream = LE.encodeRLEHybrid 2 (nlDefLevels leaf)
      !repStream = LE.encodeRLEHybrid 1 (nlRepLevels leaf)
      !defLen = BS.length defStream
      !repLen = BS.length repStream
      !valBytes = nlValueBytes leaf
      !numEvents = VP.length (nlDefLevels leaf)
      !totalUncomp = repLen + defLen + BS.length valBytes
      !hdr = PP.PageHeader
        { PP.phType = PP.PtDataPageV2 PP.DataPageHeaderV2
            { PP.dph2NumValues    = fromIntegral numEvents
            , PP.dph2NumNulls     = fromIntegral (numEvents - nlValueCount leaf)
            , PP.dph2NumRows      = fromIntegral numRows
            , PP.dph2Encoding     = 0
            , PP.dph2DefLevelsLen = fromIntegral defLen
            , PP.dph2RepLevelsLen = fromIntegral repLen
            , PP.dph2IsCompressed = False
            }
        , PP.phUncompressedPageSize = Just (fromIntegral totalUncomp)
        , PP.phCompressedPageSize   = Just (fromIntegral totalUncomp)
        }
      !pageBytes = PW.encodePageHeader hdr <> repStream <> defStream <> valBytes

      !schema = V.fromList
        ( SchemaElement
            { seName          = "schema"
            , seRepetition    = Nothing
            , seType          = Nothing
            , seNumChildren   = Just 1
            , seConvertedType = Nothing
            , seLogicalType   = Nothing
            , seFieldId       = Nothing
            }
        : optionalListSchemaSegments
            (V.unsafeIndex (nlPath leaf) 0)
            leafType Nothing Nothing
        )

      !colChunk = ColumnChunk
        { ccFilePath          = Nothing
        , ccFileOffset        = 4
        , ccMetadata          = Just ColumnMetadata
            { cmType                  = leafType
            , cmEncodings             = V.singleton Plain
            , cmPathInSchema          = nlPath leaf
            , cmCodec                 = Uncompressed
            , cmNumValues             = fromIntegral numEvents
            , cmTotalUncompressedSize = fromIntegral (BS.length pageBytes)
            , cmTotalCompressedSize   = fromIntegral (BS.length pageBytes)
            , cmDataPageOffset        = 4
            , cmDictionaryPageOffset = Nothing
            , cmStatistics            = Just Statistics
                { statMin = Nothing, statMax = Nothing
                , statNullCount = Just (fromIntegral (numEvents - nlValueCount leaf))
                , statDistinctCount = Nothing
                , statMinValue = Nothing, statMaxValue = Nothing
                }
            , cmBloomFilterOffset     = Nothing
            , cmBloomFilterLength     = Nothing
            }
        , ccOffsetIndexOffset = Nothing
        , ccOffsetIndexLength = Nothing
        , ccColumnIndexOffset = Nothing
        , ccColumnIndexLength = Nothing
        }
      !rg = RowGroup
        { rgColumns       = V.singleton colChunk
        , rgTotalByteSize = fromIntegral (BS.length pageBytes)
        , rgNumRows       = fromIntegral numRows
        , rgSortingColumns = Nothing
        }
      !fm = FileMetadata
        { fmVersion   = 2
        , fmSchema    = schema
        , fmNumRows   = fromIntegral numRows
        , fmRowGroups = V.singleton rg
        , fmCreatedBy = Just "wireform"
        , fmColumnOrders = Nothing
        }
   in PW.writeParquetFile fm (V.singleton (V.singleton pageBytes))

-- ============================================================
-- Schema serialisation
-- ============================================================

-- | Walk a top-level @(name, NestedSchema)@ vector and produce the
-- flat 'V.Vector SchemaElement' Parquet's footer expects. The first
-- element is always the row-group root; the rest are the schema
-- nodes in pre-order DFS, with @num_children@ populated on every
-- internal node.
nestedSchemaToFlatSchema
  :: V.Vector (Text, NestedSchema)
  -> V.Vector SchemaElement
nestedSchemaToFlatSchema columns =
  let !root = SchemaElement
        { seName          = "schema"
        , seRepetition    = Nothing
        , seType          = Nothing
        , seNumChildren   = Just (fromIntegral (V.length columns))
        , seConvertedType = Nothing
        , seLogicalType   = Nothing
        , seFieldId       = Nothing
        }
      !cols = concatMap (\(name, sch) -> emitColumn name sch)
                         (V.toList columns)
   in V.fromList (root : cols)

-- | Emit a column's flat schema entries in pre-order DFS. The
-- top-level element gets a name and (for compound types) the
-- LogicalType / ConvertedType annotation.
emitColumn :: Text -> NestedSchema -> [SchemaElement]
emitColumn outerName outerSchema = goTop outerName outerSchema
  where
    -- Top-level: peel the outer Optional / Required wrappers,
    -- recording the repetition. The actual /child/ that gets the
    -- name and annotations is what's underneath.
    goTop name (NSOptional inner) = goNamed name (Just Optional) inner
    goTop name (NSRequired inner) = goNamed name (Just Required) inner
    goTop name other              = goNamed name (Just Required) other

    -- A "named" node: pick its annotation + group-or-leaf shape.
    goNamed name rep node = case node of
      NSPrimitive lt ->
        [SchemaElement
           { seName          = name
           , seRepetition    = rep
           , seType          = Just (leafParquetType lt)
           , seNumChildren   = Nothing
           , seConvertedType = leafConvertedType lt
           , seLogicalType   = leafLogicalType lt
           , seFieldId       = Nothing
           }]
      NSOptional inner -> goNamed name (Just Optional) inner
      NSRequired inner -> goNamed name (Just Required) inner
      NSList inner ->
        let !groupHead = SchemaElement
              { seName          = name
              , seRepetition    = rep
              , seType          = Nothing
              , seNumChildren   = Just 1
              , seConvertedType = Just CTList
              , seLogicalType   = Just LTList
              , seFieldId       = Nothing
              }
            !listGroup = SchemaElement
              { seName          = "list"
              , seRepetition    = Just Repeated
              , seType          = Nothing
              , seNumChildren   = Just 1
              , seConvertedType = Nothing
              , seLogicalType   = Nothing
              , seFieldId       = Nothing
              }
         in groupHead : listGroup : goNamed "element" (Just Required) inner
      NSMap k v ->
        let !groupHead = SchemaElement
              { seName          = name
              , seRepetition    = rep
              , seType          = Nothing
              , seNumChildren   = Just 1
              , seConvertedType = Just CTMap
              , seLogicalType   = Just LTMap
              , seFieldId       = Nothing
              }
            !kvGroup = SchemaElement
              { seName          = "key_value"
              , seRepetition    = Just Repeated
              , seType          = Nothing
              , seNumChildren   = Just 2
              , seConvertedType = Nothing
              , seLogicalType   = Nothing
              , seFieldId       = Nothing
              }
         in groupHead : kvGroup
              : goNamed "key"   (Just Required) k
              ++ goNamed "value" (Just Required) v
      NSStruct fields ->
        let !groupHead = SchemaElement
              { seName          = name
              , seRepetition    = rep
              , seType          = Nothing
              , seNumChildren   = Just (fromIntegral (V.length fields))
              , seConvertedType = Nothing
              , seLogicalType   = Nothing
              , seFieldId       = Nothing
              }
         in groupHead
              : concatMap (\(fname, fch) -> goNamed fname (Just Required) fch)
                          (V.toList fields)
      NSVariant ->
        -- The Iceberg V3 / Spark Variant unshredded shape is:
        --   <rep> group <name> {
        --     required binary metadata;
        --     required binary value;
        --   }
        -- The 'VARIANT(1)' annotation is a LogicalType the writer
        -- side doesn't currently emit to the footer thrift (see
        -- comment in Parquet.Footer); the physical layout is what
        -- pyarrow / Spark match against and that's what we ensure.
        let !groupHead = SchemaElement
              { seName          = name
              , seRepetition    = rep
              , seType          = Nothing
              , seNumChildren   = Just 2
              , seConvertedType = Nothing
              , seLogicalType   = Nothing
              , seFieldId       = Nothing
              }
            !metadataLeaf = SchemaElement
              { seName          = "metadata"
              , seRepetition    = Just Required
              , seType          = Just PTByteArray
              , seNumChildren   = Nothing
              , seConvertedType = Nothing
              , seLogicalType   = Nothing
              , seFieldId       = Nothing
              }
            !valueLeaf = SchemaElement
              { seName          = "value"
              , seRepetition    = Just Required
              , seType          = Just PTByteArray
              , seNumChildren   = Nothing
              , seConvertedType = Nothing
              , seLogicalType   = Nothing
              , seFieldId       = Nothing
              }
         in [groupHead, metadataLeaf, valueLeaf]

-- ============================================================
-- Whole-file builder for arbitrary nested schemas
-- ============================================================

-- | Build a complete Parquet file from a vector of top-level
-- @(name, NestedSchema)@ columns plus a vector of row-major
-- 'NestedRow' values, one per column. Row count = length of the
-- inner vector for any column (they must all match).
--
-- The file layout is one DATA_PAGE_V2 per primitive leaf, no
-- compression, no bloom filter / page index. Suitable for the
-- pyarrow round-trip test that ground-truths the shred against
-- arrow-cpp's reader.
buildNestedFile
  :: V.Vector (Text, NestedSchema)
  -> V.Vector (V.Vector NestedRow)         -- ^ outer index = column, inner = row
  -> Either String ByteString
buildNestedFile columns rowsPerColumn
  | V.length columns /= V.length rowsPerColumn =
      Left "Parquet.Nested.buildNestedFile: column count mismatch"
  | V.null columns = Left "Parquet.Nested.buildNestedFile: at least one column required"
  | otherwise = do
      let !numRows = V.length (V.unsafeIndex rowsPerColumn 0)
      -- Sanity: every column must have the same row count.
      let !rowCounts = V.map V.length rowsPerColumn
      if V.any (/= numRows) rowCounts
        then Left "Parquet.Nested.buildNestedFile: row counts differ between columns"
        else do
          shreds <- V.imapM
            (\i (_name, sch) ->
                let !rows = V.unsafeIndex rowsPerColumn i
                 in shred sch rows)
            columns
          -- Each column may produce >= 1 leaf; concatenate.
          let !allLeaves = V.concat (V.toList shreds)
              !flatSchema = nestedSchemaToFlatSchema columns
              -- Patch each leaf's path so that its first element is
              -- the column name; the shredder uses "" by default.
              !columnNames = V.toList (V.map fst columns)
              !leavesByCol = V.toList shreds
              !pathedLeaves =
                concat (zipWith
                          (\name leaves ->
                              map (\l -> l { nlPath = V.cons name (V.tail (nlPath l)) })
                                  (V.toList leaves))
                          columnNames
                          leavesByCol)
          -- Encode each leaf as one DATA_PAGE_V2.
          let !pageBytesList = map encodeNestedLeafAsV2Page pathedLeaves
              !rowGroupBytes = mconcat pageBytesList
              !startOfData   = 4 :: Int
          -- Build per-column metadata, threading the file-relative
          -- offset.
          let buildCol :: Int -> [(NestedLeaf, ByteString)] -> [ColumnChunk]
              buildCol _ [] = []
              buildCol off ((leaf, pgBytes) : rest) =
                let sz = BS.length pgBytes
                    -- Find the matching descriptor (recompute) for
                    -- the leaf's primitive type. Walk
                    -- 'flattenSchema' for that column - simple lookup
                    -- by path.
                    ldType' = case findLeafType columns (nlPath leaf) of
                      Just t  -> leafParquetType t
                      Nothing -> PTByteArray
                    cm = ColumnMetadata
                      { cmType                  = ldType'
                      , cmEncodings             = V.singleton Plain
                      , cmPathInSchema          = V.tail (nlPath leaf)
                      , cmCodec                 = Uncompressed
                      , cmNumValues             = fromIntegral
                          (VP.length (nlDefLevels leaf))
                      , cmTotalUncompressedSize = fromIntegral sz
                      , cmTotalCompressedSize   = fromIntegral sz
                      , cmDataPageOffset        = fromIntegral off
                      , cmDictionaryPageOffset = Nothing
                      , cmStatistics            = Just Statistics
                          { statMin = Nothing, statMax = Nothing
                          , statNullCount = Just (fromIntegral
                              (VP.length (nlDefLevels leaf) - nlValueCount leaf))
                          , statDistinctCount = Nothing
                          , statMinValue = Nothing, statMaxValue = Nothing
                          }
                      , cmBloomFilterOffset = Nothing
                      , cmBloomFilterLength = Nothing
                      }
                    cc = ColumnChunk
                      { ccFilePath          = Nothing
                      , ccFileOffset        = fromIntegral off
                      , ccMetadata          = Just cm
                      , ccOffsetIndexOffset = Nothing
                      , ccOffsetIndexLength = Nothing
                      , ccColumnIndexOffset = Nothing
                      , ccColumnIndexLength = Nothing
                      }
                 in cc : buildCol (off + sz) rest
              !columnChunks = buildCol startOfData
                                (zip pathedLeaves pageBytesList)
              !rg = RowGroup
                { rgColumns       = V.fromList columnChunks
                , rgTotalByteSize = fromIntegral (BS.length rowGroupBytes)
                , rgNumRows       = fromIntegral numRows
                , rgSortingColumns = Nothing
                }
              !fm = FileMetadata
                { fmVersion   = 2
                , fmSchema    = flatSchema
                , fmNumRows   = fromIntegral numRows
                , fmRowGroups = V.singleton rg
                , fmCreatedBy = Just "wireform"
                , fmColumnOrders = Nothing
                }
          Right (PW.writeParquetFile fm
                   (V.singleton (V.fromList pageBytesList)))
  where
    -- Find a leaf's primitive type by walking the schema along its
    -- (unrooted) path. The path's first element is the column name
    -- (after the V.cons fixup above); subsequent are 'list',
    -- 'key_value', struct field names, and the terminal leaf name
    -- ('element', 'key', 'value', or a struct field name).
    findLeafType columns' path
      | V.null path = Nothing
      | otherwise =
          let colName = V.unsafeIndex path 0
              rest    = V.tail path
           in case V.find (\(n, _) -> n == colName) columns' of
                Just (_, sch) -> walkSchema sch rest
                Nothing       -> Nothing

    walkSchema (NSPrimitive lt) p
      | V.null p  = Just lt
      | V.length p == 1 = Just lt  -- terminal leaf name (struct field, "element", "key", "value")
      | otherwise = Nothing
    walkSchema (NSOptional s) p   = walkSchema s p
    walkSchema (NSRequired s) p   = walkSchema s p
    walkSchema (NSList s) p
      | V.length p >= 2 && V.unsafeIndex p 0 == "list" && V.unsafeIndex p 1 == "element" =
          walkSchema s (V.drop 2 p)
      | otherwise = Nothing
    walkSchema (NSMap k v) p
      | V.length p >= 2 && V.unsafeIndex p 0 == "key_value" =
          let next = V.unsafeIndex p 1
              rest = V.drop 2 p
           in case next of
                "key"   -> walkSchema k rest
                "value" -> walkSchema v rest
                _       -> Nothing
      | otherwise = Nothing
    walkSchema (NSStruct fields) p
      | V.null p  = Nothing
      | otherwise =
          let h = V.unsafeIndex p 0
              t = V.tail p
           in case V.find (\(n, _) -> n == h) fields of
                Just (_, child) -> walkSchema child t
                Nothing         -> Nothing
    walkSchema NSVariant p
      | V.length p == 1 && (V.unsafeIndex p 0 == "metadata"
                             || V.unsafeIndex p 0 == "value") = Just LtBinary
      | otherwise = Nothing

-- | Encode one shredded leaf as a single uncompressed DATA_PAGE_V2.
encodeNestedLeafAsV2Page :: NestedLeaf -> ByteString
encodeNestedLeafAsV2Page leaf =
  let !defStream = LE.encodeRLEHybrid (LE.bitWidthFor (nlMaxDef leaf)) (nlDefLevels leaf)
      !repStream = LE.encodeRLEHybrid (LE.bitWidthFor (nlMaxRep leaf)) (nlRepLevels leaf)
      !defLen = BS.length defStream
      !repLen = BS.length repStream
      !valBytes = nlValueBytes leaf
      !numEvents = VP.length (nlDefLevels leaf)
      !totalUncomp = repLen + defLen + BS.length valBytes
      !hdr = PP.PageHeader
        { PP.phType = PP.PtDataPageV2 PP.DataPageHeaderV2
            { PP.dph2NumValues    = fromIntegral numEvents
            , PP.dph2NumNulls     = fromIntegral (numEvents - nlValueCount leaf)
            , PP.dph2NumRows      = fromIntegral numEvents -- not strictly correct for nested
                                                            -- but pyarrow ignores this field
                                                            -- when the column is nested.
            , PP.dph2Encoding     = 0
            , PP.dph2DefLevelsLen = fromIntegral defLen
            , PP.dph2RepLevelsLen = fromIntegral repLen
            , PP.dph2IsCompressed = False
            }
        , PP.phUncompressedPageSize = Just (fromIntegral totalUncomp)
        , PP.phCompressedPageSize   = Just (fromIntegral totalUncomp)
        }
   in PW.encodePageHeader hdr <> repStream <> defStream <> valBytes

-- silence unused 'leafConvertedType' / 'leafLogicalType' / 'leafParquetType' warnings
_unusedHelpers :: (LeafType -> ParquetType, LeafType -> Maybe ConvertedType, LeafType -> Maybe LogicalType)
_unusedHelpers = (leafParquetType, leafConvertedType, leafLogicalType)
