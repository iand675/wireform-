{-# LANGUAGE OverloadedStrings #-}

module VariantParquetRegression (tests) where

import Arrow.Column qualified as C
import Arrow.Types qualified as A
import Control.Exception (evaluate)
import Control.Monad (forM_, unless)
import Data.ByteString qualified as BS
import Data.Either (isLeft, isRight)
import Data.Int (Int32)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Vector qualified as V
import GHC.Conc (getAllocationCounter)
import Parquet.Arrow qualified as Arrow
import Parquet.Nested qualified as N
import Parquet.Read qualified as Read
import Parquet.Types qualified as P
import Parquet.Variant qualified as Variant
import Parquet.Variant.Parquet qualified as VP
import Parquet.Variant.Schema qualified as VariantSchema
import VariantRegression qualified as Cases


load :: Either String a -> IO a
load = either fail pure


tests :: IO ()
tests = do
  let check name condition = unless condition (fail ("Variant Parquet regression: " ++ name))
      values = V.cons Nothing (Just <$> Cases.values)
      canonical Nothing = Right Nothing
      canonical (Just value) = Just <$> Variant.encodeVariant value
  bytes <- load (VP.buildVariantParquetFile "v" values)
  file <- load (Read.loadParquetFile bytes)
  output <- load (VP.readVariantColumn file 0 "v")
  check "all primitive/container payloads and IEEE bits survive the file" (traverse canonical output == traverse canonical values)
  let footer = Read.pfFooter file
      schema = P.fmSchema footer
      withoutAnnotation = file {Read.pfFooter = footer {P.fmSchema = V.imap (\i element -> if i == 1 then element {P.seLogicalType = Nothing} else element) schema}}
  check "native writer emits VARIANT(1)" (P.seLogicalType (schema V.! 1) == Just (P.LTVariant 1))
  check "semantic file API requires annotation" (isLeft (VP.readVariantColumn withoutAnnotation 0 "v"))
  check "missing column fails" (isLeft (VP.readVariantColumn file 0 "absent"))
  -- Reorder footer children and column descriptors without touching their
  -- data bytes. The semantic reader must use names instead of positions.
  let reorderedSchema = V.fromList [schema V.! 0, schema V.! 1, schema V.! 3, schema V.! 2]
      reorderedGroups = V.map (\group -> group {P.rgColumns = V.reverse (P.rgColumns group)}) (P.fmRowGroups footer)
      reordered = file {Read.pfFooter = footer {P.fmSchema = reorderedSchema, P.fmRowGroups = reorderedGroups}}
  reorderedValues <- load (VP.readVariantColumn reordered 0 "v")
  check "metadata/value fields are accessed by name" (traverse canonical reorderedValues == traverse canonical values)
  forM_ [0, 2] $ \version -> do
    let changed = file {Read.pfFooter = footer {P.fmSchema = schema V.// [(1, (schema V.! 1) {P.seLogicalType = Just (P.LTVariant version)})]}}
    check "unknown annotation version is not interpreted as v1" (isLeft (VP.readVariantColumn changed 0 "v"))
  forM_ Cases.malformed $ \(name, metadata, value) ->
    check ("writer rejects malformed " ++ name) (isLeft (N.buildNestedFile (V.singleton ("v", N.NSVariant)) (V.singleton (V.singleton (N.NRVariantBytes metadata value)))))
  let invalidVariant = N.NRVariantBytes (BS.pack [1, 0, 0]) (BS.singleton 84)
      integer = N.NRLeaf (N.LvInt32 1)
      nestedShapes =
        [ (N.NSStruct (V.fromList [("ordinary", N.NSPrimitive N.LtInt32), ("values", N.NSList (N.NSOptional N.NSVariant))]), N.NRStruct (V.fromList [integer, N.NRList (V.fromList [N.NRNull, invalidVariant])]))
        , (N.NSMap N.NSVariant (N.NSPrimitive N.LtInt32), N.NRMapEntries (V.singleton (invalidVariant, integer)))
        , (N.NSMap (N.NSPrimitive N.LtInt32) (N.NSOptional N.NSVariant), N.NRMapEntries (V.singleton (integer, invalidVariant)))
        ]
  forM_ nestedShapes $ \(shape, row) ->
    check "deferred semantic validation visits nested structs/lists/map keys and values" (isLeft (N.shred shape (V.singleton row)))
  -- Ordinary unannotated binary data must remain byte preserving even when
  -- a caller chooses the names metadata/value and the bytes aren't Variants.
  raw <- load (N.buildNestedFile (V.singleton ("v", N.NSStruct (V.fromList [("metadata", N.NSPrimitive N.LtBinary), ("value", N.NSPrimitive N.LtBinary)]))) (V.singleton (V.singleton (N.NRStruct (V.fromList [N.NRLeaf (N.LvBinary "invalid"), N.NRLeaf (N.LvBinary "bytes")])))))
  rawFile <- load (Read.loadParquetFile raw)
  rawSchema <- load (Arrow.parquetFileArrowSchemaChecked rawFile)
  check "ordinary binary reads preserve arbitrary bytes" (isRight (Arrow.parquetRowGroupToArrow rawSchema rawFile 0))
  let sample = V.fromList [Just (Variant.VInt8 4), Nothing, Just Variant.VNull, Just (Variant.VObject (M.singleton "nested" (Variant.VBool True)))]
  encoded <- V.mapM (maybe (pure Nothing) (fmap Just . load . Variant.encodeVariant)) sample
  let metadata = V.imap (\i value -> if i == 1 then Just "garbage" else Variant.variantMetadata <$> value) encoded
      payload = V.imap (\i value -> if i == 1 then Just "garbage" else Variant.variantValue <$> value) encoded
      validity = C.validityFromBools (V.fromList [True, False, True, True])
  forM_ [(C.fromMaybeByteStrings metadata, C.fromMaybeByteStrings payload), (C.fromMaybeLargeByteStrings metadata, C.fromMaybeLargeByteStrings payload), (C.fromMaybeBinaryView metadata, C.fromMaybeBinaryView payload)] $ \(m, v) -> do
    column <- load (C.mkStruct 4 validity (V.fromList [("value", v), ("metadata", m)]))
    reader <- load (VP.compileVariantReader column)
    actual <- load (V.generateM 4 (VP.variantAt reader))
    check "parent null masks arbitrary children in all binary storage profiles" (traverse canonical actual == traverse canonical sample)
    check "Variant null and absent slot stay distinct" (actual V.! 1 == Nothing && actual V.! 2 == Just Variant.VNull)
    check "row bounds rejected" (isLeft (VP.variantAt reader (-1)) && isLeft (VP.variantAt reader 4))
  missingMetadata <- load (C.mkStruct 1 Nothing (V.fromList [("metadata", C.fromMaybeByteStrings (V.singleton Nothing)), ("value", C.fromMaybeByteStrings (V.singleton (Just (BS.singleton 0))))]))
  reader <- load (VP.compileVariantReader missingMetadata)
  check "present parent requires metadata" (isLeft (VP.variantAt reader 0))
  let deep = foldr (\_ value -> Variant.VArray (V.singleton value)) Variant.VNull [1 .. 501 :: Int]
      raised = Variant.VariantLimits 501 502 0
  deepBytes <- load (VP.buildVariantParquetFileWithLimits raised "v" (V.singleton (Just deep)))
  deepFile <- load (Read.loadParquetFile deepBytes)
  check "default depth limit applies on semantic file reads" (isLeft (VP.readVariantColumn deepFile 0 "v"))
  deepOutput <- load (VP.readVariantColumnWithLimits raised deepFile 0 "v")
  check "explicit resource override works through both file APIs" (deepOutput == V.singleton (Just deep))
  -- The dictionary is valid and large, so a test using arbitrary invalid
  -- bytes would miss eager validation before the cumulative page bound.
  bigMetadata <- load (Variant.buildVariantMetadata (V.singleton (T.replicate (1024 * 1024) "a")))
  let dictionaryBytes = Variant.metadataBytes bigMetadata
      count = fromIntegral (maxBound :: Int32) `quot` (BS.length dictionaryBytes + 4) + 1
      bigRows = V.replicate count (N.NRVariantBytes dictionaryBytes (BS.singleton 0))
  _ <- evaluate (BS.length dictionaryBytes)
  _ <- evaluate (V.length bigRows)
  forM_ [N.shred N.NSVariant bigRows >> pure (), N.buildNestedFile (V.singleton ("v", N.NSVariant)) (V.singleton bigRows) >> pure ()] $ \operation -> do
    before <- getAllocationCounter
    rejected <- evaluate (isLeft operation)
    after <- getAllocationCounter
    check "cumulative size bound precedes valid dictionary parsing" rejected
    check ("rejected valid Variant page stays below allocation bound: " ++ show (before - after)) (before - after < 8 * 1024 * 1024)
  nativeTests
  putStrLn "Variant Parquet annotation/semantic/null/limits regressions passed"


-- Build real encoded physical leaves with the normal nested writer, then
-- annotate the parsed group. The external suite also tests actual foreign
-- footers; these cases isolate native schema/column/projection invariants.
nativeTests :: IO ()
nativeTests = do
  let check name good = unless good (fail ("native Variant regression: " ++ name))
      annotate file =
        let footer = Read.pfFooter file
            schema = P.fmSchema footer
        in file {Read.pfFooter = footer {P.fmSchema = schema V.// [(1, (schema V.! 1) {P.seLogicalType = Just (P.LTVariant 1)})]}}
      metadata = N.NRLeaf (N.LvBinary (BS.pack [1, 0, 0]))
      value = N.NRLeaf (N.LvBinary (BS.singleton 0))
      make shape rows = annotate <$> (load . Read.loadParquetFile =<< load (N.buildNestedFile (V.singleton ("v", shape)) (V.singleton rows)))
      primitive leaf = N.NSStruct (V.fromList [("metadata", N.NSPrimitive N.LtBinary), ("value", N.NSOptional (N.NSPrimitive N.LtBinary)), ("typed_value", N.NSOptional (N.NSPrimitive leaf))])
      row typed = N.NRStruct (V.fromList [metadata, N.NRNull, N.NRLeaf typed])
      cases =
        [ (N.LtDecimal 9 2, N.LvInt32 12345, A.ADecimal32 9 2, Variant.VDecimal4 2 12345)
        , (N.LtDecimal 10 3, N.LvInt64 (-12345), A.ADecimal64 10 3, Variant.VDecimal8 3 (-12345))
        , (N.LtDecimal 19 4, N.LvFixedBinary (BS.replicate 14 0 <> BS.pack [48, 57]), A.ADecimal 19 4, Variant.VDecimal16 4 12345)
        , (N.LtUuid, N.LvFixedBinary (BS.pack [0 .. 15]), A.AFixedSizeBinary 16, Variant.VUuid (BS.pack [0 .. 15]))
        , (N.LtTimestamp True P.LtNanos, N.LvInt64 0, A.ATimestamp A.Nanosecond (Just "UTC"), Variant.VTimestampNanos 0)
        ]
  forM_ cases $ \(leaf, input, typ, expected) -> do
    file <- make (primitive leaf) (V.fromList [row input, N.NRStruct (V.fromList [metadata, value, N.NRNull])])
    schema <- load (Arrow.parquetFileArrowSchemaChecked file)
    let field = V.head (A.arrowFields schema)
        typed = A.fieldChildren field V.! 2
    check "native field carries canonical extension" (VariantSchema.isVariantField field)
    check "exact typed decimal/timestamp/UUID field type" (A.fieldType typed == typ)
    load (VariantSchema.validateVariantField field)
    actual <- load (VP.readVariantColumn file 0 "v")
    check "native typed value preserves Variant identity and null" (actual == V.fromList [Just expected, Just Variant.VNull])
  intFile <- make (primitive N.LtInt32) (V.singleton (row (N.LvInt32 7)))
  schema <- load (Arrow.parquetFileArrowSchemaChecked intFile)
  let field = V.head (A.arrowFields schema)
      fields = A.fieldChildren field
      target changed = schema {A.arrowFields = V.singleton changed}
      widened = field {A.fieldChildren = fields V.// [(2, (fields V.! 2) {A.fieldType = A.AInt 64 True})]}
      dropped = field {A.fieldChildren = V.filter ((/= "value") . A.fieldName) fields}
  check "retained Variant extension forbids integer widening" (isLeft (Arrow.parquetRowGroupToArrow (target widened) intFile 0))
  check "retained Variant extension forbids dropping fallback storage" (isLeft (Arrow.parquetRowGroupToArrow (target dropped) intFile 0))
  check "explicit raw projection may widen and omit fields" (isRight (Arrow.parquetRowGroupToArrow (target widened {A.fieldMetadata = V.empty, A.fieldChildren = V.singleton ((fields V.! 2) {A.fieldType = A.AInt 64 True})}) intFile 0))
  let reordered = field {A.fieldChildren = V.reverse fields}
  check "retained Variant permits lossless field reorder" (isRight (Arrow.parquetRowGroupToArrow (target reordered) intFile 0))
  forM_ [N.LtDecimal 9 2, N.LtDecimal 18 2] $ \leaf -> do
    file <- make (primitive leaf) (V.singleton (row (if leaf == N.LtDecimal 9 2 then N.LvInt32 1 else N.LvInt64 1)))
    let footer = Read.pfFooter file
        original = P.fmSchema footer
        leafIndex = V.length original - 1
        native = original V.! leafIndex
        invalid = native {P.seType = if P.seType native == Just P.PTInt32 then Just P.PTInt64 else Just P.PTInt32}
        malformed = file {Read.pfFooter = footer {P.fmSchema = original V.// [(leafIndex, invalid)]}}
    check "wrong native decimal precision carrier rejected before decode" (isLeft (Arrow.parquetFileArrowSchemaChecked malformed))
  forM_ [N.LtInteger 32 False, N.LtTimestamp False P.LtMillis, N.LtTime True P.LtMicros, N.LtJson, N.LtFixedBinary 16] $ \leaf -> do
    -- No rows are needed: schema validity must not depend on row-group data.
    file <- make (primitive leaf) V.empty
    check "noncanonical typed primitive rejected on empty data" (isLeft (Arrow.parquetFileArrowSchemaChecked file))
  -- A LIST wrapper called array normally triggers the legacy two-level
  -- heuristic. Known Variant lists must always retain the required element.
  let element = N.NSStruct (V.singleton ("typed_value", N.NSOptional (N.NSPrimitive N.LtInt32)))
      arrayShape = N.NSStruct (V.fromList [("metadata", N.NSPrimitive N.LtBinary), ("typed_value", N.NSOptional (N.NSList element))])
      arrayRow = N.NRStruct (V.fromList [metadata, N.NRList (V.fromList [N.NRStruct (V.singleton (N.NRLeaf (N.LvInt32 3))), N.NRStruct (V.singleton N.NRNull)])])
  file <- make arrayShape (V.singleton arrayRow)
  let footer = Read.pfFooter file
      original = P.fmSchema footer
      renamed = V.map (\se -> if P.seName se == "list" then se {P.seName = "array"} else se) original
      renamePath name = if name == "list" then "array" else name
      groups = V.map (\group -> group {P.rgColumns = V.map (\column -> column {P.ccMetadata = fmap (\meta -> meta {P.cmPathInSchema = V.map renamePath (P.cmPathInSchema meta)}) (P.ccMetadata column)}) (P.rgColumns group)}) (P.fmRowGroups footer)
      changed = file {Read.pfFooter = footer {P.fmSchema = renamed, P.fmRowGroups = groups}}
  actual <- load (VP.readVariantColumn changed 0 "v")
  check "exact Variant three-level list bypasses legacy-name heuristic" (actual == V.singleton (Just (Variant.VArray (V.fromList [Variant.VInt32 3, Variant.VNull]))))

  let mutate file change =
        let footer = Read.pfFooter file
        in file {Read.pfFooter = footer {P.fmSchema = change (P.fmSchema footer)}}
      changeLast f elements = elements V.// [(V.length elements - 1, f (V.last elements))]
  forM_ [(8, True, 128), (16, True, 32768), (8, False, 256), (16, False, 65536)] $ \(width, signed, badValue) -> do
    wide <- make (primitive N.LtInt32) (V.singleton (row (N.LvInt32 badValue)))
    let invalid = mutate wide (changeLast (\se -> se {P.seLogicalType = Just (P.LTInteger width signed)}))
    check "out-of-domain narrow integer is rejected instead of wrapping" (isLeft (VP.readVariantColumn invalid 0 "v"))
  decimal <- make (primitive (N.LtDecimal 9 0)) (V.singleton (row (N.LvInt32 999999999)))
  let outOfRange = mutate decimal (changeLast (\se -> se {P.seLogicalType = Just (P.LTDecimal 8 0)}))
  check "narrow decimal buffer enforces declared precision" (isLeft (VP.readVariantColumn outOfRange 0 "v"))
  forM_ [(2, Just P.Optional), (3, Just P.Required), (4, Just P.Required)] $ \(index, repetition) -> do
    let malformed = mutate intFile (\elements -> elements V.// [(index, (elements V.! index) {P.seRepetition = repetition})])
    check "Variant metadata/value/typed repetition is validated before decode" (isLeft (Arrow.parquetFileArrowSchemaChecked malformed))
  forM_ [0, 1] $ \count -> do
    unshredded <- make (N.NSStruct (V.fromList [("metadata", N.NSPrimitive N.LtBinary), ("value", N.NSPrimitive N.LtBinary)])) (V.replicate count (N.NRStruct (V.fromList [metadata, value])))
    let zeroTyped =
          mutate
            unshredded
            ( \elements ->
                let parent = (elements V.! 1) {P.seNumChildren = Just 3}
                    fallback = (elements V.! 3) {P.seRepetition = Just P.Optional}
                    typed = P.SchemaElement "typed_value" (Just P.Optional) Nothing (Just 0) Nothing Nothing Nothing Nothing
                in (elements V.// [(1, parent), (3, fallback)]) `V.snoc` typed
            )
    inferred <- load (Arrow.parquetFileArrowSchemaChecked zeroTyped)
    if count == 0
      then check "zero-row native empty typed object has exact canonical schema" (isRight (Arrow.parquetRowGroupToArrow inferred zeroTyped 0))
      else check "optional zero-leaf object presence cannot be invented" (isLeft (Arrow.parquetRowGroupToArrow inferred zeroTyped 0))
