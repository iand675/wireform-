{-# LANGUAGE OverloadedStrings #-}

module Selection (run) where

import Arrow.Column qualified as A
import Arrow.Column.Internal qualified as AI
import Arrow.Column.Buffer qualified as Buffer
import Arrow.Types qualified as T
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (evaluate, finally)
import Control.Monad (replicateM_, unless, when)
import Data.Bits (setBit, shiftL, shiftR, testBit, xor, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Lazy qualified as Lazy
import Data.Either (isLeft, isRight)
import Data.Int (Int32, Int64)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as TextValue
import Data.Text.Encoding qualified as Text
import Data.Vector qualified as V
import Data.Vector.Generic.Base qualified as VG
import Data.Vector.Primitive qualified as P
import Data.Vector.Storable qualified as S
import Data.Word (Word32, Word64, Word8)
import Foreign.ForeignPtr (mallocForeignPtrBytes)
import GHC.Conc (getAllocationCounter)
import GHC.Float (castDoubleToWord64, castFloatToWord32, castWord32ToFloat, castWord64ToDouble)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Parquet.Arrow qualified as Arrow
import Parquet.ByteStreamSplit qualified as ByteStreamSplit
import Parquet.DeltaEncode qualified as Delta
import Parquet.Footer qualified as Footer
import Parquet.LevelsEncode qualified as Levels
import Parquet.Nested qualified as Nested
import Parquet.Page qualified as Page
import Parquet.PageIndex qualified as Index
import Parquet.Predicate qualified as Pred
import Parquet.Read qualified as Read
import Parquet.Types
import Parquet.Write qualified as Write
import System.Mem (performGC)
import Wireform.Hash qualified as Hash


data TestPage = TestPage
  { testBytes :: !ByteString
  , testRows :: {-# UNPACK #-} !Int
  , testStats :: !Statistics
  }


data Fixture = Fixture
  { fixtureFile :: !Read.ParquetFile
  , fixtureLocations :: !(V.Vector PageLocation)
  , fixtureFooterStart :: {-# UNPACK #-} !Int
  }


run :: IO ()
run = do
  let leaf = SchemaElement "x" (Just Required) (Just PTInt32) Nothing Nothing Nothing Nothing Nothing
      schema = V.fromList [root, leaf]
      native = field (T.AInt 32 True) False
      values = V.fromList [intPage [1, 2, 3], intPage [10, 20, 30]]
      plain = indexed schema (V.singleton "x") values Nothing
      file = fixtureFile plain
      predicate = Pred.PGtEq (Pred.PVInt32 10)
      selected input target = Arrow.readParquetColumnWithPagePruning input 0 0 target predicate
      expected = A.primColumn A.PInt32 (S.fromList [10, 20, 30])
  check
    "dense selection returns only the retained page"
    (selected file native == Right (Just (1, 2), expected))
  check
    "required file can be projected into a nullable selected target"
    (selected file (native {T.fieldNullable = True}) == Right (Just (1, 2), expected))
  check
    "selected reads apply checked integer widening"
    ( selected file (field (T.AInt 64 True) False)
        == Right (Just (1, 2), A.primColumn A.PInt64 (S.fromList [10, 20, 30]))
    )
  mapM_
    ( \wrong -> do
        check
          "selected reads reject physical reinterpretation"
          (isLeft (selected file (field wrong True)))
        check
          "empty selections still reject an incompatible target"
          (isLeft (Arrow.readParquetColumnWithPagePruning file 0 0 (field wrong True) (Pred.PGt (Pred.PVInt32 100))))
    )
    [T.ABool, T.AFloatingPoint T.Single, T.AInt 32 False]
  let first = fixtureLocations plain V.! 0
      corrupted =
        file
          { Read.pfBytes =
              replace
                (fromIntegral (plOffset first))
                (BS.replicate (fromIntegral (plCompressedPageSize first)) 0xff)
                (Read.pfBytes file)
          }
  check
    "skipped pages are not parsed or decoded"
    (selected corrupted native == Right (Just (1, 2), expected))
  check
    "a retained corrupt page is rejected"
    (isLeft (Arrow.readParquetColumnWithPagePruning corrupted 0 0 native (Pred.PGtEq (Pred.PVInt32 0))))
  let withoutIndexes =
        mapChunk
          ( \chunk ->
              chunk
                { ccColumnIndexOffset = Nothing
                , ccColumnIndexLength = Nothing
                , ccOffsetIndexOffset = Nothing
                , ccOffsetIndexLength = Nothing
                }
          )
          file
  check
    "fallback and indexed reads use the same widening rules"
    ( selected withoutIndexes (field (T.AInt 64 True) False)
        == Right (Nothing, A.primColumn A.PInt64 (S.fromList [1, 2, 3, 10, 20, 30]))
    )
  let noBounds =
        mapChunk
          ( \chunk ->
              chunk
                { ccColumnIndexOffset = Nothing
                , ccColumnIndexLength = Nothing
                , ccOffsetIndexOffset = Just (-1)
                }
          )
          file
  check
    "fallback does not read an unused offset index"
    (selected noBounds native == Right (Nothing, A.primColumn A.PInt32 (S.fromList [1, 2, 3, 10, 20, 30])))
  let contexts = Read.PageSelection (fixtureLocations plain) (V.fromList [False, True]) Nothing (Just 6)
  mapM_
    ( \bad ->
        check
          "selected readers validate sizes/order/rows/masks before indexing"
          (isLeft (Read.readInt32SelectedLevels Uncompressed 0 0 (Read.pfBytes file) bad))
    )
    [ contexts {Read.selectionKeep = V.empty}
    , contexts {Read.selectionLocations = V.reverse (fixtureLocations plain)}
    , contexts {Read.selectionLocations = fixtureLocations plain V.// [(1, first)]}
    , contexts {Read.selectionLocations = fixtureLocations plain V.// [(1, (fixtureLocations plain V.! 1) {plOffset = -1})]}
    , contexts {Read.selectionLocations = fixtureLocations plain V.// [(1, (fixtureLocations plain V.! 1) {plCompressedPageSize = 1})]}
    , contexts {Read.selectionRowCount = Just 7}
    ]
  let tiny = mapChunk (\chunk -> chunk {ccMetadata = (\md -> md {cmTotalCompressedSize = 1}) <$> ccMetadata chunk}) file
  check
    "offset indexes cannot escape their column chunk"
    (isLeft (Index.readOffsetIndex tiny 0 0))
  check
    "external-column page indexes cannot read local bytes"
    (isLeft (selected (mapChunk (\chunk -> chunk {ccFilePath = Just "external.parquet"}) file) native))
  let badCount = mapChunk (\chunk -> chunk {ccMetadata = (\md -> md {cmNumValues = 7}) <$> ccMetadata chunk}) file
  check
    "full column reads reject inconsistent column event counts"
    (isLeft (Arrow.readParquetColumn badCount 0 0 native))
  let shortMetadata =
        mapChunk
          (\chunk -> chunk {ccMetadata = (\md -> md {cmNumValues = 5}) <$> ccMetadata chunk})
          (file {Read.pfFooter = (Read.pfFooter file) {fmRowGroups = (\group -> group {rgNumRows = 5}) <$> fmRowGroups (Read.pfFooter file)}})
  check
    "full column reads reject decoded rows differing from consistent footer counts"
    (isLeft (Arrow.readParquetColumn shortMetadata 0 0 native))
  check
    "an empty selection cannot hide inconsistent column event counts"
    (isLeft (Arrow.readParquetColumnWithPagePruning badCount 0 0 native (Pred.PGt (Pred.PVInt32 100))))
  index <- either error (maybe (error "missing index") pure) (Index.readColumnIndex file 0 0)
  let short =
        Index.encodeColumnIndex
          ( index
              { ciNullPages = V.take 1 (ciNullPages index)
              , ciMinValues = V.take 1 (ciMinValues index)
              , ciMaxValues = V.take 1 (ciMaxValues index)
              , ciNullCounts = V.take 1 <$> ciNullCounts index
              }
          )
      chunk = rgColumns (V.head (fmRowGroups (Read.pfFooter file))) V.! 0
      indexOffset = maybe (error "missing index offset") fromIntegral (ccColumnIndexOffset chunk)
      mismatched =
        mapChunk
          (\column -> column {ccColumnIndexLength = Just (fromIntegral (BS.length short))})
          (file {Read.pfBytes = replace indexOffset short (Read.pfBytes file)})
  check
    "column and offset indexes must describe the same page count"
    (isLeft (selected mismatched native))
  nullableSelection
  nullableByteSelection
  dictionarySelection
  binaryBuffersRegression
  dictionaryExpansionRegression
  dictionaryAllocationRegression
  dictionarySequenceRegression
  scalarBuffersRegression
  packedBoolRegression
  byteStreamSplitRegression
  legacyChunkAccumulationRegression
  statisticsTraversalRegression
  nullableAssemblyRegression
  deltaEncodingRegression
  deltaPackingRegression
  dictionaryFloatingRegression
  dictionaryConstructionRegression
  dictionaryRefinementGrowthRegression
  dictionaryBucketMixRegression
  dictionaryScalarStorageRegression
  dictionaryEqualityRegression
  dictionaryEqualityCancellationRegression
  dictionaryOwnerRegression
  nestedAnnotationRegression
  nestedPageBoundsRegression
  nestedShredBoundsRegression
  footerWriteBoundsRegression
  nestedProjectionRegression


data NestedTypeCase = NestedTypeCase
  { nestedInputType :: !T.ArrowType
  , nestedInputColumn :: !A.ColumnArray
  , nestedExpectedType :: !T.ArrowType
  , nestedExpectedColumn :: !A.ColumnArray
  }


footerWriteBoundsRegression :: IO ()
footerWriteBoundsRegression = do
  let name = TextValue.replicate (512 * 1024) "é"
      count = 4096
      element = SchemaElement name (Just Required) (Just PTInt32) Nothing Nothing Nothing Nothing Nothing
      schema = V.cons (root {seNumChildren = Just (fromIntegral count)}) (V.replicate count element)
      metadata = FileMetadata 2 schema 0 V.empty Nothing Nothing Nothing
  _ <- evaluate (TextValue.length name)
  beforeMetadata <- getAllocationCounter
  rejectedMetadata <- evaluate (Footer.writeFooterEither metadata)
  afterMetadata <- getAllocationCounter
  check "typed metadata exceeds footer budget before allocating a 4GiB encoded buffer" $
    rejectedMetadata == Left "Thrift.Encode: encoded value exceeds size limit"
  check ("typed oversized metadata sizing stays bounded: " ++ show (beforeMetadata - afterMetadata)) (beforeMetadata - afterMetadata < 16 * 1024 * 1024)
  let footerEncryption = Write.FooterEncryption (BS.replicate 16 0) "fileid01" BS.empty "test-key"
  beforeEncrypted <- getAllocationCounter
  rejectedEncrypted <- Write.buildParquetFileWithIndexEncryptedFooter footerEncryption schema V.empty V.empty
  afterEncrypted <- getAllocationCounter
  check "encrypted footer rejects oversized schema before serialization or encryption" $
    rejectedEncrypted == Left "Thrift.Encode: encoded value exceeds size limit"
  check "encrypted oversized schema sizing stays bounded" (beforeEncrypted - afterEncrypted < 16 * 1024 * 1024)
  check "raw footer writes a little-endian length and four-byte magic" $
    Footer.writeRawFooterEither Footer.parquetMagic "abc" == Right ("abc" <> BS.pack [3, 0, 0, 0] <> "PAR1")
  check "empty encrypted footer framing preserves PARE" $
    Footer.writeRawFooterEither Footer.parquetEncryptedMagic BS.empty == Right (BS.replicate 4 0 <> "PARE")
  mapM_
    (\magic -> check "raw footer rejects magic with the wrong byte length" (isLeft (Footer.writeRawFooterEither magic "metadata")))
    [BS.empty, "PAR", "PAR11"]
  when (toInteger (maxBound :: Int) > toInteger (maxBound :: Word32)) $ do
    storage <- mallocForeignPtrBytes 1
    let limit = fromIntegral (maxBound :: Word32)
        bytes n = BSI.fromForeignPtr storage 0 n
    -- Synthetic length only: neither case may read the one-byte backing store.
    cryptoRejected <-
      Write.buildParquetFileWithIndexEncryptedFooter
        (footerEncryption {Write.feKeyMetadata = bytes limit})
        (V.singleton (root {seNumChildren = Just 0}))
        V.empty
        V.empty
    check "encrypted footer reserves crypto headers, nonce and authentication tag" $
      cryptoRejected == Left "Thrift.Encode: encoded value exceeds size limit"
    combinedRejected <-
      Write.buildParquetFileWithIndexEncryptedFooter
        (footerEncryption {Write.feKeyMetadata = bytes (limit - 64)})
        (V.fromList [root, element])
        V.empty
        V.empty
    check "encrypted footer checks the combined crypto and plaintext metadata budget before copying either" $
      combinedRejected == Left "Thrift.Encode: encoded value exceeds size limit"
    before <- getAllocationCounter
    rejected <- evaluate (Footer.writeRawFooterEither Footer.parquetMagic (bytes (limit + 1)))
    accepted <- evaluate (isRight (Footer.writeRawFooterEither Footer.parquetMagic (bytes limit)))
    after <- getAllocationCounter
    check "raw footer rejects unsigned32 length overflow before reading/copying metadata" $
      rejected == Left "Parquet.Footer: metadata length exceeds unsigned 32-bit footer limit"
    check "raw footer accepts the full unsigned32 length range without forcing output" accepted
    check "raw footer length checks have bounded allocation" (before - after < 65536)


nestedShredBoundsRegression :: IO ()
nestedShredBoundsRegression = do
  let width = 1024 * 1024
      count = 2048
      binary = BS.replicate width 0
      row = Nested.NRLeaf . Nested.LvBinary
      binaryRows = V.replicate count (row binary)
      binarySchema = Nested.NSPrimitive Nested.LtBinary
      sources =
        [ Nested.LvBinary binary
        , Nested.LvString (TextValue.replicate (width `quot` 2) "é")
        , Nested.LvString (TextValue.replicate (width `quot` 4) "🙂")
        ]
  mapM_
    ( \value -> do
        let typ = case value of Nested.LvBinary {} -> Nested.LtBinary; _ -> Nested.LtString
            schema = Nested.NSPrimitive typ
            rows = V.replicate count (Nested.NRLeaf value)
        _ <- evaluate (Nested.NRLeaf value)
        _ <- evaluate (V.length rows)
        before <- getAllocationCounter
        rejected <- evaluate (isLeft (Nested.shred schema rows))
        after <- getAllocationCounter
        check "shred rejects cumulative binary/UTF8 bytes before materializing a 2GiB buffer" rejected
        check ("oversized binary/UTF8 preflight allocation remains bounded: " ++ show (before - after)) (before - after < 2 * 1024 * 1024)
        beforeValid <- getAllocationCounter
        accepted <- evaluate (isRight (Nested.shred schema (V.take (count - 1) rows)))
        afterValid <- getAllocationCounter
        check "shred accepts a near-limit valid cumulative byte count without forcing output bytes" accepted
        check "accepted preflight does not materialize the large output buffer" (beforeValid - afterValid < 2 * 1024 * 1024)
    )
    sources
  let variant = Nested.NSVariant
  mapM_
    ( \variantRow -> do
        _ <- evaluate variantRow
        let rows = V.replicate count variantRow
        before <- getAllocationCounter
        rejected <- evaluate (isLeft (Nested.shred variant rows))
        after <- getAllocationCounter
        check "shred bounds each Variant metadata/value leaf independently" rejected
        check "oversized Variant rejection does not materialize either leaf" (before - after < 4 * 1024 * 1024)
    )
    [Nested.NRVariantBytes binary BS.empty, Nested.NRVariantBytes BS.empty binary]
  _ <- evaluate (V.length binaryRows)
  beforeFile <- getAllocationCounter
  rejectedFile <- evaluate (isLeft (Nested.buildNestedFile (V.singleton ("v", binarySchema)) (V.singleton binaryRows)))
  afterFile <- getAllocationCounter
  check "nested whole-file builder rejects cumulative oversized values" rejectedFile
  check "nested whole-file rejection happens before value allocation" (beforeFile - afterFile < 2 * 1024 * 1024)
  let larger = BS.replicate (2 * width) 0
      first = V.take (count - 1) binaryRows
      second = V.replicate (count - 1) (row larger)
  _ <- evaluate (BS.length larger)
  _ <- evaluate (V.length second)
  beforeColumns <- getAllocationCounter
  rejectedColumns <- evaluate (isLeft (Nested.buildNestedFile (V.fromList [("a", binarySchema), ("b", binarySchema)]) (V.fromList [first, second])))
  afterColumns <- getAllocationCounter
  check "all column plans are checked before an earlier valid large column is materialized" rejectedColumns
  check "later-column overflow leaves value allocation bounded" (beforeColumns - afterColumns < 4 * 1024 * 1024)
  when (toInteger (maxBound :: Int) > toInteger (maxBound :: Int32)) $ do
    storage <- mallocForeignPtrBytes 1
    let limit = fromIntegral (maxBound :: Int32)
        -- Only lengths are inspected: copying these metadata-only views is
        -- a regression, as is traversing their tiny backing arrays.
        bytes n = BSI.fromForeignPtr storage 0 n
        leafInput payload = V.singleton (row payload)
        listItems = VG.basicUnsafeSlice 0 (limit + 1) (V.singleton Nested.NRNull)
        mapItems = VG.basicUnsafeSlice 0 (limit + 1) (V.singleton (Nested.NRLeaf (Nested.LvInt32 1), Nested.NRNull))
        hugeRows = VG.basicUnsafeSlice 0 (limit + 1) (V.singleton Nested.NRNull)
    before <- getAllocationCounter
    single <- evaluate (isLeft (Nested.shred binarySchema (leafInput (bytes (limit - 3)))))
    combined <- evaluate (Nested.shred (Nested.NSOptional binarySchema) (leafInput (bytes (limit - 4))))
    lists <- evaluate (isLeft (Nested.shred (Nested.NSList (Nested.NSOptional (Nested.NSPrimitive Nested.LtInt32))) (V.singleton (Nested.NRList listItems))))
    maps <- evaluate (isLeft (Nested.shred (Nested.NSMap (Nested.NSPrimitive Nested.LtInt32) (Nested.NSOptional (Nested.NSPrimitive Nested.LtInt32))) (V.singleton (Nested.NRMapEntries mapItems))))
    rows <- evaluate (isLeft (Nested.shred (Nested.NSOptional binarySchema) hugeRows))
    after <- getAllocationCounter
    check "single oversized byte values are rejected before length-prefix narrowing" single
    check "shred includes level streams before materializing near-limit value bytes" (combined == Left "Parquet.Nested: complete page payload exceeds signed 32-bit limit")
    check "oversized list/map/row views are rejected before traversing elements" (lists && maps && rows)
    check "metadata-only oversized cases do not allocate value buffers" (before - after < 65536)


nestedPageBoundsRegression :: IO ()
nestedPageBoundsRegression = do
  let input = V.fromList [Just (V.fromList [Just 1, Nothing]), Nothing, Just V.empty]
      leaf = Nested.encodeOptionalListOptionalI32 "v" input
      encode = Nested.buildOptionalListFileEither PTInt32
      invalidLeaves =
        [ leaf {Nested.nlDefLevels = P.fromList [-1, 2, 0, 1]}
        , leaf {Nested.nlDefLevels = P.fromList [4, 2, 0, 1]}
        , leaf {Nested.nlDefLevels = P.fromList [0, 3, 0, 1]}
        , leaf {Nested.nlDefLevels = P.fromList [3, 1, 0, 1]}
        , leaf {Nested.nlRepLevels = P.fromList [0, -1, 0, 0]}
        , leaf {Nested.nlRepLevels = P.fromList [0, 2, 0, 0]}
        , leaf {Nested.nlRepLevels = P.fromList [1, 1, 0, 0]}
        , leaf {Nested.nlRepLevels = P.singleton 0}
        , leaf {Nested.nlValueCount = -1}
        , leaf {Nested.nlValueCount = 5}
        , leaf {Nested.nlValueCount = 0}
        , leaf {Nested.nlMaxDef = -1}
        , leaf {Nested.nlMaxDef = 2}
        , leaf {Nested.nlMaxRep = -1}
        , leaf {Nested.nlMaxRep = 2}
        , leaf {Nested.nlPath = V.empty}
        , leaf {Nested.nlPath = V.fromList ["v", "wrong", "element"]}
        , leaf {Nested.nlPath = V.fromList ["v", "list", "wrong"]}
        , leaf {Nested.nlValueBytes = BS.take 3 (Nested.nlValueBytes leaf)}
        ]
  check "checked optional-list writer accepts a valid leaf" (isRight (encode leaf 3))
  mapM_ (\bad -> check "checked optional-list writer rejects malformed leaf metadata/levels/payload" (isLeft (encode bad 3))) invalidLeaves
  mapM_ (\rows -> check "nested V2 counts must fit and match complete rows" (isLeft (encode leaf rows))) [-1, 0, 1, 4, maxBound]
  check "legacy list schema rejects an unspecified fixed-byte width" (isLeft (Nested.buildOptionalListFileEither PTFixedLenByteArray leaf 3))
  check "legacy list writer rejects a different fixed physical value width" (isLeft (Nested.buildOptionalListFileEither PTInt64 leaf 3))
  let sliced =
        leaf
          { Nested.nlDefLevels = P.slice 1 4 (P.fromList [99, 3, 2, 0, 1, 99])
          , Nested.nlRepLevels = P.slice 1 4 (P.fromList [99, 0, 1, 0, 0, 99])
          }
  check "nested page preflight respects sliced level vectors" (encode sliced 3 == encode leaf 3)
  goodBytes <- either fail pure (encode leaf 3)
  (header, bodyOffset) <- either fail pure (Page.readPageHeaderAt goodBytes 4)
  case Page.phType header of
    Page.PtDataPageV2 page ->
      mapM_
        ( \(defs, reps) -> do
            -- Deliberately malformed reader fixture: mutate a valid page's
            -- same-sized level streams, keeping its counts/footer intact.
            -- The checked writer must not generate these invalid boundaries.
            let definition = Levels.encodeRLEHybrid 2 (P.fromList defs)
                repetition = Levels.encodeRLEHybrid 1 (P.fromList reps)
                definitionOffset = bodyOffset + fromIntegral (Page.dph2RepLevelsLen page)
            check "malformed ancestor fixture preserves encoded stream lengths" $
              BS.length definition == fromIntegral (Page.dph2DefLevelsLen page)
                && BS.length repetition == fromIntegral (Page.dph2RepLevelsLen page)
            let badBytes = replace definitionOffset definition (replace bodyOffset repetition goodBytes)
            file <- either fail pure (Read.loadParquetFile badBytes)
            schema <- either fail pure (Arrow.parquetFileArrowSchemaChecked file)
            check "nested reader rejects orphan/continued values beneath null or empty lists" $
              isLeft (Arrow.parquetRowGroupToArrow schema file 0)
        )
        [([0, 3, 0, 1], [0, 1, 0, 0]), ([3, 1, 0, 1], [0, 1, 0, 0]), ([3, 2, 0, 1], [1, 0, 0, 0])]
    _ -> fail "nested boundary fixture must use DATA_PAGE_V2"
  let binary payload = Nested.buildOptionalListFileEither PTByteArray (leaf {Nested.nlValueBytes = payload}) 3
  check "legacy BYTE_ARRAY accepts one complete empty value" (isRight (binary (BS.replicate 4 0)))
  mapM_
    ( \version -> do
        let schema = V.fromList [root, SchemaElement "x" (Just Optional) (Just PTInt32) Nothing Nothing Nothing Nothing Nothing]
            values = V.singleton (V.singleton (Write.PCOptional (Write.OptInt32 (V.fromList [Just 1, Nothing, Just 2]))))
            aux = V.singleton (V.singleton (Write.emptyColumnAux {Write.caPageVersion = version}))
        bytes <- either fail pure (Write.buildParquetFileMixedWithAux schema values aux)
        file <- either fail pure (Read.loadParquetFile bytes)
        let chunk = V.head (rgColumns (V.head (fmRowGroups (Read.pfFooter file))))
        check "nullable flat metadata advertises PLAIN values and RLE levels" $
          fmap cmEncodings (ccMetadata chunk) == Just (V.fromList [Plain, RLE])
    )
    [Write.PageV1, Write.PageV2]
  mapM_
    (\payload -> check "legacy BYTE_ARRAY rejects truncated lengths/values and trailing data" (isLeft (binary payload)))
    [BS.empty, BS.replicate 3 0, BS.pack [1, 0, 0, 0], BS.pack [255, 255, 255, 255], BS.replicate 5 0]
  when (toInteger (maxBound :: Int) > toInteger (maxBound :: Int32)) $ do
    storage <- mallocForeignPtrBytes 1
    let limit = fromIntegral (maxBound :: Int32)
        -- Metadata-only oversized views. Preflight must reject their lengths
        -- without ever reading the deliberately tiny backing storage.
        hugeBytes n = BSI.fromForeignPtr storage 0 n
        oversizedEvents = leaf {Nested.nlDefLevels = VG.basicUnsafeSlice 0 (limit + 1) (P.singleton 0)}
        oversizedRows = VG.basicUnsafeSlice 0 (limit + 1) (V.singleton (Nested.NRLeaf (Nested.LvInt32 1)))
    before <- getAllocationCounter
    eventResult <- evaluate (encode oversizedEvents 3)
    payloadResult <- evaluate (binary (hugeBytes (limit + 1)))
    combinedResult <- evaluate (binary (hugeBytes limit))
    rowsResult <- evaluate (Nested.buildNestedFile (V.singleton ("v", Nested.NSPrimitive Nested.LtInt32)) (V.singleton oversizedRows))
    after <- getAllocationCounter
    check "nested event overflow is rejected before reading levels" (eventResult == Left "Parquet.Nested: event count exceeds signed 32-bit page limit")
    check "nested payload overflow is rejected before copying bytes" (payloadResult == Left "Parquet.Nested: value payload size exceeds signed 32-bit page limit")
    check "nested payload includes both level streams in the signed32 limit" (combinedResult == Left "Parquet.Nested: complete page payload exceeds signed 32-bit limit")
    check "nested row overflow is rejected before shredding" (rowsResult == Left "Parquet.Nested: row count exceeds signed 32-bit page limit")
    check "nested oversized rejection stays bounded in allocation" (before - after < 65536)
  valid <- H.check $ H.withTests 100 $ H.property $ do
    rows <- H.forAll (V.fromList <$> Gen.list (Range.linear 0 32) (Gen.maybe (V.fromList <$> Gen.list (Range.linear 0 16) (Gen.maybe (Gen.int32 Range.linearBounded)))))
    let optionalValue = maybe Nested.NRNull (Nested.NRLeaf . Nested.LvInt32)
        nestedRows = V.map (maybe Nested.NRNull (Nested.NRList . V.map optionalValue)) rows
        schema = Nested.NSOptional (Nested.NSList (Nested.NSOptional (Nested.NSPrimitive Nested.LtInt32)))
    legacy <- H.evalEither (Nested.buildOptionalListFileEither PTInt32 (Nested.encodeOptionalListOptionalI32 "v" rows) (V.length rows))
    generic <- H.evalEither (Nested.buildNestedFile (V.singleton ("v", schema)) (V.singleton nestedRows))
    legacy H.=== generic
    file <- H.evalEither (Read.loadParquetFile legacy)
    let chunk = V.head (rgColumns (V.head (fmRowGroups (Read.pfFooter file))))
    fmap cmEncodings (ccMetadata chunk) H.=== Just (V.fromList [Plain, RLE])
    arrowSchema <- H.evalEither (Arrow.parquetFileArrowSchemaChecked file)
    columns <- H.evalEither (Arrow.parquetRowGroupToArrow arrowSchema file 0)
    actual <- H.evalEither (Arrow.columnArrayToNestedRows (V.head (T.arrowFields arrowSchema)) (V.head columns))
    actual H.=== nestedRows
  check "checked legacy/generic nested pages preserve arbitrary null/list shapes" valid


nestedAnnotationRegression :: IO ()
nestedAnnotationRegression = do
  let field name nullable typ = T.Field name nullable typ V.empty Nothing V.empty
      same typ column = NestedTypeCase typ column typ column
      cases =
        [ same (T.AInt 8 True) (A.primColumn A.PInt8 (S.fromList [-128, -1, 0, 127]))
        , same (T.AInt 16 True) (A.primColumn A.PInt16 (S.fromList [-32768, -1, 0, 32767]))
        , same (T.AInt 32 True) (A.primColumn A.PInt32 (S.fromList [minBound, -1, 0, maxBound]))
        , same (T.AInt 64 True) (A.primColumn A.PInt64 (S.fromList [minBound, -1, 0, maxBound]))
        , same (T.AInt 8 False) (A.primColumn A.PUInt8 (S.fromList [0, 127, 128, 255]))
        , same (T.AInt 16 False) (A.primColumn A.PUInt16 (S.fromList [0, 32767, 32768, 65535]))
        , same (T.AInt 32 False) (A.primColumn A.PUInt32 (S.fromList [0, 2147483647, 2147483648, maxBound]))
        , same (T.AInt 64 False) (A.primColumn A.PUInt64 (S.fromList [0, 9223372036854775807, 9223372036854775808, maxBound]))
        , same (T.ADate T.DateDay) (A.primColumn A.PDate32 (S.fromList [-1, 0, 1, 20000]))
        , NestedTypeCase
            (T.ADate T.DateMillisecond)
            (A.primColumn A.PDate64 (S.fromList [-86400000, 0, 86400000, 172800000]))
            (T.ADate T.DateDay)
            (A.primColumn A.PDate32 (S.fromList [-1, 0, 1, 2]))
        , NestedTypeCase
            (T.ATime T.Second 32)
            (A.primColumn A.PTime32 (S.fromList [0, 1, 43200, 86399]))
            (T.ATime T.Millisecond 32)
            (A.primColumn A.PTime32 (S.fromList [0, 1000, 43200000, 86399000]))
        , same (T.ATime T.Millisecond 32) (A.primColumn A.PTime32 (S.fromList [0, 1, 43200000, 86399999]))
        , same (T.ATime T.Microsecond 64) (A.primColumn A.PTime64 (S.fromList [0, 1, 43200000000, 86399999999]))
        , same (T.ATime T.Nanosecond 64) (A.primColumn A.PTime64 (S.fromList [0, 1, 43200000000000, 86399999999999]))
        , same T.ABool (A.fromBools (V.fromList [True, False, True, True]))
        ]
          ++ concatMap
            ( \zone ->
                NestedTypeCase
                  (T.ATimestamp T.Second zone)
                  (A.primColumn A.PTimestamp (S.fromList [-1, 0, 1, 2147483648]))
                  (T.ATimestamp T.Millisecond zone)
                  (A.primColumn A.PTimestamp (S.fromList [-1000, 0, 1000, 2147483648000]))
                  : map
                    (\unit -> same (T.ATimestamp unit zone) (A.primColumn A.PTimestamp (S.fromList [minBound, -1, 0, maxBound])))
                    [T.Millisecond, T.Microsecond, T.Nanosecond]
            )
            [Nothing, Just "UTC"]
      roundtrip label inputField inputColumn expectedField expectedColumn = do
        schema <- either fail pure (Arrow.arrowFieldToNestedSchema inputField)
        rows <- either fail pure (Arrow.columnArrayToNestedRows inputField inputColumn)
        bytes <- either fail pure (Nested.buildNestedFile (V.singleton (T.fieldName inputField, schema)) (V.singleton rows))
        file <- either fail pure (Read.loadParquetFile bytes)
        inferred <- either fail pure (Arrow.parquetFileArrowSchemaChecked file)
        check ("nested annotations survive " ++ label) $
          V.concatMap fieldTypes (T.arrowFields inferred) == fieldTypes expectedField
        decoded <- either (fail . show) pure (Arrow.parquetRowGroupToArrow (T.Schema (V.singleton expectedField) T.Little V.empty V.empty) file 0)
        check ("nested values survive " ++ label) (decoded == V.singleton expectedColumn)
      fieldTypes f = V.cons (T.fieldType f) (V.concatMap fieldTypes (T.fieldChildren f))
      mask = A.validityFromBools (V.fromList [True, False, True, True])
  mapM_
    ( \sample -> do
        let inputField = field "v" False (nestedInputType sample)
            expectedField = field "v" False (nestedExpectedType sample)
            label = show (nestedInputType sample)
        roundtrip label inputField (nestedInputColumn sample) expectedField (nestedExpectedColumn sample)
        inputNullable <- either fail pure (A.maskValidity mask (nestedInputColumn sample))
        expectedNullable <- either fail pure (A.maskValidity mask (nestedExpectedColumn sample))
        roundtrip
          (label ++ " nullable/sliced")
          (inputField {T.fieldNullable = True})
          (A.sliceColumnArray 1 3 inputNullable)
          (expectedField {T.fieldNullable = True})
          (A.sliceColumnArray 1 3 expectedNullable)
        let struct f = T.Field "parent" False T.AStruct (V.singleton f) Nothing V.empty
        inputStruct <- either fail pure (A.mkStruct 4 Nothing (V.singleton ("v", nestedInputColumn sample)))
        expectedStruct <- either fail pure (A.mkStruct 4 Nothing (V.singleton ("v", nestedExpectedColumn sample)))
        roundtrip (label ++ " struct") (struct inputField) inputStruct (struct expectedField) expectedStruct
        let list typ f = T.Field "list" False typ (V.singleton (f {T.fieldName = "element"})) Nothing V.empty
        inputList <- either fail pure (A.mkList Nothing (S.fromList [1, 3, 3, 4]) (nestedInputColumn sample))
        expectedList <- either fail pure (A.mkList Nothing (S.fromList [1, 3, 3, 4]) (nestedExpectedColumn sample))
        roundtrip (label ++ " list") (list T.AList inputField) inputList (list T.AList expectedField) expectedList
        inputLarge <- either fail pure (A.mkLargeList Nothing (S.fromList [1, 3, 3, 4]) (nestedInputColumn sample))
        -- Parquet stores both Arrow list widths as the standard LIST layout.
        roundtrip (label ++ " large list") (list T.ALargeList inputField) inputLarge (list T.AList expectedField) expectedList
        let keyField = field "key" False (T.AInt 32 False)
            mapField f =
              T.Field
                "map"
                False
                (T.AMap False)
                (V.singleton (T.Field "entries" False T.AStruct (V.fromList [keyField, f {T.fieldName = "value"}]) Nothing V.empty))
                Nothing
                V.empty
            keys = A.primColumn A.PUInt32 (S.fromList [1, 2, 3, 4])
        inputMap <- either fail pure (A.mkMap Nothing (S.fromList [1, 3, 3, 4]) keys (nestedInputColumn sample))
        expectedMap <- either fail pure (A.mkMap Nothing (S.fromList [1, 3, 3, 4]) keys (nestedExpectedColumn sample))
        roundtrip (label ++ " map") (mapField inputField) inputMap (mapField expectedField) expectedMap
    )
    cases
  let boolValues = V.generate 12 ((== 0) . (`rem` 3))
  leaves <- either fail pure (Nested.shred (Nested.NSPrimitive Nested.LtBool) (V.map (Nested.NRLeaf . Nested.LvBool) boolValues))
  check "nested BOOLEAN PLAIN packs eight present values per byte, LSB first" $
    Nested.nlValueBytes (V.unsafeHead leaves) == BS.pack [0x49, 0x02]
  mapM_
    ( \n -> do
        let values = V.generate n (\i -> if i `rem` 4 == 0 then Nothing else Just (i `rem` 3 == 0))
            column = A.fromMaybeBools values
            boolField = field "v" True T.ABool
        roundtrip ("BOOLEAN null/tail " ++ show n) boolField column boolField column
    )
    [0 .. 65]
  check "nested integer annotation rejects invalid widths even with no rows" $
    isLeft (Nested.shred (Nested.NSPrimitive (Nested.LtInteger 7 True)) V.empty)
  let primitive = Nested.NSPrimitive Nested.LtInt32
      invalidSchemas =
        [ Nested.NSOptional (Nested.NSOptional primitive)
        , Nested.NSOptional (Nested.NSRequired primitive)
        , Nested.NSRequired (Nested.NSOptional primitive)
        , Nested.NSRequired (Nested.NSRequired primitive)
        , Nested.NSMap (Nested.NSOptional primitive) primitive
        , Nested.NSStruct V.empty
        , Nested.NSOptional (Nested.NSList (Nested.NSStruct V.empty))
        , Nested.NSOptional (Nested.NSStruct (V.singleton ("hidden", Nested.NSMap (Nested.NSOptional primitive) primitive)))
        ]
  mapM_
    ( \schema ->
        mapM_
          ( \rows -> do
              check "invalid nested schema is rejected before shredding any rows" $
                isLeft (Nested.shred schema rows)
              check "invalid nested schema cannot become a Parquet file" $
                isLeft (Nested.buildNestedFile (V.singleton ("v", schema)) (V.singleton rows))
          )
          [V.empty, V.singleton Nested.NRNull]
    )
    invalidSchemas
  mapM_
    ( \(typ, value) ->
        check "nested logical integers reject out-of-range payloads" $
          isLeft (Nested.shred (Nested.NSPrimitive typ) (V.singleton (Nested.NRLeaf (Nested.LvInt32 value))))
    )
    [(Nested.LtInteger 8 True, -129), (Nested.LtInteger 8 True, 128), (Nested.LtInteger 8 False, -1), (Nested.LtInteger 8 False, 256), (Nested.LtInteger 16 False, 65536)]
  mapM_
    ( \sample ->
        check "nested temporal conversion rejects precision loss, invalid time and overflow" $
          isLeft (Arrow.columnArrayToNestedRows (field "v" False (nestedInputType sample)) (nestedInputColumn sample))
    )
    [ same (T.ADate T.DateMillisecond) (A.primColumn A.PDate64 (S.singleton 1))
    , same (T.ADate T.DateMillisecond) (A.primColumn A.PDate64 (S.singleton maxBound))
    , same (T.ATime T.Second 32) (A.primColumn A.PTime32 (S.singleton (-1)))
    , same (T.ATime T.Second 32) (A.primColumn A.PTime32 (S.singleton 86400))
    , same (T.ATime T.Millisecond 32) (A.primColumn A.PTime32 (S.singleton 86400000))
    , same (T.ATime T.Microsecond 64) (A.primColumn A.PTime64 (S.singleton 86400000000))
    , same (T.ATime T.Nanosecond 64) (A.primColumn A.PTime64 (S.singleton (-1)))
    , same (T.ATimestamp T.Second Nothing) (A.primColumn A.PTimestamp (S.singleton minBound))
    , same (T.ATimestamp T.Second Nothing) (A.primColumn A.PTimestamp (S.singleton maxBound))
    ]
  check "nested duration rejects an unsupported logical type instead of silently writing an integer" $
    isLeft (Arrow.arrowFieldToNestedSchema (field "v" False (T.ADuration T.Millisecond)))
  let parentMask = A.validityFromBools (V.fromList [True, False, True])
      dateField = field "date" False (T.ADate T.DateMillisecond)
      expectedDateField = field "date" False (T.ADate T.DateDay)
      parent child = T.Field "parent" True T.AStruct (V.singleton child) Nothing V.empty
  inputDates <- either fail pure (A.mkPrim A.PDate64 parentMask (S.fromList [-86400000, 1, 86400000]))
  expectedDates <- either fail pure (A.mkPrim A.PDate32 parentMask (S.fromList [-1, 0, 1]))
  inputParent <- either fail pure (A.mkStruct 3 parentMask (V.singleton ("date", inputDates)))
  expectedParent <- either fail pure (A.mkStruct 3 parentMask (V.singleton ("date", expectedDates)))
  roundtrip "DATE64 poison and required null hidden beneath a null struct" (parent dateField) inputParent (parent expectedDateField) expectedParent
  check "a visible null remains invalid for a required child" $
    isLeft (Arrow.columnArrayToNestedRows dateField inputDates)
  let secondsField = field "element" False (T.ATime T.Second 32)
      millisField = field "element" False (T.ATime T.Millisecond 32)
      listField child = T.Field "list" False T.AList (V.singleton child) Nothing V.empty
      poisonTimes = A.primColumn A.PTime32 (S.fromList [maxBound, 0, 1, minBound])
  inputTimes <- either fail pure (A.mkList Nothing (S.fromList [1, 3]) poisonTimes)
  expectedTimes <- either fail pure (A.mkList Nothing (S.fromList [0, 2]) (A.primColumn A.PTime32 (S.fromList [0, 1000])))
  roundtrip "unused poisoned list prefix/suffix" (listField secondsField) inputTimes (listField millisField) expectedTimes
  let keyField = field "key" False (T.AInt 32 True)
      valueField = field "value" False (T.AInt 32 True)
      mapField sorted nullable =
        T.Field
          "map"
          nullable
          (T.AMap sorted)
          (V.singleton (T.Field "entries" False T.AStruct (V.fromList [keyField, valueField]) Nothing V.empty))
          Nothing
          V.empty
      values = A.primColumn A.PInt32 (S.fromList [10, 20])
      nullMap = A.validityFromBools (V.singleton False)
  unsorted <- either fail pure (A.mkMap Nothing (S.fromList [0, 2]) (A.primColumn A.PInt32 (S.fromList [2, 1])) values)
  check "a sorted map field rejects visible out-of-order keys" $
    isLeft (Arrow.columnArrayToNestedRows (mapField True False) unsorted)
  hiddenUnsorted <- either fail pure (A.maskValidity nullMap unsorted)
  check "null map rows do not inspect hidden sorted keys" $
    Arrow.columnArrayToNestedRows (mapField True True) hiddenUnsorted == Right (V.singleton Nested.NRNull)
  nullKeys <- either fail pure (A.mkPrim A.PInt32 (A.validityFromBools (V.fromList [False, True])) (S.fromList [1, 2]))
  check "Arrow map construction rejects physical null keys" (isLeft (A.mkMap Nothing (S.fromList [0, 2]) nullKeys values))
  -- Bypass the checked constructor to exercise malformed physical storage.
  -- Arrow's map-key invariant also applies beneath a null map row.
  let invalidMap = AI.ColMap Nothing (S.fromList [0, 2]) nullKeys values
      hiddenKeys = AI.ColMap nullMap (S.fromList [0, 2]) nullKeys values
  check "visible null map keys are rejected" (isLeft (Arrow.columnArrayToNestedRows (mapField False False) invalidMap))
  check "physical null map keys remain invalid beneath a null map" $
    isLeft (Arrow.columnArrayToNestedRows (mapField False True) hiddenKeys)
  let count = 32768
      intField = field "v" False (T.AInt 32 True)
      boolField = field "v" False T.ABool
      intColumn = A.primColumn A.PInt32 (S.generate count fromIntegral)
      boolColumn = A.fromBools (V.generate count odd)
  mapM_
    ( \(typ, column) -> do
        _ <- evaluate (A.columnLength column)
        before <- getAllocationCounter
        rows <- either fail pure =<< evaluate (Arrow.columnArrayToNestedRows typ column)
        after <- getAllocationCounter
        check ("nested conversion allocates output rows without per-row result/index wrappers: " ++ show (before - after)) $
          before - after < fromIntegral (count * 48 + 65536) && V.length rows == count
    )
    [(intField, intColumn), (boolField, boolColumn)]


dictionaryOwnerRegression :: IO ()
dictionaryOwnerRegression = do
  let !count = 32768
      !uniqueCount = 257
      !width = 128
      word key = BS.pack (map (\shift -> fromIntegral ((fromIntegral key :: Word64) `shiftR` shift)) [0, 8 .. 56])
      value key = BS.replicate 8 97 <> word (key `rem` 2) <> word (key `quot` 2) <> BS.replicate (width - 24) 97
      pool = V.generate uniqueCount value
      buildOwned = do
        -- IO allocation prevents the source from becoming a retained CAF.
        owners <- V.generateM count (\i -> BS.useAsCStringLen (pool V.! (i `rem` uniqueCount)) BS.packCStringLen)
        let source =
              V.imap
                ( \i owner ->
                    let prefix = i `rem` 8
                        bytes = BS.replicate prefix 0xa5 <> BS.copy owner <> BS.replicate 8 0x5a
                    in BS.take width (BS.drop prefix bytes)
                )
                owners
        evaluate (Write.buildDictionary (Write.ColByteArray source))
  -- Lazy source payloads create allocation pressure during interning and
  -- growth. Only the returned owners remain after this scope and a GC.
  dictionary <- buildOwned
  performGC
  case Write.dictUniques dictionary of
    Write.ColByteArray owners -> do
      copied <- V.mapM (\bytes -> BS.useAsCStringLen bytes BS.packCStringLen) owners
      check "dictionary cached addresses retain their sliced owners across growth, promotion and GC" (copied == pool)
    _ -> error "dictionary owner regression returned a non-binary column"
  check
    "dictionary ownership changes preserve stable indices"
    (Write.dictIndices dictionary == P.generate count (\i -> fromIntegral (i `rem` uniqueCount)))


nullableSelection :: IO ()
nullableSelection = do
  let parent = SchemaElement "parent" (Just Optional) Nothing (Just 1) Nothing Nothing Nothing Nothing
      leaf = SchemaElement "x" (Just Optional) (Just PTInt32) Nothing Nothing Nothing Nothing Nothing
      schema = V.fromList [root, parent, leaf]
  mapM_
    ( \v2 -> do
        let first = levelPage v2 2 [2, 1, 0] [1] 0
            second = levelPage v2 2 [2, 0, 1, 2] [10, 30] 0
            fixture = indexed schema (V.fromList ["parent", "x"]) (V.fromList [first, second]) Nothing
            file = fixtureFile fixture
            target = field (T.AInt 32 True) True
            predicate = Pred.PGtEq (Pred.PVInt32 10)
            selected = Arrow.readParquetColumnWithPagePruning file 0 0 target predicate
        check
          "selected nullable ancestors use the real maximum definition level"
          (selected == Right (Just (1, 2), A.fromMaybes A.PInt32 (V.fromList [Just 10, Nothing, Nothing, Just 30])))
        check
          "selected nulls cannot be hidden by a required target"
          (isLeft (Arrow.readParquetColumnWithPagePruning file 0 0 (target {T.fieldNullable = False}) predicate))
        let nullFile = fixtureFile (indexed schema (V.fromList ["parent", "x"]) (V.singleton (levelPage v2 2 [0, 1, 0] [] 0)) Nothing)
            presentFile = fixtureFile (indexed schema (V.fromList ["parent", "x"]) (V.singleton (levelPage v2 2 [2, 2] [10, 30] 0)) Nothing)
        check
          "selected all-null primitive leaves retain their parent row count"
          (Arrow.readParquetColumnWithPagePruning nullFile 0 0 target Pred.PIsNull == Right (Just (1, 1), A.fromMaybes A.PInt32 (V.replicate 3 Nothing)))
        check
          "selected optional primitive leaves also handle all-present pages"
          (Arrow.readParquetColumnWithPagePruning presentFile 0 0 target predicate == Right (Just (1, 1), A.primColumn A.PInt32 (S.fromList [10, 30])))
    )
    [False, True]
  let bad = levelPage True 2 [2, 0, 1, 2] [10, 30] 1
  check
    "V2 null counts must match the definition stream"
    (isLeft (Read.readInt32Levels Uncompressed 0 2 (testBytes bad)))
  let ordinary = levelPage True 1 [1, 0, 1] [10, 30] 0
  (header, body) <- either error pure (Page.readPageHeaderAt (testBytes ordinary) 0)
  case Page.phType header of
    Page.PtDataPageV2 page -> do
      let explicit =
            Write.encodePageHeader
              ( header
                  { Page.phType = Page.PtDataPageV2 (page {Page.dph2RepLevelsLen = 1})
                  , Page.phCompressedPageSize = (+ 1) <$> Page.phCompressedPageSize header
                  , Page.phUncompressedPageSize = (+ 1) <$> Page.phUncompressedPageSize header
                  }
              )
              <> BS.singleton 6
              <> BS.drop body (testBytes ordinary)
      decoded <- either error pure (Read.readInt32Levels Uncompressed 0 1 explicit)
      check
        "redundant zero-width V2 repetition streams remain readable"
        (Read.clDefinition decoded == P.fromList [1, 0, 1] && Read.clValues decoded == P.fromList [10, 30])
    _ -> error "missing V2 test page"


nullableByteSelection :: IO ()
nullableByteSelection = mapM_ checkType [False, True]
  where
    checkType text = do
      let large = BS.replicate (1024 * 1024) 109
          present = V.fromList [large, "a", "z"]
          values = V.singleton (Just large) <> V.replicate 4096 Nothing <> V.fromList [Just "a", Just "z"]
          leaf = SchemaElement "x" (Just Optional) (Just PTByteArray) Nothing Nothing (if text then Just LTString else Nothing) Nothing Nothing
          bytes = either error id (Write.encodeOptionalColumnPageV2 Uncompressed (Write.OptByteArray values))
          page = TestPage bytes (V.length values) ((Write.columnDataStatistics (Write.ColByteArray present)) {statNullCount = Just 4096})
          file = fixtureFile (indexed (V.fromList [root, leaf]) (V.singleton "x") (V.singleton page) Nothing)
          expected = if text then A.fromMaybeTexts ((Text.decodeUtf8 <$>) <$> values) else A.fromMaybeByteStrings values
          target = field (if text then T.AUtf8 else T.ABinary) True
      case Arrow.readParquetColumnWithPagePruning file 0 0 target Pred.PIsNull of
        Right (Just (1, 1), column) -> do
          check "sparse nullable byte/text selection retains all values" (column == expected)
          let size = case column of
                A.ColUtf8 _ _ dataBytes -> BS.length dataBytes
                A.ColBinary _ _ dataBytes -> BS.length dataBytes
                _ -> error "missing sparse byte/text column"
          check "null rows do not duplicate a large first value" (size == BS.length large + 2)
        other -> error ("sparse byte/text selection failed: " <> show other)


dictionarySelection :: IO ()
dictionarySelection = do
  let leaf = SchemaElement "x" (Just Required) (Just PTByteArray) Nothing (Just CTUtf8) (Just LTString) Nothing Nothing
      schema = V.fromList [root, leaf]
      dictionary = Write.Dictionary (Write.ColByteArray (V.fromList ["a", "b", "c", "d"])) P.empty
      page indexes texts =
        TestPage
          (Write.encodeDictDataPage (dictionary {Write.dictIndices = P.fromList indexes}))
          (length indexes)
          (Write.columnDataStatistics (Write.ColByteArray (V.fromList texts)))
      fixture =
        indexed
          schema
          (V.singleton "x")
          (V.fromList [page [0, 1, 0] ["a", "b", "a"], page [2, 3] ["c", "d"]])
          (Just (Write.encodeDictPage dictionary))
      file = fixtureFile fixture
      target = field T.AUtf8 False
      predicate = Pred.PGtEq (Pred.PVText "c")
      expected = A.fromTexts (V.fromList ["c", "d"])
  check
    "standard indexes initialize a dictionary stored outside data-page locations"
    (Arrow.readParquetColumnWithPagePruning file 0 0 target predicate == Right (Just (1, 2), expected))
  let footerless = file {Read.pfBytes = BS.take (fixtureFooterStart fixture) (Read.pfBytes file)}
  check
    "metadata-aware selection does not reparse the file footer"
    (Arrow.readParquetColumnWithPagePruning footerless 0 0 target predicate == Right (Just (1, 2), expected))
  check
    "legacy selected API resolves a needed dictionary from full-file metadata"
    ( Read.readGenericByteArraySelectedPages
        Uncompressed
        (Read.pfBytes file)
        (fixtureLocations fixture)
        (V.fromList [False, True])
        == Right (V.fromList ["c", "d"])
    )
  let brokenDictionary = file {Read.pfBytes = replace 4 (BS.replicate (BS.length (Write.encodeDictPage dictionary)) 0xff) (Read.pfBytes file)}
  check
    "an empty selection does not read an unused dictionary"
    ( Arrow.readParquetColumnWithPagePruning brokenDictionary 0 0 target (Pred.PGt (Pred.PVText "z"))
        == Right (Just (0, 2), A.fromTexts V.empty)
    )
  check
    "a needed corrupt dictionary is rejected"
    (isLeft (Arrow.readParquetColumnWithPagePruning brokenDictionary 0 0 target predicate))


binaryBuffersRegression :: IO ()
binaryBuffersRegression = do
  passed <- H.check $ H.withTests 100 $ H.property $ do
    values <- H.forAll (V.fromList <$> Gen.list (Range.linear 0 128) (Gen.bytes (Range.linear 0 48)))
    prefix <- H.forAll (Gen.bytes (Range.linear 1 15))
    let raw = Lazy.toStrict (Builder.toLazyByteString (foldMap (\bytes -> Builder.word32LE (fromIntegral (BS.length bytes)) <> Builder.byteString bytes) values))
        sliced = BS.take (BS.length raw) (BS.drop (BS.length prefix) (prefix <> raw <> "unused suffix"))
        expectedOffsets = S.convert (V.scanl' (\n bytes -> n + fromIntegral (BS.length bytes)) (0 :: Int32) values)
        expectedBytes = BS.concat (V.toList values)
        page xs = let (header, body) = Write.encodeColumnDataPageParts (Write.ColByteArray xs) in header <> body
        middle = V.length values `quot` 2
        twoPages = page (V.take middle values) <> page (V.drop middle values)
        assertBuffers buffers = do
          Read.binaryOffsets buffers H.=== expectedOffsets
          Read.binaryData buffers H.=== expectedBytes
    direct <- H.evalEither (Read.decodePlainBinaryBuffers (V.length values) sliced)
    assertBuffers direct
    combined <- H.evalEither (Read.readBinaryBuffersColumnChunk Uncompressed twoPages)
    assertBuffers combined
  check "direct binary buffers preserve arbitrary sliced bytes and rebase multiple page offsets" passed
  mapM_
    (\(count, bytes) -> check "direct binary buffers reject invalid counts, lengths and trailing bytes" (isLeft (Read.decodePlainBinaryBuffers count bytes)))
    [ (-1, BS.empty)
    , (maxBound, BS.empty)
    , (1, BS.pack [0, 0, 0])
    , (1, BS.pack [255, 255, 255, 255])
    , (1, BS.pack [4, 0, 0, 0, 1])
    , (1, BS.pack [0, 0, 0, 0, 0])
    , (0, "trailing")
    ]
  let dictionary entries indices =
        let dict = Write.Dictionary (Write.ColByteArray entries) indices
        in Write.encodeDictPage dict <> Write.encodeDictDataPage dict
      -- Deliberately malformed reader fixture: index 3 fits width 2 but
      -- exceeds the three-entry dictionary. Do not ask the writer to
      -- produce an invalid dictionary now that it validates ordinals.
      invalidBody = BS.singleton 2 <> Levels.encodeRLEHybrid 2 (P.fromList [0, 1, 3])
      invalidDictionary =
        Write.encodeDictPage (Write.Dictionary (Write.ColByteArray (V.fromList ["", "value", "other"])) P.empty)
          <> invalidHeader
          <> invalidBody
      invalidHeader =
        Write.encodePageHeader
          Page.PageHeader
            { Page.phType = Page.PtDataPage (Page.DataPageHeader 3 8)
            , Page.phUncompressedPageSize = Just (fromIntegral (BS.length invalidBody))
            , Page.phCompressedPageSize = Just (fromIntegral (BS.length invalidBody))
            }
  check
    "direct dictionary buffers reject invalid keys before copying"
    (isLeft (Read.readBinaryBuffersColumnChunk Uncompressed invalidDictionary))
  -- About 1 MiB of input requests 2 GiB of output. This must fail in the
  -- size preflight, without attempting that allocation or narrowing offsets.
  check
    "direct dictionary buffers reject expansion past signed 32-bit offsets"
    (isLeft (Read.readBinaryBuffersColumnChunk Uncompressed (dictionary (V.singleton (BS.replicate (1024 * 1024) 120)) (P.replicate 2048 0))))
  let largeDictionary = Write.Dictionary (Write.ColByteArray (V.singleton (BS.replicate (1024 * 1024) 120))) (P.replicate 1024 0)
      largePage = Write.encodeDictDataPage largeDictionary
      twoLargePages = Write.encodeDictPage largeDictionary <> largePage <> largePage
  check
    "combined binary pages reject 2 GiB before materializing either 1 GiB page"
    (isLeft (Read.readBinaryBuffersColumnChunk Uncompressed twoLargePages))
  let leaf = SchemaElement "x" (Just Required) (Just PTByteArray) Nothing (Just CTUtf8) (Just LTString) Nothing Nothing
      invalid = Write.ColByteArray (V.fromList [BS.singleton 0xc3, BS.singleton 0xa9])
      (header, body) = Write.encodeColumnDataPageParts invalid
      fixture = indexed (V.fromList [root, leaf]) (V.singleton "x") (V.singleton (TestPage (header <> body) 2 (Write.columnDataStatistics invalid))) Nothing
  check
    "UTF-8 validation rejects a character split across two valid binary offsets"
    (isLeft (Arrow.readParquetColumn (fixtureFile fixture) 0 0 (field T.AUtf8 False)))


dictionaryExpansionRegression :: IO ()
dictionaryExpansionRegression = do
  valid <- H.check $ H.withTests 100 $ H.property $ do
    sizes <- H.forAll (V.fromList <$> Gen.list (Range.linear 1 32) (Gen.int32 (Range.linear 0 64)))
    keys <- H.forAll (P.fromList <$> Gen.list (Range.linear 0 128) (Gen.int32 (Range.linear 0 (fromIntegral (V.length sizes - 1)))))
    count <- H.forAll (Gen.int (Range.linear 0 (P.length keys)))
    limit <- H.forAll (Gen.int (Range.linear 0 2048))
    let width = Levels.bitWidthFor (V.length sizes - 1)
        encoded = BS.singleton (fromIntegral width) <> Levels.encodeRLEHybrid width keys
        offsets = S.convert (V.scanl' (+) 7 sizes)
        needed = P.foldl' (\total key -> total + fromIntegral (sizes V.! fromIntegral key)) (0 :: Int) (P.take count keys)
    isRight (Read.validateDictionaryExpansion limit offsets count encoded) H.=== (needed <= limit)
  check "dictionary expansion preflight matches independent prefix byte sums" valid
  malformed <- H.check $ H.withTests 100 $ H.property $ do
    encoded <- H.forAll (Gen.bytes (Range.linear 0 24))
    count <- H.forAll (Gen.int (Range.linear 0 64))
    limit <- H.forAll (Gen.int (Range.linear 0 256))
    let offsets = S.fromList [0, 0, 2, 7]
        expected = case Read.decodeDictionaryIndices count encoded of
          Left _ -> False
          Right keys ->
            P.all (\key -> key >= 0 && key < 3) keys
              && P.foldl' (\total key -> total + fromIntegral (offsets S.! (fromIntegral key + 1) - offsets S.! fromIntegral key)) (0 :: Int) keys <= limit
    isRight (Read.validateDictionaryExpansion limit offsets count encoded) H.=== expected
  check "dictionary expansion preflight agrees with bounded decoding of arbitrary malformed streams" malformed
  mapM_
    ( \width -> do
        let offsets = S.fromList [0, 2, 5]
            repeated = BS.pack (fromIntegral width : 2 : 1 : replicate ((width + 7) `quot` 8 - 1) 0)
            keys = P.fromList ([0, 1, 1, 0, 1, 0, 0, 1] :: [Word32])
            packedByte byte = foldl' (\acc bit -> let absolute = byte * 8 + bit in if testBit (keys P.! (absolute `quot` width)) (absolute `rem` width) then setBit acc bit else acc) 0 [0 .. 7]
            packed = BS.pack (fromIntegral width : 3 : (packedByte <$> [0 .. width - 1]))
        check "dictionary preflight handles every RLE width" (Read.validateDictionaryExpansion 3 offsets 1 repeated == Right ())
        check "dictionary preflight checks RLE budgets" (isLeft (Read.validateDictionaryExpansion 2 offsets 1 repeated))
        check "dictionary preflight handles unaligned bit-packed values" (Read.validateDictionaryExpansion 13 offsets 5 packed == Right ())
        check "dictionary preflight checks packed prefix budgets" (isLeft (Read.validateDictionaryExpansion 12 offsets 5 packed))
    )
    [1 .. 32]
  let clipped = BS.pack [2, 3, 244, 255, 128]
      offsets = S.fromList [0, 2, 5]
  check "unused invalid packed indices and truncated suffix are not inspected" (Read.validateDictionaryExpansion 5 offsets 2 clipped == Right ())
  check "a requested invalid packed index is rejected" (isLeft (Read.validateDictionaryExpansion 100 offsets 3 clipped))
  check "unsigned 32-bit dictionary indices are range checked before offset lookup" (isLeft (Read.validateDictionaryExpansion 100 offsets 1 (BS.pack [32, 2, 255, 255, 255, 255])))
  check "an unused index stream and dictionary need not be inspected" (Read.validateDictionaryExpansion 0 S.empty 0 BS.empty == Right ())
  check "implicit zero-width references need no run or vector allocation" (Read.validateDictionaryExpansion 0 (S.fromList [0, 0]) maxBound (BS.singleton 0) == Right ())
  check "huge implicit references reject without multiplication overflow" (isLeft (Read.validateDictionaryExpansion 8 (S.fromList [0, 1]) maxBound (BS.singleton 0)))
  -- Run lengths constrain the declared stream, even when only one value
  -- is requested. Packed lengths count values, not groups of eight.
  mapM_
    ( \width -> do
        let padded = BS.replicate ((width + 7) `quot` 8) 0
            maxRepeated = BS.pack [254, 255, 255, 255, 15] <> padded
            oversizedRepeated = BS.pack [128, 128, 128, 128, 16] <> padded
            oversizedPacked = BS.pack [129, 128, 128, 128, 2]
            validate payload = Read.validateDictionaryExpansion 0 (S.fromList [0, 0]) 1 (BS.singleton (fromIntegral width) <> payload)
        check "maximum signed 32-bit repeated runs allow clipped decoding" $
          Read.decodeHybridRleUnsigned32 width 1 maxRepeated == Right (P.singleton 0)
            && validate maxRepeated == Right ()
        mapM_
          ( \(payload, expected) -> do
              check "decoder rejects an oversized declared run before clipping" (Read.decodeHybridRleUnsigned32 width 1 payload == Left expected)
              check "dictionary preflight rejects an oversized declared run before clipping" (validate payload == Left expected)
          )
          [ (oversizedRepeated, "Parquet.RLE: RLE run exceeds signed 32-bit length")
          , (oversizedPacked, "Parquet.RLE: bit-packed run exceeds signed 32-bit length")
          ]
    )
    [1 .. 32]
  -- One million bit-packed references to an empty entry. The preflight
  -- must scan them without allocating per-index nodes or an output vector.
  let packedZeros = BS.pack [1, 145, 161, 15] <> BS.replicate 125000 0
  _ <- evaluate (BS.length packedZeros)
  beforePacked <- getAllocationCounter
  packedValid <- evaluate (isRight (Read.validateDictionaryExpansion 0 (S.fromList [0, 0]) 1000000 packedZeros))
  afterPacked <- getAllocationCounter
  let packedAllocated = beforePacked - afterPacked
  putStrLn ("packed dictionary preflight allocated " ++ show packedAllocated ++ " bytes for one million indices")
  check "large packed dictionary preflight remains valid" packedValid
  check "packed dictionary preflight has no per-index allocation" (packedAllocated < 64 * 1024)
  mapM_
    (\badOffsets -> check "dictionary preflight rejects invalid offset arrays" (isLeft (Read.validateDictionaryExpansion 8 badOffsets 1 (BS.singleton 0))))
    [S.empty, S.singleton 0, S.fromList [-1, 0], S.fromList [0, 3, 2]]


dictionaryAllocationRegression :: IO ()
dictionaryAllocationRegression = do
  let dictionary = Write.Dictionary (Write.ColByteArray (V.fromList [BS.empty, BS.replicate 4096 120])) P.empty
      -- Width 1, RLE run of 1,000,000 references to entry 1. Entry 0 is
      -- empty, so a minimum-entry-length estimate cannot detect overflow.
      body = BS.pack [1, 128, 137, 122, 1]
      header = Page.PageHeader (Page.PtDataPage (Page.DataPageHeader 1000000 8)) (Just 5) (Just 5)
      chunk = Write.encodeDictPage dictionary <> Write.encodePageHeader header <> body
  _ <- evaluate (BS.length chunk)
  before <- getAllocationCounter
  rejected <- evaluate (isLeft (Read.readBinaryBuffersColumnChunk Uncompressed chunk))
  after <- getAllocationCounter
  let allocated = before - after
  putStrLn ("oversized dictionary rejection allocated " ++ show allocated ++ " bytes")
  check "oversized dictionary expansion is rejected" rejected
  check "oversized dictionary expansion is rejected before allocating index/offset vectors" (allocated < 1024 * 1024)
  let emptyBody = BS.pack [1, 128, 137, 122, 0]
      emptyChunk = Write.encodeDictPage dictionary <> Write.encodePageHeader header <> emptyBody
  check
    "large empty outputs remain valid despite an unused large dictionary entry"
    ( case Read.readBinaryBuffersColumnChunk Uncompressed emptyChunk of
        Left _ -> False
        Right buffers -> S.length (Read.binaryOffsets buffers) == 1000001 && S.last (Read.binaryOffsets buffers) == 0 && BS.null (Read.binaryData buffers)
    )


dictionarySequenceRegression :: IO ()
dictionarySequenceRegression = do
  let values = V.fromList ["a", "b"]
      dictionary = Write.Dictionary (Write.ColByteArray values) (P.fromList [0, 1])
      dictionaryPage = Write.encodeDictPage dictionary
      dictionaryData = Write.encodeDictDataPage dictionary
      (header, body) = Write.encodeColumnDataPageParts (Write.ColByteArray values)
      plain = header <> body
      valid = dictionaryPage <> dictionaryData <> plain
  check "a dictionary may fall back to PLAIN pages" (Read.readGenericByteArrayColumnChunk Uncompressed valid == Right (values <> values))
  mapM_
    ( \bad -> do
        check "required byte reader rejects late or replacement dictionaries" (isLeft (Read.readGenericByteArrayColumnChunk Uncompressed bad))
        check "buffer reader rejects late or replacement dictionaries" (isLeft (Read.readBinaryBuffersColumnChunk Uncompressed bad))
        check "leveled reader rejects late or replacement dictionaries" (isLeft (Read.readByteArrayLevels Uncompressed 0 0 bad))
    )
    [plain <> dictionaryPage <> dictionaryData, dictionaryPage <> dictionaryPage <> dictionaryData, dictionaryPage <> dictionaryData <> dictionaryPage <> dictionaryData]
  let ints = Write.Dictionary (Write.ColInt32 (P.fromList [1, 2])) (P.fromList [0, 1])
      intsPage = Write.encodeDictPage ints
      intsData = Write.encodeDictDataPage ints
      duplicate = intsPage <> intsPage <> intsData
  check "legacy required dictionary reader rejects duplicate dictionaries" (isLeft (Read.readPlainDictionaryInt32ColumnChunk Uncompressed duplicate))
  check "legacy optional dictionary reader rejects duplicate dictionaries" (isLeft (Read.readDictionaryInt32OptionalColumnChunk Uncompressed 0 0 duplicate))


scalarBuffersRegression :: IO ()
scalarBuffersRegression = do
  words32 <- H.check $ H.withTests 100 $ H.property $ do
    values <- H.forAll (Gen.list (Range.linear 0 128) (Gen.word32 Range.constantBounded))
    let split = length values `quot` 2
        bytes xs = Lazy.toStrict (Builder.toLazyByteString (foldMap Builder.word32LE xs))
        chunk = plain split (bytes (take split values)) <> plain (length values - split) (bytes (drop split values))
        decode physical = fmap (S.toList . (Buffer.bytesToStorable :: ByteString -> S.Vector Word32) . Read.scalarData) (Read.readScalarBuffersColumnChunk physical Uncompressed (BS.drop 1 (BS.cons 0 chunk)))
    decode PTInt32 H.=== Right values
    decode PTFloat H.=== Right values
  check "native multi-page INT32/FLOAT buffers preserve arbitrary bits and unaligned input" words32
  words64 <- H.check $ H.withTests 100 $ H.property $ do
    values <- H.forAll (Gen.list (Range.linear 0 128) (Gen.word64 Range.constantBounded))
    let split = length values `quot` 2
        bytes xs = Lazy.toStrict (Builder.toLazyByteString (foldMap Builder.word64LE xs))
        chunk = plain split (bytes (take split values)) <> plain (length values - split) (bytes (drop split values))
        decode physical = fmap (S.toList . (Buffer.bytesToStorable :: ByteString -> S.Vector Word64) . Read.scalarData) (Read.readScalarBuffersColumnChunk physical Uncompressed chunk)
    decode PTInt64 H.=== Right values
    decode PTDouble H.=== Right values
  check "native multi-page INT64/DOUBLE buffers preserve arbitrary bits" words64
  let entries = [0x80000000, 0x7fc00001, 0x7fa12345, 0xffffffff] :: [Word32]
      raw = Lazy.toStrict (Builder.toLazyByteString (foldMap Builder.word32LE entries))
      dictionary = page (Page.PtDictionaryPage (Page.DictionaryPageHeader 4 0)) raw
      indices = P.fromList [3, 0, 2, 1, 2, 3]
      encoded = BS.singleton 2 <> Levels.encodeRLEHybrid 2 indices
      dataPage = page (Page.PtDataPage (Page.DataPageHeader 6 8)) encoded
      decode chunk = fmap (S.toList . (Buffer.bytesToStorable :: ByteString -> S.Vector Word32) . Read.scalarData) (Read.readScalarBuffersColumnChunk PTFloat Uncompressed chunk)
      expected = map (entries !!) [3, 0, 2, 1, 2, 3]
  check "dictionary scalar gathers preserve NaN payloads and signed zero across pages" (decode (dictionary <> dataPage <> dataPage) == Right (expected <> expected))
  check "dictionary scalar buffers preserve a PLAIN fallback" (decode (dictionary <> dataPage <> plain 4 raw) == Right (expected <> entries))
  let entries64 = [0x8000000000000000, 0x7ff8000000000001, 0x7ff0123456789abc, 0xffffffffffffffff] :: [Word64]
      raw64 = Lazy.toStrict (Builder.toLazyByteString (foldMap Builder.word64LE entries64))
      dictionary64 = page (Page.PtDictionaryPage (Page.DictionaryPageHeader 4 0)) raw64
      expected64 = map (entries64 !!) [3, 0, 2, 1, 2, 3]
      chunk64 = dictionary64 <> dataPage <> plain 4 raw64
  mapM_
    ( \offset -> do
        let input = BS.drop offset (BS.replicate offset 0 <> chunk64)
            decoded physical = fmap (S.toList . (Buffer.bytesToStorable :: ByteString -> S.Vector Word64) . Read.scalarData) (Read.readScalarBuffersColumnChunk physical Uncompressed input)
        check "64-bit dictionary scalar buffers preserve bits at every source alignment" (decoded PTDouble == Right (expected64 <> entries64) && decoded PTInt64 == Right (expected64 <> entries64))
    )
    [0 .. 7]
  let bad = page (Page.PtDataPage (Page.DataPageHeader 1 8)) (BS.pack [3, 2, 4])
  check "scalar buffers reject invalid dictionary indices" (isLeft (decode (dictionary <> bad)))
  check "scalar buffers reject late dictionaries" (isLeft (decode (plain 4 raw <> dictionary <> dataPage)))
  mapM_ (\n -> check "scalar buffers reject invalid counts before allocation" (isLeft (decode (plain n (BS.singleton 0))))) ([-1, 1, fromIntegral (maxBound :: Int32)] :: [Int])
  check "scalar buffers reject unsupported physical types" (isLeft (Read.readScalarBuffersColumnChunk PTBoolean Uncompressed BS.empty))
  where
    plain :: Int -> ByteString -> ByteString
    plain n = page (Page.PtDataPage (Page.DataPageHeader (fromIntegral n) 0))
    page :: Page.PageType -> ByteString -> ByteString
    page typ bytes =
      Write.encodePageHeader (Page.PageHeader typ (Just (fromIntegral (BS.length bytes))) (Just (fromIntegral (BS.length bytes)))) <> bytes


nullableAssemblyRegression :: IO ()
nullableAssemblyRegression = do
  mapM_
    ( \width -> do
        check "empty hybrid level streams do not read a first element" (Levels.encodeRLEHybrid width P.empty == BS.empty)
        check "empty V1 level streams retain only the required length prefix" (Levels.encodeLengthPrefixedHybrid width P.empty == if width == 0 then BS.empty else BS.replicate 4 0)
    )
    [0 .. 32]
  passed <- H.check $ H.withTests 200 $ H.property $ do
    base <- H.forAll (Gen.list (Range.linear 0 128) (Gen.int32 Range.constantBounded))
    flags <- H.forAll (Gen.list (Range.singleton (length base)) Gen.bool)
    mode <- H.forAll (Gen.element [0 :: Int, 1, 2])
    v2 <- H.forAll Gen.bool
    let rows = zipWith (\present value -> if mode == 0 || (mode == 2 && present) then Just value else Nothing) flags base
        definitions = map (maybe 0 (const 1)) rows
        values = V.toList (V.mapMaybe id (V.fromList rows))
        leaf = SchemaElement "x" (Just Optional) (Just PTInt32) Nothing Nothing Nothing Nothing Nothing
        file = fixtureFile (indexed (V.fromList [root, leaf]) (V.singleton "x") (V.singleton (levelPage v2 1 definitions values 0)) Nothing)
        target = field (T.AInt 32 True) True
        verify column expected = do
          array <- H.evalMaybe (A.asPrim A.PInt32 column)
          V.generate (A.columnLength column) (A.unsafePrimAt array) H.=== V.fromList expected
          A.nullCount column H.=== length (filter (== Nothing) expected)
          case A.validity column of
            Nothing -> H.assert (all (/= Nothing) expected)
            Just validity -> do
              let bits = A.validityBits validity
                  bytes = Buffer.bitmapBytes bits
                  remainder = length expected `rem` 8
              Buffer.bitmapOffset bits H.=== 0
              Buffer.bitmapLength bits H.=== length expected
              if remainder == 0 then pure () else (BS.last bytes `shiftR` remainder) H.=== 0
    H.cover 20 "all valid" (length values == length rows)
    H.cover 20 "all null" (null values)
    H.cover 10 "mixed mask" (not (null values) && length values < length rows)
    H.annotate "full column decode"
    column <- H.evalEither (Arrow.readParquetColumn file 0 0 target)
    verify column rows
    H.annotate "selected column decode"
    selected <- H.evalEither (Arrow.readParquetColumnWithPagePruning file 0 0 target (Pred.PGtEq (Pred.PVInt32 minBound)))
    verify (snd selected) (if null values then [] else rows)
  check "nullable primitive assembly preserves rows, null counts and bitmap tails for both page versions" passed
  let definitions = [1, 0, 1, 1, 0, 1, 1, 0, 1]
      words32 = [0x80000000, 0x7fa00001, 0x7fc12345, 0xff800000, 0, 0xffffffff] :: [Word32]
      words64 = [0x8000000000000000, 0x7ff0123456789abc, 0x7ff8000000000042, 0xfff0000000000000, 0, 0xffffffffffffffff] :: [Word64]
      expected values = case values of
        [a, b, c, d, e, f] -> [Just a, Nothing, Just b, Just c, Nothing, Just d, Just e, Nothing, Just f]
        _ -> error "invalid floating bit-pattern fixture"
      file typ column =
        let leaf = SchemaElement "x" (Just Optional) (Just typ) Nothing Nothing Nothing Nothing Nothing
            pages = V.fromList [levelPageData False 1 definitions column 0, levelPageData True 1 definitions column 0]
        in fixtureFile (indexed (V.fromList [root, leaf]) (V.singleton "x") pages Nothing)
  floats <- either error pure (Arrow.readParquetColumn (file PTFloat (Write.ColFloat (P.fromList (map castWord32ToFloat words32)))) 0 0 (field (T.AFloatingPoint T.Single) True))
  doubles <- either error pure (Arrow.readParquetColumn (file PTDouble (Write.ColDouble (P.fromList (map castWord64ToDouble words64)))) 0 0 (field (T.AFloatingPoint T.DoublePrecision) True))
  let floatArray = maybe (error "missing FLOAT array") id (A.asPrim A.PFloat floats)
      doubleArray = maybe (error "missing DOUBLE array") id (A.asPrim A.PDouble doubles)
  check "nullable FLOAT assembly preserves NaN payloads, infinities and signed zero across V1/V2 pages" (V.generate 18 (fmap castFloatToWord32 . A.unsafePrimAt floatArray) == V.fromList (expected words32 <> expected words32))
  check "nullable DOUBLE assembly preserves NaN payloads, infinities and signed zero across V1/V2 pages" (V.generate 18 (fmap castDoubleToWord64 . A.unsafePrimAt doubleArray) == V.fromList (expected words64 <> expected words64))


deltaEncodingRegression :: IO ()
deltaEncodingRegression = do
  let wrapUp = P.fromList [minBound, maxBound, minBound]
      wrapDown = P.fromList [maxBound, minBound, maxBound]
      upGolden = BS.pack [128, 1, 4, 3, 255, 255, 255, 255, 15, 1, 2, 0, 0, 0, 8] <> BS.replicate 7 0
      downGolden = BS.pack [128, 1, 4, 3, 254, 255, 255, 255, 15, 1, 2, 0, 0, 0, 2] <> BS.replicate 7 0
  check "INT32 delta differences wrap before widening (minimum to maximum)" (Delta.encodeDeltaBinaryPackedInt32 wrapUp == upGolden)
  check "INT32 delta differences wrap before widening (maximum to minimum)" (Delta.encodeDeltaBinaryPackedInt32 wrapDown == downGolden)
  let span64 = P.fromList [0, minBound, minBound + 1]
      spanGolden =
        BS.pack [128, 1, 4, 3, 0]
          <> BS.replicate 9 255
          <> BS.pack [1, 64, 0, 0, 0]
          <> BS.replicate 8 0
          <> BS.pack [1, 0, 0, 0, 0, 0, 0, 128]
          <> BS.replicate 240 0
  check "INT64 delta miniblocks retain adjusted values with their high bit set" (Delta.encodeDeltaBinaryPackedInt64 span64 == spanGolden)
  check "INT64 high-bit delta golden decodes without losing the last value" (Read.decodeDeltaBinaryPackedInt64 3 spanGolden == Right span64)
  let empty = BS.pack [128, 1, 4, 0, 0]
  check "empty typed delta streams keep a complete header" (Delta.encodeDeltaBinaryPackedInt32 P.empty == empty && Delta.encodeDeltaBinaryPackedInt64 P.empty == empty)
  int32 <- H.check $ H.withTests 200 $ H.property $ do
    values <- H.forAll (Gen.list (Range.linear 0 300) (Gen.int32 Range.constantBounded))
    let vector = P.fromList values
    Read.decodeDeltaBinaryPackedInt32 (length values) (Delta.encodeDeltaBinaryPackedInt32 vector) H.=== Right vector
  check "typed INT32 delta round-trips cover the full numeric range and block boundaries" int32
  int64 <- H.check $ H.withTests 200 $ H.property $ do
    values <- H.forAll (Gen.list (Range.linear 0 300) (Gen.int64 Range.constantBounded))
    let vector = P.fromList values
    Read.decodeDeltaBinaryPackedInt64 (length values) (Delta.encodeDeltaBinaryPackedInt64 vector) H.=== Right vector
  check "typed INT64 delta round-trips cover the full numeric range and block boundaries" int64


data DeltaPackingCase = DeltaPackingCase
  { packingInput :: !(P.Vector Int64)
  , packingExpected :: !ByteString
  }


deltaPackingCase :: Int -> P.Vector Word64 -> DeltaPackingCase
deltaPackingCase width randomOffsets = DeltaPackingCase input (header <> payload)
  where
    count = P.length randomOffsets
    mask = if width == 64 then maxBound else (1 `shiftL` width) - 1
    offsets = P.imap (\i value -> if i == 0 then 0 else if i == 1 then 1 `shiftL` (width - 1) else value .&. mask) randomOffsets
    deltas = P.map (\offset -> minBound + fromIntegral offset) offsets :: P.Vector Int64
    input = P.scanl' (+) 0 deltas
    header = BS.pack [128, 1, 4, fromIntegral (count + 1), 0] <> BS.replicate 9 255 <> BS.pack [1, fromIntegral width, 0, 0, 0]
    payload = BS.pack (map byteAt [0 .. 4 * width - 1])
    -- Independent bit-by-bit oracle, including zero padding of the
    -- 32-value miniblock. The writer uses word-sized accumulator stores.
    byteAt byte = foldl' (\acc bit -> let absolute = byte * 8 + bit; lane = absolute `quot` width in if lane < count && testBit (offsets P.! lane) (absolute `rem` width) then setBit acc bit else acc) (0 :: Word8) [0 .. 7]


deltaPackingRegression :: IO ()
deltaPackingRegression = do
  mapM_
    ( \width -> do
        let sample = deltaPackingCase width (P.generate (2 + width `rem` 31) (\i -> fromIntegral i * 104729))
        check "delta word packing matches the bit oracle at every width and padded tail" (Delta.encodeDeltaBinaryPackedInt64 (packingInput sample) == packingExpected sample)
    )
    [1 .. 64]
  passed <- H.check $ H.withTests 200 $ H.property $ do
    width <- H.forAll (Gen.int (Range.constant 1 64))
    values <- H.forAll (Gen.list (Range.linear 2 32) (Gen.word64 Range.constantBounded))
    let sample = deltaPackingCase width (P.fromList values)
        encoded = Delta.encodeDeltaBinaryPackedInt64 (packingInput sample)
    encoded H.=== packingExpected sample
    Read.decodeDeltaBinaryPackedInt64 (length values + 1) encoded H.=== Right (packingInput sample)
  check "delta word packing matches independent bytes for arbitrary widths, values and final padding" passed


dictionaryFloatingRegression :: IO ()
dictionaryFloatingRegression = do
  let bits32 = P.fromList [0x3f800000, 0x7fc01234, 0xffa05678, 0x7fc01234, 0, 0x80000000, 0, 0x80000000]
      bits64 = P.fromList [0x3ff0000000000000, 0x7ff8123456789abc, 0xfff056789abcdef0, 0x7ff8123456789abc, 0, 0x8000000000000000, 0, 0x8000000000000000]
      expectedIndices = P.fromList [0, 1, 2, 1, 3, 4, 3, 4]
      dictionary32 = Write.buildDictionary (Write.ColFloat (P.map castWord32ToFloat bits32))
      dictionary64 = Write.buildDictionary (Write.ColDouble (P.map castWord64ToDouble bits64))
  check "Float dictionaries distinguish NaN payloads and signed zero, retaining first appearance" $
    Write.dictIndices dictionary32 == expectedIndices
      && case Write.dictUniques dictionary32 of
        Write.ColFloat values -> P.map castFloatToWord32 values == P.fromList [0x3f800000, 0x7fc01234, 0xffa05678, 0, 0x80000000]
        _ -> False
  check "Double dictionaries distinguish NaN payloads and signed zero, retaining first appearance" $
    Write.dictIndices dictionary64 == expectedIndices
      && case Write.dictUniques dictionary64 of
        Write.ColDouble values -> P.map castDoubleToWord64 values == P.fromList [0x3ff0000000000000, 0x7ff8123456789abc, 0xfff056789abcdef0, 0, 0x8000000000000000]
        _ -> False
  let roundtrip32 bits =
        let dictionary = Write.buildDictionary (Write.ColFloat (P.map castWord32ToFloat bits))
            pages = Write.encodeDictPage dictionary <> Write.encodeDictDataPage dictionary
        in P.map castFloatToWord32 . Read.clValues <$> Read.readFloatLevels Uncompressed 0 0 pages
      roundtrip64 bits =
        let dictionary = Write.buildDictionary (Write.ColDouble (P.map castWord64ToDouble bits))
            pages = Write.encodeDictPage dictionary <> Write.encodeDictDataPage dictionary
        in P.map castDoubleToWord64 . Read.clValues <$> Read.readDoubleLevels Uncompressed 0 0 pages
  check "Float dictionary pages preserve NaNs and signed zero" (roundtrip32 bits32 == Right bits32)
  check "Double dictionary pages preserve NaNs and signed zero" (roundtrip64 bits64 == Right bits64)
  passed32 <- H.check $ H.property $ do
    values <- H.forAll (Gen.list (Range.linear 0 100) (Gen.word32 Range.constantBounded))
    let bits = bits32 P.++ P.fromList (values ++ reverse values)
    roundtrip32 bits H.=== Right bits
  check "Float dictionary pages preserve arbitrary IEEE bits and duplicate entries" passed32
  passed64 <- H.check $ H.property $ do
    values <- H.forAll (Gen.list (Range.linear 0 100) (Gen.word64 Range.constantBounded))
    let bits = bits64 P.++ P.fromList (values ++ reverse values)
    roundtrip64 bits H.=== Right bits
  check "Double dictionary pages preserve arbitrary IEEE bits and duplicate entries" passed64


-- An intentionally simple list oracle is independent of the writer's
-- mutable hash table. It also specifies stable first-appearance ids.
dictionaryModel :: Eq a => ([a] -> Write.ColumnData) -> [a] -> Write.Dictionary
dictionaryModel makeColumn values = Write.Dictionary (makeColumn uniques) indices
  where
    uniques = List.nub values
    indices = P.fromList (map (\value -> maybe (error "dictionary oracle lost a value") fromIntegral (List.elemIndex value uniques)) values)


-- Independent copies force content equality rather than address equality.
-- Interior mismatches share the sampled end key and must be rejected by
-- the byte comparison before adaptive refinement can choose another key.
dictionaryEqualityRegression :: IO ()
dictionaryEqualityRegression = do
  mapM_
    ( \width ->
        mapM_
          ( \prefix ->
              mapM_
                ( \position -> do
                    let original = BS.replicate width 97
                        different = BS.take position original <> BS.singleton 98 <> BS.drop (position + 1) original
                        sliced ordinal bytes = BS.take width (BS.drop prefix (BS.replicate prefix ordinal <> bytes <> BS.singleton ordinal))
                        values = zipWith sliced [0 ..] [original, different, original, different]
                    unless
                      (Write.buildDictionary (Write.ColByteArray (V.fromList values)) == dictionaryModel (Write.ColByteArray . V.fromList) values)
                      (fail ("dictionary byte equality: width " ++ show width ++ ", prefix " ++ show prefix ++ ", mismatch " ++ show position))
                )
                [0 .. width - 1]
          )
          [0 .. 7]
    )
    [9 .. 128]
  putStrLn "OK: dictionary byte equality preserves all 65,760 length/alignment/mismatch cases"


-- Two different interior words change the same bit, while both ends stay
-- equal. These remain distinct values even when a parity-style reduction
-- of their word differences would cancel. Independent slices also keep
-- address equality and surrounding bytes out of the comparison.
dictionaryEqualityCancellationRegression :: IO ()
dictionaryEqualityCancellationRegression = do
  let original = BS.replicate 64 97
  mapM_
    ( \firstWord ->
        mapM_
          ( \secondWord ->
              mapM_
                ( \byteInWord ->
                    mapM_
                      ( \bitInByte ->
                          mapM_
                            ( \prefix -> do
                                let first = firstWord * 8 + byteInWord
                                    second = secondWord * 8 + byteInWord
                                    mask = 1 `shiftL` bitInByte
                                    different = BS.pack $ map (\offset -> let value = BS.index original offset in if offset == first || offset == second then value `xor` mask else value) [0 .. 63]
                                    sliced ordinal bytes = BS.take 64 (BS.drop prefix (BS.replicate prefix ordinal <> bytes <> BS.singleton ordinal))
                                    values = zipWith sliced [0 ..] [original, different, original, different]
                                unless
                                  (Write.buildDictionary (Write.ColByteArray (V.fromList values)) == dictionaryModel (Write.ColByteArray . V.fromList) values)
                                  (fail ("dictionary paired differences: words " ++ show firstWord ++ "/" ++ show secondWord ++ ", byte " ++ show byteInWord ++ ", bit " ++ show bitInByte ++ ", prefix " ++ show prefix))
                            )
                            [0 .. 7]
                      )
                      [0 .. 7]
                )
                [0 .. 7]
          )
          [firstWord + 1 .. 6]
    )
    [1 .. 5]
  putStrLn "OK: dictionary byte equality preserves all 7,680 paired interior changes"


dictionaryConstructionRegression :: IO ()
dictionaryConstructionRegression = do
  mapM_
    ( \width -> do
        let count = 32768
            pool = V.generate 8 (\key -> BS.singleton (fromIntegral key) <> BS.replicate (width - 1) 97)
        source <- V.generateM count (\i -> BS.useAsCStringLen (V.unsafeIndex pool (i `rem` 8)) BS.packCStringLen)
        _ <- evaluate (V.foldl' (\total value -> total + BS.length value) 0 source)
        before <- getAllocationCounter
        dictionary <- evaluate (Write.buildDictionary (Write.ColByteArray source))
        after <- getAllocationCounter
        check ("dictionary row bookkeeping avoids per-row thunks: " ++ show (before - after) ++ " bytes") $
          before - after < fromIntegral (count * 6 + 32768)
            && Write.columnDataLength (Write.dictUniques dictionary) == 8
            && P.length (Write.dictIndices dictionary) == count
    )
    ([1 .. 9] ++ [15, 16, 17, 31, 32, 33, 63, 64, 65, 127, 128, 129, 256])
  let uniqueCount = 8192
      uniqueBytes key = BS.pack (map (\shift -> fromIntegral ((fromIntegral key :: Word64) `shiftR` shift)) [0, 8 .. 56])
  uniqueSource <- evaluate (V.generate uniqueCount uniqueBytes)
  _ <- evaluate (V.foldl' (\total value -> total + BS.length value) 0 uniqueSource)
  beforeUnique <- getAllocationCounter
  uniqueDictionary <- evaluate (Write.buildDictionary (Write.ColByteArray uniqueSource))
  afterUnique <- getAllocationCounter
  check ("dictionary uniques avoid deferred indexing closures: " ++ show (beforeUnique - afterUnique) ++ " bytes") $
    beforeUnique - afterUnique < fromIntegral (uniqueCount * 90 + 32768)
      && Write.columnDataLength (Write.dictUniques uniqueDictionary) == uniqueCount
  mapM_
    ( \prefix -> do
        let tagged = concatMap (\size -> [BS.replicate size 0, BS.replicate size 255, BS.pack (take size [1 .. 7])]) [0 .. 7]
            eight = [BS.replicate 8 0, BS.replicate 8 255, BS.pack [255, 0, 255, 0, 255, 0, 255, 0]]
            mixed = tagged ++ eight
            sliced bytes = BS.take (BS.length bytes) (BS.drop prefix (BS.replicate prefix 0xa5 <> bytes <> BS.replicate 8 0x5a))
        mapM_
          ( \original -> do
              let values = map sliced (original ++ reverse original)
              check "exact short-key dispatch preserves lengths, full high bits and unaligned slices" $
                Write.buildDictionary (Write.ColByteArray (V.fromList values)) == dictionaryModel (Write.ColByteArray . V.fromList) values
          )
          [tagged, eight, mixed]
    )
    [0 .. 7]
  -- Matching ends with distinct middle bytes force the full-hash mode.
  let promotionPrefix = [BS.replicate 64 0x42, BS.replicate 8 0x42 <> BS.singleton 0x43 <> BS.replicate 55 0x42, BS.replicate 24 0x42 <> BS.singleton 0x44 <> BS.replicate 39 0x42]
  -- Distinct 64-byte values chosen to collide in the medium lookup mix.
  -- One differs at the first word, the other only in the last 16 bytes.
  let mediumBase = BS.replicate 64 97
      earlyCollision = BS.concat [BS.pack [96, 97, 97, 97, 97, 97, 97, 97], BS.replicate 24 97, BS.pack [254, 171, 118, 155, 54, 239, 41, 199], BS.replicate 24 97]
      lateCollision = BS.concat [BS.replicate 48 97, BS.pack [96, 97, 97, 97, 97, 97, 97, 97], BS.pack [45, 123, 240, 164, 64, 21, 0, 38]]
      mediumSlice bytes = BS.take 64 (BS.drop 3 (BS.replicate 3 0xa5 <> bytes <> BS.replicate 8 0x5a))
      mediumValues = map mediumSlice (promotionPrefix ++ [mediumBase, earlyCollision, mediumBase, lateCollision, earlyCollision, lateCollision])
  check "medium-key collisions compare all words, including late mismatches in unaligned values" $
    Write.buildDictionary (Write.ColByteArray (V.fromList mediumValues)) == dictionaryModel (Write.ColByteArray . V.fromList) mediumValues
  let larger = BS.replicate 129 97
      largerHash = Hash.xxh64 0 larger
      sameKey = BS.pack (map (\shift -> fromIntegral (largerHash `shiftR` shift)) [0, 8 .. 56])
      collisions = promotionPrefix ++ [larger, sameKey, larger, sameKey]
  check "a large-value hash colliding with an exact short key still compares full payloads" $
    Write.buildDictionary (Write.ColByteArray (V.fromList collisions)) == dictionaryModel (Write.ColByteArray . V.fromList) collisions
  -- These distinct 129-byte fixtures collide in XXH64, including length.
  -- Altering a pair of words in one lane preserves its accumulator.
  let largeEarly = BS.concat [BS.pack [96, 97, 97, 97, 97, 97, 97, 97], BS.replicate 24 97, BS.pack [239, 26, 196, 184, 36, 158, 242, 163], BS.replicate 89 97]
      largeLate = BS.concat [BS.replicate 64 97, BS.pack [96, 97, 97, 97, 97, 97, 97, 97], BS.replicate 24 97, BS.pack [56, 26, 164, 255, 164, 182, 226, 68], BS.replicate 25 97]
      largeSlice bytes = BS.take 129 (BS.drop 5 (BS.replicate 5 0xa5 <> bytes <> BS.replicate 8 0x5a))
      largeValues = promotionPrefix ++ map largeSlice [larger, largeEarly, larger, largeLate, largeEarly, largeLate]
  check "large collision fixtures have equal hashes and distinct payloads" $
    Hash.xxh64 0 larger == Hash.xxh64 0 largeEarly
      && Hash.xxh64 0 larger == Hash.xxh64 0 largeLate
      && length (List.nub largeValues) == 6
  check "same-length large hashes still compare full unaligned payloads" $
    Write.buildDictionary (Write.ColByteArray (V.fromList largeValues)) == dictionaryModel (Write.ColByteArray . V.fromList) largeValues
  mapM_
    ( \prefix -> do
        let originals = map (\size -> BS.replicate size 0) [0 .. 9] ++ map (\size -> BS.pack (take size [1 .. 8])) [0 .. 8] ++ map (\size -> BS.pack (take size (cycle [1 .. 255]))) [15, 16, 17, 31, 32, 33, 63, 64, 65, 127, 128, 129, 255, 256, 257]
            slice i bytes = BS.take (BS.length bytes) (BS.drop prefix (BS.replicate prefix (fromIntegral (i * 17)) <> bytes <> BS.replicate 8 (fromIntegral i)))
            values = zipWith slice [0 :: Int ..] (originals ++ reverse originals)
        check "short byte keys preserve lengths, zero padding and unaligned slices" $
          Write.buildDictionary (Write.ColByteArray (V.fromList values)) == dictionaryModel (Write.ColByteArray . V.fromList) values
    )
    [0 .. 7]
  mapM_
    ( \distinct -> do
        let values = take (if distinct == 0 then 0 else distinct * 2 + 19) (cycle [0 .. distinct - 1])
            integers = map (\value -> fromIntegral value * 0x100000001) values :: [Int64]
            bytes = map (\value -> BS.replicate (1 + value `rem` 7) (fromIntegral value)) values
            sliced input = P.slice 1 (P.length input) (P.singleton 999 P.++ input P.++ P.singleton 999)
            actual64 = Write.buildDictionary (Write.ColInt64 (sliced (P.fromList integers)))
            actualBytes = Write.buildDictionary (Write.ColByteArray (V.slice 1 (length bytes) (V.fromList ("sentinel" : bytes ++ ["sentinel"]))))
        check "dictionary growth/empty/sliced INT64 inputs retain stable values and ids" (actual64 == dictionaryModel (Write.ColInt64 . P.fromList) integers)
        check "dictionary growth/empty/sliced byte inputs retain stable values and ids" (actualBytes == dictionaryModel (Write.ColByteArray . V.fromList) bytes)
    )
    [0, 1, 31, 32, 33, 63, 64, 65, 127, 128, 129, 257]
  -- Re-key after earlier table growth, then grow again in full-hash mode.
  -- Prefix rows, repetitions and the triggering row must keep their ids.
  mapM_
    ( \distinct -> do
        let word :: Int -> ByteString
            word key = BS.pack (map (\shift -> fromIntegral ((fromIntegral key :: Word64) `shiftR` shift)) [0, 8 .. 56])
            value key = word key <> BS.replicate 56 97
            prefix = map value [0 .. distinct - 1]
            ambiguous = word 0 <> BS.singleton 98 <> BS.replicate 55 97
            secondAmbiguous = word 0 <> BS.replicate 16 97 <> BS.singleton 99 <> BS.replicate 39 97
            values = prefix ++ reverse prefix ++ [ambiguous] ++ map value [distinct .. distinct * 2 + 1] ++ [secondAmbiguous, value 0, ambiguous, secondAmbiguous] ++ prefix
        check "adaptive dictionaries preserve ids across both late promotions and subsequent growth" $
          Write.buildDictionary (Write.ColByteArray (V.fromList values)) == dictionaryModel (Write.ColByteArray . V.fromList) values
    )
    [1, 31, 32, 33, 63, 64, 65, 127, 128, 129, 257]
  mapM_
    ( \(width, refined) -> do
        source <- V.generateM 32768 $ \i ->
          let offset = if refined then 24 else 0
              value = BS.replicate offset 97 <> BS.singleton (fromIntegral (i `rem` 8)) <> BS.replicate (width - offset - 1) 97
          in BS.useAsCStringLen value BS.packCStringLen
        _ <- evaluate (V.foldl' (\total value -> total + BS.length value) 0 source)
        before <- getAllocationCounter
        dictionary <- evaluate (Write.buildDictionary (Write.ColByteArray source))
        after <- getAllocationCounter
        check ("byte dictionary scan retains bounded allocation at " ++ show width ++ " bytes, refined " ++ show refined ++ ": " ++ show (before - after)) $
          before - after < 32768 * 6 + 32768
            && Write.columnDataLength (Write.dictUniques dictionary) == 8
            && P.length (Write.dictIndices dictionary) == 32768
    )
    [(9, False), (64, False), (128, False), (64, True), (128, True)]
  mapM_
    ( \width ->
        mapM_
          ( \wideHint -> do
              source <- V.generateM 32768 $ \i ->
                let key = i `rem` 8
                    (high, low) = key `quotRem` 2
                    size = if wideHint && i == 3 then 256 else width
                    value = BS.replicate 8 97 <> BS.singleton (fromIntegral low) <> BS.replicate 15 97 <> BS.singleton (fromIntegral high) <> BS.replicate (size - 25) 97
                in BS.useAsCStringLen value BS.packCStringLen
              _ <- evaluate (V.foldl' (\total value -> total + BS.length value) 0 source)
              beforePromotion <- getAllocationCounter
              dictionary <- evaluate (Write.buildDictionary (Write.ColByteArray source))
              afterPromotion <- getAllocationCounter
              check ("full-hash promotion retains bounded allocation at " ++ show width ++ " bytes, wide hint " ++ show wideHint ++ ": " ++ show (beforePromotion - afterPromotion)) $
                beforePromotion - afterPromotion < 32768 * 6 + 32768
                  && Write.columnDataLength (Write.dictUniques dictionary) == (if wideHint then 9 else 8)
                  && P.length (Write.dictIndices dictionary) == 32768
          )
          (if width <= 128 then [False, True] else [False])
    )
    [64, 128, 256]
  refinedSource <- V.generateM 32768 $ \i ->
    let value = BS.replicate 24 97 <> BS.singleton (fromIntegral (i `rem` 8)) <> BS.replicate 231 97
    in BS.useAsCStringLen value BS.packCStringLen
  _ <- evaluate (V.foldl' (\total value -> total + BS.length value) 0 refinedSource)
  beforeRefinement <- getAllocationCounter
  refinedDictionary <- evaluate (Write.buildDictionary (Write.ColByteArray refinedSource))
  afterRefinement <- getAllocationCounter
  check ("refined-key scan retains bounded allocation: " ++ show (beforeRefinement - afterRefinement) ++ " bytes") $
    beforeRefinement - afterRefinement < 32768 * 6 + 32768
      && Write.columnDataLength (Write.dictUniques refinedDictionary) == 8
      && P.length (Write.dictIndices refinedDictionary) == 32768
  mapM_
    ( \prefix -> do
        let first = BS.replicate 64 0x42
            second = BS.replicate 48 0x42 <> BS.singleton 0x43 <> BS.replicate 15 0x42
            payloads = map (\size -> BS.pack (take size (cycle [1 .. 255]))) ([9 .. 47] ++ [0 .. 8])
            originals = [first, second] ++ payloads ++ [first, second] ++ reverse payloads
            sliced i bytes = BS.take (BS.length bytes) (BS.drop prefix (BS.replicate prefix (fromIntegral i) <> bytes <> BS.replicate 8 (fromIntegral (i * 17))))
            values = zipWith sliced [0 :: Int ..] originals
        check "refined pivots clamp to shorter logical slices without reading neighboring bytes" $
          Write.buildDictionary (Write.ColByteArray (V.fromList values)) == dictionaryModel (Write.ColByteArray . V.fromList) values
    )
    [0 .. 7]
  mapM_
    ( \prefix -> do
        let word key = BS.pack (map (\shift -> fromIntegral ((fromIntegral key :: Word64) `shiftR` shift)) [0, 8 .. 56])
            small = map (\key -> word key <> BS.replicate 56 97) [0 :: Int .. 31]
            wide = BS.replicate 4096 97
            refined = BS.take 2000 wide <> BS.singleton 98 <> BS.drop 2001 wide
            full = BS.take 2048 wide <> BS.singleton 99 <> BS.drop 2049 wide
            originals = small ++ [wide, refined] ++ take 1 small ++ [full] ++ reverse small ++ [wide, refined, full]
            sliced bytes = BS.take (BS.length bytes) (BS.drop prefix (BS.replicate prefix 0xa5 <> bytes <> BS.replicate 8 0x5a))
            values = map sliced originals
        check "medium-width hints allow later wide values through growth and both promotions" $
          Write.buildDictionary (Write.ColByteArray (V.fromList values)) == dictionaryModel (Write.ColByteArray . V.fromList) values
    )
    [0 .. 7]
  promoted <- H.check $ H.withTests 300 $ H.property $ do
    keys <- H.forAll (Gen.list (Range.linear 0 300) (Gen.word64 (Range.linear 0 300)))
    let word key = BS.pack (map (\shift -> fromIntegral (key `shiftR` shift)) [0, 8 .. 56])
        value key = let (high, low) = key `quotRem` 4 in BS.replicate 8 97 <> word low <> word high <> BS.replicate 8 97
        values = map value keys
    Write.buildDictionary (Write.ColByteArray (V.fromList values)) H.=== dictionaryModel (Write.ColByteArray . V.fromList) values
  check "sampled-key collisions promote before duplicate middle values can be merged" promoted
  int32 <- H.check $ H.property $ do
    values <- H.forAll (Gen.list (Range.linear 0 400) (Gen.choice [Gen.int32 Range.constantBounded, Gen.int32 (Range.constant (-64) 64)]))
    Write.buildDictionary (Write.ColInt32 (P.fromList values)) H.=== dictionaryModel (Write.ColInt32 . P.fromList) values
  check "INT32 dictionaries match the independent model across collisions, duplicates and growth" int32
  int64 <- H.check $ H.property $ do
    values <- H.forAll (Gen.list (Range.linear 0 400) (Gen.choice [Gen.int64 Range.constantBounded, Gen.int64 (Range.constant (-64) 64)]))
    Write.buildDictionary (Write.ColInt64 (P.fromList values)) H.=== dictionaryModel (Write.ColInt64 . P.fromList) values
  check "INT64 dictionaries match the independent model across collisions, duplicates and growth" int64
  bytes <- H.check $ H.property $ do
    values <- H.forAll (Gen.list (Range.linear 0 300) (Gen.bytes (Range.linear 0 512)))
    Write.buildDictionary (Write.ColByteArray (V.fromList values)) H.=== dictionaryModel (Write.ColByteArray . V.fromList) values
  check "byte dictionaries match the independent model, including empty and duplicate payloads" bytes
  booleans <- H.check $ H.property $ do
    values <- H.forAll (Gen.list (Range.linear 0 400) Gen.bool)
    Write.buildDictionary (Write.ColBool (V.fromList values)) H.=== dictionaryModel (Write.ColBool . V.fromList) values
  check "boolean dictionaries match the independent model" booleans


root :: SchemaElement
root = SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing Nothing Nothing


field :: T.ArrowType -> Bool -> T.Field
field typ nullable = T.Field "x" nullable typ V.empty Nothing V.empty


intPage :: [Int32] -> TestPage
intPage values =
  let column = Write.ColInt32 (P.fromList values)
      (header, body) = Write.encodeColumnDataPageParts column
  in TestPage (header <> body) (length values) (Write.columnDataStatistics column)


levelPage :: Bool -> Int -> [Int32] -> [Int32] -> Int -> TestPage
levelPage v2 maximumDef definitions values = levelPageData v2 maximumDef definitions (Write.ColInt32 (P.fromList values))


levelPageData :: Bool -> Int -> [Int32] -> Write.ColumnData -> Int -> TestPage
levelPageData v2 maximumDef definitions column nullAdjustment =
  let defs = P.fromList definitions
      (_, payload) = Write.encodeColumnDataPageParts column
      stream =
        (if v2 then Levels.encodeRLEHybrid else Levels.encodeLengthPrefixedHybrid)
          (Levels.bitWidthFor maximumDef)
          defs
      body = stream <> payload
      n = length definitions
      nulls = n - Write.columnDataLength column
      pageHeader =
        if v2
          then
            Page.PtDataPageV2
              ( Page.DataPageHeaderV2
                  (fromIntegral n)
                  (fromIntegral (nulls + nullAdjustment))
                  (fromIntegral n)
                  0
                  (fromIntegral (BS.length stream))
                  0
                  False
              )
          else Page.PtDataPage (Page.DataPageHeader (fromIntegral n) 0)
      header = Page.PageHeader pageHeader (Just (fromIntegral (BS.length body))) (Just (fromIntegral (BS.length body)))
  in TestPage
       (Write.encodePageHeader header <> body)
       n
       ((Write.columnDataStatistics column) {statNullCount = Just (fromIntegral nulls)})


indexed :: V.Vector SchemaElement -> V.Vector Text -> V.Vector TestPage -> Maybe ByteString -> Fixture
indexed schema path pages dictionary =
  let dictionaryBytes = maybe BS.empty id dictionary
      start = 4 + BS.length dictionaryBytes
      offsets = V.scanl' (+) start (BS.length . testBytes <$> pages)
      rows = V.scanl' (+) 0 (testRows <$> pages)
      locations =
        V.imap
          ( \i page ->
              PageLocation
                (fromIntegral (offsets V.! i))
                (fromIntegral (BS.length (testBytes page)))
                (fromIntegral (rows V.! i))
          )
          pages
      offsetBytes = Index.encodeOffsetIndex (OffsetIndex locations Nothing)
      columnIndex =
        ColumnIndex
          { ciNullPages = (\p -> statNullCount (testStats p) == Just (fromIntegral (testRows p))) <$> pages
          , ciMinValues = maybe BS.empty id . statMinValue . testStats <$> pages
          , ciMaxValues = maybe BS.empty id . statMaxValue . testStats <$> pages
          , ciBoundaryOrder = OrderUnordered
          , ciNullCounts = traverse (statNullCount . testStats) pages
          , ciNanCounts = Nothing
          , ciRepetitionLevelHistograms = Nothing
          , ciDefinitionLevelHistograms = Nothing
          }
      columnBytes = Index.encodeColumnIndex columnIndex
      offsetsStart = V.last offsets
      columnsStart = offsetsStart + BS.length offsetBytes
      footerStart = columnsStart + BS.length columnBytes
      columnMetadata =
        ColumnMetadata
          { cmType = maybe (error "missing leaf") id (seType (V.last schema))
          , cmEncodings = V.fromList [Plain, RLEDictionary]
          , cmPathInSchema = path
          , cmCodec = Uncompressed
          , cmNumValues = fromIntegral (V.last rows)
          , cmTotalUncompressedSize = fromIntegral (offsetsStart - 4)
          , cmTotalCompressedSize = fromIntegral (offsetsStart - 4)
          , cmDataPageOffset = fromIntegral start
          , cmDictionaryPageOffset = (const 4) <$> dictionary
          , cmStatistics = Nothing
          , cmBloomFilterOffset = Nothing
          , cmBloomFilterLength = Nothing
          }
      chunk =
        ColumnChunk
          Nothing
          0
          (Just columnMetadata)
          (Just (fromIntegral offsetsStart))
          (Just (fromIntegral (BS.length offsetBytes)))
          (Just (fromIntegral columnsStart))
          (Just (fromIntegral (BS.length columnBytes)))
      group = RowGroup (V.singleton chunk) (fromIntegral (offsetsStart - 4)) (fromIntegral (V.last rows)) Nothing
      metadata =
        FileMetadata
          2
          schema
          (fromIntegral (V.last rows))
          (V.singleton group)
          (Just "selection-regression")
          (Just (V.singleton TypeDefinedOrder))
          Nothing
      bytes =
        "PAR1"
          <> dictionaryBytes
          <> BS.concat (V.toList (testBytes <$> pages))
          <> offsetBytes
          <> columnBytes
          <> Footer.writeFooter metadata
  in Fixture (Read.ParquetFile bytes metadata) locations footerStart


mapChunk :: (ColumnChunk -> ColumnChunk) -> Read.ParquetFile -> Read.ParquetFile
mapChunk change file =
  file
    { Read.pfFooter =
        (Read.pfFooter file)
          { fmRowGroups = V.map (\group -> group {rgColumns = change <$> rgColumns group}) (fmRowGroups (Read.pfFooter file))
          }
    }


replace :: Int -> ByteString -> ByteString -> ByteString
replace offset replacement bytes = BS.take offset bytes <> replacement <> BS.drop (offset + BS.length replacement) bytes


check :: String -> Bool -> IO ()
check label passed = do
  unless passed (error ("FAIL: " ++ label))
  putStrLn ("OK: " ++ label)


nestedProjectionRegression :: IO ()
nestedProjectionRegression = do
  let int = Nested.NSPrimitive Nested.LtInt32
      float = Nested.NSPrimitive Nested.LtFloat
      scalar = Nested.NRLeaf . Nested.LvInt32
      optional = Nested.NSOptional
      required = Nested.NSRequired
      fld name nullable typ kids = T.Field name nullable typ kids Nothing V.empty
      primitive name nullable typ = fld name nullable typ V.empty
      listField typ child = fld "v" True typ (V.singleton child)
      i64 name nullable = primitive name nullable (T.AInt 64 True)
      dbl name nullable = primitive name nullable (T.AFloatingPoint T.DoublePrecision)
      mask = A.validityFromBools (V.fromList [True, False, True, True])
      offsets32 = S.fromList [0, 3, 3, 3, 4]
      offsets64 = S.fromList [0, 3, 3, 3, 4]
      listRows = V.fromList [Nested.NRList (V.fromList [scalar 1, Nested.NRNull, scalar 2]), Nested.NRNull, Nested.NRList V.empty, Nested.NRList (V.singleton (scalar 3))]
      childValues = A.fromMaybes A.PInt64 (V.fromList [Just 1, Nothing, Just 2, Just 3])
      fileFor schema rows = do
        bytes <- either fail pure (Nested.buildNestedFile (V.singleton ("v", schema)) (V.singleton rows))
        either fail pure (Read.loadParquetFile bytes)
      project requested file = Arrow.parquetRowGroupToArrow (T.Schema (V.singleton requested) T.Little V.empty V.empty) file 0
      matches label requested file expected =
        check label (project requested file == Right (V.singleton expected))
  listFile <- fileFor (optional (Nested.NSList (optional int))) listRows
  expectedList <- either fail pure (A.mkList mask offsets32 childValues)
  expectedLarge <- either fail pure (A.mkLargeList mask offsets64 childValues)
  matches
    "nested list projection accepts custom element name and integer widening"
    (listField T.AList (i64 "my_item" True))
    listFile
    expectedList
  matches
    "nested list projection builds requested 64-bit offsets"
    (listField T.ALargeList (i64 "my_item" True))
    listFile
    expectedLarge
  check "nested required element projection rejects visible null elements" $
    isLeft (project (listField T.AList (i64 "my_item" False)) listFile)
  check "nested projection rejects incompatible leaf reinterpretation" $
    isLeft (project (listField T.AList (dbl "my_item" True)) listFile)
  let nativeStruct = Nested.NSStruct (V.fromList [("unused", required int), ("small", optional int), ("float", optional float)])
      record a b c = Nested.NRStruct (V.fromList [scalar a, b, c])
      structRows = V.fromList [record 9 (scalar 1) (Nested.NRLeaf (Nested.LvFloat 1.5)), Nested.NRNull, record 7 (scalar 2) Nested.NRNull, record 5 (scalar 3) (Nested.NRLeaf (Nested.LvFloat 4.5))]
      requestedStruct name nullable = fld name nullable T.AStruct (V.fromList [dbl "float" True, i64 "small" False])
      floats = A.fromMaybes A.PDouble (V.fromList [Just 1.5, Nothing, Nothing, Just 4.5])
      ints = A.primColumn A.PInt64 (S.fromList [1, 1, 2, 3])
  structFile <- fileFor (optional nativeStruct) structRows
  expectedStruct <- either fail pure (A.mkStruct 4 mask (V.fromList [("float", floats), ("small", ints)]))
  matches
    "nested struct projection reorders, drops and widens fields; hidden parent nulls do not reject required children"
    (requestedStruct "v" True)
    structFile
    expectedStruct
  let footer = Read.pfFooter structFile
      groups = V.map (\rg -> rg {rgColumns = V.imap (\i col -> if i == 0 then col {ccMetadata = Nothing} else col) (rgColumns rg)}) (fmRowGroups footer)
      poisoned = structFile {Read.pfFooter = footer {fmRowGroups = groups}}
  check "unprojected malformed column actually fails a full read" $
    isLeft (Arrow.parquetFileArrowSchemaChecked poisoned >>= \schema -> either (Left . show) Right (Arrow.parquetRowGroupToArrow schema poisoned 0))
  matches
    "nested struct projection does not decode omitted physical leaves"
    (requestedStruct "v" True)
    poisoned
    expectedStruct
  check "nested required struct projection rejects visible parent nulls" $
    isLeft (project (requestedStruct "v" False) structFile)
  badStruct <- fileFor (optional nativeStruct) (structRows V.// [(2, record 7 Nested.NRNull Nested.NRNull)])
  check "nested required child projection rejects a visible null under a present parent" $
    isLeft (project (requestedStruct "v" True) badStruct)
  check "nested struct projection reports absent children" $
    isLeft (project (fld "v" True T.AStruct (V.singleton (i64 "missing" True))) structFile)
  emptyStruct <- either fail pure (A.mkStruct 4 mask V.empty)
  matches
    "empty struct projection retains parent rows and validity"
    (fld "v" True T.AStruct V.empty)
    structFile
    emptyStruct
  listStructFile <-
    fileFor (optional (Nested.NSList (optional nativeStruct))) $
      V.fromList [Nested.NRList (V.take 3 structRows), Nested.NRNull, Nested.NRList V.empty, Nested.NRList (V.singleton (V.last structRows))]
  listStruct <- either fail pure (A.mkLargeList mask offsets64 expectedStruct)
  matches
    "large-list projection recursively reorders and widens struct children"
    (listField T.ALargeList (requestedStruct "custom_element" True))
    listStructFile
    listStruct
  let mapRows = V.fromList [Nested.NRMapEntries (V.fromList [(scalar 1, Nested.NRLeaf (Nested.LvFloat 1.5)), (scalar 2, Nested.NRNull)]), Nested.NRNull, Nested.NRMapEntries V.empty, Nested.NRMapEntries (V.singleton (scalar 3, Nested.NRLeaf (Nested.LvFloat 3.5)))]
      mapTarget = fld "v" True (T.AMap False) (V.singleton (fld "pairs" False T.AStruct (V.fromList [i64 "k" False, dbl "x" True])))
  mapFile <- fileFor (optional (Nested.NSMap (required int) (optional float))) mapRows
  expectedMap <- either fail pure (A.mkMap mask (S.fromList [0, 2, 2, 2, 3]) (A.primColumn A.PInt64 (S.fromList [1, 2, 3])) (A.fromMaybes A.PDouble (V.fromList [Just 1.5, Nothing, Just 3.5])))
  matches
    "nested map projection accepts custom wrapper names and widens keys and values"
    mapTarget
    mapFile
    expectedMap


-- Exercise real dictionary outputs rather than mirroring the private hash.
-- A boxed high/low product on the row loop would exceed this allocation cap.
dictionaryBucketMixRegression :: IO ()
dictionaryBucketMixRegression = do
  let count = 32768
      uniqueCount = 257
      word key = (fromIntegral key `shiftL` 48) `xor` 0x80000000000000ff :: Word64
      numeric = Write.ColInt64 (P.generate count (\i -> fromIntegral (word (i `rem` uniqueCount))))
      bytes key = Lazy.toStrict (Builder.toLazyByteString (Builder.word64LE (word key)))
      pool = V.generate uniqueCount bytes
      binary = Write.ColByteArray (V.generate count (\i -> V.unsafeIndex pool (i `rem` uniqueCount)))
      checkAllocation label source expected = do
        _ <- evaluate (Write.columnDataLength source)
        before <- getAllocationCounter
        actual <- evaluate (Write.buildDictionary source)
        after <- getAllocationCounter
        check (label ++ " dictionary mix has no per-row allocation: " ++ show (before - after)) $
          before - after < fromIntegral (count * 6 + uniqueCount * 128 + 32768)
        check (label ++ " dictionary retains upper key bits and stable first-appearance order") (actual == expected)
      expectedIndices = P.generate count (\i -> fromIntegral (i `rem` uniqueCount))
  checkAllocation "upper-bit numeric" numeric $
    Write.Dictionary (Write.ColInt64 (P.generate uniqueCount (fromIntegral . word))) expectedIndices
  _ <- evaluate (V.foldl' (\total value -> total + BS.length value) 0 pool)
  checkAllocation "upper-bit binary" binary $
    Write.Dictionary (Write.ColByteArray pool) expectedIndices
  mapM_
    ( \position -> do
        let values = [0, bitWord position, maxBound, bitWord position, 0]
            source = Write.ColInt64 (P.fromList (map fromIntegral values))
        check "every high/low key bit survives dictionary construction" $
          Write.buildDictionary source == dictionaryModel (Write.ColInt64 . P.fromList) (map fromIntegral values)
    )
    [0 .. 63]
  where
    bitWord position = 1 `shiftL` position :: Word64


-- A scalar dictionary owns its native unique values directly. The bound
-- catches the former source-row cache and final gather, while allowing
-- geometric growth and one Int32 row index per input value.
dictionaryScalarStorageRegression :: IO ()
dictionaryScalarStorageRegression = do
  let count = 8192
      words32 = P.generate count (\i -> fromIntegral i * 1664525 + 1013904223) :: P.Vector Word32
      words64 = P.generate count (\i -> fromIntegral i * 6364136223846793005 + 1442695040888963407) :: P.Vector Word64
      int32 = Write.ColInt32 . P.map fromIntegral
      int64 = Write.ColInt64 . P.map fromIntegral
      float = Write.ColFloat . P.map castWord32ToFloat
      double = Write.ColDouble . P.map castWord64ToDouble
      int32Bits = \case
        Write.ColInt32 values -> Just (P.map fromIntegral values :: P.Vector Word32)
        _ -> Nothing
      int64Bits = \case
        Write.ColInt64 values -> Just (P.map fromIntegral values :: P.Vector Word64)
        _ -> Nothing
      floatBits = \case
        Write.ColFloat values -> Just (P.map castFloatToWord32 values)
        _ -> Nothing
      doubleBits = \case
        Write.ColDouble values -> Just (P.map castDoubleToWord64 values)
        _ -> Nothing
      storage label width makeColumn recover wireBits = do
        let source = makeColumn wireBits
        _ <- evaluate (Write.columnDataLength source)
        before <- getAllocationCounter
        dictionary <- evaluate (Write.buildDictionary source)
        after <- getAllocationCounter
        check (label ++ " scalar dictionary avoids source-row and gather buffers: " ++ show (before - after)) $
          before - after < fromIntegral (count * (24 + width * 2) + 16384)
        check (label ++ " scalar dictionary retains every wire bit and stable ids") $
          recover (Write.dictUniques dictionary) == Just wireBits
            && Write.dictIndices dictionary == P.generate count fromIntegral
      boundary label makeColumn recover prefix salt distinct = do
        let originals = take distinct (prefix ++ map salt [0 .. distinct])
            wireBits = originals ++ reverse originals ++ take 7 originals
            expected = List.nub wireBits
            expectedIds = P.fromList (map (\value -> maybe (error "scalar oracle lost a value") fromIntegral (List.elemIndex value expected)) wireBits)
            values = P.fromList wireBits
        mapM_
          ( \offset -> do
              let sliced = P.slice offset (P.length values) (P.replicate offset 0x55 P.++ values P.++ P.singleton 0xaa)
                  dictionary = Write.buildDictionary (makeColumn sliced)
              check (label ++ " scalar dictionary preserves slices, growth and IEEE identity") $
                recover (Write.dictUniques dictionary) == Just (P.fromList expected)
                  && Write.dictIndices dictionary == expectedIds
          )
          [0, 1, 3, 7]
  storage "INT32" 4 int32 int32Bits words32
  storage "INT64" 8 int64 int64Bits words64
  storage "FLOAT" 4 float floatBits words32
  storage "DOUBLE" 8 double doubleBits words64
  mapM_
    ( \distinct -> do
        let prefix32 = [0, maxBound, 0x80000000, 0x7f800000, 0xff800000, 0x7fc00001, 0x7fa00001, 0xffc00002]
            prefix64 = [0, maxBound, 0x8000000000000000, 0x7ff0000000000000, 0xfff0000000000000, 0x7ff8000000000001, 0x7ff0000000000001, 0xfff8000000000002]
            spread32 i = fromIntegral i * 1664525 + 1013904223 :: Word32
            spread64 i = fromIntegral i * 6364136223846793005 + 1442695040888963407 :: Word64
        boundary "INT32" int32 int32Bits prefix32 spread32 distinct
        boundary "INT64" int64 int64Bits prefix64 spread64 distinct
        boundary "FLOAT" float floatBits prefix32 spread32 distinct
        boundary "DOUBLE" double doubleBits prefix64 spread64 distinct
    )
    [0, 1, 31, 32, 33, 63, 64, 65, 127, 128, 129, 257]


-- Use independent list bounds, including prefixes whose padded word keys
-- are equal. The allocation cap catches a boxed accumulator per input row.
statisticsTraversalRegression :: IO ()
statisticsTraversalRegression = do
  let expected encode legacy values =
        let low = if null values then Nothing else Just (encode (minimum values))
            high = if null values then Nothing else Just (encode (maximum values))
            exact = if null values then Nothing else Just True
        in Statistics
             { statMin = if legacy then low else Nothing
             , statMax = if legacy then high else Nothing
             , statNullCount = Just 0
             , statDistinctCount = Nothing
             , statMinValue = low
             , statMaxValue = high
             , statMinValueExact = exact
             , statMaxValueExact = exact
             , statNanCount = Nothing
             }
      enc32 value = Lazy.toStrict (Builder.toLazyByteString (Builder.int32LE value))
      enc64 value = Lazy.toStrict (Builder.toLazyByteString (Builder.int64LE value))
      check32 values =
        mapM_
          ( \offset -> do
              let source = P.slice offset (length values) (P.replicate offset 17 P.++ P.fromList values P.++ P.singleton 23)
              check "INT32 one-pass statistics preserve signed bounds, empty vectors and slices" $
                Write.columnDataStatistics (Write.ColInt32 source) == expected enc32 True values
          )
          [0, 1, 3, 7]
      check64 values =
        mapM_
          ( \offset -> do
              let source = P.slice offset (length values) (P.replicate offset 17 P.++ P.fromList values P.++ P.singleton 23)
              check "INT64 one-pass statistics preserve signed bounds, empty vectors and slices" $
                Write.columnDataStatistics (Write.ColInt64 source) == expected enc64 True values
          )
          [0, 1, 3, 7]
      checkBytes values =
        mapM_
          ( \prefix -> do
              let sliced ordinal value =
                    let marker = fromIntegral ordinal
                    in BS.take (BS.length value) (BS.drop prefix (BS.replicate prefix marker <> value <> BS.singleton marker))
                  source = V.fromList (zipWith sliced [0 :: Int ..] values)
                  actual = Write.columnDataStatistics (Write.ColByteArray source)
                  reference = expected id False values
              unless (actual == reference) $
                error ("byte statistics mismatch at slice offset " ++ show prefix ++ ": " ++ show values ++ "; actual " ++ show actual ++ "; expected " ++ show reference)
              check "byte statistics preserve unsigned order, NUL padding and unaligned logical slices" (actual == reference)
          )
          [0 .. 7]
      short = concatMap (\size -> [BS.replicate size 0, BS.replicate size 255, BS.pack (take size [0 .. 7])]) [0 .. 8]
      prefixZeros = map (`BS.replicate` 0) [0 .. 128]
  mapM_ check32 [[], [minBound], [maxBound, 0, minBound, -1, 1, maxBound], reverse [minBound, -1, 0, 1, maxBound]]
  mapM_ check64 [[], [minBound], [maxBound, 0, minBound, -1, 1, maxBound], reverse [minBound, -1, 0, 1, maxBound]]
  mapM_ checkBytes [[], [BS.empty], short, reverse short, prefixZeros, reverse prefixZeros]
  mapM_
    ( \prefixCount -> do
        let initial = take prefixCount (cycle ["name_0", "name_7", "name_19"])
        checkBytes (initial ++ [BS.replicate 9 0] ++ ["m", "z"])
        checkBytes (initial ++ [BS.replicate 9 255] ++ ["a", "m"])
    )
    [1, 7, 31, 32, 257]
  passed32 <- H.check $ H.withTests 1000 $ H.property $ do
    values <- H.forAll (Gen.list (Range.linear 0 500) (Gen.int32 Range.constantBounded))
    Write.columnDataStatistics (Write.ColInt32 (P.fromList values)) H.=== expected enc32 True values
  check "INT32 statistics match an independent list oracle" passed32
  passed64 <- H.check $ H.withTests 1000 $ H.property $ do
    values <- H.forAll (Gen.list (Range.linear 0 500) (Gen.int64 Range.constantBounded))
    Write.columnDataStatistics (Write.ColInt64 (P.fromList values)) H.=== expected enc64 True values
  check "INT64 statistics match an independent list oracle" passed64
  passedBytes <- H.check $ H.withTests 1000 $ H.property $ do
    values <- H.forAll (Gen.list (Range.linear 0 150) (Gen.bytes (Range.linear 0 256)))
    Write.columnDataStatistics (Write.ColByteArray (V.fromList values)) H.=== expected id False values
  check "binary statistics match an independent list oracle" passedBytes
  let count = 32768
      measure label source = do
        _ <- evaluate (Write.columnDataLength source)
        case source of
          Write.ColByteArray values -> evaluate (V.foldl' (\n value -> n + BS.length value) 0 values) >> pure ()
          _ -> pure ()
        before <- getAllocationCounter
        stats <- evaluate (Write.columnDataStatistics source)
        let checksum bytes = BS.foldl' (\acc byte -> acc + fromIntegral byte) (0 :: Word64) bytes
        _ <- evaluate (maybe 0 checksum (statMinValue stats) + maybe 0 checksum (statMaxValue stats))
        after <- getAllocationCounter
        check (label ++ " statistics avoid per-row accumulator allocation: " ++ show (before - after)) (before - after < 32768)
  measure "INT32" (Write.ColInt32 (P.generate count (\i -> fromIntegral i * 1664525 + 1013904223)))
  measure "INT64" (Write.ColInt64 (P.generate count (\i -> fromIntegral i * 6364136223846793005 + 1442695040888963407)))
  measure "ascending INT32" (Write.ColInt32 (P.generate count fromIntegral))
  measure "descending INT32" (Write.ColInt32 (P.generate count (fromIntegral . negate)))
  measure "ascending INT64" (Write.ColInt64 (P.generate count fromIntegral))
  measure "descending INT64" (Write.ColInt64 (P.generate count (fromIntegral . negate)))
  measure "ascending FLOAT" (Write.ColFloat (P.generate count fromIntegral))
  measure "descending FLOAT" (Write.ColFloat (P.generate count (fromIntegral . negate)))
  measure "ascending DOUBLE" (Write.ColDouble (P.generate count fromIntegral))
  measure "descending DOUBLE" (Write.ColDouble (P.generate count (fromIntegral . negate)))
  measure "short bytes" (Write.ColByteArray (V.generate count (\i -> V.unsafeIndex (V.fromList short) (i `rem` length short))))
  measure "long common-prefix bytes" (Write.ColByteArray (V.generate count (\i -> BS.replicate 32 97 <> BS.singleton (fromIntegral (i `rem` 8)))))


-- Adjacent middle words need the block mode; another difference outside
-- that block needs full keys. Both modes must preserve owners and ids
-- while growing and retrying rows of later, unrelated lengths.
dictionaryRefinementGrowthRegression :: IO ()
dictionaryRefinementGrowthRegression = do
  let original = BS.replicate 64 97
      firstMiddle = BS.replicate 16 97 <> BS.singleton 98 <> BS.replicate 47 97
      secondMiddle = BS.replicate 24 97 <> BS.singleton 99 <> BS.replicate 39 97
      outsideBlock = BS.replicate 40 97 <> BS.singleton 100 <> BS.replicate 23 97
      -- Flipping bit 0 in word 2 and bit 40 in word 3 cancels after
      -- their rotations by 17 and 41. The payloads are still distinct.
      insideCollision = BS.replicate 16 97 <> BS.singleton 96 <> BS.replicate 12 97 <> BS.singleton 96 <> BS.replicate 34 97
      shortValues = BS.empty : map (\size -> BS.replicate size (fromIntegral size)) [1 .. 8]
      widths = V.fromList [9, 10, 11, 12, 13, 14, 15, 16, 17, 31, 32, 33, 63, 64, 65, 127, 128, 129, 255, 256, 257, 4096]
      payload key =
        let width = V.unsafeIndex widths (key `rem` V.length widths)
        in Lazy.toStrict (Builder.toLazyByteString (Builder.word64LE (fromIntegral key))) <> BS.replicate (width - 8) 0xa5
      lateShort = BS.singleton 255
      lateWide = BS.replicate 4097 0x5a
  mapM_
    ( \full -> do
        let promotion = [original, firstMiddle, secondMiddle] ++ if full then [outsideBlock] else []
            initial = promotion ++ replicate (32 - length promotion) original
            known = length promotion + length shortValues
        mapM_
          ( \uniqueCount -> do
              let payloads = map payload [0 .. uniqueCount - known - 1]
                  values = initial ++ shortValues ++ payloads ++ [lateShort, lateWide, insideCollision, outsideBlock] ++ reverse payloads ++ reverse shortValues ++ promotion ++ [lateWide, lateShort, insideCollision, outsideBlock]
                  reference = dictionaryModel (Write.ColByteArray . V.fromList) values
              mapM_
                ( \prefix -> do
                    let sliced ordinal value =
                          let marker = fromIntegral ordinal
                          in BS.take (BS.length value) (BS.drop prefix (BS.replicate prefix marker <> value <> BS.singleton marker))
                        source = V.fromList (zipWith sliced [0 :: Int ..] values)
                    check ("refined dictionary growth preserves mixed-width owners and retry ids at " ++ show uniqueCount ++ " entries, alignment " ++ show prefix ++ ", full mode " ++ show full) $
                      Write.buildDictionary (Write.ColByteArray source) == reference
                )
                [0 .. 7]
          )
          [31, 32, 33, 63, 64, 65, 127, 128, 129, 255, 256, 257, 1024, 1025]
    )
    [False, True]

  mapM_
    ( \width -> do
        let count = 32768
            word key = Lazy.toStrict (Builder.toLazyByteString (Builder.word64LE (fromIntegral key)))
        source <- V.generateM count $ \i ->
          let (high, low) = (i `rem` 8) `quotRem` 4
              size = if i < 32 then 64 else width
              value = BS.replicate 16 97 <> word low <> word high <> BS.replicate (size - 32) 97
          in BS.useAsCStringLen value BS.packCStringLen
        _ <- evaluate (V.foldl' (\total value -> total + BS.length value) 0 source)
        before <- getAllocationCounter
        dictionary <- evaluate (Write.buildDictionary (Write.ColByteArray source))
        after <- getAllocationCounter
        check ("common-prefix byte dictionary scan retains bounded allocation at " ++ show width ++ " bytes: " ++ show (before - after)) $
          before - after < fromIntegral (count * 6 + 32768)
            && Write.columnDataLength (Write.dictUniques dictionary) == (if width == 64 then 8 else 16)
            && P.length (Write.dictIndices dictionary) == count
    )
    [64, 128, 256]


legacyChunkAccumulationRegression :: IO ()
legacyChunkAccumulationRegression = do
  let plain column =
        let (header, body) = Write.encodeColumnDataPageParts column
        in header <> body
      rawPage encoding count body =
        let size = fromIntegral (BS.length body)
        in Write.encodePageHeader (Page.PageHeader (Page.PtDataPage (Page.DataPageHeader (fromIntegral count) encoding)) (Just size) (Just size)) <> body
      rawBytes column = case column of
        Write.ColByteArray values -> rawPage 0 (V.length values) (Lazy.toStrict (Builder.toLazyByteString (foldMap Builder.byteString values)))
        _ -> error "legacy raw-byte fixture expects byte arrays"
      bytes width first count = Write.ColByteArray $ V.generate count $ \row ->
        BS.take width (Lazy.toStrict (Builder.toLazyByteString (Builder.word64LE (fromIntegral (first + row)) <> Builder.word64LE 0xff00ff00)))
      binaryValue ordinal = BS.take (ordinal `rem` 9) (Lazy.toStrict (Builder.toLazyByteString (Builder.word64LE (fromIntegral ordinal))))
      optionalValues :: Int -> Int -> (Int -> a) -> V.Vector (Maybe a)
      optionalValues first count valueAt = V.generate count $ \row ->
        let ordinal = first + row
        in if ordinal `rem` 3 == 0 then Nothing else Just (valueAt ordinal)
      sliced prefix chunk = BS.take (BS.length chunk) (BS.drop prefix (BS.replicate prefix 0xe3 <> chunk <> BS.singleton 0x77))
  mapM_
    ( \pageCount ->
        mapM_
          ( \pageRows -> do
              let total = pageCount * pageRows
                  firstRows = map (* pageRows) [0 .. pageCount - 1]
                  required name make decode encode = do
                    let pages = map (encode . (\first -> make first pageRows)) firstRows
                        chunk = BS.concat (encode (make 0 0) : pages ++ [encode (make total 0)])
                        expected = Right (make 0 total)
                    mapM_
                      (\prefix -> check (name ++ " legacy pages preserve order/empty pages/slices at " ++ show pageCount ++ "x" ++ show pageRows ++ "/" ++ show prefix) (decode (sliced prefix chunk) == expected))
                      [0, 1, 3, 7]
                  optional name make decode = do
                    let pages = map (Write.encodeOptionalColumnPage . (\first -> make first pageRows)) firstRows
                        chunk = BS.concat (Write.encodeOptionalColumnPage (make 0 0) : pages ++ [Write.encodeOptionalColumnPage (make total 0)])
                        expected = Right (make 0 total)
                    mapM_
                      (\prefix -> check (name ++ " legacy optional pages preserve nulls/order/slices at " ++ show pageCount ++ "x" ++ show pageRows ++ "/" ++ show prefix) (decode (sliced prefix chunk) == expected))
                      [0, 1, 3, 7]
              required "INT32" (\first count -> Write.ColInt32 (P.generate count (\row -> fromIntegral (first + row) - 20000))) (fmap Write.ColInt32 . Read.readPlainInt32ColumnChunk Uncompressed) plain
              required "INT64" (\first count -> Write.ColInt64 (P.generate count (\row -> fromIntegral (first + row) * 1000000007 - 9000000000))) (fmap Write.ColInt64 . Read.readPlainInt64ColumnChunk Uncompressed) plain
              required "FLOAT" (\first count -> Write.ColFloat (P.generate count (\row -> fromIntegral (first + row) * 0.25 - 100))) (fmap Write.ColFloat . Read.readPlainFloatColumnChunk Uncompressed) plain
              required "DOUBLE" (\first count -> Write.ColDouble (P.generate count (\row -> fromIntegral (first + row) * (-0.125) - 100))) (fmap Write.ColDouble . Read.readPlainDoubleColumnChunk Uncompressed) plain
              required "BOOL" (\first count -> Write.ColBool (V.generate count (odd . (first +)))) (fmap Write.ColBool . Read.readPlainBoolColumnChunk Uncompressed) plain
              required "BYTE_ARRAY" (\first count -> Write.ColByteArray (V.generate count (binaryValue . (first +)))) (fmap Write.ColByteArray . Read.readPlainByteArrayColumnChunk Uncompressed) plain
              required "INT96" (bytes 12) (fmap Write.ColByteArray . Read.readPlainInt96ColumnChunk Uncompressed) rawBytes
              required "FIXED_LEN_BYTE_ARRAY" (bytes 3) (fmap Write.ColByteArray . Read.readPlainFixedLenByteArrayColumnChunk 3 Uncompressed) rawBytes
              optional "INT32" (\first count -> Write.OptInt32 (optionalValues first count (\value -> fromIntegral value - 10000))) (fmap Write.OptInt32 . Read.readPlainInt32OptionalColumnChunk Uncompressed 0 1)
              optional "INT64" (\first count -> Write.OptInt64 (optionalValues first count (\value -> fromIntegral value * 1000000007))) (fmap Write.OptInt64 . Read.readPlainInt64OptionalColumnChunk Uncompressed 0 1)
              optional "FLOAT" (\first count -> Write.OptFloat (optionalValues first count (\value -> fromIntegral value * 0.25))) (fmap Write.OptFloat . Read.readPlainFloatOptionalColumnChunk Uncompressed 0 1)
              optional "DOUBLE" (\first count -> Write.OptDouble (optionalValues first count (\value -> fromIntegral value * (-0.125)))) (fmap Write.OptDouble . Read.readPlainDoubleOptionalColumnChunk Uncompressed 0 1)
              optional "BOOL" (\first count -> Write.OptBool (optionalValues first count odd)) (fmap Write.OptBool . Read.readPlainBoolOptionalColumnChunk Uncompressed 0 1)
              optional "BYTE_ARRAY" (\first count -> Write.OptByteArray (optionalValues first count binaryValue)) (fmap Write.OptByteArray . Read.readPlainByteArrayOptionalColumnChunk Uncompressed 0 1)
          )
          [0, 1, 7, 8, 9, 33]
    )
    [0, 1, 2, 7, 33]

  let uniques = Write.ColInt32 (P.fromList [10, 11, 12, 13])
      dict = Write.Dictionary uniques P.empty
      dictionary = Write.encodeDictPage dict
      dictionaryPage first count = Write.encodeDictDataPage (Write.Dictionary uniques (P.generate count (\row -> fromIntegral ((first + row) `rem` 4))))
      emptyData = dictionaryPage 0 0
      nullablePage first count =
        let values = optionalValues first count (\ordinal -> 10 + fromIntegral (ordinal `rem` 4) :: Int32)
            definitions = P.generate count (\row -> case V.unsafeIndex values row of Nothing -> 0; Just _ -> 1)
            indices = P.fromList (foldMap (maybe [] (\value -> [value - 10])) values)
            encoded = Write.encodeDictDataPage (Write.Dictionary uniques indices)
            (_, _, body, _) = either error id (Read.decompressDataPageBody Uncompressed encoded 0)
        in rawPage 8 count (Levels.encodeLengthPrefixedHybrid 1 definitions <> body)
  mapM_
    ( \pageCount -> do
        let pageRows = 9
            count = pageCount * pageRows
            starts = map (* pageRows) [0 .. pageCount - 1]
            chunk = dictionary <> emptyData <> BS.concat (map (`dictionaryPage` pageRows) starts) <> emptyData
            expected = P.generate count (\row -> 10 + fromIntegral (row `rem` 4))
            optionalChunk = dictionary <> nullablePage 0 0 <> BS.concat (map (`nullablePage` pageRows) starts) <> nullablePage count 0
            optionalExpected = optionalValues 0 count (\ordinal -> 10 + fromIntegral (ordinal `rem` 4) :: Int32)
            fallback first = plain (Write.ColInt32 (P.generate pageRows (\row -> fromIntegral (first + row))))
            mixedChunk = dictionary <> BS.concat (concatMap (\first -> [dictionaryPage first pageRows, fallback first]) starts)
            mixedExpected = P.concat (map (\first -> P.generate (pageRows * 2) (\row -> if row < pageRows then 10 + fromIntegral ((first + row) `rem` 4) else fromIntegral (first + row - pageRows))) starts)
        mapM_
          ( \prefix -> do
              let source = sliced prefix chunk
              check "legacy dictionary reader retains its empty-column rejection" $
                if count == 0 then isLeft (Read.readPlainDictionaryInt32ColumnChunk Uncompressed source) else Read.readPlainDictionaryInt32ColumnChunk Uncompressed source == Right expected
              check "legacy dictionary optional pages preserve empty pages, nulls and order" $
                Read.readDictionaryInt32OptionalColumnChunk Uncompressed 0 1 (sliced prefix optionalChunk) == Right optionalExpected
              check "legacy dictionary/plain fallback pages preserve order" $
                if count == 0 then isLeft (Read.readPlainDictionaryInt32ColumnChunk Uncompressed (sliced prefix mixedChunk)) else Read.readPlainDictionaryInt32ColumnChunk Uncompressed (sliced prefix mixedChunk) == Right mixedExpected
          )
          [0, 1, 3, 7]
    )
    [0, 1, 2, 7, 33]

  -- A copied source prevents a cached pure decode from satisfying this guard.
  -- The old growing-vector append allocates 138/277 MB on these inputs.
  let pages = 1024
      pageRows = 64
      rows = pages * pageRows
      starts = map (* pageRows) [0 .. pages - 1]
      ints first = P.generate pageRows (\row -> fromIntegral (first + row) - 20000 :: Int32)
      booleans first = optionalValues first pageRows odd
      intInput = BS.concat (map (plain . Write.ColInt32 . ints) starts)
      boolInput = BS.concat (map (Write.encodeOptionalColumnPage . Write.OptBool . booleans) starts)
  intSource <- BS.useAsCStringLen intInput BS.packCStringLen
  _ <- evaluate (BS.length intSource)
  performGC
  beforeInts <- getAllocationCounter
  actualInts <- evaluate (Read.readPlainInt32ColumnChunk Uncompressed intSource)
  intRows <- evaluate (either error P.length actualInts)
  afterInts <- getAllocationCounter
  check ("legacy INT32 page accumulation has linear allocation: " ++ show (beforeInts - afterInts)) $
    beforeInts - afterInts < fromIntegral (rows * 128 + 65536)
  check "legacy INT32 large-page model" $
    intRows == rows && actualInts == Right (P.generate rows (\row -> fromIntegral row - 20000))
  boolSource <- BS.useAsCStringLen boolInput BS.packCStringLen
  _ <- evaluate (BS.length boolSource)
  performGC
  beforeBools <- getAllocationCounter
  actualBools <- evaluate (Read.readPlainBoolOptionalColumnChunk Uncompressed 0 1 boolSource)
  boolRows <- evaluate (either error V.length actualBools)
  afterBools <- getAllocationCounter
  check ("legacy optional BOOL page accumulation has linear allocation: " ++ show (beforeBools - afterBools)) $
    beforeBools - afterBools < fromIntegral (rows * 256 + 65536)
  check "legacy optional BOOL large-page model" $
    boolRows == rows && actualBools == Right (optionalValues 0 rows odd)


packedBoolRegression :: IO ()
packedBoolRegression = do
  let dictionaryValues = V.fromList [False, True, False, True, False, True, False, True, False]
      dictionary = Write.encodeDictPage (Write.Dictionary (Write.ColBool dictionaryValues) P.empty)
      plainBits values = snd (Write.encodeColumnDataPageParts (Write.ColBool values))
      boolPage v2 encoding nullable values =
        let defs = P.generate (V.length values) (\i -> case V.unsafeIndex values i of Nothing -> 0; Just _ -> 1)
            dense = V.mapMaybe id values
            payload0
              | encoding == 3 = Levels.encodeLengthPrefixedHybrid 1 (P.fromList (foldMap (\b -> [if b then 1 else 0]) dense))
              | encoding == 8 || encoding == 2 =
                  let ix = P.generate (V.length dense) (\i -> if V.unsafeIndex dense i then 1 else if odd i then 0 else 8)
                      encoded = Write.encodeDictDataPage (Write.Dictionary (Write.ColBool dictionaryValues) ix)
                      (_, _, body, _) = either error id (Read.decompressDataPageBody Uncompressed encoded 0)
                  in body
              | otherwise = plainBits dense
            -- Padding is deliberately nonzero, and must never leak into the
            -- next page when its preceding value count is not byte-aligned.
            payload =
              if encoding == 0 && not (BS.null payload0) && V.length dense `rem` 8 /= 0
                then BS.init payload0 <> BS.singleton (BS.last payload0 .|. (255 `shiftL` (V.length dense `rem` 8)))
                else payload0
            levels = if nullable then (if v2 then Levels.encodeRLEHybrid else Levels.encodeLengthPrefixedHybrid) 1 defs else BS.empty
            body = levels <> payload
            count = V.length values
            nulls = count - V.length dense
            header =
              if v2
                then Page.PtDataPageV2 (Page.DataPageHeaderV2 (fromIntegral count) (fromIntegral nulls) (fromIntegral count) encoding (fromIntegral (BS.length levels)) 0 False)
                else Page.PtDataPage (Page.DataPageHeader (fromIntegral count) encoding)
            stats = (Write.columnDataStatistics (Write.ColBool dense)) {statNullCount = Just (fromIntegral nulls)}
        in TestPage (Write.encodePageHeader (Page.PageHeader header (Just (fromIntegral (BS.length body))) (Just (fromIntegral (BS.length body)))) <> body) count stats
      makeFile nullable encoding pages =
        let schema = V.fromList [root, SchemaElement "x" (Just (if nullable then Optional else Required)) (Just PTBoolean) Nothing Nothing Nothing Nothing Nothing]
            fixture = indexed schema (V.singleton "x") pages (if encoding == 8 || encoding == 2 then Just dictionary else Nothing)
            old = fixtureFile fixture
            update cm = cm {cmEncodings = V.fromList [Plain, RLE, RLEDictionary]}
            changed = mapChunk (\cc -> cc {ccMetadata = update <$> ccMetadata cc}) old
            bytes = BS.take (fixtureFooterStart fixture) (Read.pfBytes changed) <> Footer.writeFooter (Read.pfFooter changed)
        in (fixture {fixtureFile = either error id (Read.loadParquetFile bytes)})
      fromBits buffers = do
        bits <- A.mkBitmap (Read.boolData buffers) 0 (Read.boolCount buffers)
        A.mkBool Nothing bits
      sourceValue i = odd (i `quot` 3 + i `quot` 11)
      values nullable start n = V.generate n (\i -> if nullable && (start + i) `rem` 5 == 0 then Nothing else Just (sourceValue (start + i)))
      verify v2 encoding nullable chunks = do
        let pages = V.map (boolPage v2 encoding nullable) chunks
            fixture = makeFile nullable encoding pages
            file = fixtureFile fixture
            expectedValues = V.concat (V.toList chunks)
            expected = A.fromMaybeBools expectedValues
            target = field T.ABool nullable
            dense = A.fromBools (V.mapMaybe id expectedValues)
            chunk = either error id (Read.columnChunkSlice file 0 0)
            context = "BOOLEAN packed encoding " ++ show encoding ++ ", V2 " ++ show v2 ++ ", nullable " ++ show nullable
        check (context ++ " Arrow roundtrip, rows " ++ show (map V.length (V.toList chunks))) (Arrow.readParquetColumn file 0 0 target == Right expected)
        check (context ++ " legacy generic reader") $
          if nullable
            then Read.readGenericBoolOptionalColumnChunk Uncompressed 0 1 chunk == Right expectedValues
            else Read.readGenericBoolColumnChunk Uncompressed chunk == Right (V.mapMaybe id expectedValues)
        if nullable
          then do
            levels <- either fail pure (Read.readBoolBuffersLevels Uncompressed 0 1 chunk)
            check (context ++ " dense packed defined values") (fromBits (Read.clValues levels) == Right dense)
          else check (context ++ " packed required values") ((Read.readBoolBuffersColumnChunk Uncompressed chunk >>= fromBits) == Right dense)
        when (V.all ((> 0) . V.length) chunks) $ do
          selected <- either fail pure (Arrow.readParquetColumnWithPagePruning file 0 0 target (Pred.PIn [Pred.PVBool False, Pred.PVBool True]))
          check (context ++ " indexed Arrow path") (snd selected == A.fromMaybeBools (V.concat (V.toList (V.filter (V.any (/= Nothing)) chunks))))
          let keep = V.generate (V.length chunks) even
              selection = Read.PageSelection (fixtureLocations fixture) keep (if encoding == 8 || encoding == 2 then Just 4 else Nothing) (Just (fromIntegral (V.length expectedValues)))
              retained = V.concat (V.toList (V.ifilter (\i _ -> even i) chunks))
              retainedDense = A.fromBools (V.mapMaybe id retained)
          if nullable
            then do
              result <- either fail pure (Read.readBoolBuffersSelectedLevels Uncompressed 0 1 (Read.pfBytes file) selection)
              check (context ++ " partial selected packed values") (fromBits (Read.clValues result) == Right retainedDense && P.length (Read.clDefinition result) == V.length retained)
            else check (context ++ " partial selected required values") ((Read.readBoolBuffersSelectedValues Uncompressed (Read.pfBytes file) selection >>= fromBits) == Right retainedDense)
  mapM_
    ( \v2 ->
        mapM_
          ( \encoding ->
              mapM_
                ( \nullable ->
                    mapM_
                      (\n -> verify v2 encoding nullable (V.fromList [values nullable 0 n, values nullable n (n + 1), values nullable (2 * n + 1) 2]))
                      [0, 1, 2, 7, 8, 9, 17, 65]
                )
                [False, True]
          )
          [0, 3, 8, 2]
    )
    [False, True]
  passed <- H.check $ H.withTests 300 $ H.property $ do
    v2 <- H.forAll Gen.bool
    encoding <- H.forAll (Gen.element [0, 3, 8, 2])
    chunks <- H.forAll (Gen.list (Range.linear 1 12) (Gen.list (Range.linear 0 90) (Gen.maybe Gen.bool)))
    H.evalIO (verify v2 encoding True (V.fromList (map V.fromList chunks)))
  check "packed BOOLEAN pages match independent nullable models" passed

  let rawPage n encoding body =
        Write.encodePageHeader (Page.PageHeader (Page.PtDataPage (Page.DataPageHeader n encoding)) (Just (fromIntegral (BS.length body))) (Just (fromIntegral (BS.length body)))) <> body
      framed body = Lazy.toStrict (Builder.toLazyByteString (Builder.word32LE (fromIntegral (BS.length body)))) <> body
  mapM_
    (\body -> check "packed BOOLEAN RLE rejects malformed runs before allocating output" (isLeft (Read.readBoolBuffersColumnChunk Uncompressed (rawPage 1 3 body))))
    [BS.empty, framed BS.empty, framed (BS.singleton 0), framed (BS.singleton 128), framed (BS.singleton 2), framed (BS.singleton 3), BS.pack [255, 255, 255, 255], framed (BS.replicate 10 255)]
  mapM_
    ( \dictionaryByte -> do
        let literalHeader = Page.PageHeader (Page.PtDictionaryPage (Page.DictionaryPageHeader 2 0)) (Just 1) (Just 1)
            literalDictionary = Write.encodePageHeader literalHeader <> BS.singleton dictionaryByte
            literalData = rawPage 4 8 (BS.pack [1, 3, 6])
            expected = if dictionaryByte == 2 then V.fromList [False, True, True, False] else V.fromList [True, False, False, True]
        check "BOOLEAN dictionary matches literal LSB-first/index golden" $
          Read.readGenericBoolColumnChunk Uncompressed (literalDictionary <> literalData) == Right expected
    )
    [2, 1]
  let smallDict = Write.encodeDictPage (Write.Dictionary (Write.ColBool (V.fromList [False, True])) P.empty)
      invalid = smallDict <> rawPage 1 8 (BS.pack [2, 2, 3])
  check "packed BOOLEAN dictionary validates every index" (isLeft (Read.readBoolBuffersColumnChunk Uncompressed invalid))
  check "legacy BOOLEAN dictionary validates every index" (isLeft (Read.readGenericBoolColumnChunk Uncompressed invalid))
  check "packed BOOLEAN dictionary requires a preceding dictionary" (isLeft (Read.readBoolBuffersColumnChunk Uncompressed (rawPage 1 8 (BS.pack [0]))))
  check "packed BOOLEAN rejects duplicate dictionaries" (isLeft (Read.readBoolBuffersColumnChunk Uncompressed (smallDict <> smallDict)))
  check "packed BOOLEAN rejects a late dictionary" (isLeft (Read.readBoolBuffersColumnChunk Uncompressed (rawPage 1 0 (BS.singleton 1) <> smallDict)))

  -- Fresh input makes these guards measure a real decode rather than a CAF.
  mapM_
    ( \nullable -> do
        let pageCount = 257
            rows = 1025
            count = pageCount * rows
            chunks = V.generate pageCount (\i -> values nullable (i * rows) rows)
            file = fixtureFile (makeFile nullable 0 (V.map (boolPage True 0 nullable) chunks))
        bytes <- BS.useAsCStringLen (Read.pfBytes file) BS.packCStringLen
        parsed <- either fail pure (Read.loadParquetFile bytes)
        performGC
        before <- getAllocationCounter
        result <- either fail pure =<< evaluate (Arrow.readParquetColumn parsed 0 0 (field T.ABool nullable))
        actualCount <- evaluate (A.columnLength result)
        after <- getAllocationCounter
        check ("BOOLEAN buffer path avoids boxed rows, nullable " ++ show nullable ++ ": " ++ show (before - after)) $
          before - after < fromIntegral (count * (if nullable then 20 else 4) + 524288)
        check "large packed BOOLEAN roundtrip" (actualCount == count && result == A.fromMaybeBools (values nullable 0 count))
    )
    [False, True]


-- Independent byte-lane models preserve every IEEE bit, including NaN
-- payloads and signed zero. Slices cross both the SIMD tail and bounded
-- unsafe / pinned-safe call transitions.
byteStreamSplitRegression :: IO ()
byteStreamSplitRegression = do
  let counts = [0 .. 129] ++ [255, 256, 257, 1023, 1024, 1025, 8191, 8192, 8193, 16383, 16384, 16385, 65535, 65536, 65537]
  mapM_
    (\n -> mapM_ (\offset -> mapM_ (\mis -> bssCheck32 n offset mis >> bssCheck64 n offset mis) [0, 1, 7]) [0, 1, 3, 7])
    counts
  check "3480 BYTE_STREAM_SPLIT slice/IEEE/truncation models" True
  mapM_
    ( \n -> do
        check "BYTE_STREAM_SPLIT FLOAT rejects invalid counts" (isLeft (Read.decodeByteStreamSplitFloat n BS.empty))
        check "BYTE_STREAM_SPLIT DOUBLE rejects invalid counts" (isLeft (Read.decodeByteStreamSplitDouble n BS.empty))
    )
    [-1, minBound, maxBound]
  passed <- H.check $ H.withTests 300 $ H.property $ do
    words32 <- H.forAll (Gen.list (Range.linear 0 257) (Gen.word32 Range.constantBounded))
    words64 <- H.forAll (Gen.list (Range.linear 0 257) (Gen.word64 Range.constantBounded))
    let n32 = length words32
        n64 = length words64
        model32 = bssModel32 words32
        model64 = bssModel64 words64
        source32 = P.map castWord32ToFloat (P.fromList words32)
        source64 = P.map castWord64ToDouble (P.fromList words64)
    ByteStreamSplit.encodeByteStreamSplitFloat source32 H.=== model32
    ByteStreamSplit.encodeByteStreamSplitDouble source64 H.=== model64
    fmap (P.map castFloatToWord32) (Read.decodeByteStreamSplitFloat n32 model32) H.=== Right (P.fromList words32)
    fmap (P.map castDoubleToWord64) (Read.decodeByteStreamSplitDouble n64 model64) H.=== Right (P.fromList words64)
  check "BYTE_STREAM_SPLIT arbitrary IEEE bits match independent models" passed
  done <- newEmptyMVar
  _ <- forkIO (replicateM_ 100 performGC `finally` putMVar done ())
  replicateM_ 20 (bssCheck32 65537 3 1 >> bssCheck64 65537 7 7)
  takeMVar done
  check "BYTE_STREAM_SPLIT pinned owners survive concurrent GC" True


bssCheck32 :: Int -> Int -> Int -> IO ()
bssCheck32 n offset mis = do
  let bitPatterns = map (bssWord32At . (+ offset)) [0 .. n - 1]
      values = P.slice offset n (P.generate (n + offset + 1) (castWord32ToFloat . bssWord32At))
      golden = bssModel32 bitPatterns
      input = BS.drop mis (BS.replicate mis 0xa5 <> golden)
  unless (ByteStreamSplit.encodeByteStreamSplitFloat values == golden) (fail "BYTE_STREAM_SPLIT sliced FLOAT bytes differ")
  unless (fmap (P.map castFloatToWord32) (Read.decodeByteStreamSplitFloat n input) == Right (P.fromList bitPatterns)) (fail "BYTE_STREAM_SPLIT FLOAT IEEE bits differ")
  when (n > 0) $
    unless (isLeft (Read.decodeByteStreamSplitFloat n (BS.take (BS.length input - 1) input))) (fail "BYTE_STREAM_SPLIT FLOAT accepted short input")


bssCheck64 :: Int -> Int -> Int -> IO ()
bssCheck64 n offset mis = do
  let bitPatterns = map (bssWord64At . (+ offset)) [0 .. n - 1]
      values = P.slice offset n (P.generate (n + offset + 1) (castWord64ToDouble . bssWord64At))
      golden = bssModel64 bitPatterns
      input = BS.drop mis (BS.replicate mis 0xa5 <> golden)
  unless (ByteStreamSplit.encodeByteStreamSplitDouble values == golden) (fail "BYTE_STREAM_SPLIT sliced DOUBLE bytes differ")
  unless (fmap (P.map castDoubleToWord64) (Read.decodeByteStreamSplitDouble n input) == Right (P.fromList bitPatterns)) (fail "BYTE_STREAM_SPLIT DOUBLE IEEE bits differ")
  when (n > 0) $
    unless (isLeft (Read.decodeByteStreamSplitDouble n (BS.take (BS.length input - 1) input))) (fail "BYTE_STREAM_SPLIT DOUBLE accepted short input")


bssModel32 :: [Word32] -> ByteString
bssModel32 bitPatterns = BS.pack (concatMap (\lane -> map (\word -> fromIntegral (word `shiftR` (lane * 8))) bitPatterns) [0 .. 3])


bssModel64 :: [Word64] -> ByteString
bssModel64 bitPatterns = BS.pack (concatMap (\lane -> map (\word -> fromIntegral (word `shiftR` (lane * 8))) bitPatterns) [0 .. 7])


bssWord32At :: Int -> Word32
bssWord32At i = case i of
  0 -> 0
  1 -> 0x80000000
  2 -> 0x7f800000
  3 -> 0xff800000
  4 -> 0x7fc12345
  5 -> 0x7f812345
  6 -> 1
  7 -> 0x7f7fffff
  _ -> fromIntegral i * 1664525 + 1013904223


bssWord64At :: Int -> Word64
bssWord64At i = case i of
  0 -> 0
  1 -> 0x8000000000000000
  2 -> 0x7ff0000000000000
  3 -> 0xfff0000000000000
  4 -> 0x7ff8123456789abc
  5 -> 0x7ff0123456789abc
  6 -> 1
  7 -> 0x7fefffffffffffff
  _ -> fromIntegral i * 6364136223846793005 + 1442695040888963407
