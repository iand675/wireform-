{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Read external Parquet files and optionally export the decoded values as
Arrow IPC so the external reader can compare them with its original input.
-}
module Main (main) where

import Arrow.Column qualified as AC
import Arrow.File qualified as ArrowFile
import Arrow.Stream qualified as AS
import Arrow.Types qualified as AT
import Control.DeepSeq (force)
import Control.Exception (SomeException, catch, evaluate)
import Control.Monad (forM_, when)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (FromJSON (..), object, withObject, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int32, Int64)
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.Read qualified as TextRead
import Data.Vector qualified as V
import Data.Vector.Primitive qualified as VP
import Data.Vector.Storable qualified as S
import Data.Word (Word64)
import Numeric (showHex)
import Parquet.Aggregate qualified as Aggregate
import Parquet.Arrow qualified as PArrow
import Parquet.BloomFilter qualified as Bloom
import Parquet.Decryption qualified as Decryption
import Parquet.Encryption qualified as Encryption
import Parquet.Footer qualified as Footer
import Parquet.HighLevel qualified as PHL
import Parquet.Levels qualified as Levels
import Parquet.PageIndex qualified as PageIndex
import Parquet.Predicate qualified as Predicate
import Parquet.Read qualified as Read
import Parquet.Types qualified as Parquet
import Parquet.Variant qualified as Variant
import Parquet.Variant.Arrow qualified as VariantArrow
import Parquet.Variant.Parquet qualified as VariantParquet
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.FilePath (takeExtension, (</>))


data ProbeResult = ProbeResult
  { probeName :: !FilePath
  , probeError :: !(Maybe String)
  }


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--encrypted-read", path, keys, output] -> encryptedRead path keys output
    ["--authenticate-footer", path, key, prefix] -> authenticateFooter path (Text.pack key) (Text.pack prefix)
    ["--large-map-summary", path] -> largeMapSummary path
    ["--variant-arrow-export", path, out] -> variantArrowExport path out
    ["--variant-payloads", path] -> variantPayloads path
    ["--variant-export", path, name, out] -> variantExport path (Text.pack name) out
    ["--prune", path, literal, out] -> prune path literal out
    ["--aggregate", path] -> aggregate path
    ["--project", path, schemaPath, output] -> project path schemaPath output
    ["--help"] -> putStrLn "usage: wireform-parquet-reverse-probe <dir> [--arrow-output <dir>] | --authenticate-footer <file> <public-fixture-key-hex> <prefix-hex-or-dash> | --aggregate <file> | --project <file> <schema-stream> <arrow-output> | --prune <file> <lower-bound> <arrow-output>"
    [dir] -> run dir Nothing
    [dir, "--arrow-output", out] -> do
      createDirectoryIfMissing True out
      run dir (Just out)
    _ -> do
      putStrLn "usage: wireform-parquet-reverse-probe <dir> [--arrow-output <dir>] | --authenticate-footer <file> <public-fixture-key-hex> <prefix-hex-or-dash> | --aggregate <file> | --project <file> <schema-stream> <arrow-output> | --prune <file> <lower-bound> <arrow-output>"
      exitFailure


-- Fixture-only authentication mode. The conformance driver supplies public
-- test keys, resolves external key material through the C++ reference, and
-- compares footer counts independently of column-page decoding.
authenticateFooter :: FilePath -> Text.Text -> Text.Text -> IO ()
authenticateFooter path keyHex prefixHex = do
  key <- either fail pure (hexBytes keyHex)
  prefix <- if prefixHex == "-" then pure Nothing else Just <$> either fail pure (hexBytes prefixHex)
  bytes <- BS.readFile path
  authenticated <- either fail pure (Encryption.authenticateFooter key prefix Nothing bytes)
  metadata <- either fail pure (Footer.thriftToFileMetadata (Encryption.afValue authenticated))
  case Encryption.afCryptoMetadata authenticated of
    Nothing -> fail "probe: expected an encrypted fixture"
    Just info -> do
      let actualPrefix = maybe (maybe BS.empty id (Encryption.fcmStoredAadPrefix info)) id prefix
      loaded <- either fail pure (Read.loadParquetFileEncrypted (Read.FooterDecryption key (Encryption.fcmFileId info) actualPrefix) bytes)
      when (Parquet.fmSchema (Read.pfFooter loaded) /= Parquet.fmSchema metadata || Parquet.fmNumRows (Read.pfFooter loaded) /= Parquet.fmNumRows metadata) (fail "probe: authenticated footer identity differs from public loader")
  putStrLn
    ( "{\"rows\":"
        ++ show (Parquet.fmNumRows metadata)
        ++ ",\"rowGroups\":"
        ++ show (V.length (Parquet.fmRowGroups metadata))
        ++ ",\"columns\":"
        ++ show (map (V.length . Parquet.rgColumns) (V.toList (Parquet.fmRowGroups metadata)))
        ++ "}"
    )


hexBytes :: Text.Text -> Either String BS.ByteString
hexBytes text
  | odd (Text.length text) = Left "probe: odd hexadecimal byte count"
  | otherwise = BS.pack <$> traverse decode (Text.chunksOf 2 text)
  where
    decode pair = do
      (value, trailing) <- TextRead.hexadecimal pair
      if Text.null trailing then Right value else Left "probe: invalid hexadecimal bytes"


data FixtureKeys = FixtureKeys
  { fixtureFooter :: !Text.Text
  , fixtureFooterMetadata :: !Text.Text
  , fixturePrefix :: !(Maybe Text.Text)
  , fixtureMode :: !Text.Text
  , fixtureColumns :: !(V.Vector FixtureColumnKey)
  , fixtureProjection :: !(Maybe (V.Vector Text.Text))
  , fixturePruneLower :: !(Maybe Text.Text)
  }


data FixtureColumnKey = FixtureColumnKey
  { fixtureColumnPath :: !(V.Vector Text.Text)
  , fixtureColumnKey :: !Text.Text
  , fixtureColumnMetadata :: !Text.Text
  }


instance FromJSON FixtureKeys where
  parseJSON = withObject "public fixture keys" $ \o ->
    FixtureKeys
      <$> o .: "footer"
      <*> o .: "footerMetadata"
      <*> o .:? "prefix"
      <*> o .: "mode"
      <*> o .: "columns"
      <*> o .:? "projection"
      <*> o .:? "prune"


instance FromJSON FixtureColumnKey where
  parseJSON = withObject "public fixture column key" $ \o ->
    FixtureColumnKey
      <$> o .: "path"
      <*> o .: "key"
      <*> o .: "metadata"


encryptedRead :: FilePath -> FilePath -> FilePath -> IO ()
encryptedRead path keyPath output = do
  keyBytes <- BS.readFile keyPath
  keys <- either fail pure (Aeson.eitherDecodeStrict' keyBytes)
  footer <- either fail pure (hexBytes (fixtureFooter keys))
  footerMetadata <- either fail pure (hexBytes (fixtureFooterMetadata keys))
  prefix <- traverse (either fail pure . hexBytes) (fixturePrefix keys)
  columns <-
    V.mapM
      ( \entry -> do
          key <- either fail pure (hexBytes (fixtureColumnKey entry))
          metadata <- either fail pure (hexBytes (fixtureColumnMetadata entry))
          pure (ResolvedFixtureKey (fixtureColumnPath entry) key metadata)
      )
      (fixtureColumns keys)
  let explicit = fixtureMode keys == "explicit"
      config =
        Decryption.defaultDecryptionConfig
          { Decryption.dcFooterKey = if explicit then Just footer else Nothing
          , Decryption.dcColumnKeys = if explicit then V.foldl' (\m c -> Map.insert (resolvedPath c) (resolvedKey c) m) Map.empty columns else Map.empty
          , Decryption.dcKeyMetadataKeys = if explicit then Map.empty else V.foldl' (\m c -> Map.insert (resolvedMetadata c) (resolvedKey c) m) (Map.singleton footerMetadata footer) columns
          , Decryption.dcAadPrefix = prefix
          }
  bytes <- BS.readFile path
  file <- either fail pure (PHL.decodeParquet PHL.defaultReadOptions {PHL.readDecryptionConfig = Just config} bytes)
  native <- either fail pure (PArrow.parquetFileArrowSchemaChecked file)
  selected <- case fixtureProjection keys of
    Nothing -> pure (AT.arrowFields native)
    Just names -> V.mapM (\name -> maybe (fail "probe: projected field missing") pure (V.find ((== name) . AT.fieldName) (AT.arrowFields native))) names
  let schema = native {AT.arrowFields = selected}
      metadata = Read.pfFooter file
      groups = Parquet.fmRowGroups metadata
      leaves = V.filter (maybe False (const True) . Parquet.seType) (Parquet.fmSchema metadata)
  batches <- case fixturePruneLower keys of
    Nothing -> either (fail . show) pure (V.generateM (V.length groups) (PArrow.parquetRowGroupToArrow schema file))
    Just lower -> do
      when (V.length selected /= 1) (fail "probe: pruning needs one selected field")
      let field = V.head selected
      literal <- either fail pure (predicateValue (AT.fieldType field) lower)
      V.generateM (V.length groups) $ \groupIndex -> do
        let group = groups V.! groupIndex
        index <- maybe (fail "probe: pruning column missing") pure (V.findIndex (\column -> maybe False ((== V.singleton (AT.fieldName field)) . Parquet.cmPathInSchema) (Parquet.ccMetadata column)) (Parquet.rgColumns group))
        (_, values) <- either fail pure (PArrow.readParquetColumnWithPagePruning file groupIndex index field (Predicate.PGtEq literal))
        pure (V.singleton values)
  _ <- evaluate (force batches)
  encoded <- either fail pure (AS.encodeArrowStream AS.defaultWriteOptions schema (V.toList batches))
  BS.writeFile output encoded
  BL.putStr (Aeson.encode (object ["rows" .= Parquet.fmNumRows metadata, "rowGroups" .= V.length groups, "columns" .= V.length leaves]))
  putStrLn ""
  V.imapM_
    ( \groupIndex group ->
        V.imapM_
          ( \columnIndex column -> case Parquet.ccMetadata column of
              Nothing -> pure ()
              Just columnMetadata -> do
                let pathParts = Parquet.cmPathInSchema columnMetadata
                    wanted = case fixtureProjection keys of
                      Nothing -> True
                      Just names -> not (V.null pathParts) && V.head pathParts `V.elem` names
                when (wanted && fixturePruneLower keys == Nothing) $ do
                  chunk <- either fail pure (Read.columnChunkSlice file groupIndex columnIndex)
                  (maxRep, maxDef) <- either fail pure (Levels.maxLevelsForColumnPath (Parquet.fmSchema metadata) pathParts)
                  leaf <- maybe (fail "probe: missing physical leaf") pure (leaves V.!? columnIndex)
                  physical <- either fail pure (physicalColumn leaf columnMetadata maxRep maxDef chunk)
                  bounds <- either fail pure (PageIndex.readColumnIndex file groupIndex columnIndex)
                  offsets <- either fail pure (PageIndex.readOffsetIndex file groupIndex columnIndex)
                  bloom <- either fail pure (Bloom.readBloomFilter file groupIndex columnIndex)
                  BL.putStr
                    ( Aeson.encode
                        ( object
                            [ "rowGroup" .= groupIndex
                            , "column" .= columnIndex
                            , "path" .= Text.intercalate "." (V.toList pathParts)
                            , "type" .= Parquet.parquetTypeToInt (Parquet.cmType columnMetadata)
                            , "maxDefinition" .= maxDef
                            , "maxRepetition" .= maxRep
                            , "columnIndex" .= maybe False (const True) bounds
                            , "offsetPages" .= maybe 0 (V.length . Parquet.oiPageLocations) offsets
                            , "bloomBytes" .= maybe 0 Bloom.sbbfNumBytes bloom
                            , "events" .= VP.length (physicalDefinitions physical)
                            , "present" .= V.length (physicalValues physical)
                            , "definitions" .= VP.toList (physicalDefinitions physical)
                            , "repetitions" .= VP.toList (physicalRepetitions physical)
                            , "values" .= V.map hexString (physicalValues physical)
                            ]
                        )
                    )
                  putStrLn ""
          )
          (Parquet.rgColumns group)
    )
    groups


data ResolvedFixtureKey = ResolvedFixtureKey
  { resolvedPath :: !(V.Vector Text.Text)
  , resolvedKey :: !BS.ByteString
  , resolvedMetadata :: !BS.ByteString
  }


data PhysicalColumn = PhysicalColumn
  { physicalRepetitions :: !(VP.Vector Int32)
  , physicalDefinitions :: !(VP.Vector Int32)
  , physicalValues :: !(V.Vector BS.ByteString)
  }


physicalColumn :: Parquet.SchemaElement -> Parquet.ColumnMetadata -> Int -> Int -> BS.ByteString -> Either String PhysicalColumn
physicalColumn leaf metadata maxRep maxDef bytes = case Parquet.cmType metadata of
  Parquet.PTBoolean -> convert (V.map (BS.singleton . \value -> if value then 1 else 0)) (Read.readBoolLevels codec maxRep maxDef bytes)
  Parquet.PTInt32 -> convert (primitive Builder.int32LE) (Read.readInt32Levels codec maxRep maxDef bytes)
  Parquet.PTInt64 -> convert (primitive Builder.int64LE) (Read.readInt64Levels codec maxRep maxDef bytes)
  Parquet.PTFloat -> convert (primitive Builder.floatLE) (Read.readFloatLevels codec maxRep maxDef bytes)
  Parquet.PTDouble -> convert (primitive Builder.doubleLE) (Read.readDoubleLevels codec maxRep maxDef bytes)
  Parquet.PTInt96 -> convert id (Read.readFixedLevels 12 codec maxRep maxDef bytes)
  Parquet.PTByteArray -> convert id (Read.readByteArrayLevels codec maxRep maxDef bytes)
  Parquet.PTFixedLenByteArray -> do
    width <- maybe (Left "probe: missing fixed-byte width") Right (Parquet.seTypeLength leaf)
    convert id (Read.readFixedLevels (fromIntegral width) codec maxRep maxDef bytes)
  where
    codec = Parquet.cmCodec metadata
    convert values decoded = do
      levels <- decoded
      Right (PhysicalColumn (Read.clRepetition levels) (Read.clDefinition levels) (values (Read.clValues levels)))
    primitive encode values = V.generate (VP.length values) (BL.toStrict . Builder.toLazyByteString . encode . (values VP.!))


hexString :: BS.ByteString -> String
hexString = concatMap (\byte -> let digits = showHex byte "" in if length digits == 1 then '0' : digits else digits) . BS.unpack


variantArrowExport :: FilePath -> FilePath -> IO ()
variantArrowExport path out = do
  bytes <- BS.readFile path
  (schema, batches) <- either fail pure (ArrowFile.readArrowFileColumns AS.defaultReadOptions bytes)
  when (V.length (AT.arrowFields schema) /= 1) (fail "Variant Arrow probe expects one column")
  createDirectoryIfMissing True out
  _ <-
    V.foldM'
      ( \base columns -> do
          when (V.length columns /= 1) (fail "Variant Arrow probe expects one column")
          reader <- either fail pure (VariantArrow.compileVariantArrowReader (V.head (AT.arrowFields schema)) (V.head columns))
          forM_ [0 .. VariantArrow.variantArrowLength reader - 1] $ \row -> do
            value <- either fail pure (VariantArrow.variantArrowAt reader row)
            let digits = show (base + row)
                stem = out </> replicate (max 0 (6 - length digits)) '0' ++ digits
            case value of
              Nothing -> BS.writeFile (stem ++ ".null") BS.empty
              Just present -> do
                encoded <- either fail pure (Variant.encodeVariant present)
                BS.writeFile (stem ++ ".meta") (Variant.variantMetadata encoded)
                BS.writeFile (stem ++ ".value") (Variant.variantValue encoded)
          pure (base + VariantArrow.variantArrowLength reader)
      )
      0
      batches
  pure ()


-- Explicit semantic mode for corpus fixtures containing unannotated binary
-- metadata/value columns. Ordinary binary reads must preserve these bytes.
variantPayloads :: FilePath -> IO ()
variantPayloads path = do
  bytes <- BS.readFile path
  let result = do
        file <- PHL.decodeParquet PHL.defaultReadOptions bytes
        schema <- PArrow.parquetFileArrowSchemaChecked file
        fields <- V.mapM (\name -> maybe (Left "variant probe: missing metadata/value field") Right (V.find ((== name) . AT.fieldName) (AT.arrowFields schema))) (V.fromList ["metadata", "value"])
        let group i = do
              columns <- either (Left . show) Right (PArrow.parquetRowGroupToArrow schema {AT.arrowFields = fields} file i)
              let count = AC.columnLength (V.head columns)
              column <- AC.mkStruct count Nothing (V.zip (AT.fieldName <$> fields) columns)
              reader <- VariantParquet.compileVariantReader column
              let loop row
                    | row == count = Right count
                    | otherwise = VariantParquet.variantAt reader row >> loop (row + 1)
              loop 0
        sum <$> mapM group [0 .. PArrow.numRowGroups file - 1]
  checked <- evaluate (force result)
  case checked of
    Left message -> putStrLn message >> exitFailure
    Right count -> putStrLn ("Variant payloads validated: " ++ show count)


variantExport :: FilePath -> Text.Text -> FilePath -> IO ()
variantExport path name out = do
  bytes <- BS.readFile path
  file <- either fail pure (PHL.decodeParquet PHL.defaultReadOptions bytes)
  createDirectoryIfMissing True out
  forM_ [0 .. PArrow.numRowGroups file - 1] $ \group -> do
    values <- either fail pure (VariantParquet.readVariantColumn file group name)
    V.imapM_
      ( \row value -> do
          let stem = out </> show group ++ "-" ++ show row
          case value of
            Nothing -> BS.writeFile (stem ++ ".null") BS.empty
            Just present -> do
              encoded <- either fail pure (Variant.encodeVariant present)
              BS.writeFile (stem ++ ".meta") (Variant.variantMetadata encoded)
              BS.writeFile (stem ++ ".value") (Variant.variantValue encoded)
      )
      values


-- Exercise the public indexed Arrow reader, retaining whole matching pages.
prune :: FilePath -> String -> FilePath -> IO ()
prune path literal output = do
  bytes <- BS.readFile path
  either
    (\err -> putStrLn err >> exitFailure)
    ( \(kept, encoded) -> do
        BS.writeFile output encoded
        putStrLn ("pages=" ++ show kept)
    )
    $ do
      file <- PHL.decodeParquet PHL.defaultReadOptions bytes
      schema <- PArrow.parquetFileArrowSchemaChecked file
      let fields = flattenFields (AT.arrowFields schema)
      native <- maybe (Left "probe: no leaf field") Right (fields V.!? 0)
      value <- predicateValue (AT.fieldType native) (Text.pack literal)
      let target = native {AT.fieldNullable = True}
      (kept, column) <- PArrow.readParquetColumnWithPagePruning file 0 0 target (Predicate.PGtEq value)
      encoded <-
        AS.encodeArrowStream
          AS.defaultWriteOptions
          (schema {AT.arrowFields = V.singleton target})
          [V.singleton column]
      Right (kept, encoded)
  where
    flattenFields =
      V.concatMap
        ( \field ->
            if V.null (AT.fieldChildren field) then V.singleton field else flattenFields (AT.fieldChildren field)
        )


predicateValue :: AT.ArrowType -> Text.Text -> Either String Predicate.PValue
predicateValue typ input = case typ of
  AT.AInt width signed
    | not signed -> do
        integer <- integral
        if integer < 0 || integer > toInteger (maxBound :: Word64)
          then Left "probe: unsigned literal out of range"
          else Right (Predicate.PVUnsigned (fromInteger integer))
    | width <= 32 -> do
        integer <- integral
        if integer < toInteger (minBound :: Int32) || integer > toInteger (maxBound :: Int32)
          then Left "probe: INT32 literal out of range"
          else Right (Predicate.PVInt32 (fromInteger integer))
    | otherwise -> int64
  AT.AFloatingPoint AT.Single -> Predicate.PVFloat <$> fractional
  AT.AFloatingPoint _ -> Predicate.PVDouble <$> fractional
  AT.ADecimal {} -> Predicate.PVDecimal <$> fractional
  AT.ADecimal256 {} -> Predicate.PVDecimal <$> fractional
  AT.AUtf8 -> Right (Predicate.PVText input)
  AT.ABinary -> binary
  AT.AFixedSizeBinary _ -> binary
  AT.ABool -> case Text.toLower input of
    "true" -> Right (Predicate.PVBool True)
    "false" -> Right (Predicate.PVBool False)
    _ -> Left "probe: invalid boolean literal"
  AT.ADate _ -> Predicate.PVInt32 . fromInteger <$> integral
  AT.ATime _ 32 -> Predicate.PVInt32 . fromInteger <$> integral
  AT.ATime _ _ -> int64
  AT.ATimestamp {} -> int64
  _ -> Left "probe: unsupported predicate type"
  where
    entire parsed = do
      (value, trailing) <- parsed
      if Text.null trailing then Right value else Left "probe: trailing literal text"
    integral = entire (TextRead.signed TextRead.decimal input) :: Either String Integer
    fractional :: Fractional a => Either String a
    fractional = entire (TextRead.signed TextRead.rational input)
    int64 = do
      integer <- integral
      if integer < toInteger (minBound :: Int64) || integer > toInteger (maxBound :: Int64)
        then Left "probe: INT64 literal out of range"
        else Right (Predicate.PVInt64 (fromInteger integer))
    binary
      | odd (Text.length input) = Left "probe: odd hexadecimal byte count"
      | otherwise =
          Predicate.PVBinary . BS.pack
            <$> traverse
              (\pair -> entire (TextRead.hexadecimal pair))
              (Text.chunksOf 2 input)


-- A metadata-only mode lets an independent Thrift implementation exercise
-- compact bounds without relying on this probe's value decoder.
aggregate :: FilePath -> IO ()
aggregate path = do
  bytes <- BS.readFile path
  case Footer.readFooter bytes of
    Left err -> putStrLn err >> exitFailure
    Right metadata -> do
      putStrLn ("rows=" ++ show (Aggregate.fileRowCountChecked metadata))
      putStrLn ("non_null=" ++ show (Aggregate.columnNonNullCount metadata 0))
      putStrLn ("minimum=" ++ show (Aggregate.columnMin metadata 0))
      putStrLn ("maximum=" ++ show (Aggregate.columnMax metadata 0))


run :: FilePath -> Maybe FilePath -> IO ()
run dir out = do
  files <- sort . filter ((== ".parquet") . takeExtension) <$> listDirectory dir
  when (null files) $ do
    putStrLn "FAIL: no Parquet files found"
    exitFailure
  results <- mapM (probe dir out) files
  let !nFail = length (filter hasError results)
  forM_ results $ \result ->
    case probeError result of
      Nothing -> putStrLn ("  OK   " ++ probeName result)
      Just err -> putStrLn ("  FAIL " ++ probeName result ++ ": " ++ err)
  putStrLn (show (length results) ++ " files, " ++ show nFail ++ " failed")
  when (nFail /= 0) (exitWith (ExitFailure (min 99 nFail)))
  where
    hasError result = case probeError result of
      Nothing -> False
      Just _ -> True


probe :: FilePath -> Maybe FilePath -> FilePath -> IO ProbeResult
probe dir out name =
  check `catch` \err -> pure (ProbeResult name (Just (show (err :: SomeException))))
  where
    check = do
      bytes <- BS.readFile (dir </> name)
      -- Force within the exception handler: a lazy decoder exception must
      -- count as a failed file, rather than escaping while printing results.
      decoded <- evaluate (force (decodeOnce bytes))
      case decoded of
        Left err -> pure (ProbeResult name (Just err))
        Right arrowBytes -> do
          forM_ out $ \path -> BS.writeFile (path </> (name ++ ".arrows")) arrowBytes
          pure (ProbeResult name Nothing)


decodeOnce :: BS.ByteString -> Either String BS.ByteString
decodeOnce bytes = do
  pf <- PHL.decodeParquet PHL.defaultReadOptions bytes
  schema <- PArrow.parquetFileArrowSchemaChecked pf
  batches <- mapM (readGroup schema pf) [0 .. PArrow.numRowGroups pf - 1]
  AS.encodeArrowStream AS.defaultWriteOptions schema batches
  where
    readGroup schema pf i = case PArrow.parquetRowGroupToArrow schema pf i of
      Left err -> Left ("row group " ++ show i ++ ": " ++ show err)
      Right batch -> Right batch


-- The target schema is supplied as an Arrow IPC stream, avoiding a second
-- ad-hoc schema vocabulary in the external conformance harness.
project :: FilePath -> FilePath -> FilePath -> IO ()
project path schemaPath output = do
  bytes <- BS.readFile path
  schemaBytes <- BS.readFile schemaPath
  let result = do
        (schema, _) <- AS.decodeArrowStream AS.defaultReadOptions schemaBytes
        file <- PHL.decodeParquet PHL.defaultReadOptions bytes
        batches <-
          mapM
            (\i -> either (Left . show) Right (PArrow.parquetRowGroupToArrow schema file i))
            [0 .. PArrow.numRowGroups file - 1]
        AS.encodeArrowStream AS.defaultWriteOptions schema batches
  encoded <- evaluate (force result)
  case encoded of
    Left err -> putStrLn err >> exitFailure
    Right arrowBytes -> BS.writeFile output arrowBytes


-- The official large-map fixture has two1GiB keys. Summarize validated Arrow
-- buffers directly so the oracle never needs a multi-gigabyte IPC temporary
-- or Python string objects. The public projection requests64-bit key offsets.
largeMapSummary :: FilePath -> IO ()
largeMapSummary path = do
  bytes <- BS.readFile path
  file <- either fail pure (PHL.decodeParquet PHL.defaultReadOptions bytes)
  native <- either fail pure (PArrow.parquetFileArrowSchemaChecked file)
  let wide field =
        field
          { AT.fieldType = case AT.fieldType field of AT.AUtf8 -> AT.ALargeUtf8; AT.ABinary -> AT.ALargeBinary; typ -> typ
          , AT.fieldChildren = wide <$> AT.fieldChildren field
          }
      target = native {AT.arrowFields = wide <$> AT.arrowFields native}
  columns <- either (fail . show) pure (PArrow.parquetRowGroupToArrow target file 0)
  case V.toList columns of
    [column@(AC.ColMap _ offsets keys values)] -> case (keys, values) of
      (AC.ColLargeUtf8 _ keyOffsets payload, AC.ColInt32 _ integers) -> do
        putStrLn ("rows=" ++ show (AC.columnLength column))
        putStrLn ("map_nulls=" ++ show (AC.nullCount column))
        putStrLn ("map_offsets=" ++ show (S.toList offsets))
        putStrLn ("key_nulls=" ++ show (AC.nullCount keys))
        putStrLn ("key_offsets=" ++ show (S.toList keyOffsets))
        putStrLn ("key_bytes=" ++ show (BS.length payload))
        putStrLn ("key_sha256=" ++ show (hash payload :: Digest SHA256))
        putStrLn ("value_nulls=" ++ show (AC.nullCount values))
        putStrLn ("values=" ++ show (S.toList integers))
      _ -> fail "large-map probe expected LargeUtf8 keys and Int32 values"
    _ -> fail "large-map probe expected one Map column"
