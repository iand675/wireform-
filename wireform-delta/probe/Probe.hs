{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | wireform-delta interop probe.

Opens a Delta table on disk via 'Delta.IO.openDeltaTable' and
writes a JSON summary of what wireform sees: protocol +
metadata + active file set + per-app txn versions + last
commit operation + last_checkpoint pointer + checkpoint
discovery.

Usage:
  wireform-delta-interop-probe <table-root> [<output.json>]
-}
module Main (main) where

import Arrow.Column qualified as AC
import Arrow.Types qualified as AT
import Columnar.Stream (iterIOToList, iterToList)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Data.Word (Word64)
import Delta.Checkpoint qualified as DC
import Delta.IO qualified as DIO
import Delta.Log qualified as D
import Delta.Scan qualified as Scan
import GHC.Float (float2Double)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Exit qualified
import Text.Read (readMaybe)


main :: IO ()
main = do
  args <- getArgs
  (tableRoot, outputDest) <- case args of
    ["--dump-ckpt", p] -> do
      r <- DC.readCheckpointFile p
      case r of
        Right acts -> do
          putStrLn $ "decoded " ++ show (length acts) ++ " action rows"
          mapM_ (putStrLn . show) (take 30 acts)
        Left e -> putStrLn ("ERROR: " ++ e)
      exitFailure
    ["--at", verStr, i, o] ->
      case readMaybe verStr of
        Just v -> probeAt v i o
        Nothing -> exitFailure
    ["--validate-checksum", version, root] -> do
      table <- case readMaybe version of
        Just number -> DIO.openDeltaTableAt root number >>= either fail pure
        Nothing -> fail "invalid version"
      present <- DIO.validateVersionChecksum table >>= either fail pure
      BL.putStr (Aeson.encode present)
      System.Exit.exitSuccess
    ["--scan-ids", root, out] -> do
      table <- DIO.openDeltaTable root >>= either fail pure
      values <- traverse (scanIDs root) (DIO.dtActiveFiles table)
      BL.writeFile out (Aeson.encode (concat values))
      System.Exit.exitSuccess
    ["--scan-table", root, out] -> do
      table <- DIO.openDeltaTable root >>= either fail pure
      (schema, stream) <- either fail pure (Scan.scanTable table)
      batches <- iterIOToList stream >>= either fail pure
      rows <- either fail pure (concat <$> traverse (tableRows (AT.arrowFields schema)) batches)
      readStats <- either fail pure (Scan.tableFileStats table)
      stats <- either fail pure (traverse readStats (DIO.dtActiveFiles table))
      BL.writeFile out (Aeson.encode (Aeson.object ["rows" Aeson..= rows, "stats" Aeson..= map (fmap statsJSON) stats]))
      System.Exit.exitSuccess
    "--scan-projected" : root : out : names -> do
      table <- DIO.openDeltaTable root >>= either fail pure
      (schema, stream) <- either fail pure (Scan.scanTableProjected (map T.pack names) table)
      batches <- iterIOToList stream >>= either fail pure
      rows <- either fail pure (concat <$> traverse (tableRows (AT.arrowFields schema)) batches)
      BL.writeFile out (Aeson.encode (Aeson.object ["columns" Aeson..= fmap AT.fieldName (AT.arrowFields schema), "rows" Aeson..= rows]))
      System.Exit.exitSuccess
    ["--scan-tracked", root, out] -> do
      table <- DIO.openDeltaTable root >>= either fail pure
      (schema, stream) <- either fail pure (Scan.scanTrackedTable table)
      batches <- iterIOToList stream >>= either fail pure
      rows <- either fail pure (concat <$> traverse (tableRows (AT.arrowFields schema) . Scan.trackedData) batches)
      let ids = concatMap (VS.toList . Scan.rowIds) batches
          versions = concatMap (VS.toList . Scan.rowCommitVersions) batches
      BL.writeFile out (Aeson.encode (Aeson.object ["rows" Aeson..= rows, "ids" Aeson..= ids, "versions" Aeson..= versions]))
      System.Exit.exitSuccess
    [i] -> pure (i, Nothing)
    [i, o] -> pure (i, Just o)
    _ -> do
      putStrLn "usage: wireform-delta-interop-probe [--at VER] <table-root> [<output.json>]"
      exitFailure

  res <- DIO.openDeltaTable tableRoot
  case res of
    Left err -> do
      putStrLn ("wireform-delta-interop-probe: " ++ err)
      exitFailure
    Right dt -> writeProbeOutput tableRoot outputDest dt


scanIDs :: FilePath -> D.AddAction -> IO [Int64]
scanIDs root file = do
  (_, stream) <- Scan.openDataFile root file >>= either fail pure
  batches <- either fail pure (iterToList stream)
  fmap concat $ traverse oneColumn batches
  where
    oneColumn batch = case V.toList (Scan.batchColumns batch) of
      [AC.ColInt64 _ ids] -> pure (VS.toList ids)
      _ -> fail "expected one Int64 column"


tableRows :: V.Vector AT.Field -> Scan.DataBatch -> Either String [Aeson.Value]
tableRows fields batch = traverse row [0 .. Scan.batchRowCount batch - 1]
  where
    row i = Aeson.Object . KM.fromList . V.toList <$> V.zipWithM (fieldAt i) fields (Scan.batchColumns batch)
    fieldAt i field column = (Key.fromText (AT.fieldName field),) <$> cell i column
    cell i column
      | not (AC.isValidAt (AC.validity column) i) = pure Aeson.Null
      | otherwise = case column of
          AC.ColNull _ -> pure Aeson.Null
          AC.ColInt8 _ values -> pure (Aeson.toJSON (values VS.! i))
          AC.ColInt16 _ values -> pure (Aeson.toJSON (values VS.! i))
          AC.ColInt32 _ values -> pure (Aeson.toJSON (values VS.! i))
          AC.ColInt64 _ values -> pure (Aeson.toJSON (values VS.! i))
          AC.ColFloat _ values -> pure (floatingCell (float2Double (values VS.! i)))
          AC.ColDouble _ values -> pure (floatingCell (values VS.! i))
          AC.ColDecimal128 _ _ _ values -> pure (Aeson.toJSON (AC.decimal128ToInteger (values VS.! i)))
          AC.ColDate32 _ values -> pure (Aeson.toJSON (values VS.! i))
          AC.ColTimestamp _ values -> pure (Aeson.toJSON (values VS.! i))
          AC.ColUtf8 {} -> pure (maybe Aeson.Null Aeson.String (AC.anyTextAt column i))
          AC.ColBinary {} -> pure (maybe Aeson.Null (Aeson.toJSON . BS.unpack) (AC.anyBytesAt column i))
          AC.ColStruct _ _ children -> do
            entries <- V.mapM (\(name, values) -> (Key.fromText name,) <$> cell i values) children
            pure (Aeson.Object (KM.fromList (V.toList entries)))
          AC.ColList _ offsets values ->
            Aeson.toJSON <$> traverse (`cell` values) [fromIntegral (offsets VS.! i) .. fromIntegral (offsets VS.! (i + 1)) - 1]
          AC.ColMap _ offsets keys values ->
            Aeson.Object . KM.fromList
              <$> traverse
                ( \j -> do
                    k <- maybe (Left "expected UTF-8 map key") Right (AC.anyTextAt keys j)
                    v <- cell j values
                    pure (Key.fromText k, v)
                )
                [fromIntegral (offsets VS.! i) .. fromIntegral (offsets VS.! (i + 1)) - 1]
          _ -> Left ("unsupported probe column " ++ AC.columnTag column)
    floatingCell value
      | isNegativeZero value = Aeson.String "-0"
      | isNaN value = Aeson.String "NaN"
      | otherwise = Aeson.toJSON (value :: Double)


probeAt :: Word64 -> FilePath -> FilePath -> IO a
probeAt v tableRoot output = do
  res <- DIO.openDeltaTableAt tableRoot v
  case res of
    Left err -> do
      putStrLn ("wireform-delta-interop-probe (--at): " ++ err)
      exitFailure
    Right dt -> do
      writeProbeOutput tableRoot (Just output) dt
      System.Exit.exitSuccess


writeProbeOutput
  :: FilePath
  -> Maybe FilePath
  -> DIO.DeltaTable
  -> IO ()
writeProbeOutput tableRoot outputDest dt = do
  let snap = DIO.dtSnapshot dt
  hist <- DIO.historyEntries dt
  let activeFlat = DIO.activeFilePaths dt
      partsByPV = DIO.partitionedActiveFiles dt
  ckpt <- case DIO.dtCheckpointAvailable dt of
    Just v -> do
      let p = tableRoot ++ "/_delta_log/" ++ pad20 v ++ ".checkpoint.parquet"
      exists <- doesFileExist p
      if not exists
        then pure Nothing
        else do
          r <- DC.readCheckpointFile p
          case r of
            Right acts ->
              pure (Just (D.snapshotFromActions acts))
            Left _ -> pure Nothing
    Nothing -> pure Nothing
  let summary =
        Aeson.Object $
          KM.fromList
            [ (Key.fromString "version", maybe Aeson.Null numW64 (DIO.dtVersion dt))
            , (Key.fromString "num_commits", Aeson.Number (fromIntegral (length (DIO.dtCommits dt))))
            , (Key.fromString "active_files", filesJSON snap)
            , (Key.fromString "active_file_count", Aeson.Number (fromIntegral (Map.size (D.tsFiles snap))))
            , (Key.fromString "domains", Aeson.toJSON (D.tsDomains snap))
            , (Key.fromString "protocol", protocolJSON snap)
            , (Key.fromString "metadata", metadataJSON snap)
            ,
              ( Key.fromString "txn_app_versions"
              , Aeson.Object $
                  KM.fromList (map txnEntry (Map.toAscList (D.tsAppIds snap)))
              )
            ,
              ( Key.fromString "last_commit_operation"
              , case D.tsLastCommit snap of
                  Nothing -> Aeson.Null
                  Just c -> case D.ciOperation c of
                    Just op -> Aeson.String op
                    Nothing -> Aeson.Null
              )
            , (Key.fromString "last_checkpoint", lastCheckpointJSON (DIO.dtLastCheckpoint dt))
            ,
              ( Key.fromString "checkpoint_parquet_version"
              , maybe Aeson.Null numW64 (DIO.dtCheckpointAvailable dt)
              )
            ,
              ( Key.fromString "checkpoint_active_files"
              , case ckpt of
                  Just s ->
                    Aeson.Array $
                      V.fromList $
                        map
                          (Aeson.String . D.addPath)
                          (Map.elems (D.tsFiles s))
                  Nothing -> Aeson.Null
              )
            ,
              ( Key.fromString "checkpoint_protocol"
              , case ckpt of
                  Just s -> protocolJSON s
                  Nothing -> Aeson.Null
              )
            ,
              ( Key.fromString "checkpoint_metadata"
              , case ckpt of
                  Just s -> metadataJSON s
                  Nothing -> Aeson.Null
              )
            ,
              ( Key.fromString "active_relative_paths"
              , Aeson.Array (V.fromList (map Aeson.String activeFlat))
              )
            ,
              ( Key.fromString "active_partition_count"
              , Aeson.Number (fromIntegral (Map.size partsByPV))
              )
            , (Key.fromString "history", historyJSON hist)
            ]
  case outputDest of
    Nothing -> BL.putStr (Aeson.encode summary)
    Just o -> BL.writeFile o (Aeson.encode summary)


numW64 :: Word64 -> Aeson.Value
numW64 = Aeson.Number . fromIntegral


txnEntry :: (Text, Int64) -> (Key.Key, Aeson.Value)
txnEntry (k, v) = (Key.fromText k, Aeson.Number (fromIntegral v))


pad20 :: Word64 -> String
pad20 v =
  let s = show v
  in replicate (20 - length s) '0' ++ s


historyJSON :: [DIO.HistoryEntry] -> Aeson.Value
historyJSON xs = Aeson.Array $ V.fromList $ map entry xs
  where
    entry e =
      Aeson.Object $
        KM.fromList
          [ (Key.fromString "version", numW64 (DIO.heVersion e))
          , (Key.fromString "operation", maybe Aeson.Null Aeson.String (DIO.heOperation e))
          , (Key.fromString "timestamp", maybe Aeson.Null (Aeson.Number . fromIntegral) (DIO.heTimestamp e))
          ,
            ( Key.fromString "isolation_level"
            , maybe Aeson.Null Aeson.String (DIO.heIsolationLevel e)
            )
          ]


filesJSON :: D.TableSnapshot -> Aeson.Value
filesJSON snap =
  Aeson.Array $ V.fromList $ map fileEntry $ Map.toAscList (D.tsFiles snap)
  where
    fileEntry (path, a) =
      Aeson.Object $
        KM.fromList
          [ (Key.fromString "path", Aeson.String path)
          , (Key.fromString "size", numW64 (D.addSize a))
          , (Key.fromString "modificationTime", Aeson.Number (fromIntegral (D.addModificationTime a)))
          , (Key.fromString "stats", maybe Aeson.Null (either (Aeson.String . T.pack) statsJSON) (D.decodeAddStats a))
          ,
            ( Key.fromString "partition_values"
            , Aeson.Object $
                KM.fromList (map pvEntry (Map.toAscList (D.addPartitionValues a)))
            )
          ]
    pvEntry (k, Nothing) = (Key.fromText k, Aeson.Null)
    pvEntry (k, Just t) = (Key.fromText k, Aeson.String t)


statsJSON :: D.AddStats -> Aeson.Value
statsJSON stats =
  Aeson.object
    [ "numRecords" Aeson..= D.asNumRecords stats
    , "minValues" Aeson..= D.asMinValues stats
    , "maxValues" Aeson..= D.asMaxValues stats
    , "nullCount" Aeson..= fmap nullCountJSON (D.asNullCounts stats)
    , "tightBounds" Aeson..= D.effectiveTightBounds stats
    ]
  where
    nullCountJSON (D.NullCount n) = Aeson.toJSON n
    nullCountJSON (D.NestedNullCounts children) = Aeson.toJSON (fmap nullCountJSON children)


protocolJSON :: D.TableSnapshot -> Aeson.Value
protocolJSON snap = case D.tsProtocol snap of
  Nothing -> Aeson.Null
  Just p ->
    Aeson.Object $
      KM.fromList
        [ (Key.fromString "min_reader_version", Aeson.Number (fromIntegral (D.pMinReaderVersion p)))
        , (Key.fromString "min_writer_version", Aeson.Number (fromIntegral (D.pMinWriterVersion p)))
        , (Key.fromString "reader_features", Aeson.Array (V.fromList (map Aeson.String (D.pReaderFeatures p))))
        , (Key.fromString "writer_features", Aeson.Array (V.fromList (map Aeson.String (D.pWriterFeatures p))))
        ]


metadataJSON :: D.TableSnapshot -> Aeson.Value
metadataJSON snap = case D.tsMetaData snap of
  Nothing -> Aeson.Null
  Just md ->
    Aeson.Object $
      KM.fromList
        [ (Key.fromString "id", Aeson.String (D.mdId md))
        ,
          ( Key.fromString "partition_columns"
          , Aeson.Array (V.fromList (map Aeson.String (D.mdPartitionColumns md)))
          )
        ,
          ( Key.fromString "configuration"
          , Aeson.Object $
              KM.fromList
                [ (Key.fromText k, Aeson.String v)
                | (k, v) <- Map.toAscList (D.mdConfiguration md)
                ]
          )
        , (Key.fromString "schema_field_names", schemaFieldNames md)
        ]
  where
    schemaFieldNames mdv = case D.parseDeltaSchema (D.mdSchemaString mdv) of
      Right s -> Aeson.Array (V.fromList (map (Aeson.String . D.dfName) (D.dsFields s)))
      Left _ -> Aeson.Null


lastCheckpointJSON :: Maybe D.LastCheckpoint -> Aeson.Value
lastCheckpointJSON Nothing = Aeson.Null
lastCheckpointJSON (Just lc) =
  Aeson.Object $
    KM.fromList
      [ (Key.fromString "version", numW64 (D.lcVersion lc))
      , (Key.fromString "size", numW64 (D.lcSize lc))
      ,
        ( Key.fromString "parts"
        , maybe Aeson.Null numW64 (D.lcParts lc)
        )
      ,
        ( Key.fromString "size_in_bytes"
        , maybe Aeson.Null numW64 (D.lcSizeInBytes lc)
        )
      ,
        ( Key.fromString "num_of_add_files"
        , maybe Aeson.Null numW64 (D.lcNumOfAddFiles lc)
        )
      ]
