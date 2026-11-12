{-# LANGUAGE OverloadedStrings #-}

{- | Tests for 'Delta.Log'.

These cover the public surface — action variants, the
'TableSnapshot' replay, the schema-string decoder, and the
@_last_checkpoint@ parser — without trying to test things
inherent to aeson (e.g. \"ints round-trip through JSON\").
-}
module Main (main) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict qualified as HM
import Data.Map.Strict qualified as Map
import Delta.Log qualified as D
import Test.Syd


main :: IO ()
main =
  sydTest $
    describe "wireform-delta" $
      sequence_
        [ it "parseLogLine: add" addLineTest
        , it "parseLogLine: remove" removeLineTest
        , it "parseLogLine: protocol" protocolLineTest
        , it "parseLogLine: metaData" metaDataLineTest
        , it "parseLogLine: commitInfo" commitInfoLineTest
        , it "parseLogLine: txn" txnLineTest
        , it "parseLogLine: cdc" cdcLineTest
        , it "snapshot fold collapses add+remove" snapshotFoldTest
        , it "replays deletion-vector replacements in either action order" deletionVectorReplayTest
        , it "uses the last signed transaction version for each app" transactionReplayTest
        , it "snapshot reflects metadata + protocol" snapshotProtoMetaTest
        , it "schemaString decodes nested types" schemaTest
        , it "_last_checkpoint parser" lastCheckpointTest
        , it "AddStats decoder" addStatsTest
        , it "decodeAddStats helper" decodeAddStatsHelperTest
        ]


-- ============================================================
-- Action parser cases
-- ============================================================

addLineTest :: IO ()
addLineTest = case D.parseLogLine addJson of
  Just (D.ActionAdd a) -> do
    D.addPath a `shouldBe` "data/file.parquet"
    D.addSize a `shouldBe` 1024
    D.addModificationTime a `shouldBe` 1700000000000
    D.addDataChange a `shouldBe` True
    D.addStats a `shouldBe` Just "{\"numRecords\":42}"
    Map.lookup "p" (D.addPartitionValues a) `shouldBe` Just (Just "v")
  _ -> expectationFailure "expected ActionAdd"
  where
    addJson = "{\"add\":{\"path\":\"data/file.parquet\",\"size\":1024,\"modificationTime\":1700000000000,\"dataChange\":true,\"stats\":\"{\\\"numRecords\\\":42}\",\"partitionValues\":{\"p\":\"v\"}}}"


removeLineTest :: IO ()
removeLineTest = case D.parseLogLine removeJson of
  Just (D.ActionRemove r) -> do
    D.removePath r `shouldBe` "data/old.parquet"
    D.removeDeletionTimestamp r `shouldBe` Just 1700000005000
  _ -> expectationFailure "expected ActionRemove"
  where
    removeJson = "{\"remove\":{\"path\":\"data/old.parquet\",\"deletionTimestamp\":1700000005000,\"dataChange\":false}}"


protocolLineTest :: IO ()
protocolLineTest = case D.parseLogLine protoJson of
  Just (D.ActionProtocol p) -> do
    D.pMinReaderVersion p `shouldBe` 3
    D.pMinWriterVersion p `shouldBe` 7
    D.pReaderFeatures p `shouldBe` ["deletionVectors"]
    D.pWriterFeatures p `shouldBe` ["deletionVectors", "columnMapping"]
  _ -> expectationFailure "expected ActionProtocol"
  where
    protoJson = "{\"protocol\":{\"minReaderVersion\":3,\"minWriterVersion\":7,\"readerFeatures\":[\"deletionVectors\"],\"writerFeatures\":[\"deletionVectors\",\"columnMapping\"]}}"


metaDataLineTest :: IO ()
metaDataLineTest = case D.parseLogLine mdJson of
  Just (D.ActionMetaData md) -> do
    D.mdId md `shouldBe` "abc-uuid"
    D.mdPartitionColumns md `shouldBe` ["p"]
    D.mdSchemaString md `shouldBe` "{\"type\":\"struct\",\"fields\":[]}"
  _ -> expectationFailure "expected ActionMetaData"
  where
    mdJson = "{\"metaData\":{\"id\":\"abc-uuid\",\"format\":{\"provider\":\"parquet\",\"options\":{}},\"schemaString\":\"{\\\"type\\\":\\\"struct\\\",\\\"fields\\\":[]}\",\"partitionColumns\":[\"p\"],\"configuration\":{}}}"


commitInfoLineTest :: IO ()
commitInfoLineTest = case D.parseLogLine ciJson of
  Just (D.ActionCommitInfo ci) -> do
    D.ciTimestamp ci `shouldBe` Just 1700000000000
    D.ciOperation ci `shouldBe` Just "WRITE"
    D.ciIsBlindAppend ci `shouldBe` Just True
  _ -> expectationFailure "expected ActionCommitInfo"
  where
    ciJson = "{\"commitInfo\":{\"timestamp\":1700000000000,\"operation\":\"WRITE\",\"operationParameters\":{},\"isBlindAppend\":true}}"


txnLineTest :: IO ()
txnLineTest = case D.parseLogLine txnJson of
  Just (D.ActionTxn t) -> do
    D.txnAppId t `shouldBe` "stream-1"
    D.txnVersion t `shouldBe` 7
  _ -> expectationFailure "expected ActionTxn"
  where
    txnJson = "{\"txn\":{\"appId\":\"stream-1\",\"version\":7,\"lastUpdated\":1700000000000}}"


cdcLineTest :: IO ()
cdcLineTest = case D.parseLogLine cdcJson of
  Just (D.ActionCdc c) -> do
    D.cdcPath c `shouldBe` "_change_data/cdc-1.parquet"
    D.cdcSize c `shouldBe` 2048
    D.cdcDataChange c `shouldBe` False
  _ -> expectationFailure "expected ActionCdc"
  where
    cdcJson = "{\"cdc\":{\"path\":\"_change_data/cdc-1.parquet\",\"size\":2048,\"dataChange\":false,\"partitionValues\":{}}}"


-- ============================================================
-- Snapshot fold
-- ============================================================

snapshotFoldTest :: IO ()
snapshotFoldTest = do
  let actions =
        [ D.ActionAdd
            D.AddAction
              { D.addPath = "data/a.parquet"
              , D.addSize = 100
              , D.addModificationTime = 1
              , D.addDataChange = True
              , D.addStats = Nothing
              , D.addPartitionValues = Map.empty
              , D.addTags = Map.empty
              , D.addDeletionVector = Nothing
              }
        , D.ActionAdd
            D.AddAction
              { D.addPath = "data/b.parquet"
              , D.addSize = 200
              , D.addModificationTime = 2
              , D.addDataChange = True
              , D.addStats = Nothing
              , D.addPartitionValues = Map.empty
              , D.addTags = Map.empty
              , D.addDeletionVector = Nothing
              }
        , D.ActionRemove
            D.RemoveAction
              { D.removePath = "data/a.parquet"
              , D.removeDeletionTimestamp = Just 3
              , D.removeDataChange = True
              , D.removeExtendedFileMetadata = Nothing
              , D.removeSize = Nothing
              , D.removePartitionValues = Map.empty
              , D.removeDeletionVector = Nothing
              }
        ]
      snap = D.snapshotFromActions actions
  Map.keys (D.tsFiles snap) `shouldBe` ["data/b.parquet"]
  map D.addPath (D.activeFiles actions) `shouldBe` ["data/b.parquet"]


deletionVectorReplayTest :: IO ()
deletionVectorReplayTest = do
  let noVector = "{\"add\":{\"path\":\"p.parquet\",\"size\":1,\"modificationTime\":0,\"dataChange\":true}}"
      removeNoVector = "{\"remove\":{\"path\":\"p.parquet\",\"dataChange\":true}}"
      action prefix offset =
        prefix <> "{\"storageType\":\"p\",\"pathOrInlineDv\":\"file:///vectors\",\"offset\":"
          <> offset <> ",\"sizeInBytes\":32,\"cardinality\":1}}}"
      addVector = action "{\"add\":{\"path\":\"p.parquet\",\"size\":1,\"modificationTime\":0,\"dataChange\":true,\"deletionVector\":"
      removeVector = action "{\"remove\":{\"path\":\"p.parquet\",\"dataChange\":true,\"deletionVector\":"
      replay = D.tsFiles . D.snapshotFromActions . D.parseLogFile . BL.intercalate "\n"
      expected = replay [addVector "2"]
  Map.keys expected `shouldBe` ["p.parquet"]
  replay [noVector, addVector "2", removeNoVector] `shouldBe` expected
  replay [noVector, removeNoVector, addVector "2"] `shouldBe` expected
  replay [addVector "1", addVector "2", removeVector "1"] `shouldBe` expected
  replay [addVector "1", removeVector "1", addVector "2"] `shouldBe` expected
  replay [addVector "2", removeVector "2"] `shouldBe` Map.empty


transactionReplayTest :: IO ()
transactionReplayTest = do
  let record app version = "{\"txn\":{\"appId\":\"" <> app <> "\",\"version\":" <> version <> "}}"
      replay = D.tsAppIds . D.snapshotFromActions . D.parseLogFile . BL.intercalate "\n"
      events = [record "a" "7", record "b" "4", record "a" "3"]
  replay events `shouldBe` Map.fromList [("a", 3), ("b", 4)]
  replay (events ++ [record "a" "-1"]) `shouldBe` Map.fromList [("a", -1), ("b", 4)]


snapshotProtoMetaTest :: IO ()
snapshotProtoMetaTest = do
  let proto =
        D.ActionProtocol
          D.ProtocolAction
            { D.pMinReaderVersion = 1
            , D.pMinWriterVersion = 2
            , D.pReaderFeatures = []
            , D.pWriterFeatures = []
            }
      md =
        D.ActionMetaData
          D.MetaDataAction
            { D.mdId = "u"
            , D.mdName = Just "tbl"
            , D.mdDescription = Nothing
            , D.mdFormat = Nothing
            , D.mdSchemaString = "{\"type\":\"struct\",\"fields\":[]}"
            , D.mdPartitionColumns = []
            , D.mdConfiguration = Map.empty
            , D.mdCreatedTime = Nothing
            }
      snap = D.snapshotFromActions [proto, md]
  D.pMinWriterVersion <$> D.tsProtocol snap `shouldBe` Just 2
  fmap D.mdName (D.tsMetaData snap) `shouldBe` Just (Just "tbl")


-- ============================================================
-- Schema-string decoder
-- ============================================================

schemaTest :: IO ()
schemaTest = case D.parseDeltaSchema schemaJson of
  Left err -> expectationFailure err
  Right s -> do
    let fs = D.dsFields s
    length fs `shouldBe` 4
    let f0 = head fs
    D.dfName f0 `shouldBe` "id"
    D.dfType f0 `shouldBe` D.DTLong
    D.dfNullable f0 `shouldBe` False
    let f1 = fs !! 1
    D.dfType f1 `shouldBe` D.DTString
    let f2 = fs !! 2
    case D.dfType f2 of
      D.DTArray D.DTString _ -> pure ()
      other -> expectationFailure ("unexpected tags type: " ++ show other)
    let f3 = fs !! 3
    D.dfType f3 `shouldBe` D.DTDecimal 10 2
  where
    schemaJson =
      "{\"type\":\"struct\",\"fields\":[\
      \{\"name\":\"id\",\"type\":\"long\",\"nullable\":false,\"metadata\":{}},\
      \{\"name\":\"name\",\"type\":\"string\",\"nullable\":true,\"metadata\":{}},\
      \{\"name\":\"tags\",\"type\":{\"type\":\"array\",\"elementType\":\"string\",\"containsNull\":true},\"nullable\":true,\"metadata\":{}},\
      \{\"name\":\"amount\",\"type\":\"decimal(10,2)\",\"nullable\":true,\"metadata\":{}}\
      \]}"


-- ============================================================
-- _last_checkpoint
-- ============================================================

lastCheckpointTest :: IO ()
lastCheckpointTest = case D.parseLastCheckpoint payload of
  Nothing -> expectationFailure "expected Just"
  Just lc -> do
    D.lcVersion lc `shouldBe` 12
    D.lcSize lc `shouldBe` 100
    D.lcParts lc `shouldBe` Just 4
    D.lcSizeInBytes lc `shouldBe` Just 1024
    D.lcNumOfAddFiles lc `shouldBe` Just 90
  where
    payload = "{\"version\":12,\"size\":100,\"parts\":4,\"sizeInBytes\":1024,\"numOfAddFiles\":90}"


-- ============================================================
-- AddStats
-- ============================================================

addStatsTest :: IO ()
addStatsTest = case decodeAddStats statsJson of
  Just s -> do
    D.asNumRecords s `shouldBe` Just 1000
    HM.lookup "id" (D.asNullCount s) `shouldBe` Just 3
  Nothing -> expectationFailure "expected to decode stats"
  where
    statsJson = "{\"numRecords\":1000,\"minValues\":{\"id\":1},\"maxValues\":{\"id\":999},\"nullCount\":{\"id\":3}}"


decodeAddStats :: BL.ByteString -> Maybe D.AddStats
decodeAddStats bs = case Aeson.decode bs of
  Just v -> case Aeson.fromJSON v of
    Aeson.Success a -> Just a
    Aeson.Error _ -> Nothing
  Nothing -> Nothing


{- | The 'D.decodeAddStats' helper threads the raw @stats@
string through 'D.AddStats' for callers; we just need to
pin its three branches.
-}
decodeAddStatsHelperTest :: IO ()
decodeAddStatsHelperTest = do
  let mkAdd s =
        D.AddAction
          { D.addPath = "x"
          , D.addSize = 0
          , D.addModificationTime = 0
          , D.addDataChange = True
          , D.addStats = s
          , D.addPartitionValues = mempty
          , D.addTags = mempty
          , D.addDeletionVector = Nothing
          }
  -- No stats: Nothing.
  D.decodeAddStats (mkAdd Nothing) `shouldBe` Nothing
  -- Valid JSON: Just (Right ...).
  case D.decodeAddStats (mkAdd (Just "{\"numRecords\":7}")) of
    Just (Right s) -> D.asNumRecords s `shouldBe` Just 7
    other -> expectationFailure ("expected Just (Right ...), got " ++ show other)
  -- Malformed JSON: Just (Left ...).
  case D.decodeAddStats (mkAdd (Just "not-json")) of
    Just (Left _) -> pure ()
    other -> expectationFailure ("expected Just (Left _), got " ++ show other)
