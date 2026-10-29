{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Running streams instances against the in-memory
"Kafka.Streams.Runtime.MockBroker", and waiting on them without
sleeping: every wait advances on the instances' loop ticks.
-}
module Streams.RuntimeHarness (
  testConfig,
  startOn,
  eventually,
  latestBy,
  visible,
  textBytes,
  i64,
  decodeI64,
  withStateDirs,
) where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Kafka.Streams.Imperative
import Kafka.Streams.Runtime.Driver (DriverRecord (..))
import Kafka.Streams.Runtime.MockBroker
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Syd


-- | Fast loop, frequent commits, state under @dir@.
testConfig :: Text -> FilePath -> StreamsConfig
testConfig app dir =
  defaultStreamsConfig
    { applicationId = app
    , bootstrapServers = ["mock:0"]
    , pollMs = 2
    , commitIntervalMs = Just 20
    , stateDir = dir
    }


-- | Start an instance of @topo@ on @broker@.
startOn :: MockBroker -> StreamsConfig -> TopologyValid -> IO KafkaStreams
startOn broker cfg topo = do
  ks <- newKafkaStreams cfg topo
  drv <- mockDriverFor broker cfg
  startKafkaStreamsWith ks drv
  pure ks


{- | Wait until @cond@ holds, advancing on the first instance that is
still live (fails after a bounded number of loop iterations).
-}
eventually :: [KafkaStreams] -> String -> IO Bool -> IO ()
eventually kss what cond = go (8_000 :: Int)
  where
    go 0 = do
      sts <- mapM streamsStatus kss
      expectationFailure ("timed out waiting for " <> what <> "; instances: " <> show sts)
    go n = do
      ok <- cond
      if ok
        then pure ()
        else do
          sts <- mapM streamsStatus kss
          case filter (live . snd) (zip kss sts) of
            [] -> do
              ok' <- cond
              if ok' then pure () else expectationFailure (what <> ": no live instance left; " <> show sts)
            ((ks, _) : _) -> awaitTicks ks 1 >> go (n - 1)
    live = \case
      StreamsRebalancing -> True
      StreamsRunning -> True
      _ -> False


-- | The last value per key in @records@ (in offset order per partition).
latestBy :: [DriverRecord] -> Map ByteString (Maybe ByteString)
latestBy = foldl (\m r -> maybe m (\k -> Map.insert k r.drValue m) r.drKey) Map.empty


visible :: MockBroker -> Text -> IO [DriverRecord]
visible = mockVisibleRecords


textBytes :: Text -> ByteString
textBytes = TE.encodeUtf8


i64 :: Int64 -> ByteString
i64 = serialize int64Serde


decodeI64 :: ByteString -> Int64
decodeI64 = either (error . T.unpack) id . deserialize int64Serde


-- | @n@ fresh state directories.
withStateDirs :: Int -> ([FilePath] -> IO a) -> IO a
withStateDirs n k = withSystemTempDirectory "streams-rt" $ \root ->
  k (map (\i -> root </> show i) [1 .. n])

