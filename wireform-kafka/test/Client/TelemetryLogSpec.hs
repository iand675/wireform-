{-# LANGUAGE OverloadedStrings #-}

{- | The client's diagnostics go out as OpenTelemetry log records,
through the global logger provider, and cost nothing when no processor
would take them.
-}
module Client.TelemetryLogSpec (tests) where

import Control.Exception (bracket)
import Data.HashMap.Strict qualified as HashMap
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Vector qualified as V
import Kafka.Client.Internal.BatchAccumulator qualified as BA
import Kafka.Client.Internal.ProducerSender qualified as Sender
import Kafka.Client.Metadata qualified as Meta
import Kafka.Compression.Types qualified as Compression
import Kafka.Network.Connection qualified as Conn
import Kafka.Protocol.ApiVersions (createVersionCache)
import Kafka.Protocol.RecordBatch qualified as RB
import Kafka.Telemetry.Log qualified as KLog
import Kafka.Time qualified as KafkaTime
import OpenTelemetry.Internal.Log.Types (ImmutableLogRecord (..), UMaybe (..))
import OpenTelemetry.Log.Core qualified as OTel
import OpenTelemetry.LogAttributes qualified as LA
import OpenTelemetry.Processor.LogRecord (FlushResult (..), LogRecordProcessor (..), ShutdownResult (..))
import Test.Syd


tests :: Spec
tests =
  -- the logger provider is process-global
  sequential $
    describe "Telemetry.Log" $
      sequence_
        [ it "emits a record with severity, body, scope and attributes" emits
        , it "builds nothing below the provider's minimum severity" filtered
        , it "the producer's retry warning carries the partition" sender_retry
        ]


-- | What a captured record says.
data Seen = Seen
  { seenScope :: Text
  , seenSeverity :: Maybe OTel.SeverityNumber
  , seenBody :: OTel.AnyValue
  , seenAttrs :: HashMap.HashMap Text OTel.AnyValue
  }


-- | Run with a global provider whose one processor records every emit.
withCapture :: (OTel.LoggerProvider -> IORef [Seen] -> IO a) -> IO a
withCapture k = do
  seen <- newIORef []
  let processor =
        LogRecordProcessor
          { logRecordProcessorOnEmit = \lr _ -> do
              r <- OTel.readLogRecord lr
              let !s =
                    Seen
                      { seenScope = OTel.libraryName (OTel.readLogRecordInstrumentationScope lr)
                      , seenSeverity = case logRecordSeverityNumber r of
                          UJust sev -> Just sev
                          UNothing -> Nothing
                      , seenBody = logRecordBody r
                      , seenAttrs = snd (LA.getAttributeMap (logRecordAttributes r))
                      }
              atomicModifyIORef' seen (\xs -> (s : xs, ()))
          , logRecordProcessorShutdown = pure ShutdownSuccess
          , logRecordProcessorForceFlush = pure FlushSuccess
          }
  lp <- OTel.createLoggerProvider [processor] OTel.emptyLoggerProviderOptions
  bracket OTel.getGlobalLoggerProvider OTel.setGlobalLoggerProvider $ \_ -> do
    OTel.setGlobalLoggerProvider lp
    k lp seen


emits :: IO ()
emits = withCapture $ \_ seen -> do
  KLog.logWarn KLog.kafkaInstrumentationLibrary "test: something happened" [KLog.attrText "k" "v", KLog.attrInt "n" (3 :: Int)]
  rs <- filter ((== OTel.TextValue "test: something happened") . seenBody) <$> readIORef seen
  map seenScope rs `shouldBe` ["wireform-kafka"]
  map seenSeverity rs `shouldBe` [Just OTel.Warn]
  map (HashMap.lookup "k" . seenAttrs) rs `shouldBe` [Just (OTel.TextValue "v")]
  map (HashMap.lookup "n" . seenAttrs) rs `shouldBe` [Just (OTel.IntValue 3)]


filtered :: IO ()
filtered = withCapture $ \lp seen -> do
  OTel.setLoggerMinSeverity lp (Just OTel.Error)
  -- neither the message nor the attributes may be forced
  KLog.logWarn KLog.kafkaInstrumentationLibrary (error "message built") [error "attribute built"]
  KLog.logError KLog.streamsInstrumentationLibrary "test: kept" []
  rs <- readIORef seen
  map seenScope rs `shouldBe` ["wireform-kafka-streams"]


sender_retry :: IO ()
sender_retry = withCapture $ \_ seen -> do
  acc <- BA.createBatchAccumulator 16384 0 Compression.NoCompression (Compression.defaultLevel Compression.NoCompression)
  meta <- Meta.createMetadataCache
  cm <- Conn.createConnectionManager
  vc <- createVersionCache
  st <- Sender.createSenderState acc meta cm Conn.defaultConnectionConfig Sender.defaultRetryConfig (-1) 60000 Compression.NoCompression "t" vc 5
  now <- KafkaTime.currentTimeMillis
  let batch =
        BA.ProducerBatch
          { BA.batchTopicPartition = BA.TopicPartition "orders" 3
          , BA.batchRecords = V.empty
          , BA.batchSizeBytes = 0
          , BA.batchCreateTime = now
          , BA.batchBaseTimestamp = 0
          , BA.batchState = BA.Filling
          , BA.batchCompression = Compression.NoCompression
          , BA.batchCompressionLevel = Compression.defaultLevel Compression.NoCompression
          , BA.batchCallbacks = V.empty
          , BA.batchAttempts = 0
          , BA.batchProducerId = RB.noProducerId
          , BA.batchProducerEpoch = RB.noProducerEpoch
          , BA.batchBaseSequence = RB.noSequence
          , BA.batchIsTransactional = False
          }
  Sender.retryOrFail st "NOT_LEADER_OR_FOLLOWER" batch
  rs <- filter ((== Just (OTel.TextValue "orders")) . HashMap.lookup "messaging.destination.name" . seenAttrs) <$> readIORef seen
  map seenSeverity rs `shouldBe` [Just OTel.Warn]
  map (HashMap.lookup "messaging.destination.partition.id" . seenAttrs) rs `shouldBe` [Just (OTel.TextValue "3")]
  map (HashMap.lookup "messaging.system" . seenAttrs) rs `shouldBe` [Just (OTel.TextValue "kafka")]
