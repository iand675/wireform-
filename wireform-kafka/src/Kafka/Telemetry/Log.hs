{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Telemetry.Log
Description : The client's diagnostics, as OpenTelemetry log records.

Everything the Kafka client and the Streams runtime have to say (a
retried batch, a failed heartbeat, a reconnect, an aborted commit) is
emitted as an OpenTelemetry log record through the global
'OTel.LoggerProvider', under the instrumentation scope of the
component that said it ('kafkaInstrumentationLibrary' for the client,
'streamsInstrumentationLibrary' for Kafka Streams). Nothing is
written to stdout or stderr.

With no SDK installed the global provider has no processors and every
call here is a no-op: one 'IORef' read, and the message and its
attributes are never built. Install an SDK logger provider (and its
exporter of choice) to collect them, e.g. with @hs-opentelemetry-sdk@:

@
import OpenTelemetry.Log.Core (setGlobalLoggerProvider)

main = do
  lp <- ... -- an SDK LoggerProvider with a batch processor + OTLP exporter
  setGlobalLoggerProvider lp
  ...
@

Records carry the active span's context (the one on the calling
thread), so a log line emitted inside a traced operation correlates
with its span. Attribute names follow the OpenTelemetry semantic
conventions where one applies (@messaging.*@, @server.*@,
@error.type@).
-}
module Kafka.Telemetry.Log (
  -- * Emitting
  logWith,
  logDebug,
  logInfo,
  logWarn,
  logError,

  -- * Scopes
  kafkaInstrumentationLibrary,
  streamsInstrumentationLibrary,

  -- * Attributes
  Attr,
  attrText,
  attrInt,
  attrShow,
  topicAttrs,
  serverAttrs,
  errorAttr,

  -- * Re-exports
  OTel.SeverityNumber (..),
) where

import Control.Monad (when)
import Data.HashMap.Strict qualified as HashMap
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Telemetry.OpenTelemetry (kafkaInstrumentationLibrary)
import OpenTelemetry.Attributes qualified as OAttr
import OpenTelemetry.Log.Core qualified as OTel


-- | One attribute of a log record.
type Attr = (Text, OTel.AnyValue)


-- | The instrumentation scope of the Kafka Streams runtime.
streamsInstrumentationLibrary :: OTel.InstrumentationLibrary
streamsInstrumentationLibrary =
  OTel.InstrumentationLibrary
    { OTel.libraryName = "wireform-kafka-streams"
    , OTel.libraryVersion = "0.1.0.0"
    , OTel.librarySchemaUrl = ""
    , OTel.libraryAttributes = OAttr.emptyAttributes
    }


{- | Emit @message@ at @severity@ under @scope@, through the global
logger provider. Nothing is built or sent unless a processor would
take a record of that severity.
-}
logWith :: OTel.InstrumentationLibrary -> OTel.SeverityNumber -> Text -> [Attr] -> IO ()
logWith scope severity message attrs = do
  lp <- OTel.getGlobalLoggerProvider
  let logger = OTel.makeLogger lp scope
  enabled <- OTel.loggerIsEnabled logger (Just severity) Nothing
  when enabled $ do
    _ <-
      OTel.emitLogRecord logger $
        OTel.LogRecordArguments
          { OTel.timestamp = Nothing
          , OTel.observedTimestamp = Nothing
          , OTel.context = Nothing
          , OTel.severityText = OTel.toShortName severity
          , OTel.severityNumber = Just severity
          , OTel.body = OTel.TextValue message
          , OTel.attributes = HashMap.fromList attrs
          , OTel.eventName = Nothing
          }
    pure ()
{-# INLINE logWith #-}


-- | 'logWith' at a fixed severity.
logDebug, logInfo, logWarn, logError :: OTel.InstrumentationLibrary -> Text -> [Attr] -> IO ()
logDebug scope = logWith scope OTel.Debug
logInfo scope = logWith scope OTel.Info
logWarn scope = logWith scope OTel.Warn
logError scope = logWith scope OTel.Error


attrText :: Text -> Text -> Attr
attrText k v = (k, OTel.TextValue v)


attrInt :: Integral a => Text -> a -> Attr
attrInt k v = (k, OTel.IntValue (fromIntegral v :: Int64))


-- | An attribute rendered with 'show' (exceptions, error values).
attrShow :: Show a => Text -> a -> Attr
attrShow k v = (k, OTel.TextValue (T.pack (show v)))


-- | @messaging.system@, @messaging.destination.name@ and the partition.
topicAttrs :: Text -> Int64 -> [Attr]
topicAttrs topic partition =
  [ attrText "messaging.system" "kafka"
  , attrText "messaging.destination.name" topic
  , attrText "messaging.destination.partition.id" (T.pack (show partition))
  ]


-- | @server.address@ / @server.port@ of a broker.
serverAttrs :: String -> Int -> [Attr]
serverAttrs host port = [attrText "server.address" (T.pack host), attrInt "server.port" port]


-- | @error.type@.
errorAttr :: Text -> Attr
errorAttr = attrText "error.type"
