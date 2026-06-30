{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Kafka.Telemetry.OpenTelemetry
Description : OpenTelemetry instrumentation for the Kafka client.
Copyright   : (c) 2025
License     : BSD-3-Clause

OpenTelemetry spans for Kafka producer / consumer / transaction
operations, plus context propagation across producer →
consumer hops over Kafka record headers. Built directly on top of
[@hs-opentelemetry-api@](https://hackage.haskell.org/package/hs-opentelemetry-api):
'Tracer's are real 'OTel.Tracer's, spans are real 'OTel.Span's,
and the propagator that ships with whatever 'OTel.TracerProvider'
you configure (W3C Trace Context by default in the SDK) is the
one we delegate to for header inject / extract.

= Producer side

@
import qualified OpenTelemetry.Trace.Core      as OTel
import qualified Kafka.Telemetry.OpenTelemetry as KOTel
import qualified Kafka.Client.Producer         as Producer

main :: IO ()
main = do
  tp <- OTel.getGlobalTracerProvider
  let tr = KOTel.kafkaTracer tp
  Producer.withProducer [\"localhost:9092\"] Producer.defaultProducerConfig $ \\p -> do
    KOTel.inProducerSpan tr \"events\" 0 $ \\sp -> do
      hs <- KOTel.injectIntoProducerHeaders tr sp []
      _  <- Producer.sendMessage p \"events\" Nothing \"hello\"
      pure ()
@

= Consumer side

The handler runs as a child of whatever span sent the record. Pull
the parent context off the record's headers, attach it as the
thread-local context, then open the consumer span — 'inConsumerSpan'
will see it and use it as the parent:

@
import qualified OpenTelemetry.Context.ThreadLocal as OCtxTL

forM_ records $ \\rec -> do
  parent <- KOTel.extractFromConsumerHeaders tr rec.headers
  _ <- OCtxTL.attachContext parent
  KOTel.inConsumerSpan tr rec.topic
    rec.partition rec.offset \"my-group\" $ \\_sp ->
      handle rec
  _ <- OCtxTL.detachContext
  pure ()
@

= Semantic conventions

Span attributes follow the
[OTel messaging semantic conventions](https://opentelemetry.io/docs/specs/semconv/messaging/kafka/):

  * @messaging.system@               = @\"kafka\"@
  * @messaging.destination.name@     = topic name
  * @messaging.operation@            = @\"publish\"@ / @\"process\"@
  * @messaging.kafka.partition@      = partition number
  * @messaging.kafka.message.offset@ = offset (consumer side only)
  * @messaging.kafka.consumer.group@ = consumer-group id (consumer side only)
  * @messaging.kafka.transaction.id@ = transactional id (transaction spans)

= No-op behaviour without an SDK

If the supplied 'OTel.TracerProvider' has no registered span
processors (i.e. no SDK has been initialised), every span created
through this module is a no-op — it costs roughly one allocation
and zero exports. Safe to call unconditionally.
-}
module Kafka.Telemetry.OpenTelemetry (
  -- * Tracer
  kafkaTracer,
  kafkaInstrumentationLibrary,

  -- * Bracketed spans
  inProducerSpan,
  inConsumerSpan,
  inTransactionSpan,

  -- * SpanArguments builders

  --

  {- | Lower-level entry points for callers that want to assemble
  their own 'OTel.inSpan'' wrapper. Each value is a
  'SpanArguments' pre-populated with the OTel messaging
  semantic-convention attributes.
  -}
  producerSpanArguments,
  consumerSpanArguments,
  transactionSpanArguments,

  -- * Context propagation over Kafka record headers
  injectIntoProducerHeaders,
  extractFromConsumerHeaders,
  tracingProducerInterceptor,

  -- * Configuration
  TelemetryConfig (..),
  defaultTelemetryConfig,
) where

import Data.ByteString (ByteString)
import Data.HashMap.Strict qualified as HashMap
import Data.Int (Int32, Int64)
import Data.Text (Text)
#if MIN_VERSION_hs_opentelemetry_api(1,0,0)
import Data.Text qualified as T
#else
import Data.CaseInsensitive qualified as CI
#endif
import Data.Text.Encoding qualified as TE
import GHC.Stack (HasCallStack)
import OpenTelemetry.Attributes qualified as OAttr
import OpenTelemetry.Context qualified as OCtx
import OpenTelemetry.Context.ThreadLocal qualified as OCtxTL
import OpenTelemetry.Propagator qualified as OProp
import OpenTelemetry.Trace.Core (
  Span,
  SpanArguments (..),
  SpanKind (..),
  Tracer,
  TracerProvider,
  defaultSpanArguments,
  tracerOptions,
 )
import OpenTelemetry.Trace.Core qualified as OTel


----------------------------------------------------------------------
-- Configuration
----------------------------------------------------------------------

{- | Side-channel configuration for telemetry emission. Not consulted
by the @inXSpan@ helpers below — they always go through the
'TracerProvider''s configured processors — but exposed so callers
can thread their own flags (e.g. @telemetryIncludePayload@) into
interceptors that read it.
-}
data TelemetryConfig = TelemetryConfig
  { telemetryEnabled :: !Bool
  -- ^ Whether telemetry should be emitted at all. Defaults to 'True'.
  , telemetryServiceName :: !Text
  {- ^ Service name reported on spans / metrics. Defaults to
  @\"wireform-kafka\"@.
  -}
  , telemetryIncludePayload :: !Bool
  {- ^ Whether to copy the record payload into the span as an
  attribute. Defaults to 'False' (privacy-by-default).
  -}
  }


defaultTelemetryConfig :: TelemetryConfig
defaultTelemetryConfig =
  TelemetryConfig
    { telemetryEnabled = True
    , telemetryServiceName = "wireform-kafka"
    , telemetryIncludePayload = False
    }


----------------------------------------------------------------------
-- Tracer
----------------------------------------------------------------------

{- | The 'OTel.InstrumentationLibrary' record used for the
@wireform-kafka@ tracer. Exposed so callers can match against it
in custom processor filters / samplers.
-}
kafkaInstrumentationLibrary :: OTel.InstrumentationLibrary
kafkaInstrumentationLibrary =
  OTel.InstrumentationLibrary
    { OTel.libraryName = "wireform-kafka"
    , OTel.libraryVersion = "0.1.0.0"
    , OTel.librarySchemaUrl = ""
    , OTel.libraryAttributes = OAttr.emptyAttributes
    }


{- | Make a 'Tracer' for the @wireform-kafka@ instrumentation library
from the supplied 'TracerProvider'. Pass the result to
'inProducerSpan', 'inConsumerSpan', and friends.

If the supplied 'TracerProvider' has no registered span
processors every span this tracer creates is a no-op, so this
call is safe to make unconditionally at service-start time even
when no SDK is initialised.
-}
kafkaTracer :: TracerProvider -> Tracer
kafkaTracer tp = OTel.makeTracer tp kafkaInstrumentationLibrary tracerOptions


----------------------------------------------------------------------
-- SpanArguments builders
----------------------------------------------------------------------

{- | 'SpanArguments' pre-populated with the OTel messaging
semantic-convention attributes for a producer @publish@ span.
-}
producerSpanArguments :: Text -> Int32 -> SpanArguments
producerSpanArguments topic partition =
  defaultSpanArguments
    { kind = Producer
    , attributes =
        HashMap.fromList
          [ ("messaging.system", OTel.toAttribute @Text "kafka")
          , ("messaging.destination.name", OTel.toAttribute topic)
          , ("messaging.operation", OTel.toAttribute @Text "publish")
          , ("messaging.kafka.partition", OTel.toAttribute (fromIntegral partition :: Int64))
          ]
    }


-- | 'SpanArguments' for a consumer @process@ span.
consumerSpanArguments :: Text -> Int32 -> Int64 -> Text -> SpanArguments
consumerSpanArguments topic partition offset groupId =
  defaultSpanArguments
    { kind = Consumer
    , attributes =
        HashMap.fromList
          [ ("messaging.system", OTel.toAttribute @Text "kafka")
          , ("messaging.destination.name", OTel.toAttribute topic)
          , ("messaging.operation", OTel.toAttribute @Text "process")
          , ("messaging.kafka.partition", OTel.toAttribute (fromIntegral partition :: Int64))
          , ("messaging.kafka.message.offset", OTel.toAttribute offset)
          , ("messaging.kafka.consumer.group", OTel.toAttribute groupId)
          ]
    }


{- | 'SpanArguments' for a transactional operation. @operation@ is
typically one of @\"init\"@, @\"begin\"@, @\"commit\"@, @\"abort\"@,
@\"send_offsets\"@.
-}
transactionSpanArguments :: Text -> Text -> SpanArguments
transactionSpanArguments operation txnId =
  defaultSpanArguments
    { kind = Client
    , attributes =
        HashMap.fromList
          [ ("messaging.system", OTel.toAttribute @Text "kafka")
          , ("messaging.operation", OTel.toAttribute operation)
          , ("messaging.kafka.transaction.id", OTel.toAttribute txnId)
          ]
    }


----------------------------------------------------------------------
-- inSpan wrappers
----------------------------------------------------------------------

{- | Wrap an 'IO' action in a producer @publish@ span. The span ends
when the action returns (or throws). The body receives the live
'Span' so it can call 'OTel.addAttribute' / 'OTel.addEvent' /
'OTel.recordException' as needed.
-}
inProducerSpan
  :: HasCallStack
  => Tracer
  -> Text
  -- ^ topic
  -> Int32
  -- ^ partition
  -> (Span -> IO a)
  -> IO a
inProducerSpan tr topic partition body =
  OTel.inSpan'
    tr
    ("kafka.publish " <> topic)
    (producerSpanArguments topic partition)
    body


{- | Wrap an 'IO' action in a consumer @process@ span. The span is
a child of the current thread-local 'OCtx.Context'; for a record
that carries a parent trace, call 'extractFromConsumerHeaders'
and 'OCtxTL.attachContext' before this so the parent shows up
correctly.
-}
inConsumerSpan
  :: HasCallStack
  => Tracer
  -> Text
  -- ^ topic
  -> Int32
  -- ^ partition
  -> Int64
  -- ^ offset
  -> Text
  -- ^ consumer group id
  -> (Span -> IO a)
  -> IO a
inConsumerSpan tr topic partition offset groupId body =
  OTel.inSpan'
    tr
    ("kafka.process " <> topic)
    (consumerSpanArguments topic partition offset groupId)
    body


{- | Wrap an 'IO' action in a transactional-operation span. Use the
@operation@ argument to distinguish init / begin / commit / abort
/ send-offsets.
-}
inTransactionSpan
  :: HasCallStack
  => Tracer
  -> Text
  -- ^ operation, e.g. @\"commit\"@
  -> Text
  -- ^ transactional id
  -> (Span -> IO a)
  -> IO a
inTransactionSpan tr operation txnId body =
  OTel.inSpan'
    tr
    ("kafka.txn." <> operation)
    (transactionSpanArguments operation txnId)
    body


----------------------------------------------------------------------
-- Header propagation
----------------------------------------------------------------------

{- | Inject the supplied 'Span''s context into a Kafka producer's
header list. Delegates to whichever propagator the
'TracerProvider' was configured with — the SDK defaults to W3C
Trace Context.

Existing trace-context headers in the input list are overwritten;
unrelated headers pass through unchanged.
-}
injectIntoProducerHeaders
  :: Tracer
  -> Span
  -> [(Text, ByteString)]
  -> IO [(Text, ByteString)]
injectIntoProducerHeaders tr sp headers = do
  let tp = OTel.getTracerTracerProvider tr
      prop = OTel.getTracerProviderPropagators tp
      ctx = OCtx.insertSpan sp OCtx.empty
#if MIN_VERSION_hs_opentelemetry_api(1,0,0)
  -- OTel >= 1.0: inject the trace fields into a fresh 'TextMap' carrier,
  -- then merge them back over the original headers. This overwrites any
  -- existing trace fields and leaves unrelated (possibly binary) headers
  -- untouched — safer than round-tripping every header through 'Text'.
  tm <- OProp.inject prop ctx OProp.emptyTextMap
  let injected = fromTextMap tm
      fields = map T.toLower (OProp.propagatorFields prop)
      keep (k, _) = T.toLower k `notElem` fields
  pure (filter keep headers <> injected)
#else
  -- OTel 0.2: the propagator's carrier is the header list itself
  -- ('Propagator Context RequestHeaders ResponseHeaders'). Inject directly
  -- into the (CI-keyed) header list — the W3C injector overwrites stale
  -- trace fields in place — then convert back to Kafka's 'Text' keys.
  injected <- OProp.inject prop ctx (toRequestHeaders headers)
  pure (fromRequestHeaders injected)
#endif


{- | Extract a parent 'OCtx.Context' from a Kafka consumer record's
headers. The starting context is the current thread-local one so
baggage and any running parent span are preserved when the record
carries no @traceparent@.

Attach the result with 'OCtxTL.attachContext' before calling
'inConsumerSpan' to make the work performed under the record a
child of the upstream producer span.
-}
extractFromConsumerHeaders
  :: Tracer
  -> [(Text, ByteString)]
  -> IO OCtx.Context
extractFromConsumerHeaders tr headers = do
  let tp = OTel.getTracerTracerProvider tr
      prop = OTel.getTracerProviderPropagators tp
  ctx0 <- OCtxTL.getContext
#if MIN_VERSION_hs_opentelemetry_api(1,0,0)
  OProp.extract prop (toTextMap headers) ctx0
#else
  OProp.extract prop (toRequestHeaders headers) ctx0
#endif


{- | Build a pre-send producer interceptor that opens a
@kafka.enqueue@ span, stamps the record's headers with the
resulting trace context, and ends the span before returning. The
span represents the /enqueue/ event only — to instrument the
broker round-trip too, wrap your
'Kafka.Client.Producer.sendMessage' call in 'inProducerSpan'
yourself.
-}
tracingProducerInterceptor
  :: Tracer
  -> Text
  -- ^ topic
  -> Int32
  -- ^ partition
  -> [(Text, ByteString)]
  -- ^ existing headers
  -> IO [(Text, ByteString)]
tracingProducerInterceptor tr topic partition headers =
  OTel.inSpan'
    tr
    ("kafka.enqueue " <> topic)
    (producerSpanArguments topic partition)
    $ \sp ->
      injectIntoProducerHeaders tr sp headers


----------------------------------------------------------------------
-- Internal: Kafka header <-> OTel TextMap conversion
--
-- Kafka record headers are @[(Text, ByteString)]@ pairs. The OTel 1.0
-- propagator API carries a case-insensitive 'OProp.TextMap' of
-- @Text@-valued fields. Trace-context values are ASCII, so a lenient
-- UTF-8 decode of the (small) header set is safe at this boundary.
----------------------------------------------------------------------

#if MIN_VERSION_hs_opentelemetry_api(1,0,0)
toTextMap :: [(Text, ByteString)] -> OProp.TextMap
toTextMap =
  OProp.textMapFromList . map (\(k, v) -> (k, TE.decodeUtf8Lenient v))


fromTextMap :: OProp.TextMap -> [(Text, ByteString)]
fromTextMap =
  map (\(k, v) -> (k, TE.encodeUtf8 v)) . OProp.textMapToList
#else
-- OTel 0.2 carrier conversion. The pre-1.0 propagator carries
-- 'Network.HTTP.Types.RequestHeaders' (@[(CI ByteString, ByteString)]@).
-- Keys are ASCII trace-context field names; values pass through untouched
-- so binary Kafka headers survive the round trip.
toRequestHeaders :: [(Text, ByteString)] -> [(CI.CI ByteString, ByteString)]
toRequestHeaders = map (\(k, v) -> (CI.mk (TE.encodeUtf8 k), v))


fromRequestHeaders :: [(CI.CI ByteString, ByteString)] -> [(Text, ByteString)]
fromRequestHeaders = map (\(k, v) -> (TE.decodeUtf8Lenient (CI.foldedCase k), v))
#endif
