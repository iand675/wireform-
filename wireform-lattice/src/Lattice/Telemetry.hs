{- | OpenTelemetry instrumentation for the Lattice origin (spec §19).

This is the only module in the package that imports @hs-opentelemetry-api@;
every instrumentation touch-point ("Lattice.Server", "Lattice.Server.Execute",
"Lattice.Server.Coalesce") goes through the helpers here so the OTel surface
stays in one place and the disabled path stays cheap.

== The handle

'LatticeTelemetry' bundles the origin's tracer with the ten §19.3
instruments. 'noTelemetry' (the 'Lattice.Server.OriginConfig' default) is a
no-op: no tracer, no-op instruments, and every helper fast-paths on
'telemetryEnabled' before building attributes — zero overhead when
telemetry is off. 'newLatticeTelemetry' builds a live handle from a
'TracerProvider' and 'MeterProvider' (install @hs-opentelemetry-sdk@
providers in production; tests use 'createTracerProvider' with an
in-memory 'SpanProcessor' — see the recipe below).

== Span topology (§19.2)

Server spans are named by route template only (@GET \/q\/{hash}@,
@POST \/m\/{name}@, …) — never a concrete hash or key; those go in
attributes (@lattice.query.hash@ is an attribute, not a name). Children:
@lattice.compile@ (memo miss only), @lattice.execute@ (one per slice
execution), @lattice.round[i]@, @lattice.load@ (one per loader invocation,
with @lattice.loader.name@ \/ @lattice.loader.batch_size@). Scoped errors
surface as span EVENTS named @lattice.error@ carrying
@lattice.error.scope@ \/ @lattice.error.code@ (never the entity id).

== Context propagation (§19.1)

Inbound @traceparent@ is honored ('withServerSpan'); @tracestate@ is
currently not parsed into the remote context (no W3C propagator package in
the build; the trace\/span ids — what linking needs — propagate). Relay
work links to the originating mutation via the span context carried on
'Lattice.Server.InvalEvent' ('withLatticeSpan' link targets). Nothing
sensitive ever enters baggage: this module never touches baggage at all,
and no vc\/claims\/proof value is ever passed to a recorder.

§19.4: 'traceresponseValue' renders the @traceresponse@ header value;
"Lattice.Server" attaches it ONLY to uncacheable responses (priv,
mutations, oneshots) — shared-cacheable responses never carry it.

== Metrics (§19.3)

All ten instruments are registered by 'newLatticeTelemetry' with their
exact spec names. Instruments the origin cannot fully observe today:

* @lattice.plan.supersessions@ — counted on the origin's
  @lattice:plan-superseded@ response path.
* @lattice.convergence.retries@ — a client-side signal (cross-slice
  assembly); the Haskell client does not yet drive a 'LatticeTelemetry'
  handle, so the counter is registered and 'countConvergenceRetry' is
  exported for that wiring, but nothing records it yet.

Derived-field /hidden/ loads ("Lattice.Server.Execute" §3.7 batches)
record @lattice.loader.batch_size@ but do not open per-load spans; the
visible traversal's rounds-×-loaders span bound is unaffected.

== Test capture recipe

Spans — hand an in-memory processor to the API's own provider, then read
each ended span's final state out of its 'spanHot' ref:

@
ref <- newIORef []
let processor = SpanProcessor
      { spanProcessorOnStart = \\_ _ -> pure ()
      , spanProcessorOnEnd = \\sp -> modifyIORef' ref (sp :)
      , spanProcessorShutdown = pure ShutdownSuccess
      , spanProcessorForceFlush = pure FlushSuccess
      }
tp <- createTracerProvider [processor] emptyTracerProviderOptions
tel <- newLatticeTelemetry tp noopMeterProvider   -- spans only
…
spans <- readIORef ref
hots  <- traverse (readIORef . spanHot) spans     -- 'hotName', 'hotAttributes', 'hotEvents'
@

Parent\/child structure: 'spanParent' on 'ImmutableSpan'; link targets:
'hotLinks'. Metrics — the API package has no aggregating 'MeterProvider';
tests install a tiny collecting one (a 'MeterProvider' whose
'OpenTelemetry.Metric.Core.meterProviderGetMeter' returns a 'Meter' with
TVar-backed instruments) and assert the recorded (name, value, attribute)
triples — the same pattern as @wireform-kafka@'s @ObservabilityOTelSpec@.
The needed types are re-exported here so tests need no direct
@hs-opentelemetry-api@ dependency.
-}
module Lattice.Telemetry (
  -- * The telemetry handle
  LatticeTelemetry (..),
  newLatticeTelemetry,
  noTelemetry,
  telemetryEnabled,
  latticeInstrumentationScope,

  -- * Spans
  withLatticeSpan,
  withServerSpan,
  addSpanAttrs,
  addActiveSpanAttrs,
  errorEvent,
  activeSpanContext,
  traceresponseValue,

  -- * Attribute values
  Attr,
  txtA,
  intA,
  boolA,

  -- * Metric recorders (all no-ops when disabled)
  recordLoaderBatch,
  recordCoalesceWait,
  recordCompileDuration,
  recordPurgeFanout,
  recordInvalidationLag,
  recordDerivationLag,
  countTenurePromotion,
  countPlanSupersession,
  countMutationReplay,
  countConvergenceRetry,

  -- * Timing
  elapsedMs,

  -- * Re-exports for origin wiring and test capture
  Span,
  SpanContext (..),
  SpanKind (..),
  Tracer,
  TracerProvider,
  createTracerProvider,
  emptyTracerProviderOptions,
  SpanProcessor (..),
  ImmutableSpan (..),
  SpanHot (..),
  Event (..),
  Link (..),
  getSpanContext,
  appendOnlyBoundedCollectionValues,
  ShutdownResult (..),
  FlushResult (..),
  MeterProvider (..),
  Meter (..),
  Histogram (..),
  Counter (..),
  noopMeterProvider,
  Attributes,
  lookupAttribute,
  Attribute (..),
  PrimitiveAttribute (..),
) where

import Control.Exception (finally)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.Foldable (for_)
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.Text (Text)
import GHC.Clock (getMonotonicTimeNSec)
import OpenTelemetry.Attributes (
  Attribute (..),
  Attributes,
  PrimitiveAttribute (..),
  ToAttribute (toAttribute),
  emptyAttributes,
  lookupAttribute,
  unsafeAttributesFromListIgnoringLimits,
 )
import OpenTelemetry.Attributes.Map qualified as AttrMap
import OpenTelemetry.Processor.Span (FlushResult (..), ShutdownResult (..), SpanProcessor (..))
import OpenTelemetry.Context qualified as Ctx
import OpenTelemetry.Context.ThreadLocal (adjustContext, getContext)
import OpenTelemetry.Metric.Core (
  Counter (..),
  Histogram (..),
  Meter (..),
  MeterProvider (..),
  defaultAdvisoryParameters,
  getMeter,
  noopMeterProvider,
 )
import OpenTelemetry.Trace.Core (
  Event (..),
  Link (..),
  ImmutableSpan (..),
  InstrumentationLibrary (..),
  NewLink (..),
  Span,
  SpanContext (..),
  SpanHot (..),
  SpanKind (..),
  Tracer,
  TracerProvider,
  addAttribute,
  addEvent,
  createSpanWithoutCallStack,
  createTracerProvider,
  defaultSpanArguments,
  emptyTracerProviderOptions,
  endSpan,
  getActiveSpan,
  getSpanContext,
  makeTracer,
  newEventWith,
  traceFlagsFromWord8,
  traceFlagsValue,
  tracerOptions,
  wrapSpanContext,
 )
import OpenTelemetry.Trace.Core qualified as OTel
import OpenTelemetry.Trace.Id (decodeTraceparent, encodeTraceparent)
import OpenTelemetry.Util (appendOnlyBoundedCollectionValues)
import OpenTelemetry.Trace.TraceState qualified as TraceState


-- | An OTel attribute value; construct via 'txtA' \/ 'intA' \/ 'boolA' so
-- call sites never import @hs-opentelemetry-api@.
type Attr = Attribute


txtA :: Text -> Attr
txtA = toAttribute


intA :: Int -> Attr
intA n = toAttribute (fromIntegral n :: Int64)


boolA :: Bool -> Attr
boolA = toAttribute


{- | The tracer plus the ten §19.3 instruments. 'ltTracer' doubles as the
enabled flag: 'noTelemetry' carries 'Nothing' and no-op instruments.
-}
data LatticeTelemetry = LatticeTelemetry
  { ltTracer :: Maybe Tracer
  , ltLoaderBatchSize :: Histogram
  -- ^ @lattice.loader.batch_size@ — the N+1 regression detector.
  , ltCoalesceWait :: Histogram
  -- ^ @lattice.coalesce.wait@ — µs a point fetch spent in the window (§6.9).
  , ltCompileDuration :: Histogram
  -- ^ @lattice.compile.duration@ — ms, cold-path cost; rejections attributed.
  , ltPurgeFanout :: Histogram
  -- ^ @lattice.purge.fanout@ — keys per published purge batch.
  , ltInvalidationLag :: Histogram
  -- ^ @lattice.invalidation.lag@ — ms, outbox commit to CDN purge hook return.
  , ltDerivationLag :: Histogram
  -- ^ @lattice.derivation.lag@ — ms, outbox trigger to maintained-value commit.
  , ltTenurePromotions :: Counter Int64
  -- ^ @lattice.tenure.promotions@.
  , ltPlanSupersessions :: Counter Int64
  -- ^ @lattice.plan.supersessions@.
  , ltMutationReplays :: Counter Int64
  -- ^ @lattice.mutation.replays@ — by mutation and effect class.
  , ltConvergenceRetries :: Counter Int64
  -- ^ @lattice.convergence.retries@ — registered; client-side (module haddock).
  }


-- | The cheap guard every recorder checks first.
telemetryEnabled :: LatticeTelemetry -> Bool
telemetryEnabled = isJust . ltTracer


-- | The instrumentation scope all Lattice spans and metrics report under.
latticeInstrumentationScope :: InstrumentationLibrary
latticeInstrumentationScope =
  InstrumentationLibrary
    { libraryName = "wireform-lattice"
    , libraryVersion = "0.1.0.0"
    , librarySchemaUrl = ""
    , libraryAttributes = emptyAttributes
    }


-- | Build a live handle: one tracer, the ten instruments, exact §19.3 names.
newLatticeTelemetry :: TracerProvider -> MeterProvider -> IO LatticeTelemetry
newLatticeTelemetry tp mp = do
  meter <- getMeter mp latticeInstrumentationScope
  let hist name unit desc =
        meterCreateHistogram meter name (Just unit) (Just desc) defaultAdvisoryParameters
      cnt name desc =
        meterCreateCounterInt64 meter name Nothing (Just desc) defaultAdvisoryParameters
  LatticeTelemetry (Just (makeTracer tp latticeInstrumentationScope tracerOptions))
    <$> hist "lattice.loader.batch_size" "{key}" "keys per loader invocation (N+1 detector)"
    <*> hist "lattice.coalesce.wait" "us" "time a point fetch spent in the coalescing window"
    <*> hist "lattice.compile.duration" "ms" "cold-path compile cost"
    <*> hist "lattice.purge.fanout" "{key}" "surrogate keys per published purge batch"
    <*> hist "lattice.invalidation.lag" "ms" "outbox commit to CDN purge acknowledgment"
    <*> hist "lattice.derivation.lag" "ms" "outbox trigger to maintained-value commit"
    <*> cnt "lattice.tenure.promotions" "hash-form queries promoted to long cache tenure"
    <*> cnt "lattice.plan.supersessions" "responses answered plan-superseded"
    <*> cnt "lattice.mutation.replays" "idempotency-key replays, by mutation and effect class"
    <*> cnt "lattice.convergence.retries" "client cross-slice convergence retries"


-- | The default: everything is a no-op and 'telemetryEnabled' is 'False'.
noTelemetry :: LatticeTelemetry
noTelemetry =
  LatticeTelemetry
    { ltTracer = Nothing
    , ltLoaderBatchSize = noHist
    , ltCoalesceWait = noHist
    , ltCompileDuration = noHist
    , ltPurgeFanout = noHist
    , ltInvalidationLag = noHist
    , ltDerivationLag = noHist
    , ltTenurePromotions = noCnt
    , ltPlanSupersessions = noCnt
    , ltMutationReplays = noCnt
    , ltConvergenceRetries = noCnt
    }
  where
    noHist = Histogram {histogramRecord = \_ _ -> pure (), histogramEnabled = pure False}
    noCnt = Counter {counterAdd = \_ _ -> pure (), counterEnabled = pure False}


-- ---------------------------------------------------------------------------
-- Spans
-- ---------------------------------------------------------------------------

{- | Run an action inside a span parented on the thread-local context (the
enclosing 'withLatticeSpan' \/ 'withServerSpan'). Disabled telemetry runs
the action directly with 'Nothing'. The span is always ended and the
thread-local context restored, exceptions included.
-}
withLatticeSpan ::
  LatticeTelemetry ->
  Text ->
  SpanKind ->
  [SpanContext] ->
  -- ^ Link targets (§19.1: relay work LINKS, never parents).
  [(Text, Attr)] ->
  (Maybe Span -> IO a) ->
  IO a
withLatticeSpan tel name k linkCtxs attrs act = case ltTracer tel of
  Nothing -> act Nothing
  Just tracer -> do
    parent <- getContext
    sp <-
      createSpanWithoutCallStack
        tracer
        parent
        name
        defaultSpanArguments
          { OTel.kind = k
          , OTel.links = map (\sc -> NewLink {linkContext = sc, linkAttributes = mempty}) linkCtxs
          }
    for_ attrs (uncurry (addAttribute sp))
    adjustContext (Ctx.insertSpan sp)
    act (Just sp) `finally` (endSpan sp Nothing *> adjustContext (const parent))


{- | The request-level server span (§19.2): named by route template, kind
'Server', parented on the inbound @traceparent@ when one is presented
(@tracestate@ is not parsed — module haddock).
-}
withServerSpan ::
  LatticeTelemetry ->
  Maybe ByteString ->
  -- ^ Raw @traceparent@ header value, if any.
  Text ->
  -- ^ Route template — never a concrete hash\/key.
  [(Text, Attr)] ->
  (Maybe Span -> IO a) ->
  IO a
withServerSpan tel traceparent name attrs act = case ltTracer tel of
  Nothing -> act Nothing
  Just tracer -> do
    base <- getContext
    let parent = case remoteContext =<< traceparent of
          Nothing -> base
          Just sc -> Ctx.insertSpan (wrapSpanContext sc) base
    sp <-
      createSpanWithoutCallStack tracer parent name defaultSpanArguments {OTel.kind = Server}
    for_ attrs (uncurry (addAttribute sp))
    adjustContext (Ctx.insertSpan sp)
    act (Just sp) `finally` (endSpan sp Nothing *> adjustContext (const base))


-- | Decode a W3C @traceparent@ value into a remote parent 'SpanContext'.
remoteContext :: ByteString -> Maybe SpanContext
remoteContext raw = do
  (_version, tid, sid, flags) <- decodeTraceparent raw
  pure
    SpanContext
      { traceFlags = traceFlagsFromWord8 flags
      , isRemote = True
      , traceId = tid
      , spanId = sid
      , traceState = TraceState.empty
      }


-- | Add attributes to a (possibly absent) span.
addSpanAttrs :: Maybe Span -> [(Text, Attr)] -> IO ()
addSpanAttrs Nothing _ = pure ()
addSpanAttrs (Just sp) attrs = for_ attrs (uncurry (addAttribute sp))


{- | Add attributes to whatever span is active on this thread — how deep
call sites (e.g. the query pipeline discovering @lattice.plan.id@ after
routing already opened the server span) enrich the enclosing span.
-}
addActiveSpanAttrs :: LatticeTelemetry -> [(Text, Attr)] -> IO ()
addActiveSpanAttrs tel attrs =
  when (telemetryEnabled tel) $ do
    msp <- getActiveSpan
    addSpanAttrs msp attrs


{- | A @lattice.error@ span event carrying @lattice.error.scope@ (the
scope's @$tag@) and @lattice.error.code@ (§19.2) — never the entity\/item
identifier itself.
-}
errorEvent :: Maybe Span -> Maybe Text -> Maybe Text -> IO ()
errorEvent Nothing _ _ = pure ()
errorEvent (Just sp) scopeTag code =
  addEvent sp . newEventWith "lattice.error" . AttrMap.fromList . mconcat $
    [ maybe [] (\t -> [("lattice.error.scope", txtA t)]) scopeTag
    , maybe [] (\c -> [("lattice.error.code", txtA c)]) code
    ]


-- | The active span's context, for stamping onto 'Lattice.Server.InvalEvent'.
activeSpanContext :: LatticeTelemetry -> IO (Maybe SpanContext)
activeSpanContext tel
  | telemetryEnabled tel = do
      msp <- getActiveSpan
      traverse getSpanContext msp
  | otherwise = pure Nothing


{- | Render the W3C @traceresponse@ header value for a span. §19.4:
attach ONLY to uncacheable responses.
-}
traceresponseValue :: Span -> IO ByteString
traceresponseValue sp = do
  sc <- getSpanContext sp
  pure (encodeTraceparent 0 (traceId sc) (spanId sc) (traceFlagsValue (traceFlags sc)))


-- ---------------------------------------------------------------------------
-- Metric recorders
-- ---------------------------------------------------------------------------

mkAttrs :: [(Text, Attr)] -> Attributes
mkAttrs = unsafeAttributesFromListIgnoringLimits


whenOn :: LatticeTelemetry -> IO () -> IO ()
whenOn tel = when (telemetryEnabled tel)
{-# INLINE whenOn #-}


-- | @lattice.loader.batch_size@, attributed with @lattice.loader.name@.
recordLoaderBatch :: LatticeTelemetry -> Text -> Int -> IO ()
recordLoaderBatch tel loader n =
  whenOn tel $
    histogramRecord
      (ltLoaderBatchSize tel)
      (fromIntegral n)
      (mkAttrs [("lattice.loader.name", txtA loader)])


-- | @lattice.coalesce.wait@ (µs), attributed with the window's type.
recordCoalesceWait :: LatticeTelemetry -> Text -> Int -> IO ()
recordCoalesceWait tel ty micros =
  whenOn tel $
    histogramRecord
      (ltCoalesceWait tel)
      (fromIntegral micros)
      (mkAttrs [("lattice.loader.name", txtA ty)])


-- | @lattice.compile.duration@ (ms), attributed with the rejection outcome.
recordCompileDuration :: LatticeTelemetry -> Double -> Bool -> IO ()
recordCompileDuration tel ms rejected =
  whenOn tel $
    histogramRecord
      (ltCompileDuration tel)
      ms
      (mkAttrs [("lattice.compile.rejected", boolA rejected)])


-- | @lattice.purge.fanout@: keys per published purge batch.
recordPurgeFanout :: LatticeTelemetry -> Int -> IO ()
recordPurgeFanout tel n =
  whenOn tel $ histogramRecord (ltPurgeFanout tel) (fromIntegral n) (mkAttrs [])


-- | @lattice.invalidation.lag@ (ms).
recordInvalidationLag :: LatticeTelemetry -> Double -> IO ()
recordInvalidationLag tel ms =
  whenOn tel $ histogramRecord (ltInvalidationLag tel) ms (mkAttrs [])


-- | @lattice.derivation.lag@ (ms), attributed with @lattice.derivation.name@.
recordDerivationLag :: LatticeTelemetry -> Text -> Double -> IO ()
recordDerivationLag tel deriv ms =
  whenOn tel $
    histogramRecord
      (ltDerivationLag tel)
      ms
      (mkAttrs [("lattice.derivation.name", txtA deriv)])


countTenurePromotion :: LatticeTelemetry -> IO ()
countTenurePromotion tel =
  whenOn tel $ counterAdd (ltTenurePromotions tel) 1 (mkAttrs [])


countPlanSupersession :: LatticeTelemetry -> IO ()
countPlanSupersession tel =
  whenOn tel $ counterAdd (ltPlanSupersessions tel) 1 (mkAttrs [])


-- | @lattice.mutation.replays@ by mutation name and effect class.
countMutationReplay :: LatticeTelemetry -> Text -> Text -> IO ()
countMutationReplay tel name effectClass =
  whenOn tel $
    counterAdd
      (ltMutationReplays tel)
      1
      ( mkAttrs
          [ ("lattice.mutation.name", txtA name)
          , ("lattice.effect_class", txtA effectClass)
          ]
      )


-- | @lattice.convergence.retries@ (client-side; module haddock).
countConvergenceRetry :: LatticeTelemetry -> IO ()
countConvergenceRetry tel =
  whenOn tel $ counterAdd (ltConvergenceRetries tel) 1 (mkAttrs [])


-- ---------------------------------------------------------------------------
-- Timing
-- ---------------------------------------------------------------------------

-- | Run an action, returning its result and wall-clock milliseconds.
elapsedMs :: IO a -> IO (a, Double)
elapsedMs act = do
  t0 <- getMonotonicTimeNSec
  r <- act
  t1 <- getMonotonicTimeNSec
  pure (r, fromIntegral (t1 - t0) / 1e6)
