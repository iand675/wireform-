{-# LANGUAGE PatternSynonyms #-}
{- | The Lattice origin: an HTTP handler for "Network.HTTP.Server" serving
discovery, schema documents, queries (hash\/inline\/QUERY\/POST forms),
point fetches, and mutations over a deployment-supplied
'Lattice.Backend.Backend' (spec: @website\/src\/content\/docs\/lattice\/spec.md@).

The handler is stateless per request; 'Origin' carries the shared caches
(query memo, idempotency store, tenure counters), all advisory and
rebuildable from traffic.

== Buffering

Responses are fully materialized before headers are committed —
correctness first: degradation is always known before the status line, so
@207 Multi-Status@ and @Lattice-Outcome@ are exact rather than
opportunistic (spec §9.4.6 SHOULD), and @If-None-Match@ can compare the
manifest etag without a validator memo. Chunked NDJSON streaming (and the
early-hints \/ deferral machinery that rides on it) is deliberately
deferred.

== Decisions where the design doc is silent (and accepted deviations)

* __Header rendering.__ @Lattice-Plan@, @Lattice-Schema@, @Lattice-Outcome@
  and @Surrogate-Key@ members are emitted as bare RFC 8941 sf-tokens
  (matching the spec's inline examples); @Lattice-Snapshot@ is the
  dictionary @domain=\"token\"@. @ETag@ uses standard HTTP quoting (weak
  @W\/\"m:…\"@ for query responses, strong @\"ver\"@ for point fetches).
* __Default slice.__ A @\/q@ GET with no @slice@ parameter serves the plan
  pseudo-slice (§6.6); QUERY\/POST introductions default to @pub@ (their
  spec examples return data).
* __Minor problem codes__ beyond the spec table, all
  @https:\/\/lattice.dev\/problems\/{code}@: @lattice:forbidden@ (mutation
  guard\/denial, 403), @lattice:not-found@ (unrouted paths, unknown
  types\/mutations, vanished entities, 404), @lattice:unknown-plan@
  (@\/q\/{hash}\/plan\/{planId}@ for a plan this instance cannot derive,
  404), @lattice:unsupported-media@ (415 on non-@application\/x-lattice-query@
  introduction bodies).
* __priv admission__ accepts any @Authorization@ header as the principal
  (the deployment glue point); a @vc@ parameter presented alongside is
  decoded (and proof-checked when a verifier is configured) so gated
  edges can evaluate their predicates, but claims never join the priv
  cache key.
* __Mutations.__ The output selection is the entire visible field set of
  the returned type at the caller's level, including bounded edges,
  excluding paginated edges (the IDL declares no output selection).
  Batch manifests use @root: {\"result\": […]}@ with per-record @item@
  tags (the manifest root map is typed @Map Text [Ref]@, so item keys
  cannot stand in the root map as the spec example sketches).
  @AllOrNothing@ batches call 'beMutate' per item and abort at the first
  failure with a single unscoped error and no partial records — true
  atomicity (and rollback after a write-scope violation) is the
  transactional backend's obligation. A write-scope violation on a
  __singular__ mutation is @500 lattice:write-scope@ per the design; on a
  __batch item__ it is an @Item@-scoped @lattice:write-scope@ error record
  so sibling items' verdicts still reach the client. Replays of a
  completed @Idempotency-Key@ return the stored response with
  @Idempotency-Replayed: true@.
* __Verb bindings (§11.7\/§11.8).__ A bound PUT decodes its body into the
  mutation's single non-key argument (the full replacement value); PATCH
  decodes an @application\/x-lattice-merge-patch@ object into the input
  record argument (unknown fields 400, wrong content type 415); DELETE
  takes no body. Conditional headers are supported exactly as pinned:
  @If-Match: \"ver\"@ (single, strong) and @If-None-Match: *@ on PUT;
  anything else is 400. Per §15 the 412\/428 responses carry no
  @lattice:@ code: 428 is an @about:blank@ problem, 412 an ordinary
  entity stream with current state. Preconditions reach the backend via
  'MutatePrecondition' and are evaluated in the effect bracket; the named
  @POST \/m\/{name}@ form of a bound mutation ignores conditional headers
  (the binding chooses the wire spelling only), and collection (batch)
  forms reject them (items carry their own keys). A 412 is never stored
  as an idempotency replay (a refused precondition is not an acceptance).
  Creation POST answers 201 + @Location@ only when the effect reports a
  result ref of the bound type; batch creation answers the ordinary
  batch stream.
* __Point fetches.__ With neither @fragment@ nor @f@, the mask is every
  field at or below the caller's presented level (anonymous → @pub@).
  Fragment masks take top-level fields and matching inline fragments;
  nested schema-fragment spreads are skipped, and paginated edges inside
  a fragment are skipped (an @f=@ mask naming one is rejected 400).
  Tombstones answer @410@ with the tombstone record in the ordinary
  NDJSON frame and the same cache policy as a success (deletion is a
  stable fact). A version-pinned fetch of a tombstoned or elided row is
  @404 lattice:version-unavailable@.
* __Root membership keys.__ A @get@ root contributes no collection
  surrogate key (the spec's key families have no root-reassignment key);
  its entity key covers field changes.
* __Cache digests (§10.4)__: @X-Have@ \/ @X-Have-Digest@ are honored on
  priv slices and oneshot POSTs only — matching @(id, ver)@ entity
  records emit as @unchanged@ markers ("Lattice.Digest"). On pub\/ctx
  slices both headers are ignored entirely (responses never vary on
  them). @project=refs@ covers the refs-only partial-loading path.
* __Live queries (§12)__ are served: @live=sse@ (or an
  @Accept: text\/event-stream@ hint) on a memoized hash-form GET opens
  an SSE stream — snapshot burst, bus-driven delta pushes, reauth on
  proof expiry. Pins and deviations are on 'serveLiveQuery'; the
  @503 lattice:live-over-capacity@ problem joins the minor-code table
  above. The subscription machinery lives in "Lattice.Server.Live".
* __The @nodes@ root (§14.4)__ is implicit: 'Lattice.Query.Validate' and
  'Lattice.Plan' inject it, so it needs no routing here — a nodes query
  memoizes, hash-GETs, subscribes live, and slices like any query. The
  executor-side pins live on 'Lattice.Server.Execute.nodesRootJobs'.
* __The invalidation feed (§18.6)__: @GET \/invalidations?since=&live=sse@
  streams the §11.5 bus over SSE — replay via 'invalidationsSince', then
  the live tail. UNGATED in the reference implementation (deployments
  gate it at the proxy\/service tier); pins on 'serveInvalidations'.
* 'ocNow' is carried for deployment wiring (e.g. building a
  'ProofVerifier'); the handler itself never reads the clock.
* __Admission & coalescing.__ Signed admission (§14.3) is enforced only on
  the compile paths (QUERY\/POST introduction, inline @d=@, oneshot) —
  the memo means a hash-form GET was already admitted. Discovery's
  @coalesceWindowMs@ reports the origin's live 'ocCoalesce' window
  (0 when coalescing is off), shadowing the static
  'Lattice.Schema.coalesceWindowMs' budget field.
-}
module Lattice.Server (
  OriginConfig (..),
  Origin,
  newOrigin,
  latticeHandler,
  originCoalescer,

  -- * The invalidation bus (spec §11.5)

  -- | Every purge the origin emits — mutation write sets and degraded
  -- responses' self-purges — is published as an 'InvalEvent' with a
  -- monotone outbox cursor before the CDN hook ('ocPurge') runs. Live
  -- queries (§12), the federation invalidation feed (§18.6), and
  -- 'Lattice.Schema.Maintained' derivation recomputation (§3.7) are all
  -- consumers of this one pipe.
  InvalEvent (..),
  publishPurge,
  subscribeInvalidations,
  invalidationsSince,

  -- * Maintained derivations (§3.7)
  startMaintainedRelay,
  maintainedRecompute,

  -- * Live queries (spec §12)

  -- | The SSE subscription surface: a hash-form GET with @live=sse@
  -- (or an @Accept: text\/event-stream@ hint) upgrades to an event
  -- stream served from the "Lattice.Server.Live" table. Config rides
  -- in 'ocLive'; the machinery's knobs are re-exported here so
  -- deployments configure the origin from one import.
  LiveConfig (..),
  defaultLiveConfig,
  originLiveSubscribers,

  -- * Compatibility registry glue (spec §17)

  -- | The Origin-facing half of the registry: the deployment log and
  -- pure checker live in "Lattice.Registry" \/ "Lattice.Compat";
  -- 'exportCorpus' lives here because it snapshots 'Origin' internals
  -- (memo, tenure, @Lattice-Client@ attribution). Served over HTTP as
  -- @GET \/schema\/corpus@ and consumed by @POST \/schema\/check@, both
  -- routed only when 'ocRegistry' is configured.
  exportCorpus,
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (SomeAsyncException (..), SomeException, bracket_, catch, fromException, onException, throwIO, try)
import Control.Monad (unless, void, when)
import Data.Aeson.Types qualified as A
import Data.Aeson ((.=))
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL qualified as B64U
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.Char (isAlphaNum)
import Data.Foldable (for_, traverse_)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (elemIndex, find, foldl', sort, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe, maybeToList)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Data.Vector qualified as V
import Data.Word (Word64)
import Lattice.Backend
import Lattice.Canonical (Compiled (..), canonicalFieldKey, compileText)
import Lattice.Compat (CheckConfig (..), CheckMode (..), LoggedSchema (..), checkSchemas, parseCheckMode, parseWindow)
import Lattice.Compat qualified as Compat
import Lattice.Compress (Dictionary, decompressQuery, schemaDictionary)
import Lattice.Digest (elideKnown, requestDigest)
import Lattice.Hash (b64url, blake3, dictHash, manifestEtagHash, schemaHash)
import Lattice.IDL.Parser (SchemaError (..), parseSchema)
import Lattice.IDL.Print (canonicalIdl)
import Lattice.Plan
import Lattice.Query.AST (Argument (..), Field (..), QValue (..), Selection (..), TypeRefQ (..), VarDef (..))
import Lattice.Query.Validate (CompileError (..), normalizeTypeAlias)
import Lattice.Registry (CorpusEntry (..), DeployEntry (..), Registry, registryLog)
import Lattice.Schema
import Lattice.Server.Auth
import Lattice.Server.Coalesce
import Lattice.Server.Execute
import Lattice.Telemetry (
  LatticeTelemetry,
  SpanContext,
  activeSpanContext,
  addActiveSpanAttrs,
  addSpanAttrs,
  boolA,
  countMutationReplay,
  countPageContention,
  countPlanSupersession,
  countTenurePromotion,
  elapsedMs,
  intA,
  recordCompileDuration,
  recordDerivationLag,
  recordInvalidationLag,
  recordPurgeFanout,
  traceresponseValue,
  txtA,
  withLatticeSpan,
  withServerSpan,
 )
import Lattice.Telemetry qualified as Tel
import Lattice.Server.Live
import Lattice.Types
import Lattice.Value
import Lattice.Wire
import Network.HTTP.Client.SSE (
  ServerSentEvent (..),
  SseFrame (SseComment, SseDispatch),
  sseResponseBodyFrames,
 )
import Network.HTTP.Message (Request (..), Response (..))
import Network.HTTP.PercentEncoding (decodeQueryString, encodePathSegment, encodeQueryComponent, percentDecode)
import Network.HTTP.Server (Handler)
import Network.HTTP.Types.Body (Body (..))
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method (Method (..), fromMethod)
import Network.HTTP.Types.Status (pattern Status)


-- ---------------------------------------------------------------------------
-- Origin lifecycle
-- ---------------------------------------------------------------------------

data OriginConfig = OriginConfig
  { ocSchema :: Schema
  , ocBudgets :: Budgets
  , ocBackend :: Backend
  , ocVerifier :: Maybe ProofVerifier
  -- ^ 'Nothing' = dev mode: the @vc@ payload is trusted, no proof required.
  , ocSnapshotDomain :: Text
  -- ^ e.g. @\"main\"@ → @Lattice-Snapshot: main=\"\<token\>\"@.
  , ocPurge :: [SurrogateKey] -> IO ()
  -- ^ CDN purge hook; @const (pure ())@ for no CDN.
  , ocCors :: Bool
  -- ^ Permissive CORS for demo\/browser use.
  , ocNow :: IO POSIXTime
  , ocAdmission :: QueryAdmission
  -- ^ Cold-path query admission (spec §14.3); 'AdmitOpen' = today's behavior.
  , ocCoalesce :: Maybe CoalesceConfig
  -- ^ Point-fetch load coalescing (spec §6.9); 'Nothing' = direct loads.
  , ocRegistry :: Maybe Registry
  -- ^ The compatibility registry (spec §17.1): routes @POST \/schema\/check@
  -- and @GET \/schema\/corpus@ and enables @Lattice-Client@ corpus
  -- attribution. 'Nothing' (the norm — the registry is analysis
  -- infrastructure, never a serving dependency) leaves the endpoints
  -- unrouted (404) and recording disabled at zero cost.
  , ocLive :: LiveConfig
  -- ^ Live-query subscriptions (spec §12): keep-alive period, reauth
  -- grace, subscriber cap. 'defaultLiveConfig' for production defaults;
  -- live serving is always routed (a subscription costs nothing until
  -- someone opens one).
  , ocTelemetry :: LatticeTelemetry
  -- ^ §19 OpenTelemetry instrumentation. 'Lattice.Telemetry.noTelemetry'
  -- (the default) is a guaranteed no-op; build a live handle with
  -- 'Lattice.Telemetry.newLatticeTelemetry'.
  }


-- | Idempotency store entry (spec §11.2).
data IdemEntry
  = IdemInFlight
  | IdemDone
      { idDigest :: ByteString
      , idStatus :: Int
      , idHeaders :: Headers
      , idBody :: ByteString
      }


data Origin = Origin
  { oConfig :: OriginConfig
  , oIdl :: Text
  , oIdlHash :: Text
  , oDict :: Dictionary
  , oDictHash :: Text
  , oCoalescer :: Maybe Coalescer
  -- ^ Live per-type accumulation windows when 'ocCoalesce' is set (§6.9).
  , oMemo :: TVar (Map Text (Compiled, Plan))
  , oIdem :: TVar (Map (MutationName, Text, Text) IdemEntry)
  , oTenure :: TVar (Map Text Int)
  , oInvalCursor :: TVar Word64
  , oInvalLog :: TVar (Seq InvalEvent)
  -- ^ Bounded replay window for @/invalidations?since=@ resumption.
  , oInvalBus :: TChan InvalEvent
  -- ^ Broadcast channel; subscribe via 'subscribeInvalidations'.
  , oFloors :: TVar FloorIndex
  -- ^ §10.2 validity-floor index: the last intersecting invalidation per
  -- surrogate key, recorded in domain-token space.
  , oPendingEffects :: TVar Int
  -- ^ §10.2 prefix-completeness gate: effects between commit and floor
  -- indexing. While nonzero, floors refuse to certify past the response
  -- token (point interval) — see 'withEffectGate' and 'floorTokenFor'.
  , oClients :: TVar (Map Text (Set.Set Text))
  -- ^ Advisory @Lattice-Client@ builds seen per query hash (§17.1),
  -- recorded only when 'ocRegistry' is configured; corpus attribution.
  , oLive :: LiveState
  -- ^ The §12 subscription table ("Lattice.Server.Live").
  }


{- | The origin's live coalescer, when 'ocCoalesce' configured one — the
handle for the deterministic test hooks ('Lattice.Server.Coalesce.flushNow',
'Lattice.Server.Coalesce.awaitPending', 'Lattice.Server.Coalesce.coalesceStats').
-}
originCoalescer :: Origin -> Maybe Coalescer
originCoalescer = oCoalescer


{- | One drained batch of the invalidation pipeline (§11.5): the surrogate
keys of a committed effect (or of a degraded response's self-purge, §9.4.5),
tagged with a monotone outbox cursor. Cursors are per-process and 1-based;
they make feed consumption resumable ('invalidationsSince'), not durable.
-}
data InvalEvent = InvalEvent
  { ieCursor :: !Word64
  , ieKeys :: [SurrogateKey]
  , ieContext :: Maybe SpanContext
  -- ^ §19.1 \"the outbox carries context\": the span context active when
  -- the purge was published (the committing mutation, a degraded
  -- response's self-purge, or a recompute). Relay work — CDN purges,
  -- live-query re-execution, maintained-derivation recomputation —
  -- LINKS its spans here rather than parenting under them. 'Nothing'
  -- when telemetry is off or the publish happened outside any span.
  }
  deriving stock (Eq, Show)


-- | How many drained batches 'oInvalLog' retains for @since=@ replay.
invalLogBound :: Int
invalLogBound = 4096


{- | Publish a purge batch: assign the next outbox cursor, append to the
replay log, wake every bus subscriber, then invoke the CDN hook. This is
the only path by which keys leave the origin; anything wanting change
notifications taps the bus rather than wrapping 'ocPurge' (§18.6's
"keeps those consumers off the mutation path").
-}
publishPurge :: Origin -> [SurrogateKey] -> IO ()
publishPurge o keys = void (publishPurgeAt o keys)


{- | 'publishPurge' returning the assigned outbox cursor. Internal: the
maintained-derivation relay records the cursors of its own purges so it
can skip them on the bus instead of re-triggering itself.
-}
publishPurgeAt :: Origin -> [SurrogateKey] -> IO Word64
publishPurgeAt o keys = do
  let tel = ocTelemetry (oConfig o)
  -- §19.1: the outbox row carries the publishing span's context so relay
  -- consumers (live queries, derivation recompute, the feed) can LINK.
  mctx <- activeSpanContext tel
  n <- atomically $ do
    n <- stateTVar (oInvalCursor o) (\c -> let c' = c + 1 in (c', c'))
    let ev = InvalEvent n keys mctx
    modifyTVar' (oInvalLog o) $ \l ->
      Seq.drop (Seq.length l + 1 - invalLogBound) (l Seq.|> ev)
    writeTChan (oInvalBus o) ev
    pure n
  -- §10.2: index the purge in domain-token space so later responses can
  -- bound their validity floors. beSnapshot runs post-commit, so the
  -- recorded token is at or after the write's visibility point —
  -- conservative (may widen floors upward), never unsound.
  tok <- beSnapshot (ocBackend (oConfig o))
  atomically (modifyTVar' (oFloors o) (recordFloors n tok keys))
  -- §19.2 relay span around the CDN hook: purge.key_count + outbox.cursor
  -- attributes, linked (not parented) to the publishing context. The
  -- hook's wall time is the origin-observable @lattice.invalidation.lag@.
  recordPurgeFanout tel (length keys)
  ((), lagMs) <-
    withLatticeSpan
      tel
      "lattice.purge"
      Tel.Internal
      (maybeToList mctx)
      [ ("lattice.purge.key_count", intA (length keys))
      , ("lattice.outbox.cursor", intA (fromIntegral n))
      ]
      (\_ -> elapsedMs (ocPurge (oConfig o) keys))
  recordInvalidationLag tel lagMs
  pure n


-- ---------------------------------------------------------------------------
-- Validity floors (§10.2)
-- ---------------------------------------------------------------------------

{- | Last intersecting invalidation per surrogate key, in domain-token
space. 'fiEpoch' is the retention horizon: the floor for keys the index
has never seen (or has evicted) — initialized to the backend token at
origin birth, raised by eviction. Floors only ever widen upward (§10.2),
so a lost entry costs convergence traffic, never soundness.
-}
data FloorIndex = FloorIndex
  { fiKeys :: !(Map SurrogateKey (Word64, SnapshotToken))
  -- ^ key → (publish cursor, backend token at publish).
  , fiEpoch :: !(Word64, SnapshotToken)
  }


initialFloorIndex :: SnapshotToken -> FloorIndex
initialFloorIndex tok = FloorIndex Map.empty (0, tok)


-- | Entries retained; beyond it the oldest half folds into the epoch.
floorIndexBound :: Int
floorIndexBound = 8192


recordFloors :: Word64 -> SnapshotToken -> [SurrogateKey] -> FloorIndex -> FloorIndex
recordFloors n tok keys fi = prune fi {fiKeys = inserted}
  where
    inserted = foldl' (\m k -> Map.insert k (n, tok) m) (fiKeys fi) keys
    prune f
      | Map.size (fiKeys f) <= floorIndexBound = f
      | otherwise =
          let es = sortOn (fst . snd) (Map.toList (fiKeys f))
              (evicted, kept) = splitAt (Map.size (fiKeys f) - floorIndexBound `div` 2) es
              epoch' = case reverse evicted of
                ((_, e) : _) | fst e > fst (fiEpoch f) -> e
                _ -> fiEpoch f
          in FloorIndex (Map.fromList kept) epoch'


{- | The §10.2 floor for a response depending on @keys@: the newest
intersecting invalidation's token (the epoch for unknown keys), clamped
to the response's own snapshot token — a publish racing this response's
execution may already have indexed a token past @snap@, and the interval
past @snap@ is simply not claimed.

Prefix completeness (§10.2, model-checked by @tla/FloorsStaleIndex.cfg@):
the pending-effects gate is read AFTER the caller took @snap@. If it is
zero, every gated effect either finished indexing its floors (visible to
the read below) or had not yet begun committing (invisible at @snap@); if
it is nonzero, some write may be committed but not yet indexed, and the
only sound floor is the point interval.
-}
floorTokenFor :: Origin -> [SurrogateKey] -> SnapshotToken -> IO SnapshotToken
floorTokenFor o keys snap = do
  pending <- readTVarIO (oPendingEffects o)
  if pending > 0
    then pure snap
    else do
      fi <- readTVarIO (oFloors o)
      let pick acc k = case Map.lookup k (fiKeys fi) of
            Just e | fst e > fst acc -> e
            _ -> acc
          flr = snd (foldl' pick (fiEpoch fi) keys)
      pure (if compareSnapshotTokens flr snap == GT then snap else flr)


floorHdr :: OriginConfig -> SnapshotToken -> Header
floorHdr cfg flr =
  (hLatticeSnapshotFloor, TE.encodeUtf8 (ocSnapshotDomain cfg <> "=\"" <> flr <> "\""))


{- | Bracket a fact-changing effect (a mutation-serving request, a
maintained-derivation recompute pass) so §10.2 prefix completeness holds:
between the effect's commit and its floor indexing, 'floorTokenFor'
answers with the point interval. Coarse by design — the gate spans the
whole request — because a floor may only ever be too high, never too low.
-}
withEffectGate :: Origin -> IO a -> IO a
withEffectGate o =
  bracket_
    (atomically (modifyTVar' (oPendingEffects o) (+ 1)))
    (atomically (modifyTVar' (oPendingEffects o) (subtract 1)))


{- | Subscribe to the live invalidation bus. Returns a blocking STM read
of the subscriber's private queue; events published after the subscription
are seen exactly once, in cursor order.
-}
subscribeInvalidations :: Origin -> IO (STM InvalEvent)
subscribeInvalidations o = do
  ch <- atomically (dupTChan (oInvalBus o))
  pure (readTChan ch)


{- | Replay every retained event with cursor greater than @since@, plus a
live tail subscribed atomically with the replay cut (no gap, no overlap).
A @since@ older than the replay window means loss: the first replayed
event's cursor will exceed @since + 1@, which consumers detect and treat
as "resync from scratch".
-}
invalidationsSince :: Origin -> Word64 -> IO ([InvalEvent], STM InvalEvent)
invalidationsSince o since = atomically $ do
  ch <- dupTChan (oInvalBus o)
  logd <- readTVar (oInvalLog o)
  let missed = filter ((> since) . ieCursor) (foldr (:) [] logd)
  pure (missed, readTChan ch)


-- ---------------------------------------------------------------------------
-- Maintained derivations (§3.7 Materialization)
-- ---------------------------------------------------------------------------

-- | One @maintained@ derived field, indexed for relay matching.
data MaintainedDeriv = MaintainedDeriv
  { mdType :: TypeName
  , mdField :: FieldName
  , mdEnt :: EntityDef
  , mdDeriv :: Derivation
  }


maintainedDerivs :: Schema -> [MaintainedDeriv]
maintainedDerivs schema = concatMap entOne (Map.toList (schemaEntities schema))
  where
    entOne (ty, ent) = mapMaybe (fieldOne ty ent) (Map.toList (entityFields ent))
    fieldOne ty ent (f, fd) = case fieldDerivation fd of
      Just d | Maintained <- derivMaterialize d -> Just (MaintainedDeriv ty f ent d)
      _ -> Nothing


{- | The owners a purged key affects for one derivation, recovered
mechanically from the read set (§3.7 Invalidation): an entity key of the
owning type matches @own(…)@ deps; a collection tag matches a
@ViaCollection@ dep, with the owner key read out of the tag's grouping
values at the link field's position (elaboration guarantees the link is
in the grouping for @maintained@). Edge deps are rejected at elaboration
(no reverse index in v1).
-}
affectedOwners :: MaintainedDeriv -> SurrogateKey -> [Text]
affectedOwners md k = concatMap dep (NE.toList (derivReads (mdDeriv md)))
  where
    dep = \case
      OwnFields _
        | Just r <- parseRef k
        , refType r == mdType md ->
            [refKey r]
      ViaCollection rf _
        | Just ToMany {relCollection = col} <- lookupEntityRel (mdEnt md) rf
        , Just rest <- T.stripPrefix (unCollectionName (colName col) <> ":") k
        , Just ix <- elemIndex (colLink col) (NE.toList (colGrouping col))
        , ownerKey : _ <- drop ix (T.splitOn "," rest) ->
            [ownerKey]
      _ -> []


{- | Recompute every @maintained@ derivation whose read-set keys intersect
the given purged keys (§3.7 Materialization): load the affected owners,
assemble 'DepValues' (own fields + aggregates), 'beDerive', and write
back through 'beStoreDerived' (write scope: the owning entity), producing
ordinary @ver@ bumps published as one 'InvalEvent'. Owners whose stored
value already equals the recomputed one are skipped — the convergence
guard: a recompute triggered by its own purge writes nothing and the loop
ends. Returns the entity keys it purged (@[]@ when converged).

Deterministic tests drive this directly with the keys of interest — no
relay thread, no sleeping.
-}
maintainedRecompute :: Origin -> [SurrogateKey] -> IO [SurrogateKey]
maintainedRecompute o keys = fst <$> maintainedRecomputeAt o keys


maintainedRecomputeAt :: Origin -> [SurrogateKey] -> IO ([SurrogateKey], Maybe Word64)
maintainedRecomputeAt o = maintainedRecomputeFrom o Nothing


{- | 'maintainedRecomputeAt' with the triggering outbox event's span
context: each derivation recomputed runs in a @lattice.recompute@ span
(§19.2: @lattice.derivation.name@ attribute) that LINKS to the
originating mutation (§19.1), and a committing recompute records its
trigger-to-commit wall time as @lattice.derivation.lag@.
-}
maintainedRecomputeFrom :: Origin -> Maybe SpanContext -> [SurrogateKey] -> IO ([SurrogateKey], Maybe Word64)
maintainedRecomputeFrom o mctx keys = withEffectGate o $ do
  let schema = ocSchema (oConfig o)
      be = ocBackend (oConfig o)
      work =
        mapMaybe
          ( \md -> case dedupOrd (concatMap (affectedOwners md) keys) of
              [] -> Nothing
              owners -> Just (md, owners)
          )
          (maintainedDerivs schema)
  written <- concat <$> traverse (recomputeOne be) work
  case dedupOrd written of
    [] -> pure ([], Nothing)
    ks -> do
      cur <- publishPurgeAt o ks
      pure (ks, Just cur)
  where
    tel = ocTelemetry (oConfig o)
    derivName md = unTypeName (mdType md) <> "." <> unFieldName (mdField md)
    recomputeOne be (md, owners) =
      withLatticeSpan
        tel
        "lattice.recompute"
        Tel.Internal
        (maybeToList mctx)
        [("lattice.derivation.name", txtA (derivName md))]
        $ \_ -> do
          (written, ms) <- elapsedMs (recomputeBody be md owners)
          unless (null written) (recordDerivationLag tel (derivName md) ms)
          pure written
    recomputeBody be md owners = do
      -- The recompute reads exactly the derivation's row-side read set:
      -- @own(…)@ deps, grouped-by override fields of 'ViaCollection'
      -- deps, and the maintained field itself (value-identical writes
      -- are skipped by comparing the stored value).
      let reads' = NE.toList (derivReads (mdDeriv md))
          ownNames =
            concatMap
              ( \case
                  OwnFields fs -> NE.toList fs
                  _ -> []
              )
              reads'
          proj =
            ProjectFields . Set.fromList $
              mdField md
                : ownNames
                  <> concatMap
                    ( \case
                        ViaCollection rf _
                          | Just ToMany {relCollection = col} <- lookupEntityRel (mdEnt md) rf ->
                              NE.filter (/= colLink col) (colGrouping col)
                        _ -> []
                    )
                    reads'
      loaded <- beLoad be (mdType md) proj owners
      let rows =
            mapMaybe
              ( \k -> case Map.lookup k loaded of
                  Just (Right (RowFound row)) -> Just (k, row)
                  _ -> Nothing
              )
              owners
          aggDeps =
            mapMaybe
              ( \case
                  ViaCollection rf agg
                    | Just ToMany {relCollection = col} <- lookupEntityRel (mdEnt md) rf ->
                        Just (rf, col, agg)
                  _ -> Nothing
              )
              reads'
      if null rows
        then pure []
        else do
          aggVals <-
            traverse
              ( \(rf, col, agg) -> do
                  let gks = map (\(k, row) -> (k, deriveGroupKey col (Ref (mdType md) k) row)) rows
                  res <- beAggregate be (colName col) agg (dedupOrd (map snd gks))
                  pure (rf, Map.fromList gks, res)
              )
              aggDeps
          let depOf (k, row) =
                let aggsFor =
                      Map.fromList
                        ( mapMaybe
                            ( \(rf, gkMap, res) -> case res of
                                Right vals ->
                                  Map.lookup k gkMap >>= \gk -> (,) rf <$> Map.lookup gk vals
                                Left _ -> Nothing
                            )
                            aggVals
                        )
                in ( k
                   , DepValues
                      { dvOwn = Map.restrictKeys (rowFields row) (Set.fromList ownNames)
                      , dvEdges = Map.empty
                      , dvAggregates = aggsFor
                      }
                   )
          vals <- beDerive be (mdType md) (mdField md) (Map.fromList (map depOf rows))
          let rowsMap = Map.fromList rows
              changed =
                Map.filterWithKey
                  ( \k v -> case Map.lookup k rowsMap of
                      Nothing -> False
                      Just row ->
                        fmap canonicalJson (Map.lookup (mdField md) (rowFields row))
                          /= Just (canonicalJson v)
                  )
                  vals
          if Map.null changed
            then pure []
            else do
              stored <- beStoreDerived be (mdType md) (mdField md) changed
              pure (map (\k -> renderRef (Ref (mdType md) k)) (Map.keys stored))


{- | Start the @maintained@-derivation relay (§3.7 Materialization): a bus
consumer that recomputes affected maintained values and writes them back,
producing ordinary @ver@ bumps and purges. Returns the (idempotent)
shutdown action. Loop guards, both live: the relay skips events it
published itself (matched by outbox cursor via 'publishPurgeAt'), and
'maintainedRecompute' skips value-identical writes — so recomputation
converges even for derivations reading their own entity's fields. A
crashing recompute pass degrades to a skipped event; the relay survives.

Deterministic testing: call 'maintainedRecompute' directly (no thread),
or subscribe via 'subscribeInvalidations' and block in STM on the
recompute's own purge event — never sleep.
-}
startMaintainedRelay :: Origin -> IO (IO ())
startMaintainedRelay o = do
  next <- subscribeInvalidations o
  stopV <- newTVarIO False
  skipRef <- newIORef Set.empty
  let loop = do
        step <-
          atomically $
            orElse
              (Left <$> (readTVar stopV >>= check))
              (Right <$> next)
        case step of
          Left () -> pure ()
          Right ev -> do
            skips <- readIORef skipRef
            if Set.member (ieCursor ev) skips
              then writeIORef skipRef (Set.delete (ieCursor ev) skips)
              else do
                outcome <- try (maintainedRecomputeFrom o (ieContext ev) (ieKeys ev))
                case outcome of
                  Right (_, mcur) -> for_ mcur (\c -> modifyIORef' skipRef (Set.insert c))
                  Left e
                    | Just SomeAsyncException {} <- fromException e -> throwIO e
                    | otherwise -> pure ()
            loop
  _ <- forkIO loop
  pure (atomically (writeTVar stopV True))


-- | Allocate the origin's shared state and precompute the schema documents.
newOrigin :: OriginConfig -> IO Origin
newOrigin cfg = do
  let idl = canonicalIdl (ocSchema cfg)
      dict = schemaDictionary (ocSchema cfg)
  coalescer <- traverse (newCoalescerWith (ocTelemetry cfg)) (ocCoalesce cfg)
  Origin cfg idl (schemaHash idl) dict (dictHash dict) coalescer
    <$> newTVarIO Map.empty
    <*> newTVarIO Map.empty
    <*> newTVarIO Map.empty
    <*> newTVarIO 0
    <*> newTVarIO Seq.empty
    <*> newBroadcastTChanIO
    <*> (newTVarIO . initialFloorIndex =<< beSnapshot (ocBackend cfg))
    <*> newTVarIO 0
    <*> newTVarIO Map.empty
    <*> newLiveState


-- ---------------------------------------------------------------------------
-- Handler and routing
-- ---------------------------------------------------------------------------

{- | The request handler. Telemetry (§19): every routed request runs in a
server span named by route template ('routeTemplate'), parented on an
inbound @traceparent@; a @traceresponse@ header is attached ONLY to
uncacheable responses (§19.4 — shared-cacheable responses MUST NOT carry
per-request telemetry identifiers, so anything with a @public@
@Cache-Control@ never gets one).
-}
latticeHandler :: Origin -> Handler
latticeHandler origin req = do
  let tel = ocTelemetry (oConfig origin)
      (segs, _) = pathAndParams req
      serve = route origin req `catch` onCrash
  resp <- case routeTemplate (requestMethod req) segs of
    Nothing -> serve
    Just template ->
      withServerSpan
        tel
        (lookupHeader "traceparent" (requestHeaders req))
        template
        [("lattice.snapshot.domains", txtA (ocSnapshotDomain (oConfig origin)))]
        $ \msp -> do
          r <- serve
          case msp of
            Just sp | uncacheable r -> do
              tr <- traceresponseValue sp
              pure r {responseHeaders = insertHeader "traceresponse" tr (responseHeaders r)}
            _ -> pure r
  pure (addCorsHeaders (ocCors (oConfig origin)) resp)
  where
    -- Shared-cacheable = an explicit public Cache-Control (§19.4). No
    -- Cache-Control at all (schema docs, 405s, …) is treated as
    -- uncacheable-by-default and MAY carry traceresponse.
    uncacheable r = case lookupHeader hCacheControl (responseHeaders r) of
      Just cc -> not ("public" `BS.isInfixOf` cc)
      Nothing -> True
    onCrash :: SomeException -> IO Response
    onCrash e
      | Just SomeAsyncException {} <- fromException e = throwIO e
      | otherwise =
          pure . problemResponse req $
            (mkProblem 500 "lattice:internal") {pDetail = Just "unhandled exception"}


{- | The §19.2 server-span name: the route template, never a concrete
hash\/key\/name. Mirrors 'route' case-for-case; unrouted paths get no
span ('Nothing') — they are 404\/405 noise, not protocol surface.
-}
routeTemplate :: Method -> [Text] -> Maybe Text
routeTemplate method segs = (prefix <>) <$> tpl
  where
    prefix = lenientText (fromMethod method) <> " "
    tpl = case (method, segs) of
      (OPTIONS, _) -> Just "*"
      (GET, [".well-known", "lattice"]) -> Just "/.well-known/lattice"
      (GET, ["schema", "current"]) -> Just "/schema/current"
      (GET, ["schema", "dict", _]) -> Just "/schema/dict/{hash}"
      (GET, ["schema", "corpus"]) -> Just "/schema/corpus"
      (GET, ["schema", _]) -> Just "/schema/{hash}"
      (GET, ["q"]) -> Just "/q"
      (GET, ["q", _]) -> Just "/q/{hash}"
      (GET, ["q", _, "source"]) -> Just "/q/{hash}/source"
      (GET, ["q", _, "explain"]) -> Just "/q/{hash}/explain"
      (GET, ["q", _, "plan", _]) -> Just "/q/{hash}/plan/{planId}"
      (GET, ["e", _, _]) -> Just "/e/{Type}/{key}"
      (GET, ["invalidations"]) -> Just "/invalidations"
      (QUERY, ["q"]) -> Just "/q"
      (POST, ["q"]) -> Just "/q"
      (POST, ["m", _]) -> Just "/m/{name}"
      (POST, ["schema", "check"]) -> Just "/schema/check"
      (POST, ["e", _]) -> Just "/e/{Type}"
      (PUT, ["e", _, _]) -> Just "/e/{Type}/{key}"
      (PATCH, ["e", _, _]) -> Just "/e/{Type}/{key}"
      (PATCH, ["e", _]) -> Just "/e/{Type}"
      (DELETE, ["e", _, _]) -> Just "/e/{Type}/{key}"
      (DELETE, ["e", _]) -> Just "/e/{Type}"
      _ -> Nothing


route :: Origin -> Request -> IO Response
route o req = case requestMethod req of
  OPTIONS
    | ocCors (oConfig o) -> pure (preflight req)
  GET -> case segs of
    [".well-known", "lattice"] -> pure (discovery o req)
    ["schema", "current"] -> pure (schemaCurrent o req)
    ["schema", "dict", h] -> pure (schemaDict o req h)
    ["schema", "corpus"] -> serveCorpus o req
    ["schema", h] -> pure (schemaDoc o req h)
    ["q"] -> case lookup "d" params of
      Just d -> serveQuery o req params (QInline d (lookup "dv" params)) NotIntro
      Nothing -> pure (problemResponse req (badRequest ["the inline query form requires the d parameter"]))
    ["q", h] -> case liveRequested req params of
      Left p -> pure (problemResponse req p)
      Right True -> serveLiveQuery o req params h
      Right False -> serveQuery o req params (QHash h) NotIntro
    ["q", h, "source"] -> serveSource o req h
    ["q", h, "explain"] -> serveExplain o req h
    ["q", h, "plan", pid] -> servePlanDoc o req h pid
    ["e", ty, key] -> serveEntity o req params ty key
    ["invalidations"] -> serveInvalidations o req params
    _ -> pure (fallback o req segs)
  QUERY -> case segs of
    ["q"] -> withQueryBody req $ \txt -> serveQuery o req params (QText txt) Introduce
    _ -> pure (fallback o req segs)
  POST -> case segs of
    ["q"] -> case lookup "intent" params of
      Just "oneshot" -> withQueryBody req $ \txt -> serveQuery o req params (QText txt) Oneshot
      Just "introduce" -> asIntro
      Nothing -> asIntro
      Just other -> pure (problemResponse req (badRequest ["unknown intent: " <> other]))
    ["m", name] -> serveMutation o req params (MutationName name)
    ["e", ty] -> serveBound o req params BindCreate ty Nothing
    ["schema", "check"] -> serveSchemaCheck o req params
    _ -> pure (fallback o req segs)
  PUT -> case segs of
    ["e", ty, key] -> serveBound o req params BindPut ty (Just key)
    _ -> pure (fallback o req segs)
  PATCH -> case segs of
    ["e", ty, key] -> serveBound o req params BindPatch ty (Just key)
    ["e", ty] -> serveBound o req params BindPatch ty Nothing
    _ -> pure (fallback o req segs)
  DELETE -> case segs of
    ["e", ty, key] -> serveBound o req params BindDelete ty (Just key)
    ["e", ty] -> serveBound o req params BindDelete ty Nothing
    _ -> pure (fallback o req segs)
  _ -> pure (fallback o req segs)
  where
    (segs, params) = pathAndParams req
    asIntro = withQueryBody req $ \txt -> serveQuery o req params (QText txt) Introduce


-- | Percent-decoded path segments and query parameters.
pathAndParams :: Request -> ([Text], [(Text, Text)])
pathAndParams req =
  let (path, q) = BS8.break (== '?') (requestTarget req)
      segs = mapMaybe seg (BS8.split '/' path)
      qbs = BS.drop 1 q
      params =
        if BS.null qbs
          then []
          else map (\(k, v) -> (lenientText k, lenientText v)) (decodeQueryString qbs)
  in (segs, params)
  where
    seg s
      | BS.null s = Nothing
      | otherwise = Just (lenientText (fromMaybe s (percentDecode s)))


lenientText :: ByteString -> Text
lenientText = TE.decodeUtf8With TEE.lenientDecode


{- | 405 with @Allow@ for known paths under the wrong method, else 404.
Entity URLs advertise the schema's verb bindings (§11.7): the keyed form
allows GET plus every keyed binding of the type; the collection form
exists only where a binding declares it.
-}
fallback :: Origin -> Request -> [Text] -> Response
fallback o req segs = case allowedFor segs of
  [] -> problemResponse req (mkProblem 404 "lattice:not-found")
  methods ->
    mkResponse
      req
      405
      [(hAllow, BS.intercalate ", " methods), (hCacheControl, "no-store")]
      ""
  where
    allowedFor :: [Text] -> [ByteString]
    allowedFor = \case
      [".well-known", "lattice"] -> ["GET"]
      ("schema" : _) -> ["GET"]
      ["q"] -> ["GET", "QUERY", "POST"]
      ("q" : _) -> ["GET"]
      ["e", ty, _] -> "GET" : boundVerbs ty ShapeKeyed
      ["e", ty] -> boundVerbs ty ShapeCollection
      ["m", _] -> ["POST"]
      _ -> []
    boundVerbs ty shape =
      mapMaybe
        ( \v ->
            if Map.member (v, TypeName ty, shape) (bindingIndex (ocSchema (oConfig o)))
              then Just (TE.encodeUtf8 (bindVerbName v))
              else Nothing
        )
        [BindCreate, BindPut, BindPatch, BindDelete]


-- ---------------------------------------------------------------------------
-- Response plumbing
-- ---------------------------------------------------------------------------

mkResponse :: Request -> Int -> Headers -> ByteString -> Response
mkResponse req code hdrs body =
  Response
    { responseStatus = Status (fromIntegral code)
    , responseVersion = requestVersion req
    , responseHeaders = hdrs
    , responseBody = if BS.null body then BodyEmpty else BodyBytes body
    , responseTrailers = pure []
    , responseH2StreamId = 0
    , responseCancel = pure ()
    , responsePushPromises = pure []
    }


ndjsonType :: ByteString
ndjsonType = "application/x-ndjson"


ndjsonResponse :: Request -> Int -> Headers -> [Record] -> Response
ndjsonResponse req code hdrs records =
  mkResponse req code ((hContentType, ndjsonType) : hdrs) (encodeRecords records)


jsonResponse :: Request -> Int -> Headers -> A.Value -> Response
jsonResponse req code hdrs v =
  mkResponse req code ((hContentType, "application/json") : hdrs) (BL.toStrict (A.encode v))


immutableCC :: ByteString
immutableCC = "public, max-age=31536000, immutable"


planHdr :: Plan -> Header
planHdr p = (hLatticePlan, TE.encodeUtf8 (planId p))


schemaHdr :: Origin -> Header
schemaHdr o = (hLatticeSchema, TE.encodeUtf8 (oIdlHash o))


snapshotHdr :: OriginConfig -> SnapshotToken -> Header
snapshotHdr cfg snap =
  (hLatticeSnapshot, TE.encodeUtf8 (ocSnapshotDomain cfg <> "=\"" <> snap <> "\""))


keysHdr :: [SurrogateKey] -> Header
keysHdr keys = (hSurrogateKey, TE.encodeUtf8 (T.intercalate " " keys))


addCorsHeaders :: Bool -> Response -> Response
addCorsHeaders False r = r
addCorsHeaders True r =
  r
    { responseHeaders =
        insertHeader "Access-Control-Allow-Origin" "*" $
          insertHeader "Access-Control-Expose-Headers" corsExpose (responseHeaders r)
    }
  where
    corsExpose =
      "Location, Content-Location, Lattice-Plan, Lattice-Schema, Lattice-Snapshot, ETag, Surrogate-Key, Idempotency-Replayed"


preflight :: Request -> Response
preflight req =
  mkResponse
    req
    204
    [ ("Access-Control-Allow-Origin", "*")
    , ("Access-Control-Allow-Methods", "GET, POST, PUT, PATCH, DELETE, QUERY, OPTIONS")
    , ("Access-Control-Allow-Headers", "Content-Type, Authorization, Idempotency-Key, If-Match, If-None-Match, X-Vc-Auth, X-Have, X-Have-Digest, Lattice-Query-Name")
    , ("Access-Control-Max-Age", "600")
    ]
    ""


-- ---------------------------------------------------------------------------
-- Problems (RFC 9457, spec §15 + §10.8)
-- ---------------------------------------------------------------------------

data Problem = Problem
  { pStatus :: Int
  , pCode :: Text
  , pDetail :: Maybe Text
  , pDiagnostics :: [Text]
  , pExtra :: [A.Pair]
  , pHeaders :: Headers
  , pCache :: ByteString
  }


mkProblem :: Int -> Text -> Problem
mkProblem st code =
  Problem
    { pStatus = st
    , pCode = code
    , pDetail = Nothing
    , pDiagnostics = []
    , pExtra = []
    , pHeaders = []
    , pCache = problemCache code
    }


-- | Explicit cache directives per spec §10.8; @no-store@ default.
problemCache :: Text -> ByteString
problemCache = \case
  "lattice:compile-rejected" -> "public, s-maxage=300"
  "lattice:fragment-shadow" -> "public, s-maxage=300"
  "lattice:cursor-retired" -> "public, s-maxage=60"
  _ -> "no-store"


badRequest :: [Text] -> Problem
badRequest ds = (mkProblem 400 "lattice:compile-rejected") {pDiagnostics = ds}


compileProblem :: CompileError -> Problem
compileProblem ce =
  (mkProblem (ceStatus ce) (ceCode ce))
    { pDiagnostics = ceDiagnostics ce
    , pHeaders = if ceStatus ce >= 500 then [(hRetryAfter, "1")] else []
    }


problemResponse :: Request -> Problem -> Response
problemResponse req p = mkResponse req (pStatus p) hdrs body
  where
    hdrs =
      (hContentType, "application/problem+json")
        : (hCacheControl, pCache p)
        : pHeaders p
    body =
      BL.toStrict . A.encode . A.object $
        [ "type" .= ("https://lattice.dev/problems/" <> codeSuffix (pCode p))
        , "title" .= pCode p
        , "status" .= pStatus p
        ]
          <> catMaybes [("detail" .=) <$> pDetail p]
          <> (if null (pDiagnostics p) then [] else ["diagnostics" .= pDiagnostics p])
          <> pExtra p
    codeSuffix c = fromMaybe c (T.stripPrefix "lattice:" c)


-- ---------------------------------------------------------------------------
-- Discovery and schema documents (spec §7.1)
-- ---------------------------------------------------------------------------

discovery :: Origin -> Request -> Response
discovery o req =
  jsonResponse req 200 [(hCacheControl, "public, max-age=60")] $
    A.object
      [ "endpoints"
          .= A.object
            [ "query" .= ("/q" :: Text)
            , "mutation" .= ("/m" :: Text)
            , "entity" .= ("/e" :: Text)
            , "schema" .= ("/schema" :: Text)
            ]
      , "schema" .= A.object ["current" .= ("/schema/" <> oIdlHash o)]
      , "admission" .= admissionMode (ocAdmission (oConfig o))
      , "queryMediaType" .= TE.decodeUtf8 queryMediaType
      , "methods" .= A.object ["introduce" .= (["QUERY", "POST"] :: [Text])]
      , "dictionary"
          .= A.object
            [ "current" .= ("/schema/dict/" <> oDictHash o)
            , "algorithm" .= ("deflate-raw/9" :: Text)
            ]
      , "budgets" .= budgetsJson (effectiveCoalesceMs (oConfig o)) (ocBudgets (oConfig o))
      , "idempotency" .= A.object ["defaultRetention" .= ("PT24H" :: Text)]
      ]


-- | Discovery's @admission@ field (§7.1 / §14.3).
admissionMode :: QueryAdmission -> Text
admissionMode = \case
  AdmitOpen -> "open"
  AdmitSigned _ -> "signed"


{- | Discovery's @coalesceWindowMs@: what this origin actually does —
the live 'ocCoalesce' window in whole milliseconds, 0 when coalescing is
disabled. The static 'Lattice.Schema.coalesceWindowMs' budget field is
shadowed here so the published document never promises batching the
origin does not perform.
-}
effectiveCoalesceMs :: OriginConfig -> Int
effectiveCoalesceMs cfg =
  maybe 0 (\cc -> max 0 (ccWindowMicros cc) `div` 1000) (ocCoalesce cfg)


budgetsJson :: Int -> Budgets -> A.Value
budgetsJson windowMs b =
  A.object
    [ "maxCanonicalBytes" .= maxCanonicalBytes b
    , "maxDepth" .= maxDepth b
    , "maxRoots" .= maxRoots b
    , "maxRounds" .= maxRounds b
    , "maxRoundFanout" .= maxRoundFanout b
    , "maxSurrogateKeys" .= maxSurrogateKeys b
    , "maxBatchItems" .= maxBatchItems b
    , "maxPageDefault" .= maxPageDefault b
    , "coalesceWindowMs" .= windowMs
    ]


schemaCurrent :: Origin -> Request -> Response
schemaCurrent o req =
  mkResponse
    req
    307
    [ (hLocation, TE.encodeUtf8 ("/schema/" <> oIdlHash o))
    , (hCacheControl, "max-age=60")
    ]
    ""


schemaDoc :: Origin -> Request -> Text -> Response
schemaDoc o req h
  | h == oIdlHash o =
      mkResponse
        req
        200
        [(hContentType, "text/plain; charset=utf-8"), (hCacheControl, immutableCC)]
        (TE.encodeUtf8 (oIdl o))
  | otherwise =
      problemResponse req ((mkProblem 404 "lattice:not-found") {pDetail = Just "unknown schema document"})


schemaDict :: Origin -> Request -> Text -> Response
schemaDict o req h
  | h == oDictHash o =
      mkResponse
        req
        200
        [(hContentType, "application/octet-stream"), (hCacheControl, immutableCC)]
        (oDict o)
  | otherwise =
      problemResponse req ((mkProblem 404 "lattice:not-found") {pDetail = Just "unknown dictionary"})


-- ---------------------------------------------------------------------------
-- Query inspection endpoints (spec §7.2)
-- ---------------------------------------------------------------------------

withMemo :: Origin -> Request -> Text -> ((Compiled, Plan) -> IO Response) -> IO Response
withMemo o req h k = do
  memo <- readTVarIO (oMemo o)
  case Map.lookup h memo of
    Nothing ->
      pure . problemResponse req $
        (mkProblem 404 "lattice:unknown-query")
          {pDetail = Just "no memoized query with this hash; introduce it via QUERY or POST"}
    Just cp -> k cp


serveSource :: Origin -> Request -> Text -> IO Response
serveSource o req h = withMemo o req h $ \(c, p) ->
  pure $
    mkResponse
      req
      200
      [ (hContentType, "text/plain; charset=utf-8")
      , (hCacheControl, immutableCC)
      , planHdr p
      , schemaHdr o
      ]
      (TE.encodeUtf8 (compiledText c))


serveExplain :: Origin -> Request -> Text -> IO Response
serveExplain o req h = withMemo o req h $ \(_, p) ->
  pure $
    jsonResponse
      req
      200
      [(hCacheControl, "public, s-maxage=300"), planHdr p, schemaHdr o]
      (explainJson (ocSchema (oConfig o)) (ocBudgets (oConfig o)) p)


servePlanDoc :: Origin -> Request -> Text -> Text -> IO Response
servePlanDoc o req h pid = withMemo o req h $ \(_, p) ->
  if pid == planId p
    then
      pure $
        ndjsonResponse
          req
          200
          [ (hCacheControl, immutableCC)
          , planHdr p
          , schemaHdr o
          , keysHdr [planKey (planId p)]
          ]
          [RPlan (planSliceRecord p)]
    else
      pure . problemResponse req $
        (mkProblem 404 "lattice:unknown-plan")
          {pDetail = Just "this instance cannot derive that plan; fetch the current plan document"}


-- ---------------------------------------------------------------------------
-- Query pipeline (spec §6)
-- ---------------------------------------------------------------------------

data QueryInput
  = QHash Text
  | QInline Text (Maybe Text)
  | QText Text


data Intro = NotIntro | Introduce | Oneshot
  deriving stock (Eq)


-- | Drain and validate an introduction body.
withQueryBody :: Request -> (Text -> IO Response) -> IO Response
withQueryBody req k = case lookupHeader hContentType (requestHeaders req) of
  Just ct
    | not (queryMediaType `BS.isPrefixOf` ct) ->
        pure . problemResponse req $
          (mkProblem 415 "lattice:unsupported-media")
            {pDetail = Just ("introduction bodies must be " <> TE.decodeUtf8 queryMediaType)}
  _ -> do
    body <- drainBody (requestBody req)
    case TE.decodeUtf8' body of
      Left _ -> pure (problemResponse req (badRequest ["query body is not valid UTF-8"]))
      Right txt -> k txt


drainBody :: Body -> IO ByteString
drainBody = \case
  BodyEmpty -> pure BS.empty
  BodyBytes bs -> pure bs
  BodyStream next -> go next []
  where
    go next acc =
      next >>= \case
        Nothing -> pure (BS.concat (reverse acc))
        Just c -> go next (c : acc)


-- | Resolve a request to its compiled query and plan.
resolveQuery :: Origin -> Maybe ByteString -> QueryInput -> Intro -> IO (Either Problem (Compiled, Plan))
resolveQuery o sig input intro = case input of
  QHash h -> do
    memo <- readTVarIO (oMemo o)
    case Map.lookup h memo of
      Nothing ->
        pure . Left $
          (mkProblem 404 "lattice:unknown-query")
            {pDetail = Just "no memoized query with this hash; fall to a lower ladder rung"}
      Just cp -> pure (Right cp)
  QInline d mdv -> case dictFor mdv of
    Left p -> pure (Left p)
    Right dict -> case b64urlDecode (TE.encodeUtf8 d) of
      Left _ -> pure (Left (badRequest ["the d parameter is not valid base64url"]))
      Right bytes -> case decompressQuery dict bytes of
        Left e -> pure (Left (badRequest ["inline query failed to inflate: " <> e]))
        Right txt -> compileMemo o sig True txt
  QText txt -> compileMemo o sig (intro /= Oneshot) txt
  where
    dictFor = \case
      Nothing -> Right Nothing
      Just dv
        | dv == oDictHash o -> Right (Just (oDict o))
        | otherwise ->
            Left
              ( (mkProblem 400 "lattice:unknown-dictionary")
                  {pDetail = Just ("this origin never published dictionary " <> dv)}
              )


b64urlDecode :: ByteString -> Either String ByteString
b64urlDecode bs = case B64U.decodeUnpadded bs of
  Right ok -> Right ok
  Left _ -> B64U.decode bs


{- | Compile (and optionally memoize) canonical text — the cold path.
'ocAdmission' (§14.3) is enforced here and only here: every compile
spends admission-governed resources, while a hash-form GET's memo hit
means the text was already admitted. The signature is verified against
the origin's own canonical text ('compiledText'), after
re-canonicalization; in signed mode a missing header fast-fails before
any compile work, and a query failing admission is never memoized.
-}
compileMemo :: Origin -> Maybe ByteString -> Bool -> Text -> IO (Either Problem (Compiled, Plan))
compileMemo o sig memoize txt
  | AdmitSigned _ <- admission
  , Nothing <- sig =
      pure (Left (admissionDenied "signed admission: the X-Lattice-Query-Sig header is required"))
  | otherwise =
      -- §19.2: @lattice.compile@ exists only here — the cold path (a
      -- hash-form memo hit never compiles), so @compile.cold@ is
      -- constitutively true; a rejection carries @compile.rejected@ +
      -- @error.type@ and feeds the same duration histogram.
      withLatticeSpan tel "lattice.compile" Tel.Internal [] [("lattice.compile.cold", boolA True)] $ \msp -> do
        (outcome, ms) <- elapsedMs $ case compiled of
          Left ce -> pure (Left (compileProblem ce))
          Right (c, p) -> case admitQuery admission sig (compiledText c) of
            Left detail -> pure (Left (admissionDenied detail))
            Right () -> do
              when memoize . atomically . modifyTVar' (oMemo o) $
                Map.insert (compiledHash c) (c, p)
              pure (Right (c, p))
        case outcome of
          Left p ->
            addSpanAttrs
              msp
              [ ("lattice.compile.rejected", boolA True)
              , ("error.type", txtA (pCode p))
              ]
          Right _ -> pure ()
        recordCompileDuration tel ms (either (const True) (const False) outcome)
        pure outcome
  where
    tel = ocTelemetry (oConfig o)
    admission = ocAdmission (oConfig o)
    admissionDenied d = (mkProblem 403 "lattice:admission-denied") {pDetail = Just d}
    schema = ocSchema (oConfig o)
    budgets = ocBudgets (oConfig o)
    compiled = do
      c <- compileText schema budgets txt
      p <- planQuery schema budgets c
      pure (c, p)


serveQuery :: Origin -> Request -> [(Text, Text)] -> QueryInput -> Intro -> IO Response
serveQuery o req params input intro =
  resolveQuery o (lookupHeader hLatticeQuerySig (requestHeaders req)) input intro >>= \case
    Left p -> pure (problemResponse req p)
    Right (c, plan) -> recordClient o req (compiledHash c) *> case lookup "p" params of
      Just pid
        | pid /= planId plan -> do
            -- §19.3 @lattice.plan.supersessions@: the deploy-boundary signal.
            countPlanSupersession (ocTelemetry (oConfig o))
            pure . problemResponse req $
              (mkProblem 409 "lattice:plan-superseded")
                {pExtra = ["plan" .= RPlan (planSliceRecord plan)]}
      _ -> case lookup "slice" params of
        Just "page"
          | intro == Oneshot -> servePage o req params c plan
          | otherwise ->
              -- §6.6: the composed page form is valid only where shared
              -- caches are already out of the loop.
              pure . problemResponse req $
                badRequest ["slice=page is valid only on one-shot POSTs and live subscriptions"]
        mslice -> case maybe (Right defSlice) parseSliceParam mslice of
          Left p -> pure (problemResponse req p)
          Right SlicePlan -> pure (planResponse o req c plan input intro)
          Right slice -> serveDataSlice o req params c plan slice input intro
  where
    defSlice = case intro of
      NotIntro -> SlicePlan
      _ -> SlicePub


parseSliceParam :: Text -> Either Problem SliceName
parseSliceParam t =
  maybe (Left (badRequest ["unknown slice: " <> t])) Right (parseSlice t)


-- | The @slice=plan@ pseudo-slice: exactly one plan record, no end (§9.2).
planResponse :: Origin -> Request -> Compiled -> Plan -> QueryInput -> Intro -> Response
planResponse o req c plan input intro =
  ndjsonResponse req 200 hdrs [RPlan (planSliceRecord plan)]
  where
    cc = case intro of
      Oneshot -> "no-store"
      _ -> "public, s-maxage=60"
    hdrs =
      [ (hCacheControl, cc)
      , planHdr plan
      , schemaHdr o
      , keysHdr [planKey (planId plan)]
      ]
        <> locationHdrs
    locationHdrs = case (input, intro) of
      (QHash _, _) -> []
      (_, Oneshot) -> []
      _ ->
        let url = TE.encodeUtf8 ("/q/" <> compiledHash c)
        in [(hLocation, url), (hContentLocation, url)]


serveDataSlice ::
  Origin ->
  Request ->
  [(Text, Text)] ->
  Compiled ->
  Plan ->
  SliceName ->
  QueryInput ->
  Intro ->
  IO Response
serveDataSlice o req params c plan slice input intro = do
  let cfg = oConfig o
      schema = ocSchema cfg
      outcome = do
        refsOnly <- refsProjection params
        vars <- bindVariables schema (planVars plan) params
        pure (refsOnly, vars)
  case outcome of
    Left p -> pure (problemResponse req p)
    Right (refsOnly, vars) ->
      admitSlice o req params (requiredClaims plan) slice >>= \case
        Left p -> pure (problemResponse req p)
        Right (claims, vcRaw) -> do
          -- §19.2 query-span attributes (on the enclosing server span):
          -- hash/name/plan/slice — attributes, never span names. The
          -- advisory name comes from the Lattice-Query-Name header.
          addActiveSpanAttrs (ocTelemetry cfg) $
            [ ("lattice.query.hash", txtA (compiledHash c))
            , ("lattice.plan.id", txtA (planId plan))
            , ("lattice.slice", txtA (renderSlice slice))
            ]
              <> maybe
                []
                (\n -> [("lattice.query.name", txtA (lenientText n))])
                (lookupHeader "lattice-query-name" (requestHeaders req))
          let env =
                ExecEnv
                  { xSchema = schema
                  , xBudgets = ocBudgets cfg
                  , xBackend = ocBackend cfg
                  , xClaims = claims
                  , xVars = vars
                  , xMode = EmitEq slice
                  , xTelemetry = ocTelemetry cfg
                  , xProjections = planProjections schema plan
                  }
          executeRoots env (planRoots plan) >>= \case
            Left AbortCursorRetired -> pure (problemResponse req (mkProblem 410 "lattice:cursor-retired"))
            Left AbortCursorMalformed -> pure (problemResponse req (badRequest ["malformed cursor"]))
            Left (AbortBudget d) -> pure (problemResponse req (badRequest [d]))
            Right xr -> do
              snap <- beSnapshot (ocBackend cfg)
              let keys = coarsenKeys (ocBudgets cfg) (planId plan) xr
              flr <- floorTokenFor o keys snap
              promoted <- case intro of
                Oneshot -> pure True
                _ -> bumpTenure o (compiledHash c)
              let etag = manifestEtag plan vars vcRaw (xrIdVers xr) (xrWitness xr)
                  -- §10.4: digests are consulted only on priv slices and
                  -- oneshot POSTs; pub/ctx responses never vary on them.
                  haveDigest =
                    if slice == SlicePriv || intro == Oneshot
                      then requestDigest (requestHeaders req)
                      else Nothing
                  manifest =
                    Manifest
                      { mQuery = Just (compiledHash c)
                      , mMutation = Nothing
                      , mPlan = Just (planId plan)
                      , mSlice = Just slice
                      , mRoot = xrRootMap xr
                      , mEtag = etag
                      , mBatch = Nothing
                      }
                  body =
                    [RManifest manifest]
                      <> projectRecords refsOnly (elideKnown haveDigest (xrRecords xr))
                      <> [REnd (EndRecord (xrComplete xr) (Just etag))]
                  cc = cacheControlFor slice intro promoted
                  common =
                    [ (hETag, weakEtag etag)
                    , planHdr plan
                    , schemaHdr o
                    , snapshotHdr cfg snap
                    , floorHdr cfg flr
                    , keysHdr keys
                    ]
                      <> varyPriv slice
                  outcomeHdrs =
                    if xrDegraded xr
                      then [(hLatticeOutcome, "degraded")]
                      else []
                  introHdrs = case (input, intro) of
                    (QHash _, _) -> []
                    (_, Oneshot) -> []
                    _ ->
                      let url = introUrl (compiledHash c) plan slice vars refsOnly
                      in [(hLocation, url), (hContentLocation, url)]
                  status = if xrDegraded xr then 207 else 200
              when (xrDegraded xr) (publishPurge o keys)
              if inmMatch req (weakEtag etag)
                then pure (mkResponse req 304 ((hCacheControl, cc) : common) "")
                else
                  pure $
                    mkResponse
                      req
                      status
                      ([(hContentType, ndjsonType), (hCacheControl, cc)] <> common <> outcomeHdrs <> introHdrs)
                      (encodeRecords body)


-- | The nonempty data slices of a plan, in slice order (§6.5/§12 framing).
pageSlices :: Plan -> [SliceName]
pageSlices plan = case filter (/= SlicePlan) (Map.keys (planSlices plan)) of
  [] -> [SlicePub]
  ds -> ds


-- | Optimistic single-snapshot composition attempts before
-- @503 lattice:snapshot-contention@ (§6.5) / burst skip (§12).
pageComposeAttempts :: Int
pageComposeAttempts = 4


{- | @slice=page@ on a one-shot POST (§6.5): every nonempty data slice of
the plan, composed under a single per-domain snapshot (§13.2 guarantee 7)
by optimistic validation — observe the token, execute every slice, observe
again, retry on movement, and answer @503 lattice:snapshot-contention@
when the window never quiesces (model: @tla/LatticePageComposition.tla@).
Admission is strict: the request must carry credentials admitting every
nonempty slice. Framing: slice-ordered sections, each opening with its
own manifest (which carries that slice's etag); one @end@ record with no
etag closes the response.
-}
servePage :: Origin -> Request -> [(Text, Text)] -> Compiled -> Plan -> IO Response
servePage o req params c plan = do
  let outcome = do
        refsOnly <- refsProjection params
        vars <- bindVariables (ocSchema cfg) (planVars plan) params
        pure (refsOnly, vars)
  case outcome of
    Left p -> pure (problemResponse req p)
    Right (refsOnly, vars) ->
      admitAll (pageSlices plan) >>= \case
        Left p -> pure (problemResponse req p)
        Right creds -> do
          addActiveSpanAttrs
            (ocTelemetry cfg)
            [ ("lattice.query.hash", txtA (compiledHash c))
            , ("lattice.plan.id", txtA (planId plan))
            , ("lattice.slice", txtA "page")
            ]
          attempt creds vars refsOnly pageComposeAttempts
  where
    cfg = oConfig o

    admitAll = go []
      where
        go acc [] = pure (Right (reverse acc))
        go acc (s : rest) =
          admitSlice o req params (requiredClaims plan) s >>= \case
            Left p -> pure (Left p)
            Right (claims, vcRaw) -> go ((s, claims, vcRaw) : acc) rest

    attempt creds vars refsOnly n = do
      t0 <- beSnapshot (ocBackend cfg)
      execAll creds vars >>= \case
        Left resp -> pure resp
        Right secs -> do
          t1 <- beSnapshot (ocBackend cfg)
          if t0 == t1
            then emit secs vars refsOnly t1
            else do
              -- §19.3 @lattice.page.contention@: a write moved the token
              -- mid-composition; the composition is discarded, never mixed.
              countPageContention (ocTelemetry cfg)
              if n > 1
                then attempt creds vars refsOnly (n - 1)
                else
                  pure . problemResponse req $
                    (mkProblem 503 "lattice:snapshot-contention")
                      { pDetail = Just "could not compose the page under a single snapshot; retry shortly"
                      , pHeaders = [(hRetryAfter, "1")]
                      }

    execAll creds vars = go [] creds
      where
        go acc [] = pure (Right (reverse acc))
        go acc ((slice, claims, vcRaw) : rest) = do
          let env =
                ExecEnv
                  { xSchema = ocSchema cfg
                  , xBudgets = ocBudgets cfg
                  , xBackend = ocBackend cfg
                  , xClaims = claims
                  , xVars = vars
                  , xMode = EmitEq slice
                  , xTelemetry = ocTelemetry cfg
                  , xProjections = planProjections (ocSchema cfg) plan
                  }
          executeRoots env (planRoots plan) >>= \case
            Left AbortCursorRetired -> pure (Left (problemResponse req (mkProblem 410 "lattice:cursor-retired")))
            Left AbortCursorMalformed -> pure (Left (problemResponse req (badRequest ["malformed cursor"])))
            Left (AbortBudget d) -> pure (Left (problemResponse req (badRequest [d])))
            Right xr -> go ((slice, vcRaw, xr) : acc) rest

    emit secs vars refsOnly snap = do
      let keys = dedupOrd (concatMap (\(_, _, xr) -> coarsenKeys (ocBudgets cfg) (planId plan) xr) secs)
      flr <- floorTokenFor o keys snap
      let haveDigest = requestDigest (requestHeaders req) -- §10.4: consulted on one-shot POSTs
          sectionRecs (slice, vcRaw, xr) =
            let etag = manifestEtag plan vars vcRaw (xrIdVers xr) (xrWitness xr)
                manifest =
                  Manifest
                    { mQuery = Just (compiledHash c)
                    , mMutation = Nothing
                    , mPlan = Just (planId plan)
                    , mSlice = Just slice
                    , mRoot = xrRootMap xr
                    , mEtag = etag
                    , mBatch = Nothing
                    }
            in RManifest manifest : projectRecords refsOnly (elideKnown haveDigest (xrRecords xr))
          degraded = any (\(_, _, xr) -> xrDegraded xr) secs
          complete = all (\(_, _, xr) -> xrComplete xr) secs
          body = concatMap sectionRecs secs <> [REnd (EndRecord complete Nothing)]
          hdrs =
            [ (hContentType, ndjsonType)
            , (hCacheControl, "no-store")
            , planHdr plan
            , schemaHdr o
            , snapshotHdr cfg snap
            , floorHdr cfg flr
            , keysHdr keys
            ]
              <> (if degraded then [(hLatticeOutcome, "degraded")] else [])
          status = if degraded then 207 else 200
      when degraded (publishPurge o keys)
      pure (mkResponse req status hdrs (encodeRecords body))


requiredClaims :: Plan -> [ClaimName]
requiredClaims plan = maybe [] siClaims (Map.lookup SliceCtx (planSlices plan))


refsProjection :: [(Text, Text)] -> Either Problem Bool
refsProjection params = case lookup "project" params of
  Nothing -> Right False
  Just "refs" -> Right True
  Just other -> Left (badRequest ["unknown projection: " <> other])


projectRecords :: Bool -> [Record] -> [Record]
projectRecords False rs = rs
projectRecords True rs = map unch rs
  where
    unch = \case
      REntity er -> RUnchanged (erId er) (erVer er)
      r -> r


bumpTenure :: Origin -> Text -> IO Bool
bumpTenure o h = do
  n <- atomically $ do
    m <- readTVar (oTenure o)
    let n = 1 + Map.findWithDefault 0 h m
    writeTVar (oTenure o) (Map.insert h n m)
    pure n
  -- §19.3 @lattice.tenure.promotions@: count the crossing, not every hit.
  when (n == 3) (countTenurePromotion (ocTelemetry (oConfig o)))
  pure (n >= 3)


cacheControlFor :: SliceName -> Intro -> Bool -> ByteString
cacheControlFor slice intro promoted
  | Oneshot <- intro = "no-store"
  | otherwise = case slice of
      SlicePub ->
        if promoted
          then "public, s-maxage=300, stale-while-revalidate=60, stale-if-error=600"
          else "public, s-maxage=15, stale-while-revalidate=60, stale-if-error=600"
      SliceCtx ->
        if promoted
          then "public, s-maxage=120, stale-while-revalidate=30, stale-if-error=600"
          else "public, s-maxage=15, stale-while-revalidate=30, stale-if-error=600"
      SlicePriv -> "private, max-age=30"
      SlicePlan -> "public, s-maxage=60"


varyPriv :: SliceName -> Headers
varyPriv = \case
  SlicePriv -> [(hVary, "Authorization")]
  _ -> []


{- | The manifest etag input, exactly as pinned: @canonicalJson@ of
@[planId, {sorted var bindings}, vc payload or \"\", [[id, ver]…]]@ with
the id\/ver pairs sorted (tombstone versions included, elisions as @\"\"@).
A response touching @on read@ derived fields (§3.7 Validators) appends a
fifth element, the sorted witness array ('witnessValue'; exact bytes
pinned in "Lattice.Server.Execute"); an empty witness keeps the
four-element input, so responses without derived fields hash exactly as
before.
-}
manifestEtag :: Plan -> Map VarName A.Value -> Text -> [(Text, Text)] -> Set.Set WitnessEntry -> Text
manifestEtag plan vars vcRaw idvers witness =
  manifestEtagHash . canonicalJson . A.Array . V.fromList $
    [ A.String (planId plan)
    , A.Object (KM.fromList (map (\(VarName n, v) -> (AK.fromText n, v)) (Map.toAscList vars)))
    , A.String vcRaw
    , A.toJSON (map (\(i, v) -> [i, v]) idvers)
    ]
      <> (if Set.null witness then [] else [witnessValue witness])


weakEtag :: Text -> ByteString
weakEtag etag = "W/\"" <> TE.encodeUtf8 etag <> "\""


-- | Weak comparison against every member of @If-None-Match@.
inmMatch :: Request -> ByteString -> Bool
inmMatch req current = case lookupHeader hIfNoneMatch (requestHeaders req) of
  Nothing -> False
  Just v -> v == "*" || any matches (BS8.split ',' v)
  where
    target = unquote (stripWeak current)
    matches t = unquote (stripWeak (trim t)) == target
    trim = BS8.dropWhile (== ' ') . fst . BS8.spanEnd (== ' ')
    stripWeak t = fromMaybe t (BS.stripPrefix "W/" t)
    unquote t
      | BS.length t >= 2, BS8.head t == '"', BS8.last t == '"' = BS.init (BS.tail t)
      | otherwise = t


coarsenKeys :: Budgets -> Text -> ExecResult -> [SurrogateKey]
coarsenKeys budgets pid xr =
  if length full <= cap
    then full
    else take cap (base <> Set.toAscList (xrEntityKeys xr `Set.difference` xrCovered xr))
  where
    cap = fromIntegral (maxSurrogateKeys budgets) :: Int
    base = dedupOrd (planKey pid : xrCollectionKeys xr)
    full = base <> Set.toAscList (xrEntityKeys xr)


introUrl :: Text -> Plan -> SliceName -> Map VarName A.Value -> Bool -> ByteString
introUrl h plan slice vars refsOnly =
  "/q/" <> TE.encodeUtf8 h <> "?" <> BS.intercalate "&" (map one pairs)
  where
    pairs =
      [("p", planId plan), ("slice", renderSlice slice)]
        <> map (\(VarName n, v) -> (n, valueToUrlParam v)) (Map.toAscList vars)
        <> (if refsOnly then [("project", "refs")] else [])
    one (k, v) = TE.encodeUtf8 k <> "=" <> encodeQueryComponent (TE.encodeUtf8 v)


-- ---------------------------------------------------------------------------
-- Live queries (spec §12)
-- ---------------------------------------------------------------------------

{- | Is this hash-form GET a live subscription? The @live@ parameter is
authoritative (@live=sse@ subscribes, any other value is a 400); absent
the parameter, an @Accept: text\/event-stream@ header is honored as a
hint (§12's example sends both).
-}
liveRequested :: Request -> [(Text, Text)] -> Either Problem Bool
liveRequested req params = case lookup "live" params of
  Just "sse" -> Right True
  Just other -> Left (badRequest ["unknown live mode: " <> other])
  Nothing ->
    Right $ case lookupHeader hAccept (requestHeaders req) of
      Just accept -> "text/event-stream" `BS.isInfixOf` accept
      Nothing -> False


-- | Live subscribers currently registered (the 'ocLive' cap's meter).
originLiveSubscribers :: Origin -> IO Int
originLiveSubscribers = liveSubscriberCount . oLive


{- | Serve a §12 subscription: @GET \/q\/{hash}?slice=…&live=sse@.

Only memoized hash-form URLs subscribe (an unknown hash is the ordinary
404); admission, variable binding, and @project=refs@ are exactly the
pull path's. Pins where the spec leaves latitude:

* No @slice@ parameter subscribes @pub@ — the pull default (the plan
  pseudo-slice) has no membership to watch, and an explicit
  @slice=plan@ is a 400.
* @X-Have@ \/ @X-Have-Digest@ are ignored: elision against a moving
  store cannot stay coherent across deltas.
* A degraded live execution does __not__ self-purge (the pull path's
  §9.4.5 behavior) — a subscription purging its own registered keys
  would re-trigger itself forever.
* @Last-Event-ID@ on reconnect is accepted and ignored: every
  (re)connect gets a fresh snapshot burst; the delta event ids exist so
  a smarter origin /could/ resume, not so correctness depends on it.
* The response carries no @Lattice-Snapshot@\/@Surrogate-Key@\/@ETag@
  headers: header facts freeze at subscription time while the stream
  moves, and live queries bypass shared caches by nature (§12).
* Over the 'liveMaxSubscribers' cap the origin answers
  @503 lattice:live-over-capacity@.
-}
serveLiveQuery :: Origin -> Request -> [(Text, Text)] -> Text -> IO Response
serveLiveQuery o req params h = withMemo o req h $ \(c, plan) ->
  case lookup "p" params of
    Just pid
      | pid /= planId plan ->
          pure . problemResponse req $
            (mkProblem 409 "lattice:plan-superseded")
              {pExtra = ["plan" .= RPlan (planSliceRecord plan)]}
    _ -> case lookup "slice" params of
      Just "page" -> serveLiveTarget o req params c plan LivePage
      mslice -> case maybe (Right SlicePub) parseSliceParam mslice of
        Left p -> pure (problemResponse req p)
        Right SlicePlan ->
          pure (problemResponse req (badRequest ["live queries subscribe data slices, not the plan pseudo-slice"]))
        Right slice -> serveLiveTarget o req params c plan (LiveSlice slice)


{- | The shared §12 subscription body, per watch target. A page target
admits every nonempty slice strictly (§12 page subscriptions), keys its
single-flight group on the private credential whenever the plan's priv
slice is nonempty (the page re-entangles the audiences that per-slice
fanout keeps separate), and composes every burst under a single snapshot
via 'executePageOnce'.
-}
serveLiveTarget :: Origin -> Request -> [(Text, Text)] -> Compiled -> Plan -> LiveTarget -> IO Response
serveLiveTarget o req params c plan target = do
  let outcome = do
        refsOnly <- refsProjection params
        vars <- bindVariables (ocSchema cfg) (planVars plan) params
        pure (refsOnly, vars)
  case outcome of
    Left p -> pure (problemResponse req p)
    Right (refsOnly, vars) ->
      admitTarget >>= \case
        Left p -> pure (problemResponse req p)
        Right creds -> do
          void (bumpTenure o (compiledHash c))
          let key =
                LiveKey
                  { lkHash = compiledHash c
                  , lkTarget = target
                  , lkVars =
                      canonicalJsonText
                        (A.Object (KM.fromList (map (\(VarName n, v) -> (AK.fromText n, v)) (Map.toAscList vars))))
                  , lkClaims = claimsComponent creds
                  , lkAuth = authComponent creds
                  }
              mExpiry = do
                verifier <- ocVerifier cfg
                vcText <- lookup "vc" params
                proofExpiry verifier (TE.encodeUtf8 vcText) (lookupHeader hVcAuth (requestHeaders req))
              busSub = do
                rd <- subscribeInvalidations o
                pure ((\ev -> (ieCursor ev, ieKeys ev)) <$> rd)
              execOnce = case target of
                LiveSlice slice -> case find (\(s, _, _) -> s == slice) creds of
                  Just (_, claims, vcRaw) ->
                    fmap Just <$> liveExecuteOnce o c plan slice vars vcRaw claims refsOnly
                  Nothing -> pure (Left (mkProblem 500 "lattice:internal"))
                LivePage -> executePageOnce o c plan vars refsOnly creds
          liveSubscribe (ocLive cfg) (oLive o) key busSub execOnce mExpiry (ocNow cfg) >>= \case
            Left LiveOverCapacity ->
              pure . problemResponse req $
                (mkProblem 503 "lattice:live-over-capacity")
                  {pDetail = Just "live subscriber cap reached; retry later or against another instance"}
            Left LiveContention ->
              pure . problemResponse req $
                (mkProblem 503 "lattice:snapshot-contention")
                  { pDetail = Just "could not compose the initial page snapshot under a single token; retry shortly"
                  , pHeaders = [(hRetryAfter, "1")]
                  }
            Left (LiveExecRefused p) -> pure (problemResponse req p)
            Right sub ->
              pure
                Response
                  { responseStatus = Status 200
                  , responseVersion = requestVersion req
                  , responseHeaders =
                      [ (hContentType, "text/event-stream")
                      , (hCacheControl, "no-store")
                      , planHdr plan
                      , schemaHdr o
                      ]
                        <> vary
                  , responseBody = sseResponseBodyFrames (lsubSource sub)
                  , responseTrailers = pure []
                  , responseH2StreamId = 0
                  , responseCancel = lsubClose sub
                  , responsePushPromises = pure []
                  }
  where
    cfg = oConfig o

    targetSlices = case target of
      LiveSlice s -> [s]
      LivePage -> pageSlices plan

    admitTarget = go [] targetSlices
      where
        go acc [] = pure (Right (reverse acc))
        go acc (s : rest) =
          admitSlice o req params (requiredClaims plan) s >>= \case
            Left p -> pure (Left p)
            Right (claims, vcRaw) -> go ((s, claims, vcRaw) : acc) rest

    claimsComponent creds = case target of
      LiveSlice _ -> case creds of
        ((_, _, r) : _) -> r
        [] -> ""
      LivePage -> maybe "" (\(_, _, r) -> r) (find (\(s, _, _) -> s == SliceCtx) creds)

    -- §12: a page group's identity includes the principal when priv is in
    -- play; per-slice groups keep the pre-existing audience sharing.
    authComponent creds = case target of
      LivePage
        | SlicePriv `elem` map (\(s, _, _) -> s) creds ->
            maybe "" (b64url . blake3) (lookupHeader hAuthorization (requestHeaders req))
      _ -> ""

    vary = case target of
      LiveSlice s -> varyPriv s
      LivePage -> if SlicePriv `elem` targetSlices then varyPriv SlicePriv else []


{- | One §12 (re-)execution: the pull pipeline's execute-and-frame core
('serveDataSlice') minus HTTP concerns — no tenure bump, no digest
elision, no degraded self-purge (see 'serveLiveQuery' pins). The
registered key set is the pull response's @Surrogate-Key@ set
('coarsenKeys' output, coarsening included: a coarsened plan key makes
re-execution /more/ eager, never blind).
-}
liveExecuteOnce ::
  Origin ->
  Compiled ->
  Plan ->
  SliceName ->
  Map VarName A.Value ->
  Text ->
  Claims ->
  Bool ->
  IO (Either Problem LiveSnapshot)
liveExecuteOnce o c plan slice vars vcRaw claims refsOnly = do
  let cfg = oConfig o
      env =
        ExecEnv
          { xSchema = ocSchema cfg
          , xBudgets = ocBudgets cfg
          , xBackend = ocBackend cfg
          , xClaims = claims
          , xVars = vars
          , xMode = EmitEq slice
          , xTelemetry = ocTelemetry cfg
          , xProjections = planProjections (ocSchema cfg) plan
          }
  executeRoots env (planRoots plan) >>= \case
    Left AbortCursorRetired -> pure (Left (mkProblem 410 "lattice:cursor-retired"))
    Left AbortCursorMalformed -> pure (Left (badRequest ["malformed cursor"]))
    Left (AbortBudget d) -> pure (Left (badRequest [d]))
    Right xr -> do
      let etag = manifestEtag plan vars vcRaw (xrIdVers xr) (xrWitness xr)
      pure . Right $
        LiveSnapshot
          { lsSections =
              [ LiveSection
                  { lsecSlice = slice
                  , lsecManifest =
                      Manifest
                        { mQuery = Just (compiledHash c)
                        , mMutation = Nothing
                        , mPlan = Just (planId plan)
                        , mSlice = Just slice
                        , mRoot = xrRootMap xr
                        , mEtag = etag
                        , mBatch = Nothing
                        }
                  , lsecRecords = projectRecords refsOnly (xrRecords xr)
                  , lsecRoot = xrRootMap xr
                  , lsecEtag = etag
                  }
              ]
          , lsKeys = Set.fromList (coarsenKeys (ocBudgets cfg) (planId plan) xr)
          , lsComplete = xrComplete xr
          }


{- | One §12 page (re-)execution: every nonempty slice composed under a
single per-domain snapshot via the same optimistic validation as
'servePage' (model: @tla/LatticePageComposition.tla@). @Right Nothing@
means contention persisted through the bounded retries: refused at
subscribe time (@503 lattice:snapshot-contention@), skipped on a delta —
the intersecting write that caused it has already queued the group's next
trigger, so the skip is self-healing.
-}
executePageOnce ::
  Origin ->
  Compiled ->
  Plan ->
  Map VarName A.Value ->
  Bool ->
  [(SliceName, Claims, Text)] ->
  IO (Either Problem (Maybe LiveSnapshot))
executePageOnce o c plan vars refsOnly creds = attempt pageComposeAttempts
  where
    cfg = oConfig o

    attempt n = do
      t0 <- beSnapshot (ocBackend cfg)
      execAll [] creds >>= \case
        Left p -> pure (Left p)
        Right secs -> do
          t1 <- beSnapshot (ocBackend cfg)
          if t0 == t1
            then pure (Right (Just (toSnapshot secs)))
            else do
              countPageContention (ocTelemetry cfg)
              if n > 1 then attempt (n - 1) else pure (Right Nothing)

    execAll acc [] = pure (Right (reverse acc))
    execAll acc ((slice, claims, vcRaw) : rest) = do
      let env =
            ExecEnv
              { xSchema = ocSchema cfg
              , xBudgets = ocBudgets cfg
              , xBackend = ocBackend cfg
              , xClaims = claims
              , xVars = vars
              , xMode = EmitEq slice
              , xTelemetry = ocTelemetry cfg
              , xProjections = planProjections (ocSchema cfg) plan
              }
      executeRoots env (planRoots plan) >>= \case
        Left AbortCursorRetired -> pure (Left (mkProblem 410 "lattice:cursor-retired"))
        Left AbortCursorMalformed -> pure (Left (badRequest ["malformed cursor"]))
        Left (AbortBudget d) -> pure (Left (badRequest [d]))
        Right xr -> execAll ((slice, vcRaw, xr) : acc) rest

    toSnapshot secs =
      LiveSnapshot
        { lsSections = map toSection secs
        , lsKeys = Set.fromList (concatMap (\(_, _, xr) -> coarsenKeys (ocBudgets cfg) (planId plan) xr) secs)
        , lsComplete = all (\(_, _, xr) -> xrComplete xr) secs
        }

    toSection (slice, vcRaw, xr) =
      let etag = manifestEtag plan vars vcRaw (xrIdVers xr) (xrWitness xr)
      in LiveSection
           { lsecSlice = slice
           , lsecManifest =
               Manifest
                 { mQuery = Just (compiledHash c)
                 , mMutation = Nothing
                 , mPlan = Just (planId plan)
                 , mSlice = Just slice
                 , mRoot = xrRootMap xr
                 , mEtag = etag
                 , mBatch = Nothing
                 }
           , lsecRecords = projectRecords refsOnly (xrRecords xr)
           , lsecRoot = xrRootMap xr
           , lsecEtag = etag
           }


-- ---------------------------------------------------------------------------
-- The invalidation feed (spec §18.6)
-- ---------------------------------------------------------------------------

{- | @GET \/invalidations?since={outboxCursor}&live=sse@ (§18.6): the
origin's subscribable invalidation feed, straight off the §11.5 bus. Each
event is one JSON object @{\"cursor\": n, \"keys\": [\"Post:17\", …]}@ with
SSE @id: {cursor}@ — the same framing as §12, keep-alive @: ping@ comments
included ('livePingMicros'; the feed reuses the live-query knob).

* @since@ replays every retained event with a cursor greater than the
  parameter ('invalidationsSince'), then continues with the live tail,
  subscribed atomically with the replay cut. A consumer whose first
  replayed cursor exceeds @since + 1@ knows the bounded window was outrun
  and resyncs from scratch rather than trusting the gap.
* @since@ absent subscribes the live tail only. A non-natural @since@ is
  a 400.
* Without @live=sse@ the request is a 400: the polling form is out of
  scope for the reference implementation (the feed is a push surface;
  resumability comes from @since@, not from polling).
* The reference implementation leaves the feed UNGATED — invalidation
  keys are cache metadata, not entity data, but they do reveal write
  activity, so deployments SHOULD gate the route at the proxy\/service
  tier (the gateway subscribes as a service principal there).
-}
serveInvalidations :: Origin -> Request -> [(Text, Text)] -> IO Response
serveInvalidations o req params = case lookup "live" params of
  Just "sse" -> case traverse parseSince (lookup "since" params) of
    Nothing ->
      pure (problemResponse req (badRequest ["malformed since cursor: a non-negative integer is required"]))
    Just mSince -> do
      (missed, nextEv) <- case mSince of
        Just n -> invalidationsSince o n
        Nothing -> ([],) <$> subscribeInvalidations o
      backlog <- newIORef (SseComment "lattice invalidations" : map invalFrame missed)
      pure
        Response
          { responseStatus = Status 200
          , responseVersion = requestVersion req
          , responseHeaders =
              [ (hContentType, "text/event-stream")
              , (hCacheControl, "no-store")
              , schemaHdr o
              ]
          , responseBody = sseResponseBodyFrames (popFeedFrame o backlog nextEv)
          , responseTrailers = pure []
          , responseH2StreamId = 0
          , responseCancel = pure ()
          , responsePushPromises = pure []
          }
  _ ->
    pure
      ( problemResponse
          req
          (badRequest ["the invalidation feed is SSE-only; subscribe with live=sse (§18.6)"])
      )
  where
    parseSince :: Text -> Maybe Word64
    parseSince t = A.decodeStrict (TE.encodeUtf8 t)


-- | One §18.6 feed event: @{\"cursor\": n, \"keys\": […]}@, @id: {cursor}@.
invalFrame :: InvalEvent -> SseFrame
invalFrame ev =
  SseDispatch
    ServerSentEvent
      { sseEventType = Nothing
      , sseEventId = Just (BS8.pack (show (ieCursor ev)))
      , sseData =
          BL.toStrict (A.encode (A.object ["cursor" .= ieCursor ev, "keys" .= ieKeys ev]))
      }


{- | The feed's frame source: drain the replay backlog (leading comment
included), then block on the live tail, emitting a @: ping@ comment per
idle 'livePingMicros' period (disabled at @<= 0@, like §12). The source
never ends of its own accord — teardown is the client's disconnect, and
the bus subscription is just a private 'TChan' handed to GC with the
response.
-}
popFeedFrame :: Origin -> IORef [SseFrame] -> STM InvalEvent -> IO (Maybe SseFrame)
popFeedFrame o backlog nextEv = do
  queued <- readIORef backlog
  case queued of
    f : rest -> do
      writeIORef backlog rest
      pure (Just f)
    []
      | pingMicros <= 0 -> Just . invalFrame <$> atomically nextEv
      | otherwise -> do
          timer <- registerDelay pingMicros
          next <-
            atomically $
              (Just <$> nextEv) `orElse` (readTVar timer >>= check >> pure Nothing)
          pure . Just $ case next of
            Just ev -> invalFrame ev
            Nothing -> SseComment "ping"
  where
    pingMicros = livePingMicros (ocLive (oConfig o))


-- ---------------------------------------------------------------------------
-- Variable binding (spec §6.1)
-- ---------------------------------------------------------------------------

reservedParams :: [Text]
reservedParams = ["p", "slice", "vc", "project", "live", "d", "dv", "intent"]


bindVariables :: Schema -> [VarDef] -> [(Text, Text)] -> Either Problem (Map VarName A.Value)
bindVariables schema vdefs params = do
  let declared = map (unVarName . vdName) vdefs
      unknown = filter (\(k, _) -> k `notElem` reservedParams && k `notElem` declared) params
  unless (null unknown) $
    Left (badRequest (map (\(k, _) -> "unknown query parameter: " <> k) unknown))
  bound <- traverse bindOne vdefs
  Right (Map.fromList (catMaybes bound))
  where
    bindOne vd = case lookup (unVarName (vdName vd)) params of
      Just txt -> case urlParamToValue (textyVar schema (vdType vd)) txt of
        Left e -> Left (badRequest ["invalid value for $" <> unVarName (vdName vd) <> ": " <> e])
        Right v -> checked vd v
      Nothing -> case vdDefault vd >>= qvalueToJson of
        Just v -> checked vd v
        Nothing
          | trOptional (vdType vd) -> Right Nothing
          | otherwise -> Left (badRequest ["missing required variable $" <> unVarName (vdName vd)])
    -- §3.5.2: a variable binding an empty array where its declared
    -- (newtype-resolved) type is a nonempty list is rejected at the request.
    checked vd v
      | violatesList1 schema (varFieldType (vdType vd)) v =
          Left (badRequest ["invalid value for $" <> unVarName (vdName vd) <> ": an empty array violates its nonempty list type"])
      | otherwise = Right (Just (vdName vd, v))
    varFieldType tr =
      (if trOptional tr then TOptional else id) (TNamed (TypeName (trName tr)))


{- | Is a variable's declared type texty (bound verbatim rather than
interpreted)? Text\/Uuid\/Cursor\/Timestamp\/Date\/TimeOfDay\/Duration,
enums, and newtypes over any of those, resolved through 'schemaTypes'
(alias-normalized, depth-bounded against pathological chains).
-}
textyVar :: Schema -> TypeRefQ -> Bool
textyVar schema tr = goName (8 :: Int) (normalizeTypeAlias (trName tr))
  where
    goName n name
      | n <= 0 = False
      | name `elem` textyPrims = True
      | name `elem` otherPrims = False
      | otherwise = case Map.lookup (TypeName name) (schemaTypes schema) of
          Just (DeclEnum _ _) -> True
          Just (DeclNewtype ft _) -> goType (n - 1) ft
          _ -> False
    goType n = \case
      TOptional f -> goType n f
      TPrim p -> p `elem` [PText, PUuid, PCursor, PTimestamp, PDate, PTimeOfDay, PDuration]
      TNamed (TypeName t) -> goName (n - 1) (normalizeTypeAlias t)
      _ -> False
    textyPrims = ["Text", "Uuid", "Cursor", "Timestamp", "Date", "TimeOfDay", "Duration"]
    otherPrims =
      [ "Bool"
      , "I8"
      , "I16"
      , "I32"
      , "I64"
      , "W8"
      , "W16"
      , "W32"
      , "W64"
      , "Integer"
      , "Decimal"
      , "F32"
      , "F64"
      , "Bytes"
      , "Json"
      ]


-- ---------------------------------------------------------------------------
-- Admission (spec §8.2)
-- ---------------------------------------------------------------------------

{- | Admit a request to a slice: @ctx@ requires the @vc@ parameter (proof
checked via 'ocVerifier' when configured) and coverage of the required
claim names; @priv@ requires an @Authorization@ header (a @vc@ presented
alongside is decoded so gated-edge predicates can evaluate, but never
joins the etag input). Returns the claims and the raw @vc@ payload text
(the manifest-etag component; empty except on @ctx@).
-}
admitSlice ::
  Origin ->
  Request ->
  [(Text, Text)] ->
  [ClaimName] ->
  SliceName ->
  IO (Either Problem (Claims, Text))
admitSlice o req params required = \case
  SlicePub -> pure (Right (Map.empty, ""))
  SlicePlan -> pure (Right (Map.empty, ""))
  SliceCtx -> case lookup "vc" params of
    Nothing -> pure (Left (authProblem "the ctx slice requires the vc parameter"))
    Just vcText ->
      verifyVc o req vcText >>= \case
        Left p -> pure (Left p)
        Right payload -> do
          let missing = filter (\cn -> not (Map.member cn (cpClaims payload))) required
          if null missing
            then pure (Right (cpClaims payload, lenientText (cpRaw payload)))
            else
              pure . Left . authProblem $
                "presented claims do not cover the slice: missing "
                  <> T.intercalate ", " (map unClaimName missing)
  SlicePriv -> case lookupHeader hAuthorization (requestHeaders req) of
    Nothing -> pure (Left (authProblem "the priv slice requires an Authorization header"))
    Just _ -> case lookup "vc" params of
      Nothing -> pure (Right (Map.empty, ""))
      Just vcText ->
        verifyVc o req vcText >>= \case
          Left p -> pure (Left p)
          Right payload -> pure (Right (cpClaims payload, ""))


authProblem :: Text -> Problem
authProblem d = (mkProblem 401 "lattice:proof-expired") {pDetail = Just d}


verifyVc :: Origin -> Request -> Text -> IO (Either Problem ClaimsPayload)
verifyVc o req vcText = case decodeClaims vcText of
  Left e -> pure (Left (authProblem ("invalid vc payload: " <> e)))
  Right payload -> case ocVerifier (oConfig o) of
    Nothing -> pure (Right payload)
    Just v -> do
      res <- verifyProof v (cpRaw payload) (lookupHeader hVcAuth (requestHeaders req))
      pure $ case res of
        Left _ -> Left (authProblem "visibility proof missing, invalid, or expired")
        Right () -> Right payload


-- ---------------------------------------------------------------------------
-- Point fetches (spec §6.7)
-- ---------------------------------------------------------------------------

{- | The backend point fetches execute against: 'beLoad' routed through
the origin's coalescer when one is configured (§6.9). Row loads from
concurrent point fetches accumulate into per-type windows and flush as
one 'beLoad' per window; everything per-response — masks, visibility,
@ETag@, @ver@ pinning — still happens after the batch, per response, in
'entityResponse' (loads are policy-free, rendering is policy-full).
Traversal loads reached from a point fetch (bounded edges) coalesce too:
they are the same loaders queries use. With 'ocCoalesce' unset this is
exactly 'ocBackend', byte-identical behavior.
-}
coalescedBackend :: Origin -> Backend
coalescedBackend o = case oCoalescer o of
  Nothing -> be
  -- The point-fetch path always passes ProjectAll ('serveEntity',
  -- mutation output), so every join of a window carries the same
  -- projection — the coalescer's loader-equivalence contract holds by
  -- construction. Pin it here so a future non-ProjectAll caller cannot
  -- silently mix projections inside one window.
  Just cz -> be {beLoad = \ty _proj -> coalescedLoadMany cz ty (beLoad be ty ProjectAll)}
  where
    be = ocBackend (oConfig o)


serveEntity :: Origin -> Request -> [(Text, Text)] -> Text -> Text -> IO Response
serveEntity o req params tyText key = do
  let cfg = oConfig o
      schema = ocSchema cfg
      ty = TypeName tyText
  case lookupEntity schema ty of
    Nothing -> pure (problemResponse req vanish)
    Just ent -> case entityFetchBy ent of
      Nothing -> pure (problemResponse req vanish)
      Just fetchPol -> do
        let base = policyLevel fetchPol
            hasAuth = isJust (lookupHeader hAuthorization (requestHeaders req))
            hasVc = isJust (lookup "vc" params)
            callerSlice
              | hasAuth = SlicePriv
              | hasVc = SliceCtx
              | otherwise = SlicePub
        case maskFor schema ent base callerSlice ty params of
          Left p -> pure (problemResponse req p)
          Right (node, needSlice) ->
            admitSlice o req params [] needSlice >>= \case
              Left p -> pure (problemResponse req p)
              Right (claims, _) -> do
                let ref = Ref ty key
                    env =
                      ExecEnv
                        { xSchema = schema
                        , xBudgets = ocBudgets cfg
                        , xBackend = coalescedBackend o
                        , xClaims = claims
                        , xVars = Map.empty
                        , xMode = EmitAtMost needSlice
                        , xTelemetry = ocTelemetry cfg
                        , -- Point fetches render the whole visible entity
                          -- (§6.7 masks): loads are ProjectAll by design.
                          xProjections = Map.empty
                        }
                executeSeeds env [(ref, node)] >>= \case
                  Left _ ->
                    pure (problemResponse req (badRequest ["cursor arguments are not accepted on point fetches"]))
                  Right xr -> entityResponse o req params ref xr needSlice
  where
    vanish = mkProblem 404 "lattice:not-found"


entityResponse ::
  Origin ->
  Request ->
  [(Text, Text)] ->
  Ref ->
  ExecResult ->
  SliceName ->
  IO Response
entityResponse o req params ref xr needSlice = do
  snap <- beSnapshot (ocBackend cfg)
  -- §3.7 Invalidation: derived deps' entity keys and aggregate collection
  -- tags join the point fetch's own key so existing write sets purge
  -- derived values.
  let keys =
        dedupOrd
          (entityKeyOf ref : (xrCollectionKeys xr <> Set.toAscList (xrEntityKeys xr)))
  flr <- floorTokenFor o keys snap
  let common etag =
        catMaybes
          [ Just (snapshotHdr cfg snap)
          , Just (floorHdr cfg flr)
          , Just (schemaHdr o)
          , Just (keysHdr keys)
          , (\e -> (hETag, strongEtag e)) <$> etag
          ]
      frame inner etag =
        [ RManifest
            Manifest
              { mQuery = Nothing
              , mMutation = Nothing
              , mPlan = Nothing
              , mSlice = Nothing
              , mRoot = Map.singleton "node" [ref]
              , mEtag = fromMaybe "" etag
              , mBatch = Nothing
              }
        ]
          <> inner
          <> [REnd (EndRecord True etag)]
  case () of
    ()
      | Just e <- firstErr ->
          pure . problemResponse req $
            Problem
              { pStatus = if errRetryable e then 503 else 500
              , pCode = fromMaybe "lattice:internal" (errCode e)
              , pDetail = errMessage e
              , pDiagnostics = []
              , pExtra = []
              , pHeaders = if errRetryable e then [(hRetryAfter, "1")] else []
              , pCache = "no-store"
              }
      | Just v <- tombVer -> case pinned of
          Just want
            | want /= v -> pure (problemResponse req (versionUnavailable req params ref))
          _ ->
            pure $
              mkResponse
                req
                410
                ([(hContentType, ndjsonType), (hCacheControl, unpinnedCC)] <> common (Just v))
                (encodeRecords (frame [RTombstone ref v Nothing] (Just v)))
      | Just er <- entRec -> do
          let v = erVer er
              etagVal = validatorOf v
              inner = REntity er : fieldErrs
          case pinned of
            Just want
              | want /= v -> pure (problemResponse req (versionUnavailable req params ref))
              | otherwise ->
                  -- §3.7: a pinned fetch pins row bytes, but a derived
                  -- value's deps move independently of @ver@ — with a
                  -- nonempty witness the response cannot be immutable.
                  pure $
                    mkResponse
                      req
                      200
                      ([(hContentType, ndjsonType), (hCacheControl, ccOf pinnedCC)] <> common (Just etagVal))
                      (encodeRecords (frame inner (Just etagVal)))
            Nothing
              | inmMatch req (strongEtag etagVal) ->
                  pure (mkResponse req 304 ((hCacheControl, ccOf unpinnedCC) : common (Just etagVal)) "")
              | otherwise ->
                  pure $
                    mkResponse
                      req
                      200
                      ([(hContentType, ndjsonType), (hCacheControl, ccOf unpinnedCC)] <> common (Just etagVal))
                      (encodeRecords (frame inner (Just etagVal)))
      | elided ->
          case pinned of
            Just _ -> pure (problemResponse req (versionUnavailable req params ref))
            Nothing ->
              pure $
                mkResponse
                  req
                  200
                  ([(hContentType, ndjsonType), (hCacheControl, "private, max-age=30")] <> common Nothing)
                  (encodeRecords (frame [RElided ref] Nothing))
      | otherwise -> pure (problemResponse req (mkProblem 404 "lattice:not-found"))
  where
    cfg = oConfig o
    recs = xrRecords xr
    witness = xrWitness xr
    -- §3.7 Validators: a point fetch touching derived fields validates
    -- with @hash(ver, witness)@ ('witnessEtag') instead of the bare ver.
    validatorOf v
      | Set.null witness = v
      | otherwise = witnessEtag v witness
    -- §9.4: an entity present with a derived field unavailable is
    -- delivered, field absent, plus its Field-scoped error records; only
    -- non-field errors fail the whole fetch.
    fieldErrs =
      mapMaybe
        ( \case
            r@(RError ErrorRecord {errScope = Just (ScopeField _ _)}) -> Just r
            _ -> Nothing
        )
        recs
    ccOf base = if null fieldErrs then base else "no-store"
    pinnedCC = if Set.null witness then immutableCC else unpinnedCC
    firstErr =
      listToMaybe $
        mapMaybe
          ( \case
              RError e
                | Just (ScopeField _ _) <- errScope e -> Nothing
                | otherwise -> Just e
              _ -> Nothing
          )
          recs
    entRec =
      listToMaybe $
        mapMaybe
          ( \case
              REntity er | erId er == ref -> Just er
              _ -> Nothing
          )
          recs
    tombVer =
      listToMaybe $
        mapMaybe
          ( \case
              RTombstone r v _ | r == ref -> Just v
              _ -> Nothing
          )
          recs
    elided =
      any
        ( \case
            RElided r -> r == ref
            _ -> False
        )
        recs
    pinned = lookup "ver" params
    unpinnedCC =
      if needSlice == SlicePub
        then "public, s-maxage=300, stale-while-revalidate=60"
        else "private, max-age=30"


strongEtag :: Text -> ByteString
strongEtag v = "\"" <> TE.encodeUtf8 v <> "\""


versionUnavailable :: Request -> [(Text, Text)] -> Ref -> Problem
versionUnavailable _req params ref =
  (mkProblem 404 "lattice:version-unavailable")
    { pDetail = Just "the origin no longer holds this version; refetch unpinned"
    , pHeaders = [(hContentLocation, unpinnedUrl)]
    }
  where
    unpinnedUrl =
      "/e/"
        <> encodePathSegment (TE.encodeUtf8 (unTypeName (refType ref)))
        <> "/"
        <> encodePathSegment (TE.encodeUtf8 (refKey ref))
        <> renderQ (filter (\(k, _) -> k /= "ver") params)
    renderQ [] = ""
    renderQ ps =
      "?"
        <> BS.intercalate
          "&"
          (map (\(k, v) -> TE.encodeUtf8 k <> "=" <> encodeQueryComponent (TE.encodeUtf8 v)) ps)


-- ---------------------------------------------------------------------------
-- Point-fetch masks (spec §6.7)
-- ---------------------------------------------------------------------------

type MaskItem = (FieldName, [(ArgName, A.Value)])


{- | Resolve the request's mask to a node selection and the slice it
requires: a schema fragment, an ad hoc @f=@ list, or (neither) every
field at or below the caller's presented level.
-}
maskFor ::
  Schema ->
  EntityDef ->
  Level ->
  SliceName ->
  TypeName ->
  [(Text, Text)] ->
  Either Problem (NodeSelection, SliceName)
maskFor schema ent base callerSlice ty params =
  case (lookup "fragment" params, lookup "f" params) of
    (Just fname, _) -> do
      fd <-
        maybe
          (Left (badRequest ["unknown fragment: " <> fname]))
          Right
          (Map.lookup (FragmentName fname) (schemaFragments schema))
      unless (fragmentApplies fd) $
        Left (badRequest ["fragment " <> fname <> " is not declared on " <> unTypeName ty])
      buildMask schema ent base False (canonicalizeMask ent (fragmentMaskItems fd))
    (Nothing, Just fmask) -> do
      items <- either (\e -> Left (badRequest [e])) Right (parseMaskText fmask)
      buildMask schema ent base True (canonicalizeMask ent items)
    (Nothing, Nothing) -> do
      node <-
        maybe (Left (mkProblem 404 "lattice:not-found")) Right $
          outputSelection schema callerSlice base ty
      Right (node, callerSlice)
  where
    fragmentApplies fd =
      fragOn fd == unTypeName ty
        || Set.member (InterfaceName (fragOn fd)) (entityImplements ent)
    fragmentMaskItems fd = concatMap (selItems fd) (fragSelection fd)
    selItems fd = \case
      SField f -> [(fName f, mapMaybe (argLit fd) (fArgs f))]
      SInline t ss | t == ty -> concatMap (selItems fd) ss
      _ -> []
    argLit fd (Argument n v) = (,) n <$> resolveQVal fd v
    -- Literals render directly; a variable falls back to the fragment
    -- parameter's declared default (an optional parameter without one
    -- erases the argument — omission is the only spelling of absence).
    resolveQVal fd v = case qvalueToJson v of
      Just j -> Just j
      Nothing -> case v of
        QVar vn -> find ((== vn) . vdName) (fragParams fd) >>= vdDefault >>= qvalueToJson
        _ -> Nothing


{- | Canonicalize a mask: per field, erase default-equal arguments and sort
the rest by name; sort and deduplicate items by canonical field key.
-}
canonicalizeMask :: EntityDef -> [MaskItem] -> [MaskItem]
canonicalizeMask ent = dedupByKey . sortOn keyOf . map canonItem
  where
    canonItem (n, args) =
      let decl = maybe [] fieldArgs (lookupEntityField ent n)
          erase (a, v) = case find ((== a) . adName) decl of
            Just ad
              | Just dv <- qvalueToJson =<< adDefault ad
              , canonicalJson dv == canonicalJson v ->
                  Nothing
            _ -> Just (a, v)
      in (n, sortOn fst (mapMaybe erase args))
    keyOf (n, args) = canonicalFieldKey n args
    dedupByKey = go Set.empty
      where
        go _ [] = []
        go seen (x : xs)
          | Set.member (keyOf x) seen = go seen xs
          | otherwise = x : go (Set.insert (keyOf x) seen) xs


{- | Resolve mask items against the entity: scalars become plan fields,
@has one@ and bounded @has many@ edges become traversal-free plan edges.
@strict@ (an @f=@ mask) rejects unknown fields and paginated edges;
fragment masks skip them (fragments are shared with full queries).
-}
buildMask ::
  Schema ->
  EntityDef ->
  Level ->
  Bool ->
  [MaskItem] ->
  Either Problem (NodeSelection, SliceName)
buildMask _schema ent base strict items = do
  resolved <- catMaybes <$> traverse resolveItem items
  let fields = mapMaybe (either Just (const Nothing)) resolved
      edges = mapMaybe (either (const Nothing) Just) resolved
      levels = map pfLevel fields <> map peLevel edges
      needRank = foldr (max . levelRank) 0 levels
  when (null resolved) $
    Left (badRequest ["empty field mask"])
  Right (NodeSelection fields edges, rankSlice needRank)
  where
    rankSlice = \case
      0 -> SlicePub
      1 -> SliceCtx
      _ -> SlicePriv
    skipOr err = if strict then Left err else Right Nothing
    resolveItem (n, args) = case lookupEntityField ent n of
      Just fd ->
        Right . Just . Left $
          PlanField
            { pfName = n
            , pfArgs = map (\(a, v) -> (a, ArgLit v)) args
            , pfKey = canonicalFieldKey n args
            , pfLevel = joinLevel base (policyLevel (entityFieldPolicy ent fd))
            , pfDerivation = fieldDerivation fd
            }
      Nothing -> case lookupEntityRel ent n of
        Just rel -> case rel of
          ToMany {relCollection = col}
            | Paginated _ <- colWindow col ->
                skipOr (badRequest ["field " <> unFieldName n <> " is a paginated edge; not fetchable via a mask"])
          _
            | not (null args) ->
                Left (badRequest ["edge " <> unFieldName n <> " takes no arguments"])
            | otherwise ->
                Right . Just . Right $
                  PlanEdge
                    { peField = n
                    , peRel = rel
                    , peArgs = []
                    , peKey = unFieldName n
                    , peLevel = joinLevel base (policyLevel (fromMaybe Public (relPolicy rel)))
                    , peDepth = Nothing
                    , peSelection = TypedSelection Map.empty
                    }
        Nothing -> skipOr (badRequest ["unknown field: " <> unFieldName n])


{- | Parse an @f=@ mask: top-level comma-separated items, each
@name@ or @name(arg:value,…)@. Values parse as JSON scalars, bare
words as strings (enum constructors).
-}
parseMaskText :: Text -> Either Text [MaskItem]
parseMaskText t = traverse parseItem (splitTop t)
  where
    splitTop :: Text -> [Text]
    splitTop txt = map T.pack (go (0 :: Int) [] (T.unpack txt))
      where
        go _ acc [] = [reverse acc]
        go d acc (ch : cs)
          | ch == ',' && d == 0 = reverse acc : go 0 [] cs
          | ch == '(' = go (d + 1) (ch : acc) cs
          | ch == ')' = go (d - 1) (ch : acc) cs
          | otherwise = go d (ch : acc) cs
    parseItem raw =
      let s = T.strip raw
      in case T.breakOn "(" s of
          (name, "")
            | isName name -> Right (FieldName name, [])
            | otherwise -> Left ("malformed field mask item: " <> s)
          (name, rest) -> do
            unless (isName name) (Left ("malformed field mask item: " <> s))
            inner <-
              maybe (Left ("malformed field mask item: " <> s)) Right $
                T.stripSuffix ")" (T.drop 1 rest)
            args <- traverse parseArg (splitTop inner)
            Right (FieldName name, args)
    parseArg a = case T.breakOn ":" (T.strip a) of
      (an, av)
        | not (T.null av), isName an -> Right (ArgName an, parseScalar (T.drop 1 av))
        | otherwise -> Left ("malformed argument in field mask: " <> a)
    parseScalar v = fromMaybe (A.String v) (A.decodeStrict (TE.encodeUtf8 v))
    isName n = not (T.null n) && T.all (\c -> isAlphaNum c || c == '_') n


-- ---------------------------------------------------------------------------
-- Mutations (spec §11)
-- ---------------------------------------------------------------------------

data Invocation
  = Singular (Map ArgName A.Value)
  | BatchInv Atomicity [(Text, Map ArgName A.Value)]


-- | Per-item execution result, aggregated into the batch response.
data ItemOut = ItemOut
  { ioRecords :: [Record]
  , ioInvalidated :: Maybe Record
  , ioKeys :: [SurrogateKey]
  , ioResult :: [Ref]
  , ioCommitted :: Bool
  }


serveMutation :: Origin -> Request -> [(Text, Text)] -> MutationName -> IO Response
serveMutation o req params name = withEffectGate o $ do
  let cfg = oConfig o
      schema = ocSchema cfg
  case Map.lookup name (schemaMutations schema) of
    Nothing ->
      pure (problemResponse req ((mkProblem 404 "lattice:not-found") {pDetail = Just "unknown mutation"}))
    Just mdef -> do
      body <- drainBody (requestBody req)
      resolvePrincipal o req params >>= \case
        Left p -> pure (problemResponse req p)
        Right (claims, principalKey) -> do
          let hasAuth = isJust (lookupHeader hAuthorization (requestHeaders req))
              callerSlice
                | hasAuth = SlicePriv
                | not (Map.null claims) = SliceCtx
                | otherwise = SlicePub
          case checkGuard (mutGuard mdef) claims hasAuth of
            Left p -> pure (problemResponse req p)
            Right () -> case parseMutationBody schema mdef body of
              Left p -> pure (problemResponse req p)
              Right invocation -> do
                addMutationAttrs o name mdef
                withIdempotency o req name mdef principalKey body $
                  runMutation o req name mdef claims callerSlice namedCtx invocation


-- ---------------------------------------------------------------------------
-- Verb bindings (spec §11.7, §11.8)
-- ---------------------------------------------------------------------------

-- | Context a bound (verb) invocation adds to the ordinary mutation
-- pipeline: the conditional-request check handed to the effect bracket,
-- and whether a singular success is a creation (@201@ + @Location@).
data BoundCtx = BoundCtx
  { bcPre :: Maybe MutatePrecondition
  , bcCreated :: Bool
  }


-- | The named @POST /m/{name}@ form: unconditional, never a creation
-- response. Preconditions and 201 belong to the entity-space spellings.
namedCtx :: BoundCtx
namedCtx = BoundCtx {bcPre = Nothing, bcCreated = False}


-- | How a bound URL names its target: @/e/{Type}/{key}@ or @/e/{Type}@.
data BindShape = ShapeKeyed | ShapeCollection
  deriving stock (Eq, Ord)


{- | Every entity-space URL shape the schema binds, to its mutation: a
singular binding contributes its own shape, a batch collection binding
(@batch … as VERB \/e\/{Type}@) the collection shape. A creation binding
is itself the collection shape and serves both the singular create and
the array batch (§11.8). Shape uniqueness is an elaboration check, so
this index is well-defined; the schema is small and the fold is cheap
enough to run per request without touching 'Origin'.
-}
bindingIndex :: Schema -> Map (BindVerb, TypeName, BindShape) (MutationName, MutationDef)
bindingIndex schema = Map.fromList (concatMap shapes (Map.toList (schemaMutations schema)))
  where
    shapes (n, mdef) =
      let single = case mutBinding mdef of
            Nothing -> []
            Just b ->
              [((vbVerb b, vbTarget b, maybe ShapeCollection (const ShapeKeyed) (vbKeyArg b)), (n, mdef))]
          batchB = case mutBatch mdef >>= bpBound of
            Nothing -> []
            Just (v, ty) -> [((v, ty, ShapeCollection), (n, mdef))]
       in single <> batchB


{- | Dispatch an entity-space verb request (§11.7): find the bound
mutation, admit the caller exactly as the named form does, decode the
invocation from URL\/headers\/body per verb, and run the ordinary
mutation pipeline — the binding chooses the wire spelling only. An
unbound verb\/URL falls back to 404\/405 like any unrouted path.
-}
serveBound :: Origin -> Request -> [(Text, Text)] -> BindVerb -> Text -> Maybe Text -> IO Response
serveBound o req params verb tyText mkey = withEffectGate o $ do
  let schema = ocSchema (oConfig o)
      ty = TypeName tyText
      shape = maybe ShapeCollection (const ShapeKeyed) mkey
  case Map.lookup (verb, ty, shape) (bindingIndex schema) of
    Nothing -> pure (fallback o req ("e" : tyText : maybeToList mkey))
    Just (name, mdef) -> do
      body <- drainBody (requestBody req)
      resolvePrincipal o req params >>= \case
        Left p -> pure (problemResponse req p)
        Right (claims, principalKey) -> do
          let hasAuth = isJust (lookupHeader hAuthorization (requestHeaders req))
              callerSlice
                | hasAuth = SlicePriv
                | not (Map.null claims) = SliceCtx
                | otherwise = SlicePub
          case checkGuard (mutGuard mdef) claims hasAuth of
            Left p -> pure (problemResponse req p)
            Right () -> case boundRequest schema name mdef verb ty mkey req params body of
              Left resp -> pure resp
              Right (inv, bctx) -> do
                addMutationAttrs o name mdef
                withIdempotency o req name mdef principalKey body $
                  runMutation o req name mdef claims callerSlice bctx inv


{- | Decode a bound invocation: the URL's key segment binds the binding's
key argument, the body binds per verb (PUT: the single non-key argument's
full replacement value; PATCH: a merge-patch into the input record
argument; DELETE: no body; POST: the named form's object-or-array rule),
and the conditional headers become the effect's 'MutatePrecondition'.
-}
boundRequest ::
  Schema ->
  MutationName ->
  MutationDef ->
  BindVerb ->
  TypeName ->
  Maybe Text ->
  Request ->
  [(Text, Text)] ->
  ByteString ->
  Either Response (Invocation, BoundCtx)
boundRequest schema name mdef verb ty mkey req params body = case (verb, mkey) of
  (BindPut, Just key) -> keyed key $ \ka -> do
    arg <- soleNonKeyArg ka
    v <- maybe (Left (problem (badRequest ["PUT body must be valid JSON (the full replacement representation)"]))) Right (A.decodeStrict body)
    args <- argsFor [(ka, A.String key), (adName arg, v)]
    pure (Singular args)
  (BindPatch, Just key) -> keyed key $ \ka -> do
    requireMergePatch
    arg <- soleNonKeyArg ka
    obj <- case A.decodeStrict body of
      Just (A.Object obj) -> Right obj
      _ -> Left (problem (badRequest ["a merge-patch body must be a JSON object"]))
    mapLeft problem (patchFieldsOk schema arg obj)
    args <- argsFor [(ka, A.String key), (adName arg, A.Object obj)]
    pure (Singular args)
  (BindDelete, Just key) -> keyed key $ \ka -> do
    args <- argsFor [(ka, A.String key)]
    pure (Singular args)
  (BindCreate, Nothing) -> do
    noPreconditions
    arg <- soleArg
    case A.decodeStrict body of
      -- §11.8: the singular creation body is the BARE fields of the input
      -- record (the collection URL receives the representation, PUT-style);
      -- the array form is the batch, each item the {"key"?, "input": {...}}
      -- envelope around the same bare-fields record.
      Just v@(A.Object _) -> do
        args <- argsFor [(adName arg, v)]
        pure (Singular args, BoundCtx {bcPre = Nothing, bcCreated = True})
      Just (A.Array arr) -> do
        bp <- maybe (Left (problem (mkProblem 400 "lattice:batch-not-supported"))) Right (mutBatch mdef)
        checkBatchSize bp (length (V.toList arr))
        items <- traverse (createItem arg) (zip [0 :: Int ..] (V.toList arr))
        pure (BatchInv (bpAtomicity bp) items, BoundCtx {bcPre = Nothing, bcCreated = False})
      _ -> Left (problem (badRequest ["a creation POST body must be a JSON object (bare input fields) or array (batch)"]))
  (BindPatch, Nothing) -> collection $ \ka bp -> do
    arg <- soleNonKeyArg ka
    requireMergePatch
    arr <- case A.decodeStrict body of
      Just (A.Array arr) -> Right (V.toList arr)
      _ -> Left (problem (badRequest ["a collection PATCH body must be a JSON array of merge-patches"]))
    checkBatchSize bp (length arr)
    items <- traverse (patchItem ka arg) (zip [0 :: Int ..] arr)
    pure (BatchInv (bpAtomicity bp) items)
  (BindDelete, Nothing) -> collection $ \ka bp -> do
    let ids = map snd (filter ((== "id") . fst) params)
        keys = map snd (filter ((== "key") . fst) params)
    when (null ids) $
      Left (problem (badRequest ["a collection DELETE names its targets via id query parameters (§11.8)"]))
    when (length keys > length ids) $
      Left (problem (badRequest ["more key parameters than id parameters"]))
    checkBatchSize bp (length ids)
    let itemKeyAt ix = fromMaybe (tshow ix) (listToMaybe (drop ix keys))
        items = zipWith (\ix i -> (itemKeyAt ix, Map.singleton ka (A.String i))) [0 ..] ids
    pure (BatchInv (bpAtomicity bp) items)
  _ ->
    -- Unreachable: 'route' never pairs these verb/shape combinations.
    Left (problem (mkProblem 404 "lattice:not-found"))
  where
    problem = problemResponse req
    mapLeft f = either (Left . f) Right

    ifMatchHdr = lookupHeader hIfMatch (requestHeaders req)
    ifNoneHdr = lookupHeader hIfNoneMatch (requestHeaders req)

    -- Keyed form: bind the key argument and run the verb-specific body
    -- decoding FIRST (RFC 9110 §13.2.2: preconditions are ignored when the
    -- request would fail anyway, so 415/400 precede 428/412), then parse
    -- the conditional headers, demanding one on non-lww bindings (428,
    -- plain per §15).
    keyed key k = do
      binding <- maybe (Left (problem (mkProblem 404 "lattice:not-found"))) Right (mutBinding mdef)
      ka <- maybe (Left (problem (mkProblem 404 "lattice:not-found"))) Right (vbKeyArg binding)
      inv <- k ka
      pre <- parsePre binding (Ref ty key)
      -- A successful create-if-absent PUT created the resource (RFC 9110
      -- §9.3.4 demands the 201).
      let creates = (mpCheck <$> pre) == Just PreIfAbsent
      pure (inv, BoundCtx {bcPre = pre, bcCreated = creates})

    -- Collection form: no preconditions apply; the batch policy and its
    -- key argument come from the singular binding (elaboration guarantees
    -- both exist wherever a collection shape is indexed).
    collection k = do
      noPreconditions
      binding <- maybe (Left (problem (mkProblem 404 "lattice:not-found"))) Right (mutBinding mdef)
      ka <- maybe (Left (problem (mkProblem 404 "lattice:not-found"))) Right (vbKeyArg binding)
      bp <- maybe (Left (problem (mkProblem 400 "lattice:batch-not-supported"))) Right (mutBatch mdef)
      inv <- k ka bp
      pure (inv, BoundCtx {bcPre = Nothing, bcCreated = False})

    noPreconditions =
      case (ifMatchHdr, ifNoneHdr) of
        (Nothing, Nothing) -> Right ()
        _ -> Left (problem (badRequest ["preconditions (If-Match / If-None-Match) apply only to keyed entity URLs"]))

    parsePre binding target = case (ifMatchHdr, ifNoneHdr) of
      (Just _, Just _) ->
        Left (problem (badRequest ["supply at most one of If-Match and If-None-Match"]))
      (Just im, Nothing) -> do
        v <- strongEtagOf (lenientText im)
        Right (Just MutatePrecondition {mpTarget = target, mpCheck = PreIfMatch v})
      (Nothing, Just inm)
        | T.strip (lenientText inm) == "*" ->
            if vbVerb binding == BindPut
              then Right (Just MutatePrecondition {mpTarget = target, mpCheck = PreIfAbsent})
              else Left (problem (badRequest ["If-None-Match: * applies only to PUT (create-if-absent, §11.7)"]))
        | otherwise ->
            Left (problem (badRequest ["only If-None-Match: * (create-if-absent) is supported on bound mutations"]))
      (Nothing, Nothing)
        | vbLww binding -> Right Nothing
        | otherwise ->
            Left . plainProblem req 428 "Precondition Required" $
              "mutation `"
                <> unMutationName name
                <> "` is verb-bound and not last-writer-wins: supply If-Match: \"<ver>\""
                <> (if vbVerb binding == BindPut then " or If-None-Match: *" else "")

    -- A single strong quoted etag; weak etags never match state-changing
    -- preconditions (RFC 9110 §13.1.1) and @*@ is not supported.
    strongEtagOf raw =
      let t = T.strip raw
       in if T.isPrefixOf "\"" t && T.isSuffixOf "\"" t && T.length t >= 2 && not (T.isInfixOf "," t)
            then Right (T.drop 1 (T.dropEnd 1 t))
            else Left (problem (badRequest ["If-Match takes a single strong version etag, e.g. If-Match: \"e4\""]))

    soleNonKeyArg ka = case filter ((/= ka) . adName) (mutParams mdef) of
      [arg] -> Right arg
      _ -> Left (problem ((mkProblem 500 "lattice:internal") {pDetail = Just "binding arity invariant violated"}))

    -- The creation record argument of a POST binding (elaboration pins
    -- exactly one, a declared record type).
    soleArg = case mutParams mdef of
      [arg] -> Right arg
      _ -> Left (problem ((mkProblem 500 "lattice:internal") {pDetail = Just "binding arity invariant violated"}))

    -- One bound-batch creation item (§11.8): the {"key"?, "input": {...}}
    -- envelope; "input" carries the bare-fields creation record.
    createItem arg (ix, v) = case v of
      A.Object obj -> do
        let itemKey = case KM.lookup "key" obj of
              Just (A.String k) -> k
              _ -> tshow ix
        inner <- case KM.lookup "input" obj of
          Just innerV@(A.Object _) -> Right innerV
          _ -> Left (problem (badRequest ["batch item " <> tshow ix <> " must carry its creation record under \"input\""]))
        args <- argsFor [(adName arg, inner)]
        Right (itemKey, args)
      _ -> Left (problem (badRequest ["batch item " <> tshow ix <> " must be a JSON object"]))

    argsFor pairs =
      mapLeft problem $
        itemArgs schema mdef (KM.fromList (map (\(ArgName a, v) -> (AK.fromText a, v)) pairs))

    requireMergePatch =
      case lookupHeader hContentType (requestHeaders req) of
        Just ct
          | T.toLower (T.strip (T.takeWhile (/= ';') (lenientText ct))) == "application/x-lattice-merge-patch" ->
              Right ()
        _ ->
          Left . problem $
            (mkProblem 415 "lattice:unsupported-media")
              {pDetail = Just "PATCH bodies use application/x-lattice-merge-patch"}

    checkBatchSize bp n =
      if fromIntegral n > bpMaxItems bp
        then
          Left . problem $
            (mkProblem 400 "lattice:batch-too-large")
              {pDetail = Just ("this mutation admits at most " <> tshow (bpMaxItems bp) <> " items")}
        else Right ()

    -- One collection-PATCH item (§11.8): the target's key field inline,
    -- an optional item key under @"key"@, the rest the merge-patch.
    patchItem ka arg (ix, v) = case v of
      A.Object obj -> do
        let keyField = case Map.lookup ty (schemaEntities schema) of
              Just ent | kf :| [] <- entityKey ent -> unFieldName kf
              _ -> "id"
        keyVal <- case KM.lookup (AK.fromText keyField) obj of
          Just kv -> Right kv
          Nothing -> Left (problem (badRequest ["batch item " <> tshow ix <> " lacks the key field `" <> keyField <> "`"]))
        let itemKey = case KM.lookup "key" obj of
              Just (A.String k) -> k
              _ -> tshow ix
            patch = KM.delete "key" (KM.delete (AK.fromText keyField) obj)
        mapLeft problem (patchFieldsOk schema arg patch)
        args <- argsFor [(ka, keyVal), (adName arg, A.Object patch)]
        Right (itemKey, args)
      _ -> Left (problem (badRequest ["batch item " <> tshow ix <> " must be a merge-patch object"]))


-- | Reject merge-patch fields the input record does not declare.
patchFieldsOk :: Schema -> ArgDef -> A.Object -> Either Problem ()
patchFieldsOk schema argDef obj = case adType argDef of
  TNamed rt
    | Just (DeclRecord fs) <- Map.lookup rt (schemaTypes schema) ->
        let declared = Set.fromList (map (unFieldName . fst) fs)
            unknown = filter (\k -> not (Set.member (AK.toText k) declared)) (KM.keys obj)
         in if null unknown
              then Right ()
              else Left (badRequest (map (\k -> "unknown merge-patch field: " <> AK.toText k) unknown))
  _ -> Right ()


{- | An RFC 9457 problem with type @about:blank@: the §15 table gives 412
and 428 no @lattice:@ code — they are plain HTTP semantics.
-}
plainProblem :: Request -> Int -> Text -> Text -> Response
plainProblem req status title detail =
  mkResponse
    req
    status
    [(hContentType, "application/problem+json"), (hCacheControl, "no-store")]
    ( BL.toStrict . A.encode . A.object $
        [ "type" .= ("about:blank" :: Text)
        , "title" .= title
        , "status" .= status
        , "detail" .= detail
        ]
    )


{- | The @412@ of a failed conditional request (§11.7): plain HTTP status
(no problem document) whose body is an ordinary entity stream carrying the
target's current state — manifest, entity\/tombstone records, end,
@no-store@ — so a client rebases and retries with no follow-up fetch.
-}
preconditionFailed ::
  Origin ->
  Request ->
  MutationName ->
  MutationDef ->
  Claims ->
  SliceName ->
  Ref ->
  IO Response
preconditionFailed o req name mdef claims callerSlice target = do
  let cfg = oConfig o
  snap <- beSnapshot (ocBackend cfg)
  (rendered, _) <- renderOutput o mdef claims callerSlice Nothing [target]
  let etag = mutationEtag name (recIdVers rendered)
      recs =
        [ RManifest
            Manifest
              { mQuery = Nothing
              , mMutation = Just name
              , mPlan = Nothing
              , mSlice = Nothing
              , mRoot = Map.singleton "node" [target]
              , mEtag = etag
              , mBatch = Nothing
              }
        ]
          <> rendered
          <> [REnd (EndRecord True (Just etag))]
  pure $
    mkResponse
      req
      412
      [ (hContentType, ndjsonType)
      , (hCacheControl, "no-store")
      , snapshotHdr cfg snap
      , schemaHdr o
      ]
      (encodeRecords recs)


{- | The mutation principal: the @vc@ payload when presented (proof-checked
when a verifier is configured), else the @Authorization@ header, else
anonymous. The principal key scopes the idempotency store.
-}
resolvePrincipal :: Origin -> Request -> [(Text, Text)] -> IO (Either Problem (Claims, Text))
resolvePrincipal o req params = case lookup "vc" params of
  Just vcText ->
    verifyVc o req vcText >>= \case
      Left p -> pure (Left p)
      Right payload -> pure (Right (cpClaims payload, b64url (blake3 (cpRaw payload))))
  Nothing -> case lookupHeader hAuthorization (requestHeaders req) of
    Just auth -> pure (Right (Map.empty, b64url (blake3 auth)))
    Nothing -> pure (Right (Map.empty, b64url (blake3 "anon")))


{- | The @allow@ guard. @RequiresClaims@ checks every row-independent
predicate against the presented claims; @caller.x = entityField@ guards
are the backend's obligation (they reference the written entity).
-}
checkGuard :: Policy -> Claims -> Bool -> Either Problem ()
checkGuard pol claims hasAuth = case pol of
  Public -> Right ()
  Private
    | hasAuth -> Right ()
    | otherwise -> Left forbidden
  RequiresClaims _
    | claimOnlyPredicates claims pol -> Right ()
    | otherwise -> Left forbidden


forbidden :: Problem
forbidden = mkProblem 403 "lattice:forbidden"


parseMutationBody :: Schema -> MutationDef -> ByteString -> Either Problem Invocation
parseMutationBody schema mdef body = case A.decodeStrict body of
  Just (A.Object obj) -> Singular <$> itemArgs schema mdef obj
  Just (A.Array arr) -> case mutBatch mdef of
    Nothing -> Left (mkProblem 400 "lattice:batch-not-supported")
    Just bp -> do
      let items = V.toList arr
      when (fromIntegral (length items) > bpMaxItems bp) $
        Left
          ( (mkProblem 400 "lattice:batch-too-large")
              {pDetail = Just ("this mutation admits at most " <> tshow (bpMaxItems bp) <> " items")}
          )
      parsed <- traverse (batchItem schema mdef) (zip [0 :: Int ..] items)
      Right (BatchInv (bpAtomicity bp) parsed)
  _ -> Left (badRequest ["mutation body must be a JSON object or array"])


{- | One batch item: @{\"key\"?, …input fields}@ inline, or the
@{\"key\"?, \"input\": {…}}@ envelope (recognized only when @input@ is not
itself a declared parameter). Items without a key get their position.
-}
batchItem :: Schema -> MutationDef -> (Int, A.Value) -> Either Problem (Text, Map ArgName A.Value)
batchItem schema mdef (ix, v) = case v of
  A.Object obj -> do
    let keyTxt = case KM.lookup "key" obj of
          Just (A.String k) -> k
          _ -> tshow ix
        rest = KM.delete "key" obj
        inputDeclared = any ((== ArgName "input") . adName) (mutParams mdef)
        inputObj = case (KM.lookup "input" rest, KM.size rest, inputDeclared) of
          (Just (A.Object inner), 1, False) -> inner
          _ -> rest
    args <- itemArgs schema mdef inputObj
    Right (keyTxt, args)
  _ -> Left (badRequest ["batch item " <> tshow ix <> " must be a JSON object"])


itemArgs :: Schema -> MutationDef -> A.Object -> Either Problem (Map ArgName A.Value)
itemArgs schema mdef obj = do
  let declared = map adName (mutParams mdef)
      unknown = filter (\k -> ArgName (AK.toText k) `notElem` declared) (KM.keys obj)
  unless (null unknown) $
    Left (badRequest (map (\k -> "unknown input field: " <> AK.toText k) unknown))
  pairs <- traverse one (mutParams mdef)
  Right (Map.fromList (catMaybes pairs))
  where
    one ad = case KM.lookup (AK.fromText (unArgName (adName ad))) obj of
      Just v
        -- §3.5.2: an empty array in a nonempty-list (@[t]+@) input is
        -- rejected at the request, like any other type mismatch.
        | violatesList1 schema (adType ad) v ->
            Left (badRequest ["invalid value for input field " <> unArgName (adName ad) <> ": an empty array violates its nonempty list type"])
        | otherwise -> Right (Just (adName ad, v))
      Nothing -> case adDefault ad >>= qvalueToJson of
        Just v -> Right (Just (adName ad, v))
        Nothing
          | optionalType (adType ad) -> Right Nothing
          | otherwise -> Left (badRequest ["missing input field: " <> unArgName (adName ad)])
    optionalType = \case
      TOptional _ -> True
      _ -> False


{- | The idempotency envelope (spec §11.2): claim the key in STM, replay a
completed response with @Idempotency-Replayed: true@, reject digest
mismatches (@422 lattice:key-reuse@) and concurrent executions
(@409 lattice:key-in-flight@). The entry is removed if the action throws.
-}
withIdempotency :: Origin -> Request -> MutationName -> MutationDef -> Text -> ByteString -> IO Response -> IO Response
withIdempotency o req name mdef principalKey body act =
  case lookupHeader hIdempotencyKey (requestHeaders req) of
    Nothing -> act
    Just keyBs -> do
      let k = (name, principalKey, lenientText keyBs)
          digest = blake3 body
      claim <- atomically $ do
        m <- readTVar (oIdem o)
        case Map.lookup k m of
          Just e -> pure (Just e)
          Nothing -> do
            writeTVar (oIdem o) (Map.insert k IdemInFlight m)
            pure Nothing
      case claim of
        Just IdemInFlight ->
          pure . problemResponse req $
            (mkProblem 409 "lattice:key-in-flight") {pHeaders = [(hRetryAfter, "1")]}
        Just done@IdemDone {}
          | idDigest done == digest -> do
              -- §19.2/§19.3: the replay is visible on the span and counted
              -- by mutation and effect class.
              countMutationReplay tel (unMutationName name) (effectClassText (mutEffect mdef))
              addActiveSpanAttrs tel [("lattice.idempotency.replayed", boolA True)]
              pure $
                mkResponse
                  req
                  (idStatus done)
                  (insertHeader hIdempotencyReplayed "true" (idHeaders done))
                  (idBody done)
          | otherwise -> pure (problemResponse req (mkProblem 422 "lattice:key-reuse"))
        Nothing -> do
          resp <- act `onException` atomically (modifyTVar' (oIdem o) (Map.delete k))
          -- A 412 is a refused precondition, not an acceptance (§11.2's
          -- at-most-once applies to accepted keys): storing it would replay
          -- the stale failure at the client's corrected retry. Release the
          -- key instead.
          if (statusInt resp :: Int) == 412
            then atomically (modifyTVar' (oIdem o) (Map.delete k))
            else do
              let stored =
                    IdemDone
                      { idDigest = digest
                      , idStatus = statusInt resp
                      , idHeaders = responseHeaders resp
                      , idBody = bodyBytesOf resp
                      }
              atomically (modifyTVar' (oIdem o) (Map.insert k stored))
          pure resp
  where
    tel = ocTelemetry (oConfig o)
    statusInt r = case responseStatus r of
      Status w -> fromIntegral w
    bodyBytesOf r = case responseBody r of
      BodyBytes bs -> bs
      _ -> BS.empty


-- | §19.2 mutation-span attributes, added to the enclosing server span.
addMutationAttrs :: Origin -> MutationName -> MutationDef -> IO ()
addMutationAttrs o (MutationName n) mdef =
  addActiveSpanAttrs
    (ocTelemetry (oConfig o))
    [ ("lattice.mutation.name", txtA n)
    , ("lattice.effect_class", txtA (effectClassText (mutEffect mdef)))
    ]


-- | The wire spelling of an effect class (§11.2), as a span attribute value.
effectClassText :: EffectClass -> Text
effectClassText = \case
  Transactional -> "transactional"
  NaturallyIdempotent _ -> "natural"
  Workflow -> "workflow"


runMutation ::
  Origin ->
  Request ->
  MutationName ->
  MutationDef ->
  Claims ->
  SliceName ->
  BoundCtx ->
  Invocation ->
  IO Response
runMutation o req name mdef claims callerSlice bctx = \case
  Singular args -> do
    outcome <- beMutate (ocBackend cfg) name claims args (bcPre bctx)
    case outcome of
      MutationDenied -> pure (problemResponse req forbidden)
      MutationFailed bf ->
        pure (problemResponse req ((mkProblem 500 "lattice:internal") {pDetail = bfMessage bf}))
      MutationPreconditionFailed -> case bcPre bctx of
        Just p -> preconditionFailed o req name mdef claims callerSlice (mpTarget p)
        Nothing ->
          pure . problemResponse req $
            (mkProblem 500 "lattice:internal") {pDetail = Just "backend reported a precondition failure for an unconditional mutation"}
      MutationDomainError v -> do
        snap <- beSnapshot (ocBackend cfg)
        let er = ErrorRecord Nothing Nothing (Just v) False Nothing
            etag = mutationEtag name []
            recs =
              [ RManifest (mutManifest name Nothing [] etag)
              , RError er
              , REnd (EndRecord True (Just etag))
              ]
        pure (mutResponse o req 207 snap [] (encodeRecords recs))
      MutationCommitted cr -> case checkWriteSet (ocSchema cfg) mdef args (crWrites cr) of
        Left violation ->
          pure (problemResponse req ((mkProblem 500 "lattice:write-scope") {pDetail = Just violation}))
        Right () -> do
          keys <- invalidationKeys o mdef args (crWrites cr)
          (rendered, degraded) <- renderOutput o mdef claims callerSlice Nothing (crResult cr)
          let recs = rendered <> familyTombstones (ocSchema cfg) Nothing (crWrites cr) rendered
              etag = mutationEtag name (recIdVers recs)
              bodyRecs =
                [RManifest (mutManifest name Nothing (crResult cr) etag)]
                  <> recs
                  <> [RInvalidated keys Nothing, REnd (EndRecord True (Just etag))]
              -- Creation (§11.7): 201 with Location at the created entity's
              -- point-fetch URL, provided the effect produced a result ref.
              created = bcCreated bctx && not degraded
              status
                | degraded = 207
                | created && locationOf (crResult cr) /= Nothing = 201
                | otherwise = 200
          publishPurge o keys
          let resp = mutResponse o req status (crSnapshot cr) keys (encodeRecords bodyRecs)
          pure $ case (created, locationOf (crResult cr)) of
            (True, Just loc) -> resp {responseHeaders = insertHeader hLocation loc (responseHeaders resp)}
            _ -> resp
  BatchInv BestEffort items -> do
    results <- traverse (runItem o mdef claims callerSlice name) items
    snap <- beSnapshot (ocBackend cfg)
    let allRecs = concatMap ioRecords results
        invRecs = mapMaybe ioInvalidated results
        keys = dedupOrd (concatMap ioKeys results)
        resultRefs = concatMap ioResult results
        etag = mutationEtag name (recIdVers allRecs)
        manifest =
          mutManifest name (Just (BatchInfo "best-effort" (length items))) resultRefs etag
        degraded = any isErrorRec allRecs
        status = if degraded then 207 else 200
        bodyRecs = [RManifest manifest] <> allRecs <> invRecs <> [REnd (EndRecord True (Just etag))]
    unless (null keys) (publishPurge o keys)
    pure (mutResponse o req status snap keys (encodeRecords bodyRecs))
  BatchInv AllOrNothing items -> goAll items []
  where
    cfg = oConfig o
    locationOf refs =
      ( \r ->
          "/e/"
            <> encodePathSegment (TE.encodeUtf8 (unTypeName (refType r)))
            <> "/"
            <> encodePathSegment (TE.encodeUtf8 (refKey r))
      )
        <$> find (\r -> refType r == mutReturns mdef) refs
    isErrorRec = \case
      RError _ -> True
      _ -> False
    -- AllOrNothing: per-item effects, aborting at the first failure with a
    -- single unscoped error and no partial records; rollback of already-run
    -- items is the transactional backend's obligation (documented
    -- simplification — the effect class is compile-restricted to
    -- Transactional/NaturallyIdempotent).
    goAll [] acc = do
      let results = reverse acc
      snap <- beSnapshot (ocBackend cfg)
      let allRecs = concatMap ioRecords results
          invRecs = mapMaybe ioInvalidated results
          keys = dedupOrd (concatMap ioKeys results)
          resultRefs = concatMap ioResult results
          etag = mutationEtag name (recIdVers allRecs)
          manifest =
            mutManifest name (Just (BatchInfo "all-or-nothing" (length results))) resultRefs etag
          degraded = any isErrorRec allRecs
          status = if degraded then 207 else 200
          bodyRecs = [RManifest manifest] <> allRecs <> invRecs <> [REnd (EndRecord True (Just etag))]
      unless (null keys) (publishPurge o keys)
      pure (mutResponse o req status snap keys (encodeRecords bodyRecs))
    goAll (item : rest) acc = do
      out <- runItem o mdef claims callerSlice name item
      if ioCommitted out && not (any isErrorRec (ioRecords out))
        then goAll rest (out : acc)
        else do
          snap <- beSnapshot (ocBackend cfg)
          let fatal = case mapMaybe errOf (ioRecords out) of
                (e : _) -> RError e {errScope = Nothing}
                [] -> RError (ErrorRecord Nothing (Just "lattice:internal") Nothing False Nothing)
              etag = mutationEtag name []
              manifest =
                mutManifest name (Just (BatchInfo "all-or-nothing" 0)) [] etag
              bodyRecs = [RManifest manifest, fatal, REnd (EndRecord True (Just etag))]
          pure (mutResponse o req 207 snap [] (encodeRecords bodyRecs))
    errOf = \case
      RError e -> Just e
      _ -> Nothing


runItem ::
  Origin ->
  MutationDef ->
  Claims ->
  SliceName ->
  MutationName ->
  (Text, Map ArgName A.Value) ->
  IO ItemOut
runItem o mdef claims callerSlice name (itemKey, args) = do
  -- Batch items carry no preconditions (§11.8: per-item concurrency
  -- control is the item key's job; If-* headers on collection URLs are
  -- rejected before dispatch).
  outcome <- beMutate (ocBackend (oConfig o)) name claims args Nothing
  case outcome of
    MutationDenied -> pure (errItem (Just "lattice:forbidden") Nothing Nothing False False)
    MutationFailed bf ->
      pure (errItem (Just (bfCode bf)) Nothing (bfMessage bf) (bfRetryable bf) False)
    MutationPreconditionFailed ->
      -- Unreachable from a Nothing precondition; classify as backend fault.
      pure (errItem (Just "lattice:internal") Nothing (Just "unexpected precondition failure") False False)
    MutationDomainError v -> pure (errItem Nothing (Just v) Nothing False False)
    MutationCommitted cr -> case checkWriteSet schema mdef args (crWrites cr) of
      Left violation ->
        -- The effect committed backend-side; report the violation
        -- item-scoped so sibling verdicts survive (module haddock).
        pure (errItem (Just "lattice:write-scope") Nothing (Just violation) False True)
      Right () -> do
        keys <- invalidationKeys o mdef args (crWrites cr)
        (rendered, _) <- renderOutput o mdef claims callerSlice (Just itemKey) (crResult cr)
        pure
          ItemOut
            { ioRecords =
                rendered <> familyTombstones schema (Just itemKey) (crWrites cr) rendered
            , ioInvalidated = Just (RInvalidated keys (Just itemKey))
            , ioKeys = keys
            , ioResult = crResult cr
            , ioCommitted = True
            }
  where
    schema = ocSchema (oConfig o)
    errItem code domainV msg retryable committed =
      ItemOut
        { ioRecords =
            [ RError
                ErrorRecord
                  { errScope = Just (ScopeItem itemKey)
                  , errCode = code
                  , errDomain = domainV
                  , errRetryable = retryable
                  , errMessage = msg
                  }
            ]
        , ioInvalidated = Nothing
        , ioKeys = []
        , ioResult = []
        , ioCommitted = committed
        }


{- | Render a mutation's output: the whole visible field set of the
returned type at the caller's level, reloaded through 'beLoad' so fresh
versions appear. Entity\/tombstone records are tagged with the batch item
key; scoped errors from rendering keep their entity\/edge scopes.
-}
renderOutput ::
  Origin ->
  MutationDef ->
  Claims ->
  SliceName ->
  Maybe Text ->
  [Ref] ->
  IO ([Record], Bool)
renderOutput o mdef claims callerSlice itemKey refs = do
  let cfg = oConfig o
      schema = ocSchema cfg
      retTy = mutReturns mdef
      mNode = outputSelection schema callerSlice LPublic retTy
      seeds = mapMaybe (\r -> if refType r == retTy then (\n -> (r, n)) <$> mNode else Nothing) refs
      env =
        ExecEnv
          { xSchema = schema
          , xBudgets = ocBudgets cfg
          , xBackend = ocBackend cfg
          , xClaims = claims
          , xVars = Map.empty
          , xMode = EmitAtMost callerSlice
          , xTelemetry = ocTelemetry cfg
          , -- Mutation output renders the whole visible field set
            -- ('outputSelection'): loads are ProjectAll by design.
            xProjections = Map.empty
          }
  executeSeeds env seeds >>= \case
    Left _ ->
      pure
        ( [RError (ErrorRecord Nothing (Just "lattice:internal") Nothing False (Just "output rendering aborted"))]
        , True
        )
    Right xr -> pure (map tagItem (xrRecords xr), xrDegraded xr)
  where
    tagItem r = case itemKey of
      Nothing -> r
      Just k -> case r of
        REntity er -> REntity er {erItem = Just k}
        RTombstone ref v _ -> RTombstone ref v (Just k)
        other -> other


{- | Enforce the declared write set over the effect's facts (spec §11.4).
A declared scope naming any member of a shared-truth family admits writes
recorded against any other member (§3.8): the family is one record of
truth, so the scope is instantiated at the family.
-}
checkWriteSet :: Schema -> MutationDef -> Map ArgName A.Value -> [WriteFact] -> Either Text ()
checkWriteSet schema mdef args = traverse_ checkFact
  where
    decls = mutWrites mdef
    checkFact = \case
      WroteEntity r -> entityOk r
      DeletedEntity r _ -> entityOk r
      WroteCollection c _ ->
        if any (collMatch c) decls
          then Right ()
          else Left ("write outside declared write set: collection " <> unCollectionName c)
    collMatch c = \case
      WCollection c' _ -> c' == c
      _ -> False
    entityOk r =
      if any (entMatch r) decls
        then Right ()
        else Left ("write outside declared write set: " <> renderRef r)
    entMatch r = \case
      WEntity t ke
        | elem (refType r) (sharedTruthFamily schema t) -> case ke of
            KeyNew -> True
            KeyArg a -> (renderScalarKey <$> Map.lookup a args) == Just (refKey r)
      _ -> False


{- | Invalidation keys: one per write fact — expanded through the written
entity's shared-truth family (§3.8): a write to any @refines@ family
member mints the entity keys of the base and every refinement, while a
@joins@ companion stays a singleton — plus the declared
@invalidates writes + …@ extras instantiated against the arguments
(@GroupOfWritten T.f@ loads the written rows and reads @f@).
-}
invalidationKeys :: Origin -> MutationDef -> Map ArgName A.Value -> [WriteFact] -> IO [SurrogateKey]
invalidationKeys o mdef args facts = do
  extras <- case mutInvalidates mdef of
    ExactlyWrites -> pure []
    WritesPlus ds -> concat <$> traverse extraKey ds
  pure (dedupOrd (concatMap factKey facts <> extras))
  where
    backend = ocBackend (oConfig o)
    schema = ocSchema (oConfig o)
    factKey = \case
      WroteEntity r -> familyKeys r
      DeletedEntity r _ -> familyKeys r
      WroteCollection c vals -> [collectionKey c vals]
    familyKeys r =
      map
        (\t -> entityKeyOf (Ref t (refKey r)))
        (NE.toList (sharedTruthFamily schema (refType r)))
    -- Written keys attributable to type @t@: facts recorded against any
    -- member of @t@'s shared-truth family count (same row, same key).
    writtenOf t =
      mapMaybe
        ( \case
            WroteEntity r | sameTruth r -> Just (refKey r)
            DeletedEntity r _ | sameTruth r -> Just (refKey r)
            _ -> Nothing
        )
        facts
      where
        sameTruth r = elem (refType r) (sharedTruthFamily schema t)
    extraKey = \case
      WEntity t (KeyArg a) ->
        pure (maybe [] (familyKeys . Ref t . renderScalarKey) (Map.lookup a args))
      WEntity _ KeyNew -> pure []
      WCollection c (GroupArg a) ->
        pure (maybeToList ((\v -> collectionKey c [renderScalarKey v]) <$> Map.lookup a args))
      WCollection c (GroupOfWritten t f) -> case writtenOf t of
        [] -> pure []
        keys -> do
          rows <- beLoad backend t (ProjectFields (Set.singleton f)) keys
          pure $
            mapMaybe
              ( \k -> case Map.lookup k rows of
                  Just (Right (RowFound row)) ->
                    (\v -> collectionKey c [renderScalarKey v]) <$> Map.lookup f (rowFields row)
                  _ -> Nothing
              )
              keys


{- | Family tombstones (§3.8): deleting the __base__ of a shared-truth
family tombstones every member — a refinement of a deleted row cannot
outlive it. Synthesized from the @DeletedEntity@ facts at the base's
tombstone version (the family shares one @ver@ sequence), skipping
members the rendered output already tombstoned. Deleting a refinement
alone does not cascade: the base outlives its projections.
-}
familyTombstones :: Schema -> Maybe Text -> [WriteFact] -> [Record] -> [Record]
familyTombstones schema itemKey facts rendered = concatMap expand facts
  where
    present =
      Set.fromList
        ( mapMaybe
            ( \case
                RTombstone r _ _ -> Just r
                _ -> Nothing
            )
            rendered
        )
    expand = \case
      DeletedEntity r v -> case sharedTruthFamily schema (refType r) of
        base :| rest@(_ : _)
          | base == refType r ->
              mapMaybe
                ( \t ->
                    let r' = Ref t (refKey r)
                     in if Set.member r' present
                          then Nothing
                          else Just (RTombstone r' v itemKey)
                )
                (base : rest)
        _ -> []
      _ -> []


mutManifest :: MutationName -> Maybe BatchInfo -> [Ref] -> Text -> Manifest
mutManifest name batch resultRefs etag =
  Manifest
    { mQuery = Nothing
    , mMutation = Just name
    , mPlan = Nothing
    , mSlice = Nothing
    , mRoot = Map.singleton "result" resultRefs
    , mEtag = etag
    , mBatch = batch
    }


-- | Mutation manifest etag: the mutation name and the rendered fact set.
mutationEtag :: MutationName -> [(Text, Text)] -> Text
mutationEtag (MutationName n) idvers =
  manifestEtagHash . canonicalJson . A.Array . V.fromList $
    [A.String n, A.toJSON (map (\(i, v) -> [i, v]) idvers)]


recIdVers :: [Record] -> [(Text, Text)]
recIdVers =
  sort
    . mapMaybe
      ( \case
          REntity er -> Just (renderRef (erId er), erVer er)
          RTombstone r v _ -> Just (renderRef r, v)
          RElided r -> Just (renderRef r, "")
          _ -> Nothing
      )


mutResponse :: Origin -> Request -> Int -> SnapshotToken -> [SurrogateKey] -> ByteString -> Response
mutResponse o req status snap keys body =
  mkResponse req status hdrs body
  where
    hdrs =
      [ (hContentType, ndjsonType)
      , (hCacheControl, "no-store")
      , snapshotHdr (oConfig o) snap
      , schemaHdr o
      ]
        <> (if status == 207 then [(hLatticeOutcome, "degraded")] else [])
        <> (if null keys then [] else [keysHdr keys])


tshow :: (Show a) => a -> Text
tshow = T.pack . show


-- ---------------------------------------------------------------------------
-- Compatibility registry endpoints (spec §17)
-- ---------------------------------------------------------------------------

{- | Gate a registry endpoint on 'ocRegistry' (§17.1): the registry is
optional analysis infrastructure, so an unconfigured origin treats the
path as unrouted.
-}
withRegistry :: Origin -> Request -> (Registry -> IO Response) -> IO Response
withRegistry o req k = case ocRegistry (oConfig o) of
  Nothing ->
    pure . problemResponse req $
      (mkProblem 404 "lattice:not-found") {pDetail = Just "no compatibility registry configured"}
  Just reg -> k reg


{- | Snapshot the origin's query corpus (§17.4): every memoized canonical
text with its tenure hit count and the @Lattice-Client@ builds recorded
against it. One STM transaction, so the cut is consistent; the memo IS
the corpus — there is nothing to register.
-}
exportCorpus :: Origin -> IO [CorpusEntry]
exportCorpus o = atomically $ do
  memo <- readTVar (oMemo o)
  tenure <- readTVar (oTenure o)
  clients <- readTVar (oClients o)
  pure
    [ CorpusEntry
        { ceText = compiledText c
        , ceHits = fromIntegral (max 0 (Map.findWithDefault 0 h tenure))
        , ceClients = Set.toAscList (Map.findWithDefault Set.empty h clients)
        }
    | (h, (c, _plan)) <- Map.toList memo
    ]


-- | @GET /schema/corpus@ (§17.4): the corpus export, for the
-- registry-as-consumer story. Traffic statistics, so never cacheable.
serveCorpus :: Origin -> Request -> IO Response
serveCorpus o req = withRegistry o req $ \_reg -> do
  entries <- exportCorpus o
  pure (jsonResponse req 200 [(hCacheControl, "no-store")] (A.object ["corpus" .= entries]))


{- | Record the advisory @Lattice-Client@ header (§17.1) against a served
query's hash, for corpus attribution. Only when a registry is configured
(attribution has no other consumer — the default path stays allocation-free),
and bounded per hash so a header-spinning client cannot grow the map.
-}
recordClient :: Origin -> Request -> Text -> IO ()
recordClient o req h
  | Nothing <- ocRegistry (oConfig o) = pure ()
  | otherwise = for_ (client =<< lookupHeader hLatticeClient (requestHeaders req)) $ \cl ->
      atomically . modifyTVar' (oClients o) $
        Map.insertWith
          (\_new old -> if Set.size old >= clientBound then old else Set.insert cl old)
          h
          (Set.singleton cl)
  where
    client = either (const Nothing) Just . TE.decodeUtf8'


-- | Client builds retained per query hash; advisory data, so clamp hard.
clientBound :: Int
clientBound = 64


idlMediaType :: ByteString
idlMediaType = "application/x-lattice-idl"


{- | @POST \/schema\/check?mode=…&window=…@ (§17.3): check a candidate IDL
against the deployment log and the live corpus. The HTTP status describes
the check request, not the verdict: a checked candidate is @200@ and the
report body's @pass@ field carries the verdict; @400@ is an unparseable
candidate or unknown mode\/window; @415@ a wrong media type; @404@ an
unconfigured registry. CI gates read the body, not the status line.
-}
serveSchemaCheck :: Origin -> Request -> [(Text, Text)] -> IO Response
serveSchemaCheck o req params = withRegistry o req $ \reg ->
  case lookupHeader hContentType (requestHeaders req) of
    Just ct
      | not (idlMediaType `BS.isPrefixOf` ct) ->
          pure . problemResponse req $
            (mkProblem 415 "lattice:unsupported-media")
              {pDetail = Just ("candidate schemas must be " <> TE.decodeUtf8 idlMediaType)}
    _ -> do
      body <- drainBody (requestBody req)
      case TE.decodeUtf8' body of
        Left _ -> pure (problemResponse req (badRequest ["candidate IDL is not valid UTF-8"]))
        Right src -> case parseSchema src of
          Left errs -> pure (problemResponse req (badRequest (map renderIdlError errs)))
          Right candidate -> case checkParams params of
            Left p -> pure (problemResponse req p)
            Right (mode, transitive, window) -> do
              now <- posixSecondsToUTCTime <$> ocNow (oConfig o)
              corpus <- exportCorpus o
              baselines <- map logged <$> registryLog reg
              let cfg =
                    CheckConfig
                      { ccMode = mode
                      , ccTransitive = transitive
                      , ccWindow = window
                      , ccNow = now
                      , ccBudgets = ocBudgets (oConfig o)
                      }
              pure . jsonResponse req 200 [(hCacheControl, "no-store")] $
                A.toJSON (checkSchemas cfg baselines corpus candidate)
  where
    logged e = LoggedSchema {lsDeployedAt = deTime e, lsHash = deHash e, lsSchema = deSchema e}


{- | Parse @mode@\/@window@: an absent mode is the §17.3 default gate
(client-backward, non-transitive); unknown values are 400 per §17.3
("only an unparseable candidate or unknown mode is a 4xx").
-}
checkParams :: [(Text, Text)] -> Either Problem (CheckMode, Bool, Maybe Compat.Window)
checkParams params = do
  (mode, transitive) <- case lookup "mode" params of
    Nothing -> Right (ClientBackward, False)
    Just m -> maybe (Left (badRequest ["unknown mode: " <> m])) Right (parseCheckMode m)
  window <- case lookup "window" params of
    Nothing -> Right Nothing
    Just w -> maybe (Left (badRequest ["unparseable window: " <> w])) (Right . Just) (parseWindow w)
  pure (mode, transitive, window)


renderIdlError :: SchemaError -> Text
renderIdlError e = maybe "" (\l -> "line " <> tshow l <> ": ") (seLine e) <> seMessage e

