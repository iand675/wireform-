{-# LANGUAGE PatternSynonyms #-}
{- | The federation gateway (spec §18.3–§18.8): a full Lattice origin over
the fused schema of its upstreams, whose 'Backend' is synthesized from
ordinary Lattice requests — @nodes@ queries for entity loads, one-root
subqueries for collections and roots, whole-mutation proxying by owner.

== Execution model (§18.3)

The gateway IS an 'Origin' ('newOrigin' over 'fusedSchema'); the ordinary
round-batched executor drives a synthesized 'Backend':

* __Entity loads__ ('beLoad'): one @nodes@ query per contributing upstream
  (owner + extenders) with the @$refs: EntityRef@ spelling (§14.4), refs
  bound as a JSON array of @\"Type:key\"@ strings — the round's full key
  set for that type in one request. Loads batch per (upstream, __type__,
  round): the 'Backend' seam is per-type, so a round touching several
  types issues one @nodes@ query per type rather than folding them into a
  single per-(upstream, round) request (accepted narrowing of the §18.3
  ideal; N+1 stays inexpressible — no per-entity call exists anywhere).
* __Collections and roots__ ('beListRoot'\/'beGetRoot'\/'beChildren'): a
  per-upstream one-root subquery in the upstream's schema, selection
  restricted to that upstream's fields of the target type, pagination
  re-rendered from the gateway's 'Window'. Rows arriving in upstream
  responses are cached per request for derivation and per-src record
  splitting.
* Every subquery is compiled\/canonicalized against the __upstream's
  served schema__ and issued hash-form (@GET \/q\/{hash}@), falling back
  to introduction (@POST \/q?intent=introduce@) on
  @lattice:unknown-query@ — the standard client ladder, so the
  gateway-to-upstream hop stays ordinary cacheable HTTP.

== Wire composition (§18.4)

The executor produces ONE fused manifest and one record stream; a
post-processing pass ('translateResponse'):

* splits each entity record per contributing module — the owner's fields
  keep the row's @ver@ (loads carried the owner's version), each
  extension's fields ride a separate record with that upstream's own
  @ver@ and @src@ tag (§18.1: versions are per contributing module);
* tags tombstone\/elided\/unchanged\/error records with @src@ __in the
  wire JSON only__ — the typed surface exists only on
  'Lattice.Wire.EntityRecord' ('erSrc'); for the other kinds the member
  is additive and decoder-tolerant (typed decoders without a slot drop
  it, which loses nothing: a tombstone evicts the whole entity and an
  @unchanged@ matches any per-src version);
* rewrites @Surrogate-Key@ to the prefixed union (@posts\/Post:17@,
  @social\/reactions:17@; @plan:@ keys stay gateway-local — they name
  gateway plans no upstream knows) and @Lattice-Snapshot@ to the
  namespaced vector (@posts\/main=\"…\", social\/main=\"…\"@) captured
  from THIS request's upstream responses (§18.5: components are
  per-upstream, no cross-upstream snapshot relationship exists).

The coarsening budget (§10.5) is applied by the origin before
translation; prefixing preserves the key count, so the budget holds.

== Invalidation (§18.6)

'newGateway' subscribes to each upstream's @\/invalidations@ feed (SSE,
@since=@ resume, @Last-Event-ID@ carried too). The initial connect is
__synchronous__: when 'newGateway' returns, every upstream's live tail is
registered, so a purge published immediately after is observed with no
sleeping. Keys republish through the gateway's own 'publishPurge' both
__prefixed__ (@posts\/Post:17@ — what the gateway's responses tagged, for
the CDN hook) and __raw__ (@Post:17@ — what the gateway's own live-query
subscriptions registered), so CDN purging and §12 live queries compose
for free. An outrun replay window (first replayed cursor > since+1), a
failed (re)connect, or a feed exception publishes the full-wildcard purge
@{upstream}\/*@ and calls 'gwOnResync' — 'ocPurge' cannot express a
prefix wildcard, so the @\/*@ key + hook call is the documented resync
pin; the subscription then ends (deployments restart by policy).

== Auth (§18.8) and mutations (§18.7)

The inbound proof is verified once, by the gateway's own admission
('gwVerifier'); each upstream request re-mints via 'upMint' with the
caller's claims narrowed to that upstream's declared 'upClaims'
(narrowing preserves coarseness), plus the 'upServiceAuth'
service-principal headers on every hop. Mutations (@POST \/m\/{name}@ and
bound entity-space verbs) are proxied whole to the owner upstream with
@Idempotency-Key@ forwarded untouched — the dedupe transaction happens
where the effect does, and a replay hits the __upstream's__ idempotency
store; the response streams back with @src@ tags, prefixed keys
(@invalidated@ records included), and the namespaced snapshot.

== Discovery

Served by the fused origin itself: the discovery document, the fused IDL
content-addressed (@\/schema\/{hash}@ — 'Lattice.Module.fusedIdl' is the
canonical print of the fused schema, which is exactly what 'newOrigin'
publishes), plan documents, and the gateway's own @\/invalidations@ feed.
A client cannot tell the gateway from a monolith except by the snapshot
header's domain namespaces.

== Deliberate v1 pins (documented deviations)

* Parameterized computed fields on federated types elide ('beComputed'
  returns 'Nothing'): their canonical argument spelling is not re-derived
  for subqueries.
* Root\/collection page cursors do not cross the gateway ('pageNext'\/
  'pagePrev' are 'Nothing' on federated pages); clients paginate by
  explicit cursor arguments, which forward verbatim.
* Interface-targeted edges\/roots across upstreams are unsupported
  (scoped @lattice:internal@).
* Live-query SSE streams serve unsplit records without @src@ tags (the
  translation pass applies to materialized bodies only); the feed-driven
  re-execution itself composes fine.
* Aggregate deps feeding an extension's derived field are answered
  @null@ locally; the derived __value__ is read off the owning upstream's
  row (§18.1: the upstream owns the compute).
* Enum-typed root arguments are indistinguishable from strings at the
  'Backend' seam and render quoted (unsupported).
-}
module Lattice.Gateway (
  -- * Configuration
  Upstream (..),
  GatewayConfig (..),
  defaultGatewayConfig,

  -- * Lifecycle
  Gateway,
  GatewayError (..),
  newGateway,
  shutdownGateway,

  -- * Serving
  gatewayHandler,
  gatewayOrigin,
  gatewayFused,
) where

import Control.Concurrent (ThreadId, forkIO, killThread, myThreadId)
import Control.Concurrent.STM
import Control.Exception (SomeException, finally, try)
import Control.Monad (forM, forM_, unless, when)
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types qualified as AT
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL qualified as B64U
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive (CI)
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Data.Vector qualified as V
import Data.Word (Word64)
import Lattice.Backend
import Lattice.Canonical (Compiled (..), compileText, renderQValue)
import Lattice.Cursor (Cursor (..))
import Lattice.IDL.Parser (SchemaError, parseSchema)
import Lattice.Module (Fused (..), FusionError, ModuleName (..), SchemaModule (..), fuseModules)
import Lattice.Plan (Plan (..), planQuery)
import Lattice.Query.AST (QValue (..))
import Lattice.Schema
import Lattice.Server
import Lattice.Server.Auth (ProofVerifier, QueryAdmission (..), cpClaims, decodeClaims)
import Lattice.Telemetry (noTelemetry)
import Lattice.Types
import Lattice.Value (canonicalJson, valueToUrlParam)
import Lattice.Wire
import Network.HTTP.Client.SSE (ServerSentEvent (..), SseFrame (..), parseEventStream, sseFramePopper)
import Network.HTTP.Connection (ConnectionConfig (..), defaultConnectionConfig, sendOn, withConnection)
import Network.HTTP.Message (Request (..), Response (..), Scheme (..))
import Network.HTTP.PercentEncoding (decodeQueryString, encodePathSegment, percentDecode, renderQueryString)
import Network.HTTP.Server (Handler)
import Network.HTTP.Types.Body (Body (..))
import Network.HTTP.Types.Header (Header, Headers, hAccept, hCacheControl, hContentLength, hContentType, hIfMatch, hIfNoneMatch, hLocation, lookupHeader)
import Network.HTTP.Types.Method (Method (..))
import Network.HTTP.Types.Status (statusCode, pattern Status)
import Network.HTTP.Types.Version qualified as V


-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | One upstream origin (§18.3): identity, transport, and auth material.
data Upstream = Upstream
  { upName :: Text
  -- ^ The module\/namespace name: snapshot domains and surrogate keys are
  -- prefixed @{upName}\/@; it is also the fusion 'ModuleName'.
  , upBase :: Text
  -- ^ Base URL, @http:\/\/host:port@. Used only when 'upTransport' is
  -- 'Nothing' (one plaintext HTTP\/1.1 connection per request; the feed
  -- subscription holds its own long-lived connection).
  , upClaims :: [ClaimName]
  -- ^ The claims this upstream's registry declares; inbound claims are
  -- narrowed to this set before re-minting (§18.8).
  , upMint :: [(ClaimName, A.Value)] -> IO (Text, Text)
  -- ^ Re-mint the narrowed claims: the @vc@ payload (base64url, ready for
  -- the URL parameter) and the @X-Vc-Auth@ proof header value.
  , upServiceAuth :: [(CI ByteString, ByteString)]
  -- ^ Service-principal headers attached to every upstream request
  -- (e.g. an @Authorization@ satisfying priv slices and @nodes@
  -- policies, §18.3).
  , upTransport :: Maybe (Request -> IO Response)
  -- ^ The loopback seam: drive an in-process upstream
  -- ('Lattice.Server.latticeHandler' of its 'Origin') with zero sockets.
  -- 'Nothing' dials 'upBase'.
  , upModuleIdl :: Maybe Text
  -- ^ The module IDL used for __fusion__ when it differs from the served
  -- document — the stub-skeleton pattern: an extension upstream SERVES a
  -- plain schema declaring the foreign entity locally (key + @fetch by@
  -- + its extension members, since an @extend@ block alone declares no
  -- @fetch by@ and would leave @nodes@ forbidden), while its composition
  -- IDL uses @extend entity@. 'Nothing' fuses the served document. This
  -- is a spec gap worth noting: §18 does not say how a standalone
  -- extension origin gates @nodes@ on a foreign type it extends.
  }


data GatewayConfig = GatewayConfig
  { gwUpstreams :: NonEmpty Upstream
  , gwVerifier :: Maybe ProofVerifier
  -- ^ Inbound proof verification — checked ONCE at the gateway (§18.8).
  , gwBudgets :: Budgets
  , gwPurge :: [SurrogateKey] -> IO ()
  -- ^ The gateway's own CDN purge hook. Receives both prefixed and raw
  -- key forms (module haddock, /Invalidation/).
  , gwOnResync :: Text -> IO ()
  -- ^ Full-resync hook: the named upstream's feed was outrun (or lost);
  -- everything held from it must be considered stale. Runs after the
  -- @{upstream}\/*@ wildcard purge is published.
  , gwNow :: IO POSIXTime
  }


-- | Anonymous-friendly defaults: no verifier, default budgets, no-op hooks.
defaultGatewayConfig :: NonEmpty Upstream -> GatewayConfig
defaultGatewayConfig ups =
  GatewayConfig
    { gwUpstreams = ups
    , gwVerifier = Nothing
    , gwBudgets = defaultBudgets
    , gwPurge = const (pure ())
    , gwOnResync = const (pure ())
    , gwNow = getPOSIXTime
    }


data GatewayError
  = -- | Fetching an upstream's schema failed (upstream name, detail).
    GatewayUpstreamUnreachable Text Text
  | -- | An upstream's served IDL failed to parse\/elaborate.
    GatewayUpstreamSchemaInvalid Text [SchemaError]
  | -- | 'fuseModules' rejected the composition.
    GatewayFusionFailed [FusionError]
  deriving stock (Eq, Show)


-- ---------------------------------------------------------------------------
-- Runtime state
-- ---------------------------------------------------------------------------

-- | One upstream at runtime: its spec and served schema (subqueries
-- compile and hash against the served document).
data Up = Up
  { uSpec :: Upstream
  , uSchema :: Schema
  }


-- | Per-request context, keyed by the serving thread.
data ReqCtx = ReqCtx
  { rcClaims :: Claims
  -- ^ Inbound claims (decoded @vc@), for per-upstream re-minting.
  , rcSnaps :: TVar (Map Text Text)
  -- ^ upstream name → its raw @Lattice-Snapshot@ value (@dom=\"tok\"@)
  -- observed during THIS request.
  , rcRows :: TVar (Map (Ref, Text) (Text, Map Text A.Value))
  -- ^ (entity, upstream) → (ver, raw fields) fetched during this
  -- request: serves 'beDerive' and per-src record splitting.
  , rcErrs :: TVar [A.Value]
  -- ^ Scoped, src-tagged error records synthesized at the backend seam
  -- (an extension upstream's partial failure, §18.4) — injected into
  -- the response stream before its @end@ record.
  }


-- | Everything the synthesized backend needs (the origin is NOT in here:
-- backend and origin construct without a knot).
data GwCore = GwCore
  { cCfg :: GatewayConfig
  , cFused :: Fused
  , cUps :: Map ModuleName Up
  , cSeen :: TVar (Map (Ref, Text) (Text, Map Text A.Value))
  -- ^ Last-seen (entity, upstream) rows across requests — the fallback
  -- for splitting and derivation outside a request context.
  , cCtx :: TVar (Map ThreadId ReqCtx)
  }


data Gateway = Gateway
  { gCore :: GwCore
  , gOrigin :: Origin
  , gFeeds :: TVar [ThreadId]
  }


{- | The gateway's own 'Origin' — the deterministic test seam: subscribe
to its invalidation bus ('subscribeInvalidations') before mutating an
upstream, then block on the subscription read; the feed relay delivers
with no sleeping.
-}
gatewayOrigin :: Gateway -> Origin
gatewayOrigin = gOrigin


-- | The fusion result the gateway serves (ownership routing tables).
gatewayFused :: Gateway -> Fused
gatewayFused = cFused . gCore


-- ---------------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------------

{- | Build a gateway: fetch every upstream's served IDL
(@GET \/schema\/current@ → @GET \/schema\/{hash}@), fuse the module IDLs
('upModuleIdl' or the served document) via 'Lattice.Module.fuseModules',
allocate the origin over the fused schema, and open every upstream feed
subscription — synchronously, so a purge published at an upstream right
after 'newGateway' returns is already inside the live tail.
-}
newGateway :: GatewayConfig -> IO (Either GatewayError Gateway)
newGateway cfg = do
  fetched <- forM (NE.toList (gwUpstreams cfg)) $ \u -> do
    r <- fetchIdl u
    pure $ case r of
      Left e -> Left e
      Right idl -> case parseSchema idl of
        Left errs -> Left (GatewayUpstreamSchemaInvalid (upName u) errs)
        Right schema ->
          Right (u, schema, SchemaModule {smName = upName u, smIdl = fromMaybe idl (upModuleIdl u)})
  case sequence fetched of
    Left e -> pure (Left e)
    Right ups -> case fuseModules (NE.fromList [m | (_, _, m) <- ups]) of
      Left errs -> pure (Left (GatewayFusionFailed errs))
      Right fused -> do
        seen <- newTVarIO Map.empty
        ctxs <- newTVarIO Map.empty
        feeds <- newTVarIO []
        let upMap =
              Map.fromList
                [(ModuleName (upName u), Up {uSpec = u, uSchema = s}) | (u, s, _) <- ups]
            core =
              GwCore
                { cCfg = cfg
                , cFused = fused
                , cUps = upMap
                , cSeen = seen
                , cCtx = ctxs
                }
        origin <-
          newOrigin
            OriginConfig
              { ocSchema = fusedSchema fused
              , ocBudgets = gwBudgets cfg
              , ocBackend = gatewayBackend core
              , ocVerifier = gwVerifier cfg
              , ocSnapshotDomain = "gateway"
              , ocPurge = gwPurge cfg
              , ocCors = False
              , ocNow = gwNow cfg
              , ocAdmission = AdmitOpen
              , ocCoalesce = Nothing
              , ocRegistry = Nothing
              , ocLive = defaultLiveConfig
              , ocTelemetry = noTelemetry
              }
        let gw = Gateway {gCore = core, gOrigin = origin, gFeeds = feeds}
        forM_ (Map.elems upMap) $ \up -> do
          mtid <- feedStart gw up
          forM_ mtid $ \tid -> atomically (modifyTVar' feeds (tid :))
        pure (Right gw)


-- | Stop the feed-subscription threads. The origin itself owns none.
shutdownGateway :: Gateway -> IO ()
shutdownGateway gw = do
  tids <- atomically (swapTVar (gFeeds gw) [])
  mapM_ killThread tids


-- | Fetch an upstream's served IDL through the schema endpoints.
fetchIdl :: Upstream -> IO (Either GatewayError Text)
fetchIdl u = do
  r <- try @SomeException $ do
    (st, hdrs, body) <- sendUpstreamRaw u GET "/schema/current" [] BodyEmpty
    case lookupHeader hLocation hdrs of
      Just loc | st >= 300 && st < 400 -> do
        (st2, _, body2) <- sendUpstreamRaw u GET loc [] BodyEmpty
        pure (st2, body2)
      _ -> pure (st, body)
  pure $ case r of
    Left e -> Left (GatewayUpstreamUnreachable (upName u) (tshow e))
    Right (200, body) -> Right (lenientText body)
    Right (st, _) ->
      Left (GatewayUpstreamUnreachable (upName u) ("schema fetch answered " <> tshow st))


-- ---------------------------------------------------------------------------
-- Upstream transport
-- ---------------------------------------------------------------------------

-- | Issue one request to an upstream, fully draining the body.
sendUpstreamRaw :: Upstream -> Method -> ByteString -> Headers -> Body -> IO (Int, Headers, ByteString)
sendUpstreamRaw u method target headers body = do
  resp <- transportOf u (mkUpReq u method target headers body)
  b <- drainBody (responseBody resp)
  pure (fromIntegral (statusCode (responseStatus resp)), responseHeaders resp, b)


-- | The upstream's request function: the loopback seam, or a per-request
-- plaintext HTTP\/1.1 dial of 'upBase' (bodies buffered inside the
-- connection bracket; the feed loop holds its own long-lived bracket).
transportOf :: Upstream -> Request -> IO Response
transportOf u req = case upTransport u of
  Just f -> f req
  Nothing -> withUpConnection u $ \send -> do
    resp <- send req
    b <- drainBody (responseBody resp)
    pure resp {responseBody = if BS.null b then BodyEmpty else BodyBytes b}


withUpConnection :: Upstream -> ((Request -> IO Response) -> IO a) -> IO a
withUpConnection u k = do
  let (host, port) = hostPortOf (upBase u)
  withConnection defaultConnectionConfig {connectionHost = host, connectionPort = port} $ \conn ->
    k (sendOn conn)


hostPortOf :: Text -> (String, String)
hostPortOf base =
  let noScheme = fromMaybe base (T.stripPrefix "http://" base)
      hp = T.takeWhile (/= '/') noScheme
  in case T.splitOn ":" hp of
       [h, p] -> (T.unpack h, T.unpack p)
       _ -> (T.unpack hp, "80")


mkUpReq :: Upstream -> Method -> ByteString -> Headers -> Body -> Request
mkUpReq u method target headers body =
  Request
    { requestMethod = method
    , requestTarget = target
    , requestAuthority = Just authority
    , requestScheme = SchemeHttp
    , requestHeaders = headers <> upServiceAuth u
    , requestBody = body
    , requestVersion = V.HTTP1_1
    , requestTrailers = pure []
    }
  where
    authority =
      let (h, p) = hostPortOf (if T.null (upBase u) then upName u <> ":0" else upBase u)
      in BS8.pack (h <> ":" <> p)


drainBody :: Body -> IO ByteString
drainBody = \case
  BodyEmpty -> pure BS.empty
  BodyBytes bs -> pure bs
  BodyStream pop -> go []
    where
      go acc =
        pop >>= \case
          Nothing -> pure (BS.concat (reverse acc))
          Just c -> go (c : acc)


-- ---------------------------------------------------------------------------
-- The subquery ladder (§18.3)
-- ---------------------------------------------------------------------------

-- | One upstream subresponse: all fetched slices' records merged.
data UpResult = UpResult
  { urRecords :: [Record]
  , urRoots :: Map Text [Ref]
  }


{- | Run one query against an upstream: compile locally against the served
schema, fetch every nonempty data slice hash-form (introducing on
@lattice:unknown-query@), attach per-slice credentials (§18.8), record
the upstream's snapshot component into the request context, and stash
every entity row for splitting\/derivation\/short-circuits.
-}
runUpQuery :: GwCore -> Up -> Text -> Map VarName A.Value -> IO (Either BackendFailure UpResult)
runUpQuery core up text vars = case compileText (uSchema up) (gwBudgets (cCfg core)) text of
  Left e -> pure (Left (internalError (Just ("gateway subquery failed to compile: " <> tshow e))))
  Right compiled -> case planQuery (uSchema up) (gwBudgets (cCfg core)) compiled of
    Left e -> pure (Left (internalError (Just ("gateway subquery failed to plan: " <> tshow e))))
    Right plan -> do
      let slices = [s | s <- [SlicePub, SliceCtx, SlicePriv], Map.member s (planSlices plan)]
      r <- try @SomeException (go compiled plan slices [])
      case r of
        Left e -> pure (Left (BackendFailure "lattice:upstream-unavailable" True (Just (tshow e))))
        Right inner -> do
          forM_ inner (stashRows core up . urRecords)
          pure inner
  where
    go _ _ [] acc =
      pure . Right $
        UpResult
          { urRecords = concat (reverse acc)
          , urRoots = Map.unionsWith mergeRefs (map rootsOf acc)
          }
    go compiled plan (s : rest) acc =
      fetchSlice core up compiled plan s vars >>= \case
        Left f -> pure (Left f)
        Right recs -> go compiled plan rest (recs : acc)

    mergeRefs a b = a <> filter (`notElem` a) b
    rootsOf recs = Map.unionsWith mergeRefs [mRoot m | RManifest m <- recs]


fetchSlice ::
  GwCore ->
  Up ->
  Compiled ->
  Plan ->
  SliceName ->
  Map VarName A.Value ->
  IO (Either BackendFailure [Record])
fetchSlice core up compiled plan slice vars =
  sliceCreds core up plan slice >>= \case
    Nothing -> pure (Right [])
    Just (credParams, credHeaders) -> do
      let params =
            [("p", TE.encodeUtf8 (planId plan)), ("slice", TE.encodeUtf8 (renderSlice slice))]
              <> credParams
              <> varParams vars
          target = targetFor ("/q/" <> encodePathSegment (TE.encodeUtf8 (compiledHash compiled))) params
      (st, hdrs, body) <- sendUpstreamRaw (uSpec up) GET target credHeaders BodyEmpty
      if st == 404 && "unknown-query" `BS.isInfixOf` body
        then do
          (st2, hdrs2, body2) <-
            sendUpstreamRaw
              (uSpec up)
              POST
              (targetFor "/q" (("intent", "introduce") : params))
              ((hContentType, queryMediaType) : credHeaders)
              (BodyBytes (TE.encodeUtf8 (compiledText compiled)))
          finish st2 hdrs2 body2
        else finish st hdrs body
  where
    finish st hdrs body
      | st >= 200 && st < 300 = do
          captureSnapshot core up hdrs
          pure (Right [r | Right r <- decodeRecords body])
      | otherwise =
          pure . Left $
            BackendFailure
              (if st >= 500 then "lattice:upstream-unavailable" else "lattice:internal")
              (st >= 500)
              (Just (upNameOf up <> " answered " <> tshow st <> " on slice " <> renderSlice slice))


-- | Per-slice upstream credentials (§18.8): ctx re-mints the narrowed
-- inbound claims; pub and priv ride the service headers alone. An
-- upstream ctx slice whose required claims the narrowed credential
-- cannot cover is SKIPPED ('Nothing') rather than refused: the gated
-- facts are exactly what this caller could not see anyway, and the
-- upstream would answer 401 to a non-covering payload.
sliceCreds :: GwCore -> Up -> Plan -> SliceName -> IO (Maybe ([(ByteString, ByteString)], Headers))
sliceCreds core up plan = \case
  SliceCtx -> do
    claims <- currentClaims core
    let narrowed = [(c, v) | (c, v) <- Map.toList claims, c `elem` upClaims (uSpec up)]
        required = maybe [] siClaims (Map.lookup SliceCtx (planSlices plan))
    if all (`elem` map fst narrowed) required
      then do
        (vc, proof) <- upMint (uSpec up) narrowed
        pure (Just ([("vc", TE.encodeUtf8 vc)], [(hVcAuth, TE.encodeUtf8 proof)]))
      else pure Nothing
  _ -> pure (Just ([], []))


captureSnapshot :: GwCore -> Up -> Headers -> IO ()
captureSnapshot core up hdrs = forM_ (lookupHeader hLatticeSnapshot hdrs) $ \v -> do
  mctx <- currentCtx core
  forM_ mctx $ \ctx ->
    atomically (modifyTVar' (rcSnaps ctx) (Map.insert (upNameOf up) (lenientText v)))


upNameOf :: Up -> Text
upNameOf = upName . uSpec


targetFor :: ByteString -> [(ByteString, ByteString)] -> ByteString
targetFor path params
  | null params = path
  | otherwise = path <> "?" <> renderQueryString params


varParams :: Map VarName A.Value -> [(ByteString, ByteString)]
varParams = map one . Map.toList
  where
    one (VarName n, v) = (TE.encodeUtf8 n, TE.encodeUtf8 (valueToUrlParam v))


-- ---------------------------------------------------------------------------
-- Request context
-- ---------------------------------------------------------------------------

currentCtx :: GwCore -> IO (Maybe ReqCtx)
currentCtx core = do
  tid <- myThreadId
  Map.lookup tid <$> readTVarIO (cCtx core)


currentClaims :: GwCore -> IO Claims
currentClaims core = maybe Map.empty rcClaims <$> currentCtx core


-- | Record fetched rows into the request cache and the cross-request map.
stashRows :: GwCore -> Up -> [Record] -> IO ()
stashRows core up recs = do
  mctx <- currentCtx core
  let entries = [((erId er, upNameOf up), (erVer er, erFields er)) | REntity er <- recs]
  unless (null entries) . atomically $ do
    modifyTVar' (cSeen core) (\m -> foldl' ins m entries)
    forM_ mctx $ \ctx -> modifyTVar' (rcRows ctx) (\m -> foldl' ins m entries)
  where
    ins m (k, v) = Map.insertWith merge k v m
    merge (nv, nf) (ov, of')
      | nv == ov = (nv, Map.union nf of')
      | otherwise = (nv, nf)


-- | (entity, upstream) → last fetched (ver, fields): request cache first.
lookupRow :: GwCore -> Maybe ReqCtx -> Ref -> Text -> IO (Maybe (Text, Map Text A.Value))
lookupRow core mctx ref src = do
  fromReq <- case mctx of
    Nothing -> pure Nothing
    Just ctx -> Map.lookup (ref, src) <$> readTVarIO (rcRows ctx)
  case fromReq of
    Just hit -> pure (Just hit)
    Nothing -> Map.lookup (ref, src) <$> readTVarIO (cSeen core)


{- | Record scoped, src-tagged error records for the current request —
the §18.4 partial-degradation channel for failures the 'Backend' seam
cannot express alongside a result (an extension upstream failing while
the owner's row still serves). Outside a request context (live-query
re-executions) the errors are dropped with the rest of the src surface.
-}
recordSrcErrors :: GwCore -> [(Maybe Scope, ModuleName, BackendFailure)] -> IO ()
recordSrcErrors _ [] = pure ()
recordSrcErrors core errs = do
  mctx <- currentCtx core
  forM_ mctx $ \ctx ->
    atomically . modifyTVar' (rcErrs ctx) $
      (<> [ withSrc
              (Just (unModuleName m))
              ( RError
                  ErrorRecord
                    { errScope = scope
                    , errCode = Just (bfCode f)
                    , errDomain = Nothing
                    , errRetryable = bfRetryable f
                    , errMessage = bfMessage f
                    }
              )
          | (scope, m, f) <- errs
          ])

-- ---------------------------------------------------------------------------
-- Ownership tables
-- ---------------------------------------------------------------------------

typeOwner :: Fused -> TypeName -> Maybe ModuleName
typeOwner fused t = Map.lookup t (fusedOwner fused)


fieldOwnerOf :: Fused -> TypeName -> FieldName -> Maybe ModuleName
fieldOwnerOf fused t f = case Map.lookup (t, f) (fusedFieldOwner fused) of
  Just m -> Just m
  Nothing -> typeOwner fused t


-- | Modules contributing extension fields to a type (owner excluded).
extendersOf :: Fused -> TypeName -> [ModuleName]
extendersOf fused t =
  Set.toAscList . Set.fromList $
    [ m
    | ((t', _), m) <- Map.toAscList (fusedFieldOwner fused)
    , t' == t
    , Just m /= typeOwner fused t
    ]


-- | Collection name → owning module (mirrors 'Lattice.Module.fuseBackends').
collectionOwners :: Fused -> Map CollectionName ModuleName
collectionOwners fused = Map.fromList (entityCols <> rootCols)
  where
    schema = fusedSchema fused
    entityCols =
      [ (colName (relCollection rel), m)
      | (t, ed) <- Map.toAscList (schemaEntities schema)
      , (f, rel) <- Map.toAscList (entityRels ed)
      , ToMany {} <- [rel]
      , Just m <- [fieldOwnerOf fused t f]
      ]
    rootCols =
      [ (colName col, m)
      | (r, rd) <- Map.toAscList (schemaRoots schema)
      , Just col <- [rootCollection rd]
      , Just m <- [Map.lookup r (fusedRootOwner fused)]
      ]


{- | The argument-free fields of @t@ that module @m@ contributes AND the
projection wants, as declared by the __upstream's served schema__ (the
wire truth for what @m@ can answer). No fallback: empty means this
module has nothing the caller needs.
-}
ownedFieldsFor :: GwCore -> Up -> ModuleName -> TypeName -> Projection -> [FieldName]
ownedFieldsFor core up m t proj = case Map.lookup t (schemaEntities (uSchema up)) of
  Nothing -> []
  Just ed -> map fst (filter wanted (Map.toList (entityFields ed)))
    where
      wanted (f, fd) =
        null (fieldArgs fd)
          && fieldOwnerOf (cFused core) t f == Just m
          && projectsField proj f


{- | 'ownedFieldsFor' at 'ProjectAll', falling back to the served key
fields so a selection is never empty.
-}
fieldsFor :: GwCore -> Up -> ModuleName -> TypeName -> [FieldName]
fieldsFor core up m t = case ownedFieldsFor core up m t ProjectAll of
  [] -> maybe [] (NE.toList . entityKey) (Map.lookup t (schemaEntities (uSchema up)))
  owned -> owned


selectionTextOf :: [FieldName] -> Text
selectionTextOf fs = T.intercalate " " (map unFieldName fs)


-- ---------------------------------------------------------------------------
-- The synthesized backend
-- ---------------------------------------------------------------------------

gatewayBackend :: GwCore -> Backend
gatewayBackend core =
  Backend
    { beSnapshot = snapshot
    , beGetRoot = getRoot
    , beListRoot = listRoot
    , beChildren = children
    , beLoad = load
    , beComputed = \_ _ _ _ -> pure Nothing
    , beMutate = \_ _ _ _ ->
        pure (MutationFailed (internalError (Just "gateway mutations are proxied, not executed")))
    , beAggregate = \_ _ gks -> pure (Right (Map.fromList [(gk, A.Null) | gk <- gks]))
    , beDerive = derive
    , beStoreDerived = \_ _ _ -> pure Map.empty
    }
  where
    fused = cFused core

    upOf m = Map.lookup m (cUps core)

    missing what = internalError (Just ("no upstream owns this " <> what))

    -- The raw namespaced vector; the handler renders the §18.4 header
    -- from the same request context.
    snapshot = do
      mctx <- currentCtx core
      case mctx of
        Nothing -> pure ""
        Just ctx -> do
          snaps <- readTVarIO (rcSnaps ctx)
          pure (T.intercalate "," [u <> "/" <> v | (u, v) <- Map.toAscList snaps])

    -- One nodes query per contributing upstream for the round's key set.
    load t proj keys = case typeOwner fused t of
      Nothing -> pure (Map.fromList [(k, Left (missing "type")) | k <- keys])
      Just owner -> do
        -- An extender none of whose fields the projection wants adds
        -- nothing to the merge: skip its upstream roundtrip entirely.
        let contributes m = case upOf m of
              Nothing -> True -- surfaces the missing-upstream error below
              Just up -> not (null (ownedFieldsFor core up m t proj))
            contributors = owner : filter contributes (extendersOf fused t)
        parts <- forM contributors $ \m -> case upOf m of
          Nothing -> pure (m, Left (missing "type"))
          Just up -> do
            r <- nodesFetch core up m t proj keys
            pure (m, r)
        let (results, extErrs) = assembleLoad t keys owner parts
        recordSrcErrors core [(Just (ScopeEntity ref), m, f) | (ref, m, f) <- extErrs]
        pure results

    children t f parents w = case fieldOwnerOf fused t f >>= \m -> (,) m <$> upOf m of
      Nothing -> pure (Map.fromList [(r, Left (missing "edge")) | (r, _) <- parents])
      Just (m, up) -> childrenFetch core up m t f parents w

    getRoot r args = case Map.lookup r (fusedRootOwner fused) >>= upOf of
      Nothing -> pure (Left (missing "root"))
      Just up ->
        rootFetch core up r args Nothing >>= \case
          Left f -> pure (Left f)
          Right refs -> pure (Right (headMay refs))

    listRoot r args w = case Map.lookup r (fusedRootOwner fused) >>= upOf of
      Nothing -> pure (Left (missing "root"))
      Just up ->
        rootFetch core up r args (Just w) >>= \case
          Left f -> pure (Left f)
          Right refs ->
            pure . Right $
              Page
                { pageRefs = refs
                , pageNext = Nothing
                , pagePrev = Nothing
                , pageTotal = Nothing
                , pageOverflow = False
                }

    -- The derived VALUE is read off the owning upstream's row (§18.1:
    -- the upstream owns the compute; the assembled DepValues are local
    -- placeholders and are ignored).
    derive t f deps = case fieldOwnerOf fused t f of
      Nothing -> pure Map.empty
      Just m -> do
        mctx <- currentCtx core
        found <- forM (Map.keys deps) $ \k -> do
          mrow <- lookupRow core mctx (Ref t k) (unModuleName m)
          pure ((,) k <$> (mrow >>= Map.lookup (unFieldName f) . snd))
        pure (Map.fromList (mapMaybe id found))


headMay :: [a] -> Maybe a
headMay = \case
  [] -> Nothing
  (x : _) -> Just x


{- | One @nodes@ query for a key set at one upstream, its selection
narrowed to the gateway plan's 'Projection' for the type — the fused
plan's field needs propagate to each upstream, whose own planner then
narrows its local 'beLoad' the same way. The key-fields fallback keeps
the selection nonempty when the projection touches none of this module's
fields (the load still verifies existence and fetches @ver@).
-}
nodesFetch :: GwCore -> Up -> ModuleName -> TypeName -> Projection -> [Text] -> IO (Either BackendFailure UpResult)
nodesFetch core up m t proj keys = do
  let fields = case ownedFieldsFor core up m t proj of
        [] -> maybe [] (NE.toList . entityKey) (Map.lookup t (schemaEntities (uSchema up)))
        owned -> owned
      text =
        "query ($refs: EntityRef) { nodes(refs: $refs) { ... on "
          <> unTypeName t
          <> " { "
          <> selectionTextOf fields
          <> " } } }"
      vars = Map.singleton (VarName "refs") (A.toJSON [renderRef (Ref t k) | k <- keys])
  runUpQuery core up text vars


{- | Assemble per-key load results from the contributing upstreams'
subresponses: the owner's row decides existence (an extension row cannot
resurrect an entity, and a policy-denied\/absent ref emits nothing, §14.4
— indistinguishable from nonexistence, which is exactly 'RowAbsent');
extension fields union in. A contributing failure fails the key: a
silently partial record would be indistinguishable from a complete one.
-}
assembleLoad ::
  TypeName ->
  [Text] ->
  ModuleName ->
  [(ModuleName, Either BackendFailure UpResult)] ->
  (Map Text (Either BackendFailure LoadResult), [(Ref, ModuleName, BackendFailure)])
assembleLoad t keys owner parts =
  let assembled = [(k, one k) | k <- keys]
  in (Map.fromList [(k, r) | (k, (r, _)) <- assembled], concat [es | (_, (_, es)) <- assembled])
  where
    one k = case lookup owner parts of
      Nothing -> (Left (internalError (Just "owner upstream missing")), [])
      Just (Left f) -> (Left f, [])
      Just (Right ur) ->
        let ref = Ref t k
        in case refResult ref ur of
             Left f -> (Left f, [])
             Right (Just (RowTomb v)) -> (Right (RowTombstone v), [])
             Right Nothing -> (Right RowAbsent, [])
             Right (Just (RowVals ver fs)) ->
               let (row, errs) = foldl' (extend ref) (EntityRow ver (toFieldMap fs), []) exts
               in (Right (RowFound row), errs)
    exts = [(m, r) | (m, r) <- parts, m /= owner]
    -- An extension upstream's failure does NOT fail the key: the
    -- owner's facts still emit, and the failure degrades exactly that
    -- upstream's contribution as a scoped, src-tagged error (§18.4).
    extend ref (row, errs) (m, sub) = case sub of
      Left f -> (row, (ref, m, f) : errs)
      Right ur -> case refResult ref ur of
        Left f -> (row, (ref, m, f) : errs)
        Right (Just (RowVals _ fs)) ->
          (row {rowFields = rowFields row `Map.union` toFieldMap fs}, errs)
        _ -> (row, errs)
    toFieldMap = Map.mapKeys FieldName


data RefResult = RowVals Text (Map Text A.Value) | RowTomb Text


-- | What one subresponse said about one ref.
refResult :: Ref -> UpResult -> Either BackendFailure (Maybe RefResult)
refResult ref ur = go Nothing (urRecords ur)
  where
    go acc [] = Right acc
    go acc (r : rest) = case r of
      REntity er
        | erId er == ref -> go (Just (merge acc (RowVals (erVer er) (erFields er)))) rest
      RTombstone tr v _ | tr == ref -> go (Just (RowTomb v)) rest
      RError e
        | errScopeRef e == Just ref ->
            Left (BackendFailure (fromMaybe "lattice:internal" (errCode e)) (errRetryable e) (errMessage e))
      _ -> go acc rest
    merge (Just (RowVals _ old)) (RowVals v new) = RowVals v (Map.union new old)
    merge _ new = new


errScopeRef :: ErrorRecord -> Maybe Ref
errScopeRef e = case errScope e of
  Just (ScopeEntity r) -> Just r
  _ -> Nothing


{- | Resolve a @has many@ edge for a round's parents: one @nodes@ query on
the parent refs selecting the edge (window re-rendered as literal
pagination arguments) with the target's full upstream surface, so the
target rows cache for the next round's loads.
-}
childrenFetch ::
  GwCore ->
  Up ->
  ModuleName ->
  TypeName ->
  FieldName ->
  [(Ref, EntityRow)] ->
  Window ->
  IO (Map Ref (Either BackendFailure Page))
childrenFetch core up m t f parents w =
  case Map.lookup t (schemaEntities (fusedSchema (cFused core))) >>= Map.lookup f . entityRels of
    Just rel@ToMany {} -> case relTarget rel of
      TargetEntity tt -> do
        let subsel = selectionTextOf (fieldsFor core up m tt)
            text =
              "query ($refs: EntityRef) { nodes(refs: $refs) { ... on "
                <> unTypeName t
                <> " { "
                <> unFieldName f
                <> windowArgs w
                <> " { "
                <> subsel
                <> " } } } }"
            vars = Map.singleton (VarName "refs") (A.toJSON [renderRef r | (r, _) <- parents])
        runUpQuery core up text vars >>= \case
          Left fl -> pure (failAll fl)
          Right ur -> pure (Map.fromList [(r, parentPage r ur) | (r, _) <- parents])
      _ -> pure (failAll (internalError (Just "federated interface-targeted edges are unsupported")))
    _ -> pure (failAll (internalError (Just "unknown edge")))
  where
    failAll fl = Map.fromList [(r, Left fl) | (r, _) <- parents]

    -- The upstream's verdict per parent, honestly (§18.4): a scoped
    -- error forwards as the edge's failure (a fabricated empty page
    -- would be indistinguishable from the real fact "no children"); a
    -- served parent record carries exactly the edge we selected; a
    -- parent unknown to the extension upstream (never extended) or
    -- tombstoned there has no children to contribute.
    parentPage pref ur = case refResult pref ur of
      Left fl -> Left fl
      Right (Just (RowVals _ fs)) ->
        case [v | (k, v) <- Map.toList fs, k == unFieldName f || (unFieldName f <> "(") `T.isPrefixOf` k] of
          (v : _) -> Right (pageOfValue v)
          [] ->
            Left (internalError (Just (upNameOf up <> " served the parent without the selected edge")))
      _ -> Right (Page [] Nothing Nothing Nothing False)


-- | A wire collection value → 'Page': a plain ref-string array (bounded)
-- or the @{\"$page\":…}@ wrapper (paginated). Boundary cursors forward
-- verbatim when the upstream supplied them.
pageOfValue :: A.Value -> Page
pageOfValue v = case v of
  A.Array _ -> case AT.parseMaybe (A.parseJSON @[Text]) v of
    Just ts -> Page (mapMaybe parseRef ts) Nothing Nothing Nothing False
    Nothing -> Page [] Nothing Nothing Nothing False
  _ -> case pageFromJSON v of
    Just pv -> Page (pvItems pv) (pvNext pv) (pvPrev pv) (pvTotal pv) False
    Nothing -> Page [] Nothing Nothing Nothing False


-- | Re-render a 'Window' as literal pagination arguments.
windowArgs :: Window -> Text
windowArgs = \case
  WWhole _ -> ""
  WPage n dir anchor ->
    let count = case dir of
          PageBackward -> ("last", QInt (fromIntegral n))
          _ -> ("first", QInt (fromIntegral n))
        cur = maybe [] (\c -> [(dirArg dir, QString (cursorText c))]) anchor
    in "(" <> T.intercalate ", " [a <> ": " <> renderQValue q | (a, q) <- count : cur] <> ")"
  where
    dirArg = \case
      PageForward -> "after"
      PageBackward -> "before"
      PageAround -> "around"


-- | Re-encode a decoded cursor for the upstream URL — the §3.2 layout
-- over the embedded spec hash; encode ∘ decode round-trips exactly.
cursorText :: Cursor -> Text
cursorText c =
  "cur_"
    <> TE.decodeUtf8
      (B64U.encodeUnpadded (canonicalJson (A.Array (V.fromList (A.String (curSpec c) : curValues c)))))


-- | Fetch a root subquery: refs come from the manifest's root entry.
rootFetch ::
  GwCore ->
  Up ->
  RootName ->
  Map ArgName A.Value ->
  Maybe Window ->
  IO (Either BackendFailure [Ref])
rootFetch core up r args mw = case Map.lookup r (schemaRoots (uSchema up)) of
  Nothing -> pure (Left (internalError (Just "root not served by its upstream")))
  Just rd -> case rootTarget rd of
    TargetEntity tt -> do
      let m = ModuleName (upNameOf up)
          subsel = selectionTextOf (fieldsFor core up m tt)
          declArgs = [(unArgName n, lit) | (n, v) <- Map.toList args, Just lit <- [jsonLit v]]
          winArgs = case mw of
            Just (WPage n dir anchor) ->
              [ ( case dir of PageBackward -> "last"; _ -> "first"
                , renderQValue (QInt (fromIntegral n))
                )
              ]
                <> maybe
                  []
                  ( \c ->
                      [
                        ( case dir of
                            PageForward -> "after"
                            PageBackward -> "before"
                            PageAround -> "around"
                        , renderQValue (QString (cursorText c))
                        )
                      ]
                  )
                  anchor
            _ -> []
          allArgs = declArgs <> winArgs
          argsTxt
            | null allArgs = ""
            | otherwise = "(" <> T.intercalate ", " [n <> ": " <> v | (n, v) <- allArgs] <> ")"
          text = "query { " <> unRootName r <> argsTxt <> " { " <> subsel <> " } }"
      runUpQuery core up text Map.empty >>= \case
        Left f -> pure (Left f)
        Right ur -> case rootError ur of
          Just f -> pure (Left f)
          Nothing -> pure (Right (rootRefs (unRootName r) (urRoots ur)))
    _ -> pure (Left (internalError (Just "federated interface-targeted roots are unsupported")))
  where
    rootError ur =
      headMay
        [ BackendFailure (fromMaybe "lattice:internal" (errCode e)) (errRetryable e) (errMessage e)
        | RError e <- urRecords ur
        , case errScope e of Just (ScopeRoot rn) -> rn == r; _ -> False
        ]
    -- The upstream manifest keys roots canonically (argument-bearing
    -- roots as e.g. @post(id:"p1")@): exact name first, else the
    -- argument-bearing spelling(s).
    rootRefs name roots = case Map.lookup name roots of
      Just rs -> rs
      Nothing -> concat [rs | (k, rs) <- Map.toList roots, (name <> "(") `T.isPrefixOf` k]


-- | JSON value → query literal (strings quoted; enum-typed arguments are
-- indistinguishable at this seam — module haddock pin).
jsonLit :: A.Value -> Maybe Text
jsonLit = \case
  A.String t -> Just (renderQValue (QString t))
  A.Number n -> Just (renderQValue (QNum n))
  A.Bool b -> Just (renderQValue (QBool b))
  _ -> Nothing


-- ---------------------------------------------------------------------------
-- The handler
-- ---------------------------------------------------------------------------

{- | The gateway's HTTP handler: mutations (named and bound entity-space
verbs) proxy whole to their owning upstream (§18.7); everything else runs
through the fused origin and the §18.4 wire translation.
-}
gatewayHandler :: Gateway -> Handler
gatewayHandler gw req = case mutationRoute (gCore gw) req of
  Just m -> proxyMutation gw m req
  Nothing -> do
    tid <- myThreadId
    snaps <- newTVarIO Map.empty
    rows <- newTVarIO Map.empty
    errs <- newTVarIO []
    let ctx = ReqCtx {rcClaims = inboundClaims req, rcSnaps = snaps, rcRows = rows, rcErrs = errs}
        core = gCore gw
    atomically (modifyTVar' (cCtx core) (Map.insert tid ctx))
    resp <-
      latticeHandler (gOrigin gw) req
        `finally` atomically (modifyTVar' (cCtx core) (Map.delete tid))
    translateResponse gw ctx resp


-- | Decode the inbound @vc@ parameter's claims (verification is the
-- origin's admission; this feeds only re-minting).
inboundClaims :: Request -> Claims
inboundClaims req = fromMaybe Map.empty $ do
  vc <- lookup "vc" (queryParams req)
  either (const Nothing) (Just . cpClaims) (decodeClaims vc)


queryParams :: Request -> [(Text, Text)]
queryParams req = case BS8.break (== '?') (requestTarget req) of
  (_, rest)
    | BS.null rest -> []
    | otherwise ->
        [(lenientText k, lenientText v) | (k, v) <- decodeQueryString (BS.drop 1 rest)]


pathSegments :: Request -> [Text]
pathSegments req =
  [ lenientText (fromMaybe s (percentDecode s))
  | s <- BS8.split '/' (fst (BS8.break (== '?') (requestTarget req)))
  , not (BS.null s)
  ]


lenientText :: ByteString -> Text
lenientText = TE.decodeUtf8With TEE.lenientDecode


-- | Which upstream owns this request, if it is a mutation spelling:
-- @POST \/m\/{name}@ or a bound entity-space verb (§11.7\/§11.8).
mutationRoute :: GwCore -> Request -> Maybe ModuleName
mutationRoute core req = case (requestMethod req, pathSegments req) of
  (POST, ["m", name]) -> Map.lookup (MutationName name) (fusedMutationOwner (cFused core))
  (mth, ("e" : ty : rest))
    | mth `elem` [PUT, PATCH, DELETE] && length rest <= 1 -> byType ty
    | mth == POST && null rest -> byType ty
  _ -> Nothing
  where
    byType ty = typeOwner (cFused core) (TypeName ty)


{- | Proxy a mutation whole (§18.7): forward the body, @Idempotency-Key@,
and conditional headers untouched; re-mint the @vc@ credential (§18.8);
then translate the response — @src@ tags, prefixed keys (@invalidated@
records included), namespaced snapshot.
-}
proxyMutation :: Gateway -> ModuleName -> Request -> IO Response
proxyMutation gw m req = case Map.lookup m (cUps (gCore gw)) of
  Nothing -> pure (problem502 req "no transport for the owning upstream")
  Just up -> do
    body <- drainBody (requestBody req)
    (target, mintHdrs) <- remintTarget up req
    let fwdHeaders =
          [ (n, v)
          | (n, v) <- requestHeaders req
          , n `elem` [hContentType, hIdempotencyKey, hIfMatch, hIfNoneMatch, hAccept]
          ]
            <> mintHdrs
        upReq =
          mkUpReq
            (uSpec up)
            (requestMethod req)
            target
            fwdHeaders
            (if BS.null body then BodyEmpty else BodyBytes body)
    r <- try @SomeException (transportOf (uSpec up) upReq)
    case r of
      Left e -> pure (problem502 req (tshow e))
      Right resp -> do
        rbody <- drainBody (responseBody resp)
        let uname = upNameOf up
            out =
              BS.concat
                [ encodeLine (srcTagRecord uname rec)
                | Right rec <- decodeRecords rbody
                ]
            newBody = if BS.null rbody then rbody else out
            hdrs = setContentLength (BS.length newBody) (map (prefixMutHeader uname) (responseHeaders resp))
        pure
          resp
            { responseBody = if BS.null newBody then BodyEmpty else BodyBytes newBody
            , responseHeaders = hdrs
            }
  where
    prefixMutHeader uname (n, v)
      | n == hSurrogateKey =
          (n, TE.encodeUtf8 (T.unwords [uname <> "/" <> k | k <- T.words (lenientText v)]))
      | n == hLatticeSnapshot = (n, TE.encodeUtf8 (uname <> "/" <> lenientText v))
      | otherwise = (n, v)


-- | Rewrite the request target's @vc@ with the re-minted per-upstream
-- credential (§18.8); every other parameter forwards verbatim.
remintTarget :: Up -> Request -> IO (ByteString, Headers)
remintTarget up req = do
  let params = queryParams req
      path = fst (BS8.break (== '?') (requestTarget req))
  case lookup "vc" params of
    Nothing -> pure (requestTarget req, [])
    Just vcT -> do
      let claims = either (const Map.empty) cpClaims (decodeClaims vcT)
          narrowed = [(c, v) | (c, v) <- Map.toList claims, c `elem` upClaims (uSpec up)]
      (vc, proof) <- upMint (uSpec up) narrowed
      let params' =
            [ (TE.encodeUtf8 k, TE.encodeUtf8 (if k == "vc" then vc else v))
            | (k, v) <- params
            ]
      pure (targetFor path params', [(hVcAuth, TE.encodeUtf8 proof)])


problem502 :: Request -> Text -> Response
problem502 req detail =
  Response
    { responseStatus = Status 502
    , responseVersion = requestVersion req
    , responseHeaders = [(hContentType, "application/problem+json"), (hCacheControl, "no-store")]
    , responseBody =
        BodyBytes . BL.toStrict . A.encode $
          A.object
            [ "type" A..= ("https://lattice.dev/problems/upstream-unavailable" :: Text)
            , "status" A..= (502 :: Int)
            , "detail" A..= detail
            ]
    , responseTrailers = pure []
    , responseH2StreamId = 0
    , responseCancel = pure ()
    , responsePushPromises = pure []
    }


-- ---------------------------------------------------------------------------
-- §18.4 wire translation
-- ---------------------------------------------------------------------------

{- | Post-process a fused origin response (module haddock, /Wire
composition/). Streaming (SSE) bodies pass through untouched. Injected
extension-failure errors degrade the response exactly as origin-side
degradation would (§9.4.6): a clean @200@ becomes @207 Multi-Status@
with @Lattice-Outcome: degraded@, and the response self-purges its keys
(§9.4.5) through the gateway bus.
-}
translateResponse :: Gateway -> ReqCtx -> Response -> IO Response
translateResponse gw ctx resp = case responseBody resp of
  BodyBytes body | isNdjson (responseHeaders resp) -> do
    rows <- readTVarIO (rcRows ctx)
    seen <- readTVarIO (cSeen core)
    snaps <- readTVarIO (rcSnaps ctx)
    injected <- readTVarIO (rcErrs ctx)
    let verOf ref src = fst <$> firstJust (Map.lookup (ref, src) rows) (Map.lookup (ref, src) seen)
        recs = [r | Right r <- decodeRecords body]
        (pre, post) = break (\case REnd {} -> True; _ -> False) recs
        out =
          BS.concat . map encodeLine $
            concatMap (translateRecord core verOf) pre
              <> injected
              <> concatMap (translateRecord core verOf) post
        hdrs0 = map (prefixKeysHeader core) (responseHeaders resp)
        hdrs1
          | Map.null snaps = hdrs0
          | otherwise =
              [ if n == hLatticeSnapshot then (n, TE.encodeUtf8 (renderSnapVector snaps)) else (n, v)
              | (n, v) <- hdrs0
              ]
        degradedNow = not (null injected) && statusCode (responseStatus resp) == 200
        hdrs2
          | not degradedNow = hdrs1
          | otherwise = case lookupHeader hLatticeOutcome hdrs1 of
              Just _ -> hdrs1
              Nothing -> (hLatticeOutcome, "degraded") : hdrs1
        selfPurgeKeys =
          maybe [] (T.words . lenientText) (lookupHeader hSurrogateKey hdrs1)
            <> maybe [] (T.words . lenientText) (lookupHeader hSurrogateKey (responseHeaders resp))
    when degradedNow (publishPurge (gOrigin gw) selfPurgeKeys)
    pure
      resp
        { responseBody = BodyBytes out
        , responseHeaders = setContentLength (BS.length out) hdrs2
        , responseStatus = if degradedNow then Status 207 else responseStatus resp
        }
  _ -> pure resp
  where
    firstJust a b = maybe b Just a
    core = gCore gw


isNdjson :: Headers -> Bool
isNdjson hdrs = case lookupHeader hContentType hdrs of
  Just v -> "ndjson" `BS.isInfixOf` v
  Nothing -> False


-- | @posts\/main=\"…\", social\/main=\"…\"@ (§18.4).
renderSnapVector :: Map Text Text -> Text
renderSnapVector snaps = T.intercalate ", " [u <> "/" <> raw | (u, raw) <- Map.toAscList snaps]


prefixKeysHeader :: GwCore -> Header -> Header
prefixKeysHeader core (n, v)
  | n == hSurrogateKey =
      (n, TE.encodeUtf8 (T.unwords (map (prefixKey core) (T.words (lenientText v)))))
  | otherwise = (n, v)


-- | @Type:key@ → owner prefix; @{collection}:{grouping}@ → collection
-- owner; @plan:{id}@ and unknown shapes stay gateway-local.
prefixKey :: GwCore -> SurrogateKey -> SurrogateKey
prefixKey core k
  | "plan:" `T.isPrefixOf` k = k
  | otherwise = case T.breakOn ":" k of
      (name, rest)
        | T.null rest -> k
        | otherwise -> case Map.lookup (TypeName name) (fusedOwner (cFused core)) of
            Just (ModuleName m) -> m <> "/" <> k
            Nothing -> case Map.lookup (CollectionName name) (collectionOwners (cFused core)) of
              Just (ModuleName m) -> m <> "/" <> k
              Nothing -> k


-- | Split\/tag one record for the fused wire (module haddock).
translateRecord :: GwCore -> (Ref -> Text -> Maybe Text) -> Record -> [A.Value]
translateRecord core verOf = \case
  REntity er ->
    let t = refType (erId er)
        owner = ownerName t
        grouped =
          Map.toAscList $
            Map.fromListWith
              Map.union
              [ (srcOf t k, Map.singleton k v)
              | (k, v) <- Map.toList (erFields er)
              ]
        ownerFields = fromMaybe Map.empty (lookup owner grouped)
        extGroups = [(s, fs) | (s, fs) <- grouped, s /= owner]
        ownerRec = A.toJSON (REntity er {erFields = ownerFields, erSrc = Just owner})
        extRec (s, fs) =
          A.toJSON
            ( REntity
                er
                  { erFields = fs
                  , erSrc = Just s
                  , erVer = fromMaybe (erVer er) (verOf (erId er) s)
                  }
            )
    in ownerRec : map extRec extGroups
  r@(RTombstone ref _ _) -> [withSrc (ownerOf (refType ref)) r]
  r@(RElided ref) -> [withSrc (ownerOf (refType ref)) r]
  r@(RUnchanged ref _) -> [withSrc (ownerOf (refType ref)) r]
  r@(RError e) -> [withSrc (errorSrc core e) r]
  r -> [A.toJSON r]
  where
    fused = cFused core
    ownerOf t = unModuleName <$> Map.lookup t (fusedOwner fused)
    ownerName t = fromMaybe "" (ownerOf t)
    -- Field keys carry canonical arguments (@reactions(first:5)@);
    -- ownership is by the base name.
    srcOf t k =
      maybe (ownerName t) unModuleName (fieldOwnerOf fused t (FieldName (T.takeWhile (/= '(') k)))


-- | The @src@ of a scoped error: the owner of the scope's target (§18.4:
-- scoped errors forward with scopes intact, tagged by source).
errorSrc :: GwCore -> ErrorRecord -> Maybe Text
errorSrc core e = case errScope e of
  Just (ScopeEntity r) -> unModuleName <$> typeOwner fused (refType r)
  Just (ScopeField r f) -> unModuleName <$> fieldOwnerOf fused (refType r) f
  Just (ScopeEdge r f) -> unModuleName <$> fieldOwnerOf fused (refType r) f
  Just (ScopeRoot rn) -> unModuleName <$> Map.lookup rn (fusedRootOwner fused)
  _ -> Nothing
  where
    fused = cFused core


-- | Add a @src@ member to a record's wire JSON (decoder-tolerant: typed
-- decoders without a slot drop it; module haddock).
withSrc :: Maybe Text -> Record -> A.Value
withSrc msrc r = case (msrc, A.toJSON r) of
  (Just s, A.Object o) -> A.Object (KM.insert "src" (A.String s) o)
  (_, v) -> v


-- | Tag every record of a proxied mutation stream with its upstream and
-- prefix @invalidated@ keys (§18.7).
srcTagRecord :: Text -> Record -> A.Value
srcTagRecord uname = \case
  REntity er -> A.toJSON (REntity er {erSrc = Just uname})
  RInvalidated keys item -> A.toJSON (RInvalidated (map ((uname <> "/") <>) keys) item)
  r@RTombstone {} -> withSrc (Just uname) r
  r@RElided {} -> withSrc (Just uname) r
  r@RUnchanged {} -> withSrc (Just uname) r
  r@RError {} -> withSrc (Just uname) r
  r -> A.toJSON r


encodeLine :: A.Value -> ByteString
encodeLine v = BL.toStrict (A.encode v) <> "\n"


setContentLength :: Int -> Headers -> Headers
setContentLength len =
  map (\(n, v) -> if n == hContentLength then (n, BS8.pack (show len)) else (n, v))


-- ---------------------------------------------------------------------------
-- §18.6 feed subscription
-- ---------------------------------------------------------------------------

{- | Open one upstream's feed subscription synchronously (the caller
returns only once the upstream answered the stream head) and fork the
drain loop. A refused head resyncs immediately and forks nothing.
-}
feedStart :: Gateway -> Up -> IO (Maybe ThreadId)
feedStart gw up = do
  r <- try @SomeException (feedConnect up Nothing)
  case r of
    Left _ -> feedResync gw up >> pure Nothing
    Right Nothing -> feedResync gw up >> pure Nothing
    Right (Just pop) -> Just <$> forkIO (feedRun gw up pop Nothing)


-- | @GET \/invalidations?[since=n&]live=sse@ → the SSE frame popper, or
-- 'Nothing' on a non-200 head.
feedConnect :: Up -> Maybe Word64 -> IO (Maybe (IO (Maybe SseFrame)))
feedConnect up since = do
  let params = maybe [] (\c -> [("since", BS8.pack (show c))]) since <> [("live", "sse")]
      hdrs =
        (hAccept, "text/event-stream")
          : maybe [] (\c -> [("Last-Event-ID", BS8.pack (show c))]) since
  resp <- transportOf (uSpec up) (mkUpReq (uSpec up) GET (targetFor "/invalidations" params) hdrs BodyEmpty)
  if statusCode (responseStatus resp) /= 200
    then pure Nothing
    else
      Just <$> case responseBody resp of
        BodyEmpty -> pure (pure Nothing)
        BodyBytes bs -> do
          frames <- newTVarIO (parseEventStream bs)
          pure . atomically $ do
            fs <- readTVar frames
            case fs of
              [] -> pure Nothing
              f : rest -> writeTVar frames rest >> pure (Just f)
        BodyStream p -> sseFramePopper (fromMaybe BS.empty <$> p)


{- | Drain feed events, republishing keys prefixed AND raw through the
gateway's bus; reconnect with @since=@ on EOF; resync on outrun, refused
reconnect, or exception (module haddock, /Invalidation/).
-}
feedRun :: Gateway -> Up -> IO (Maybe SseFrame) -> Maybe Word64 -> IO ()
feedRun gw up pop0 since0 = do
  r <- try @SomeException (drainFrom pop0 since0)
  case r of
    Left _ -> feedResync gw up
    Right () -> pure ()
  where
    uname = upNameOf up

    drainFrom pop since = do
      lastC <- drain pop since True
      case lastC of
        Nothing | isJust since -> reconnect since
        Nothing -> feedResync gw up
        Just c -> reconnect (Just c)

    reconnect since =
      feedConnect up since >>= \case
        Nothing -> feedResync gw up
        Just pop -> drainFrom pop since

    -- Returns the last cursor seen ('Nothing': no event before EOF).
    drain pop since = loopD Nothing
      where
        loopD lastC first =
          pop >>= \case
            Nothing -> pure lastC
            Just (SseDispatch ev) -> case decodeFeedEvent (sseData ev) of
              Nothing -> loopD lastC first
              Just (cursor, keys) -> do
                -- Outrun rule (§18.6): the first replayed cursor after a
                -- resume must be since+1; a gap means the bounded window
                -- was outrun and the gap is unknowable.
                if first && isJust since && Just (cursor - 1) > since
                  then do
                    feedResync gw up
                    pure (Just cursor)
                  else do
                    publishPurge (gOrigin gw) (map ((uname <> "/") <>) keys <> keys)
                    loopD (Just cursor) False
            Just _ -> loopD lastC first


-- | The §18.6 resync pin: publish the full-wildcard purge for this
-- upstream and call the deployment hook.
feedResync :: Gateway -> Up -> IO ()
feedResync gw up = do
  publishPurge (gOrigin gw) [upNameOf up <> "/*"]
  gwOnResync (cCfg (gCore gw)) (upNameOf up)


decodeFeedEvent :: ByteString -> Maybe (Word64, [Text])
decodeFeedEvent bs = do
  v <- A.decodeStrict bs
  AT.parseMaybe (A.withObject "feed-event" $ \o -> (,) <$> o A..: "cursor" <*> o A..: "keys") v


tshow :: (Show a) => a -> Text
tshow = T.pack . show
