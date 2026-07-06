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
* __@X-Have@ \/ digest elision and live queries are not implemented__;
  @project=refs@ covers the partial-loading path.
* 'ocNow' is carried for deployment wiring (e.g. building a
  'ProofVerifier'); the handler itself never reads the clock.
-}
module Lattice.Server (
  OriginConfig (..),
  Origin,
  newOrigin,
  latticeHandler,
) where

import Control.Concurrent.STM
import Control.Exception (SomeAsyncException (..), SomeException, catch, fromException, onException, throwIO)
import Control.Monad (unless, when)
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
import Data.Foldable (traverse_)
import Data.List (find, sort, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe, maybeToList)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Vector qualified as V
import Lattice.Backend
import Lattice.Canonical (Compiled (..), canonicalFieldKey, compileText)
import Lattice.Compress (Dictionary, decompressQuery, schemaDictionary)
import Lattice.Hash (b64url, blake3, dictHash, manifestEtagHash, schemaHash)
import Lattice.IDL.Print (canonicalIdl)
import Lattice.Plan
import Lattice.Query.AST (Argument (..), Field (..), QValue (..), Selection (..), TypeRefQ (..), VarDef (..))
import Lattice.Query.Validate (CompileError (..), normalizeTypeAlias)
import Lattice.Schema
import Lattice.Server.Auth
import Lattice.Server.Execute
import Lattice.Types
import Lattice.Value
import Lattice.Wire
import Network.HTTP.Message (Request (..), Response (..))
import Network.HTTP.PercentEncoding (decodeQueryString, encodePathSegment, encodeQueryComponent, percentDecode)
import Network.HTTP.Server (Handler)
import Network.HTTP.Types.Body (Body (..))
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method (Method (..))
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
  , oMemo :: TVar (Map Text (Compiled, Plan))
  , oIdem :: TVar (Map (MutationName, Text, Text) IdemEntry)
  , oTenure :: TVar (Map Text Int)
  }


-- | Allocate the origin's shared state and precompute the schema documents.
newOrigin :: OriginConfig -> IO Origin
newOrigin cfg = do
  let idl = canonicalIdl (ocSchema cfg)
      dict = schemaDictionary (ocSchema cfg)
  Origin cfg idl (schemaHash idl) dict (dictHash dict)
    <$> newTVarIO Map.empty
    <*> newTVarIO Map.empty
    <*> newTVarIO Map.empty


-- ---------------------------------------------------------------------------
-- Handler and routing
-- ---------------------------------------------------------------------------

latticeHandler :: Origin -> Handler
latticeHandler origin req = do
  resp <- route origin req `catch` onCrash
  pure (addCorsHeaders (ocCors (oConfig origin)) resp)
  where
    onCrash :: SomeException -> IO Response
    onCrash e
      | Just SomeAsyncException {} <- fromException e = throwIO e
      | otherwise =
          pure . problemResponse req $
            (mkProblem 500 "lattice:internal") {pDetail = Just "unhandled exception"}


route :: Origin -> Request -> IO Response
route o req = case requestMethod req of
  OPTIONS
    | ocCors (oConfig o) -> pure (preflight req)
  GET -> case segs of
    [".well-known", "lattice"] -> pure (discovery o req)
    ["schema", "current"] -> pure (schemaCurrent o req)
    ["schema", "dict", h] -> pure (schemaDict o req h)
    ["schema", h] -> pure (schemaDoc o req h)
    ["q"] -> case lookup "d" params of
      Just d -> serveQuery o req params (QInline d (lookup "dv" params)) NotIntro
      Nothing -> pure (problemResponse req (badRequest ["the inline query form requires the d parameter"]))
    ["q", h] -> serveQuery o req params (QHash h) NotIntro
    ["q", h, "source"] -> serveSource o req h
    ["q", h, "explain"] -> serveExplain o req h
    ["q", h, "plan", pid] -> servePlanDoc o req h pid
    ["e", ty, key] -> serveEntity o req params ty key
    _ -> pure (fallback req segs)
  QUERY -> case segs of
    ["q"] -> withQueryBody req $ \txt -> serveQuery o req params (QText txt) Introduce
    _ -> pure (fallback req segs)
  POST -> case segs of
    ["q"] -> case lookup "intent" params of
      Just "oneshot" -> withQueryBody req $ \txt -> serveQuery o req params (QText txt) Oneshot
      Just "introduce" -> asIntro
      Nothing -> asIntro
      Just other -> pure (problemResponse req (badRequest ["unknown intent: " <> other]))
    ["m", name] -> serveMutation o req params (MutationName name)
    _ -> pure (fallback req segs)
  _ -> pure (fallback req segs)
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


-- | 405 with @Allow@ for known paths under the wrong method, else 404.
fallback :: Request -> [Text] -> Response
fallback req segs = case allowedFor segs of
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
      ["e", _, _] -> ["GET"]
      ["m", _] -> ["POST"]
      _ -> []


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
    , ("Access-Control-Allow-Methods", "GET, POST, QUERY, OPTIONS")
    , ("Access-Control-Allow-Headers", "Content-Type, Idempotency-Key, X-Vc-Auth, X-Have, Lattice-Query-Name")
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
      , "admission" .= ("open" :: Text)
      , "queryMediaType" .= TE.decodeUtf8 queryMediaType
      , "methods" .= A.object ["introduce" .= (["QUERY", "POST"] :: [Text])]
      , "dictionary"
          .= A.object
            [ "current" .= ("/schema/dict/" <> oDictHash o)
            , "algorithm" .= ("deflate-raw/9" :: Text)
            ]
      , "budgets" .= budgetsJson (ocBudgets (oConfig o))
      , "idempotency" .= A.object ["defaultRetention" .= ("PT24H" :: Text)]
      ]


budgetsJson :: Budgets -> A.Value
budgetsJson b =
  A.object
    [ "maxCanonicalBytes" .= maxCanonicalBytes b
    , "maxDepth" .= maxDepth b
    , "maxRoots" .= maxRoots b
    , "maxRounds" .= maxRounds b
    , "maxRoundFanout" .= maxRoundFanout b
    , "maxSurrogateKeys" .= maxSurrogateKeys b
    , "maxBatchItems" .= maxBatchItems b
    , "maxPageDefault" .= maxPageDefault b
    , "coalesceWindowMs" .= coalesceWindowMs b
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
resolveQuery :: Origin -> QueryInput -> Intro -> IO (Either Problem (Compiled, Plan))
resolveQuery o input intro = case input of
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
        Right txt -> compileMemo o True txt
  QText txt -> compileMemo o (intro /= Oneshot) txt
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


compileMemo :: Origin -> Bool -> Text -> IO (Either Problem (Compiled, Plan))
compileMemo o memoize txt =
  case compiled of
    Left ce -> pure (Left (compileProblem ce))
    Right (c, p) -> do
      when memoize . atomically . modifyTVar' (oMemo o) $
        Map.insert (compiledHash c) (c, p)
      pure (Right (c, p))
  where
    schema = ocSchema (oConfig o)
    budgets = ocBudgets (oConfig o)
    compiled = do
      c <- compileText schema budgets txt
      p <- planQuery schema budgets c
      pure (c, p)


serveQuery :: Origin -> Request -> [(Text, Text)] -> QueryInput -> Intro -> IO Response
serveQuery o req params input intro =
  resolveQuery o input intro >>= \case
    Left p -> pure (problemResponse req p)
    Right (c, plan) -> case lookup "p" params of
      Just pid
        | pid /= planId plan ->
            pure . problemResponse req $
              (mkProblem 409 "lattice:plan-superseded")
                {pExtra = ["plan" .= RPlan (planSliceRecord plan)]}
      _ -> case maybe (Right defSlice) parseSliceParam (lookup "slice" params) of
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
          let env =
                ExecEnv
                  { xSchema = schema
                  , xBudgets = ocBudgets cfg
                  , xBackend = ocBackend cfg
                  , xClaims = claims
                  , xVars = vars
                  , xMode = EmitEq slice
                  }
          executeRoots env (planRoots plan) >>= \case
            Left AbortCursorRetired -> pure (problemResponse req (mkProblem 410 "lattice:cursor-retired"))
            Left AbortCursorMalformed -> pure (problemResponse req (badRequest ["malformed cursor"]))
            Right xr -> do
              snap <- beSnapshot (ocBackend cfg)
              promoted <- case intro of
                Oneshot -> pure True
                _ -> bumpTenure o (compiledHash c)
              let etag = manifestEtag plan vars vcRaw (xrIdVers xr)
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
                      <> projectRecords refsOnly (xrRecords xr)
                      <> [REnd (EndRecord (xrComplete xr) (Just etag))]
                  keys = coarsenKeys (ocBudgets cfg) (planId plan) xr
                  cc = cacheControlFor slice intro promoted
                  common =
                    [ (hETag, weakEtag etag)
                    , planHdr plan
                    , schemaHdr o
                    , snapshotHdr cfg snap
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
              when (xrDegraded xr) (ocPurge cfg keys)
              if inmMatch req (weakEtag etag)
                then pure (mkResponse req 304 ((hCacheControl, cc) : common) "")
                else
                  pure $
                    mkResponse
                      req
                      status
                      ([(hContentType, ndjsonType), (hCacheControl, cc)] <> common <> outcomeHdrs <> introHdrs)
                      (encodeRecords body)


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
bumpTenure o h = atomically $ do
  m <- readTVar (oTenure o)
  let n = 1 + Map.findWithDefault 0 h m
  writeTVar (oTenure o) (Map.insert h n m)
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
-}
manifestEtag :: Plan -> Map VarName A.Value -> Text -> [(Text, Text)] -> Text
manifestEtag plan vars vcRaw idvers =
  manifestEtagHash . canonicalJson . A.Array . V.fromList $
    [ A.String (planId plan)
    , A.Object (KM.fromList (map (\(VarName n, v) -> (AK.fromText n, v)) (Map.toAscList vars)))
    , A.String vcRaw
    , A.toJSON (map (\(i, v) -> [i, v]) idvers)
    ]


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
                        , xBackend = ocBackend cfg
                        , xClaims = claims
                        , xVars = Map.empty
                        , xMode = EmitAtMost needSlice
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
  let common etag =
        catMaybes
          [ Just (snapshotHdr cfg snap)
          , Just (schemaHdr o)
          , Just (keysHdr [entityKeyOf ref])
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
          case pinned of
            Just want
              | want /= v -> pure (problemResponse req (versionUnavailable req params ref))
              | otherwise ->
                  pure $
                    mkResponse
                      req
                      200
                      ([(hContentType, ndjsonType), (hCacheControl, immutableCC)] <> common (Just v))
                      (encodeRecords (frame [REntity er] (Just v)))
            Nothing
              | inmMatch req (strongEtag v) ->
                  pure (mkResponse req 304 ((hCacheControl, unpinnedCC) : common (Just v)) "")
              | otherwise ->
                  pure $
                    mkResponse
                      req
                      200
                      ([(hContentType, ndjsonType), (hCacheControl, unpinnedCC)] <> common (Just v))
                      (encodeRecords (frame [REntity er] (Just v)))
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
    firstErr =
      listToMaybe $
        mapMaybe
          ( \case
              RError e -> Just e
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
serveMutation o req params name = do
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
              Right invocation ->
                withIdempotency o req name principalKey body $
                  runMutation o req name mdef claims callerSlice invocation


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
withIdempotency :: Origin -> Request -> MutationName -> Text -> ByteString -> IO Response -> IO Response
withIdempotency o req name principalKey body act =
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
          | idDigest done == digest ->
              pure $
                mkResponse
                  req
                  (idStatus done)
                  (insertHeader hIdempotencyReplayed "true" (idHeaders done))
                  (idBody done)
          | otherwise -> pure (problemResponse req (mkProblem 422 "lattice:key-reuse"))
        Nothing -> do
          resp <- act `onException` atomically (modifyTVar' (oIdem o) (Map.delete k))
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
    statusInt r = case responseStatus r of
      Status w -> fromIntegral w
    bodyBytesOf r = case responseBody r of
      BodyBytes bs -> bs
      _ -> BS.empty


runMutation ::
  Origin ->
  Request ->
  MutationName ->
  MutationDef ->
  Claims ->
  SliceName ->
  Invocation ->
  IO Response
runMutation o req name mdef claims callerSlice = \case
  Singular args -> do
    outcome <- beMutate (ocBackend cfg) name claims args
    case outcome of
      MutationDenied -> pure (problemResponse req forbidden)
      MutationFailed bf ->
        pure (problemResponse req ((mkProblem 500 "lattice:internal") {pDetail = bfMessage bf}))
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
              status = if degraded then 207 else 200
          ocPurge cfg keys
          pure (mutResponse o req status (crSnapshot cr) keys (encodeRecords bodyRecs))
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
    unless (null keys) (ocPurge cfg keys)
    pure (mutResponse o req status snap keys (encodeRecords bodyRecs))
  BatchInv AllOrNothing items -> goAll items []
  where
    cfg = oConfig o
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
      unless (null keys) (ocPurge cfg keys)
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
  outcome <- beMutate (ocBackend (oConfig o)) name claims args
  case outcome of
    MutationDenied -> pure (errItem (Just "lattice:forbidden") Nothing Nothing False False)
    MutationFailed bf ->
      pure (errItem (Just (bfCode bf)) Nothing (bfMessage bf) (bfRetryable bf) False)
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
          rows <- beLoad backend t keys
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
