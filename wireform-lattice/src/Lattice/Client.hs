{- | The Lattice HTTP client: query, mutate, and point-fetch against an
origin, patching a normalized 'Store' ("Lattice.Client.Store") with every
response.

== Transport ladder (§5.2, §6)

With a schema ('ccSchema' present) a query compiles locally
('Lattice.Canonical.compileText' + 'Lattice.Plan.planQuery'); the client
fetches every nonempty data slice of the local plan (pub, then ctx, then
priv), each as a hash-form @GET \/q\/{hash}?p={planId}&slice=…&{vars}@.
A @404 lattice:unknown-query@ falls back to introduction
(@POST \/q?intent=introduce@ with the canonical text as the body,
@application\/x-lattice-query@), and the returned @Location@ is
remembered per canonical text.

Without a schema the client cannot canonicalize, so the first use of a
query introduces its raw text, remembers the returned @Location@ /
@Lattice-Plan@ per input text, learns the nonempty slice set from the
plan record (@GET \/q\/{hash}@ with no @slice@), and uses hash-form GETs
thereafter. Schema-less limitations (documented, accepted for v1): field
keys that rely on schema-declared argument defaults may not match the
origin's canonical keys, and schema fragments are not expanded during
denormalization.

Requests attach credentials per slice: @vc@ (the encoded claims payload,
'Lattice.Server.Auth.encodeClaims') plus the @X-Vc-Auth@ proof header on
ctx fetches, and the @Authorization@ header on priv fetches. Mutations
and point fetches attach whatever credentials the config carries.

== Out of scope for v1 (deliberate)

* @\/.well-known\/lattice@ discovery is not fetched; base paths are the
  fixed @\/q@, @\/m@, @\/e@, @\/schema@.
* Conditional requests (@If-None-Match@ \/ 304 revalidation), the
  @X-Have@ cache digest, and live mode (SSE) are not implemented.
* The compressed inline form (@\/q?d=…@) is not used.

A whole-request failure surfaces as 'LatticeError'; RFC 9457 problem
bodies parse into 'HttpProblem'.
-}
module Lattice.Client (
  -- * Configuration
  ClientConfig (..),
  defaultClientConfig,
  LatticeClient,
  withLatticeClient,
  clientStore,

  -- * Operations
  query,
  mutate,
  pointFetch,

  -- * Results
  QueryResult (..),
  MutationResult (..),
  LatticeError (..),
) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Exception (Handler (..), IOException, catches)
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.Either (partitionEithers)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Data.Vector qualified as V
import Lattice.Canonical (Compiled (..), compileText)
import Lattice.Client.Store
import Lattice.Plan (Plan (..), planQuery)
import Lattice.Query.AST (Document)
import Lattice.Query.Parser (ParseError (..), parseDocument)
import Lattice.Query.Validate (CompileError)
import Lattice.Schema (Schema, defaultBudgets)
import Lattice.Server.Auth (encodeClaims)
import Lattice.Types
import Lattice.Value (valueToUrlParam)
import Lattice.Wire
import Network.HTTP.Connection (
  Connection,
  ConnectionConfig (..),
  ConnectionError,
  defaultConnectionConfig,
  sendOn,
  withConnection,
 )
import Network.HTTP.Message (Request (..), Response (..), Scheme (..))
import Network.HTTP.PercentEncoding (encodePathSegment, renderQueryString)
import Network.HTTP.Types.Body (Body (..))
import Network.HTTP.Types.Header (Headers, lookupHeader)
import Network.HTTP.Types.Method (Method (..))
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.Version qualified as V


-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

data ClientConfig = ClientConfig
  { ccHost :: String
  , ccPort :: String
  , ccSchema :: Maybe Schema
  -- ^ Enables local compilation and full denormalization. Must match the
  -- origin's schema (drift surfaces as @409 lattice:plan-superseded@).
  , ccClaims :: Maybe (Claims, Text)
  -- ^ Presented claims and the @X-Vc-Auth@ proof header value.
  , ccAuthorization :: Maybe ByteString
  -- ^ @Authorization@ header value for priv-slice fetches.
  }


-- | Plaintext HTTP\/1.1 against the demo origin's default port (8917),
-- anonymous, schema-less.
defaultClientConfig :: ClientConfig
defaultClientConfig =
  ClientConfig
    { ccHost = "127.0.0.1"
    , ccPort = "8917"
    , ccSchema = Nothing
    , ccClaims = Nothing
    , ccAuthorization = Nothing
    }


-- | What introduction taught us about one query, keyed by canonical text
-- (schema mode) or raw input text (schema-less mode).
data KnownQuery = KnownQuery
  { kqHash :: Text
  , kqPlanId :: Maybe Text
  , kqSlices :: [SliceName]
  }


data LatticeClient = LatticeClient
  { lcConfig :: ClientConfig
  , lcConn :: Connection
  , lcStore :: Store
  , lcKnown :: TVar (Map Text KnownQuery)
  }


-- | The client's normalized entity store: every response and mutation
-- result is applied to it, giving read-your-writes locally.
clientStore :: LatticeClient -> Store
clientStore = lcStore


{- | Open one plaintext HTTP\/1.1 connection to the origin and run the
action. The connection is dialed eagerly (an unreachable origin throws
'IOException' out of the bracket); per-request transport failures after
that surface as 'TransportError'.
-}
withLatticeClient :: ClientConfig -> (LatticeClient -> IO a) -> IO a
withLatticeClient cfg action = do
  store <- newStore
  known <- newTVarIO Map.empty
  let connCfg =
        defaultConnectionConfig
          { connectionHost = ccHost cfg
          , connectionPort = ccPort cfg
          }
  withConnection connCfg $ \conn ->
    action
      LatticeClient
        { lcConfig = cfg
        , lcConn = conn
        , lcStore = store
        , lcKnown = known
        }


-- ---------------------------------------------------------------------------
-- Results and errors
-- ---------------------------------------------------------------------------

data QueryResult = QueryResult
  { qrData :: Map Text A.Value
  -- ^ The denormalized per-root tree ('denormalize').
  , qrManifest :: Manifest
  -- ^ Root maps merged across fetched slices; the etag is the last
  -- fetched slice's.
  , qrErrors :: [ErrorRecord]
  , qrRecords :: [Record]
  -- ^ Every record received, in slice order (pub, ctx, priv).
  , qrDegraded :: Bool
  -- ^ Any scoped error record, or any @207 Multi-Status@ response.
  }
  deriving stock (Eq, Show)


data MutationResult = MutationResult
  { mrCommitted :: Bool
  -- ^ The §9.4.3 test: the response carries at least one @entity@,
  -- @tombstone@, or @invalidated@ record.
  , mrData :: Map Text A.Value
  -- ^ Per manifest root (e.g. @result@), an array of shallow entity
  -- objects (@$ref@, @$ver@, carried fields verbatim; edges stay as
  -- @{\"$ref\":…}@ \/ page values) built from this response's records.
  , mrInvalidated :: [SurrogateKey]
  , mrErrors :: [ErrorRecord]
  , mrReplayed :: Bool
  -- ^ @Idempotency-Replayed: true@ was present.
  }
  deriving stock (Eq, Show)


{- | Whole-request failures. 'HttpProblem' carries the status code, the
problem @type@ (falling back to @title@) of an RFC 9457 body, and the
decoded body when it was JSON.
-}
data LatticeError
  = HttpProblem Int Text (Maybe A.Value)
  | TransportError Text
  | DecodeError Text
  | -- | Local (client-side) compile or plan failure in schema mode.
    ClientCompileError CompileError
  deriving stock (Eq, Show)


-- ---------------------------------------------------------------------------
-- Query
-- ---------------------------------------------------------------------------

{- | Run a query. Variables bind into URL parameters via
'Lattice.Value.valueToUrlParam'; a variable with a declared default may
be omitted. All nonempty data slices are fetched and merged store-side
in slice order (pub, ctx, priv); the first failing slice fails the
whole query.
-}
query :: LatticeClient -> Text -> Map VarName A.Value -> IO (Either LatticeError QueryResult)
query lc text vars = case ccSchema (lcConfig lc) of
  Just schema -> case compileText schema defaultBudgets text of
    Left ce -> pure (Left (ClientCompileError ce))
    Right compiled -> case planQuery schema defaultBudgets compiled of
      Left ce -> pure (Left (ClientCompileError ce))
      Right plan -> do
        let slices = nonEmptySlices (Map.keys (planSlices plan))
            key = compiledText compiled
        r <- runSlices lc key key (compiledHash compiled) (Just (planId plan)) slices slices vars
        either (pure . Left) (finishQuery lc (compiledDoc compiled) vars) r
  Nothing -> case parseDocument text of
    Left pe ->
      pure
        ( Left
            ( DecodeError
                ("query parse error at offset " <> tshow (peOffset pe) <> ": " <> peMessage pe)
            )
        )
    Right doc -> do
      known <- readTVarIO (lcKnown lc)
      r <- case Map.lookup text known of
        Just kq ->
          runSlices lc text text (kqHash kq) (kqPlanId kq) (kqSlices kq) (kqSlices kq) vars
        Nothing -> introduceFirst lc text vars
      either (pure . Left) (finishQuery lc doc vars) r
  where
    nonEmptySlices ss = case filter (/= SlicePlan) ss of
      [] -> [SlicePub]
      ds -> ds


-- | Fetch the given slices hash-form, falling back to introduction on
-- @lattice:unknown-query@. Aborts at the first failure.
runSlices ::
  LatticeClient ->
  Text ->
  Text ->
  Text ->
  Maybe Text ->
  [SliceName] ->
  [SliceName] ->
  Map VarName A.Value ->
  IO (Either LatticeError [SliceStream])
runSlices lc memoKey bodyText hash mPlanId memoSlices toFetch vars = go toFetch []
  where
    go [] acc = pure (Right (reverse acc))
    go (s : rest) acc = do
      r <- fetchSlice lc memoKey bodyText hash mPlanId memoSlices s vars
      case r of
        Left e -> pure (Left e)
        Right ss -> go rest (ss : acc)


fetchSlice ::
  LatticeClient ->
  Text ->
  Text ->
  Text ->
  Maybe Text ->
  [SliceName] ->
  SliceName ->
  Map VarName A.Value ->
  IO (Either LatticeError SliceStream)
fetchSlice lc memoKey bodyText hash mPlanId memoSlices slice vars = do
  let (credParams, credHeaders) = credsFor (lcConfig lc) slice
      planParam = maybe [] (\p -> [("p", encodeUtf8 p)]) mPlanId
      params =
        planParam
          <> [("slice", encodeUtf8 (renderSlice slice))]
          <> credParams
          <> varParams vars
      target = targetFor ("/q/" <> encodePathSegment (encodeUtf8 hash)) params
  r <- sendRaw lc (mkReq lc GET target credHeaders BodyEmpty)
  case r of
    Left e -> pure (Left e)
    Right rr
      | is2xx rr -> pure (classifyStream slice rr)
      | otherwise -> do
          absorbFailureRecords lc rr
          let problem = problemOf rr
          if isUnknownQuery problem
            then introduce lc memoKey bodyText memoSlices slice vars
            else pure (Left problem)


-- | @POST \/q?intent=introduce@ carrying the query text. On success the
-- returned @Location@ \/ @Lattice-Plan@ are remembered under the memo key.
introduce ::
  LatticeClient ->
  Text ->
  Text ->
  [SliceName] ->
  SliceName ->
  Map VarName A.Value ->
  IO (Either LatticeError SliceStream)
introduce lc memoKey bodyText memoSlices slice vars = do
  let (credParams, credHeaders) = credsFor (lcConfig lc) slice
      params =
        [("intent", "introduce"), ("slice", encodeUtf8 (renderSlice slice))]
          <> credParams
          <> varParams vars
      headers = ("Content-Type", queryMediaType) : credHeaders
      body = BodyBytes (encodeUtf8 bodyText)
  r <- sendRaw lc (mkReq lc POST (targetFor "/q" params) headers body)
  case r of
    Left e -> pure (Left e)
    Right rr
      | is2xx rr -> case classifyStream slice rr of
          Left e -> pure (Left e)
          Right ss -> do
            remember lc memoKey memoSlices (ssHeaders ss)
            pure (Right ss)
      | otherwise -> do
          absorbFailureRecords lc rr
          pure (Left (problemOf rr))


-- | Schema-less first contact: introduce under the credential-implied
-- slice, learn the real slice set from the plan record, fetch the rest.
introduceFirst ::
  LatticeClient ->
  Text ->
  Map VarName A.Value ->
  IO (Either LatticeError [SliceStream])
introduceFirst lc text vars = do
  let s0 = impliedSlice (lcConfig lc)
  r0 <- introduce lc text text [s0] s0 vars
  case r0 of
    Left e -> pure (Left e)
    Right ss0 -> case locationHash (ssHeaders ss0) of
      Nothing -> pure (Right [ss0])
      Just h -> do
        let pid = planHeader (ssHeaders ss0)
        mSlices <- fetchPlanSlices lc h
        let slices = fromMaybe [s0] mSlices
        atomically (modifyTVar' (lcKnown lc) (Map.insert text (KnownQuery h pid slices)))
        r <- runSlices lc text text h pid slices (filter (/= s0) slices) vars
        pure (fmap (ss0 :) r)


-- | @GET \/q\/{hash}@ with no @slice@ returns the plan record; its slice
-- map names the nonempty data slices.
fetchPlanSlices :: LatticeClient -> Text -> IO (Maybe [SliceName])
fetchPlanSlices lc hash = do
  let target = "/q/" <> encodePathSegment (encodeUtf8 hash)
  r <- sendRaw lc (mkReq lc GET target [] BodyEmpty)
  pure $ case r of
    Right rr
      | is2xx rr
      , Right recs <- parseNdjson (rrBody rr)
      , (pr : _) <- planRecordsOf recs ->
          Just (Map.keys (prSlices pr))
    _ -> Nothing
  where
    planRecordsOf = foldr (\rec acc -> case rec of RPlan p -> p : acc; _ -> acc) []


-- | Order the fetched streams (pub, ctx, priv), apply every record to the
-- store in that order, then denormalize against the resulting snapshot.
finishQuery ::
  LatticeClient ->
  Document ->
  Map VarName A.Value ->
  [SliceStream] ->
  IO (Either LatticeError QueryResult)
finishQuery lc doc vars streams0 = do
  let streams = sortOn ssSlice streams0
      allRecords = concatMap ssRecords streams
      manifests = mapMaybe ssManifest streams
  case reverse manifests of
    [] -> pure (Left (DecodeError "response carried no manifest record"))
    (lastManifest : _) -> do
      snapshot <- atomically $ do
        applyRecords (lcStore lc) allRecords
        snapshotEntities (lcStore lc)
      let merged = lastManifest {mRoot = Map.unions (map mRoot manifests)}
          errs = concatMap ssErrors streams
      pure
        ( Right
            QueryResult
              { qrData = denormalize (ccSchema (lcConfig lc)) doc vars merged snapshot
              , qrManifest = merged
              , qrErrors = errs
              , qrRecords = allRecords
              , qrDegraded = not (null errs) || any (\s -> ssStatus s == 207) streams
              }
        )


-- ---------------------------------------------------------------------------
-- Mutations
-- ---------------------------------------------------------------------------

{- | @POST \/m\/{name}@. The input value is the JSON body (an object for a
singular call, an array for a batch). Response records apply to the
store (read-your-writes; @invalidated@ keys accumulate via
'Lattice.Client.Store.markStale').
-}
mutate ::
  LatticeClient ->
  MutationName ->
  A.Value ->
  -- | @Idempotency-Key@ value.
  Maybe Text ->
  IO (Either LatticeError MutationResult)
mutate lc (MutationName name) input mIdem = do
  let (credParams, credHeaders) = allCreds (lcConfig lc)
      idemHeader = maybe [] (\k -> [(hIdempotencyKey, encodeUtf8 k)]) mIdem
      headers = ("Content-Type", "application/json") : idemHeader <> credHeaders
      target = targetFor ("/m/" <> encodePathSegment (encodeUtf8 name)) credParams
      body = BodyBytes (BL.toStrict (A.encode input))
  r <- sendRaw lc (mkReq lc POST target headers body)
  case r of
    Left e -> pure (Left e)
    Right rr
      | is2xx rr -> case parseNdjson (rrBody rr) of
          Left e -> pure (Left e)
          Right recs -> do
            atomically (applyRecords (lcStore lc) recs)
            let snap = responseSnapshot recs
                roots = maybe Map.empty mRoot (firstManifest recs)
            pure
              ( Right
                  MutationResult
                    { mrCommitted = any isCommitRecord recs
                    , mrData = Map.map (shallowArray snap) roots
                    , mrInvalidated = concatMap invalidatedKeys recs
                    , mrErrors = errorRecordsOf recs
                    , mrReplayed = lookupHeader hIdempotencyReplayed (rrHeaders rr) == Just "true"
                    }
              )
      | otherwise -> do
          absorbFailureRecords lc rr
          pure (Left (problemOf rr))
  where
    isCommitRecord = \case
      REntity {} -> True
      RTombstone {} -> True
      RInvalidated {} -> True
      _ -> False
    invalidatedKeys = \case
      RInvalidated keys _ -> keys
      _ -> []


-- ---------------------------------------------------------------------------
-- Point fetch
-- ---------------------------------------------------------------------------

{- | @GET \/e\/{Type}\/{key}?f=…@ (resource mode, §6.7). An empty mask
selects every field at or below the caller's level. The result's
'qrData' maps each manifest root (@node@) to an array of shallow entity
objects built from this response's records (no query selection exists
to walk). A @410@ tombstone response applies the tombstone to the store
before surfacing as 'HttpProblem'.
-}
pointFetch :: LatticeClient -> Ref -> [Text] -> IO (Either LatticeError QueryResult)
pointFetch lc ref mask = do
  let (credParams, credHeaders) = allCreds (lcConfig lc)
      maskParam =
        if null mask
          then []
          else [("f", encodeUtf8 (T.intercalate "," mask))]
      path =
        "/e/"
          <> encodePathSegment (encodeUtf8 (unTypeName (refType ref)))
          <> "/"
          <> encodePathSegment (encodeUtf8 (refKey ref))
      target = targetFor path (maskParam <> credParams)
  r <- sendRaw lc (mkReq lc GET target credHeaders BodyEmpty)
  case r of
    Left e -> pure (Left e)
    Right rr
      | is2xx rr -> case parseNdjson (rrBody rr) of
          Left e -> pure (Left e)
          Right recs -> do
            atomically (applyRecords (lcStore lc) recs)
            let snap = responseSnapshot recs
                errs = errorRecordsOf recs
            case firstManifest recs of
              Nothing -> pure (Left (DecodeError "point fetch carried no manifest record"))
              Just m ->
                pure
                  ( Right
                      QueryResult
                        { qrData = Map.map (shallowArray snap) (mRoot m)
                        , qrManifest = m
                        , qrErrors = errs
                        , qrRecords = recs
                        , qrDegraded = not (null errs) || rrStatus rr == 207
                        }
                  )
      | otherwise -> do
          absorbFailureRecords lc rr
          pure (Left (problemOf rr))


-- ---------------------------------------------------------------------------
-- Slice streams
-- ---------------------------------------------------------------------------

data SliceStream = SliceStream
  { ssSlice :: SliceName
  , ssStatus :: Int
  , ssHeaders :: Headers
  , ssRecords :: [Record]
  , ssManifest :: Maybe Manifest
  , ssErrors :: [ErrorRecord]
  }


classifyStream :: SliceName -> RawResult -> Either LatticeError SliceStream
classifyStream slice rr = do
  recs <- parseNdjson (rrBody rr)
  Right
    SliceStream
      { ssSlice = slice
      , ssStatus = rrStatus rr
      , ssHeaders = rrHeaders rr
      , ssRecords = recs
      , ssManifest = firstManifest recs
      , ssErrors = errorRecordsOf recs
      }


parseNdjson :: ByteString -> Either LatticeError [Record]
parseNdjson body = case partitionEithers (decodeRecords body) of
  ([], recs) -> Right recs
  (bad : _, _) ->
    Left (DecodeError ("undecodable NDJSON line: " <> decodeUtf8Lenient bad))


firstManifest :: [Record] -> Maybe Manifest
firstManifest = foldr step Nothing
  where
    step (RManifest m) _ = Just m
    step _ acc = acc


errorRecordsOf :: [Record] -> [ErrorRecord]
errorRecordsOf = foldr step []
  where
    step (RError e) acc = e : acc
    step _ acc = acc


-- | A tombstone (or other record stream) carried by a failure response —
-- e.g. a point fetch @410@ — is still a set of facts; apply it.
absorbFailureRecords :: LatticeClient -> RawResult -> IO ()
absorbFailureRecords lc rr = case parseNdjson (rrBody rr) of
  Right recs | not (null recs) -> atomically (applyRecords (lcStore lc) recs)
  _ -> pure ()


-- | Merge this response's own entity records (same-ver union), for
-- shallow rendering independent of the shared store.
responseSnapshot :: [Record] -> Map Ref StoredEntity
responseSnapshot = foldl step Map.empty
  where
    step m = \case
      REntity er -> Map.insert (erId er) (mergeEntityRecord er (Map.lookup (erId er) m)) m
      _ -> m


shallowArray :: Map Ref StoredEntity -> [Ref] -> A.Value
shallowArray snap refs = A.Array (V.fromList (map (shallowEntity snap) refs))


shallowEntity :: Map Ref StoredEntity -> Ref -> A.Value
shallowEntity snap ref = case Map.lookup ref snap of
  Nothing -> A.toJSON (Map.singleton ("$ref" :: Text) (A.String (renderRef ref)))
  Just (ver, fields) ->
    A.toJSON
      ( Map.insert "$ref" (A.String (renderRef ref)) (Map.insert "$ver" (A.String ver) fields)
      )


-- ---------------------------------------------------------------------------
-- Credentials, parameters, memoization
-- ---------------------------------------------------------------------------

-- | Slice-appropriate credentials: ctx carries the @vc@ payload parameter
-- and the @X-Vc-Auth@ proof header; priv carries @Authorization@.
credsFor :: ClientConfig -> SliceName -> ([(ByteString, ByteString)], Headers)
credsFor cfg = \case
  SliceCtx -> vcCreds cfg
  SlicePriv -> case ccAuthorization cfg of
    Just auth -> ([], [("Authorization", auth)])
    Nothing -> ([], [])
  _ -> ([], [])


-- | Everything we have (mutations and point fetches, where the required
-- level is the origin's call, not the client's).
allCreds :: ClientConfig -> ([(ByteString, ByteString)], Headers)
allCreds cfg =
  let (vcParams, vcHeaders) = vcCreds cfg
      authHeaders = maybe [] (\a -> [("Authorization", a)]) (ccAuthorization cfg)
  in (vcParams, vcHeaders <> authHeaders)


vcCreds :: ClientConfig -> ([(ByteString, ByteString)], Headers)
vcCreds cfg = case ccClaims cfg of
  Just (claims, proof) ->
    ([("vc", encodeUtf8 (encodeClaims claims))], [(hVcAuth, encodeUtf8 proof)])
  Nothing -> ([], [])


-- | The slice a schema-less first introduction runs under: the highest
-- one the configured credentials can satisfy.
impliedSlice :: ClientConfig -> SliceName
impliedSlice cfg
  | Just _ <- ccAuthorization cfg = SlicePriv
  | Just _ <- ccClaims cfg = SliceCtx
  | otherwise = SlicePub


varParams :: Map VarName A.Value -> [(ByteString, ByteString)]
varParams = map one . Map.toList
  where
    one (VarName n, v) = (encodeUtf8 n, encodeUtf8 (valueToUrlParam v))


remember :: LatticeClient -> Text -> [SliceName] -> Headers -> IO ()
remember lc memoKey slices hdrs = case locationHash hdrs of
  Nothing -> pure ()
  Just h ->
    atomically
      ( modifyTVar'
          (lcKnown lc)
          (Map.insert memoKey (KnownQuery h (planHeader hdrs) slices))
      )


locationHash :: Headers -> Maybe Text
locationHash hdrs = do
  loc <- case lookupHeader "Location" hdrs of
    Just l -> Just l
    Nothing -> lookupHeader "Content-Location" hdrs
  rest <- BS.stripPrefix "/q/" loc
  let h = BS8.takeWhile (\c -> c /= '?' && c /= '/') rest
  if BS.null h then Nothing else Just (decodeUtf8Lenient h)


planHeader :: Headers -> Maybe Text
planHeader hdrs = decodeUtf8Lenient <$> lookupHeader hLatticePlan hdrs


-- ---------------------------------------------------------------------------
-- HTTP plumbing
-- ---------------------------------------------------------------------------

data RawResult = RawResult
  { rrStatus :: Int
  , rrHeaders :: Headers
  , rrBody :: ByteString
  }


sendRaw :: LatticeClient -> Request -> IO (Either LatticeError RawResult)
sendRaw lc req =
  attempt
    `catches` [ Handler (\(e :: ConnectionError) -> pure (Left (TransportError (tshow e))))
              , Handler (\(e :: IOException) -> pure (Left (TransportError (tshow e))))
              ]
  where
    attempt = do
      resp <- sendOn (lcConn lc) req
      body <- drainBody (responseBody resp)
      pure
        ( Right
            RawResult
              { rrStatus = fromIntegral (statusCode (responseStatus resp))
              , rrHeaders = responseHeaders resp
              , rrBody = body
              }
        )


mkReq :: LatticeClient -> Method -> ByteString -> Headers -> Body -> Request
mkReq lc method target headers body =
  Request
    { requestMethod = method
    , requestTarget = target
    , requestAuthority = Just authority
    , requestScheme = SchemeHttp
    , requestHeaders = headers
    , requestBody = body
    , requestVersion = V.HTTP1_1
    , requestTrailers = pure []
    }
  where
    authority = BS8.pack (ccHost (lcConfig lc) <> ":" <> ccPort (lcConfig lc))


targetFor :: ByteString -> [(ByteString, ByteString)] -> ByteString
targetFor path params
  | null params = path
  | otherwise = path <> "?" <> renderQueryString params


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


is2xx :: RawResult -> Bool
is2xx rr = rrStatus rr >= 200 && rrStatus rr < 300


problemOf :: RawResult -> LatticeError
problemOf rr = case A.decodeStrict (rrBody rr) of
  Just v@(A.Object o) ->
    let typ = case KM.lookup "type" o of
          Just (A.String t) -> t
          _ -> case KM.lookup "title" o of
            Just (A.String t) -> t
            _ -> ""
    in HttpProblem (rrStatus rr) typ (Just v)
  Just v -> HttpProblem (rrStatus rr) "" (Just v)
  Nothing -> HttpProblem (rrStatus rr) "" Nothing


isUnknownQuery :: LatticeError -> Bool
isUnknownQuery = \case
  HttpProblem 404 t _ -> "unknown-query" `T.isSuffixOf` t
  _ -> False


tshow :: (Show a) => a -> Text
tshow = T.pack . show
