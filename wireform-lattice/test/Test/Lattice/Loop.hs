{- | The shared loopback harness: a real origin ("Lattice.Server" over
"Lattice.Backend.Memory") on an ephemeral port, a counting middleware,
"Lattice.Client" client runners, a raw-HTTP escape hatch for requests the
client cannot spell (verb-bound mutations, digest headers, signed
introductions), and the JSON tree assertions shared by every E2E-style
module.

'LoopSpec' is the extension seam: 'lsTweak' edits the 'OriginConfig'
(admission, coalescing) and 'lsWrap' interposes on the 'Backend' (loader
counting) without each test module re-plumbing the server lifecycle.
-}
module Test.Lattice.Loop (
  -- * Loopback origin
  Hit (..),
  Loop (..),
  LoopSpec (..),
  loopSpec,
  withLoop,
  withLoopback,
  seedRow,
  resetHits,
  allHits,
  queryHits,
  targetHits,
  surrogateKeysOfMutation,

  -- * Clients and operations
  clientFor,
  io,
  runQuery,
  runMutate,
  expectProblem,
  expectProblemNaming,
  expectStatus,
  problemTexts,

  -- * Raw HTTP
  RawResp (..),
  httpRaw,
  rawRecords,
  rawHeader,
  rawProblemType,
  rawProblem,

  -- * JSON tree assertions
  rootValue,
  mutationResult,
  objectField,
  fieldByPrefix,
  hasFieldPrefix,
  asArray,
  asText,
  textField,
  pageNames,
  pageRefTexts,
) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM
import Control.Exception (bracket, finally)
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.CaseInsensitive (CI)
import Data.Either (rights)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Lattice.Backend (Backend (..), upstreamUnavailable)
import Lattice.Backend.Memory
import Lattice.Client
import Lattice.Schema (Schema, defaultBudgets)
import Lattice.Server (Origin, OriginConfig (..), defaultLiveConfig, latticeHandler, newOrigin)
import Lattice.Server.Auth (ProofVerifier, QueryAdmission (..))
import Lattice.Telemetry (noTelemetry)
import Lattice.Types
import Lattice.Wire (Record, decodeRecords, hSurrogateKey)
import Network.HTTP.Connection (
  ConnectionConfig (..),
  defaultConnectionConfig,
  sendOn,
  withConnection,
 )
import Network.HTTP.Message (Request (..), Response (..), Scheme (..))
import Network.HTTP.Server (ServerConfig (..), defaultServerConfig, runServerOnListener)
import Network.HTTP.Types.Body (Body (..))
import Network.HTTP.Types.Header (Headers, hCacheControl, lookupHeader)
import Network.HTTP.Types.Method (Method (..))
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.Version qualified as V
import Network.HTTP.VersionRange (preferHttp1)
import Network.Socket qualified as NS
import System.Timeout (timeout)
import Test.Lattice.Fixtures (requireRight)
import Test.Syd


-- ---------------------------------------------------------------------------
-- Loopback fixture: origin on an ephemeral port + counting middleware
-- ---------------------------------------------------------------------------

-- | One request as observed at the origin.
data Hit = Hit
  { hitMethod :: Method
  , hitTarget :: ByteString
  , hitStatus :: Int
  , hitCache :: Maybe ByteString
  , hitSurrogates :: Maybe ByteString
  , hitReqHeaders :: Headers
  -- ^ The request's headers, for asserting what a client sent (e.g. the
  -- §10.4 store advertisement form).
  }
  deriving stock (Show)


-- | A running loopback origin.
data Loop = Loop
  { loopPort :: String
  , loopHits :: TVar [Hit]
  , loopBreakEdges :: TVar Bool
  -- ^ When set, every 'beChildren' call fails with 'upstreamUnavailable'.
  , loopOrigin :: Origin
  -- ^ The origin itself, for in-process hooks (invalidation bus,
  -- coalescer flushes, maintained-derivation stepping).
  , loopDb :: MemoryDb
  -- ^ The backing tables, for out-of-band writes and row inspection.
  }


{- | What to run: schema + seed rows + hooks, with two extension knobs.
'lsWrap' wraps the fully-built 'Backend' (after the break-edges fault
injection, so counters see exactly what the origin sees); 'lsTweak' edits
the 'OriginConfig' after all common fields are set.
-}
data LoopSpec = LoopSpec
  { lsSchema :: Schema
  , lsHooks :: MemoryHooks
  , lsRows :: [(TypeName, Map FieldName A.Value)]
  , lsVerifier :: Maybe ProofVerifier
  , lsWrap :: Backend -> Backend
  , lsTweak :: OriginConfig -> OriginConfig
  }


loopSpec :: Schema -> LoopSpec
loopSpec schema =
  LoopSpec
    { lsSchema = schema
    , lsHooks = defaultHooks
    , lsRows = []
    , lsVerifier = Nothing
    , lsWrap = id
    , lsTweak = id
    }


-- | The historical four-argument form; 'withLoop' is the general one.
withLoopback ::
  Schema ->
  MemoryHooks ->
  [(TypeName, Map FieldName A.Value)] ->
  Maybe ProofVerifier ->
  (Loop -> IO a) ->
  IO a
withLoopback schema hooks rows verifier =
  withLoop (loopSpec schema) {lsHooks = hooks, lsRows = rows, lsVerifier = verifier}


withLoop :: LoopSpec -> (Loop -> IO a) -> IO a
withLoop spec action = do
  let schema = lsSchema spec
  db <- newMemoryDb
  atomically (mapM_ (seedRow schema db) (lsRows spec))
  breakEdges <- newTVarIO False
  hits <- newTVarIO []
  let inner = memoryBackend schema db (lsHooks spec)
      backend =
        lsWrap
          spec
          inner
            { beChildren = \ty field parents win -> do
                broken <- readTVarIO breakEdges
                if broken
                  then pure (Map.fromList (map (\(r, _) -> (r, Left upstreamUnavailable)) parents))
                  else beChildren inner ty field parents win
            }
  origin <-
    newOrigin
      ( lsTweak
          spec
          OriginConfig
            { ocSchema = schema
            , ocBudgets = defaultBudgets
            , ocBackend = backend
            , ocVerifier = lsVerifier spec
            , ocSnapshotDomain = "e2e"
            , ocPurge = const (pure ())
            , ocCors = False
            , ocNow = getPOSIXTime
            , ocAdmission = AdmitOpen
            , ocCoalesce = Nothing
            , ocRegistry = Nothing
            , ocLive = defaultLiveConfig
            , ocTelemetry = noTelemetry
            }
      )
  let handler req = do
        resp <- latticeHandler origin req
        atomically (modifyTVar' hits (mkHit req resp :))
        pure resp
  withServerSocket $ \sock port -> do
    let scfg =
          defaultServerConfig
            { serverHost = "127.0.0.1"
            , serverPort = show port
            , serverVersionRange = preferHttp1
            , serverHandler = handler
            }
    ready <- newEmptyMVar
    tid <- forkIO (putMVar ready () >> runServerOnListener scfg sock)
    takeMVar ready
    action
      Loop
        { loopPort = show port
        , loopHits = hits
        , loopBreakEdges = breakEdges
        , loopOrigin = origin
        , loopDb = db
        }
      `finally` killThread tid


-- | Bind port 0 on the loopback interface and hand back the listener plus
-- the port the kernel chose (the repo's standard test pattern).
withServerSocket :: (NS.Socket -> Int -> IO a) -> IO a
withServerSocket k = do
  let hints = NS.defaultHints {NS.addrFlags = [NS.AI_PASSIVE], NS.addrSocketType = NS.Stream}
  addrs <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
  case addrs of
    [] -> expectationFailure "no loopback address available for the test bind"
    (addr : _) ->
      bracket (NS.openSocket addr) NS.close $ \sock -> do
        NS.setSocketOption sock NS.ReuseAddr 1
        NS.bind sock (NS.addrAddress addr)
        NS.listen sock 128
        bound <- NS.getSocketName sock
        case bound of
          NS.SockAddrInet p _ -> k sock (fromIntegral p)
          _ -> expectationFailure "loopback listener bound to a non-inet address"


mkHit :: Request -> Response -> Hit
mkHit req resp =
  Hit
    { hitMethod = requestMethod req
    , hitTarget = requestTarget req
    , hitStatus = fromIntegral (statusCode (responseStatus resp))
    , hitCache = lookupHeader hCacheControl (responseHeaders resp)
    , hitSurrogates = lookupHeader hSurrogateKey (responseHeaders resp)
    , hitReqHeaders = requestHeaders req
    }


resetHits :: Loop -> IO ()
resetHits loop = atomically (writeTVar (loopHits loop) [])


-- | Chronological traffic since the last reset.
allHits :: Loop -> IO [Hit]
allHits loop = reverse <$> readTVarIO (loopHits loop)


-- | Chronological @/q@ traffic since the last reset.
queryHits :: Loop -> IO [Hit]
queryHits = targetHits "/q"


-- | Chronological traffic under a target prefix since the last reset.
targetHits :: ByteString -> Loop -> IO [Hit]
targetHits prefix loop =
  filter (\h -> prefix `BS8.isPrefixOf` hitTarget h) <$> allHits loop


seedRow :: Schema -> MemoryDb -> (TypeName, Map FieldName A.Value) -> STM ()
seedRow schema db (ty, fields) =
  case entityRowKey schema ty fields of
    Just key -> putRow db ty key fields
    Nothing -> error ("E2E seed row for " <> T.unpack (unTypeName ty) <> " lacks its key field")


{- | The @Surrogate-Key@ tokens of the single @\/m\/@ POST since the last
reset. Exact-token comparison on purpose: @User:u2@ is a substring of
@AdminUser:u2@, so infix assertions would be unsound here.
-}
surrogateKeysOfMutation :: Loop -> IO [Text]
surrogateKeysOfMutation loop = do
  hs <- targetHits "/m/" loop
  case hs of
    [h] -> pure (maybe [] (T.words . TE.decodeUtf8) (hitSurrogates h))
    other -> expectationFailure ("expected exactly one mutation hit, saw: " <> show other)


-- ---------------------------------------------------------------------------
-- Clients and operations
-- ---------------------------------------------------------------------------

clientFor :: Loop -> (ClientConfig -> ClientConfig) -> (LatticeClient -> IO a) -> IO a
clientFor loop f = withLatticeClient (f defaultClientConfig {ccPort = loopPort loop})


-- | Server-dependent awaits fail loudly instead of hanging the suite.
io :: String -> IO a -> IO a
io label act =
  timeout (15 * 1000000) act
    >>= maybe (expectationFailure ("E2E timed out waiting for " <> label)) pure


runQuery :: LatticeClient -> Text -> Map VarName A.Value -> IO QueryResult
runQuery lc txt vars = io "query" (query lc txt vars) >>= requireRight


runMutate :: LatticeClient -> MutationName -> A.Value -> IO MutationResult
runMutate lc name input = io "mutation" (mutate lc name input Nothing) >>= requireRight


expectProblem :: Int -> Text -> IO (Either LatticeError a) -> IO ()
expectProblem st suffix act =
  io "problem response" act >>= \case
    Left (HttpProblem got typ _) -> do
      got `shouldBe` st
      typ `shouldSatisfy` T.isSuffixOf suffix
    Left other -> expectationFailure ("expected an HTTP problem, got: " <> show other)
    Right _ -> expectationFailure "expected an HTTP problem, got a success"


{- | The action must fail with an HTTP problem of the given status whose
diagnostics (or detail) name the offender.
-}
expectProblemNaming :: Int -> Text -> IO (Either LatticeError a) -> IO ()
expectProblemNaming st needle act =
  io "problem response" act >>= \case
    Left (HttpProblem got _ body) -> do
      got `shouldBe` st
      maybe [] problemTexts body `shouldSatisfy` any (T.isInfixOf needle)
    Left other -> expectationFailure ("expected an HTTP problem, got: " <> show other)
    Right _ -> expectationFailure "expected an HTTP problem, got a success"


-- | Diagnostic strings of a decoded RFC 9457 body: @diagnostics@ + @detail@.
problemTexts :: A.Value -> [Text]
problemTexts = \case
  A.Object o -> diag (KM.lookup "diagnostics" o) <> det (KM.lookup "detail" o)
    where
      diag = \case
        Just (A.Array xs) -> mapMaybe asString (toList xs)
        _ -> []
      det = \case
        Just (A.String t) -> [t]
        _ -> []
      asString = \case
        A.String t -> Just t
        _ -> Nothing
  _ -> []


-- | The action must fail with an HTTP problem of the given status (the
-- §6.7 tombstone @410@ carries an NDJSON frame, not a problem body).
expectStatus :: Int -> IO (Either LatticeError a) -> IO ()
expectStatus st act =
  io "problem response" act >>= \case
    Left (HttpProblem got _ _) -> got `shouldBe` st
    Left other -> expectationFailure ("expected an HTTP problem, got: " <> show other)
    Right _ -> expectationFailure "expected an HTTP problem, got a success"


-- ---------------------------------------------------------------------------
-- Raw HTTP
-- ---------------------------------------------------------------------------

-- | A fully-drained raw response.
data RawResp = RawResp
  { rawStatus :: Int
  , rawHeaders :: Headers
  , rawBody :: ByteString
  }
  deriving stock (Show)


{- | One raw request on its own connection — the escape hatch for wire
shapes "Lattice.Client" cannot spell: verb-bound mutations with
preconditions, digest headers, signed introductions. A fresh connection
per call keeps concurrent raw requests genuinely concurrent at the
origin (the client's single HTTP\/1.1 connection would serialize them).
-}
httpRaw ::
  Loop ->
  Method ->
  ByteString ->
  [(CI ByteString, ByteString)] ->
  Maybe ByteString ->
  IO RawResp
httpRaw loop method target headers mBody = do
  let connCfg =
        defaultConnectionConfig
          { connectionHost = "127.0.0.1"
          , connectionPort = loopPort loop
          }
      req =
        Request
          { requestMethod = method
          , requestTarget = target
          , requestAuthority = Just (BS8.pack ("127.0.0.1:" <> loopPort loop))
          , requestScheme = SchemeHttp
          , requestHeaders = headers
          , requestBody = maybe BodyEmpty BodyBytes mBody
          , requestVersion = V.HTTP1_1
          , requestTrailers = pure []
          }
  io ("raw " <> show method <> " " <> BS8.unpack target) $
    withConnection connCfg $ \conn -> do
      resp <- sendOn conn req
      body <- drainBody (responseBody resp)
      pure
        RawResp
          { rawStatus = fromIntegral (statusCode (responseStatus resp))
          , rawHeaders = responseHeaders resp
          , rawBody = body
          }


drainBody :: Body -> IO ByteString
drainBody = \case
  BodyEmpty -> pure BS8.empty
  BodyBytes bs -> pure bs
  BodyStream pop -> go []
    where
      go acc =
        pop >>= \case
          Nothing -> pure (BS8.concat (reverse acc))
          Just c -> go (c : acc)


-- | The NDJSON records of a raw entity-stream body (undecodable lines
-- dropped, per the tolerant-fold contract).
rawRecords :: RawResp -> [Record]
rawRecords = rights . decodeRecords . rawBody


rawHeader :: CI ByteString -> RawResp -> Maybe ByteString
rawHeader name r = lookupHeader name (rawHeaders r)


-- | The RFC 9457 @type@ of a raw problem body, when it decodes.
rawProblemType :: RawResp -> Maybe Text
rawProblemType r = case A.decodeStrict (rawBody r) of
  Just (A.Object o) | Just (A.String t) <- KM.lookup "type" o -> Just t
  _ -> Nothing


{- | Assert a raw problem response: status plus RFC 9457 @type@ suffix
(problem types are absolute @https://lattice.dev/problems/…@ URIs on the
wire; tests name the @lattice:@ shorthand's tail).
-}
rawProblem :: Int -> Text -> RawResp -> IO ()
rawProblem st suffix r = do
  rawStatus r `shouldBe` st
  rawProblemType r `shouldSatisfy` maybe False (T.isSuffixOf suffix)


-- ---------------------------------------------------------------------------
-- JSON tree assertions
-- ---------------------------------------------------------------------------

rootValue :: Text -> QueryResult -> IO A.Value
rootValue name r =
  maybe
    (expectationFailure ("query data carries no root '" <> T.unpack name <> "': " <> show (qrData r)))
    pure
    (Map.lookup name (qrData r))


mutationResult :: MutationResult -> IO A.Value
mutationResult mr =
  maybe
    (expectationFailure ("mutation data carries no result root: " <> show (mrData mr)))
    pure
    (Map.lookup "result" (mrData mr))


objectField :: Text -> A.Value -> IO A.Value
objectField key v = case v of
  A.Object o ->
    maybe
      (expectationFailure ("no field '" <> T.unpack key <> "' in " <> show v))
      pure
      (KM.lookup (AK.fromText key) o)
  _ -> expectationFailure ("expected an object, got: " <> show v)


-- | The unique field whose canonical key starts with the prefix — for
-- parameterized occurrences like @friends(first:2)@ whose exact argument
-- rendering the test should not restate.
fieldByPrefix :: Text -> A.Value -> IO A.Value
fieldByPrefix prefix v = case v of
  A.Object o -> case filter (\(k, _) -> prefix `T.isPrefixOf` AK.toText k) (KM.toList o) of
    [(_, inner)] -> pure inner
    other ->
      expectationFailure
        ("expected exactly one '" <> T.unpack prefix <> "…' field, matching keys: " <> show (map fst other))
  _ -> expectationFailure ("expected an object, got: " <> show v)


hasFieldPrefix :: Text -> A.Value -> Bool
hasFieldPrefix prefix v = case v of
  A.Object o -> any (\k -> prefix `T.isPrefixOf` AK.toText k) (KM.keys o)
  _ -> False


asArray :: A.Value -> IO [A.Value]
asArray = \case
  A.Array xs -> pure (toList xs)
  v -> expectationFailure ("expected an array, got: " <> show v)


asText :: A.Value -> IO Text
asText = \case
  A.String t -> pure t
  v -> expectationFailure ("expected a string, got: " <> show v)


textField :: Text -> A.Value -> IO Text
textField key v = objectField key v >>= asText


pageNames :: A.Value -> IO [Text]
pageNames pv = objectField "items" pv >>= asArray >>= traverse (textField "name")


pageRefTexts :: A.Value -> IO [Text]
pageRefTexts pv = objectField "items" pv >>= asArray >>= traverse (textField "$ref")
