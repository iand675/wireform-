{- | The Lattice demo origin: the Star Wars dataset served over HTTP.

Configuration is environment-driven:

* @LATTICE_PORT@ — listen port (default 8917).
* @LATTICE_VERIFY=1@ — require HMAC proofs on @ctx@ slices (dev secret
  @lattice-demo@); by default the origin runs in dev mode ('ocVerifier'
  'Nothing': the vc payload is trusted without proof).
* @LATTICE_DEBUG_DELAY_MS@ — sleep that many milliseconds before handling
  any @/q@ request, widening the request-collapse window so a CDN harness
  can assert that N concurrent cold GETs cause exactly one origin fill.
* @LATTICE_PURGE_URL@ + @LATTICE_PURGE_STYLE@ (+ @LATTICE_PURGE_SECRET@) —
  CDN purge forwarding, see below. When no URL is set, purges log to
  stdout.

Two debug endpoints support CDN-harness assertions (neither counts
itself): @GET \/debug\/requests@ returns @{"count":N}@, the number of
requests that reached the origin, and @POST \/debug\/reset@ zeroes it.

Purge styles: @varnish@ issues a @PURGE@ request to the URL with an
@xkey-softpurge@ header naming the keys; @worker@ (the default) POSTs
@{"keys":[…]}@ with an @X-Purge-Secret@ header. Forwarding is
fire-and-forget on a fresh thread: a purge failure logs and never crashes
the origin.
-}
{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.Aeson qualified as A
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Lattice.Backend.Memory (memoryBackend)
import Lattice.Schema (defaultBudgets)
import Lattice.Server (OriginConfig (..), latticeHandler, newOrigin)
import Lattice.Server.Auth (hmacVerifier)
import Lattice.Wire (SurrogateKey)
import Network.HTTP.Client.URI (URI, parseURI, uriHost, uriPathAndQuery, uriPort)
import Network.HTTP.Connection (ConnectionConfig (..), defaultConnectionConfig, sendOn, withConnection)
import Network.HTTP.Message (Request (..), Response (..), Scheme (..))
import Network.HTTP.Server (ServerConfig (..), defaultServerConfig, runServer)
import Network.HTTP.Types.Body (Body (..))
import Network.HTTP.Types.Method (Method (..), methodFromBytes)
import Network.HTTP.Types.Status (Status, pattern Status)
import Network.HTTP.Types.Version (pattern HTTP1_1)
import Network.HTTP.VersionRange (preferHttp1)
import StarWars (loadStarWarsSchema, newStarWarsDb, starWarsHooks)
import System.Environment (lookupEnv)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Text.Read (readMaybe)


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  port <- fromMaybe "8917" <$> lookupEnv "LATTICE_PORT"
  delayMs <- (>>= readMaybe) <$> lookupEnv "LATTICE_DEBUG_DELAY_MS"
  verify <- lookupEnv "LATTICE_VERIFY"
  schema <- loadStarWarsSchema
  db <- newStarWarsDb schema
  purge <- mkPurger
  counter <- newTVarIO (0 :: Int)
  let config =
        OriginConfig
          { ocSchema = schema
          , ocBudgets = defaultBudgets
          , ocBackend = memoryBackend schema db (starWarsHooks schema)
          , ocVerifier =
              if verify == Just "1"
                then Just (hmacVerifier "lattice-demo" getPOSIXTime)
                else Nothing
          , ocSnapshotDomain = "main"
          , ocPurge = purge
          , ocCors = True
          , ocNow = getPOSIXTime
          }
  origin <- newOrigin config
  let handler = debugMiddleware counter delayMs (latticeHandler origin)
      base = "http://127.0.0.1:" <> port
  putStrLn ("example-lattice: Star Wars origin listening on " <> base)
  putStrLn ("  curl " <> base <> "/.well-known/lattice")
  putStrLn ("  curl -X QUERY -H 'Content-Type: application/x-lattice-query' --data 'query Hero { hero { name } }' " <> base <> "/q")
  putStrLn ("  curl -X POST -H 'Content-Type: application/json' --data '{\"episode\":\"Jedi\",\"stars\":5}' " <> base <> "/m/createReview")
  runServer
    defaultServerConfig
      { serverHost = "0.0.0.0"
      , serverPort = port
      , serverVersionRange = preferHttp1
      , serverHandler = handler
      }


-- ---------------------------------------------------------------------------
-- Debug middleware
-- ---------------------------------------------------------------------------

{- | Serve the two debug endpoints and count every request that reaches the
origin proper (debug traffic itself is not counted).
-}
debugMiddleware :: TVar Int -> Maybe Int -> (Request -> IO Response) -> Request -> IO Response
debugMiddleware counter delayMs inner req =
  case (requestMethod req, path) of
    (GET, "/debug/requests") -> do
      n <- readTVarIO counter
      pure (jsonResponse (A.object ["count" A..= n]))
    (POST, "/debug/reset") -> do
      atomically (writeTVar counter 0)
      pure (jsonResponse (A.object ["count" A..= (0 :: Int)]))
    _ -> do
      atomically (modifyTVar' counter (+ 1))
      case delayMs of
        Just ms | isQueryPath -> threadDelay (ms * 1000)
        _ -> pure ()
      inner req
  where
    path = BS8.takeWhile (/= '?') (requestTarget req)
    isQueryPath = path == "/q" || "/q/" `BS8.isPrefixOf` path


jsonResponse :: A.Value -> Response
jsonResponse v =
  Response
    { responseStatus = Status 200
    , responseVersion = HTTP1_1
    , responseHeaders =
        [ ("Content-Type", "application/json")
        , ("Cache-Control", "no-store")
        ]
    , responseBody = BodyBytes (BL.toStrict (A.encode v))
    , responseTrailers = pure []
    , responseH2StreamId = 0
    , responseCancel = pure ()
    , responsePushPromises = pure []
    }


-- ---------------------------------------------------------------------------
-- Purge forwarding
-- ---------------------------------------------------------------------------

mkPurger :: IO ([SurrogateKey] -> IO ())
mkPurger = do
  mUrl <- lookupEnv "LATTICE_PURGE_URL"
  style <- fromMaybe "worker" <$> lookupEnv "LATTICE_PURGE_STYLE"
  secret <- lookupEnv "LATTICE_PURGE_SECRET"
  case mUrl of
    Nothing -> pure $ \keys ->
      putStrLn ("[purge] " <> T.unpack (T.unwords keys))
    Just url -> case parseURI (BS8.pack url) of
      Left err -> do
        putStrLn ("[purge] ignoring malformed LATTICE_PURGE_URL: " <> err)
        pure $ \keys -> putStrLn ("[purge] (not forwarded) " <> T.unpack (T.unwords keys))
      Right uri -> pure $ \keys -> void . forkIO $ do
        outcome <- try (forwardPurge uri style secret keys)
        case outcome of
          Left (e :: SomeException) -> putStrLn ("[purge] forward failed: " <> show e)
          Right st -> putStrLn ("[purge] forwarded " <> show (length keys) <> " key(s): " <> show st)


forwardPurge :: URI -> String -> Maybe String -> [SurrogateKey] -> IO Status
forwardPurge uri style secret keys =
  withConnection cfg $ \conn -> do
    resp <- sendOn conn req
    drainBody (responseBody resp)
    pure (responseStatus resp)
  where
    cfg =
      defaultConnectionConfig
        { connectionHost = BS8.unpack (uriHost uri)
        , connectionPort = show (uriPort uri)
        }
    req = case style of
      "varnish" ->
        baseReq
          { requestMethod = methodFromBytes "PURGE"
          , requestHeaders = [("xkey-softpurge", TE.encodeUtf8 (T.unwords keys))]
          }
      _ ->
        baseReq
          { requestMethod = POST
          , requestHeaders =
              ("Content-Type", "application/json")
                : maybe [] (\s -> [("X-Purge-Secret", BS8.pack s)]) secret
          , requestBody = BodyBytes (BL.toStrict (A.encode (A.object ["keys" A..= keys])))
          }
    baseReq =
      Request
        { requestMethod = POST
        , requestTarget = uriPathAndQuery uri
        , requestAuthority = Just (uriHost uri <> ":" <> BS8.pack (show (uriPort uri)))
        , requestScheme = SchemeHttp
        , requestHeaders = []
        , requestBody = BodyEmpty
        , requestVersion = HTTP1_1
        , requestTrailers = pure []
        }


drainBody :: Body -> IO ()
drainBody = \case
  BodyStream next ->
    let go = next >>= maybe (pure ()) (const go)
    in go
  _ -> pure ()
