{- | A live-streaming Lattice demo: a handful of environmental sensors whose
readings drift every second, pushed to the browser over a §12 live query
(Server-Sent Events).

Run it:

@
cabal run example-sensors
# then open http://127.0.0.1:8918/ in a browser
@

Configuration:

* @LATTICE_PORT@ — listen port (default 8918).
* @LATTICE_TICK_MS@ — sensor update period in milliseconds (default 1000).

What happens under the hood:

* The origin serves the browser page at @GET \/@ and the Lattice protocol
  everywhere else.
* The page POSTs its query text once (the /introduction/), learns the
  content-addressed hash URL from the @Location@ header, then opens an
  @EventSource@ on @\/q\/{hash}?…&live=sse@.
* A background thread random-walks each sensor's reading, writes the new
  row, and 'publishPurge's the sensor's surrogate key. The live-query
  machinery ("Lattice.Server.Live") sees the purge intersect the
  subscription's registered keys, re-executes the query, diffs the result,
  and pushes only the changed entity records down the open SSE stream.
-}
{-# LANGUAGE PatternSynonyms #-}
module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Exception (IOException, try)
import Control.Monad (forM, void)
import Data.Aeson qualified as A
import Data.Bits (shiftR)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Word (Word64)
import Lattice.Backend.Memory (MemoryDb, defaultHooks, memoryBackend, newMemoryDb, putRow)
import Lattice.IDL.Parser (parseSchema)
import Lattice.Schema (Schema, defaultBudgets)
import Lattice.Server (OriginConfig (..), defaultLiveConfig, latticeHandler, newOrigin, publishPurge)
import Lattice.Server.Auth (QueryAdmission (..))
import Lattice.Telemetry (noTelemetry)
import Lattice.Types (FieldName, Ref (..), TypeName)
import Lattice.Wire (SurrogateKey, entityKeyOf)
import Network.HTTP.Message (Request (..), Response (..))
import Network.HTTP.Server (ServerConfig (..), defaultServerConfig, runServer)
import Network.HTTP.Types.Body (Body (..))
import Network.HTTP.Types.Method (Method (..))
import Network.HTTP.Types.Status (pattern Status)
import Network.HTTP.Types.Version (pattern HTTP1_1)
import Network.HTTP.VersionRange (preferHttp1)
import SensorsPage (embeddedPage)
import System.Environment (lookupEnv)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Text.Read (readMaybe)


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  port <- fromMaybe "8918" <$> lookupEnv "LATTICE_PORT"
  tickMs <- fromMaybe 1000 . (>>= readMaybe) <$> lookupEnv "LATTICE_TICK_MS"
  page <- loadPage
  schema <- loadSchema
  db <- newSensorDb
  origin <- newOrigin (mkConfig schema db)
  void (forkIO (runUpdater db (publishPurge origin) tickMs))
  let handler = staticMiddleware page (latticeHandler origin)
      site = "http://127.0.0.1:" <> port
  putStrLn ("example-sensors: live sensor origin listening on " <> site)
  putStrLn ("  open " <> site <> "/ in a browser to watch the stream")
  runServer
    defaultServerConfig
      { serverHost = "0.0.0.0"
      , serverPort = port
      , serverVersionRange = preferHttp1
      , serverHandler = handler
      }


mkConfig :: Schema -> MemoryDb -> OriginConfig
mkConfig schema db =
  OriginConfig
    { ocSchema = schema
    , ocBudgets = defaultBudgets
    , ocBackend = memoryBackend schema db defaultHooks
    , ocVerifier = Nothing
    , ocSnapshotDomain = "main"
    , ocPurge = \_ -> pure ()
    , ocCors = True
    , ocNow = getPOSIXTime
    , ocAdmission = AdmitOpen
    , ocCoalesce = Nothing
    , ocRegistry = Nothing
    , ocLive = defaultLiveConfig
    , ocTelemetry = noTelemetry
    }


-- ---------------------------------------------------------------------------
-- Schema
-- ---------------------------------------------------------------------------

loadSchema :: IO Schema
loadSchema = case parseSchema schemaSource of
  Right s -> pure s
  Left errs -> fail ("sensors schema failed to parse: " <> show errs)


schemaSource :: Text
schemaSource =
  T.unlines
    [ "\"Live sensor telemetry: a Lattice streaming (SSE, spec section 12) demo.\""
    , "schema sensors.example.com"
    , ""
    , "\"What a sensor measures.\""
    , "enum Metric closed = Temperature | Humidity | Pressure"
    , ""
    , "\"One physical sensor and its most recent reading.\""
    , "entity Sensor by id {"
    , "  visible to all by default"
    , ""
    , "  id:        Text"
    , "  site:      Text"
    , "  name:      Text"
    , "  metric:    Metric"
    , "  unit:      Text"
    , "  reading:   F64"
    , "  updatedAt: Timestamp"
    , ""
    , "  fetch by id: public"
    , "}"
    , ""
    , "\"Every sensor at a site, ordered by name.\""
    , "list sensors of Sensor by site"
    , "     ordered by name asc"
    , "     page 50 max 100"
    , "     public"
    ]


-- ---------------------------------------------------------------------------
-- Sensors
-- ---------------------------------------------------------------------------

data SensorDesc = SensorDesc
  { sdKey :: Text
  , sdName :: Text
  , sdMetric :: Text
  , sdUnit :: Text
  , sdInit :: Double
  }


-- | The site every seeded sensor belongs to (the @sensors(site:)@ grouping).
sensorSite :: Text
sensorSite = "hq"


sensorDescs :: [SensorDesc]
sensorDescs =
  [ SensorDesc "temp-1" "Server Room" "Temperature" "\x00B0\&C" 21.4
  , SensorDesc "temp-2" "Cold Aisle" "Temperature" "\x00B0\&C" 18.9
  , SensorDesc "temp-3" "Loading Dock" "Temperature" "\x00B0\&C" 12.6
  , SensorDesc "hum-1" "Server Room" "Humidity" "%" 44.0
  , SensorDesc "pres-1" "Rooftop" "Pressure" "hPa" 1013.2
  ]


-- | A fresh in-memory DB seeded with every sensor at its initial reading.
newSensorDb :: IO MemoryDb
newSensorDb = do
  db <- newMemoryDb
  ts <- nowIso
  atomically $ mapM_ (\d -> putRow db sensorType (sdKey d) (sensorFields d (sdInit d) ts)) sensorDescs
  pure db


sensorType :: TypeName
sensorType = "Sensor"


sensorFields :: SensorDesc -> Double -> Text -> Map FieldName A.Value
sensorFields d reading ts =
  Map.fromList
    [ ("id", A.String (sdKey d))
    , ("site", A.String sensorSite)
    , ("name", A.String (sdName d))
    , ("metric", A.String (sdMetric d))
    , ("unit", A.String (sdUnit d))
    , ("reading", A.toJSON (round1 reading))
    , ("updatedAt", A.String ts)
    ]


-- ---------------------------------------------------------------------------
-- Background updater
-- ---------------------------------------------------------------------------

{- | Every @tickMs@ milliseconds, random-walk each sensor's reading, write
the new row, and publish the sensor's surrogate key so any live
subscription re-executes and pushes the delta.
-}
runUpdater :: MemoryDb -> ([SurrogateKey] -> IO ()) -> Int -> IO ()
runUpdater db publish tickMs = do
  readingsV <- newTVarIO (Map.fromList [(sdKey d, sdInit d) | d <- sensorDescs])
  seedV <- newTVarIO (0x9E3779B97F4A7C15 :: Word64)
  let loop = do
        threadDelay (max 1 tickMs * 1000)
        ts <- nowIso
        keys <- forM sensorDescs $ \d -> do
          r <- atomically $ do
            rs <- readTVar readingsV
            s <- readTVar seedV
            let cur = Map.findWithDefault (sdInit d) (sdKey d) rs
                (u, s') = nextUnit s
                stepped = cur + (u - 0.5) * amplitude (sdMetric d)
                r' = clampReading (sdMetric d) stepped
            writeTVar seedV s'
            writeTVar readingsV (Map.insert (sdKey d) r' rs)
            pure r'
          atomically (putRow db sensorType (sdKey d) (sensorFields d r ts))
          pure (entityKeyOf (Ref sensorType (sdKey d)))
        publish keys
        loop
  loop


-- | A per-metric random-walk step size and clamp window.
amplitude :: Text -> Double
amplitude = \case
  "Temperature" -> 0.6
  "Humidity" -> 1.5
  "Pressure" -> 0.8
  _ -> 0.5


clampReading :: Text -> Double -> Double
clampReading metric x = case metric of
  "Temperature" -> clamp 5 35 x
  "Humidity" -> clamp 20 80 x
  "Pressure" -> clamp 990 1040 x
  _ -> x
  where
    clamp lo hi = max lo . min hi


-- | A splitmix-flavoured LCG producing a Double in @[0, 1)@ plus next state.
nextUnit :: Word64 -> (Double, Word64)
nextUnit s =
  let s' = s * 6364136223846793005 + 1442695040888963407
      bits = fromIntegral (s' `shiftR` 40) :: Double -- top 24 bits
  in (bits / 16777216, s')


round1 :: Double -> Double
round1 x = fromIntegral (round (x * 10) :: Integer) / 10


nowIso :: IO Text
nowIso = do
  now <- getCurrentTime
  pure (T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now))


-- ---------------------------------------------------------------------------
-- Static page serving
-- ---------------------------------------------------------------------------

{- | Prefer a live @sensors.html@ on disk (so edits show up without a
rebuild), falling back to the compiled-in copy.
-}
loadPage :: IO ByteString
loadPage = go ["example/sensors.html", "wireform-lattice/example/sensors.html"]
  where
    go [] = pure embeddedPage
    go (p : ps) =
      try (BS8.readFile p) >>= \case
        Right bs -> pure bs
        Left (_ :: IOException) -> go ps


staticMiddleware :: ByteString -> (Request -> IO Response) -> Request -> IO Response
staticMiddleware page inner req =
  case (requestMethod req, path) of
    (GET, p) | p `elem` ["/", "/index.html", "/sensors.html"] -> pure (htmlResponse page)
    _ -> inner req
  where
    path = BS8.takeWhile (/= '?') (requestTarget req)


htmlResponse :: ByteString -> Response
htmlResponse body =
  Response
    { responseStatus = Status 200
    , responseVersion = HTTP1_1
    , responseHeaders =
        [ ("Content-Type", "text/html; charset=utf-8")
        , ("Cache-Control", "no-store")
        ]
    , responseBody = BodyBytes body
    , responseTrailers = pure []
    , responseH2StreamId = 0
    , responseCancel = pure ()
    , responsePushPromises = pure []
    }
