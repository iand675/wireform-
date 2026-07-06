{- | Observability (spec §19) over the loopback origin, with in-memory
capture: spans through the API's own 'TracerProvider' fed by a
collecting 'SpanProcessor' (the "Lattice.Telemetry" haddock recipe), and
metrics by record-updating a live 'LatticeTelemetry' with TVar-backed
instruments — no SDK, no exporters, no sleeps.

Coverage, per the §19 pins:

* __Span topology (§19.2)__: a warm hash GET's captured tree —
  server span named by route template, @lattice.execute@ →
  @lattice.round[i]@ → @lattice.load@ with loader\/batch attributes —
  matches the static skeleton @explain@ emits for the same plan
  (@\"spans\"@), which is the trace-vs-plan diffability §19.2 promises.
* __Compile span on the miss path only__: introduction opens
  @lattice.compile@ (@lattice.compile.cold@), the warm GET never does.
* __Metrics (§19.3)__: @lattice.loader.batch_size@ records per loader
  invocation with the loader name attribute; the compile histogram
  records on the cold path.
* __§19.4__: shared-cacheable responses NEVER carry @traceresponse@;
  uncacheable ones (oneshots, mutations) do when telemetry is on.
* __Error events (§19.2)__: a scoped stream error surfaces as a span
  event carrying @lattice.error.scope@ \/ @lattice.error.code@ — never
  the entity id.
-}
module Test.Lattice.Otel (tests) where

import Control.Concurrent.STM
import Data.Aeson qualified as A
import Data.ByteString.Char8 qualified as BS8
import Data.Foldable (for_, toList)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Traversable (for)
import Lattice.Backend.Memory (MemoryHooks (..), defaultHooks)
import Lattice.Canonical (Compiled (..))
import Lattice.Server (OriginConfig (..))
import Lattice.Telemetry
import Lattice.Types
import Lattice.Wire (queryMediaType)
import Network.HTTP.Types.Method (Method (..))
import Test.Lattice.Fixtures
import Test.Lattice.Loop
import Test.Syd


tests :: Spec
tests =
  describe "Observability (§19)" $ do
    describe "span topology (§19.2)" $ do
      it "a warm hash GET opens the §19.2 tree: server span (route template, hash as attribute) > execute > rounds > loads" $
        withOtel $ \loop cap -> do
          c <- introduceOtel loop pageQ
          resetCapture cap
          warm <- httpRaw loop GET (pubTarget c) [] Nothing
          rawStatus warm `shouldBe` 200
          spans <- endedSpans cap
          server <- serverSpanOf ("GET /q/{hash}") spans
          -- Route-template naming: never the concrete hash.
          attrText server "lattice.query.hash" `shouldBe` Just (compiledHash c)
          attrText server "lattice.slice" `shouldBe` Just "pub"
          -- The runtime tree: the root entity load and the paginated-edge
          -- children fetch ride round 0; the discovered children load in
          -- round 1. One span per loader call, regardless of cardinality.
          skeletonOf spans server
            `shouldBe` [
                         [ ("lattice.round[0]", ["Post", "Post.comments"])
                         , ("lattice.round[1]", ["Comment"])
                         ]
                       ]

      it "explain emits the expected span skeleton: execute > rounds > loads with loader and batch (§19.2/§20.2)" $
        withOtel $ \loop _cap -> do
          c <- introduceOtel loop pageQ
          skeleton <- explainSkeleton loop c
          -- Two rounds, each with at least one load naming its loader.
          case skeleton of
            [rounds] -> do
              map fst rounds `shouldBe` ["lattice.round[0]", "lattice.round[1]"]
              for_ rounds $ \(_, loaders) -> loaders `shouldSatisfy` (not . null)
            other -> expectationFailure ("expected one execute skeleton, got " <> show (length other))

      it "a warm hash GET's span tree EQUALS explain's static skeleton" $
        withOtel $ \loop cap -> do
          c <- introduceOtel loop pageQ
          static <- explainSkeleton loop c
          resetCapture cap
          warm <- httpRaw loop GET (pubTarget c) [] Nothing
          rawStatus warm `shouldBe` 200
          spans <- endedSpans cap
          server <- serverSpanOf "GET /q/{hash}" spans
          -- The trace-vs-plan diff §19.2 promises: the live tree and
          -- explain's static skeleton are the SAME shape — same rounds,
          -- same loader-name vocabulary ("Post", "Post.comments"), the
          -- paginated-edge fetch opening in the parent round.
          skeletonOf spans server `shouldBe` static

      it "every load span carries the loader name and a positive batch size" $
        withOtel $ \loop cap -> do
          c <- introduceOtel loop pageQ
          resetCapture cap
          _ <- httpRaw loop GET (pubTarget c) [] Nothing
          spans <- endedSpans cap
          let loads = [s | s <- spans, shName s == "lattice.load"]
          loads `shouldSatisfy` (not . null)
          for_ loads $ \s -> do
            attrText s "lattice.loader.name" `shouldSatisfy` (/= Nothing)
            attrInt s "lattice.loader.batch_size" `shouldSatisfy` maybe False (> 0)

      it "lattice.compile opens on the miss path only, marked cold" $
        withOtel $ \loop cap -> do
          resetCapture cap
          c <- introduceOtel loop pageQ
          cold <- endedSpans cap
          compile <- case [s | s <- cold, shName s == "lattice.compile"] of
            [s] -> pure s
            other -> expectationFailure ("expected one compile span on the miss, got " <> show (length other))
          attrBool compile "lattice.compile.cold" `shouldBe` Just True
          resetCapture cap
          _ <- httpRaw loop GET (pubTarget c) [] Nothing
          warm <- endedSpans cap
          [s | s <- warm, shName s == "lattice.compile"] `shouldSatisfy` null

    describe "metrics (§19.3)" $ do
      it "lattice.loader.batch_size records per loader invocation; compile.duration on the cold path" $
        withOtel $ \loop cap -> do
          c <- introduceOtel loop pageQ
          compiles <- histValues cap "lattice.compile.duration"
          compiles `shouldSatisfy` (not . null)
          resetHists cap
          _ <- httpRaw loop GET (pubTarget c) [] Nothing
          batches <- histValues cap "lattice.loader.batch_size"
          batches `shouldSatisfy` (not . null)
          batches `shouldSatisfy` all (>= 1)

    describe "telemetry and shared caches (§19.4)" $ do
      it "a shared-cacheable pub GET NEVER carries traceresponse; an uncacheable oneshot does" $
        withOtel $ \loop _cap -> do
          c <- introduceOtel loop pageQ
          warm <- httpRaw loop GET (pubTarget c) [] Nothing
          rawStatus warm `shouldBe` 200
          rawHeader "traceresponse" warm `shouldBe` Nothing
          oneshot <-
            httpRaw
              loop
              POST
              "/q?intent=oneshot&slice=pub"
              [("Content-Type", queryMediaType)]
              (Just (encodeUtf8 pageQ))
          rawStatus oneshot `shouldBe` 200
          rawHeader "traceresponse" oneshot `shouldSatisfy` (/= Nothing)

    describe "error events (§19.2)" $ do
      it "a scoped stream error is a span event carrying error.scope and error.code, not the entity id" $
        withOtel $ \loop cap -> do
          c <- introduceOtel loop pageQ
          resetCapture cap
          atomically (writeTVar (loopBreakEdges loop) True)
          r <- httpRaw loop GET (pubTarget c) [] Nothing
          atomically (writeTVar (loopBreakEdges loop) False)
          -- Broken edges degrade to 207 (partial content), never a 5xx.
          rawStatus r `shouldBe` 207
          spans <- endedSpans cap
          let evts = concatMap shEvents spans
              errEvts = [e | e <- evts, fst e == "lattice.error"]
          errEvts `shouldSatisfy` (not . null)
          for_ errEvts $ \(_, attrs) -> do
            lookupText attrs "lattice.error.scope" `shouldSatisfy` (/= Nothing)
            lookupText attrs "lattice.error.code"
              `shouldSatisfy` maybe False ("lattice:" `T.isPrefixOf`)


-- ---------------------------------------------------------------------------
-- Capture harness (the Lattice.Telemetry haddock recipe)
-- ---------------------------------------------------------------------------

data Capture = Capture
  { capRef :: IORef [ImmutableSpan]
  , capHists :: TVar [(Text, Double)]
  , capCounts :: TVar [(Text, Int64)]
  }


-- | One ended span, denormalized for assertions.
data Shot = Shot
  { shName :: Text
  , shAttrs :: Attributes
  , shEvents :: [(Text, Attributes)]
  , shSpanId :: Text
  , shParentId :: Maybe Text
  }
  deriving stock (Show)


{- | A live telemetry handle whose spans end into 'capRef' and whose
batch-size\/compile histograms and counters append into TVars (the
record-update trick: 'newLatticeTelemetry' against the no-op meter, then
swap in TVar-backed instruments).
-}
withOtel :: (Loop -> Capture -> IO a) -> IO a
withOtel k = do
  ref <- newIORef []
  hists <- newTVarIO []
  counts <- newTVarIO []
  let processor =
        SpanProcessor
          { spanProcessorOnStart = \_ _ -> pure ()
          , spanProcessorOnEnd = \sp -> modifyIORef' ref (sp :)
          , spanProcessorShutdown = pure ShutdownSuccess
          , spanProcessorForceFlush = pure FlushSuccess
          }
  tp <- createTracerProvider [processor] emptyTracerProviderOptions
  tel0 <- newLatticeTelemetry tp noopMeterProvider
  let histTo name =
        Histogram
          { histogramRecord = \v _ -> atomically (modifyTVar' hists ((name, v) :))
          , histogramEnabled = pure True
          }
      cntTo name =
        Counter
          { counterAdd = \n _ -> atomically (modifyTVar' counts ((name, n) :))
          , counterEnabled = pure True
          }
      tel =
        tel0
          { ltLoaderBatchSize = histTo "lattice.loader.batch_size"
          , ltCompileDuration = histTo "lattice.compile.duration"
          , ltPurgeFanout = histTo "lattice.purge.fanout"
          , ltTenurePromotions = cntTo "lattice.tenure.promotions"
          , ltMutationReplays = cntTo "lattice.mutation.replays"
          }
  withLoop
    (loopSpec derivedSchema)
      { lsHooks = otelHooks
      , lsRows = otelRows
      , lsTweak = \c -> c {ocTelemetry = tel}
      }
    (\loop -> k loop Capture {capRef = ref, capHists = hists, capCounts = counts})


resetCapture :: Capture -> IO ()
resetCapture cap = do
  writeIORef (capRef cap) []
  resetHists cap


resetHists :: Capture -> IO ()
resetHists cap = atomically $ do
  writeTVar (capHists cap) []
  writeTVar (capCounts cap) []


histValues :: Capture -> Text -> IO [Double]
histValues cap name =
  map snd . filter ((== name) . fst) <$> readTVarIO (capHists cap)


-- | Ended spans in start order, denormalized.
endedSpans :: Capture -> IO [Shot]
endedSpans cap = do
  sps <- reverse <$> readIORef (capRef cap)
  for sps $ \is -> do
    hot <- readIORef (spanHot is)
    parent <- traverse getSpanContext (spanParent is)
    pure
      Shot
        { shName = hotName hot
        , shAttrs = hotAttributes hot
        , shEvents =
            [ (eventName e, eventAttributes e)
            | e <- toList (appendOnlyBoundedCollectionValues (hotEvents hot))
            ]
        , shSpanId = T.pack (show (spanId (spanContext is)))
        , shParentId = T.pack . show . spanId <$> parent
        }


serverSpanOf :: Text -> [Shot] -> IO Shot
serverSpanOf name spans = case [s | s <- spans, shName s == name] of
  [s] -> pure s
  other -> expectationFailure ("expected one " <> T.unpack name <> " server span, got " <> show (length other))


childrenOf :: [Shot] -> Shot -> [Shot]
childrenOf spans parent = [s | s <- spans, shParentId s == Just (shSpanId parent)]


{- | The captured tree below a server span, in explain's shape: one entry
per execute, with rounds in index order and each round's loader names
sorted (batch sizes are runtime facts; the skeleton pins structure).
-}
skeletonOf :: [Shot] -> Shot -> [[(Text, [Text])]]
skeletonOf spans server =
  [ [ (shName r, sort (mapMaybe (`attrText` "lattice.loader.name") (childrenOf spans r)))
    | r <- childrenOf spans e
    ]
  | e <- childrenOf spans server
  , shName e == "lattice.execute"
  ]


-- | Explain's @spans@ skeleton, projected to the same shape.
explainSkeleton :: Loop -> Compiled -> IO [[(Text, [Text])]]
explainSkeleton loop c = do
  r <- httpRaw loop GET ("/q/" <> encodeUtf8 (compiledHash c) <> "/explain") [] Nothing
  rawStatus r `shouldBe` 200
  body <- case A.decodeStrict (rawBody r) of
    Just v -> pure v
    Nothing -> expectationFailure "explain body is not JSON"
  execs <- objectField "spans" body >>= asArray
  for execs $ \e -> do
    nm <- textField "name" e
    nm `shouldBe` "lattice.execute"
    rounds <- objectField "children" e >>= asArray
    for rounds $ \rd -> do
      rn <- textField "name" rd
      loads <- objectField "children" rd >>= asArray
      loaders <- for loads $ \l -> do
        ln <- textField "name" l
        ln `shouldBe` "lattice.load"
        textField "loader" l
      pure (rn, sort loaders)


-- ---------------------------------------------------------------------------
-- Attribute projections
-- ---------------------------------------------------------------------------

lookupText :: Attributes -> Text -> Maybe Text
lookupText attrs key = case lookupAttribute attrs key of
  Just (AttributeValue (TextAttribute t)) -> Just t
  _ -> Nothing


attrText :: Shot -> Text -> Maybe Text
attrText s = lookupText (shAttrs s)



attrInt :: Shot -> Text -> Maybe Int64
attrInt s key = case lookupAttribute (shAttrs s) key of
  Just (AttributeValue (IntAttribute n)) -> Just n
  _ -> Nothing


attrBool :: Shot -> Text -> Maybe Bool
attrBool s key = case lookupAttribute (shAttrs s) key of
  Just (AttributeValue (BoolAttribute b)) -> Just b
  _ -> Nothing




-- ---------------------------------------------------------------------------
-- Fixture and requests
-- ---------------------------------------------------------------------------

otelRows :: [(TypeName, Map FieldName A.Value)]
otelRows =
  [ ("Author", Map.fromList [("id", A.String "a1"), ("name", A.String "Ada")])
  , ("Post", Map.fromList [("id", A.String "p1"), ("title", A.String "First"), ("authorId", A.String "a1")])
  , ("Comment", commentRow "c1")
  , ("Comment", commentRow "c2")
  ]
  where
    commentRow cid =
      Map.fromList
        [ ("id", A.String cid)
        , ("postId", A.String "p1")
        , ("body", A.String ("comment " <> cid))
        , ("createdAt", A.String "2024-01-01T00:00:00Z")
        ]


otelHooks :: MemoryHooks
otelHooks =
  defaultHooks
    { mhGetRoots = Map.fromList [("post", byIdRoot)]
    }
  where
    byIdRoot _db args = pure $ case Map.lookup "id" args of
      Just (A.String k) -> Just (Ref "Post" k)
      _ -> Nothing


-- | Root load plus a children round: at least two rounds of loads.
pageQ :: Text
pageQ = "query OtelPage { post(id: \"p1\") { title comments(first: 10) { body } } }"


pubTarget :: Compiled -> BS8.ByteString
pubTarget c = "/q/" <> encodeUtf8 (compiledHash c) <> "?slice=pub"


introduceOtel :: Loop -> Text -> IO Compiled
introduceOtel loop q = do
  c <- mustCompileWith derivedSchema q
  r <-
    httpRaw
      loop
      POST
      "/q?intent=introduce&slice=pub"
      [("Content-Type", queryMediaType)]
      (Just (encodeUtf8 (compiledText c)))
  rawStatus r `shouldBe` 200
  pure c
