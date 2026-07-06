{- | The compatibility registry as deployed infrastructure (spec §17):
the deployment log ('Lattice.Registry'), the origin's memo-as-corpus
export with @Lattice-Client@ attribution (§17.4), and the
@POST /schema/check@ \/ @GET /schema/corpus@ routes (§17.3).

The registry is optional infrastructure (§17.1): with @ocRegistry =
Nothing@ both routes are the ordinary 404 and the origin records no
client attribution. The HTTP status of a well-formed check describes
the __request__, never the verdict — pass\/fail ride inside the report
body, which is what CI gates read.
-}
module Test.Lattice.Registry (tests) where

import Control.Exception (IOException, try)
import Data.Aeson qualified as A
import Data.ByteString.Char8 qualified as BS8
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import Lattice.Backend.Memory (MemoryHooks (..), defaultHooks)
import Lattice.Canonical (Compiled (..))
import Lattice.IDL.Print (canonicalIdl)
import Lattice.Registry (
  DeployEntry (..),
  Registry,
  newRegistry,
  recordDeploy,
  registryLog,
 )
import Lattice.Server (OriginConfig (..))
import Lattice.Types
import Lattice.Wire (hLatticeClient, queryMediaType)
import Network.HTTP.Types.Header (hCacheControl, hContentType)
import Network.HTTP.Types.Method (Method (..))
import Test.Lattice.Fixtures
import Test.Lattice.Loop
import Test.Syd


tests :: Spec
tests =
  describe "Compatibility registry (§17)" $ do
    describe "the deployment log" $ do
      it "entries are content-addressed: a same-content redeploy keeps one entry at the newest time" $ do
        reg <- newRegistry
        let idl = canonicalIdl derivedSchema
        recordDeploy reg (utc 2026 1 1) idl
        recordDeploy reg (utc 2026 6 1) idl
        logd <- registryLog reg
        map deTime logd `shouldBe` [utc 2026 6 1]

      it "distinct schemas log distinct hashes, newest first" $ do
        reg <- newRegistry
        recordDeploy reg (utc 2026 1 1) (canonicalIdl derivedSchema)
        recordDeploy reg (utc 2026 6 1) (canonicalIdl digestSchema)
        logd <- registryLog reg
        map deTime logd `shouldBe` [utc 2026 6 1, utc 2026 1 1]
        case map deHash logd of
          [h1, h2] -> h1 `shouldNotBe` h2
          other -> expectationFailure ("expected two entries, got: " <> show (length other))

      it "an unparseable deploy throws (the log never holds garbage)" $ do
        reg <- newRegistry
        try (recordDeploy reg (utc 2026 1 1) "definitely not idl {") >>= \case
          Left (e :: IOException) -> show e `shouldSatisfy` (not . null)
          Right () -> expectationFailure "unparseable IDL was accepted into the deployment log"

    describe "the memo is the corpus (§17.4)" $ do
      it "GET /schema/corpus exports canonical texts with tenure hits and Lattice-Client attribution" $
        withRegistry $ \loop _reg -> do
          c <- warmCorpus loop
          r <- httpRaw loop GET "/schema/corpus" [] Nothing
          rawStatus r `shouldBe` 200
          rawHeader hCacheControl r `shouldBe` Just "no-store"
          entry <- corpusEntryFor (compiledText c) r
          hits <- objectField "hits" entry
          hits `shouldSatisfy` atLeast 3
          clients <- objectField "clients" entry >>= asArray >>= traverse asText
          clients `shouldBe` ["app-ios/3.19.2"]

    describe "POST /schema/check (§17.3): the status describes the request, the body carries the verdict" $ do
      it "the deployed schema re-submitted verbatim passes" $
        withRegistry $ \loop _reg -> do
          r <- checkPost loop "" (canonicalIdl derivedSchema)
          rawStatus r `shouldBe` 200
          verdict r >>= (`shouldBe` True)

      it "a field removal fails INSIDE a 200, naming the subject with corpus weights" $
        withRegistry $ \loop _reg -> do
          c <- warmCorpus loop
          r <- checkPost loop "" candidateNoTitle
          rawStatus r `shouldBe` 200
          verdict r >>= (`shouldBe` False)
          change <- changeNaming "Post.title" r
          corpus <- objectField "corpus" change
          texts <- objectField "texts" corpus >>= asArray >>= traverse asText
          texts `shouldSatisfy` elem (compiledText c)

      it "a transitive mode with a window is accepted" $
        withRegistry $ \loop _reg -> do
          r <- checkPost loop "?mode=client-backward-transitive&window=P18M" (canonicalIdl derivedSchema)
          rawStatus r `shouldBe` 200
          verdict r >>= (`shouldBe` True)

      it "a non-IDL content type is 415 lattice:unsupported-media" $
        withRegistry $ \loop _reg -> do
          r <-
            httpRaw
              loop
              POST
              "/schema/check"
              [(hContentType, "application/json")]
              (Just (encodeUtf8 (canonicalIdl derivedSchema)))
          rawProblem 415 "unsupported-media" r

      it "an unparseable candidate is 400 lattice:compile-rejected" $
        withRegistry $ \loop _reg -> do
          r <- checkPost loop "" "definitely not idl {"
          rawProblem 400 "compile-rejected" r

      it "an unknown mode is 400" $
        withRegistry $ \loop _reg -> do
          r <- checkPost loop "?mode=sideways" (canonicalIdl derivedSchema)
          rawStatus r `shouldBe` 400

      it "an unparseable window is 400" $
        withRegistry $ \loop _reg -> do
          r <- checkPost loop "?mode=client-backward-transitive&window=fortnight" (canonicalIdl derivedSchema)
          rawStatus r `shouldBe` 400

    describe "the registry is optional infrastructure (§17.1)" $ do
      it "without a registry both routes are the ordinary 404" $
        withPlain $ \loop -> do
          corpus <- httpRaw loop GET "/schema/corpus" [] Nothing
          rawProblem 404 "not-found" corpus
          check <- checkPost loop "" (canonicalIdl derivedSchema)
          rawProblem 404 "not-found" check


-- ---------------------------------------------------------------------------
-- Fixture: the derived schema behind a registry
-- ---------------------------------------------------------------------------

regRows :: [(TypeName, Map FieldName A.Value)]
regRows =
  [ ("Author", Map.fromList [("id", A.String "a1"), ("name", A.String "Ada")])
  , ("Post", Map.fromList [("id", A.String "p1"), ("title", A.String "First"), ("authorId", A.String "a1")])
  ]


regHooks :: MemoryHooks
regHooks =
  defaultHooks
    { mhGetRoots = Map.fromList [("post", byIdRoot)]
    }
  where
    byIdRoot _db args = pure $ case Map.lookup "id" args of
      Just (A.String k) -> Just (Ref "Post" k)
      _ -> Nothing


-- | A loop with the deployed schema recorded in a fresh registry.
withRegistry :: (Loop -> Registry -> IO a) -> IO a
withRegistry k = do
  reg <- newRegistry
  recordDeploy reg (utc 2026 1 1) (canonicalIdl derivedSchema)
  withLoop
    (loopSpec derivedSchema)
      { lsHooks = regHooks
      , lsRows = regRows
      , lsTweak = \c -> c {ocRegistry = Just reg}
      }
    (\loop -> k loop reg)


withPlain :: (Loop -> IO a) -> IO a
withPlain =
  withLoop (loopSpec derivedSchema) {lsHooks = regHooks, lsRows = regRows}


-- | Introduce the corpus query and warm it: three hash-form GETs, each
-- attributed to one client build.
warmCorpus :: Loop -> IO Compiled
warmCorpus loop = do
  c <- mustCompileWith derivedSchema "query CorpusQ { post(id: \"p1\") { title } }"
  intro <-
    httpRaw
      loop
      POST
      "/q?intent=introduce&slice=pub"
      [(hContentType, queryMediaType)]
      (Just (encodeUtf8 (compiledText c)))
  rawStatus intro `shouldBe` 200
  let target = "/q/" <> encodeUtf8 (compiledHash c) <> "?slice=pub"
      hit = httpRaw loop GET target [(hLatticeClient, "app-ios/3.19.2")] Nothing
  r1 <- hit
  rawStatus r1 `shouldBe` 200
  r2 <- hit
  rawStatus r2 `shouldBe` 200
  r3 <- hit
  rawStatus r3 `shouldBe` 200
  pure c


-- | The deployed IDL minus the @title@ field.
candidateNoTitle :: Text
candidateNoTitle =
  T.unlines
    [ ln
    | ln <- T.lines (canonicalIdl derivedSchema)
    , not ("title:" `T.isPrefixOf` T.strip ln)
    ]


checkPost :: Loop -> BS8.ByteString -> Text -> IO RawResp
checkPost loop params body =
  httpRaw
    loop
    POST
    ("/schema/check" <> params)
    [(hContentType, "application/x-lattice-idl")]
    (Just (encodeUtf8 body))


-- ---------------------------------------------------------------------------
-- Report-body assertions
-- ---------------------------------------------------------------------------

reportBody :: RawResp -> IO A.Value
reportBody r = case A.decodeStrict (rawBody r) of
  Just v -> pure v
  Nothing -> expectationFailure ("check response body is not JSON: " <> show (rawBody r))


verdict :: RawResp -> IO Bool
verdict r = do
  body <- reportBody r
  objectField "pass" body >>= \case
    A.Bool b -> pure b
    other -> expectationFailure ("expected a boolean pass field, got: " <> show other)


-- | The single reported change naming the subject.
changeNaming :: Text -> RawResp -> IO A.Value
changeNaming subj r = do
  body <- reportBody r
  changes <- objectField "changes" body >>= asArray
  named <- traverse (\c -> (,c) <$> textField "subject" c) changes
  case [c | (s, c) <- named, s == subj] of
    [c] -> pure c
    other -> expectationFailure ("expected one change naming " <> T.unpack subj <> ", got " <> show (length other))


-- | The corpus entry whose canonical text matches.
corpusEntryFor :: Text -> RawResp -> IO A.Value
corpusEntryFor text r = do
  body <- reportBody r
  entries <- objectField "corpus" body >>= asArray
  named <- traverse (\e -> (,e) <$> textField "text" e) entries
  case [e | (t, e) <- named, t == text] of
    [e] -> pure e
    other -> expectationFailure ("expected one corpus entry for the query, got " <> show (length other))


atLeast :: Int -> A.Value -> Bool
atLeast n = \case
  A.Number s -> s >= fromIntegral n
  _ -> False


utc :: Integer -> Int -> Int -> UTCTime
utc y m d = UTCTime (fromGregorian y m d) 0
