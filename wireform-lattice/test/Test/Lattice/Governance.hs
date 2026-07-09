{- | Signed admission (spec §14.3) and origin coalescing (spec §6.9) over
loopback HTTP.

Admission is enforced at memo-miss compilation only: the introduction
POST carries @X-Lattice-Query-Sig@ (unpadded base64url Ed25519 over the
UTF-8 bytes of the /canonical/ text), a hash-form GET of a memoized query
never re-verifies, and the discovery document names the mode.

Coalescing is asserted with zero sleeps: requests are forked, the
window's join count is awaited via 'awaitPending' (an STM barrier), the
flush is forced with 'flushNow', and the loader traffic is counted by a
wrapper around the origin's 'beLoad' — one loader call per flush, however
many concurrent fetches joined it.
-}
module Test.Lattice.Governance (tests) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM
import Control.Exception (throwIO)
import Data.Aeson qualified as A
import Data.ByteString.Char8 qualified as BS8
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Lattice.Backend
import Lattice.Backend.Memory
import Lattice.Canonical (Compiled (..))
import Lattice.Client
import Lattice.Server (OriginConfig (..), originCoalescer)
import Lattice.Server.Auth (
  QueryAdmission (..),
  SecretKey,
  generateSecretKey,
  signQuery,
  toPublic,
 )
import Lattice.Server.Coalesce
import Lattice.Types
import Lattice.Wire (hLatticeQuerySig, queryMediaType)
import Network.HTTP.Types.Header (HeaderName, hCacheControl, hContentLocation, hETag)
import Network.HTTP.Types.Method (Method (..))
import Test.Lattice.Fixtures (compileWith, requireRight, verbsSchema)
import Test.Lattice.Loop
import Test.Syd


tests :: Spec
tests = do
  describe "§14.3 signed admission" $ do
    it "an unsigned introduction is 403 lattice:admission-denied, no-store" $
      withSigned $ \loop _sk -> do
        r <- introducePost loop [] noteQText
        rawProblem 403 "admission-denied" r
        rawHeader hCacheControl r `shouldSatisfy` maybe False ("no-store" `BS8.isInfixOf`)

    it "a signed introduction is admitted; the warm hash GET never re-verifies" $
      withSigned $ \loop sk -> do
        c <- requireRight (compileWith verbsSchema noteQText)
        let sig = signQuery sk (compiledText c)
        r <- introducePost loop [(hLatticeQuerySig, encodeUtf8 sig)] (compiledText c)
        rawStatus r `shouldBe` 200
        -- Steady state: the memo is the proof of admission (§14.3) — a
        -- bare GET with no signature flows straight through.
        warm <- httpRaw loop GET (hashTarget c) [] Nothing
        rawStatus warm `shouldBe` 200

    it "a denied query is never memoized: the hash stays cold, a later signed introduction succeeds" $
      withSigned $ \loop sk -> do
        c <- requireRight (compileWith verbsSchema noteQText)
        denied <- introducePost loop [] (compiledText c)
        rawProblem 403 "admission-denied" denied
        -- The denial memoized nothing: the hash form is still cold (a
        -- 404, not the query and not a cached 403).
        cold <- httpRaw loop GET (hashTarget c) [] Nothing
        rawProblem 404 "unknown-query" cold
        -- And no negative cache blocks the legitimate introduction.
        let sig = signQuery sk (compiledText c)
        r <- introducePost loop [(hLatticeQuerySig, encodeUtf8 sig)] (compiledText c)
        rawStatus r `shouldBe` 200

    it "the signature covers the canonical text: a pretty spelling verifies against it" $
      withSigned $ \loop sk -> do
        c <- requireRight (compileWith verbsSchema noteQText)
        -- Sign the canonical bytes, send the original (non-canonical)
        -- spelling: the origin re-canonicalizes before verifying.
        let sig = signQuery sk (compiledText c)
        r <- introducePost loop [(hLatticeQuerySig, encodeUtf8 sig)] noteQText
        rawStatus r `shouldBe` 200

    it "a tampered signature is 403" $
      withSigned $ \loop sk -> do
        c <- requireRight (compileWith verbsSchema noteQText)
        let sig = signQuery sk (compiledText c <> "x")
        r <- introducePost loop [(hLatticeQuerySig, encodeUtf8 sig)] (compiledText c)
        rawProblem 403 "admission-denied" r

    it "a signature under an unconfigured key is 403" $
      withSigned $ \loop _sk -> do
        stranger <- generateSecretKey
        c <- requireRight (compileWith verbsSchema noteQText)
        let sig = signQuery stranger (compiledText c)
        r <- introducePost loop [(hLatticeQuerySig, encodeUtf8 sig)] (compiledText c)
        rawStatus r `shouldBe` 403

    it "the ordinary client surfaces the rejection through the ladder" $
      withSigned $ \loop _sk ->
        clientFor loop (\c -> c {ccSchema = Just verbsSchema}) $ \lc ->
          expectProblem 403 "admission-denied" (queryE lc)

    it "discovery names the admission mode" $ do
      withSigned $ \loop _sk -> do
        doc <- discoveryDoc loop
        objectField "admission" doc >>= asText >>= (`shouldBe` "signed")
      withNotes Nothing $ \loop _loads -> do
        doc <- discoveryDoc loop
        objectField "admission" doc >>= asText >>= (`shouldBe` "open")

  describe "§6.9 origin coalescing" $ do
    it "N concurrent point fetches of one type flush as ONE loader call" $
      withNotes (Just bigWindow) $ \loop loads -> do
        cz <- coalescerOf loop
        inflight <- mapM (fetchAsync loop) ["n1", "n2", "n3", "n4", "n5", "n6", "n7", "n8"]
        io "eight window joins" (atomically (awaitPending cz "Note" 8))
        flushNow cz "Note"
        rs <- mapM (io "coalesced fetch" . takeMVar) inflight
        map rawStatus rs `shouldBe` replicate 8 200
        readTVarIO loads >>= (`shouldBe` 1)
        stats <- coalesceStats cz
        map fsBatch (csFlushes stats) `shouldBe` [8]
        map fsWaiters (csFlushes stats) `shouldBe` [8]

    it "responses stay independent: each fetch keeps its own ETag and body" $
      withNotes (Just bigWindow) $ \loop _loads -> do
        cz <- coalescerOf loop
        -- Bump n2 so the two rows sit at different versions: the ETag is
        -- the row's ver, rendered per response after the shared load.
        atomically $
          putRow (loopDb loop) "Note" "n2" $
            Map.fromList [("id", A.String "n2"), ("title", A.String "Title n2 v2")]
        inflight <- mapM (fetchAsync loop) ["n1", "n2"]
        io "two window joins" (atomically (awaitPending cz "Note" 2))
        flushNow cz "Note"
        [r1, r2] <- mapM (io "coalesced fetch" . takeMVar) inflight
        rawHeader hETag r1 `shouldBe` Just "\"e1\""
        rawHeader hETag r2 `shouldBe` Just "\"e2\""
        (rawBody r1 == rawBody r2) `shouldBe` False

    it "concurrent fetches of ONE key single-flight into one slot" $
      withNotes (Just bigWindow) $ \loop loads -> do
        cz <- coalescerOf loop
        inflight <- mapM (fetchAsync loop) (replicate 6 "n1")
        io "six joins on one key" (atomically (awaitPending cz "Note" 6))
        flushNow cz "Note"
        rs <- mapM (io "coalesced fetch" . takeMVar) inflight
        map rawStatus rs `shouldBe` replicate 6 200
        -- All six render from the same row: one loader call, one distinct
        -- key in the flush, six waiters.
        readTVarIO loads >>= (`shouldBe` 1)
        stats <- coalesceStats cz
        map fsBatch (csFlushes stats) `shouldBe` [1]
        map fsWaiters (csFlushes stats) `shouldBe` [6]
        case map (rawHeader hETag) rs of
          [] -> expectationFailure "no responses collected"
          (e0 : es) -> es `shouldSatisfy` all (== e0)

    it "an absent key is only that waiter's 404; the batch is undisturbed" $
      withNotes (Just bigWindow) $ \loop loads -> do
        cz <- coalescerOf loop
        good <- mapM (fetchAsync loop) ["n1", "n2"]
        bad <- fetchAsync loop "zz"
        io "three window joins" (atomically (awaitPending cz "Note" 3))
        flushNow cz "Note"
        rs <- mapM (io "coalesced fetch" . takeMVar) good
        map rawStatus rs `shouldBe` [200, 200]
        rb <- io "absent fetch" (takeMVar bad)
        rawStatus rb `shouldBe` 404
        readTVarIO loads >>= (`shouldBe` 1)

    it "a thrown loader failure fails every waiter in that flush; the next window recovers" $
      withNotesWrap (Just bigWindow) explodeOnBoom $ \loop loads -> do
        cz <- coalescerOf loop
        okAndDoomed <- mapM (fetchAsync loop) ["n1", "boom"]
        io "two window joins" (atomically (awaitPending cz "Note" 2))
        flushNow cz "Note"
        rs <- mapM (io "coalesced fetch" . takeMVar) okAndDoomed
        -- §6.9 failure isolation: the round failed, so BOTH waiters
        -- report the ordinary whole-request 5xx.
        map rawStatus rs `shouldSatisfy` all (>= 500)
        -- Retries re-enter coalescing like any other arrival.
        retryMv <- fetchAsync loop "n1"
        io "retry join" (atomically (awaitPending cz "Note" 1))
        flushNow cz "Note"
        r <- io "retry fetch" (takeMVar retryMv)
        rawStatus r `shouldBe` 200
        readTVarIO loads >>= (`shouldBe` 2)

    it "a per-key loader failure stays per-key" $
      withNotesWrap (Just bigWindow) leftOnBad $ \loop loads -> do
        cz <- coalescerOf loop
        inflight <- mapM (fetchAsync loop) ["n1", "bad"]
        io "two window joins" (atomically (awaitPending cz "Note" 2))
        flushNow cz "Note"
        [rOk, rBad] <- mapM (io "coalesced fetch" . takeMVar) inflight
        rawStatus rOk `shouldBe` 200
        rawProblem 503 "upstream-unavailable" rBad
        readTVarIO loads >>= (`shouldBe` 1)

    it "a version-pinned mismatch through the coalesced path is still 404 version-unavailable" $
      withNotes (Just bigWindow) $ \loop _loads -> do
        cz <- coalescerOf loop
        mv <- newEmptyMVar
        _ <- forkIO (httpRaw loop GET "/e/Note/n1?ver=e99" [] Nothing >>= putMVar mv)
        io "pinned join" (atomically (awaitPending cz "Note" 1))
        flushNow cz "Note"
        r <- io "pinned fetch" (takeMVar mv)
        rawProblem 404 "version-unavailable" r
        rawHeader hCacheControl r `shouldSatisfy` maybe False ("no-store" `BS8.isInfixOf`)
        rawHeader hContentLocation r `shouldBe` Just "/e/Note/n1"

    it "query serving does not coalesce" $
      withNotes (Just bigWindow) $ \loop _loads -> do
        cz <- coalescerOf loop
        clientFor loop (\c -> c {ccSchema = Just verbsSchema}) $ \lc -> do
          r <- io "query" (queryE lc) >>= requireRight
          qrDegraded r `shouldBe` False
        stats <- coalesceStats cz
        csFlushes stats `shouldBe` []
        csPending stats `shouldBe` Map.empty

    it "discovery derives coalesceWindowMs from the config" $ do
      withNotes (Just CoalesceConfig {ccWindowMicros = 5000, ccMaxBatch = 64}) $ \loop _ -> do
        doc <- discoveryDoc loop
        ms <- objectField "budgets" doc >>= objectField "coalesceWindowMs"
        ms `shouldBe` A.Number 5
      withNotes Nothing $ \loop _ -> do
        doc <- discoveryDoc loop
        ms <- objectField "budgets" doc >>= objectField "coalesceWindowMs"
        ms `shouldBe` A.Number 0


-- ---------------------------------------------------------------------------
-- Fixture: the verbs schema's Note table under governance knobs
-- ---------------------------------------------------------------------------

noteRows :: [(TypeName, Map FieldName A.Value)]
noteRows = map row ["n1", "n2", "n3", "n4", "n5", "n6", "n7", "n8"]
  where
    row k = ("Note", Map.fromList [("id", A.String k), ("title", A.String ("Title " <> k))])


noteHooks :: MemoryHooks
noteHooks =
  defaultHooks
    { mhGetRoots =
        Map.fromList
          [ ("note", byIdRoot "Note")
          , ("flag", byIdRoot "Flag")
          ]
    }
  where
    byIdRoot ty _db args = pure $ case Map.lookup "id" args of
      Just (A.String k) -> Just (Ref ty k)
      _ -> Nothing


-- | Loader-counting origin; optional custom wrap runs INSIDE the counter
-- (the counter sees exactly the origin's loader traffic).
withNotesWrap ::
  Maybe CoalesceConfig ->
  (Backend -> Backend) ->
  (Loop -> TVar Int -> IO a) ->
  IO a
withNotesWrap cfg customWrap action = do
  loads <- newTVarIO (0 :: Int)
  let count b =
        b
          { beLoad = \ty proj ks -> do
              atomically (modifyTVar' loads (+ 1))
              beLoad b ty proj ks
          }
  withLoop
    (loopSpec verbsSchema)
      { lsHooks = noteHooks
      , lsRows = noteRows
      , lsWrap = count . customWrap
      , lsTweak = \c -> c {ocCoalesce = cfg}
      }
    (\loop -> action loop loads)


withNotes :: Maybe CoalesceConfig -> (Loop -> TVar Int -> IO a) -> IO a
withNotes cfg = withNotesWrap cfg id


-- | AdmitSigned origin over one generated key.
withSigned :: (Loop -> SecretKey -> IO a) -> IO a
withSigned action = do
  sk <- generateSecretKey
  withLoop
    (loopSpec verbsSchema)
      { lsHooks = noteHooks
      , lsRows = noteRows
      , lsTweak = \c -> c {ocAdmission = AdmitSigned [toPublic sk]}
      }
    (\loop -> action loop sk)


-- | A window long enough that only 'flushNow' (or 'ccMaxBatch') flushes
-- within a test's lifetime — the determinism guarantee.
bigWindow :: CoalesceConfig
bigWindow = CoalesceConfig {ccWindowMicros = 10_000_000, ccMaxBatch = 64}


explodeOnBoom :: Backend -> Backend
explodeOnBoom b =
  b
    { beLoad = \ty proj ks ->
        if "boom" `elem` ks
          then throwIO (userError "loader exploded")
          else beLoad b ty proj ks
    }


leftOnBad :: Backend -> Backend
leftOnBad b =
  b
    { beLoad = \ty proj ks -> do
        m <- beLoad b ty proj ks
        pure (Map.mapWithKey (\k v -> if k == "bad" then Left upstreamUnavailable else v) m)
    }


-- ---------------------------------------------------------------------------
-- Operations
-- ---------------------------------------------------------------------------

noteQText :: Text
noteQText = "query NoteTitle { note(id: \"n1\") { title } }"


queryE :: LatticeClient -> IO (Either LatticeError QueryResult)
queryE lc = query lc noteQText Map.empty


introducePost :: Loop -> [(HeaderName, BS8.ByteString)] -> Text -> IO RawResp
introducePost loop extra body =
  httpRaw
    loop
    POST
    "/q?intent=introduce&slice=pub"
    (("Content-Type", queryMediaType) : extra)
    (Just (encodeUtf8 body))


hashTarget :: Compiled -> BS8.ByteString
hashTarget c = "/q/" <> encodeUtf8 (compiledHash c) <> "?slice=pub"


fetchAsync :: Loop -> Text -> IO (MVar RawResp)
fetchAsync loop key = do
  mv <- newEmptyMVar
  _ <- forkIO (httpRaw loop GET ("/e/Note/" <> encodeUtf8 key) [] Nothing >>= putMVar mv)
  pure mv


coalescerOf :: Loop -> IO Coalescer
coalescerOf loop =
  maybe
    (expectationFailure "origin has no coalescer despite ocCoalesce")
    pure
    (originCoalescer (loopOrigin loop))


discoveryDoc :: Loop -> IO A.Value
discoveryDoc loop = do
  r <- httpRaw loop GET "/.well-known/lattice" [] Nothing
  rawStatus r `shouldBe` 200
  maybe (expectationFailure "discovery document did not decode") pure (A.decodeStrict (rawBody r))
