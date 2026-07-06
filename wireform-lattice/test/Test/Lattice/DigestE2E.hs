{- | Cache digests over loopback HTTP (spec §10.4): the transport half of
"Test.Lattice.Digest".

Server side: @X-Have@ \/ @X-Have-Digest@ are honored on priv slices and
oneshot POSTs only — a matching @(id, ver)@ entity record emits as an
@unchanged@ marker in its stream position — and ignored entirely on
ordinary pub requests (responses never vary on them).

Client side: "Lattice.Client" advertises its store on priv-slice
requests (enumerated at ≤ 32 entries, GCS at @fp=10@ above), applies
markers as keep-and-mark-fresh, and repairs a false-positive elision
(a marker for an entity the store lacks) with a follow-up point fetch.
The false positive is engineered deterministically: the test recomputes
the exact digest the client will advertise and mines a fresh key that
collides into it ('minedKey' — pure, so the found key is a constant of
the fixture).
-}
module Test.Lattice.DigestE2E (tests) where

import Control.Concurrent.STM (atomically)
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Char8 qualified as BS8
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Lattice.Backend.Memory (MemoryHooks (..), defaultHooks, putRow)
import Lattice.Canonical (Compiled (..))
import Lattice.Client
import Lattice.Client.Store (snapshotEntities, unchangedGaps)
import Lattice.Digest (
  Digest (..),
  digestContains,
  encodeGcs,
  parseHaveDigest,
  renderHave,
  renderHaveDigest,
 )
import Lattice.Types
import Lattice.Wire (EntityRecord (..), Record (..), hXHave, hXHaveDigest, queryMediaType)
import Network.HTTP.Types.Header (HeaderName, lookupHeader)
import Network.HTTP.Types.Method (Method (..))
import Test.Lattice.Fixtures
import Test.Lattice.Loop
import Test.Syd


tests :: Spec
tests =
  describe "Cache digests over loopback HTTP (§10.4)" $ do
    describe "the origin elides against the advertised digest" $ do
      it "a priv GET with X-Have emits matching records as unchanged markers, in stream position" $
        withDocs 2 $ \loop -> do
          c <- mustCompileWith digestSchema docSecretInline
          r0 <- introducePost loop "priv" [auth] (compiledText c)
          rawStatus r0 `shouldBe` 200
          r <-
            httpRaw
              loop
              GET
              ("/q/" <> encodeUtf8 (compiledHash c) <> "?slice=priv")
              [auth, (hXHave, have [("Doc:d01", "e1")])]
              Nothing
          rawStatus r `shouldBe` 200
          unchangedOf r `shouldBe` [(Ref "Doc" "d01", "e1")]
          entityRecordsOf r `shouldBe` []
          -- The marker sits exactly where the entity record would:
          -- manifest, marker, end.
          map recordKind (rawRecords r) `shouldBe` ["manifest", "unchanged", "end"]

      it "a oneshot POST honors the digest even on the pub slice" $
        withDocs 2 $ \loop -> do
          r <- oneshotPost loop "pub" [(hXHave, have [("Doc:d01", "e1")])] docTitleInline
          rawStatus r `shouldBe` 200
          unchangedOf r `shouldBe` [(Ref "Doc" "d01", "e1")]
          entityRecordsOf r `shouldBe` []

      it "an ordinary pub request ignores X-Have entirely (responses never vary on it)" $
        withDocs 2 $ \loop -> do
          intro <- introducePost loop "pub" [(hXHave, have [("Doc:d01", "e1")])] docTitleInline
          rawStatus intro `shouldBe` 200
          unchangedOf intro `shouldBe` []
          map erId (entityRecordsOf intro) `shouldBe` [Ref "Doc" "d01"]
          c <- mustCompileWith digestSchema docTitleInline
          warm <-
            httpRaw
              loop
              GET
              ("/q/" <> encodeUtf8 (compiledHash c) <> "?slice=pub")
              [(hXHave, have [("Doc:d01", "e1")])]
              Nothing
          rawStatus warm `shouldBe` 200
          unchangedOf warm `shouldBe` []
          map erId (entityRecordsOf warm) `shouldBe` [Ref "Doc" "d01"]

      it "a stale ver never elides: only the current (id, ver) matches" $
        withDocs 2 $ \loop -> do
          r <- oneshotPost loop "pub" [(hXHave, have [("Doc:d01", "e0")])] docTitleInline
          rawStatus r `shouldBe` 200
          unchangedOf r `shouldBe` []
          map erId (entityRecordsOf r) `shouldBe` [Ref "Doc" "d01"]

      it "an in-policy X-Have-Digest elides; an out-of-policy fp is ignored on the wire" $
        withDocs 2 $ \loop -> do
          let hdr fp = (hXHaveDigest, encodeUtf8 (renderHaveDigest (encodeGcs fp [("Doc:d01", "e1")])))
          rIn <- oneshotPost loop "pub" [hdr 10] docTitleInline
          rawStatus rIn `shouldBe` 200
          unchangedOf rIn `shouldBe` [(Ref "Doc" "d01", "e1")]
          rOut <- oneshotPost loop "pub" [hdr 7] docTitleInline
          rawStatus rOut `shouldBe` 200
          unchangedOf rOut `shouldBe` []
          map erId (entityRecordsOf rOut) `shouldBe` [Ref "Doc" "d01"]

    describe "the client advertises its store and applies the markers" $ do
      it "an empty store advertises nothing; at <= 32 entries the form is enumerated X-Have" $
        withDocs 2 $ \loop ->
          clientFor loop privClient $ \lc -> do
            r1 <- runQuery lc docBothText (idVar "d01")
            docFields r1 >>= (`shouldBe` ("Title d01", Just "Secret d01"))
            cold <- privHits loop
            cold `shouldNotSatisfy` null
            mapM_ (\h -> advertisementOf h `shouldBe` (Nothing, Nothing)) cold
            resetHits loop
            -- Second query: the store holds Doc:d01@e1 and says so; the
            -- origin answers the priv slice with a marker, and the
            -- store's copy (kept, marked fresh) still assembles whole.
            r2 <- runQuery lc docBothText (idVar "d01")
            docFields r2 >>= (`shouldBe` ("Title d01", Just "Secret d01"))
            unchangedRecords r2 `shouldBe` [(Ref "Doc" "d01", "e1")]
            warm <- privHits loop
            warm `shouldNotSatisfy` null
            mapM_ (\h -> advertisementOf h `shouldSatisfy` enumeratedNaming "Doc:d01@e1") warm

      it "above 32 store entries the advertisement switches to the fp=10 GCS digest" $
        withDocs 40 $ \loop ->
          clientFor loop privClient $ \lc -> do
            populate lc 40
            resetHits loop
            r <- runQuery lc docBothText (idVar "d01")
            docFields r >>= (`shouldBe` ("Title d01", Just "Secret d01"))
            -- The held entity still comes back as a marker (a GCS has no
            -- false negatives), satisfied from the store.
            unchangedRecords r `shouldBe` [(Ref "Doc" "d01", "e1")]
            hs <- privHits loop
            hs `shouldNotSatisfy` null
            let digests = mapMaybe (snd . advertisementOf) hs
            mapM_ (\h -> fst (advertisementOf h) `shouldBe` Nothing) hs
            -- Cross-check the wire bytes: every advertised digest
            -- decodes and contains every one of the 40 store members.
            length digests `shouldBe` length hs
            ds <- maybe (expectationFailure "X-Have-Digest failed to parse") pure (traverse (parseHaveDigest . decodeUtf8) digests)
            mapM_ (\d -> mapM_ (\i -> digestContains d ("Doc:" <> docKey i) "e1" `shouldBe` True) [1 .. 40]) ds

      it "a GCS false positive elides an entity the store lacks; the client repairs it with a point fetch" $
        withDocs 40 $ \loop ->
          clientFor loop privClient $ \lc -> do
            populate lc 40
            -- Mine a fresh key that the client's forthcoming digest
            -- wrongly contains, and seed it server-side (first write =
            -- ver e1, the ver the mining assumed).
            let k = minedKey 40
            atomically $
              putRow (loopDb loop) "Doc" k $
                Map.fromList
                  [ ("id", A.String k)
                  , ("title", A.String ("Title " <> k))
                  , ("secret", A.String ("Secret " <> k))
                  ]
            resetHits loop
            r <- runQuery lc docSecretText (idVar k)
            -- The false positive fired on the wire …
            unchangedRecords r `shouldBe` [(Ref "Doc" k, "e1")]
            -- … and the repair point fetch filled the gap before the
            -- tree assembled: the selected field is present and real.
            rootValue "doc" r >>= textField "secret" >>= (`shouldBe` ("Secret " <> k))
            repairs <- targetHits ("/e/Doc/" <> encodeUtf8 k) loop
            map hitStatus repairs `shouldBe` [200]
            gaps <- atomically (unchangedGaps (clientStore lc))
            gaps `shouldBe` Set.empty
            ents <- atomically (snapshotEntities (clientStore lc))
            fmap fst (Map.lookup (Ref "Doc" k) ents) `shouldBe` Just "e1"


-- ---------------------------------------------------------------------------
-- Fixture: docs with a public title and a private secret
-- ---------------------------------------------------------------------------

withDocs :: Int -> (Loop -> IO a) -> IO a
withDocs n action =
  withLoop
    (loopSpec digestSchema)
      { lsHooks = docHooks
      , lsRows = map docRow [1 .. n]
      }
    action
  where
    docRow i =
      ( "Doc"
      , Map.fromList
          [ ("id", A.String (docKey i))
          , ("title", A.String ("Title " <> docKey i))
          , ("secret", A.String ("Secret " <> docKey i))
          ]
      )


docHooks :: MemoryHooks
docHooks = defaultHooks {mhGetRoots = Map.fromList [("doc", byIdRoot "Doc")]}
  where
    byIdRoot ty _db args = pure $ case Map.lookup "id" args of
      Just (A.String k) -> Just (Ref ty k)
      _ -> Nothing


-- | @d01@ … @d40@: zero-padded so the fixture reads uniformly.
docKey :: Int -> Text
docKey i = "d" <> (if i < 10 then "0" else "") <> T.pack (show i)


docBothText :: Text
docBothText = "query DocBoth($id: DocId) { doc(id: $id) { title secret } }"


-- | Selecting only the private field: the pub slice carries the root ref
-- but no entity record, so a priv-slice elision is the only carrier of
-- the entity — the shape that exercises the false-positive repair.
docSecretText :: Text
docSecretText = "query DocSecret($id: DocId) { doc(id: $id) { secret } }"


docTitleInline :: Text
docTitleInline = "query DocTitle { doc(id: \"d01\") { title } }"


-- | The 'docSecretText' selection with the key inline: the shape the raw
-- priv-slice requests speak (no variable binding on the URL).
docSecretInline :: Text
docSecretInline = "query DocSecretInline { doc(id: \"d01\") { secret } }"


idVar :: Text -> Map VarName A.Value
idVar k = Map.singleton "id" (A.String k)


privClient :: ClientConfig -> ClientConfig
privClient c = c {ccSchema = Just digestSchema, ccAuthorization = Just "Bearer w1"}


-- | Walk the whole fixture through one client so its store holds exactly
-- @Doc:d01\@e1 … Doc:dNN\@e1@.
populate :: LatticeClient -> Int -> IO ()
populate lc n =
  mapM_
    (\i -> runQuery lc docBothText (idVar (docKey i)) >>= docFields >>= (`shouldBe` ("Title " <> docKey i, Just ("Secret " <> docKey i))))
    [1 .. n]


{- | The first fresh key whose @(Doc:{key}, e1)@ member falls into the
digest the client advertises over the fully-populated store — the
engineered §10.4 false positive. Pure: for a fixed fixture the found key
is a constant (expected search depth at @fp=10@ is ~2^10 candidates; the
cap only guards against a broken digest).
-}
minedKey :: Int -> Text
minedKey n = go (1 :: Int)
  where
    advertised = DigestGcs (encodeGcs 10 (map (\i -> ("Doc:" <> docKey i, "e1")) [1 .. n]))
    go i
      | i > 200000 = error "minedKey: no GCS collision in 200k candidates; the digest is broken"
      | digestContains advertised ("Doc:" <> candidate) "e1" = candidate
      | otherwise = go (i + 1)
      where
        candidate = "x" <> T.pack (show i)


-- ---------------------------------------------------------------------------
-- Requests and assertions
-- ---------------------------------------------------------------------------

auth :: (HeaderName, BS8.ByteString)
auth = ("Authorization", "Bearer w1")


have :: [(Text, Text)] -> BS8.ByteString
have = encodeUtf8 . renderHave


introducePost :: Loop -> BS8.ByteString -> [(HeaderName, BS8.ByteString)] -> Text -> IO RawResp
introducePost loop slice extra body =
  httpRaw
    loop
    POST
    ("/q?intent=introduce&slice=" <> slice)
    (("Content-Type", queryMediaType) : extra)
    (Just (encodeUtf8 body))


oneshotPost :: Loop -> BS8.ByteString -> [(HeaderName, BS8.ByteString)] -> Text -> IO RawResp
oneshotPost loop slice extra body =
  httpRaw
    loop
    POST
    ("/q?intent=oneshot&slice=" <> slice)
    (("Content-Type", queryMediaType) : extra)
    (Just (encodeUtf8 body))


unchangedOf :: RawResp -> [(Ref, Text)]
unchangedOf r = mapMaybe pick (rawRecords r)
  where
    pick = \case
      RUnchanged ref v -> Just (ref, v)
      _ -> Nothing


entityRecordsOf :: RawResp -> [EntityRecord]
entityRecordsOf r = mapMaybe pick (rawRecords r)
  where
    pick = \case
      REntity er -> Just er
      _ -> Nothing


recordKind :: Record -> Text
recordKind = \case
  RManifest {} -> "manifest"
  REntity {} -> "entity"
  RTombstone {} -> "tombstone"
  RUnchanged {} -> "unchanged"
  REnd {} -> "end"
  _ -> "other"


unchangedRecords :: QueryResult -> [(Ref, Text)]
unchangedRecords r = mapMaybe pick (qrRecords r)
  where
    pick = \case
      RUnchanged ref v -> Just (ref, v)
      _ -> Nothing


-- | The priv-slice traffic since the last reset (the requests that carry
-- an advertisement).
privHits :: Loop -> IO [Hit]
privHits loop = filter (("slice=priv" `BS8.isInfixOf`) . hitTarget) <$> allHits loop


-- | What a request advertised: its @(X-Have, X-Have-Digest)@ values.
advertisementOf :: Hit -> (Maybe BS8.ByteString, Maybe BS8.ByteString)
advertisementOf h =
  (lookupHeader hXHave (hitReqHeaders h), lookupHeader hXHaveDigest (hitReqHeaders h))


-- | Enumerated advertisement naming the given member; no GCS header.
enumeratedNaming :: BS8.ByteString -> (Maybe BS8.ByteString, Maybe BS8.ByteString) -> Bool
enumeratedNaming member = \case
  (Just enumerated, Nothing) -> member `BS8.isInfixOf` enumerated
  _ -> False


-- | The denormalized @doc@ root's @(title, secret)@.
docFields :: QueryResult -> IO (Text, Maybe Text)
docFields r = do
  doc <- rootValue "doc" r
  title <- textField "title" doc
  secret <- case doc of
    A.Object o -> traverse asText (KM.lookup "secret" o)
    _ -> pure Nothing
  pure (title, secret)
