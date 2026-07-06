{- | The federation gateway (spec §18.3–§18.8): "Lattice.Gateway" fusing
two in-process upstream origins — zero sockets end to end.

Transport: each upstream is a real 'Origin' over "Lattice.Backend.Memory"
whose handler rides the gateway's @upTransport@ seam; gateway traffic
rides 'gatewayHandler' directly ('latticeClientOver' for client-shaped
operations, a raw in-process request builder for header assertions).
Every upstream request is captured, every 'beLoad' counted, every
'upMint' call recorded — the §18 assertions read those journals instead
of trusting the fused stream alone.

Fixture: a @posts@ owner (Post with a claims-gated @notes@, a get root,
a grouped list root, @renamePost@) and a @social@ upstream extending
@Post@ with @score@ and a @reactions@ edge (Reaction with a claims-gated
@note@, @addReaction@). The social origin SERVES a plain skeleton schema
(extensions cannot declare @fetch by@, so a standalone @extend@ block is
not servable); its @extend entity@ module IDL rides @upModuleIdl@ into
fusion — the pinned federation shape for extension upstreams.

Coverage:

* Wire (§18.4): every gateway-emitted entity\/tombstone\/elided\/
  unchanged\/error record carries @src@; extension fields ride the
  extending upstream's record with its OWN @ver@ (§18.1 per-module
  version pin); exactly ONE fused manifest and end frame the merged
  stream; @Lattice-Snapshot@ is the namespaced union; @Surrogate-Key@
  is the prefixed union, coarsened under the gateway's own §10.5
  budget (entity-level keys drop first).
* Planning (§18.3): N cross-upstream refs are ONE nodes load per
  (upstream, round) — loader journals show one batched 'beLoad' per
  upstream, never per-entity calls; a repeated fused query rides
  hash-form GETs at the upstream (introduce once).
* Degradation (§18.4): killing one upstream's loaders degrades exactly
  that upstream's entities — scoped error records forward with scope
  AND @src@; the other upstream's records arrive intact.
* Auth (§18.8): the gateway verifies the inbound proof once and
  re-mints per upstream with ONLY that upstream's declared claims —
  asserted from the 'upMint' journal AND the captured upstream
  request's @vc@ parameter; the claims-gated fields on both sides
  emit end-to-end (the upstream accepted the re-minted proof);
  @upServiceAuth@ headers ride every subquery.
* Mutations (§18.7): routed whole to the owner with @Idempotency-Key@
  forwarded untouched; replay dedupes AT THE UPSTREAM (one effect
  invocation, @Idempotency-Replayed@ surfaces through the gateway);
  the response streams back with src tags, prefixed keys, and a
  namespaced snapshot.
* Invalidation (§18.6): an upstream purge relays through the gateway's
  own bus and CDN hook with @upstream/@-prefixed keys; a social
  mutation through the gateway is observed as a prefixed purge —
  driven deterministically on the bus, no sleeps.
* Client store (§18.1): owner and extension records coexist per
  @(id, src)@ with per-src versions — an owner-side version bump
  keeps the extension fields in the denormalized tree; a src-less
  monolithic stream still merges exactly as before.
* The §18 design goal: the same query against the gateway and against
  a monolithic origin over the same fusion ('fuseBackends') yields
  identical per-ref field unions, root maps, and denormalized trees
  (modulo @$ver@) — a client cannot tell federated from monolithic by
  the record stream, only by the snapshot header's namespaces.
-}
module Test.Lattice.Gateway (tests) where

import Control.Concurrent.STM
import Control.Exception (finally)
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Maybe (isJust)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Lattice.Backend (Backend (..), CommitResult (..), MutationOutcome (..), WriteFact (..), internalError, upstreamUnavailable)
import Lattice.Backend.Memory (MemoryDb, MemoryHooks (..), defaultHooks, memoryBackend, newMemoryDb, putRow, snapshotToken)
import Lattice.Client (ClientConfig (..), LatticeClient, MutationResult (..), QueryResult (..), defaultClientConfig, latticeClientOver, mutate)
import Lattice.Gateway
import Lattice.Module (Fused (..), SchemaModule (..), fuseBackends, fuseModules)
import Lattice.Schema (Budgets (..), Schema, defaultBudgets)
import Lattice.Server (InvalEvent (..), Origin, OriginConfig (..), defaultLiveConfig, latticeHandler, newOrigin, publishPurge, subscribeInvalidations)
import Lattice.Server.Auth (ClaimsPayload (..), QueryAdmission (..), decodeClaims, encodeClaims, hmacProof, hmacVerifier)
import Lattice.Telemetry (noTelemetry)
import Lattice.Types
import Lattice.Wire (EntityRecord (..), Manifest (..), Record (..), SurrogateKey, hIdempotencyKey, hLatticeSnapshot, hSurrogateKey, hVcAuth, queryMediaType)
import Network.HTTP.Message (Request (..), Response (..), Scheme (..))
import Network.HTTP.Types.Body (Body (..))
import Network.HTTP.Types.Header (HeaderName, lookupHeader)
import Network.HTTP.Types.Method (Method (..))
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.Version qualified as V
import Test.Lattice.Fixtures (mustParseSchema, requireRight)
import Test.Lattice.Loop (RawResp (..), fieldByPrefix, io, objectField, rootValue, runQuery, seedRow, textField)
import Test.Syd


tests :: Spec
tests =
  describe "The federation gateway (§18.3-§18.8)" $ do
    describe "wire composition (§18.4)" $ do
      it "every emitted record carries src; extension fields ride the extending upstream's record" $
        withGw $ \gw -> do
          r <- gwOneshot gw seamQ
          rawStatus r `shouldBe` 200
          objs <- rawObjs r
          let srcTagged = filter (\o -> kindOf o `elem` map Just srcKinds) objs
          srcTagged `shouldSatisfy` (not . null)
          mapM_ (\o -> srcOf o `shouldSatisfy` maybe False (`elem` ["posts", "social"])) srcTagged
          -- Post:p1 appears once per contributing upstream, fields disjoint.
          ownerFields <- entityFields' "Post:p1" "posts" objs
          extFields <- entityFields' "Post:p1" "social" objs
          KM.member "title" ownerFields `shouldBe` True
          KM.member "score" ownerFields `shouldBe` False
          KM.member "score" extFields `shouldBe` True
          KM.member "title" extFields `shouldBe` False
          -- Reactions belong to social.
          let reactionSrcs = map srcOf (entityObjsOfType "Reaction:" objs)
          reactionSrcs `shouldSatisfy` (not . null)
          reactionSrcs `shouldSatisfy` all (== Just "social")

      it "exactly one fused manifest and one end frame the merged stream" $
        withGw $ \gw -> do
          objs <- gwOneshot gw seamQ >>= rawObjs
          length (filter ((== Just "manifest") . kindOf) objs) `shouldBe` 1
          length (filter ((== Just "end") . kindOf) objs) `shouldBe` 1

      it "Lattice-Snapshot is the namespaced union of upstream domains" $
        withGw $ \gw -> do
          r <- gwOneshot gw seamQ
          snap <- maybe (expectationFailure "no Lattice-Snapshot header") (pure . decodeUtf8) (rawHeader' hLatticeSnapshot r)
          snap `shouldSatisfy` T.isInfixOf "posts/main=\""
          snap `shouldSatisfy` T.isInfixOf "social/main=\""

      it "Surrogate-Key is the prefixed union of the subresponses' keys" $
        withGw $ \gw -> do
          r <- gwOneshot gw seamQ
          keys <- surrogateKeys r
          keys `shouldSatisfy` elem "posts/Post:p1"
          keys `shouldSatisfy` elem "social/Reaction:r1"
          -- No upstream entity key leaks unprefixed.
          keys `shouldSatisfy` all (\k -> not ("Post:" `T.isPrefixOf` k || "Reaction:" `T.isPrefixOf` k))

      it "a tight gateway budget coarsens the prefixed union (entity keys drop first)" $
        withGwBudgets defaultBudgets {maxSurrogateKeys = 2} $ \gw -> do
          r <- gwOneshot gw seamQ
          keys <- surrogateKeys r
          keys `shouldSatisfy` (not . null)
          keys `shouldSatisfy` all (\k -> not ("Post:" `T.isInfixOf` k || "Reaction:" `T.isInfixOf` k))

    describe "query planning across upstreams (§18.3)" $ do
      it "N cross-upstream refs are one nodes load per (upstream, round)" $
        withGw $ \gw -> do
          c <- gwClient gw
          _ <- runQuery c feedQ Map.empty
          -- Three acme posts: the owner list root loads them as one
          -- batch, and social's extension fields arrive through ONE
          -- nodes query batching all three keys — never per-entity.
          -- The extension upstream is reached through exactly ONE
          -- batched nodes load...
          postLoads "Post" (gfSocial gw) >>= (`shouldBe` [3])
          -- ...and the owner's loads (its root subquery plus the
          -- owner-side nodes fetch) each carry the full key set.
          ownerLoads <- postLoads "Post" (gfPosts gw)
          ownerLoads `shouldSatisfy` (not . null)
          ownerLoads `shouldSatisfy` all (== 3)

      it "a repeated fused query rides hash-form GETs at the upstreams (introduce once)" $
        withGw $ \gw -> do
          c1 <- gwClient gw
          _ <- runQuery c1 feedQ Map.empty
          resetUp (gfSocial gw)
          c2 <- gwClient gw
          _ <- runQuery c2 feedQ Map.empty
          hits <- upstreamQueryHits (gfSocial gw)
          hits `shouldSatisfy` (not . null)
          map requestMethod hits `shouldSatisfy` all (== GET)

    describe "scoped degradation (§18.4)" $ do
      it "one upstream's failure degrades exactly its entities, scope and src preserved" $
        withGw $ \gw -> do
          atomically (writeTVar (upFail (gfSocial gw)) True)
          r <- gwOneshot gw seamQ
          -- Partial degradation is the ordinary 207 Multi-Status.
          rawStatus r `shouldBe` 207
          objs <- rawObjs r
          -- The owner's record is intact.
          ownerFields <- entityFields' "Post:p1" "posts" objs
          KM.lookup "title" ownerFields `shouldBe` Just (A.String "First")
          -- No social entity arrived; its failure is a scoped error
          -- record forwarded with scope AND src.
          entityObjsOfType "Reaction:" objs `shouldBe` []
          -- The failed edge is never fabricated as data: an empty page
          -- would be indistinguishable from the real fact "no reactions".
          let claimsReactions o = case KM.lookup "fields" o of
                Just (A.Object f) -> any (\k -> "reactions(" `T.isPrefixOf` AK.toText k) (KM.keys f)
                _ -> False
          filter claimsReactions objs `shouldSatisfy` null
          let errs = filter ((== Just "error") . kindOf) objs
          errs `shouldSatisfy` (not . null)
          mapM_ (\o -> srcOf o `shouldBe` Just "social") errs
          errs `shouldSatisfy` any (KM.member "scope")

    describe "authorization across upstreams (§18.8)" $ do
      it "re-minting narrows the verified inbound claims to each upstream's registry" $
        withGw $ \gw -> do
          c <- gwClaimsClient gw bothClaims
          res <- runQuery c gatedQ Map.empty
          -- End to end: both gated fields emitted, so both upstreams
          -- accepted their re-minted proofs.
          post <- rootValue "post" res
          textField "notes" post >>= (`shouldBe` "editorial")
          reactions <- fieldByPrefix "reactions(" post >>= objectField "items" >>= asArray'
          notes <- pure [v | A.Object o <- reactions, Just v <- [KM.lookup "note" o]]
          notes `shouldBe` [A.String "mine"]
          -- The mint journal: each upstream got ONLY its declared claims.
          postsMints <- readTVarIO (upMints (gfPosts gw))
          postsMints `shouldSatisfy` (not . null)
          postsMints `shouldSatisfy` all ((== ["org"]) . map fst)
          socialMints <- readTVarIO (upMints (gfSocial gw))
          socialMints `shouldSatisfy` (not . null)
          socialMints `shouldSatisfy` all ((== ["member"]) . map fst)
          -- The captured upstream requests: every presented vc decodes
          -- to exactly the narrowed payload.
          vcClaimSets (gfPosts gw) >>= (`shouldSatisfy` \sets -> sets /= [] && all (== [("org", A.String "acme")]) sets)
          vcClaimSets (gfSocial gw) >>= (`shouldSatisfy` \sets -> sets /= [] && all (== [("member", A.String "m7")]) sets)
          -- Every re-minted vc rides with its X-Vc-Auth proof header.
          vcReqs <- vcRequests (gfPosts gw)
          vcReqs `shouldSatisfy` (not . null)
          mapM_ (\rq -> lookupHeader hVcAuth (requestHeaders rq) `shouldSatisfy` maybe False (not . BS8.null)) vcReqs

      it "service-principal headers ride every upstream subquery" $
        withGw $ \gw -> do
          c <- gwClient gw
          _ <- runQuery c seamQ Map.empty
          postsQs <- upstreamQueryHits (gfPosts gw)
          postsQs `shouldSatisfy` (not . null)
          mapM_ (\rq -> lookupHeader "X-Service-Auth" (requestHeaders rq) `shouldBe` Just "gw:posts") postsQs
          socialQs <- upstreamQueryHits (gfSocial gw)
          socialQs `shouldSatisfy` (not . null)
          mapM_ (\rq -> lookupHeader "X-Service-Auth" (requestHeaders rq) `shouldBe` Just "gw:social") socialQs

    describe "mutations (§18.7)" $ do
      it "a mutation routes whole to its owner, Idempotency-Key untouched" $
        withGw $ \gw -> do
          c <- gwClient gw
          mr <- io "mutation" (mutate c "renamePost" renameArgs (Just "gw-idem-1")) >>= requireRight
          mrCommitted mr `shouldBe` True
          -- The owner saw the whole mutation with the key verbatim...
          posted <- mutationHits (gfPosts gw)
          keysSeen <- mapM (\rq -> pure (lookupHeader hIdempotencyKey (requestHeaders rq))) posted
          keysSeen `shouldBe` [Just "gw-idem-1"]
          -- ...and the other upstream saw no mutation at all.
          mutationHits (gfSocial gw) >>= (`shouldSatisfy` null)

      it "an idempotent replay dedupes at the upstream, once-only effect" $
        withGw $ \gw -> do
          c <- gwClient gw
          m1 <- io "mutation" (mutate c "renamePost" renameArgs (Just "gw-idem-2")) >>= requireRight
          m2 <- io "mutation" (mutate c "renamePost" renameArgs (Just "gw-idem-2")) >>= requireRight
          mrReplayed m1 `shouldBe` False
          mrReplayed m2 `shouldBe` True
          readTVarIO (upEffects (gfPosts gw)) >>= (`shouldBe` 1)

      it "the mutation response streams back src-tagged with prefixed keys and namespaced snapshot" $
        withGw $ \gw -> do
          r <- gwRaw gw POST "/m/renamePost" [("Content-Type", "application/json")] (Just (BS8.toStrict (A.encode renameArgs)))
          rawStatus r `shouldBe` 200
          keys <- surrogateKeys r
          keys `shouldSatisfy` elem "posts/Post:p1"
          snap <- maybe (expectationFailure "no Lattice-Snapshot header") (pure . decodeUtf8) (rawHeader' hLatticeSnapshot r)
          snap `shouldSatisfy` T.isInfixOf "posts/main=\""
          objs <- rawObjs r
          let ents = filter ((== Just "entity") . kindOf) objs
          ents `shouldSatisfy` (not . null)
          mapM_ (\o -> srcOf o `shouldBe` Just "posts") ents
          -- Any invalidated record carries prefixed keys.
          let invKeys = [k | o <- objs, kindOf o == Just "invalidated", Just (A.Array ks) <- [KM.lookup "keys" o], A.String k <- foldr (:) [] ks]
          invKeys `shouldSatisfy` all ("posts/" `T.isPrefixOf`)

    describe "invalidation across layers (§18.6)" $ do
      it "an upstream purge relays onto the gateway bus with prefixed keys" $
        withGw $ \gw -> do
          rx <- subscribeInvalidations (gatewayOrigin (gfGateway gw))
          publishPurge (upOrigin (gfSocial gw)) ["Reaction:r1", "boards:acme"]
          ev <- io "the gateway purge relay" (atomically rx)
          -- Prefixed for the CDN tier, raw alongside so the gateway's own
          -- §12 live-query subscriptions (registered unprefixed) compose.
          ieKeys ev `shouldSatisfy` \ks ->
            all (`elem` ks) ["social/Reaction:r1", "social/boards:acme"]
          -- The gateway's own CDN hook got the same translated keys.
          io "the gateway purge hook" . atomically $ do
            purges <- readTVar (gfPurges gw)
            check (any (\ks -> all (`elem` ks) ["social/Reaction:r1", "social/boards:acme"]) purges)

      it "a social mutation through the gateway purges with social/-prefixed keys" $
        withGw $ \gw -> do
          rx <- subscribeInvalidations (gatewayOrigin (gfGateway gw))
          c <- gwClient gw
          mr <- io "mutation" (mutate c "addReaction" addReactionArgs Nothing) >>= requireRight
          mrCommitted mr `shouldBe` True
          let expected = "social/Reaction:r9"
              drain = do
                ev <- io "a relayed purge" (atomically rx)
                if expected `elem` ieKeys ev then pure (ieKeys ev) else drain
          keys <- drain
          keys `shouldSatisfy` elem expected

    describe "discovery and the fused IDL" $ do
      it "the gateway serves the fused IDL content-addressed; a monolithic fusion serves the identical text" $
        withGw $ \gw -> do
          r <- getIdl (gatewayHandler (gfGateway gw))
          rawBody r `shouldBe` encodeUtf8 (fusedIdl (gatewayFused (gfGateway gw)))
          mono <- mkMonolith
          rm <- getIdl (monoHandler mono)
          rawBody rm `shouldBe` rawBody r

    describe "the client store (§18.1)" $ do
      it "owner and extension records coexist per (id, src) with per-src versions" $
        withGw $ \gw -> do
          c <- gwClient gw
          r1 <- runQuery c seamQ Map.empty
          (vPosts1, vSocial1) <- postVers r1
          _ <- io "mutation" (mutate c "renamePost" renameArgs Nothing) >>= requireRight
          c2 <- gwClient gw
          r2 <- runQuery c2 seamQ Map.empty
          (vPosts2, vSocial2) <- postVers r2
          vPosts2 `shouldNotBe` vPosts1
          vSocial2 `shouldBe` vSocial1

      it "an owner-side version bump keeps the extension fields merged in the tree" $
        withGw $ \gw -> do
          c <- gwClient gw
          _ <- runQuery c seamQ Map.empty
          _ <- io "mutation" (mutate c "renamePost" renameArgs Nothing) >>= requireRight
          r2 <- runQuery c seamQ Map.empty
          post <- rootValue "post" r2
          textField "title" post >>= (`shouldBe` "Renamed")
          score <- objectField "score" post
          score `shouldBe` A.Number 3

      it "a src-less monolithic stream still merges as before" $ do
        mono <- mkMonolith
        c <- monoClient mono Nothing
        res <- runQuery c seamQ Map.empty
        [(mSrc, _, _)] <- pure (entityRecs "Post" "p1" res)
        mSrc `shouldBe` Nothing
        post <- rootValue "post" res
        textField "title" post >>= (`shouldBe` "First")
        objectField "score" post >>= (`shouldBe` A.Number 3)

    describe "the §18 design goal" $ do
      it "a client cannot tell federated from monolithic modulo src and snapshot namespacing" $
        withGw $ \gw -> do
          mono <- mkMonolith
          gwC <- gwClaimsClient gw bothClaims
          monoC <- monoClient mono (Just bothClaims)
          gwRes <- runQuery gwC gatedQ Map.empty
          monoRes <- runQuery monoC gatedQ Map.empty
          qrErrors gwRes `shouldBe` []
          qrErrors monoRes `shouldBe` []
          -- Identical record SETS modulo src and per-src vers: the
          -- per-ref field unions coincide exactly.
          fieldUnionByRef (qrRecords gwRes) `shouldBe` fieldUnionByRef (qrRecords monoRes)
          -- Identical root maps and denormalized trees (modulo $ver).
          mRoot (qrManifest gwRes) `shouldBe` mRoot (qrManifest monoRes)
          Map.map stripVer (qrData gwRes) `shouldBe` Map.map stripVer (qrData monoRes)

      it "only the snapshot header's namespaces give federation away" $
        withGw $ \gw -> do
          rGw <- gwOneshot gw seamQ
          gwSnap <- maybe (expectationFailure "no gateway snapshot header") (pure . decodeUtf8) (rawHeader' hLatticeSnapshot rGw)
          mono <- mkMonolith
          rMono <- handlerRaw (monoHandler mono) POST "/q" [("Content-Type", queryMediaType)] (Just (encodeUtf8 seamQ))
          monoSnap <- maybe (expectationFailure "no monolith snapshot header") (pure . decodeUtf8) (rawHeader' hLatticeSnapshot rMono)
          gwSnap `shouldSatisfy` T.isInfixOf "posts/main=\""
          monoSnap `shouldSatisfy` T.isPrefixOf "main=\""


-- ---------------------------------------------------------------------------
-- Fixture: a posts owner and a social extension upstream
-- ---------------------------------------------------------------------------

postsText :: Text
postsText =
  T.unlines
    [ "schema posts.example.com"
    , ""
    , "newtype PostId = Text"
    , "newtype OrgId  = Text"
    , ""
    , "claims {"
    , "  org: OrgId"
    , "}"
    , ""
    , "entity Post by id {"
    , "  visible to all by default"
    , ""
    , "  id:    PostId"
    , "  title: Text"
    , "  orgId: OrgId"
    , "  notes: Text?  visible when caller.org = orgId"
    , ""
    , "  fetch by id: public"
    , "}"
    , ""
    , "get post(id: PostId) of Post public"
    , ""
    , "list postsByOrg of Post by orgId"
    , "     ordered by title asc"
    , "     page 10 max 50"
    , "     public"
    , ""
    , "mutation renamePost(post: PostId, title: Text) returns Post {"
    , "  allow       public"
    , "  writes      Post(post)"
    , "  invalidates writes"
    , "  effect      transactional"
    , "}"
    ]


-- | What the social origin SERVES: a plain skeleton in which the
-- extended @Post@ is an ordinary fetchable entity carrying only the
-- extension members (extensions cannot declare @fetch by@, so the
-- @extend@ form itself is not servable standalone).
socialServedText :: Text
socialServedText =
  T.unlines (socialCommonPrefix <> socialSkeletonPost <> socialCommonSuffix)


-- | What social contributes to FUSION: the same document with the
-- skeleton replaced by the @extend entity@ block (rides 'upModuleIdl').
socialModuleText :: Text
socialModuleText =
  T.unlines (socialCommonPrefix <> socialExtendPost <> socialCommonSuffix)


socialCommonPrefix :: [Text]
socialCommonPrefix =
  [ "schema social.example.com"
  , ""
  , "newtype PostId     = Text"
  , "newtype ReactionId = Text"
  , "newtype MemberId   = Text"
  , ""
  , "claims {"
  , "  member: MemberId"
  , "}"
  , ""
  , "entity Reaction by id {"
  , "  visible to all by default"
  , ""
  , "  id:      ReactionId"
  , "  postId:  PostId"
  , "  kind:    Text"
  , "  ownerId: MemberId"
  , "  note:    Text?  visible when caller.member = ownerId"
  , ""
  , "  fetch by id: public"
  , "}"
  , ""
  ]


socialSkeletonPost :: [Text]
socialSkeletonPost =
  [ "entity Post by id {"
  , "  visible to all by default"
  , ""
  , "  id:    PostId"
  , "  score: I32"
  , ""
  , "  has many reactions: Reaction by postId"
  , "                      ordered by kind asc"
  , "                      page 10 max 50"
  , ""
  , "  fetch by id: public"
  , "}"
  ]


socialExtendPost :: [Text]
socialExtendPost =
  [ "extend entity Post {"
  , "  score: I32"
  , ""
  , "  has many reactions: Reaction by postId"
  , "                      ordered by kind asc"
  , "                      page 10 max 50"
  , "}"
  ]


socialCommonSuffix :: [Text]
socialCommonSuffix =
  [ ""
  , "mutation addReaction(reaction: ReactionId, post: PostId, kind: Text) returns Reaction {"
  , "  allow       public"
  , "  writes      Reaction(reaction)"
  , "  invalidates writes"
  , "  effect      transactional"
  , "}"
  ]


postsSchema :: Schema
postsSchema = mustParseSchema postsText
{-# NOINLINE postsSchema #-}


socialServedSchema :: Schema
socialServedSchema = mustParseSchema socialServedText
{-# NOINLINE socialServedSchema #-}


socialModuleSchema :: Schema
socialModuleSchema = mustParseSchema socialModuleText
{-# NOINLINE socialModuleSchema #-}


postsRows :: [(TypeName, Map FieldName A.Value)]
postsRows =
  [ ("Post", Map.fromList [("id", A.String "p1"), ("title", A.String "First"), ("orgId", A.String "acme"), ("notes", A.String "editorial")])
  , ("Post", Map.fromList [("id", A.String "p2"), ("title", A.String "Second"), ("orgId", A.String "acme")])
  , ("Post", Map.fromList [("id", A.String "p3"), ("title", A.String "Third"), ("orgId", A.String "acme")])
  ]


socialRows :: [(TypeName, Map FieldName A.Value)]
socialRows =
  [ ("Post", Map.fromList [("id", A.String "p1"), ("score", A.Number 3)])
  , ("Post", Map.fromList [("id", A.String "p2"), ("score", A.Number 1)])
  , ("Post", Map.fromList [("id", A.String "p3"), ("score", A.Number 0)])
  , ("Reaction", Map.fromList [("id", A.String "r1"), ("postId", A.String "p1"), ("kind", A.String "clap"), ("ownerId", A.String "m7"), ("note", A.String "mine")])
  , ("Reaction", Map.fromList [("id", A.String "r2"), ("postId", A.String "p1"), ("kind", A.String "like"), ("ownerId", A.String "m9"), ("note", A.String "other")])
  , ("Reaction", Map.fromList [("id", A.String "r3"), ("postId", A.String "p2"), ("kind", A.String "wow"), ("ownerId", A.String "m7"), ("note", A.String "also")])
  ]


postsHooksFor :: TVar Int -> MemoryHooks
postsHooksFor effects =
  defaultHooks
    { mhGetRoots =
        Map.fromList
          [ ( "post"
            , \_db args -> pure $ case Map.lookup "id" args of
                Just (A.String key) -> Just (Ref "Post" key)
                _ -> Nothing
            )
          ]
    , mhMutations = Map.fromList [("renamePost", renameEffect effects)]
    }


socialHooksFor :: TVar Int -> MemoryHooks
socialHooksFor effects =
  defaultHooks {mhMutations = Map.fromList [("addReaction", addReactionEffect effects)]}


renameEffect :: TVar Int -> MemoryDb -> Claims -> Map ArgName A.Value -> IO MutationOutcome
renameEffect effects db _claims args =
  case (Map.lookup "post" args, Map.lookup "title" args) of
    (Just (A.String pid), Just title@(A.String _)) -> atomically $ do
      modifyTVar' effects (+ 1)
      putRow db "Post" pid (Map.fromList [("id", A.String pid), ("title", title), ("orgId", A.String "acme")])
      tok <- snapshotToken db
      pure . MutationCommitted $
        CommitResult
          { crResult = [Ref "Post" pid]
          , crWrites = [WroteEntity (Ref "Post" pid)]
          , crSnapshot = tok
          }
    _ -> pure (MutationFailed (internalError (Just "renamePost: post and title arguments required")))


addReactionEffect :: TVar Int -> MemoryDb -> Claims -> Map ArgName A.Value -> IO MutationOutcome
addReactionEffect effects db _claims args =
  case (Map.lookup "reaction" args, Map.lookup "post" args, Map.lookup "kind" args) of
    (Just (A.String rid), Just pid@(A.String _), Just kind@(A.String _)) -> atomically $ do
      modifyTVar' effects (+ 1)
      putRow db "Reaction" rid (Map.fromList [("id", A.String rid), ("postId", pid), ("kind", kind), ("ownerId", A.String "m7")])
      tok <- snapshotToken db
      pure . MutationCommitted $
        CommitResult
          { crResult = [Ref "Reaction" rid]
          , crWrites = [WroteEntity (Ref "Reaction" rid)]
          , crSnapshot = tok
          }
    _ -> pure (MutationFailed (internalError (Just "addReaction: reaction, post, and kind arguments required")))


renameArgs :: A.Value
renameArgs = A.object [("post", A.String "p1"), ("title", A.String "Renamed")]


addReactionArgs :: A.Value
addReactionArgs = A.object [("reaction", A.String "r9"), ("post", A.String "p1"), ("kind", A.String "fire")]


postsSecret, socialSecret, gwSecret :: ByteString
postsSecret = "posts-secret"
socialSecret = "social-secret"
gwSecret = "gateway-secret"


bothClaims :: Claims
bothClaims = Map.fromList [("org", A.String "acme"), ("member", A.String "m7")]


-- ---------------------------------------------------------------------------
-- Queries
-- ---------------------------------------------------------------------------

seamQ :: Text
seamQ = "query Seam { post(id: \"p1\") { title score reactions(first: 5) { kind } } }"


gatedQ :: Text
gatedQ = "query Gated { post(id: \"p1\") { title notes score reactions(first: 5) { kind note } } }"


feedQ :: Text
feedQ = "query Feed { postsByOrg(orgId: \"acme\") { title score } }"


-- ---------------------------------------------------------------------------
-- Upstream fixtures: in-process origins with request/load/mint journals
-- ---------------------------------------------------------------------------

data Up = Up
  { upOrigin :: Origin
  , upReqs :: TVar [Request]
  -- ^ Captured requests, newest first.
  , upFail :: TVar Bool
  -- ^ Loader fault injection ('upstreamUnavailable' on every loader).
  , upLoads :: TVar [(TypeName, Int)]
  -- ^ @(type, batch size)@ per 'beLoad', newest first.
  , upEffects :: TVar Int
  -- ^ Mutation effect invocations (idempotent replays do not count).
  , upMints :: TVar [[(ClaimName, A.Value)]]
  -- ^ 'upMint' invocations, newest first.
  }


mkUp :: Schema -> ByteString -> [(TypeName, Map FieldName A.Value)] -> (TVar Int -> MemoryHooks) -> IO Up
mkUp schema secret rows mkHooks = do
  db <- newMemoryDb
  atomically (mapM_ (seedRow schema db) rows)
  reqs <- newTVarIO []
  failV <- newTVarIO False
  loads <- newTVarIO []
  effects <- newTVarIO 0
  mints <- newTVarIO []
  let hooks =
        (mkHooks effects)
          { mhFailures = (\broken -> if broken then Just upstreamUnavailable else Nothing) <$> readTVarIO failV
          }
      inner = memoryBackend schema db hooks
      backend =
        inner
          { beLoad = \ty keys -> do
              atomically (modifyTVar' loads ((ty, length keys) :))
              beLoad inner ty keys
          }
  origin <-
    newOrigin
      OriginConfig
        { ocSchema = schema
        , ocBudgets = defaultBudgets
        , ocBackend = backend
        , ocVerifier = Just (hmacVerifier secret getPOSIXTime)
        , ocSnapshotDomain = "main"
        , ocPurge = const (pure ())
        , ocCors = False
        , ocNow = getPOSIXTime
        , ocAdmission = AdmitOpen
        , ocCoalesce = Nothing
        , ocRegistry = Nothing
        , ocLive = defaultLiveConfig
        , ocTelemetry = noTelemetry
        }
  pure
    Up
      { upOrigin = origin
      , upReqs = reqs
      , upFail = failV
      , upLoads = loads
      , upEffects = effects
      , upMints = mints
      }


-- | The zero-socket transport seam: capture, then answer in-process.
upSend :: Up -> Request -> IO Response
upSend up req = do
  atomically (modifyTVar' (upReqs up) (req :))
  latticeHandler (upOrigin up) req


resetUp :: Up -> IO ()
resetUp up = atomically $ do
  writeTVar (upReqs up) []
  writeTVar (upLoads up) []


-- | Chronological @(batch size)@ journal of one type's 'beLoad' calls.
postLoads :: TypeName -> Up -> IO [Int]
postLoads ty up = map snd . filter ((== ty) . fst) . reverse <$> readTVarIO (upLoads up)


-- | Chronological captured @/q@ requests.
upstreamQueryHits :: Up -> IO [Request]
upstreamQueryHits up =
  filter (\rq -> "/q" `BS8.isPrefixOf` requestTarget rq) . reverse <$> readTVarIO (upReqs up)


-- | Chronological captured @/m/@ requests.
mutationHits :: Up -> IO [Request]
mutationHits up =
  filter (\rq -> "/m/" `BS8.isPrefixOf` requestTarget rq) . reverse <$> readTVarIO (upReqs up)


-- | The captured requests that presented a @vc@ credential.
vcRequests :: Up -> IO [Request]
vcRequests up =
  filter (isJust . targetParam "vc" . requestTarget) . reverse <$> readTVarIO (upReqs up)


-- | Every distinct claims payload presented to this upstream via the
-- @vc@ parameter, decoded.
vcClaimSets :: Up -> IO [[(ClaimName, A.Value)]]
vcClaimSets up = do
  reqs <- reverse <$> readTVarIO (upReqs up)
  let vcs = [vc | rq <- reqs, Just vc <- [targetParam "vc" (requestTarget rq)]]
  mapM decode1 vcs
  where
    decode1 vc = case decodeClaims (decodeUtf8 vc) of
      Right p -> pure (Map.toList (cpClaims p))
      Left err -> expectationFailure ("a captured upstream vc failed to decode: " <> show err)


-- | A mint hook recording its narrowing input, minting with the
-- upstream's own secret (the upstream's verifier must accept it).
mintFor :: ByteString -> TVar [[(ClaimName, A.Value)]] -> [(ClaimName, A.Value)] -> IO (Text, Text)
mintFor secret journal claims = do
  atomically (modifyTVar' journal (claims :))
  now <- getPOSIXTime
  let payload = encodeClaims (Map.fromList claims)
  pure (payload, hmacProof secret payload (floor now + 300))


-- ---------------------------------------------------------------------------
-- The gateway fixture
-- ---------------------------------------------------------------------------

data GwFix = GwFix
  { gfGateway :: Gateway
  , gfPosts :: Up
  , gfSocial :: Up
  , gfPurges :: TVar [[SurrogateKey]]
  }


withGw :: (GwFix -> IO a) -> IO a
withGw = withGwBudgets defaultBudgets


withGwBudgets :: Budgets -> (GwFix -> IO a) -> IO a
withGwBudgets budgets k = do
  posts <- mkUp postsSchema postsSecret postsRows postsHooksFor
  social <- mkUp socialServedSchema socialSecret socialRows socialHooksFor
  purges <- newTVarIO []
  gwE <-
    newGateway
      GatewayConfig
        { gwUpstreams =
            NE.fromList
              [ Upstream
                  { upName = "posts"
                  , upBase = "http://posts.internal"
                  , upClaims = ["org"]
                  , upMint = mintFor postsSecret (upMints posts)
                  , upServiceAuth = [("X-Service-Auth", "gw:posts")]
                  , upTransport = Just (upSend posts)
                  , upModuleIdl = Nothing
                  }
              , Upstream
                  { upName = "social"
                  , upBase = "http://social.internal"
                  , upClaims = ["member"]
                  , upMint = mintFor socialSecret (upMints social)
                  , upServiceAuth = [("X-Service-Auth", "gw:social")]
                  , upTransport = Just (upSend social)
                  , upModuleIdl = Just socialModuleText
                  }
              ]
        , gwVerifier = Just (hmacVerifier gwSecret getPOSIXTime)
        , gwBudgets = budgets
        , gwPurge = \keys -> atomically (modifyTVar' purges (keys :))
        , gwOnResync = \_ -> pure ()
        , gwNow = getPOSIXTime
        }
  gw <- requireRight gwE
  -- The fixture journals record gateway startup traffic (schema
  -- fetches, feed subscriptions); tests assert on per-test traffic.
  resetUp posts
  resetUp social
  k GwFix {gfGateway = gw, gfPosts = posts, gfSocial = social, gfPurges = purges}
    `finally` shutdownGateway gw


-- | A store-owning client over the gateway handler, no credentials.
gwClient :: GwFix -> IO LatticeClient
gwClient gw =
  latticeClientOver
    defaultClientConfig {ccSchema = Just (fusedSchema (gatewayFused (gfGateway gw)))}
    (gatewayHandler (gfGateway gw))


-- | Same, presenting claims under the GATEWAY's verifier.
gwClaimsClient :: GwFix -> Claims -> IO LatticeClient
gwClaimsClient gw claims = do
  proof <- proofWith gwSecret claims
  latticeClientOver
    defaultClientConfig
      { ccSchema = Just (fusedSchema (gatewayFused (gfGateway gw)))
      , ccClaims = Just (claims, proof)
      }
    (gatewayHandler (gfGateway gw))


proofWith :: ByteString -> Claims -> IO Text
proofWith secret claims = do
  now <- getPOSIXTime
  pure (hmacProof secret (encodeClaims claims) (floor now + 300))


-- ---------------------------------------------------------------------------
-- The monolithic control: the same fusion served in one process
-- ---------------------------------------------------------------------------

newtype Monolith = Monolith {monoHandler :: Request -> IO Response}


-- | The §18 control arm: 'fuseModules' over the identical module texts,
-- 'fuseBackends' over identically seeded per-module stores, one origin.
mkMonolith :: IO Monolith
mkMonolith = do
  fused <-
    requireRight
      ( fuseModules
          ( NE.fromList
              [ SchemaModule {smName = "posts", smIdl = postsText}
              , SchemaModule {smName = "social", smIdl = socialModuleText}
              ]
          )
      )
  postsDb <- newMemoryDb
  socialDb <- newMemoryDb
  atomically (mapM_ (seedRow postsSchema postsDb) postsRows)
  atomically (mapM_ (seedRow socialServedSchema socialDb) socialRows)
  postsEffects <- newTVarIO 0
  socialEffects <- newTVarIO 0
  let postsB = memoryBackend postsSchema postsDb (postsHooksFor postsEffects)
      socialB = memoryBackend socialModuleSchema socialDb (socialHooksFor socialEffects)
      fusedB = fuseBackends (Map.fromList [("posts", postsB), ("social", socialB)]) fused
  origin <-
    newOrigin
      OriginConfig
        { ocSchema = fusedSchema fused
        , ocBudgets = defaultBudgets
        , ocBackend = fusedB
        , ocVerifier = Just (hmacVerifier gwSecret getPOSIXTime)
        , ocSnapshotDomain = "main"
        , ocPurge = const (pure ())
        , ocCors = False
        , ocNow = getPOSIXTime
        , ocAdmission = AdmitOpen
        , ocCoalesce = Nothing
        , ocRegistry = Nothing
        , ocLive = defaultLiveConfig
        , ocTelemetry = noTelemetry
        }
  pure (Monolith (latticeHandler origin))


monoClient :: Monolith -> Maybe Claims -> IO LatticeClient
monoClient mono mClaims = do
  creds <- traverse (\cl -> (,) cl <$> proofWith gwSecret cl) mClaims
  latticeClientOver
    defaultClientConfig {ccSchema = Just monoSchema, ccClaims = creds}
    (monoHandler mono)
  where
    monoSchema =
      either (error . show) fusedSchema $
        fuseModules
          ( NE.fromList
              [ SchemaModule {smName = "posts", smIdl = postsText}
              , SchemaModule {smName = "social", smIdl = socialModuleText}
              ]
          )


-- ---------------------------------------------------------------------------
-- In-process raw requests
-- ---------------------------------------------------------------------------

handlerRaw :: (Request -> IO Response) -> Method -> ByteString -> [(HeaderName, ByteString)] -> Maybe ByteString -> IO RawResp
handlerRaw h method target headers mBody = do
  resp <-
    io
      "an in-process response"
      ( h
          Request
            { requestMethod = method
            , requestTarget = target
            , requestAuthority = Just "gateway.test"
            , requestScheme = SchemeHttp
            , requestHeaders = headers
            , requestBody = maybe BodyEmpty BodyBytes mBody
            , requestVersion = V.HTTP1_1
            , requestTrailers = pure []
            }
      )
  body <- drainAll (responseBody resp)
  pure
    RawResp
      { rawStatus = fromIntegral (statusCode (responseStatus resp))
      , rawHeaders = responseHeaders resp
      , rawBody = body
      }


drainAll :: Body -> IO ByteString
drainAll = \case
  BodyEmpty -> pure BS8.empty
  BodyBytes bs -> pure bs
  BodyStream pop -> go []
    where
      go acc =
        pop >>= \case
          Nothing -> pure (BS8.concat (reverse acc))
          Just c -> go (c : acc)


gwRaw :: GwFix -> Method -> ByteString -> [(HeaderName, ByteString)] -> Maybe ByteString -> IO RawResp
gwRaw gw = handlerRaw (gatewayHandler (gfGateway gw))


-- | GET the served IDL, following the @/schema/current@ →
-- @/schema/{hash}@ content-addressing redirect.
getIdl :: (Request -> IO Response) -> IO RawResp
getIdl h = do
  r <- handlerRaw h GET "/schema/current" [] Nothing
  case (rawStatus r, lookupHeader "Location" (rawHeaders r)) of
    (307, Just loc) -> do
      r' <- handlerRaw h GET loc [] Nothing
      rawStatus r' `shouldBe` 200
      pure r'
    (200, _) -> pure r
    other -> expectationFailure ("unexpected schema/current answer: " <> show (fst other))


-- | One-shot POST /q against the gateway (public-slice stream).
gwOneshot :: GwFix -> Text -> IO RawResp
gwOneshot gw q = gwRaw gw POST "/q" [("Content-Type", queryMediaType)] (Just (encodeUtf8 q))


rawHeader' :: HeaderName -> RawResp -> Maybe ByteString
rawHeader' name r = lookupHeader name (rawHeaders r)


surrogateKeys :: RawResp -> IO [Text]
surrogateKeys r =
  maybe
    (expectationFailure "no Surrogate-Key header")
    (pure . T.words . decodeUtf8)
    (rawHeader' hSurrogateKey r)


-- | The @name=value@ pairs of a request target's query string.
targetParam :: ByteString -> ByteString -> Maybe ByteString
targetParam name target = case BS8.break (== '?') target of
  (_, "") -> Nothing
  (_, qs) ->
    lookup name [BS8.drop 1 <$> BS8.break (== '=') kv | kv <- BS8.split '&' (BS8.drop 1 qs)]


-- ---------------------------------------------------------------------------
-- Raw NDJSON assertions
-- ---------------------------------------------------------------------------

-- | Every NDJSON line of a raw stream, decoded strictly.
rawObjs :: RawResp -> IO [KM.KeyMap A.Value]
rawObjs r = mapM dec (filter (not . BS8.null) (BS8.lines (rawBody r)))
  where
    dec ln = case A.decodeStrict ln of
      Just (A.Object o) -> pure o
      _ -> expectationFailure ("undecodable NDJSON record: " <> show ln)


kindOf :: KM.KeyMap A.Value -> Maybe Text
kindOf o = case KM.lookup "kind" o of
  Just (A.String k) -> Just k
  _ -> Nothing


srcOf :: KM.KeyMap A.Value -> Maybe Text
srcOf o = case KM.lookup "src" o of
  Just (A.String s) -> Just s
  _ -> Nothing


-- | The record kinds the §18.4 contract tags with @src@.
srcKinds :: [Text]
srcKinds = ["entity", "tombstone", "elided", "unchanged", "error"]


entityObjsOfType :: Text -> [KM.KeyMap A.Value] -> [KM.KeyMap A.Value]
entityObjsOfType refPrefix objs =
  [ o
  | o <- objs
  , kindOf o == Just "entity"
  , Just (A.String rid) <- [KM.lookup "id" o]
  , refPrefix `T.isPrefixOf` rid
  ]


-- | The fields object of THE entity record for a ref from a src.
entityFields' :: Text -> Text -> [KM.KeyMap A.Value] -> IO (KM.KeyMap A.Value)
entityFields' rid src objs =
  case
    [ f
    | o <- objs
    , kindOf o == Just "entity"
    , KM.lookup "id" o == Just (A.String rid)
    , srcOf o == Just src
    , Just (A.Object f) <- [KM.lookup "fields" o]
    ] of
    [f] -> pure f
    other -> expectationFailure ("expected exactly one " <> T.unpack rid <> " record from " <> T.unpack src <> ", got " <> show (length other))


-- ---------------------------------------------------------------------------
-- Typed record and tree assertions
-- ---------------------------------------------------------------------------

-- | Every entity record for one entity: @(src, ver, fields)@.
entityRecs :: TypeName -> Text -> QueryResult -> [(Maybe Text, Text, Map Text A.Value)]
entityRecs ty key res =
  [(erSrc e, erVer e, erFields e) | REntity e <- qrRecords res, erId e == Ref ty key]


-- | The @(posts ver, social ver)@ pair of Post:p1's two records.
postVers :: QueryResult -> IO (Text, Text)
postVers res = do
  let recs = entityRecs "Post" "p1" res
  pv <- case [v | (Just "posts", v, _) <- recs] of
    [v] -> pure v
    other -> expectationFailure ("expected one posts-src Post:p1 record, got " <> show other)
  sv <- case [v | (Just "social", v, _) <- recs] of
    [v] -> pure v
    other -> expectationFailure ("expected one social-src Post:p1 record, got " <> show other)
  pure (pv, sv)


-- | Per-ref union of every entity record's fields, src and ver erased —
-- the §18 comparison currency between gateway and monolith.
fieldUnionByRef :: [Record] -> Map Ref (Map Text A.Value)
fieldUnionByRef recs =
  Map.fromListWith Map.union [(erId e, erFields e) | REntity e <- recs]


-- | Strip @$ver@ recursively: versions are per-src at the gateway and
-- owner-only at the monolith, deliberately (§18.1).
stripVer :: A.Value -> A.Value
stripVer = \case
  A.Object o -> A.Object (KM.map stripVer (KM.delete "$ver" o))
  A.Array xs -> A.Array (fmap stripVer xs)
  v -> v


asArray' :: A.Value -> IO [A.Value]
asArray' = \case
  A.Array xs -> pure (foldr (:) [] xs)
  v -> expectationFailure ("expected an array, got: " <> show v)
