{- | End-to-end loopback tests: a real origin ("Lattice.Server" over
"Lattice.Backend.Memory") on an ephemeral port, exercised through
"Lattice.Client" — the full HTTP round trip, asserted against the
protocol contracts (spec section numbers on each group).

Two fixture origins, both built in this module (the demo executable's
@example/StarWars.hs@ seed lives in the exe component and is not
importable from here):

* Star Wars (@test/fixtures/starwars.lattice@): @hero@ get root, a
  @friends@ children override, and the @createReview@ mutation — enough
  surface for the transport ladder, normalization, mutations, point
  fetches, degraded responses, and edge pagination.
* Blog (@test/fixtures/blog.lattice@): the claim-gated @feed@ root
  behind an HMAC 'ProofVerifier', plus a 51-tag post overflowing the
  bounded @Post.tags@ collection.
* Co-key (@test/fixtures/cokey.lattice@): spec §3.8 — @UserProfile
  joins User@ (adjacent truth), @AdminUser refines User@ (same truth),
  identity edges both ways, and mutations exercising family-fanout
  surrogate keys and the base-deletion tombstone cascade.
* Cardinality (the inline @cardText@ fixture, §3.4–§3.6): a dangling
  required @has one@, a floored bounded collection scanning short, a
  @[Text]+@ mutation argument and query variable, and a stored row
  violating its nonempty list type — every cardinality error path plus
  its clean positive control.

Every request is recorded by a counting middleware (method, target,
status, response @Cache-Control@), which makes ladder assertions exact:
an introduction is one failed GET plus one POST, a warm query is a
single GET.

Client-contract gaps noted while writing these tests (each is asserted
around, not skipped silently):

* 'QueryResult' exposes no response headers, so @Lattice-Snapshot@
  (§13.2) is asserted through a follow-up query and @Cache-Control@
  through the middleware log.
* Paginated __root__ collections do not transport boundary cursors (the
  manifest root map is @Map Text [Ref]@), so §3.6 cursor walking is
  exercised on the @friends@ edge, where the @$page@ value carries
  @next@.
* 'pointFetch' exposes no version pin, so the §6.7 @ver=@ form is not
  exercised from the client.
-}
module Test.Lattice.E2E (tests) where

import Control.Concurrent.STM
import Data.Aeson qualified as A
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Lattice.Backend
import Lattice.Backend.Memory
import Lattice.Client
import Lattice.Client.Store qualified as Store
import Lattice.Schema
import Lattice.Server.Auth (encodeClaims, hmacProof, hmacVerifier)
import Lattice.Types
import Lattice.Wire
import Network.HTTP.Types.Method (Method (..))
import Test.Lattice.Fixtures (blogSchema, cardSchema, cokeySchema, requireRight, starwarsSchema)
import Test.Lattice.Loop
import Test.Syd
import Text.Read (readMaybe)


tests :: Spec
tests =
  describe "End to end over loopback HTTP" $ do
    describe "§6.1/§6.3 transport ladder" $ do
      it "schema mode: cold hash GET 404s, introduction answers, a warm GET flows straight through" $
        withStarWars $ \loop -> do
          r1 <- schemaClient loop $ \lc -> do
            resetHits loop
            r1 <- runQuery lc heroQ Map.empty
            hs <- queryHits loop
            case hs of
              [h1, h2] -> do
                (hitMethod h1, hitStatus h1) `shouldBe` (GET, 404)
                (hitMethod h2, hitStatus h2) `shouldBe` (POST, 200)
              _ -> expectationFailure ("expected exactly cold GET + introduce POST, saw: " <> show hs)
            pure r1
          -- A fresh client against the now-warm origin: a single hash GET,
          -- same manifest etag, CDN-cacheable.
          schemaClient loop $ \lc -> do
            resetHits loop
            r2 <- runQuery lc heroQ Map.empty
            hs <- queryHits loop
            case hs of
              [h] -> do
                (hitMethod h, hitStatus h) `shouldBe` (GET, 200)
                hitCache h `shouldSatisfy` maybe False ("public" `BS8.isPrefixOf`)
              _ -> expectationFailure ("expected exactly one warm GET, saw: " <> show hs)
            mEtag (qrManifest r2) `shouldBe` mEtag (qrManifest r1)
            mSlice (qrManifest r2) `shouldBe` Just SlicePub

      it "schema-less mode: first contact introduces and learns the plan; repeats ride the hash form" $
        withStarWars $ \loop ->
          rawClient loop $ \lc -> do
            resetHits loop
            r1 <- runQuery lc heroNameQ Map.empty
            names1 <- rootValue "hero" r1 >>= asArray >>= traverse (textField "name")
            names1 `shouldBe` ["R2-D2"]
            hs1 <- queryHits loop
            map (\h -> (hitMethod h, hitStatus h)) hs1 `shouldBe` [(POST, 200), (GET, 200)]
            resetHits loop
            r2 <- runQuery lc heroNameQ Map.empty
            hs2 <- queryHits loop
            map (\h -> (hitMethod h, hitStatus h)) hs2 `shouldBe` [(GET, 200)]
            names2 <- rootValue "hero" r2 >>= asArray >>= traverse (textField "name")
            names2 `shouldBe` ["R2-D2"]

    describe "§9.1 normalized entity stream" $ do
      it "each entity appears once; tree and store denormalize from one response" $
        withStarWars $ \loop ->
          schemaClient loop $ \lc -> do
            r <- runQuery lc heroDeepQ Map.empty
            -- R2-D2 is the hero AND a friend of every human friend: still
            -- exactly one entity record in the stream.
            let isR2 = \case
                  REntity er -> erId er == Ref "Droid" "2001"
                  _ -> False
            length (filter isR2 (qrRecords r)) `shouldBe` 1
            hero <- rootValue "hero" r
            heroName <- textField "name" hero
            heroName `shouldBe` "R2-D2"
            friendsPage <- objectField "friends" hero
            names <- pageNames friendsPage
            names `shouldBe` ["Han Solo", "Leia Organa", "Luke Skywalker"]
            stored <- atomically (Store.lookupEntity (clientStore lc) (Ref "Human" "1000"))
            case stored of
              Just (_ver, fs) -> Map.lookup "name" fs `shouldBe` Just (A.String "Luke Skywalker")
              Nothing -> expectationFailure "Human:1000 did not normalize into the client store"

    describe "§11.2/§11.3 mutations and §13.2 read-your-writes" $ do
      it "createReview commits, invalidates, and is visible to the next query" $
        withStarWars $ \loop ->
          schemaClient loop $ \lc -> do
            mr <- runCreateReview lc reviewBody (Just "e2e-key-commit")
            -- §9.4.3 commit test: entity/invalidated records arrived.
            mrCommitted mr `shouldBe` True
            mrReplayed mr `shouldBe` False
            mrInvalidated mr `shouldSatisfy` elem "reviews:Jedi"
            mrInvalidated mr `shouldSatisfy` elem "Review:5006"
            resultRoot <- mutationResult mr
            starVals <- asArray resultRoot >>= traverse (objectField "stars")
            starVals `shouldBe` [A.Number 5]
            -- Read-your-writes over HTTP (§13.2): the next query sees the
            -- commit. (The client does not expose Lattice-Snapshot, so the
            -- monotonic-token form of this contract is asserted by data.)
            r <- runQuery lc jediReviewsQ Map.empty
            marks <- rootValue "reviews" r >>= asArray >>= traverse (textField "commentary")
            marks `shouldBe` ["marker-one", "seeded"]

      it "an Idempotency-Key replay returns the stored response without a second write" $
        withStarWars $ \loop ->
          schemaClient loop $ \lc -> do
            mr1 <- runCreateReview lc reviewBody (Just "e2e-key-replay")
            mrReplayed mr1 `shouldBe` False
            mr2 <- runCreateReview lc reviewBody (Just "e2e-key-replay")
            mrReplayed mr2 `shouldBe` True
            mrCommitted mr2 `shouldBe` True
            mrInvalidated mr2 `shouldBe` mrInvalidated mr1
            r <- runQuery lc jediReviewsQ Map.empty
            items <- rootValue "reviews" r >>= asArray
            length items `shouldBe` 2 -- one seeded + exactly one committed

      it "reusing an Idempotency-Key with a different body is 422 lattice:key-reuse" $
        withStarWars $ \loop ->
          schemaClient loop $ \lc -> do
            _ <- runCreateReview lc reviewBody (Just "e2e-key-reuse")
            expectProblem 422 "key-reuse" (mutate lc "createReview" otherReviewBody (Just "e2e-key-reuse"))

    describe "§8 authorization slices (blog fixture, HMAC-verified ctx)" $ do
      it "a proved ctx query sees exactly the caller's org through the feed root" $
        withBlog $ \loop -> do
          proof <- validProof org1T
          blogClientWith loop org1T proof $ \lc -> do
            resetHits loop
            r <- runQuery lc orgFeedQ (Map.singleton "org" (A.String org1T))
            titles <- rootValue "feed" r >>= asArray >>= traverse (textField "title")
            titles `shouldBe` ["Post One", "Post Two"] -- rank desc; org2's post never appears
            mSlice (qrManifest r) `shouldBe` Just SliceCtx
            hs <- queryHits loop
            case hs of
              [h1, h2] -> do
                (hitMethod h1, hitStatus h1) `shouldBe` (GET, 404)
                (hitMethod h2, hitStatus h2) `shouldBe` (POST, 200)
                -- claims ride the URL (cache key), the proof rides a header
                hitTarget h1 `shouldSatisfy` ("vc=" `BS8.isInfixOf`)
                hitTarget h2 `shouldSatisfy` ("vc=" `BS8.isInfixOf`)
                -- and the ctx response is still CDN-cacheable
                hitCache h2 `shouldSatisfy` maybe False ("public" `BS8.isPrefixOf`)
              _ -> expectationFailure ("expected cold GET + ctx introduce POST, saw: " <> show hs)

      it "a proof minted with the wrong secret is 401 lattice:proof-expired" $
        withBlog $ \loop -> do
          now <- getPOSIXTime
          let badProof = hmacProof "not-the-secret" (encodeClaims (orgClaims org1T)) (floor now + 300)
          blogClientWith loop org1T badProof $ \lc ->
            expectProblem 401 "proof-expired" (query lc orgFeedQ (Map.singleton "org" (A.String org1T)))

      it "an expired proof is 401 lattice:proof-expired" $
        withBlog $ \loop -> do
          proof <- expiredProof org1T
          blogClientWith loop org1T proof $ \lc ->
            expectProblem 401 "proof-expired" (query lc orgFeedQ (Map.singleton "org" (A.String org1T)))

    describe "§6.7 point fetch" $ do
      -- The client exposes no ver= pin, so the version-pinned fetch form
      -- is not exercised here (noted contract gap).
      it "a masked fetch returns exactly the masked field" $
        withStarWars $ \loop ->
          schemaClient loop $ \lc -> do
            r <- io "point fetch" (pointFetch lc (Ref "Human" "1000") ["name"]) >>= requireRight
            nodes <- rootValue "node" r >>= asArray
            case nodes of
              [obj] -> do
                refText <- textField "$ref" obj
                refText `shouldBe` "Human:1000"
                nm <- textField "name" obj
                nm `shouldBe` "Luke Skywalker"
                obj `shouldNotSatisfy` hasFieldPrefix "homePlanet"
              _ -> expectationFailure ("expected exactly one node, got: " <> show nodes)

      it "an unknown key is 404 lattice:not-found" $
        withStarWars $ \loop ->
          schemaClient loop $ \lc ->
            expectProblem 404 "not-found" (pointFetch lc (Ref "Human" "9999") ["name"])

    describe "§9.4 degraded responses" $ do
      it "a failed edge loader degrades to 207: scoped error, loaded data intact" $
        withStarWars $ \loop ->
          schemaClient loop $ \lc -> do
            atomically (writeTVar (loopBreakEdges loop) True)
            resetHits loop
            r <- runQuery lc heroQ Map.empty
            qrDegraded r `shouldBe` True
            case qrErrors r of
              [e] -> do
                errCode e `shouldBe` Just "lattice:upstream-unavailable"
                errScope e `shouldBe` Just (ScopeEdge (Ref "Droid" "2001") "friends")
                errRetryable e `shouldBe` True
              other -> expectationFailure ("expected one Edge-scoped error, got: " <> show other)
            hero <- rootValue "hero" r
            heroName <- textField "name" hero
            heroName `shouldBe` "R2-D2"
            hero `shouldNotSatisfy` hasFieldPrefix "friends"
            hs <- queryHits loop
            map hitStatus hs `shouldSatisfy` elem 207

    describe "§3.6 pagination end to end (friends edge)" $ do
      -- Paginated ROOT collections carry no cursors on the wire (manifest
      -- root map is refs only), so the cursor walk is asserted on an edge.
      it "first:2 then after:cursor walks disjoint pages in keyset order" $
        withStarWars $ \loop ->
          schemaClient loop $ \lc -> do
            r1 <- runQuery lc friendsPage1Q Map.empty
            page1 <- rootValue "hero" r1 >>= fieldByPrefix "friends"
            names1 <- pageNames page1
            names1 `shouldBe` ["Han Solo", "Leia Organa"]
            refs1 <- pageRefTexts page1
            cursor <- objectField "next" page1 >>= asText
            r2 <- runQuery lc friendsPage2Q (Map.singleton "c" (A.String cursor))
            page2 <- rootValue "hero" r2 >>= fieldByPrefix "friends"
            names2 <- pageNames page2
            names2 `shouldBe` ["Luke Skywalker"]
            refs2 <- pageRefTexts page2
            filter (`elem` refs1) refs2 `shouldBe` []

    describe "§3.6 bounded collections" $ do
      it "a bounded collection past max is an Edge-scoped lattice:collection-overflow" $
        withBlog $ \loop -> do
          proof <- validProof org1T
          blogClientWith loop org1T proof $ \lc -> do
            r <- runQuery lc feedTagsQ (Map.singleton "org" (A.String org1T))
            qrDegraded r `shouldBe` True
            let overflows = filter (\e -> errCode e == Just "lattice:collection-overflow") (qrErrors r)
            map errScope overflows `shouldBe` [Just (ScopeEdge (Ref "Post" blogPost1) "tags")]
            posts <- rootValue "feed" r >>= asArray
            case posts of
              [postOne, postTwo] -> do
                titleOne <- textField "title" postOne
                titleOne `shouldBe` "Post One"
                -- Overflow policy omits the overflowing occurrence entirely…
                postOne `shouldNotSatisfy` hasFieldPrefix "tags"
                -- …while a within-bounds sibling keeps its (empty) array.
                tagsTwo <- objectField "tags" postTwo >>= asArray
                tagsTwo `shouldBe` []
              _ -> expectationFailure ("expected two posts, got: " <> show posts)

    describe "§3.8 co-keyed entities (cokey fixture)" $ do
      it "identity edges traverse both directions; a missing joins row is a clean absent to-one (has one?)" $
        withCoKey $ \loop ->
          cokeyClient loop $ \lc -> do
            r <- runQuery lc walkQ (Map.singleton "id" (A.String ckU1))
            user <- rootValue "user" r
            adaName <- textField "name" user
            adaName `shouldBe` "Ada"
            prof <- objectField "profile" user
            -- wire identity is the companion's own type-qualified pair
            profRef <- textField "$ref" prof
            profRef `shouldBe` "UserProfile:u1"
            bio <- textField "bio" prof
            bio `shouldBe` "Analytical engines"
            back <- objectField "user" prof
            backName <- textField "name" back
            backName `shouldBe` "Ada"
            -- Grace has no profile row: the edge is declared `has one?`
            -- (§3.4/§3.8), so absence is legal — the identity edge renders
            -- as an ordinary absent to-one (a bare ref, no entity fact) and
            -- the response is a clean 200, never a degraded 207.
            resetHits loop
            r2 <- runQuery lc walkQ (Map.singleton "id" (A.String ckU2))
            qrDegraded r2 `shouldBe` False
            qrErrors r2 `shouldBe` []
            hs <- queryHits loop
            map (\h -> (hitMethod h, hitStatus h)) hs `shouldBe` [(GET, 200)]
            user2 <- rootValue "user" r2
            prof2 <- objectField "profile" user2
            prof2 `shouldNotSatisfy` hasFieldPrefix "bio"
            prof2 `shouldNotSatisfy` hasFieldPrefix "$ver"
            let isProfileEntity = \case
                  REntity er -> refType (erId er) == "UserProfile"
                  _ -> False
            filter isProfileEntity (qrRecords r2) `shouldBe` []

      it "a refines write mints surrogate keys for the whole family (§10.5)" $
        withCoKey $ \loop ->
          cokeyClient loop $ \lc -> do
            resetHits loop
            mr <- runMutate lc "promoteAdmin" (A.object [("user", A.String ckU2)])
            mrCommitted mr `shouldBe` True
            mrInvalidated mr `shouldSatisfy` elem "AdminUser:u2"
            mrInvalidated mr `shouldSatisfy` elem "User:u2"
            keys <- surrogateKeysOfMutation loop
            keys `shouldSatisfy` elem "AdminUser:u2"
            keys `shouldSatisfy` elem "User:u2"

      it "a joins write mints only its own surrogate keys; the companion may appear later" $
        withCoKey $ \loop ->
          cokeyClient loop $ \lc -> do
            resetHits loop
            mr <-
              runMutate
                lc
                "setProfile"
                (A.object [("user", A.String ckU2), ("bio", A.String "New here")])
            mrCommitted mr `shouldBe` True
            mrInvalidated mr `shouldSatisfy` elem "UserProfile:u2"
            mrInvalidated mr `shouldSatisfy` notElem "User:u2"
            keys <- surrogateKeysOfMutation loop
            keys `shouldSatisfy` elem "UserProfile:u2"
            keys `shouldSatisfy` notElem "User:u2"
            -- the fresh companion is now reachable over the identity edge
            r <- runQuery lc walkQ (Map.singleton "id" (A.String ckU2))
            user <- rootValue "user" r
            prof <- objectField "profile" user
            bio <- textField "bio" prof
            bio `shouldBe` "New here"

      it "deleting the base tombstones the family, not the joins companion; the refinement 410s" $
        withCoKey $ \loop ->
          cokeyClient loop $ \lc -> do
            resetHits loop
            mr <- runMutate lc "deleteUser" (A.object [("user", A.String ckU1)])
            mrCommitted mr `shouldBe` True
            mrInvalidated mr `shouldSatisfy` elem "User:u1"
            mrInvalidated mr `shouldSatisfy` elem "AdminUser:u1"
            mrInvalidated mr `shouldSatisfy` notElem "UserProfile:u1"
            keys <- surrogateKeysOfMutation loop
            keys `shouldSatisfy` elem "User:u1"
            keys `shouldSatisfy` elem "AdminUser:u1"
            keys `shouldSatisfy` notElem "UserProfile:u1"
            -- The response's tombstone records covered the family: the
            -- client store witnessed both evictions, and only those.
            let store = clientStore lc
            tombUser <- atomically (Store.isTombstoned store (Ref "User" ckU1))
            tombAdmin <- atomically (Store.isTombstoned store (Ref "AdminUser" ckU1))
            tombProfile <- atomically (Store.isTombstoned store (Ref "UserProfile" ckU1))
            (tombUser, tombAdmin, tombProfile) `shouldBe` (True, True, False)
            -- At the origin the refinement cannot outlive the row …
            expectStatus 410 (pointFetch lc (Ref "AdminUser" ckU1) ["permissions"])
            -- … while the joins companion's adjacent truth survives.
            rp <- io "point fetch" (pointFetch lc (Ref "UserProfile" ckU1) ["bio"]) >>= requireRight
            nodes <- rootValue "node" rp >>= asArray
            case nodes of
              [obj] -> do
                bio <- textField "bio" obj
                bio `shouldBe` "Analytical engines"
              _ -> expectationFailure ("expected exactly one node, got: " <> show nodes)

    describe "§3.4–§3.6 cardinality (card fixture)" $ do
      it "a dangling required to-one degrades to 207: Edge-scoped lattice:cardinality, rest intact" $
        withCard $ \loop _ctl ->
          cardClient loop $ \lc -> do
            resetHits loop
            r <- runQuery lc cardCustomerQ (Map.singleton "id" (A.String "o1"))
            -- §3.4: `has one` means exactly one; a dangle is reported, never
            -- silent absence, and the response degrades to 207 (§9.4.6).
            qrDegraded r `shouldBe` True
            case qrErrors r of
              [e] -> do
                errCode e `shouldBe` Just "lattice:cardinality"
                errScope e `shouldBe` Just (ScopeEdge (Ref "Order" "o1") "customer")
                errRetryable e `shouldBe` False
              other -> expectationFailure ("expected one Edge-scoped error, got: " <> show other)
            -- The rest of the data is intact: the sibling scalar emits, and
            -- the edge field still carries the dangling ref …
            order <- rootValue "order" r
            note <- textField "note" order
            note `shouldBe` "first"
            customer <- objectField "customer" order
            refText <- textField "$ref" customer
            refText `shouldBe` "Customer:ghost"
            customer `shouldNotSatisfy` hasFieldPrefix "name"
            -- … but no Customer entity fact exists to back it.
            let isCustomer = \case
                  REntity er -> refType (erId er) == "Customer"
                  _ -> False
            filter isCustomer (qrRecords r) `shouldBe` []
            hs <- queryHits loop
            map hitStatus hs `shouldSatisfy` elem 207

      it "a resolving required to-one is silent: clean 200, target loaded" $
        withCard $ \loop _ctl ->
          cardClient loop $ \lc -> do
            resetHits loop
            r <- runQuery lc cardCustomerQ (Map.singleton "id" (A.String "o2"))
            qrDegraded r `shouldBe` False
            qrErrors r `shouldBe` []
            order <- rootValue "order" r
            customer <- objectField "customer" order
            name <- textField "name" customer
            name `shouldBe` "Nia"
            hs <- queryHits loop
            map hitStatus hs `shouldSatisfy` notElem 207

      it "a floored collection scanning short degrades to 207: lattice:collection-underflow, partial emitted" $
        withCard $ \loop _ctl ->
          cardClient loop $ \lc -> do
            resetHits loop
            r <- runQuery lc cardItemsQ (Map.singleton "id" (A.String "o2"))
            -- §3.6: underflow mirrors overflow — report the integrity
            -- violation, keep the rest of the response, degrade to 207.
            qrDegraded r `shouldBe` True
            case qrErrors r of
              [e] -> do
                errCode e `shouldBe` Just "lattice:collection-underflow"
                errScope e `shouldBe` Just (ScopeEdge (Ref "Order" "o2") "lineItems")
                errRetryable e `shouldBe` False
              other -> expectationFailure ("expected one Edge-scoped error, got: " <> show other)
            order <- rootValue "order" r
            note <- textField "note" order
            note `shouldBe` "second"
            -- Underflow emits what exists (here: nothing) PLUS the error,
            -- where overflow's policy omits the occurrence entirely.
            items <- objectField "lineItems" order >>= asArray
            items `shouldBe` []
            hs <- queryHits loop
            map hitStatus hs `shouldSatisfy` elem 207

      it "a floored collection meeting its min is silent: clean 200" $
        withCard $ \loop _ctl ->
          cardClient loop $ \lc -> do
            resetHits loop
            r <- runQuery lc cardItemsQ (Map.singleton "id" (A.String "o1"))
            qrDegraded r `shouldBe` False
            qrErrors r `shouldBe` []
            order <- rootValue "order" r
            skus <- objectField "lineItems" order >>= asArray >>= traverse (textField "sku")
            skus `shouldBe` ["Widget"]
            hs <- queryHits loop
            map hitStatus hs `shouldSatisfy` notElem 207

      it "an empty [t]+ mutation argument is 400 naming the arg; the effect never runs, nothing is written" $
        withCard $ \loop ctl ->
          cardClient loop $ \lc -> do
            expectProblemNaming
              400
              "tags"
              (mutate lc "tagOrder" (A.object [("order", A.String "o3"), ("tags", A.toJSON ([] :: [Text]))]) Nothing)
            -- §3.5.2: rejected at the request — the declared effect never
            -- executed …
            writes <- readTVarIO (cardTagWrites ctl)
            writes `shouldBe` 0
            -- … and a follow-up read sees the seeded row untouched.
            r <- runQuery lc cardTagsQ (Map.singleton "id" (A.String "o3"))
            order <- rootValue "order" r
            note <- textField "note" order
            note `shouldBe` "third"
            tags <- objectField "tags" order >>= asArray
            tags `shouldBe` [A.String "gamma"]

      it "a nonempty [t]+ mutation argument commits and is visible to the next read" $
        withCard $ \loop ctl ->
          cardClient loop $ \lc -> do
            mr <- runMutate lc "tagOrder" (A.object [("order", A.String "o3"), ("tags", A.toJSON (["rush"] :: [Text]))])
            mrCommitted mr `shouldBe` True
            writes <- readTVarIO (cardTagWrites ctl)
            writes `shouldBe` 1
            r <- runQuery lc cardTagsQ (Map.singleton "id" (A.String "o3"))
            order <- rootValue "order" r
            note <- textField "note" order
            note `shouldBe` "tagged"
            tags <- objectField "tags" order >>= asArray
            tags `shouldBe` [A.String "rush"]

      it "an empty [t]+ variable is 400 before any origin work" $
        withCard $ \loop ctl ->
          rawClient loop $ \lc -> do
            expectProblemNaming
              400
              "ts"
              (query lc cardTaggedQ (Map.singleton "ts" (A.toJSON ([] :: [Text]))))
            -- The rejection happens at variable binding: the origin's root
            -- loader was never consulted.
            calls <- readTVarIO (cardTaggedCalls ctl)
            calls `shouldBe` 0

      it "a nonempty [t]+ variable reaches the root loader (positive control)" $
        withCard $ \loop ctl ->
          rawClient loop $ \lc -> do
            r <- runQuery lc cardTaggedQ (Map.singleton "ts" (A.toJSON (["gift"] :: [Text])))
            nodes <- rootValue "orderTagged" r >>= asArray
            case nodes of
              [obj] -> do
                note <- textField "note" obj
                note `shouldBe` "third"
              _ -> expectationFailure ("expected exactly one root hit, got: " <> show nodes)
            calls <- readTVarIO (cardTaggedCalls ctl)
            calls `shouldBe` 1

      it "row data violating [t]+ is a Field-scoped lattice:integrity error; the field still emits" $
        withCard $ \loop _ctl ->
          cardClient loop $ \lc -> do
            resetHits loop
            r <- runQuery lc cardTagsQ (Map.singleton "id" (A.String "o4"))
            -- §3.5.2: an empty array is invalid wherever the type governs;
            -- backend rows are reported, not silently passed through.
            qrDegraded r `shouldBe` True
            case qrErrors r of
              [e] -> do
                errCode e `shouldBe` Just "lattice:integrity"
                errScope e `shouldBe` Just (ScopeField (Ref "Order" "o4") "tags")
                errRetryable e `shouldBe` False
              other -> expectationFailure ("expected one Field-scoped error, got: " <> show other)
            order <- rootValue "order" r
            note <- textField "note" order
            note `shouldBe` "fourth"
            tags <- objectField "tags" order >>= asArray
            tags `shouldBe` []
            hs <- queryHits loop
            map hitStatus hs `shouldSatisfy` elem 207


-- ---------------------------------------------------------------------------
-- Query texts
-- ---------------------------------------------------------------------------

heroQ :: Text
heroQ = "query Hero { hero { name friends(first: 10) { name } } }"


-- | Schema-less variant: no defaulted-argument edges, because without the
-- schema the client cannot erase @first: 10@ to the origin's canonical
-- field key (documented v1 limitation in "Lattice.Client").
heroNameQ :: Text
heroNameQ = "query HeroName { hero { name } }"


heroDeepQ :: Text
heroDeepQ =
  T.unlines
    [ "query HeroDeep {"
    , "  hero {"
    , "    name"
    , "    friends(first: 10) {"
    , "      name"
    , "      ... on Human { friends(first: 10) { name } }"
    , "    }"
    , "  }"
    , "}"
    ]


friendsPage1Q :: Text
friendsPage1Q = "query FriendsPage { hero { friends(first: 2) { name } } }"


friendsPage2Q :: Text
friendsPage2Q = "query FriendsPageAfter($c: Cursor) { hero { friends(first: 2, after: $c) { name } } }"


jediReviewsQ :: Text
jediReviewsQ = "query JediReviews { reviews(episode: Jedi) { stars commentary } }"


orgFeedQ :: Text
orgFeedQ = "query OrgFeed($org: OrgId) { feed(orgId: $org, first: 10) { title } }"


feedTagsQ :: Text
feedTagsQ = "query FeedTags($org: OrgId) { feed(orgId: $org, first: 10) { title tags { name } } }"


walkQ :: Text
walkQ = "query Walk($id: UserId) { user(id: $id) { name profile { bio user { name } } } }"


cardCustomerQ :: Text
cardCustomerQ = "query OrderCustomer($id: OrderId) { order(id: $id) { note customer { name } } }"


cardItemsQ :: Text
cardItemsQ = "query OrderItems($id: OrderId) { order(id: $id) { note lineItems { sku } } }"


cardTagsQ :: Text
cardTagsQ = "query OrderTags($id: OrderId) { order(id: $id) { note tags } }"


-- | @Tags@ is a newtype over @[Text]+@: the variable-shaped spelling of
-- the nonempty list type (§3.5.2).
cardTaggedQ :: Text
cardTaggedQ = "query Tagged($ts: Tags) { orderTagged(tags: $ts) { note } }"


reviewBody :: A.Value
reviewBody =
  A.object
    [ ("episode", A.String "Jedi")
    , ("stars", A.Number 5)
    , ("commentary", A.String "marker-one")
    ]


otherReviewBody :: A.Value
otherReviewBody =
  A.object
    [ ("episode", A.String "Jedi")
    , ("stars", A.Number 4)
    , ("commentary", A.String "marker-two")
    ]


-- ---------------------------------------------------------------------------
-- Star Wars origin
-- ---------------------------------------------------------------------------

withStarWars :: (Loop -> IO a) -> IO a
withStarWars = withLoopback starwarsSchema (swHooks starwarsSchema) swRows Nothing


swRows :: [(TypeName, Map FieldName A.Value)]
swRows = characters <> reviewRows
  where
    episodes = A.toJSON (["NewHope", "Empire", "Jedi"] :: [Text])
    characters =
      [ humanRow "1000" "Luke Skywalker" ["Human:1002", "Human:1003", "Droid:2001"]
      , humanRow "1002" "Han Solo" ["Human:1000", "Human:1003", "Droid:2001"]
      , humanRow "1003" "Leia Organa" ["Human:1000", "Human:1002", "Droid:2001"]
      , droidRow "2001" "R2-D2" ["Human:1000", "Human:1002", "Human:1003"]
      ]
    humanRow key name friendRefs =
      ( "Human"
      , Map.fromList
          [ ("id", A.String key)
          , ("name", A.String name)
          , ("homePlanet", A.String "Tatooine")
          , ("appearsIn", episodes)
          , ("friendIds", A.toJSON (friendRefs :: [Text]))
          ]
      )
    droidRow key name friendRefs =
      ( "Droid"
      , Map.fromList
          [ ("id", A.String key)
          , ("name", A.String name)
          , ("primaryFunction", A.String "Astromech")
          , ("appearsIn", episodes)
          , ("friendIds", A.toJSON (friendRefs :: [Text]))
          ]
      )
    reviewRows =
      [ reviewRow "5001" "NewHope" 5 Nothing "2024-05-01T10:00:00Z"
      , reviewRow "5002" "NewHope" 4 Nothing "2024-05-02T10:00:00Z"
      , reviewRow "5003" "NewHope" 5 Nothing "2024-05-03T10:00:00Z"
      , reviewRow "5004" "NewHope" 3 Nothing "2024-05-04T10:00:00Z"
      , reviewRow "5005" "Jedi" 4 (Just "seeded") "2024-05-05T10:00:00Z"
      ]
    reviewRow key episode stars commentary createdAt =
      ( "Review"
      , Map.fromList
          ( [ ("id", A.String key)
            , ("episode", A.String episode)
            , ("stars", A.Number stars)
            , ("createdAt", A.String createdAt)
            ]
              <> maybe [] (\c -> [("commentary", A.String c)]) commentary
          )
      )


swHooks :: Schema -> MemoryHooks
swHooks schema =
  defaultHooks
    { mhGetRoots = Map.singleton "hero" (\_db _args -> pure (Just (Ref "Droid" "2001")))
    , mhChildrenOverrides =
        Map.fromList
          [ (("Human", "friends"), swFriends schema)
          , (("Droid", "friends"), swFriends schema)
          ]
    , mhMutations = Map.singleton "createReview" swCreateReview
    }


{- | The @friends@ edge: resolve each parent's @friendIds@ (ref strings) to
live targets and page them by target name through 'pageFromRows', so
cursors behave exactly like the generic machinery's. @friendIds@ is read
from the backend's own tables, not the projected parent rows
("Lattice.Backend", /Projections/).
-}
swFriends :: Schema -> MemoryDb -> [(Ref, EntityRow)] -> Window -> IO (Map Ref (Either BackendFailure Page))
swFriends schema db parents window = do
  tables <- atomically $ do
    humans <- tableRows db "Human"
    droids <- tableRows db "Droid"
    pure (Map.fromList [("Human" :: TypeName, humans), ("Droid", droids)])
  let rowOf ref = Map.lookup (refType ref) tables >>= Map.lookup (refKey ref)
      nameOf ref = do
        row <- rowOf ref
        A.String n <- Map.lookup "name" (rowFields row)
        pure n
      resolve v = do
        A.String refText <- Just v
        ref <- parseRef refText
        n <- nameOf ref
        pure (ref, Map.singleton "name" (A.String n))
      friendRows pref = case rowOf pref >>= Map.lookup "friendIds" . rowFields of
        Just (A.Array ids) -> mapMaybe resolve (toList ids)
        _ -> []
      pageFor (pref, _) =
        pageFromRows schema ["Human", "Droid"] (swFriendsWindowing schema) window (friendRows pref)
  pure (Map.fromList (map (\p -> (fst p, Right (pageFor p))) parents))


swFriendsWindowing :: Schema -> Windowing
swFriendsWindowing schema =
  case lookupEntity schema "Human" >>= (`lookupEntityRel` "friends") of
    Just ToMany {relCollection} -> colWindow relCollection
    _ -> error "starwars fixture: Human.friends collection missing"


-- | @createReview(episode, stars, commentary?)@: insert a Review under the
-- next sequential id, at a fixed timestamp newer than every seeded row.
swCreateReview :: MemoryDb -> Claims -> Map ArgName A.Value -> IO MutationOutcome
swCreateReview db _claims args =
  case (Map.lookup "episode" args, Map.lookup "stars" args) of
    (Just episode@(A.String episodeText), Just stars@(A.Number _)) -> atomically $ do
      existing <- tableRows db "Review"
      let keyNum k = fromMaybe 0 (readMaybe (T.unpack k)) :: Integer
          nextId = T.pack (show (1 + Map.foldlWithKey' (\acc k _ -> max acc (keyNum k)) 5000 existing))
          ref = Ref "Review" nextId
          commentaryField = case Map.lookup "commentary" args of
            Just c@(A.String _) -> [("commentary", c)]
            _ -> []
          fields =
            Map.fromList
              ( [ ("id", A.String nextId)
                , ("episode", episode)
                , ("stars", stars)
                , ("createdAt", A.String "2024-06-01T00:00:00Z")
                ]
                  <> commentaryField
              )
      putRow db "Review" nextId fields
      tok <- snapshotToken db
      pure . MutationCommitted $
        CommitResult
          { crResult = [ref]
          , crWrites = [WroteEntity ref, WroteCollection "reviews" [episodeText]]
          , crSnapshot = tok
          }
    _ -> pure (MutationFailed (internalError (Just "createReview: episode and stars arguments required")))


-- ---------------------------------------------------------------------------
-- Blog origin (claims + proof verifier)
-- ---------------------------------------------------------------------------

e2eSecret :: ByteString
e2eSecret = "e2e-secret"


org1T, org2T :: Text
org1T = "11111111-1111-1111-1111-111111111111"
org2T = "22222222-2222-2222-2222-222222222222"


user1T, user2T :: Text
user1T = "a0000000-0000-0000-0000-000000000001"
user2T = "a0000000-0000-0000-0000-000000000002"


blogPost1, blogPost2, blogPost3 :: Text
blogPost1 = "b0000000-0000-0000-0000-000000000001"
blogPost2 = "b0000000-0000-0000-0000-000000000002"
blogPost3 = "b0000000-0000-0000-0000-000000000003"


withBlog :: (Loop -> IO a) -> IO a
withBlog =
  withLoopback blogSchema defaultHooks blogRows (Just (hmacVerifier e2eSecret getPOSIXTime))


blogRows :: [(TypeName, Map FieldName A.Value)]
blogRows =
  [ userRow user1T org1T "Ada"
  , userRow user2T org2T "Grace"
  , postRow blogPost1 org1T user1T "Post One" 2 "2024-04-01T00:00:00Z"
  , postRow blogPost2 org1T user1T "Post Two" 1 "2024-04-02T00:00:00Z"
  , postRow blogPost3 org2T user2T "Other Org Post" 9 "2024-04-03T00:00:00Z"
  ]
    <> map tagRow [1 .. 51 :: Int]
  where
    userRow key org name =
      ( "User"
      , Map.fromList
          [ ("id", A.String key)
          , ("orgId", A.String org)
          , ("name", A.String name)
          , ("email", A.String (name <> "@example.com"))
          ]
      )
    postRow key org author title rank ts =
      ( "Post"
      , Map.fromList
          [ ("id", A.String key)
          , ("orgId", A.String org)
          , ("authorId", A.String author)
          , ("title", A.String title)
          , ("body", A.String "words")
          , ("rank", A.Number rank)
          , ("createdAt", A.String ts)
          , ("publishedAt", A.String ts)
          ]
      )
    tagRow n =
      ( "Tag"
      , Map.fromList
          [ ("id", A.String ("t" <> T.pack (show n)))
          , ("postId", A.String blogPost1)
          , ("name", A.String ("tag-" <> T.pack (show n)))
          ]
      )


orgClaims :: Text -> Claims
orgClaims org = Map.singleton "org" (A.String org)


validProof :: Text -> IO Text
validProof org = do
  now <- getPOSIXTime
  pure (hmacProof e2eSecret (encodeClaims (orgClaims org)) (floor now + 300))


expiredProof :: Text -> IO Text
expiredProof org = do
  now <- getPOSIXTime
  pure (hmacProof e2eSecret (encodeClaims (orgClaims org)) (floor now - 30))


-- ---------------------------------------------------------------------------
-- Co-keyed origin (cokey fixture, §3.8)
-- ---------------------------------------------------------------------------

ckU1, ckU2 :: Text
ckU1 = "u1"
ckU2 = "u2"


withCoKey :: (Loop -> IO a) -> IO a
withCoKey = withLoopback cokeySchema ckHooks ckRows Nothing


{- | Ada (u1) has both companions seeded: a UserProfile row (adjacent
truth) and an AdminUser row (same truth). Grace (u2) is a bare User: her
identity edges dangle until a mutation writes a companion row.
-}
ckRows :: [(TypeName, Map FieldName A.Value)]
ckRows =
  [ ("User", Map.fromList [("id", A.String ckU1), ("name", A.String "Ada")])
  , ("User", Map.fromList [("id", A.String ckU2), ("name", A.String "Grace")])
  ,
    ( "UserProfile"
    , Map.fromList
        [ ("id", A.String ckU1)
        , ("bio", A.String "Analytical engines")
        , ("location", A.String "London")
        ]
    )
  , ("AdminUser", Map.fromList [("id", A.String ckU1), ("permissions", A.String "all")])
  ]


ckHooks :: MemoryHooks
ckHooks =
  defaultHooks
    { mhGetRoots =
        Map.fromList
          [ ("user", ckByIdRoot "User")
          , ("profile", ckByIdRoot "UserProfile")
          , ("admin", ckByIdRoot "AdminUser")
          ]
    , mhMutations =
        Map.fromList
          [ ("setProfile", ckSetProfile)
          , ("promoteAdmin", ckPromoteAdmin)
          , ("deleteUser", ckDeleteUser)
          ]
    }


ckByIdRoot :: TypeName -> MemoryDb -> Map ArgName A.Value -> IO (Maybe Ref)
ckByIdRoot ty _db args = pure $ case Map.lookup "id" args of
  Just (A.String k) -> Just (Ref ty k)
  _ -> Nothing


{- | @setProfile(user, bio)@: upsert the joins companion. Adjacent truth:
the write fact names only UserProfile (§3.8).
-}
ckSetProfile :: MemoryDb -> Claims -> Map ArgName A.Value -> IO MutationOutcome
ckSetProfile db _claims args =
  case (Map.lookup "user" args, Map.lookup "bio" args) of
    (Just (A.String u), Just bio@(A.String _)) -> atomically $ do
      putRow db "UserProfile" u (Map.fromList [("id", A.String u), ("bio", bio)])
      tok <- snapshotToken db
      pure . MutationCommitted $
        CommitResult
          { crResult = [Ref "UserProfile" u]
          , crWrites = [WroteEntity (Ref "UserProfile" u)]
          , crSnapshot = tok
          }
    _ -> pure (MutationFailed (internalError (Just "setProfile: user and bio arguments required")))


{- | @promoteAdmin(user)@: write the refinement. Same truth: the server
fans the one write fact out to the whole family's keys (§3.8\/§10.5).
-}
ckPromoteAdmin :: MemoryDb -> Claims -> Map ArgName A.Value -> IO MutationOutcome
ckPromoteAdmin db _claims args =
  case Map.lookup "user" args of
    Just (A.String u) -> atomically $ do
      putRow db "AdminUser" u (Map.fromList [("id", A.String u), ("permissions", A.String "all")])
      tok <- snapshotToken db
      pure . MutationCommitted $
        CommitResult
          { crResult = [Ref "AdminUser" u]
          , crWrites = [WroteEntity (Ref "AdminUser" u)]
          , crSnapshot = tok
          }
    _ -> pure (MutationFailed (internalError (Just "promoteAdmin: user argument required")))


{- | @deleteUser(user)@: the base row and its refinement are one record of
truth, so the effect deletes both storage rows; the write fact names the
BASE only and the server expands the family (tombstones and keys,
§3.8). The joins companion's row is its own truth and stays.
-}
ckDeleteUser :: MemoryDb -> Claims -> Map ArgName A.Value -> IO MutationOutcome
ckDeleteUser db _claims args =
  case Map.lookup "user" args of
    Just (A.String u) -> atomically $ do
      deleteRow db "User" u
      deleteRow db "AdminUser" u
      tomb <- readRow db "User" u
      tok <- snapshotToken db
      let ver = case tomb of
            RowTombstone v -> v
            _ -> "t:1"
      pure . MutationCommitted $
        CommitResult
          { crResult = [Ref "User" u]
          , crWrites = [DeletedEntity (Ref "User" u) ver]
          , crSnapshot = tok
          }
    _ -> pure (MutationFailed (internalError (Just "deleteUser: user argument required")))


-- ---------------------------------------------------------------------------
-- Cardinality origin (card fixture, §3.4–§3.6)
-- ---------------------------------------------------------------------------

{- | Live handles into the card origin's hooks: invocation counters for
the @orderTagged@ get root and the @tagOrder@ mutation effect, so the
[t]+ rejection tests can assert the origin did no work.
-}
data CardCtl = CardCtl
  { cardTaggedCalls :: TVar Int
  , cardTagWrites :: TVar Int
  }


withCard :: (Loop -> CardCtl -> IO a) -> IO a
withCard action = do
  ctl <- CardCtl <$> newTVarIO 0 <*> newTVarIO 0
  withLoopback cardSchema (cardHooks ctl) cardRows Nothing (\loop -> action loop ctl)


{- | One customer, four orders: @o1@'s customer dangles (its required
@has one@ is a broken contract) but its floored @lineItems@ is satisfied;
@o2@ resolves its customer but scans zero line items (underflow); @o3@ is
fully clean (the mutation-output target); @o4@'s stored @tags@ violates
its @[Text]+@ type (row-data integrity).
-}
cardRows :: [(TypeName, Map FieldName A.Value)]
cardRows =
  [ ("Customer", Map.fromList [("id", A.String "c1"), ("name", A.String "Nia")])
  , orderRow "o1" "ghost" "first" ["intro"]
  , orderRow "o2" "c1" "second" ["beta"]
  , orderRow "o3" "c1" "third" ["gamma"]
  , orderRow "o4" "c1" "fourth" []
  , itemRow "li1" "o1" "Widget"
  , itemRow "li3" "o3" "Gadget"
  , itemRow "li4" "o4" "Gizmo"
  ]
  where
    orderRow key cust note tags =
      ( "Order"
      , Map.fromList
          [ ("id", A.String key)
          , ("customerId", A.String cust)
          , ("note", A.String note)
          , ("tags", A.toJSON (tags :: [Text]))
          ]
      )
    itemRow key order sku =
      ( "LineItem"
      , Map.fromList [("id", A.String key), ("orderId", A.String order), ("sku", A.String sku)]
      )


cardHooks :: CardCtl -> MemoryHooks
cardHooks ctl =
  defaultHooks
    { mhGetRoots =
        Map.fromList
          [ ("order", ckByIdRoot "Order")
          , ("orderTagged", cardTaggedRoot ctl)
          ]
    , mhMutations = Map.fromList [("tagOrder", cardTagOrder ctl)]
    }


-- | @orderTagged(tags)@: a fixed hit, counting invocations — the [t]+
-- variable tests assert whether the origin's loader ever ran.
cardTaggedRoot :: CardCtl -> MemoryDb -> Map ArgName A.Value -> IO (Maybe Ref)
cardTaggedRoot ctl _db _args = do
  atomically (modifyTVar' (cardTaggedCalls ctl) (+ 1))
  pure (Just (Ref "Order" "o3"))


{- | @tagOrder(order, tags)@: overwrite the order's @tags@ and stamp its
@note@, counting invocations — the [t]+ argument tests assert both that
the effect never ran and that a follow-up read sees no write.
-}
cardTagOrder :: CardCtl -> MemoryDb -> Claims -> Map ArgName A.Value -> IO MutationOutcome
cardTagOrder ctl db _claims args =
  case (Map.lookup "order" args, Map.lookup "tags" args) of
    (Just (A.String o), Just tags@(A.Array _)) -> do
      atomically (modifyTVar' (cardTagWrites ctl) (+ 1))
      atomically $ do
        existing <- readRow db "Order" o
        let base = case existing of
              RowFound row -> rowFields row
              _ -> Map.singleton "id" (A.String o)
        putRow db "Order" o (Map.insert "tags" tags (Map.insert "note" (A.String "tagged") base))
        tok <- snapshotToken db
        pure . MutationCommitted $
          CommitResult
            { crResult = [Ref "Order" o]
            , crWrites = [WroteEntity (Ref "Order" o)]
            , crSnapshot = tok
            }
    _ -> pure (MutationFailed (internalError (Just "tagOrder: order and tags arguments required")))


-- ---------------------------------------------------------------------------
-- Clients and operations
-- ---------------------------------------------------------------------------

schemaClient :: Loop -> (LatticeClient -> IO a) -> IO a
schemaClient loop = clientFor loop (\c -> c {ccSchema = Just starwarsSchema})


rawClient :: Loop -> (LatticeClient -> IO a) -> IO a
rawClient loop = clientFor loop id


blogClientWith :: Loop -> Text -> Text -> (LatticeClient -> IO a) -> IO a
blogClientWith loop org proof =
  clientFor loop (\c -> c {ccSchema = Just blogSchema, ccClaims = Just (orgClaims org, proof)})


cokeyClient :: Loop -> (LatticeClient -> IO a) -> IO a
cokeyClient loop = clientFor loop (\c -> c {ccSchema = Just cokeySchema})


cardClient :: Loop -> (LatticeClient -> IO a) -> IO a
cardClient loop = clientFor loop (\c -> c {ccSchema = Just cardSchema})


runCreateReview :: LatticeClient -> A.Value -> Maybe Text -> IO MutationResult
runCreateReview lc input mIdem = io "mutation" (mutate lc "createReview" input mIdem) >>= requireRight
