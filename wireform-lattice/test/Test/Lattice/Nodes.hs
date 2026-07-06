{- | The protocol-level @nodes@ root (spec §14.4): batched fetch-by-ref
against the implicit root every origin serves.

One inline fixture schema covers the §14.4 policy matrix: @Post@ and
@Author@ (public @fetch by@), @Member@ (claims-gated @fetch by@ — the
@fetchBy ⊔ field policy@ slicing pin), and @Vault@ (NO @fetch by@:
by-ref fetching forbidden). Vars-borne refs ride "Lattice.Client";
record-level absence assertions ride raw oneshot POSTs with inline
string-literal refs.

Coverage:

* Compilation: @nodes@ compiles against any schema with no IDL
  declaration (this fixture AND the untouched starwars fixture);
  refs are legal as an @EntityRef@ variable (bound to a JSON array of
  ref strings) and inline as string literals; selections dispatch per
  concrete type with inline
  fragments exactly like an interface target.
* Gating: a type whose @fetch by@ is absent emits NOTHING for its
  refs — not @elided@, not an error record; existence probing against
  a denied type is indistinguishable from nonexistence. Unknown and
  malformed refs behave identically.
* Slicing: a claims-gated @fetch by@ pushes even public fields of the
  fetched type into the ctx slice (@fetchBy ⊔ field policy@ through
  the ordinary path join); the proof decides emission per ref, and a
  policy miss never disturbs the other refs in the batch.
* Batching: N refs of one type are ONE 'beLoad' per type per round.
* Ordering: the manifest's @nodes@ root lists refs in request order;
  absent entries are simply missing.
* Identity and transport: a nodes query memoizes and rides the
  hash-GET ladder like any query; its planId moves with the fetch-by
  policies it touches and stays put under unrelated schema growth.
* Cross-slice composition: a nodes selection spanning slice levels (a
  pub-fetchBy type AND a claims-gated type in one refs list) splits
  the @nodes@ root across two slice manifests; 'Lattice.Client'
  merges the per-slice root lists order-preservingly, so the merged
  tree carries both types.
* Tombstones: under a row-comparing fetch-by gate a tombstoned row is
  denied like any other (no row to compare — §14.4 nonexistence);
  under a claims-only gate the tombstone emits.
* Budget: a refs list longer than @maxRoundFanout@ is the ordinary
  budget rejection naming the bound.
-}
module Test.Lattice.Nodes (tests) where

import Control.Concurrent.STM
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Char8 qualified as BS8
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Lattice.Backend (Backend (..))
import Lattice.Backend.Memory (deleteRow)
import Lattice.Client
import Lattice.Plan (Plan (..), planQuery)
import Lattice.Schema (Budgets (..), Schema, defaultBudgets)
import Lattice.Server (OriginConfig (..))
import Lattice.Server.Auth (encodeClaims, hmacProof, hmacVerifier)
import Lattice.Types
import Lattice.Wire (EntityRecord (..), Manifest (..), Record (..), SliceInfo (..), queryMediaType)
import Network.HTTP.Types.Method (Method (..))
import Test.Lattice.Fixtures (compileWith, mustParseSchema, requireRight, starwarsSchema)
import Test.Lattice.Loop
import Test.Syd


tests :: Spec
tests =
  describe "The nodes root (§14.4)" $ do
    describe "compilation: implicit, undeclared, fragment-dispatched" $ do
      it "nodes compiles against this fixture with an EntityRef variable" $
        shouldCompile nodesSchema mixedQ

      it "nodes compiles against the untouched starwars fixture (any schema)" $
        shouldCompile
          starwarsSchema
          "query Probe($refs: EntityRef) { nodes(refs: $refs) { ... on Human { name } } }"

      it "inline string-literal refs are legal" $
        shouldCompile nodesSchema "query { nodes(refs: [\"Post:p1\"]) { ... on Post { title } } }"

    describe "mixed-type dispatch and ordering" $ do
      it "one query fans out per concrete type; results ride in request order" $
        withNodes $ \loop ->
          nodesClient loop $ \lc -> do
            r <- runQuery lc mixedQ (refsVar ["Post:p1", "Author:a1", "Post:p2"])
            items <- rootValue "nodes" r >>= asArray
            titlesOrNames <- traverse anyLabel items
            titlesOrNames `shouldBe` ["First", "Ada", "Second"]

      it "the manifest's nodes root lists refs in request order" $
        withNodes $ \loop ->
          nodesClient loop $ \lc -> do
            r <- runQuery lc mixedQ (refsVar ["Author:a1", "Post:p2", "Post:p1"])
            nodesRootRefs (qrManifest r)
              `shouldBe` [Ref "Author" "a1", Ref "Post" "p2", Ref "Post" "p1"]

      it "an absent entry is simply missing, never a tombstone or an error" $
        withNodes $ \loop -> do
          r <- oneshot loop "query { nodes(refs: [\"Post:p1\", \"Post:ghost\", \"Author:a1\"]) { ... on Post { title } ... on Author { name } } }"
          rawStatus r `shouldBe` 200
          let records = rawRecords r
          [erId er | REntity er <- records]
            `shouldBe` [Ref "Post" "p1", Ref "Author" "a1"]
          [() | RTombstone {} <- records] `shouldBe` []
          [() | RError {} <- records] `shouldBe` []

    describe "per-type batching (§14.4: set-in map-out)" $ do
      it "N refs of one type are ONE beLoad; two types are two" $ do
        calls <- newTVarIO []
        withNodesCounting calls $ \loop ->
          nodesClient loop $ \lc -> do
            _ <-
              runQuery
                lc
                mixedQ
                (refsVar ["Post:p1", "Post:p2", "Post:p3", "Author:a1", "Author:a2"])
            seen <- readTVarIO calls
            Map.fromList (reverse seen)
              `shouldBe` Map.fromList [("Post", 3), ("Author", 2)]
            length seen `shouldBe` 2

    describe "denied and malformed refs emit nothing (§14.4)" $ do
      it "a type without fetch by emits nothing for its ref — the others are undisturbed" $
        withNodes $ \loop -> do
          r <- oneshot loop "query { nodes(refs: [\"Vault:v1\", \"Post:p1\"]) { ... on Vault { label } ... on Post { title } } }"
          rawStatus r `shouldBe` 200
          [erId er | REntity er <- rawRecords r] `shouldBe` [Ref "Post" "p1"]

      it "existence probing a denied type is indistinguishable from nonexistence" $
        withNodes $ \loop -> do
          -- Vault:v1 is seeded; Vault:ghost is not. Neither response may
          -- differ in any record it emits for the probed ref.
          hit <- oneshot loop "query { nodes(refs: [\"Vault:v1\"]) { ... on Vault { label } } }"
          miss <- oneshot loop "query { nodes(refs: [\"Vault:ghost\"]) { ... on Vault { label } } }"
          rawStatus hit `shouldBe` rawStatus miss
          emissionShape (rawRecords hit) `shouldBe` emissionShape (rawRecords miss)
          emissionShape (rawRecords hit) `shouldBe` []

      it "unknown-type and malformed refs emit nothing" $
        withNodes $ \loop -> do
          r <- oneshot loop "query { nodes(refs: [\"Ghost:g1\", \"garbage\", \"Post:p1\"]) { ... on Post { title } } }"
          rawStatus r `shouldBe` 200
          [erId er | REntity er <- rawRecords r] `shouldBe` [Ref "Post" "p1"]
          [() | RError {} <- rawRecords r] `shouldBe` []

    describe "fetchBy ⊔ field policy (§14.4 slicing pin)" $ do
      it "a claims-gated fetch by pushes the type's public fields into ctx" $ do
        c <- requireRight (compileWith nodesSchema memberQ)
        p <- requireRight (planQuery nodesSchema defaultBudgets c)
        Map.member SlicePub (planSlices p) `shouldBe` False
        case Map.lookup SliceCtx (planSlices p) of
          Nothing -> expectationFailure "the gated nodes query lost its ctx slice"
          Just info -> siClaims info `shouldBe` ["org"]

      it "a public fetch by keeps a public selection on the pub slice" $ do
        c <- requireRight (compileWith nodesSchema pubOnlyQ)
        p <- requireRight (planQuery nodesSchema defaultBudgets c)
        Map.keys (planSlices p) `shouldBe` [SlicePub]

      it "the matching proof admits the ref; a mismatched org emits nothing" $
        withNodes $ \loop -> do
          good <- proofFor "org-1"
          memberClient loop "org-1" good $ \lc -> do
            r <- runQuery lc memberQ (refsVar ["Member:m1"])
            items <- rootValue "nodes" r >>= asArray
            traverse (textField "name") items >>= (`shouldBe` ["Mel"])
          wrong <- proofFor "org-2"
          memberClient loop "org-2" wrong $ \lc -> do
            r <- runQuery lc memberQ (refsVar ["Member:m1"])
            -- The ref fails its fetch-by join SILENTLY: no entity, no
            -- elided marker, no error record — nothing.
            [erId er | REntity er <- qrRecords r] `shouldBe` []
            [() | RError {} <- qrRecords r] `shouldBe` []
            qrErrors r `shouldBe` []

      it "a nodes selection spanning slice levels merges both types through the client" $
        withNodes $ \loop -> do
          good <- proofFor "org-1"
          memberClient loop "org-1" good $ \lc -> do
            r <- runQuery lc memberAndPostQ (refsVar ["Member:m1", "Post:p1"])
            items <- rootValue "nodes" r >>= asArray
            -- Each SLICE manifest lists its refs in request order (§14.4);
            -- the merged cross-slice ordering is the client's own.
            labels <- traverse anyLabel items
            List.sort labels `shouldBe` ["First", "Mel"]

      it "a tombstone under a row-comparing gate is denied; under a claims-only gate it emits" $
        withNodes $ \loop -> do
          atomically $ do
            deleteRow (loopDb loop) "Member" "m1"
            deleteRow (loopDb loop) "Post" "p1"
          good <- proofFor "org-1"
          memberClient loop "org-1" good $ \lc -> do
            r <- runQuery lc memberAndPostQ (refsVar ["Member:m1", "Post:p1"])
            -- Post's public fetch by is claims-only: its tombstone rides
            -- (once per fetched slice; qrRecords concatenates raw slices).
            -- Member's gate compares against the (gone) row: nothing.
            let tombs = [rRef | RTombstone rRef _ _ <- qrRecords r]
            tombs `shouldSatisfy` (not . null)
            tombs `shouldSatisfy` all (== Ref "Post" "p1")
            [erId er | REntity er <- qrRecords r] `shouldBe` []

    describe "plan identity and transport (§14.4 + §7.3)" $ do
      it "a nodes query memoizes: introduction once, then a bare hash GET" $
        withNodes $ \loop -> do
          nodesClient loop $ \lc -> do
            resetHits loop
            _ <- runQuery lc mixedQ (refsVar ["Post:p1"])
            hs <- queryHits loop
            map hitMethod hs `shouldBe` [GET, POST]
          nodesClient loop $ \lc -> do
            resetHits loop
            r <- runQuery lc mixedQ (refsVar ["Post:p1"])
            hs <- queryHits loop
            map (\h -> (hitMethod h, hitStatus h)) hs `shouldBe` [(GET, 200)]
            items <- rootValue "nodes" r >>= asArray
            length items `shouldBe` 1

      it "planId moves with a touched fetch-by policy, queryHash stays" $ do
        base <- nodesPlan nodesSchema mixedQ
        gated <- nodesPlan nodesPostGated mixedQ
        planQueryHash gated `shouldBe` planQueryHash base
        planId gated `shouldNotBe` planId base

      it "unrelated schema growth leaves a nodes planId unchanged" $ do
        base <- nodesPlan nodesSchema mixedQ
        widened <- nodesPlan nodesPlusWidget mixedQ
        planQueryHash widened `shouldBe` planQueryHash base
        planId widened `shouldBe` planId base

    describe "budget (§14.4/§14.1)" $ do
      it "a refs list past maxRoundFanout is the ordinary budget rejection" $
        withNodesTightBudget $ \loop ->
          nodesClient loop $ \lc ->
            expectProblemNaming
              400
              "maxRoundFanout"
              (query lc mixedQ (refsVar ["Post:p1", "Post:p2", "Post:p3", "Author:a1"]))


-- ---------------------------------------------------------------------------
-- Fixture: the §14.4 policy matrix
-- ---------------------------------------------------------------------------

nodesText :: Text
nodesText =
  T.unlines
    [ "schema nodes.example.com"
    , ""
    , "newtype PostId   = Text"
    , "newtype AuthorId = Text"
    , "newtype MemberId = Text"
    , "newtype VaultId  = Text"
    , "newtype OrgId    = Text"
    , ""
    , "claims {"
    , "  org: OrgId"
    , "}"
    , ""
    , "entity Post by id {"
    , "  visible to all by default"
    , ""
    , "  id:         PostId"
    , "  title:      Text"
    , "  draftNotes: Text?   private"
    , ""
    , "  fetch by id: public"
    , "}"
    , ""
    , "entity Author by id {"
    , "  visible to all by default"
    , ""
    , "  id:   AuthorId"
    , "  name: Text"
    , ""
    , "  fetch by id: public"
    , "}"
    , ""
    , "entity Member by id {"
    , "  visible to all by default"
    , ""
    , "  id:    MemberId"
    , "  orgId: OrgId"
    , "  name:  Text"
    , ""
    , "  fetch by id: visible when caller.org = orgId"
    , "}"
    , ""
    , "entity Vault by id {"
    , "  visible to all by default"
    , ""
    , "  id:    VaultId"
    , "  label: Text"
    , "}"
    , ""
    , "get post(id: PostId) of Post public"
    ]


nodesSchema :: Schema
nodesSchema = mustParseSchema nodesText
{-# NOINLINE nodesSchema #-}


-- | Post's public fetch by, gated: pertinent to any nodes query touching Post.
nodesPostGated :: Schema
nodesPostGated =
  mustParseSchema
    (T.replace "fetch by id: public\n}\n\nentity Author" "fetch by id: private\n}\n\nentity Author" nodesText)
{-# NOINLINE nodesPostGated #-}


-- | The fixture plus an entity no nodes query here touches.
nodesPlusWidget :: Schema
nodesPlusWidget =
  mustParseSchema
    ( nodesText
        <> T.unlines
          [ ""
          , "entity Widget by id {"
          , "  visible to all by default"
          , ""
          , "  id: Text"
          , ""
          , "  fetch by id: public"
          , "}"
          ]
    )
{-# NOINLINE nodesPlusWidget #-}


nodesRows :: [(TypeName, Map FieldName A.Value)]
nodesRows =
  [ ("Post", Map.fromList [("id", A.String "p1"), ("title", A.String "First")])
  , ("Post", Map.fromList [("id", A.String "p2"), ("title", A.String "Second")])
  , ("Post", Map.fromList [("id", A.String "p3"), ("title", A.String "Third")])
  , ("Author", Map.fromList [("id", A.String "a1"), ("name", A.String "Ada")])
  , ("Author", Map.fromList [("id", A.String "a2"), ("name", A.String "Grace")])
  , ("Member", Map.fromList [("id", A.String "m1"), ("orgId", A.String "org-1"), ("name", A.String "Mel")])
  , ("Vault", Map.fromList [("id", A.String "v1"), ("label", A.String "classified")])
  ]


nodesSpec :: LoopSpec
nodesSpec =
  (loopSpec nodesSchema)
    { lsRows = nodesRows
    , lsVerifier = Just (hmacVerifier nodesSecret getPOSIXTime)
    }


withNodes :: (Loop -> IO a) -> IO a
withNodes = withLoop nodesSpec


-- | Same fixture, recording @(type, batch size)@ per 'beLoad' call.
withNodesCounting :: TVar [(TypeName, Int)] -> (Loop -> IO a) -> IO a
withNodesCounting calls =
  withLoop
    nodesSpec
      { lsWrap = \inner ->
          inner
            { beLoad = \ty keys -> do
                atomically (modifyTVar' calls ((ty, length keys) :))
                beLoad inner ty keys
            }
      }


-- | Same fixture under a 3-wide round budget.
withNodesTightBudget :: (Loop -> IO a) -> IO a
withNodesTightBudget =
  withLoop nodesSpec {lsTweak = \cfg -> cfg {ocBudgets = defaultBudgets {maxRoundFanout = 3}}}


nodesSecret :: BS8.ByteString
nodesSecret = "nodes-secret"


proofFor :: Text -> IO Text
proofFor org = do
  now <- getPOSIXTime
  pure (hmacProof nodesSecret (encodeClaims (Map.singleton "org" (A.String org))) (floor now + 300))


-- ---------------------------------------------------------------------------
-- Queries
-- ---------------------------------------------------------------------------

mixedQ :: Text
mixedQ = "query Mixed($refs: EntityRef) { nodes(refs: $refs) { ... on Post { title } ... on Author { name } } }"


memberQ :: Text
memberQ = "query Gated($refs: EntityRef) { nodes(refs: $refs) { ... on Member { name } } }"


memberAndPostQ :: Text
memberAndPostQ = "query Both($refs: EntityRef) { nodes(refs: $refs) { ... on Member { name } ... on Post { title } } }"


pubOnlyQ :: Text
pubOnlyQ = "query Pub($refs: EntityRef) { nodes(refs: $refs) { ... on Post { title } } }"


refsVar :: [Text] -> Map VarName A.Value
refsVar rs = Map.singleton "refs" (A.toJSON rs)


-- ---------------------------------------------------------------------------
-- Clients and probes
-- ---------------------------------------------------------------------------

nodesClient :: Loop -> (LatticeClient -> IO a) -> IO a
nodesClient loop = clientFor loop (\c -> c {ccSchema = Just nodesSchema})


memberClient :: Loop -> Text -> Text -> (LatticeClient -> IO a) -> IO a
memberClient loop org proof =
  clientFor
    loop
    ( \c ->
        c
          { ccSchema = Just nodesSchema
          , ccClaims = Just (Map.singleton "org" (A.String org), proof)
          }
    )


oneshot :: Loop -> Text -> IO RawResp
oneshot loop q =
  httpRaw
    loop
    POST
    "/q?intent=oneshot&slice=pub"
    [("Content-Type", queryMediaType)]
    (Just (encodeUtf8 q))


shouldCompile :: Schema -> Text -> IO ()
shouldCompile schema q = case compileWith schema q of
  Right _ -> pure ()
  Left err -> expectationFailure ("nodes query failed to compile: " <> show err)


-- | The mixed selection renders exactly one of title/name per item.
anyLabel :: A.Value -> IO Text
anyLabel v = case v of
  A.Object o -> case (KM.lookup "title" o, KM.lookup "name" o) of
    (Just (A.String t), _) -> pure t
    (_, Just (A.String n)) -> pure n
    _ -> expectationFailure ("expected a title or name, got: " <> show v)
  _ -> expectationFailure ("expected an entity object, got: " <> show v)


nodesPlan :: Schema -> Text -> IO Plan
nodesPlan schema q = do
  c <- requireRight (compileWith schema q)
  requireRight (planQuery schema defaultBudgets c)


nodesRootRefs :: Manifest -> [Ref]
nodesRootRefs m =
  case [refs | (name, refs) <- Map.toList (mRoot m), "nodes" `T.isPrefixOf` name] of
    [refs] -> refs
    _ -> []


-- | What a probe emits for its ref: every non-manifest, non-end record.
emissionShape :: [Record] -> [Record]
emissionShape = filter (not . isFraming)
  where
    isFraming = \case
      RManifest {} -> True
      REnd {} -> True
      _ -> False
