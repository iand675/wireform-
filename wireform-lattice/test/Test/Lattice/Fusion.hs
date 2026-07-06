{- | Schema modules and fusion (spec §18.1, §18.2): "Lattice.Module".

Three fixture modules — a @posts@ owner, a @social@ module extending
@Post@ with a stored field and a @has many@ edge, and an unrelated
@badges@ module — plus per-test miniature module pairs for every
'FusionError'.

Coverage:

* Determinism (§18.2): 'fuseModules' of every permutation yields
  byte-identical 'fusedIdl'; the fused text is one plain schema (no
  @extend@ anywhere) that reparses to exactly 'fusedSchema'.
* IDL surface (§18.1): a module's own canonical IDL keeps its
  @extend entity@ block (fixpoint); the FUSED IDL folds extension
  members into the owning entity's declaration — provenance lives in
  'fusedOwner'\/'fusedFieldOwner' (extension members only), not in the
  text.
* Conflicts (§18.1): every 'FusionError' constructor is exercised and
  names its offender; identical claim (and shared vocabulary type)
  declarations dedupe.
* Execution: 'fuseBackends' routes an owner load and an extension
  edge through their modules' backends, and the seam query emits ONE
  merged entity record per entity whose @ver@ is the owner's — the
  documented in-process simplification ("in-process fusion needs not
  even that", §18.1).
-}
module Test.Lattice.Fusion (tests) where

import Control.Concurrent.STM (atomically)
import Data.Aeson qualified as A
import Data.ByteString.Char8 qualified as BS8
import Data.List (permutations)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Lattice.Backend (Backend (..), EntityRow (..), LoadResult (..))
import Lattice.Backend.Memory (MemoryHooks (..), defaultHooks, memoryBackend, newMemoryDb, putRow)
import Lattice.Client (LatticeClient)
import Lattice.IDL.Parser (SchemaError (..), parseSchema)
import Lattice.IDL.Print (canonicalIdl)
import Lattice.Module
import Lattice.Schema (EntityDef (..), Schema, lookupEntity)
import Lattice.Types
import Lattice.Wire (EntityRecord (..), Record (..), hLatticeSnapshot, queryMediaType)
import Network.HTTP.Types.Method (Method (..))
import Test.Lattice.Fixtures (requireRight)
import Test.Lattice.Loop
import Test.Syd


tests :: Spec
tests =
  describe "Schema modules and fusion (§18.1/§18.2)" $ do
    describe "§18.2 fusion is deterministic and order-insensitive" $ do
      it "every permutation of the modules yields byte-identical fusedIdl" $ do
        base <- fuseRight [postsModule, socialModule, badgesModule]
        sequence_
          [ do
              f <- fuseRight perm
              fusedIdl f `shouldBe` fusedIdl base
          | perm <- permutations [postsModule, socialModule, badgesModule]
          ]

      it "the fused IDL is one plain schema: no extend blocks, reparses to fusedSchema" $ do
        f <- fuseRight [postsModule, socialModule]
        fusedIdl f `shouldNotSatisfy` T.isInfixOf "extend"
        parseSchema (fusedIdl f) `shouldBe` Right (fusedSchema f)

      it "extension members fold into the owning entity's declaration" $ do
        f <- fuseRight [postsModule, socialModule]
        folded <- requireRight (parseSchema (fusedIdl f))
        entityFieldNames "Post" folded `shouldSatisfy` elem "score"
        entityRelNames "Post" folded `shouldSatisfy` elem "reactions"

    describe "§18.1 provenance lives in Fused, not in the text" $ do
      it "fusedOwner maps each type to its declaring module" $ do
        f <- fuseRight [postsModule, socialModule, badgesModule]
        Map.lookup "Post" (fusedOwner f) `shouldBe` Just "posts"
        Map.lookup "Reaction" (fusedOwner f) `shouldBe` Just "social"
        Map.lookup "Badge" (fusedOwner f) `shouldBe` Just "badges"

      it "root and mutation owners route by declaring module" $ do
        f <- fuseRight [postsModule, socialModule, badgesModule]
        Map.lookup "post" (fusedRootOwner f) `shouldBe` Just "posts"
        Map.lookup "badge" (fusedRootOwner f) `shouldBe` Just "badges"

      it "fusedFieldOwner carries extension members ONLY; owner fields default via fusedOwner" $ do
        f <- fuseRight [postsModule, socialModule]
        Map.lookup ("Post", "score") (fusedFieldOwner f) `shouldBe` Just "social"
        Map.lookup ("Post", "reactions") (fusedFieldOwner f) `shouldBe` Just "social"
        Map.lookup ("Post", "title") (fusedFieldOwner f) `shouldBe` Nothing

    describe "§18.1 extend entity prints canonically inside the module's own IDL" $ do
      it "the extending module's canonical IDL keeps the extend block" $ do
        social <- requireRight (parseSchema (smIdl socialModule))
        canonicalIdl social `shouldSatisfy` T.isInfixOf "extend entity Post"

      it "canonical IDL of an extending module is a fixpoint" $ do
        social <- requireRight (parseSchema (smIdl socialModule))
        fmap canonicalIdl (parseSchema (canonicalIdl social))
          `shouldBe` Right (canonicalIdl social)

    describe "§18.1 fusion conflicts name the offender" $ do
      it "two owners for one entity type (even textually identical)" $
        fuseExpecting
          [ mkModule "a" ["entity Thing by id {", "  visible to all by default", "", "  id: Text", "", "  fetch by id: public", "}"]
          , mkModule "b" ["entity Thing by id {", "  visible to all by default", "", "  id: Text", "", "  fetch by id: public", "}"]
          ]
          (FETwoOwners "Thing" "a" "b")

      it "one extension member declared by two modules" $
        fuseExpecting
          [ postsModule
          , mkModule "x" ["extend entity Post {", "  score: I32", "}"]
          , mkModule "y" ["extend entity Post {", "  score: I32", "}"]
          ]
          (FEMemberConflict "Post" "score" "x" "y")

      it "an extension redeclaring a field the owner already has" $
        fuseExpecting
          [ postsModule
          , mkModule "x" ["extend entity Post {", "  title: Text", "}"]
          ]
          (FEExtensionRedeclaresMember "x" "Post" "title")

      it "a claim declared with two types" $
        fuseExpecting
          [ mkModule "a" ["newtype OrgId = Text", "", "claims {", "  org: OrgId", "}"]
          , mkModule "b" ["claims {", "  org: Text", "}"]
          ]
          (FEClaimConflict "org" "a" "b")

      it "a root name declared by two modules" $
        fuseExpecting
          [ postsModule
          , mkModule "b" $
              [ "newtype ItemId = Text"
              , ""
              , "entity Item by id {"
              , "  visible to all by default"
              , ""
              , "  id: ItemId"
              , ""
              , "  fetch by id: public"
              , "}"
              , ""
              , "get post(id: ItemId) of Item public"
              ]
          ]
          (FENameConflict "root" "post" "b" "posts")

      it "extension of a type no module owns" $
        fuseExpecting
          [ postsModule
          , mkModule "x" ["extend entity Ghost {", "  score: I32", "}"]
          ]
          (FEUnknownExtendedType "x" "Ghost")

      it "an extension redeclaring a key field" $
        fuseExpecting
          [ postsModule
          , mkModule "x" ["extend entity Post {", "  id: Text", "}"]
          ]
          (FEExtensionRedeclaresKey "x" "Post" "id")

      it "an extension redeclaring the visibility default" $
        fuseExpecting
          [ postsModule
          , mkModule "x" ["extend entity Post {", "  private by default", "", "  score: I32", "}"]
          ]
          (FEExtensionRedeclaresDefault "x" "Post")

      it "an extension redeclaring fetch by" $
        fuseExpecting
          [ postsModule
          , mkModule "x" ["extend entity Post {", "  score: I32", "", "  fetch by id: public", "}"]
          ]
          (FEExtensionRedeclaresFetchBy "x" "Post")

      it "an extension declaring a co-key" $
        fuseExpecting
          [ postsModule
          , mkModule
              "x"
              [ "newtype AnchorId = Text"
              , ""
              , "entity Anchor by id {"
              , "  visible to all by default"
              , ""
              , "  id: AnchorId"
              , ""
              , "  fetch by id: public"
              , "}"
              , ""
              , "extend entity Post joins Anchor {"
              , "  score: I32"
              , "}"
              ]
          ]
          (FEExtensionDeclaresCoKey "x" "Post")

      it "a whole-schema check failing only once the pieces meet (fused re-elaboration)" $ do
        -- The extension's stored field carries no policy, so it falls to
        -- the entity default. Locally the extension approximates that
        -- default as public and elaborates alone; fused under the owner's
        -- REAL `private by default`, the public derived field reads a
        -- private own() dep — §8.1 information flow, visible only after
        -- folding. (A collection-count dep would NOT trigger this: an
        -- omitted edge policy joins as public by the §8.1 edge rule.)
        let owner =
              mkModule
                "own"
                [ "newtype ThingId = Text"
                , ""
                , "entity Thing by id {"
                , "  private by default"
                , ""
                , "  id: ThingId"
                , ""
                , "  fetch by id: private"
                , "}"
                , ""
                , "get thing(id: ThingId) of Thing private"
                ]
            ext =
              mkModule
                "ext"
                [ "extend entity Thing {"
                , "  wordCount: I32"
                , "  loud: I32 derived reads own(wordCount) on read public"
                , "}"
                ]
        case fuseModules (NE.fromList [owner, ext]) of
          Right _ -> expectationFailure "the fused information-flow violation went unnoticed"
          Left errs -> case [es | FEFusedSchemaInvalid es <- errs] of
            [es] -> map seMessage es `shouldSatisfy` any (T.isInfixOf "loud")
            other -> expectationFailure ("expected one FEFusedSchemaInvalid, got: " <> show other)

      it "a module whose IDL does not parse" $ do
        let broken = SchemaModule {smName = "broken", smIdl = "schema broken.example.com\n\nentity {"}
        case fuseModules (NE.fromList [postsModule, broken]) of
          Right _ -> expectationFailure "a broken module unexpectedly fused"
          Left errs ->
            [m | FEModuleParse m _ <- errs] `shouldBe` ["broken"]

      it "two modules under one name" $ do
        let a = mkModule "dup" ["newtype AId = Text", "", "entity A by id {", "  visible to all by default", "", "  id: AId", "", "  fetch by id: public", "}"]
            b = mkModule "dup" ["newtype BId = Text", "", "entity B by id {", "  visible to all by default", "", "  id: BId", "", "  fetch by id: public", "}"]
        fuseExpecting [a, b] (FEDuplicateModuleName "dup")

    describe "§18.1 disjoint union with dedupe" $ do
      it "identical claim declarations dedupe" $ do
        f <-
          fuseRight
            [ withClaims postsModule
            , mkModule "other" ["newtype OrgId = Text", "", "claims {", "  org: OrgId", "}"]
            ]
        -- One org claim in the fused text, not a conflict and not a duplicate.
        T.count "org:" (fusedIdl f) `shouldBe` 1

      it "identically redeclared vocabulary types dedupe (the shared-newtype seam)" $ do
        -- socialModule redeclares PostId verbatim; fusion keeps one.
        f <- fuseRight [postsModule, socialModule]
        T.count "newtype PostId" (fusedIdl f) `shouldBe` 1

    describe "§18.1 fuseBackends: a query through the seam" $ do
      it "the tree joins owner fields, extension fields, and the extension edge" $
        withFused $ \loop _postsB ->
          fusedClient loop $ \lc -> do
            r <- runQuery lc seamQ Map.empty
            post <- rootValue "post" r >>= asArray >>= exactlyOne
            title <- textField "title" post
            title `shouldBe` "First"
            score <- objectField "score" post
            score `shouldBe` A.Number 3
            kinds <- fieldByPrefix "reactions" post >>= objectField "items" >>= asArray >>= traverse (textField "kind")
            kinds `shouldBe` ["clap", "like"]

      it "ONE merged entity record per entity; ver is the owner's (in-process pin)" $
        withFused $ \loop postsB -> do
          r <-
            httpRaw
              loop
              POST
              "/q?intent=oneshot&slice=pub"
              [("Content-Type", queryMediaType)]
              (Just (encodeUtf8 seamQ))
          rawStatus r `shouldBe` 200
          case [er | REntity er <- rawRecords r, erId er == Ref "Post" "p1"] of
            [er] -> do
              Map.lookup "title" (erFields er) `shouldBe` Just (A.String "First")
              Map.lookup "score" (erFields er) `shouldBe` Just (A.Number 3)
              ownerVer <- postOwnerVer postsB
              erVer er `shouldBe` ownerVer
            other -> expectationFailure ("expected exactly one merged Post:p1 record, got: " <> show other)

      it "beSnapshot namespaces per-module domains in the vector (§18.4 shape)" $
        withFused $ \loop _postsB -> do
          r <-
            httpRaw
              loop
              POST
              "/q?intent=oneshot&slice=pub"
              [("Content-Type", queryMediaType)]
              (Just (encodeUtf8 seamQ))
          rawStatus r `shouldBe` 200
          case rawHeader hLatticeSnapshot r of
            Nothing -> expectationFailure "the seam response carries no Lattice-Snapshot"
            Just tok -> do
              tok `shouldSatisfy` BS8.isInfixOf "posts/"
              tok `shouldSatisfy` BS8.isInfixOf "social/"


-- ---------------------------------------------------------------------------
-- Fixture modules
-- ---------------------------------------------------------------------------

postsModule :: SchemaModule
postsModule =
  mkModule
    "posts"
    [ "newtype PostId = Text"
    , ""
    , "entity Post by id {"
    , "  visible to all by default"
    , ""
    , "  id:    PostId"
    , "  title: Text"
    , ""
    , "  fetch by id: public"
    , "}"
    , ""
    , "get post(id: PostId) of Post public"
    ]


-- | Extends the foreign @Post@ with a stored field and a @has many@
-- edge; redeclares @PostId@ verbatim (modules resolve names locally,
-- identical vocabulary dedupes at fusion).
socialModule :: SchemaModule
socialModule =
  mkModule
    "social"
    [ "newtype PostId     = Text"
    , "newtype ReactionId = Text"
    , ""
    , "entity Reaction by id {"
    , "  visible to all by default"
    , ""
    , "  id:     ReactionId"
    , "  postId: PostId"
    , "  kind:   Text"
    , ""
    , "  fetch by id: public"
    , "}"
    , ""
    , "extend entity Post {"
    , "  score: I32"
    , ""
    , "  has many reactions: Reaction by postId"
    , "                      ordered by kind asc"
    , "                      page 10 max 50"
    , "}"
    ]


badgesModule :: SchemaModule
badgesModule =
  mkModule
    "badges"
    [ "newtype BadgeId = Text"
    , ""
    , "entity Badge by id {"
    , "  visible to all by default"
    , ""
    , "  id:    BadgeId"
    , "  label: Text"
    , ""
    , "  fetch by id: public"
    , "}"
    , ""
    , "get badge(id: BadgeId) of Badge public"
    ]


mkModule :: Text -> [Text] -> SchemaModule
mkModule name body =
  SchemaModule
    { smName = name
    , smIdl = T.unlines (("schema " <> name <> ".example.com") : "" : body)
    }


withClaims :: SchemaModule -> SchemaModule
withClaims m =
  m {smIdl = smIdl m <> T.unlines ["", "newtype OrgId = Text", "", "claims {", "  org: OrgId", "}"]}


fuseRight :: [SchemaModule] -> IO Fused
fuseRight = requireRight . fuseModules . NE.fromList


fuseExpecting :: [SchemaModule] -> FusionError -> IO ()
fuseExpecting mods err = case fuseModules (NE.fromList mods) of
  Right _ -> expectationFailure ("fusion unexpectedly succeeded; wanted " <> show err)
  Left errs -> errs `shouldSatisfy` elem err


-- ---------------------------------------------------------------------------
-- The fused origin
-- ---------------------------------------------------------------------------

seamQ :: Text
seamQ = "query Seam { post(id: \"p1\") { title score reactions(first: 5) { kind } } }"


{- | A loopback origin over the FUSED schema whose backend is
'fuseBackends' over two per-module memory backends: @posts@ owns
@Post:p1@ (title), @social@ holds the extension row (score) under the
same key plus the @Reaction@ table. The owner's get root rides the
posts module's hooks; the extension edge resolves through social's
schema-driven child scan.
-}
withFused :: (Loop -> Backend -> IO a) -> IO a
withFused k = do
  f <- fuseRight [postsModule, socialModule]
  postsDb <- newMemoryDb
  socialDb <- newMemoryDb
  atomically $ do
    putRow postsDb "Post" "p1" (Map.fromList [("id", A.String "p1"), ("title", A.String "First")])
    putRow socialDb "Post" "p1" (Map.fromList [("score", A.Number 3)])
    putRow socialDb "Reaction" "r1" (Map.fromList [("id", A.String "r1"), ("postId", A.String "p1"), ("kind", A.String "clap")])
    putRow socialDb "Reaction" "r2" (Map.fromList [("id", A.String "r2"), ("postId", A.String "p1"), ("kind", A.String "like")])
  postsSchema <- requireRight (parseSchema (smIdl postsModule))
  socialSchema <- requireRight (parseSchema (smIdl socialModule))
  let postsB = memoryBackend postsSchema postsDb postsHooks
      socialB = memoryBackend socialSchema socialDb defaultHooks
      fusedB = fuseBackends (Map.fromList [("posts", postsB), ("social", socialB)]) f
  withLoop
    (loopSpec (fusedSchema f)) {lsWrap = const fusedB}
    (\loop -> k loop postsB)
  where
    postsHooks =
      defaultHooks
        { mhGetRoots =
            Map.fromList
              [ ( "post"
                , \_db args -> pure $ case Map.lookup "id" args of
                    Just (A.String key) -> Just (Ref "Post" key)
                    _ -> Nothing
                )
              ]
        }


fusedClient :: Loop -> (LatticeClient -> IO a) -> IO a
fusedClient loop = clientFor loop id


-- | The owner backend's current version of @Post:p1@.
postOwnerVer :: Backend -> IO Text
postOwnerVer postsB = do
  loaded <- beLoad postsB "Post" ["p1"]
  case Map.lookup "p1" loaded of
    Just (Right (RowFound row)) -> pure (rowVer row)
    other -> expectationFailure ("the posts backend does not hold Post:p1: " <> show other)


-- ---------------------------------------------------------------------------
-- Schema model probes
-- ---------------------------------------------------------------------------

entityFieldNames :: TypeName -> Schema -> [FieldName]
entityFieldNames ty s = maybe [] (Map.keys . entityFields) (lookupEntity s ty)


entityRelNames :: TypeName -> Schema -> [FieldName]
entityRelNames ty s = maybe [] (Map.keys . entityRels) (lookupEntity s ty)


-- | Get roots render as one-element arrays in the data tree.
exactlyOne :: (Show a) => [a] -> IO a
exactlyOne = \case
  [x] -> pure x
  other -> expectationFailure ("expected exactly one element, got: " <> show other)
