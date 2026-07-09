{- | Load projections (spec /Projections/, "Lattice.Backend"): per entity
type, 'planProjections' is the static upper bound of the stored fields the
executor reads off 'beLoad' rows.

* __Unit__ — selected scalars project exactly; a traversed @has one@ edge
  projects its link field on the parent; a row-comparing field policy
  projects the compared field; declared arguments widen the type to
  'ProjectAll'; an @on read@ read set projects @own(…)@ names, the edge
  link, the dep fragment's fields on the target, and non-link grouping
  overrides; untouched types are absent; @\@depth@ knots terminate.
* __Explain__ — @explain@ carries a @projections@ object: @"*"@ for
  'ProjectAll', a sorted field array otherwise.
* __E2E__ — the executor hands each 'beLoad' exactly the plan's per-type
  projection (narrow, never 'ProjectAll'), and the strictly projected
  memory rows still serve the edge; point fetches load 'ProjectAll'.
* __Property__ — a plan's projection covers every selected stored field
  (superset invariant), over random field subsets.
-}
module Test.Lattice.Projection (tests) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO, writeTVar)
import Control.Exception (evaluate)
import Data.Aeson qualified as A
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Lattice.Backend (Backend (..), Projection (..), projectsField)
import Lattice.Backend.Memory (MemoryHooks (..), defaultHooks)
import Lattice.Plan (Plan, explainJson, planProjections, planQuery)
import Lattice.Schema (Schema, defaultBudgets)
import Lattice.Types (FieldName (..), Ref (..), TypeName)
import Lattice.Wire (EntityRecord (..), Record (..), queryMediaType)
import Network.HTTP.Types.Method (Method (..))
import Test.Lattice.Fixtures
import Test.Lattice.Loop
import Test.Syd
import Test.Syd.Hedgehog ()


tests :: Spec
tests =
  describe "load projections" $ do
    it "a scalar subset projects exactly the selected names" $ do
      p <- planOf projSchema scalarsQ
      planProjections projSchema p
        `shouldBe` Map.singleton "Post" (ProjectFields (Set.fromList ["rank", "title"]))

    it "a traversed has-one edge projects its link field on the parent, the selection on the target" $ do
      p <- planOf projSchema authorQ
      planProjections projSchema p
        `shouldBe` Map.fromList
          [ ("Author", ProjectFields (Set.fromList ["name"]))
          , -- authorId rides along unselected: the executor reads it off
            -- the parent row to resolve (and emit) the ref.
            ("Post", ProjectFields (Set.fromList ["authorId", "title"]))
          ]

    it "a row-comparing field policy projects the compared field" $ do
      p <- planOf projSchema secretQ
      planProjections projSchema p
        `shouldBe` Map.singleton "Post" (ProjectFields (Set.fromList ["orgId", "secret"]))

    it "a selected field with declared arguments widens its type to ProjectAll" $ do
      p <- planOf projSchema computedQ
      -- beComputed receives the whole row: title's narrow set is absorbed.
      planProjections projSchema p `shouldBe` Map.singleton "Post" ProjectAll

    it "an on-read read set projects own fields, edge links, fragment fields, and grouping overrides" $ do
      p <- planOf projSchema derivedQ
      planProjections projSchema p
        `shouldBe` Map.fromList
          [ -- The ViaEdge dep's fragment fields land on the target …
            ("Author", ProjectFields (Set.fromList ["name"]))
          , -- … and the owner reads own(title, rank), the edge's hidden
            -- link, and the ViaCollection dep's non-link grouping
            -- override (orgId; the link postId reads the parent key).
            -- summary itself is computed, never read off the row.
            ("Post", ProjectFields (Set.fromList ["authorId", "orgId", "rank", "title"]))
          ]

    it "types the plan never loads are absent from the map" $ do
      p <- planOf projSchema scalarsQ
      -- Author, Comment, and Category are all declared and reachable,
      -- but this plan never loads them.
      Map.keys (planProjections projSchema p) `shouldBe` ["Post"]

    it "a @depth edge terminates and projects the expansion's fields" $ do
      p <- planOf projSchema depthQ
      let projs = planProjections projSchema p
          expected = Map.singleton "Category" (ProjectFields (Set.fromList ["name", "parentId"]))
      -- The depth expansion is a cyclically tied selection: computing the
      -- comparison at all is the termination proof (loud timeout, no hang).
      _ <- io "planProjections over a @depth edge" (evaluate (projs == expected))
      projs `shouldBe` expected

    it "explain renders projections: \"*\" for ProjectAll, sorted field arrays otherwise" $ do
      pAll <- planOf projSchema computedQ
      projectionsOf pAll >>= (`shouldBe` A.object [("Post", A.String "*")])
      pSub <- planOf projSchema authorQ
      projectionsOf pSub
        >>= ( `shouldBe`
                A.object
                  [ ("Author", A.toJSON (["name"] :: [Text]))
                  , ("Post", A.toJSON (["authorId", "title"] :: [Text]))
                  ]
            )

    it "the executor hands beLoad the plan's per-type projection; point fetches load ProjectAll" $ do
      calls <- newTVarIO ([] :: [(TypeName, Projection)])
      withProjLoop calls $ \loop -> do
        p <- planOf projSchema authorQ
        let plannedFor t = Map.findWithDefault ProjectAll t (planProjections projSchema p)
        -- Narrow on purpose: the threading claim is empty without it.
        plannedFor "Post" `shouldBe` ProjectFields (Set.fromList ["authorId", "title"])
        r <- oneshotPost loop authorQ
        rawStatus r `shouldBe` 200
        recorded <- readTVarIO calls
        -- Exactly one load per type per round, newest first, each
        -- carrying the plan's projection for its type.
        recorded `shouldBe` [("Author", plannedFor "Author"), ("Post", plannedFor "Post")]
        -- The memory backend filters rows to the projection strictly:
        -- the edge still resolves and its ref emits, though authorId
        -- was never selected.
        post <- entityByRef (Ref "Post" "p1") r
        Map.lookup "title" (erFields post) `shouldBe` Just (A.String "Hello")
        Map.lookup "author" (erFields post)
          `shouldBe` Just (A.object [("$ref", A.String "Author:a1")])
        author <- entityByRef (Ref "Author" "a1") r
        Map.lookup "name" (erFields author) `shouldBe` Just (A.String "Ada")
        -- Point fetches are planless whole-row reads.
        atomically (writeTVar calls [])
        pf <- httpRaw loop GET "/e/Post/p1?f=title" [] Nothing
        rawStatus pf `shouldBe` 200
        readTVarIO calls >>= (`shouldBe` [("Post", ProjectAll)])

    it "every selected stored field is projected (superset invariant)" $
      H.withTests 100 $
        H.property $ do
          fields <- H.forAll (Gen.filter (not . null) (Gen.subsequence postStoredFields))
          c <- H.evalEither (compileWith projSchema (subsetQ fields))
          p <- H.evalEither (planQuery projSchema defaultBudgets c)
          let proj = Map.findWithDefault ProjectAll "Post" (planProjections projSchema p)
          H.annotateShow proj
          H.assert (all (projectsField proj . FieldName) fields)


-- ---------------------------------------------------------------------------
-- Fixture: every projection source on one small schema
-- ---------------------------------------------------------------------------

{- | Posts with a row-compared field policy (@secret@), a computed field
with declared arguments (@excerpt@), an @on read@ derivation reading all
three dep shapes (@summary@), a to-one edge, a collection with a non-link
@grouped by@ override, and a self-recursive @\@depth@-able edge (Category).
-}
projText :: Text
projText =
  T.unlines
    [ "schema proj.example.com"
    , ""
    , "newtype PostId     = Text"
    , "newtype AuthorId   = Text"
    , "newtype CommentId  = Text"
    , "newtype OrgId      = Text"
    , "newtype CategoryId = Text"
    , ""
    , "claims {"
    , "  org: OrgId"
    , "}"
    , ""
    , "entity Post by id {"
    , "  visible to all by default"
    , ""
    , "  id:       PostId"
    , "  orgId:    OrgId"
    , "  authorId: AuthorId"
    , "  title:    Text"
    , "  body:     Text"
    , "  rank:     F64"
    , "  secret:   Text visible when caller.org = orgId"
    , "  excerpt(len: I32 = 100): Text"
    , ""
    , "  summary: Text derived reads own(title, rank), author ...AuthorByline, comments count on read"
    , ""
    , "  has one author: Author by authorId"
    , ""
    , "  has many comments: Comment by postId"
    , "                     ordered by createdAt asc"
    , "                     page 10 max 100"
    , "                     grouped by orgId, postId"
    , ""
    , "  fetch by id: public"
    , "}"
    , ""
    , "entity Author by id {"
    , "  visible to all by default"
    , ""
    , "  id:   AuthorId"
    , "  name: Text"
    , "  bio:  Text"
    , ""
    , "  fetch by id: public"
    , "}"
    , ""
    , "entity Comment by id {"
    , "  visible to all by default"
    , ""
    , "  id:        CommentId"
    , "  postId:    PostId"
    , "  orgId:     OrgId"
    , "  body:      Text"
    , "  createdAt: Timestamp"
    , ""
    , "  fetch by id: public"
    , "}"
    , ""
    , "entity Category by id {"
    , "  visible to all by default"
    , ""
    , "  id:       CategoryId"
    , "  name:     Text"
    , "  parentId: CategoryId?"
    , ""
    , "  has one? parent: Category by parentId"
    , ""
    , "  fetch by id: public"
    , "}"
    , ""
    , "fragment AuthorByline on Author { name }"
    , ""
    , "get post(id: PostId) of Post public"
    , "get category(id: CategoryId) of Category public"
    ]


projSchema :: Schema
projSchema = mustParseSchema projText
{-# NOINLINE projSchema #-}


-- ---------------------------------------------------------------------------
-- Queries
-- ---------------------------------------------------------------------------

scalarsQ :: Text
scalarsQ = "query { post(id: \"p1\") { title rank } }"


authorQ :: Text
authorQ = "query { post(id: \"p1\") { title author { name } } }"


secretQ :: Text
secretQ = "query { post(id: \"p1\") { secret } }"


computedQ :: Text
computedQ = "query { post(id: \"p1\") { excerpt title } }"


derivedQ :: Text
derivedQ = "query { post(id: \"p1\") { summary } }"


depthQ :: Text
depthQ = "query { category(id: \"c1\") { name parent @depth(3) } }"


-- | Post's plain stored fields (no policies' RhsFields, no derivations):
-- the property's selection pool.
postStoredFields :: [Text]
postStoredFields = ["authorId", "body", "id", "orgId", "rank", "secret", "title"]


subsetQ :: [Text] -> Text
subsetQ fields = "query { post(id: \"p1\") { " <> T.unwords fields <> " } }"


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

planOf :: Schema -> Text -> IO Plan
planOf schema q = do
  c <- mustCompileWith schema q
  requireRight (planQuery schema defaultBudgets c)


-- | The @projections@ object of a plan's explain document.
projectionsOf :: Plan -> IO A.Value
projectionsOf = objectField "projections" . explainJson projSchema defaultBudgets


-- ---------------------------------------------------------------------------
-- E2E: the fixture served in-process, recording every beLoad projection
-- ---------------------------------------------------------------------------

withProjLoop :: TVar [(TypeName, Projection)] -> (Loop -> IO a) -> IO a
withProjLoop calls =
  withLoop
    (loopSpec projSchema)
      { lsRows = projRows
      , lsHooks = projHooks
      , lsWrap = \inner ->
          inner
            { beLoad = \ty proj keys -> do
                atomically (modifyTVar' calls ((ty, proj) :))
                beLoad inner ty proj keys
            }
      }


projRows :: [(TypeName, Map FieldName A.Value)]
projRows =
  [ ("Author", Map.fromList [("id", A.String "a1"), ("name", A.String "Ada"), ("bio", A.String "first programmer")])
  ,
    ( "Post"
    , Map.fromList
        [ ("id", A.String "p1")
        , ("orgId", A.String "o1")
        , ("authorId", A.String "a1")
        , ("title", A.String "Hello")
        , ("body", A.String "world")
        , ("rank", A.Number 1)
        , ("secret", A.String "classified")
        ]
    )
  ]


projHooks :: MemoryHooks
projHooks = defaultHooks {mhGetRoots = Map.fromList [("post", byIdRoot "Post")]}
  where
    byIdRoot ty _db args = pure $ case Map.lookup "id" args of
      Just (A.String k) -> Just (Ref ty k)
      _ -> Nothing


oneshotPost :: Loop -> Text -> IO RawResp
oneshotPost loop body =
  httpRaw
    loop
    POST
    "/q?intent=oneshot&slice=pub"
    [("Content-Type", queryMediaType)]
    (Just (encodeUtf8 body))


-- | The single entity record of a response carrying the ref.
entityByRef :: Ref -> RawResp -> IO EntityRecord
entityByRef ref r = case filter ((== ref) . erId) (mapMaybe pick (rawRecords r)) of
  [er] -> pure er
  ers -> expectationFailure ("expected exactly one entity record for " <> show ref <> ", got: " <> show ers)
  where
    pick = \case
      REntity er -> Just er
      _ -> Nothing
