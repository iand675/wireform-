{- | Plan contracts (spec §7.3, §8.1, §14.1, §3.8, §3.4–§3.6): the
path-join slice partition, plan identity moving with pertinent
declarations only (co-key declarations and their base transitively
included, cardinality declarations included), and the static plan budgets
naming their bound.
-}
module Test.Lattice.Plan (tests) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Lattice.Hash (schemaHash)
import Lattice.IDL.Print (canonicalIdl)
import Lattice.Plan (Plan (..), planQuery, planSliceRecord)
import Lattice.Query.Validate (CompileError (..))
import Lattice.Schema (
  Budgets (..),
  Schema,
  defaultBudgets,
  schemaDescriptions,
  schemaDirectiveDecls,
  schemaDirectives,
 )
import Lattice.Types (SliceName (..))
import Lattice.Wire (PlanRecord (..), SliceInfo (..))
import Test.Lattice.Fixtures
import Test.Syd


tests :: Spec
tests =
  describe "Plan (§7.3/§8.1)" $ do
    describe "§8.1 path join partitions slices" $ do
      it "blog FeedPage: pub slice absent, ctx depends on {org} via feed" $ do
        p <- planOf blogSchema feedPageText
        Map.member SlicePub (planSlices p) `shouldBe` False
        Map.member SlicePriv (planSlices p) `shouldBe` False
        Map.lookup SliceCtx (planSlices p)
          `shouldBe` Just (SliceInfo ["org"] ["feed"])
      it "starwars Hero: pub only (public root, public fields)" $ do
        p <- planOf starwarsSchema heroQ
        Map.keys (planSlices p) `shouldBe` [SlicePub]
        Map.lookup SlicePub (planSlices p)
          `shouldBe` Just (SliceInfo [] ["hero"])
      it "planSliceRecord carries the partition and both identities (§6.6)" $ do
        p <- planOf blogSchema feedPageText
        let pr = planSliceRecord p
        prQuery pr `shouldBe` planQueryHash p
        prPlan pr `shouldBe` planId p
        prSlices pr `shouldBe` planSlices p

    describe "§7.3 plan identity moves with pertinent declarations only" $ do
      it "editing a selected field's policy (Post.title) moves the planId" $ do
        base <- planOf blogSchema feedPageText
        edited <- planOf blogTitleGated feedPageText
        -- Same canonical text, same query hash: identity is text-only …
        planQueryHash edited `shouldBe` planQueryHash base
        -- … but the plan binds the pertinent declarations, which changed.
        planId edited `shouldNotBe` planId base
      it "adding an unrelated entity leaves the planId unchanged" $ do
        base <- planOf blogSchema feedPageText
        widened <- planOf blogPlusWidget feedPageText
        planQueryHash widened `shouldBe` planQueryHash base
        planId widened `shouldBe` planId base

    describe "§3.8 co-keyed declarations are pertinent (§7.3/§17.2)" $ do
      it "an identity-edge walk plans as an ordinary to-one (both directions, pub-only)" $ do
        p <- planOf cokeySchema identityWalkQ
        Map.keys (planSlices p) `shouldBe` [SlicePub]
        Map.lookup SlicePub (planSlices p)
          `shouldBe` Just (SliceInfo [] ["user"])
      it "identity edges consume depth like any to-one" $ do
        c <- mustCompileWith cokeySchema identityWalkQ
        planQuery cokeySchema defaultBudgets {maxDepth = 2} c
          `rejectsNaming` "maxDepth budget 2"
      it "flipping refines to joins moves a refinement query's planId" $ do
        base <- planOf cokeySchema adminQ
        flipped <- planOf cokeyAdminJoins adminQ
        -- Same canonical text, same query hash: identity is text-only …
        planQueryHash flipped `shouldBe` planQueryHash base
        -- … but the truth coupling is a pertinent declaration (§3.8).
        planId flipped `shouldNotBe` planId base
      it "editing the base entity moves a refinement query's planId (transitive pertinence)" $ do
        base <- planOf cokeySchema adminQ
        widened <- planOf cokeyUserWidened adminQ
        planQueryHash widened `shouldBe` planQueryHash base
        planId widened `shouldNotBe` planId base
      it "adding a new co-keyed entity is additive: existing planIds do not move (§17.2)" $ do
        baseUser <- planOf cokeySchema userOnlyQ
        baseAdmin <- planOf cokeySchema adminQ
        widenedUser <- planOf cokeyPlusAuditor userOnlyQ
        widenedAdmin <- planOf cokeyPlusAuditor adminQ
        planQueryHash widenedUser `shouldBe` planQueryHash baseUser
        planId widenedUser `shouldBe` planId baseUser
        planId widenedAdmin `shouldBe` planId baseAdmin

    describe "§3.4/§3.6 cardinality declarations are pertinent (§7.3)" $ do
      it "flipping `has one?` to `has one` moves a touching query's planId" $ do
        base <- planOf cokeySchema identityWalkQ
        flipped <- planOf cokeyProfileRequired identityWalkQ
        -- Same canonical text, same query hash: identity is text-only …
        planQueryHash flipped `shouldBe` planQueryHash base
        -- … but the edge's cardinality is a pertinent declaration (§3.4).
        planId flipped `shouldNotBe` planId base
      it "adding `min` to a bounded collection moves a selecting query's planId" $ do
        base <- planOf blogSchema feedTagsPlanQ
        floored <- planOf blogTagsFloored feedTagsPlanQ
        planQueryHash floored `shouldBe` planQueryHash base
        planId floored `shouldNotBe` planId base
      it "the floor is not pertinent to a query never touching the declaring entity" $ do
        base <- planOf blogSchema meQ
        floored <- planOf blogTagsFloored meQ
        planQueryHash floored `shouldBe` planQueryHash base
        planId floored `shouldBe` planId base

    describe "§14.1 plan budgets reject naming the bound" $ do
      it "maxDepth: a depth-3 traversal against maxDepth=2" $ do
        c <- mustCompileWith blogSchema "query { feed { comments { author { name } } } }"
        planQuery blogSchema defaultBudgets {maxDepth = 2} c
          `rejectsNaming` "maxDepth budget 2"
      it "maxRoots: a two-root query against maxRoots=1" $ do
        c <-
          mustCompileWith
            starwarsSchema
            "query { hero { name } reviews(episode: Empire) { stars } }"
        planQuery starwarsSchema defaultBudgets {maxRoots = 1} c
          `rejectsNaming` "maxRoots budget is 1"
      it "maxRoundFanout: a page-10 round against maxRoundFanout=5" $ do
        c <- mustCompileWith starwarsSchema heroQ
        planQuery starwarsSchema defaultBudgets {maxRoundFanout = 5} c
          `rejectsNaming` "maxRoundFanout budget 5"

    describe "§3.9/§7.3 documentation + directives are metadata, not plan pertinence" $ do
      let bareDirectives =
            directivesSchema
              { schemaDescriptions = mempty
              , schemaDirectives = mempty
              , schemaDirectiveDecls = mempty
              }
          getUserQ = "query GetUser($id: UserId) { user(id: $id) { name } }"
      it "stripping every directive and description leaves the planId identical" $ do
        full <- planOf directivesSchema getUserQ
        bare <- planOf bareDirectives getUserQ
        planId bare `shouldBe` planId full
      it "but the published schema hash moves (they are part of the canonical document)" $
        schemaHash (canonicalIdl bareDirectives)
          `shouldNotBe` schemaHash (canonicalIdl directivesSchema)


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

heroQ :: Text
heroQ = "query Hero { hero { name friends(first: 10) { name } } }"


-- | A query touching only the refinement (its base is pertinent transitively).
adminQ :: Text
adminQ = "query Admin($id: UserId) { admin(id: $id) { permissions } }"


-- | A query touching only the base entity.
userOnlyQ :: Text
userOnlyQ = "query UserOnly($id: UserId) { user(id: $id) { name } }"


-- | Identity edges walked in both directions: User → profile → user.
identityWalkQ :: Text
identityWalkQ = "query Walk($id: UserId) { user(id: $id) { name profile { bio user { name } } } }"


planOf :: Schema -> Text -> IO Plan
planOf schema q = do
  c <- mustCompileWith schema q
  requireRight (planQuery schema defaultBudgets c)


-- | The blog schema with @Post.title@ gated behind an Admin claim: a
-- pertinent declaration of any query selecting @title@.
blogTitleGated :: Schema
blogTitleGated =
  mustParseSchema
    (T.replace "title:      Text" "title: Text visible when caller.role = Admin" blogText)
{-# NOINLINE blogTitleGated #-}


-- | The blog schema plus an entity no query here touches.
blogPlusWidget :: Schema
blogPlusWidget =
  mustParseSchema $
    blogText
      <> T.unlines
        [ ""
        , "entity Widget by id {"
        , "  visible to all by default"
        , "  id: Text"
        , "}"
        ]
{-# NOINLINE blogPlusWidget #-}


-- | The cokey schema with AdminUser's truth coupling flipped refines→joins.
cokeyAdminJoins :: Schema
cokeyAdminJoins =
  mustParseSchema
    (T.replace "entity AdminUser refines User" "entity AdminUser joins User" cokeyText)
{-# NOINLINE cokeyAdminJoins #-}


-- | The cokey schema with a field added to the BASE entity only.
cokeyUserWidened :: Schema
cokeyUserWidened =
  mustParseSchema
    (T.replace "  name: Text" "  name: Text\n  nickname: Text" cokeyText)
{-# NOINLINE cokeyUserWidened #-}


-- | The cokey schema plus a second refinement no query here touches.
cokeyPlusAuditor :: Schema
cokeyPlusAuditor =
  mustParseSchema $
    cokeyText
      <> T.unlines
        [ ""
        , "entity AuditorUser refines User {"
        , "  visible to all by default"
        , "  scope: Text"
        , "}"
        ]
{-# NOINLINE cokeyPlusAuditor #-}


-- | A query selecting the floored collection's edge (blog @Post.tags@).
feedTagsPlanQ :: Text
feedTagsPlanQ = "query FeedTags($org: OrgId) { feed(orgId: $org, first: 10) { title tags { name } } }"


-- | A query touching only @User@ (no Post declaration is pertinent).
meQ :: Text
meQ = "query Me { me { name } }"


-- | The cokey schema with the partial identity edge flipped to required.
cokeyProfileRequired :: Schema
cokeyProfileRequired =
  mustParseSchema
    (T.replace "has one? profile" "has one profile" cokeyText)
{-# NOINLINE cokeyProfileRequired #-}


-- | The blog schema with a floor added to the bounded @Post.tags@.
blogTagsFloored :: Schema
blogTagsFloored =
  mustParseSchema
    (T.replace "tags: Tag by postId max 50" "tags: Tag by postId min 1 max 50" blogText)
{-# NOINLINE blogTagsFloored #-}


-- | The plan must be rejected with a diagnostic naming the violated bound.
rejectsNaming :: Either CompileError Plan -> Text -> IO ()
rejectsNaming result needle = case result of
  Right _ -> expectationFailure ("plan unexpectedly compiled; wanted " <> T.unpack needle)
  Left ce -> ceDiagnostics ce `shouldSatisfy` any (T.isInfixOf needle)
