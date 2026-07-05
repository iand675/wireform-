{- | Plan contracts (spec §7.3, §8.1, §14.1): the path-join slice partition,
plan identity moving with pertinent declarations only, and the static
plan budgets naming their bound.
-}
module Test.Lattice.Plan (tests) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Lattice.Plan (Plan (..), planQuery, planSliceRecord)
import Lattice.Query.Validate (CompileError (..))
import Lattice.Schema (Budgets (..), Schema, defaultBudgets)
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


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

heroQ :: Text
heroQ = "query Hero { hero { name friends(first: 10) { name } } }"


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


-- | The plan must be rejected with a diagnostic naming the violated bound.
rejectsNaming :: Either CompileError Plan -> Text -> IO ()
rejectsNaming result needle = case result of
  Right _ -> expectationFailure ("plan unexpectedly compiled; wanted " <> T.unpack needle)
  Left ce -> ceDiagnostics ce `shouldSatisfy` any (T.isInfixOf needle)
