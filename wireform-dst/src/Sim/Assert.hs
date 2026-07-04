-- | Assertion "lightposts" (Antithesis vocabulary) and the folding that turns a
-- run's (or campaign's) assertion events into pass/fail verdicts.
--
-- The five combinators emit 'Sim.Types.EvAssert' events; the interpreter checks
-- the /per-run/ failure kinds inline (an @Always@ that was false when reached,
-- an @Unreachable@ that was reached). The /campaign/-level kinds — a @Sometimes@
-- or @Reachable@ that never came true across an entire campaign — can only be
-- decided after folding many runs, which is what 'foldAsserts' /
-- 'campaignAssertFailures' are for.
module Sim.Assert
  ( -- * Combinators
    always
  , alwaysOrUnreachable
  , sometimes
  , reachable
  , unreachable

    -- * Reports
  , AssertReport (..)
  , emptyAssertReport
  , foldAsserts
  , runFailFromAsserts
  , campaignAssertFailures
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Sim.Prog (Prog, assertOp)
import Sim.Types

-- | Assert a predicate that must hold every time this site is reached.
always :: SiteId -> Bool -> Prog ()
always = assertOp Always

-- | Like 'always', but a campaign where the site is /never/ reached is not a
-- failure (the per-run check is identical; the distinction is campaign-level).
alwaysOrUnreachable :: SiteId -> Bool -> Prog ()
alwaysOrUnreachable = assertOp AlwaysOrUnreachable

-- | Assert that this predicate is true at least once across the campaign.
sometimes :: SiteId -> Bool -> Prog ()
sometimes = assertOp Sometimes

-- | A beacon: mark that this site was reached (a campaign where it is never
-- reached is a failure).
reachable :: SiteId -> Prog ()
reachable s = assertOp Reachable s True

-- | Assert this site is never reached (reaching it at all is a failure).
unreachable :: SiteId -> Prog ()
unreachable s = assertOp Unreachable s True

-- | Accumulated assertion verdict over one or more runs.
data AssertReport = AssertReport
  { arViolations :: ![FailReason]
  -- ^ per-run violations discovered while folding
  , arSometimes :: !(Map SiteId Bool)
  -- ^ each reached @Sometimes@ site → has it ever been true?
  , arReachable :: !(Set SiteId)
  -- ^ @Reachable@ sites actually reached
  }
  deriving stock (Eq, Show)

-- | An empty report (starting point for a campaign fold).
emptyAssertReport :: AssertReport
emptyAssertReport = AssertReport [] Map.empty Set.empty

-- | Fold one run's events into the report: record per-run violations, OR each
-- @Sometimes@ site's truth across runs, and note reached @Reachable@ sites.
foldAsserts :: [SimEvent] -> AssertReport -> AssertReport
foldAsserts evs rep0 = foldl step rep0 evs
  where
    step rep ev = case ev of
      EvAssert s Always b
        | not b -> rep {arViolations = AssertViolated s Always : arViolations rep}
      EvAssert s AlwaysOrUnreachable b
        | not b -> rep {arViolations = AssertViolated s AlwaysOrUnreachable : arViolations rep}
      EvAssert s Unreachable True ->
        rep {arViolations = AssertViolated s Unreachable : arViolations rep}
      EvAssert s Sometimes b ->
        rep {arSometimes = Map.insertWith (||) s b (arSometimes rep)}
      EvAssert s Reachable _ ->
        rep {arReachable = Set.insert s (arReachable rep)}
      _ -> rep

-- | The first /per-run/ assertion failure in an event trace (an @Always@ /
-- @AlwaysOrUnreachable@ false-when-reached, or an @Unreachable@ reached).
runFailFromAsserts :: [SimEvent] -> Maybe FailReason
runFailFromAsserts = listToMaybe . mapMaybe eventFail
  where
    eventFail = \case
      EvAssert s Always False -> Just (AssertViolated s Always)
      EvAssert s AlwaysOrUnreachable False -> Just (AssertViolated s AlwaysOrUnreachable)
      EvAssert s Unreachable True -> Just (AssertViolated s Unreachable)
      _ -> Nothing

-- | Campaign-level failures: @Sometimes@ sites that were reached but never came
-- true across the whole campaign.
campaignAssertFailures :: AssertReport -> [FailReason]
campaignAssertFailures rep =
  [AssertViolated s Sometimes | (s, everTrue) <- Map.toList (arSometimes rep), not everTrue]
