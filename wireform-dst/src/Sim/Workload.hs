-- | The campaign orchestrator: bundle a named 'Scenario' as a 'Workload', run a
-- FIND ('Sim.Search.search') → NAIL ('Sim.Localize.explainBug') campaign
-- ('runCampaign'), and persist / regression-replay bugs ('saveBug' /
-- 'replayFile'). This is the app-facing IO surface; the sim core itself stays
-- pure.
module Sim.Workload
  ( Workload (..)
  , CampaignResult (..)
  , runCampaign
  , saveBug
  , replayFile
  ) where

import Data.Aeson (eitherDecodeFileStrict, encodeFile)
import Data.Text (Text)
import Sim.Assert (AssertReport)
import Sim.Coverage (CovSet)
import Sim.Interp (RunResult, Scenario, replay)
import Sim.Localize (BugReport, explainBug)
import Sim.Search (Bug (..), SearchConfig, SearchOutcome (..), search)
import Sim.Types (Seed)

-- | A named simulation workload other packages write against.
data Workload = Workload
  { wlName :: !Text
  , wlScenario :: !Scenario
  }

-- | The outcome of a campaign: a localized report per bug, the campaign-level
-- assertion verdict, the number of expansions, and the merged coverage.
data CampaignResult = CampaignResult
  { crReports :: ![BugReport]
  , crReport :: !AssertReport
  , crRuns :: !Int
  , crCov :: !CovSet
  }

-- | Run a FIND → NAIL campaign: search for bugs, then minimize + localize each.
runCampaign :: SearchConfig -> Seed -> Workload -> IO CampaignResult
runCampaign cfg seed wl = do
  outcome <- search cfg seed (wlScenario wl)
  let reports = map (explainBug cfg (wlScenario wl) outcome) (soBugs outcome)
  pure
    CampaignResult
      { crReports = reports
      , crReport = soReport outcome
      , crRuns = soRuns outcome
      , crCov = soCov outcome
      }

-- | Persist a bug (its 'Sim.Interp.RunInput' + 'Sim.Types.FailReason') as JSON,
-- so a regression suite can replay it later with 'replayFile'.
saveBug :: FilePath -> Bug -> IO ()
saveBug = encodeFile

-- | Decode a saved bug and replay it against a workload's scenario, for
-- regression testing.
replayFile :: FilePath -> Workload -> IO RunResult
replayFile path wl = do
  e <- eitherDecodeFileStrict path
  case e of
    Left err -> ioError (userError ("replayFile: bad bug file " ++ path ++ ": " ++ err))
    Right bug -> pure (replay (wlScenario wl) (bugInput bug))
