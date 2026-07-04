-- | @wireform-dst@ — a deterministic, fault-injecting simulation-testing engine
-- (Antithesis-style) for Haskell distributed-system workloads.
--
-- This entry module re-exports the __app-facing__ surface: write a workload as a
-- 'Prog' per node, decorate it with assertion lightposts ('always' / 'sometimes'
-- / …) and coverage hints ('cover' / 'observe' / 'buggify'), bundle it into a
-- 'Scenario' / 'Workload', and run a FIND → NAIL campaign with 'runCampaign'.
-- Every failure is reproducible from its 'RunInput' via 'replay'.
--
-- Interpreter internals (@step@, @World@, @Resume@, the engine's scheduling
-- guts) are intentionally __not__ re-exported here — import "Sim.World" /
-- "Sim.Interp" directly if you need to inspect world state (e.g. to write a
-- 'scenInvariant').
module Sim
  ( -- * Writing workloads
    Prog
  , SimVar
  , fork
  , yield
  , delay
  , getTime
  , whoAmI
  , drawWord
  , send
  , recv
  , newVar
  , readVar
  , writeVar
  , modifyVar
  , storeWrite
  , storeRead
  , fsync
  , cover
  , observe
  , buggify
  , logMsg

    -- * Assertion lightposts
  , always
  , alwaysOrUnreachable
  , sometimes
  , reachable
  , unreachable

    -- * Core types
  , NodeId (..)
  , FiberId (..)
  , SimTime (..)
  , Seed (..)
  , Key (..)
  , SiteId (..)
  , FaultOp (..)
  , FaultKind (..)
  , NemesisConfig (..)
  , AssertKind (..)
  , FailReason (..)

    -- * Topology
  , LinkPolicy (..)
  , LatencyDist (..)
  , defaultLink

    -- * Scenarios & reproduction
  , Scenario (..)
  , RunInput (..)
  , RunResult (..)
  , replay

    -- * Search & campaigns
  , SearchConfig (..)
  , defaultSearchConfig
  , Bug (..)
  , Workload (..)
  , CampaignResult (..)
  , runCampaign
  , saveBug
  , replayFile
  , BugReport (..)
  ) where

import Sim.Assert (always, alwaysOrUnreachable, reachable, sometimes, unreachable)
import Sim.Interp (RunInput (..), RunResult (..), Scenario (..), replay)
import Sim.Localize (BugReport (..))
import Sim.Prog
  ( Prog
  , SimVar
  , buggify
  , cover
  , delay
  , drawWord
  , fork
  , fsync
  , getTime
  , logMsg
  , modifyVar
  , newVar
  , observe
  , readVar
  , recv
  , send
  , storeRead
  , storeWrite
  , whoAmI
  , writeVar
  , yield
  )
import Sim.Search (Bug (..), SearchConfig (..), defaultSearchConfig)
import Sim.Types
  ( AssertKind (..)
  , FailReason (..)
  , FaultKind (..)
  , FaultOp (..)
  , FiberId (..)
  , Key (..)
  , NemesisConfig (..)
  , NodeId (..)
  , Seed (..)
  , SimTime (..)
  , SiteId (..)
  )
import Sim.Workload (CampaignResult (..), Workload (..), runCampaign, replayFile, saveBug)
import Sim.World (LatencyDist (..), LinkPolicy (..), defaultLink)
