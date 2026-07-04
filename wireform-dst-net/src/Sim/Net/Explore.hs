{-# LANGUAGE ScopedTypeVariables #-}

-- | A protocol-agnostic /exploration harness/ over 'Sim.Net.Link.SimLink': a
-- seeded nemesis applies a timed schedule of faults concurrently with a real
-- networking workload, and an oracle classifies the outcome. This is the
-- randomized-campaign complement to the hand-designed @http2-fault-test@ cases
-- — instead of asserting a scenario we picked, it /generates/ fault schedules
-- from seeds and hunts for invariant violations (silent-corruption, engine
-- crashes, hangs under recoverable faults).
--
-- __Determinism boundary.__ The fault /choices/ are seeded and reproducible;
-- /when/ each fault lands relative to the real engine's progress depends on RTS
-- thread scheduling (the honest boundary of the transport-seam approach — see
-- "Sim.Net.Link"). So a failing seed reproduces the same fault schedule but not
-- necessarily byte-identical interleaving. That is exactly what a fuzzer needs;
-- byte-for-byte replay lives in @wireform-dst@'s pure 'Sim.Interp' engine.
--
-- The workload is any @'SimLink' -> IO a@ (an HTTP/2 request, a gRPC call, …);
-- drivers live with their protocol package so this library stays dependency-lean.
module Sim.Net.Explore
  ( -- * Faults
    Fault (..)
  , applyFault

    -- * Running a trial
  , Outcome (..)
  , outcomeOK
  , trial
  , withNemesis

    -- * Campaigns
  , CampaignReport (..)
  , emptyReport
  , runCampaign
  , reportClean

    -- * Seeded schedule generators
  , latencySchedule
  , cutSchedule
  , partitionHealSchedule
  , corruptSchedule
  , dropSchedule
  ) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception
  ( ArithException
  , ArrayException
  , AssertionFailed
  , ErrorCall
  , PatternMatchFail
  , SomeException
  , bracket
  , fromException
  , try
  )
import Control.Monad (foldM, forM_)
import Data.Maybe (isJust)
import System.Timeout (timeout)

import Sim.Entropy (Gen, newGen, nextDouble, uniformR)
import Sim.Net.Link
  ( Direction (..)
  , LatencyDist (..)
  , LinkControl
  , SimLink (..)
  , closeSimLink
  , corruptNext
  , cut
  , heal
  , newSimLink
  , partition
  , setDrop
  , setLatency
  )
import Sim.Types (Seed (..))

-- | One fault-injection action against a live link.
data Fault
  = FPartition !Direction
  | FHeal !Direction
  | FCut
  | FSetDrop !Direction !Double
  | FSetLatency !Direction !LatencyDist
  | FCorrupt !Direction
  deriving stock (Show)

-- | Apply a fault to a link's control handle.
applyFault :: LinkControl -> Fault -> IO ()
applyFault ctrl = \case
  FPartition d -> partition ctrl d
  FHeal d -> heal ctrl d
  FCut -> cut ctrl
  FSetDrop d p -> setDrop ctrl d p
  FSetLatency d l -> setLatency ctrl d l
  FCorrupt d -> corruptNext ctrl d

-- | The classified result of one trial. @a@ is the workload's success value
-- (e.g. @(Int, ByteString)@ for an HTTP status + body).
data Outcome a
  = -- | Workload completed and returned a value.
    OK !a
  | -- | Workload threw a /clean/ exception the engine raised deliberately
    -- (connection reset, stream error). Acceptable under cut/partition.
    Errored !String
  | -- | Workload threw a bug-smelling exception ('ErrorCall',
    -- 'PatternMatchFail', 'ArithException', …) — an engine defect.
    InternalError !String
  | -- | Workload did not finish within the timeout — a hang.
    Timeout
  deriving stock (Show)

outcomeOK :: Outcome a -> Bool
outcomeOK (OK _) = True
outcomeOK _ = False

-- Exceptions that indicate an engine bug rather than a deliberate failure.
internalException :: SomeException -> Bool
internalException e =
  any
    ($ e)
    [ isJust . asErrorCall
    , isJust . asPatternMatchFail
    , isJust . asArith
    , isJust . asArray
    , isJust . asAssertion
    ]
  where
    asErrorCall = fromException :: SomeException -> Maybe ErrorCall
    asPatternMatchFail = fromException :: SomeException -> Maybe PatternMatchFail
    asArith = fromException :: SomeException -> Maybe ArithException
    asArray = fromException :: SomeException -> Maybe ArrayException
    asAssertion = fromException :: SomeException -> Maybe AssertionFailed

-- | Run @act@ with a background nemesis thread applying a timed fault schedule.
-- Each @(delayUs, fault)@ waits @delayUs@ microseconds then fires. The nemesis
-- is always torn down (even if @act@ is interrupted by a timeout).
withNemesis :: [(Int, Fault)] -> SimLink -> IO a -> IO a
withNemesis sched l act =
  bracket (forkIO runSchedule) killThread (const act)
  where
    runSchedule = forM_ sched $ \(d, f) -> do
      if d > 0 then threadDelay d else pure ()
      applyFault (slControl l) f

trial
  :: forall a
   . Int
  -- ^ per-trial timeout (microseconds)
  -> Seed
  -> [(Int, Fault)]
  -> (SimLink -> IO a)
  -> IO (Outcome a)
trial tmoUs seed sched workload = do
  l <- newSimLink seed
  res <-
    try (timeout tmoUs (withNemesis sched l (workload l)))
      :: IO (Either SomeException (Maybe a))
  closeSimLink l
  pure $ case res of
    Left e
      | internalException e -> InternalError (show e)
      | otherwise -> Errored (show e)
    Right Nothing -> Timeout
    Right (Just a) -> OK a

-- | Aggregated results of a campaign. @crWrong@ holds trials that /completed/
-- but whose value failed the safety predicate — a silent-correctness bug, the
-- scariest outcome. @crInternal@ holds engine-defect exceptions. @crTimeout@
-- holds hangs.
data CampaignReport a = CampaignReport
  { crOK :: !Int
  , crErrored :: !Int
  , crTimeouts :: ![Seed]
  , crInternal :: ![(Seed, String)]
  , crWrong :: ![(Seed, a)]
  }
  deriving stock (Show)

emptyReport :: CampaignReport a
emptyReport = CampaignReport 0 0 [] [] []

-- | Fold a batch of seeds into a report. @schedFor@ derives the (seeded) fault
-- schedule for each seed; @okPred@ is the safety predicate applied to every
-- successful result (e.g. @== (200, "pong")@).
runCampaign
  :: Int
  -- ^ per-trial timeout (microseconds)
  -> [Seed]
  -> (Seed -> [(Int, Fault)])
  -> (a -> Bool)
  -> (SimLink -> IO a)
  -> IO (CampaignReport a)
runCampaign tmoUs seeds schedFor okPred workload =
  foldM step emptyReport seeds
  where
    step rep seed = do
      out <- trial tmoUs seed (schedFor seed) workload
      pure $ case out of
        OK a
          | okPred a -> rep {crOK = crOK rep + 1}
          | otherwise -> rep {crWrong = (seed, a) : crWrong rep}
        Errored _ -> rep {crErrored = crErrored rep + 1}
        InternalError msg -> rep {crInternal = (seed, msg) : crInternal rep}
        Timeout -> rep {crTimeouts = seed : crTimeouts rep}

-- | A report is clean iff no silent-correctness bug and no engine-defect
-- exception occurred. Liveness (@crTimeouts@) is checked separately because a
-- hang is only a bug under /recoverable/ faults.
reportClean :: CampaignReport a -> Bool
reportClean rep = null (crWrong rep) && null (crInternal rep)

-- Seed a schedule generator's own PRNG stream, distinct from the link's.
schedGen :: Seed -> Gen
schedGen (Seed w) = newGen (w * 6364136223846793005 + 1442695040888963407)

-- | Random latency on both directions, applied immediately. Should never
-- prevent completion — a pure liveness/correctness stressor.
latencySchedule :: Seed -> [(Int, Fault)]
latencySchedule seed =
  let g0 = schedGen seed
      (a, g1) = uniformR (0, 4) g0
      (b, _g2) = uniformR (0, 4) g1
      lo = min a b
      hi = max a b
   in [ (0, FSetLatency ClientToServer (UniformMs lo hi))
      , (0, FSetLatency ServerToClient (UniformMs lo hi))
      ]

-- | Cut the connection at a random moment (0–@maxUs@) after the workload
-- starts. Models a TCP reset / peer death: the engine must surface a clean
-- error or complete — never hang, never crash, never wrong data.
cutSchedule :: Int -> Seed -> [(Int, Fault)]
cutSchedule maxUs seed =
  let (d, _) = uniformR (0, maxUs) (schedGen seed)
   in [(d, FCut)]

-- | Partition one direction immediately, then heal it after a random delay.
-- A transient partition must not lose liveness: the request completes once the
-- link heals.
partitionHealSchedule :: Seed -> [(Int, Fault)]
partitionHealSchedule seed =
  let g0 = schedGen seed
      (dirBit, g1) = uniformR (0, 1) g0
      (healUs, _) = uniformR (2000, 40000) g1
      dir = if dirBit == 0 then ClientToServer else ServerToClient
   in [(0, FPartition dir), (healUs, FHeal dir)]

-- | Corrupt the next chunk in a random direction. NOT a fair liveness fault (a
-- mangled length field can legitimately stall the engine), so campaigns using
-- it check only safety (no wrong data, no crash), not liveness.
corruptSchedule :: Seed -> [(Int, Fault)]
corruptSchedule seed =
  let g0 = schedGen seed
      (dirBit, g1) = uniformR (0, 1) g0
      (d, _) = uniformR (0, 2000) g1
      dir = if dirBit == 0 then ClientToServer else ServerToClient
   in [(d, FCorrupt dir)]

-- | Drop a fraction of chunks in a random direction, then reset the connection
-- shortly after. Silent mid-stream loss hangs any reliable protocol (framing
-- desyncs and the engine waits for bytes that never come), so a bare drop is
-- neither a fair liveness test nor terminating; the trailing 'FCut' models a
-- flaky link that gives up, letting the trial finish fast. Safety/crash-only:
-- the engine must survive truncated + desynced framing plus a reset without an
-- internal exception — never a hang-forever, never a crash.
dropSchedule :: Seed -> [(Int, Fault)]
dropSchedule seed =
  let g0 = schedGen seed
      (dirBit, g1) = uniformR (0, 1) g0
      (p, g2) = nextDouble g1
      (resetUs, _) = uniformR (500, 8000) g2
      dir = if dirBit == 0 then ClientToServer else ServerToClient
   in [(0, FSetDrop dir p), (resetUs, FCut)]
