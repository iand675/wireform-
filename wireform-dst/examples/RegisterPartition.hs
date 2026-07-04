{-# LANGUAGE OverloadedStrings #-}

-- | End-to-end demonstration: a replicated register with a seeded
-- ack-before-durable bug, found by FIND (guided search) and minimized +
-- localized by NAIL.
--
-- Topology: @primary@ (node 0), @backup@ (node 1), @client@ (node 99). The
-- client issues @WRITE 1@ then @READ@; it records the last acknowledged write
-- value under durable key @"ack"@ and the read-back value under @"read"@ on its
-- own node, so the invariant — /no acknowledged write is ever lost/, i.e.
-- @read >= ack@ — is a pure function of durable 'Sim.World' state.
--
-- The bug: on @WRITE v@ the primary writes the register and __acks the client
-- immediately__, before replicating to the backup and before its write is
-- durable (no @fsync@). It manifests only when the nemesis composes
-- @TearWrite primary@ (the write is still volatile) → ack → @CrashNode primary@
-- (volatile lost) → @RebootNode primary@ (durable has nothing) → the client's
-- @READ@ returns a value older than its acknowledged write.
module Main (main) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Sim
import Sim.World (nsDurable, wNodes)
import System.Exit (exitFailure)

-- Node ids.
primary, backup, client :: NodeId
primary = NodeId 0
backup = NodeId 1
client = NodeId 99

regKey :: Key
regKey = Key "reg"

-- Message tags.
tagW, tagR, tagA, tagG, tagV :: ByteString
tagW = "W"
tagR = "R"
tagA = "A"
tagG = "G"
tagV = "V"

payloadOf :: ByteString -> ByteString
payloadOf = BS.drop 1

numOf :: ByteString -> Int
numOf b = if BS.null b then 0 else fromIntegral (BS.head b)

-- The primary: a recv loop. On WRITE it stores the register and acks the client
-- immediately (before durability / replication) — the seeded bug. RebootNode
-- respawns this program, so a rebooted primary reads only its durable store.
primaryLoop :: Prog ()
primaryLoop = do
  (from, msg) <- recv
  let tag = BS.take 1 msg
      val = payloadOf msg
  if tag == tagW
    then do
      storeWrite regKey val -- NO fsync: durable-by-default here, but a TearWrite makes it volatile
      cover "primary-write"
      send backup (tagR <> val) -- replicate (may be lost under partition/crash)
      send from (tagA <> val) -- ACK BEFORE DURABLE + BEFORE REPLICA CONFIRM
    else
      if tag == tagG
        then do
          mv <- storeRead regKey
          cover "primary-read"
          send from (tagV <> maybe BS.empty id mv)
        else pure ()
  primaryLoop

-- The backup: durably applies each replicated write.
backupLoop :: Prog ()
backupLoop = do
  (_, msg) <- recv
  storeWrite regKey (payloadOf msg)
  fsync
  cover "backup-replicate"
  backupLoop

-- The client: WRITE 1, record the ack, then READ and record what came back.
clientProg :: Prog ()
clientProg = do
  send primary (tagW <> BS.singleton 1)
  (_, ack) <- recv
  storeWrite (Key "ack") (payloadOf ack)
  observe [fromIntegral (numOf (payloadOf ack))]
  reachable "client-acked"
  send primary tagG
  (_, val) <- recv
  let v = payloadOf val
  storeWrite (Key "read") (if BS.null v then BS.singleton 0 else v)
  cover "client-read"

registerScenario :: Scenario
registerScenario =
  Scenario
    { scenNemesis = NemesisConfig 5 [KTear, KCrash, KReboot] (SimTime maxBound)
    , scenLinks = Map.empty
    , scenNodes =
        [ (primary, primaryLoop)
        , (backup, backupLoop)
        , (client, clientProg)
        ]
    , scenInvariant = inv
    }
  where
    inv w =
      let dur (NodeId n) k = IntMap.lookup n (wNodes w) >>= Map.lookup (Key k) . nsDurable
       in case (dur client "ack", dur client "read") of
            (Just a, Just r) -> numOf r >= numOf a
            _ -> True

registerWorkload :: Workload
registerWorkload = Workload "replicated-register" registerScenario

isInvariant :: BugReport -> Bool
isInvariant r = case rrFail (replay registerScenario (brMinimal r)) of
  Just (InvariantBroken _) -> True
  _ -> False

main :: IO ()
main = do
  result <- runCampaign defaultSearchConfig {scMaxRuns = 3000} (Seed 1) registerWorkload
  putStrLn ("expansions: " ++ show (crRuns result))
  -- Prefer the durability (invariant) bug; fall back to the first report.
  let picked = case filter isInvariant (crReports result) of
        (r : _) -> Just r
        [] -> case crReports result of (r : _) -> Just r; [] -> Nothing
  case picked of
    Nothing -> do
      putStrLn "NO BUG FOUND"
      exitFailure
    Just report -> do
      let minimal = brMinimal report
          n = length (riPath minimal)
          rr = replay registerScenario minimal
      putStrLn "=== BugReport ==="
      putStrLn ("failure             : " ++ show (rrFail rr))
      putStrLn ("minimal path length : " ++ show n)
      putStrLn ("minimal decisions   : " ++ show (riPath minimal))
      putStrLn ("first invariant break at decision index: " ++ show (brFirstBreak report))
      putStrLn ("proximate decision  : " ++ show (brProximate report))
      putStrLn ("top suspicious sites: " ++ show (take 3 (brSuspicious report)))
      if n <= 8 && rrFail rr /= Nothing
        then putStrLn "OK: bug found, minimized, and reproduced."
        else do
          putStrLn ("FAIL: expected minimal path <= 8 that reproduces (got " ++ show n ++ ")")
          exitFailure
