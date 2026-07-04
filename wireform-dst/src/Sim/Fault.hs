-- | Fault injection: how a 'Sim.Types.FireFault' decision mutates the 'World'
-- ('applyFault'), and the deterministic menu of faults the nemesis may propose
-- at a given world ('availableFaults'). @availableFaults@ is a pure function of
-- @(NemesisConfig, World)@, which is exactly what makes the @FireFault i@ index
-- stable under replay — there is no stored fault registry to drift.
--
-- This module is part of the interpreter (Phase 1), not a standalone worker:
-- @applyFault@ /is/ the meaning of a 'Sim.Types.FireFault' step.
module Sim.Fault
  ( applyFault
  , availableFaults
  , swizzleClogPlan
  , clogWindowNs
  , pauseWindowNs
  , skewOffsetNs
  ) where

import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Sim.Entropy (Gen, uniformR)
import Sim.Types
import Sim.World

-- | Default window (ns) a 'Clog' / 'PauseNode' / 'DiskLatency' lasts: 100 ms.
clogWindowNs :: Word
clogWindowNs = 100000000

-- | Alias used by pause/disk faults.
pauseWindowNs :: Word
pauseWindowNs = clogWindowNs

-- | Default clock-skew magnitude (ns): 50 ms.
skewOffsetNs :: Int
skewOffsetNs = 50000000

addNs :: SimTime -> Word -> SimTime
addNs (SimTime a) d = SimTime (a + fromIntegral d)

-- | Apply one fault to the world, returning the mutated world and the emitted
-- 'EvFault' event. Budget accounting lives in the caller ("Sim.Interp"'s
-- @FireFault@), which bumps @wFaultsFired@.
applyFault :: FaultOp -> World -> (World, SimEvent)
applyFault op w = (go op, EvFault op)
  where
    go :: FaultOp -> World
    go = \case
      DropMessage (MsgId m) ->
        w {wInFlight = IntMap.delete m (wInFlight w)}
      Partition a b ->
        w {wLinks = setLink a b (\lp -> lp {lpPartitioned = True})}
      Heal a b ->
        w {wLinks = setLink a b (\lp -> lp {lpPartitioned = False})}
      Clog a b t ->
        w {wLinks = setLink a b (\lp -> lp {lpClogUntil = Just t})}
      CrashNode n -> crashNode n
      RebootNode n -> rebootNode n
      PauseNode n t -> modNode n (\ns -> ns {nsPausedUntil = Just t})
      ClockSkew n off -> modNode n (\ns -> ns {nsSkewNs = off})
      CorruptWrite n -> modNode n (\ns -> ns {nsCorruptArmed = True})
      TearWrite n -> modNode n (\ns -> ns {nsTearArmed = True})
      DiskLatency n t -> modNode n (\ns -> ns {nsPausedUntil = Just t})

    -- upsert a directed link, seeding a healthy one if absent
    setLink :: NodeId -> NodeId -> (LinkPolicy -> LinkPolicy) -> Map.Map (NodeId, NodeId) LinkPolicy
    setLink a b f =
      Map.insert (a, b) (f (Map.findWithDefault (defaultLink (Fixed (SimTime 0))) (a, b) (wLinks w))) (wLinks w)

    modNode :: NodeId -> (NodeState -> NodeState) -> World
    modNode (NodeId n) f = w {wNodes = IntMap.adjust f n (wNodes w)}

    -- crash: drop volatile + heap + un-fsynced writes, kill the node's fibers
    crashNode :: NodeId -> World
    crashNode nid@(NodeId n) =
      let clearVol ns =
            ns
              { nsAlive = False
              , nsVolatile = Map.empty
              , nsHeap = IntMap.empty
              , nsHeapCtr = 0
              , nsTearArmed = False
              , nsCorruptArmed = False
              }
          killFiber fb
            | fbNode fb == nid = fb {fbStatus = FDead, fbResume = Ready (pure ())}
            | otherwise = fb
       in w
            { wNodes = IntMap.adjust clearVol n (wNodes w)
            , wFibers = IntMap.map killFiber (wFibers w)
            }

    -- reboot: mark alive + respawn the node's root program as a fresh fiber
    rebootNode :: NodeId -> World
    rebootNode nid@(NodeId n) =
      let ns0 = IntMap.findWithDefault freshNode n (wNodes w)
          ns1 = ns0 {nsAlive = True, nsPausedUntil = Nothing}
          fid = wNextFiber w
       in case IntMap.lookup n (wNodePrograms w) of
            Nothing -> w {wNodes = IntMap.insert n ns1 (wNodes w)}
            Just prog ->
              w
                { wNodes = IntMap.insert n ns1 (wNodes w)
                , wFibers =
                    IntMap.insert
                      fid
                      Fiber {fbNode = nid, fbResume = Ready prog, fbStatus = FRunnable}
                      (wFibers w)
                , wNextFiber = fid + 1
                }

-- | The deterministic, ordered menu of faults available at this world. Empty
-- once the virtual clock has reached @ncQuiesceAt@ or the fault budget is
-- spent. Otherwise, for each enabled 'FaultKind' (in @ncKinds@ order), enumerate
-- concrete ops over the current live topology / in-flight messages, with nodes
-- and message ids in ascending order so the @FireFault i@ index is stable.
availableFaults :: NemesisConfig -> World -> [FaultOp]
availableFaults nc w
  | wClock w >= ncQuiesceAt nc = []
  | wFaultsFired w >= ncBudget nc = []
  | otherwise = concatMap forKind (ncKinds nc)
  where
    clk = wClock w
    aliveNodes = [NodeId k | (k, ns) <- IntMap.toAscList (wNodes w), nsAlive ns]
    deadNodes = [NodeId k | (k, ns) <- IntMap.toAscList (wNodes w), not (nsAlive ns)]
    msgIds = [MsgId k | k <- IntMap.keys (wInFlight w)]
    orderedPairs = [(a, b) | a <- aliveNodes, b <- aliveNodes, a /= b]

    isPartitioned a b = maybe False lpPartitioned (Map.lookup (a, b) (wLinks w))
    isClogged a b = case Map.lookup (a, b) (wLinks w) of
      Just lp -> maybe False (> clk) (lpClogUntil lp)
      Nothing -> False

    forKind :: FaultKind -> [FaultOp]
    forKind = \case
      KDrop -> [DropMessage m | m <- msgIds]
      KPartition -> [Partition a b | (a, b) <- orderedPairs, not (isPartitioned a b)]
      KClog -> [Clog a b (addNs clk clogWindowNs) | (a, b) <- orderedPairs, not (isClogged a b)]
      KCrash -> [CrashNode n | n <- aliveNodes]
      KReboot -> [RebootNode n | n <- deadNodes]
      KPause -> [PauseNode n (addNs clk pauseWindowNs) | n <- aliveNodes, notPaused n]
      KSkew -> [ClockSkew n skewOffsetNs | n <- aliveNodes, noSkew n]
      KCorrupt -> [CorruptWrite n | n <- aliveNodes, notCorruptArmed n]
      KTear -> [TearWrite n | n <- aliveNodes, notTearArmed n]
      KDiskLatency -> [DiskLatency n (addNs clk pauseWindowNs) | n <- aliveNodes, notPaused n]

    nodeState (NodeId n) = IntMap.lookup n (wNodes w)
    notPaused n = maybe True (maybe True (<= clk) . nsPausedUntil) (nodeState n)
    noSkew n = maybe True ((== 0) . nsSkewNs) (nodeState n)
    notCorruptArmed n = maybe True (not . nsCorruptArmed) (nodeState n)
    notTearArmed n = maybe True (not . nsTearArmed) (nodeState n)

-- | A scripted clog-then-heal plan over a node subset: for each ordered
-- distinct pair, a 'Clog' (with a random window) followed later by a 'Heal'.
-- A convenience for scenario authors who want a canned "swizzle" of a subset;
-- not used on any hot path.
swizzleClogPlan :: [NodeId] -> Gen -> [FaultOp]
swizzleClogPlan nodes gen0 =
  let pairs = [(a, b) | a <- nodes, b <- nodes, a /= b]
      (clogs, _) = go gen0 pairs
   in clogs ++ [Heal a b | (a, b) <- pairs]
  where
    go g [] = ([], g)
    go g ((a, b) : rest) =
      let (ms, g') = uniformR (10, 200) g
          (more, g'') = go g' rest
       in (Clog a b (SimTime (fromIntegral ms * 1000000)) : more, g'')
