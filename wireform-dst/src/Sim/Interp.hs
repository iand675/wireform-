-- | The deterministic engine: 'step' (run one 'Decision' against the 'World',
-- purely), 'runnable' (the legal next decisions in a fixed total order),
-- 'initWorld', 'advanceClock', and the reproduction primitive 'replay'.
--
-- The whole module is @IO@-free: a run is a pure fold of 'step' over a
-- 'DecisionPath', so any leaf state is reproduced byte-for-byte by replaying a
-- 'RunInput'. "Sim.Search" and "Sim.Localize" build on exactly this contract.
module Sim.Interp
  ( -- * Scenario / run I-O
    Scenario (..)
  , RunInput (..)
  , RunResult (..)
  , StepResult (..)

    -- * Engine
  , step
  , runnable
  , initWorld
  , advanceClock
  , quiescent
  , canAdvanceTime
  , anyBlockedRecv
  , deadlocked
  , replay

    -- * Introspection helpers
  , skewedClock
  , eventFail
  ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Bits (shiftL, xor)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Dynamic (fromDyn, toDyn)
import Data.IntMap.Strict qualified as IntMap
import Data.Typeable (Typeable)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Sim.Fault (applyFault, availableFaults)
import Sim.Entropy (mixKey, nextDouble, nextWord64, newGen, sampleLatency)
import Sim.Prog (Prog (..), SimOp (..), SimVar (..))
import Sim.Types
import Sim.World

-- | A scenario: the adversary config, the initial link topology, one root
-- program per node, and an invariant checked after every 'step' and at the
-- terminal state.
data Scenario = Scenario
  { scenNemesis :: !NemesisConfig
  , scenLinks :: !(Map (NodeId, NodeId) LinkPolicy)
  , scenNodes :: ![(NodeId, Prog ())]
  , scenInvariant :: !(World -> Bool)
  }

-- | Everything needed to reproduce a run exactly.
data RunInput = RunInput
  { riSeed :: !Seed
  , riBuggify :: !(Map SiteId Bool)
  , riPath :: !DecisionPath
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The outcome of a run: final world, the ordered event trace, and the first
-- failure (if any).
data RunResult = RunResult
  { rrFinal :: !World
  , rrEvents :: ![SimEvent]
  , rrFail :: !(Maybe FailReason)
  }

-- | The result of one 'step': the new world and the events it emitted.
data StepResult = StepResult
  { srWorld :: !World
  , srEvents :: ![SimEvent]
  }

-- Default latency for a link not present in the topology map. Zero, because
-- delivery /timing/ is controlled explicitly by 'DeliverMsg' decisions (a
-- message waits in flight until the scheduler chooses to deliver it), not by a
-- latency clock. Scenarios that want latency-driven reordering set an explicit
-- 'LinkPolicy'.
defaultLatencyNs :: LatencyDist
defaultLatencyNs = Fixed (SimTime 0)

-- Fuel cap so a pathological ping-pong workload can't hang replay/search.
maxDriveSteps :: Int
maxDriveSteps = 200000

-- * initWorld --------------------------------------------------------------

-- | Build the initial world from a seed and scenario: one 'FRunnable' root
-- fiber per @scenNodes@ entry (fiber ids @0..n-1@), node states for every node
-- mentioned in @scenNodes@ or a link endpoint, and the node-program table used
-- by 'Sim.Types.RebootNode'.
initWorld :: Seed -> Scenario -> World
initWorld (Seed s) scen =
  World
    { wClock = SimTime 0
    , wGen = newGen s
    , wSeqCtr = 0
    , wFibers = IntMap.fromList [(i, Fiber n (Ready p) FRunnable) | (i, (n, p)) <- zip [0 ..] (scenNodes scen)]
    , wNextFiber = length (scenNodes scen)
    , wInFlight = IntMap.empty
    , wNextMsg = 0
    , wLinks = scenLinks scen
    , wNodes = IntMap.fromList [(n, freshNode) | NodeId n <- allNodeIds]
    , wBuggify = Map.empty
    , wBuggifyOverride = Map.empty
    , wNemesis = scenNemesis scen
    , wNodePrograms = IntMap.fromList [(n, p) | (NodeId n, p) <- scenNodes scen]
    , wFaultsFired = 0
    }
  where
    linkEnds = concatMap (\(a, b) -> [a, b]) (Map.keys (scenLinks scen))
    allNodeIds = dedupNodes (map fst (scenNodes scen) ++ linkEnds)

dedupNodes :: [NodeId] -> [NodeId]
dedupNodes = go []
  where
    go seen [] = reverse seen
    go seen (x : xs)
      | x `elem` seen = go seen xs
      | otherwise = go (x : seen) xs

-- * Fiber execution -------------------------------------------------------

-- How a fiber's step ends.
data StepOut
  = SDone
  | SRecv ((NodeId, ByteString) -> Prog ())
  | STimer !SimTime (Prog ())
  | SYield (Prog ())

-- Run a 'Ready' fiber through its inline ops until it hits a yield point. The
-- events are returned in emission order.
runReady :: NodeId -> World -> Prog () -> (World, [SimEvent], StepOut)
runReady node = goP []
  where
    goP evAcc w prog = case prog of
      Pure () -> (w, reverse evAcc, SDone)
      Bind op k -> case op of
        Yield -> (w, reverse evAcc, SYield (k ()))
        Delay (SimTime t) ->
          let SimTime c = wClock w
           in (w, reverse evAcc, STimer (SimTime (c + t)) (k ()))
        Recv -> (w, reverse evAcc, SRecv k)
        GetTime -> goP evAcc w (k (skewedClock node w))
        WhoAmI -> goP evAcc w (k node)
        DrawWord ->
          let (x, g') = nextWord64 (wGen w)
           in goP evAcc w {wGen = g'} (k x)
        Send dst body ->
          let (w', ev) = doSend node dst body w
           in goP (ev : evAcc) w' (k ())
        Fork p ->
          let (fid, w') = spawn node p w
           in goP evAcc w' (k fid)
        NewVar a ->
          let (v, w') = doNewVar node a w
           in goP evAcc w' (k v)
        ReadVar v -> goP evAcc w (k (doReadVar node v w))
        WriteVar v a -> goP evAcc (doWriteVar node v a w) (k ())
        ModifyVar v f ->
          let (b, w') = doModifyVar node v f w
           in goP evAcc w' (k b)
        StoreWrite key body -> goP evAcc (doStoreWrite node key body w) (k ())
        StoreRead key -> goP evAcc w (k (doStoreRead node key w))
        Fsync -> goP evAcc (doFsync node w) (k ())
        Cover s -> goP (EvCover s : evAcc) w (k ())
        Observe ws -> goP (EvObserve (mixKey ws) : evAcc) w (k ())
        Buggify s p ->
          let (b, w') = doBuggify s p w
           in goP evAcc w' (k b)
        AssertOp ak s b -> goP (EvAssert s ak b : evAcc) w (k ())
        LogMsg t -> goP (EvLog node t : evAcc) w (k ())

-- | The virtual clock as perceived by a node (adjusted by its clock skew),
-- clamped to be non-negative.
skewedClock :: NodeId -> World -> SimTime
skewedClock (NodeId n) w =
  let SimTime c = wClock w
      skew = maybe 0 nsSkewNs (IntMap.lookup n (wNodes w))
   in SimTime (fromIntegral (max 0 (toInteger c + toInteger skew)))

spawn :: NodeId -> Prog () -> World -> (FiberId, World)
spawn node p w =
  let fid = wNextFiber w
   in ( FiberId fid
      , w
          { wFibers = IntMap.insert fid (Fiber node (Ready p) FRunnable) (wFibers w)
          , wNextFiber = fid + 1
          }
      )

doSend :: NodeId -> NodeId -> ByteString -> World -> (World, SimEvent)
doSend from dst body w =
  let lp = Map.findWithDefault (defaultLink defaultLatencyNs) (from, dst) (wLinks w)
      (dropRoll, g1) = nextDouble (wGen w)
      (lat, g2) = sampleLatency (lpLatency lp) g1
      mid = wNextMsg w
      SimTime c = wClock w
      SimTime l = lat
      due = SimTime (c + l)
      base = w {wGen = g2, wNextMsg = mid + 1}
      w' =
        if dropRoll < lpDrop lp
          then base -- lost in transit; still consumed an id
          else base {wInFlight = IntMap.insert mid (InFlight from dst body due) (wInFlight base)}
   in (w', EvSend from dst (MsgId mid))

nodeOf :: NodeId -> World -> NodeState
nodeOf (NodeId n) w = IntMap.findWithDefault freshNode n (wNodes w)

setNode :: NodeId -> NodeState -> World -> World
setNode (NodeId n) ns w = w {wNodes = IntMap.insert n ns (wNodes w)}

doNewVar :: Typeable a => NodeId -> a -> World -> (SimVar a, World)
doNewVar node a w =
  let ns = nodeOf node w
      i = nsHeapCtr ns
      ns' = ns {nsHeap = IntMap.insert i (toDyn a) (nsHeap ns), nsHeapCtr = i + 1}
   in (SimVar i, setNode node ns' w)

doReadVar :: Typeable a => NodeId -> SimVar a -> World -> a
doReadVar node (SimVar i) w =
  case IntMap.lookup i (nsHeap (nodeOf node w)) of
    Just dyn -> fromDyn dyn (error "Sim.Interp.doReadVar: SimVar type mismatch")
    Nothing -> error "Sim.Interp.doReadVar: dangling SimVar (node crashed?)"

doWriteVar :: Typeable a => NodeId -> SimVar a -> a -> World -> World
doWriteVar node (SimVar i) a w =
  let ns = nodeOf node w
   in setNode node ns {nsHeap = IntMap.insert i (toDyn a) (nsHeap ns)} w

doModifyVar :: Typeable a => NodeId -> SimVar a -> (a -> (a, b)) -> World -> (b, World)
doModifyVar node v@(SimVar i) f w =
  let a = doReadVar node v w
      (a', b) = f a
      ns = nodeOf node w
   in (b, setNode node ns {nsHeap = IntMap.insert i (toDyn a') (nsHeap ns)} w)

-- Storage: durable by default; a TearWrite arms the next write to volatile;
-- a CorruptWrite flips one bit of the next durable write.
doStoreWrite :: NodeId -> Key -> ByteString -> World -> World
doStoreWrite node key body w =
  let ns = nodeOf node w
   in if nsTearArmed ns
        then setNode node ns {nsVolatile = Map.insert key body (nsVolatile ns), nsTearArmed = False} w
        else
          if nsCorruptArmed ns
            then
              let (bitW, g') = nextWord64 (wGen w)
                  body' = flipBitBS body (fromIntegral bitW)
                  ns' = ns {nsDurable = Map.insert key body' (nsDurable ns), nsCorruptArmed = False}
               in setNode node ns' w {wGen = g'}
            else setNode node ns {nsDurable = Map.insert key body (nsDurable ns)} w

doStoreRead :: NodeId -> Key -> World -> Maybe ByteString
doStoreRead node key w =
  let ns = nodeOf node w
   in case Map.lookup key (nsVolatile ns) of
        Just b -> Just b
        Nothing -> Map.lookup key (nsDurable ns)

doFsync :: NodeId -> World -> World
doFsync node w =
  let ns = nodeOf node w
      ns' = ns {nsDurable = Map.union (nsVolatile ns) (nsDurable ns), nsVolatile = Map.empty}
   in setNode node ns' w

doBuggify :: SiteId -> Double -> World -> (Bool, World)
doBuggify s p w =
  case Map.lookup s (wBuggifyOverride w) of
    Just b -> (b, w)
    Nothing -> case Map.lookup s (wBuggify w) of
      Just b -> (b, w)
      Nothing ->
        let (d, g') = nextDouble (wGen w)
            b = d < p
         in (b, w {wGen = g', wBuggify = Map.insert s b (wBuggify w)})

flipBitBS :: ByteString -> Int -> ByteString
flipBitBS bs bit
  | BS.null bs = bs
  | otherwise =
      let n = BS.length bs
          bIdx = bit `mod` (n * 8)
          byteI = bIdx `div` 8
          bitI = bIdx `mod` 8
          old = BS.index bs byteI
          new = old `xor` (1 `shiftL` bitI)
       in BS.concat [BS.take byteI bs, BS.singleton new, BS.drop (byteI + 1) bs]

-- * step / runnable -------------------------------------------------------

-- | Run one decision against the world, purely.
step :: World -> Decision -> StepResult
step w (SchedNext (FiberId f)) =
  case IntMap.lookup f (wFibers w) of
    Just fb
      | fbStatus fb == FRunnable ->
          case fbResume fb of
            Ready prog ->
              let node = fbNode fb
                  (w', evs, out) = runReady node w prog
                  fb' = case out of
                    SDone -> fb {fbStatus = FDead, fbResume = Ready (pure ())}
                    SYield p -> fb {fbStatus = FRunnable, fbResume = Ready p}
                    SRecv cont -> fb {fbStatus = FBlockedRecv, fbResume = AwaitRecv cont}
                    STimer t p -> fb {fbStatus = FBlockedTimer t, fbResume = AwaitTimer t p}
                  w'' = w' {wFibers = IntMap.insert f fb' (wFibers w'), wSeqCtr = wSeqCtr w' + 1}
               in StepResult w'' (EvStep (FiberId f) : evs)
            _ -> StepResult w []
    _ -> StepResult w []
step w (DeliverMsg (MsgId m)) =
  case IntMap.lookup m (wInFlight w) of
    Nothing -> StepResult w []
    Just inf ->
      let to = ifTo inf
       in if not (nsAlive (nodeOf to w))
            then StepResult w {wInFlight = IntMap.delete m (wInFlight w)} [] -- dropped at dead dest
            else case pickRecvFiber to w of
              Nothing -> StepResult w [] -- no waiting recv: leave in flight
              Just (fid, cont) ->
                let woken = Fiber to (Ready (cont (ifFrom inf, ifBody inf))) FRunnable
                    w' =
                      w
                        { wInFlight = IntMap.delete m (wInFlight w)
                        , wFibers = IntMap.insert fid woken (wFibers w)
                        , wSeqCtr = wSeqCtr w + 1
                        }
                 in StepResult w' [EvRecv to (MsgId m)]
step w (FireFault i) =
  let fs = availableFaults (wNemesis w) w
   in if i < 0 || i >= length fs
        then StepResult w []
        else
          let (w', ev) = applyFault (fs !! i) w
           in StepResult w' {wFaultsFired = wFaultsFired w + 1, wSeqCtr = wSeqCtr w + 1} [ev]

pickRecvFiber :: NodeId -> World -> Maybe (Int, (NodeId, ByteString) -> Prog ())
pickRecvFiber to w =
  listToMaybe
    [ (fid, cont)
    | (fid, fb) <- IntMap.toAscList (wFibers w)
    , fbNode fb == to
    , fbStatus fb == FBlockedRecv
    , AwaitRecv cont <- [fbResume fb]
    ]

-- | Whether a node may currently make progress (alive and not paused).
nodeUp :: NodeId -> World -> Bool
nodeUp (NodeId n) w = case IntMap.lookup n (wNodes w) of
  Nothing -> False
  Just ns -> nsAlive ns && maybe True (<= wClock w) (nsPausedUntil ns)

linkLive :: NodeId -> NodeId -> World -> Bool
linkLive a b w = case Map.lookup (a, b) (wLinks w) of
  Nothing -> True
  Just lp -> not (lpPartitioned lp) && maybe True (<= wClock w) (lpClogUntil lp)

deliverable :: Int -> World -> Bool
deliverable m w = case IntMap.lookup m (wInFlight w) of
  Nothing -> False
  Just inf ->
    ifDueAt inf <= wClock w
      && linkLive (ifFrom inf) (ifTo inf) w
      && nodeUp (ifTo inf) w
      && any (\fb -> fbNode fb == ifTo inf && fbStatus fb == FBlockedRecv) (IntMap.elems (wFibers w))

-- | Legal next decisions in a fixed total order: runnable fibers (by id), then
-- deliverable messages (by id), then the fault menu (by index).
runnable :: World -> [Decision]
runnable w =
  [SchedNext (FiberId k) | (k, fb) <- IntMap.toAscList (wFibers w), fbStatus fb == FRunnable, nodeUp (fbNode fb) w]
    ++ [DeliverMsg (MsgId m) | m <- IntMap.keys (wInFlight w), deliverable m w]
    ++ [FireFault i | i <- [0 .. length (availableFaults (wNemesis w) w) - 1]]

-- * Time ------------------------------------------------------------------

-- | Wake times at or before the current clock (timers/pauses that are due).
dueNow :: World -> Bool
dueNow w =
  any (\fb -> case fbStatus fb of FBlockedTimer t -> t <= wClock w; _ -> False) (IntMap.elems (wFibers w))
    || any (\ns -> maybe False (<= wClock w) (nsPausedUntil ns)) (IntMap.elems (wNodes w))

-- | The earliest strictly-future wake time: a timer, clog expiry, pause end,
-- or the due time of an in-flight message. Including message due times lets the
-- driver advance the clock to deliver a latency-delayed message rather than
-- mistaking the wait for a deadlock.
earliestFuture :: World -> Maybe SimTime
earliestFuture w =
  case futures of
    [] -> Nothing
    ts -> Just (minimum ts)
  where
    clk = wClock w
    timers = [t | fb <- IntMap.elems (wFibers w), FBlockedTimer t <- [fbStatus fb], t > clk]
    pauses = [t | ns <- IntMap.elems (wNodes w), Just t <- [nsPausedUntil ns], t > clk]
    clogs = [t | lp <- Map.elems (wLinks w), Just t <- [lpClogUntil lp], t > clk]
    msgs = [ifDueAt inf | inf <- IntMap.elems (wInFlight w), ifDueAt inf > clk]
    futures = timers ++ pauses ++ clogs ++ msgs

-- | Whether the driver can make progress purely by advancing the clock.
canAdvanceTime :: World -> Bool
canAdvanceTime w = dueNow w || earliestFuture w /= Nothing

-- | Jump the clock to the earliest pending wake (or just wake due timers if
-- some are already due) and re-enable the woken fibers/nodes. If nothing is
-- pending, returns the world unchanged.
advanceClock :: World -> World
advanceClock w
  | dueNow w = wakeDue w
  | otherwise = case earliestFuture w of
      Nothing -> w
      Just t -> wakeDue w {wClock = t}

-- Wake timer fibers whose time has arrived and unpause nodes whose pause ended.
wakeDue :: World -> World
wakeDue w =
  w
    { wFibers = IntMap.map wakeFib (wFibers w)
    , wNodes = IntMap.map unpause (wNodes w)
    }
  where
    clk = wClock w
    wakeFib fb = case (fbStatus fb, fbResume fb) of
      (FBlockedTimer t, AwaitTimer _ p) | t <= clk -> fb {fbStatus = FRunnable, fbResume = Ready p}
      _ -> fb
    unpause ns = case nsPausedUntil ns of
      Just t | t <= clk -> ns {nsPausedUntil = Nothing}
      _ -> ns

-- | Any fiber blocked on @Recv@.
anyBlockedRecv :: World -> Bool
anyBlockedRecv w = any ((== FBlockedRecv) . fbStatus) (IntMap.elems (wFibers w))
-- | A genuine deadlock: an in-flight message addressed to a /live/ node that
-- has a receiver blocked waiting for it. At a terminal state (no runnable
-- decision, no advanceable time) such a message can never arrive — it is held
-- by a permanent partition — which is a real liveness bug. Messages to a
-- crashed node (lost) and servers idling on @Recv@ with no pending message
-- (a bounded workload that simply finished) are /not/ deadlocks; assert
-- liveness with 'reachable' / 'sometimes' beacons instead.
deadlocked :: World -> Bool
deadlocked w = any stuck (IntMap.elems (wInFlight w))
  where
    stuck inf = aliveNode (ifTo inf) && blockedRecvOn (ifTo inf)
    aliveNode (NodeId n) = maybe False nsAlive (IntMap.lookup n (wNodes w))
    blockedRecvOn to =
      any (\fb -> fbNode fb == to && fbStatus fb == FBlockedRecv) (IntMap.elems (wFibers w))

-- | Terminal-clean: no decisions, no pending time, and not deadlocked.
quiescent :: World -> Bool
quiescent w = null (runnable w) && not (canAdvanceTime w) && not (deadlocked w)

-- * Failure detection -----------------------------------------------------

-- | Per-event failure: an @Always@ / @AlwaysOrUnreachable@ predicate that was
-- false when reached, or an @Unreachable@ site that was reached at all.
eventFail :: SimEvent -> Maybe FailReason
eventFail = \case
  EvAssert s Always False -> Just (AssertViolated s Always)
  EvAssert s AlwaysOrUnreachable False -> Just (AssertViolated s AlwaysOrUnreachable)
  EvAssert s Unreachable True -> Just (AssertViolated s Unreachable)
  _ -> Nothing

firstAssertFail :: [SimEvent] -> Maybe FailReason
firstAssertFail = listToMaybe . mapMaybe eventFail

-- * replay ----------------------------------------------------------------

-- | Reproduce a run exactly. Feeds @riPath@ decision-by-decision; when the next
-- recorded decision is not yet applicable (needs a clock advance) it calls
-- 'advanceClock'; once the path is exhausted it drives the remaining
-- non-fault decisions to quiescence (it never self-fires faults). Stops at the
-- first failure (assert or invariant), at deadlock, or at clean quiescence.
replay :: Scenario -> RunInput -> RunResult
replay scen ri = loop w1 (riPath ri) [] maxDriveSteps
  where
    inv = scenInvariant scen
    w0 = initWorld (riSeed ri) scen
    w1 = w0 {wBuggifyOverride = riBuggify ri}

    done w acc mr = RunResult w (reverse acc) mr

    loop w path acc fuel
      | fuel <= 0 = done w acc Nothing
      | not (inv w) = done w acc (Just (InvariantBroken "scenario invariant"))
      | otherwise = case path of
          (d : ds)
            | d `elem` runnable w -> advance w d ds acc fuel
            | canAdvanceTime w -> loop (advanceClock w) path acc (fuel - 1)
            | otherwise -> loop w ds acc fuel -- stale decision; skip it
          [] ->
            let progress = filter (not . isFault) (runnable w)
             in case progress of
                  (d : _) -> advance w d [] acc fuel
                  [] ->
                    if canAdvanceTime w
                      then loop (advanceClock w) [] acc (fuel - 1)
                      else if deadlocked w then done w acc (Just Deadlock) else done w acc Nothing

    advance w d rest acc fuel =
      let StepResult w' evs = step w d
          acc' = reverse evs ++ acc
       in case firstAssertFail evs of
            Just r -> done w' acc' (Just r)
            Nothing -> loop w' rest acc' (fuel - 1)

isFault :: Decision -> Bool
isFault (FireFault _) = True
isFault _ = False
