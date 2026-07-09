{- | Live queries (spec §12): the origin-side subscription machinery.

This module is deliberately transport- and origin-agnostic: it never sees
'Lattice.Server.Origin' or the HTTP request. The server hands it a bus
subscription (@'Data.Word.Word64'@ outbox cursor + surrogate keys, the
shape of @InvalEvent@), a re-execution action producing a 'LiveSnapshot',
and the proof-expiry facts; it hands back a frame source for
'Network.HTTP.Client.SSE.sseResponseBodyFrames' plus an idempotent
teardown. All SSE wire grammar comes from "Network.HTTP.Client.SSE" —
this module only decides /which/ frames exist, never how they render.

== Table shape (§12)

Subscribers group by 'LiveKey' — query hash, watch target (one slice or
the whole page), canonically-rendered variable bindings, the raw @vc@
claims payload, and (page targets with a nonempty priv slice only) a
digest of the private credential. Every member of a group would receive
byte-identical executions, so the group is the single-flight unit: one
worker thread per group holds the group's bus subscription (created
__before__ the first snapshot executes, so no event can fall between
snapshot and registration), drains every queued bus event in one gulp,
and runs at most one re-execution for the batch — concurrent triggers
coalesce, and the output fans out to every member.

== Registered keys

The group registers the surrogate-key set of its last emission and
replaces it with the new manifest's on every delta execution (§12). A
subscriber joining an existing group unions its snapshot's keys into the
registered set rather than replacing it — the group's other members may
not yet have seen an in-flight event, and a union can only over-trigger
(a spurious re-execution diffs to nothing), never miss.

== Delta pins

A re-execution whose per-section etag vector equals the previous
emission's pushes nothing (a coarsened-key false positive). Otherwise the
delta walks the sections in slice order: a fresh manifest first /iff/
that section's root membership changed, then entity records whose
rendered bytes changed, then changed tombstones, and after every section
the end record. Diff identity is @(slice, id)@ — the same entity may
legitimately ride in two sections with different field subsets (§8.1
plans a fragment once per level). Delta events carry @id: {outbox
cursor}@ of the last coalesced trigger; snapshot events carry no id.
A page re-execution that cannot obtain a single-snapshot window reports
"skip" (@Right Nothing@): members stay, and the intersecting write that
caused the contention has already queued the group's next trigger.

== Slow and dead subscribers

Fan-out never blocks: each subscriber owns a bounded frame queue
('subscriberQueueBound'); a subscriber that far behind is treated as dead
— its queue is closed and the worker unregisters it (the §12 teardown on
write failure; the HTTP layer stopping its popper has the same effect
once the queue drains or overflows). The frame source runs the teardown
itself when it reaches end-of-stream, so every exit path unregisters.

== Reauth (§12)

When the subscription's proof carries an expiry, a per-subscriber timer
(a 'registerDelay' loop against the supplied clock — never a blocking
sleep that tests would have to wait out; drive it with a fake clock and
an already-lapsed proof for determinism) pushes @{\"kind\":\"reauth\"}@
at expiry and closes the stream after the configured grace. A client
re-presenting a fresh proof does so by reconnecting — the in-connection
credential upgrade is out of scope for v1.
-}
module Lattice.Server.Live (
  -- * Configuration
  LiveConfig (..),
  defaultLiveConfig,

  -- * Per-origin state
  LiveState,
  newLiveState,
  liveSubscriberCount,

  -- * Subscribing
  LiveKey (..),
  LiveTarget (..),
  LiveSection (..),
  LiveSnapshot (..),
  LiveSub (..),
  LiveRefused (..),
  liveSubscribe,
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (unless, void, when)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Foldable (for_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Traversable (for)
import Data.Word (Word64)
import Lattice.Types (Ref, SliceName, renderRef)
import Lattice.Wire (EndRecord (..), EntityRecord (..), Manifest (..), Record (..), SurrogateKey, encodeRecord)
import Network.HTTP.Client.SSE (ServerSentEvent (..), SseFrame (..), defaultSseEvent)

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

data LiveConfig = LiveConfig
  { livePingMicros :: Int
  -- ^ Idle keep-alive period for the @: ping@ comment; @<= 0@ disables
  -- the ping loop entirely (deterministic tests want that, or a tiny
  -- period they block-read against).
  , liveReauthGraceMicros :: Int
  -- ^ How long a stream survives after its reauth record (§12); @<= 0@
  -- closes immediately after the record is queued.
  , liveMaxSubscribers :: Int
  -- ^ Per-origin subscriber cap; the origin answers 503 over it.
  }
  deriving stock (Eq, Show)


defaultLiveConfig :: LiveConfig
defaultLiveConfig =
  LiveConfig
    { livePingMicros = 15_000_000
    , liveReauthGraceMicros = 10_000_000
    , liveMaxSubscribers = 1024
    }


-- ---------------------------------------------------------------------------
-- State
-- ---------------------------------------------------------------------------

{- | The §12 single-flight identity: hash-form query, watch target,
canonical variable bindings, raw @vc@ claims payload (@\"\"@ when absent),
and the private-credential digest (@\"\"@ except on page targets whose
plan has a nonempty priv slice — the page re-entangles audiences, so its
single-flight identity includes the principal). Execution output is a
function of exactly this tuple, so members of one group can share every
emission.
-}
data LiveKey = LiveKey
  { lkHash :: Text
  , lkTarget :: LiveTarget
  , lkVars :: Text
  , lkClaims :: Text
  , lkAuth :: Text
  }
  deriving stock (Eq, Ord, Show)


-- | What a subscription watches: one authorization slice, or the whole
-- page multiplexed over one stream (§12 page subscriptions).
data LiveTarget = LiveSlice SliceName | LivePage
  deriving stock (Eq, Ord, Show)


{- | One slice's contribution to an execution: framed as
@manifest → records@, concatenated in slice order across sections.
-}
data LiveSection = LiveSection
  { lsecSlice :: SliceName
  , lsecManifest :: Manifest
  , lsecRecords :: [Record]
  -- ^ Entity\/tombstone\/elided\/error records, already projected.
  , lsecRoot :: Map Text [Ref]
  -- ^ Root membership (the per-section manifest-changed test).
  , lsecEtag :: Text
  }


{- | One execution's live-relevant projection, built by the origin's
executor. Per-slice subscriptions carry exactly one section; page
subscriptions one per nonempty slice, composed under a single snapshot.
-}
data LiveSnapshot = LiveSnapshot
  { lsSections :: [LiveSection]
  -- ^ Slice order.
  , lsKeys :: Set SurrogateKey
  -- ^ The response's surrogate keys — what the group registers.
  , lsComplete :: Bool
  }


{- | The group's last emission, the basis every delta diffs against.

The diff basis is the /rendered bytes/ per @(slice, id)@, not @(id, ver)@
alone: a paginated-edge occurrence or an @on read@ derived value rides in
its owner's @fields@ and can change while the owner's @ver@ stands still
(e.g. a comment insert that never touches the post row). Byte equality
is exact and strictly more sensitive than the version pair, so every
@(id, ver)@ change still re-emits. The slice component keeps sections
independent: one entity emitted in two sections diffs per section.
-}
data LiveEmission = LiveEmission
  { leBytes :: Map (SliceName, Text) ByteString
  -- ^ (slice, id) → the record's encoded bytes at last emission.
  , leEtags :: [(SliceName, Text)]
  -- ^ Per-section etag vector, in slice order.
  , leRoots :: Map SliceName (Map Text [Ref])
  , leKeys :: Set SurrogateKey
  }


data Subscriber = Subscriber
  { subQueue :: TVar (Seq SseFrame)
  , subClosed :: TVar Bool
  , subActivity :: TVar Word64
  -- ^ Dispatched-frame counter; the ping loop skips periods with traffic.
  }


data LiveGroup = LiveGroup
  { lgLock :: MVar ()
  -- ^ The single-flight execution lock: joins and delta re-executions
  -- serialize here.
  , lgLast :: TVar LiveEmission
  , lgMembers :: TVar (Map Word64 Subscriber)
  , lgStop :: TVar Bool
  }


-- | Per-origin subscription table.
data LiveState = LiveState
  { liveGroups :: TVar (Map LiveKey LiveGroup)
  , liveCount :: TVar Int
  , liveNextId :: TVar Word64
  , liveJoinLock :: MVar ()
  -- ^ Serializes group creation\/join\/unregister so a group can never
  -- be torn down while a joiner holds a reference to it. Always taken
  -- before 'lgLock', never after — the worker (which holds 'lgLock')
  -- defers its unregisters until the lock is released.
  }


newLiveState :: IO LiveState
newLiveState =
  LiveState
    <$> newTVarIO Map.empty
    <*> newTVarIO 0
    <*> newTVarIO 1
    <*> newMVar ()


-- | Current subscriber count across every group (the cap's meter).
liveSubscriberCount :: LiveState -> IO Int
liveSubscriberCount st = readTVarIO (liveCount st)


-- ---------------------------------------------------------------------------
-- Subscriber queues
-- ---------------------------------------------------------------------------

{- | Frames a subscriber may fall behind before it is declared dead. One
frame is one SSE event (one record), so this bounds per-subscriber memory
at roughly a full large response.
-}
subscriberQueueBound :: Int
subscriberQueueBound = 1024


{- | Queue a frame. 'False' means the subscriber is gone: already closed,
or it just overflowed its bound (which closes it — the caller unregisters).
-}
pushFrame :: Subscriber -> SseFrame -> STM Bool
pushFrame s f = do
  closed <- readTVar (subClosed s)
  if closed
    then pure False
    else do
      q <- readTVar (subQueue s)
      if Seq.length q >= subscriberQueueBound
        then do
          writeTVar (subClosed s) True
          pure False
        else do
          writeTVar (subQueue s) (q Seq.|> f)
          case f of
            SseDispatch _ -> modifyTVar' (subActivity s) (+ 1)
            _ -> pure ()
          pure True


-- | Blocking pop: 'Nothing' only once the queue is closed /and/ drained.
popFrame :: Subscriber -> STM (Maybe SseFrame)
popFrame s = do
  q <- readTVar (subQueue s)
  case Seq.viewl q of
    f Seq.:< rest -> do
      writeTVar (subQueue s) rest
      pure (Just f)
    Seq.EmptyL -> do
      closed <- readTVar (subClosed s)
      if closed then pure Nothing else retry


-- ---------------------------------------------------------------------------
-- Frames
-- ---------------------------------------------------------------------------

recordFrame :: Maybe Word64 -> Record -> SseFrame
recordFrame mCursor r =
  SseDispatch
    defaultSseEvent
      { sseEventId = BS8.pack . show <$> mCursor
      , sseData = encodeRecord r
      }


snapshotFrames :: LiveSnapshot -> [SseFrame]
snapshotFrames snap =
  map
    (recordFrame Nothing)
    (concatMap sectionRecords (lsSections snap) <> [endRecord snap])


sectionRecords :: LiveSection -> [Record]
sectionRecords sec = RManifest (lsecManifest sec) : lsecRecords sec


{- | Single-section streams keep the one-slice wire shape (the end record
carries the etag); page streams carry the validators on their per-section
manifests instead (§6.5 framing) and end with none.
-}
endRecord :: LiveSnapshot -> Record
endRecord snap = REnd (EndRecord (lsComplete snap) etag)
  where
    etag = case lsSections snap of
      [sec] -> Just (lsecEtag sec)
      _ -> Nothing


etagVector :: LiveSnapshot -> [(SliceName, Text)]
etagVector = map (\sec -> (lsecSlice sec, lsecEtag sec)) . lsSections


{- | The id of an id-bearing record, rendered as the wire spells it.
Manifest\/end\/plan\/error records carry no id and never join the diff
basis (errors re-emit only via the etag change they caused).
-}
recordId :: Record -> Maybe Text
recordId = \case
  REntity er -> Just (renderRef (erId er))
  RTombstone r _ _ -> Just (renderRef r)
  RElided r -> Just (renderRef r)
  RUnchanged r _ -> Just (renderRef r)
  _ -> Nothing


-- | (slice, id) → rendered bytes for every id-bearing record.
recordBytes :: [LiveSection] -> Map (SliceName, Text) ByteString
recordBytes secs = Map.fromList $ do
  sec <- secs
  r <- lsecRecords sec
  case recordId r of
    Nothing -> []
    Just i -> [((lsecSlice sec, i), encodeRecord r)]


{- | The §12 delta, or @[]@ when the emission is unchanged (etag-vector
equal — a coarsened-key false positive re-execution). Pin order, per
section in slice order: fresh manifest /iff/ that section's root
membership changed, then changed entity records, then changed tombstones
(elisions ride in this bucket: a row leaving visibility is the same kind
of fact); after all sections, the end record. Changed = the @(slice,
id)@'s rendered bytes differ from the previous emission's (new ids
included).
-}
deltaFrames :: Word64 -> LiveEmission -> LiveSnapshot -> [SseFrame]
deltaFrames cursor prev snap
  | etagVector snap == leEtags prev = []
  | otherwise =
      map (recordFrame (Just cursor)) $
        concatMap sectionDelta (lsSections snap) <> [endRecord snap]
  where
    sectionDelta sec = manifestPart <> entities <> others
      where
        manifestPart
          | Map.lookup (lsecSlice sec) (leRoots prev) /= Just (lsecRoot sec) =
              [RManifest (lsecManifest sec)]
          | otherwise = []
        changed r = case recordId r of
          Nothing -> False
          Just i -> Map.lookup (lsecSlice sec, i) (leBytes prev) /= Just (encodeRecord r)
        isEntity = \case
          REntity _ -> True
          _ -> False
        changedRecs = filter changed (lsecRecords sec)
        entities = filter isEntity changedRecs
        others = filter (not . isEntity) changedRecs


emissionOf :: LiveSnapshot -> LiveEmission
emissionOf snap =
  LiveEmission
    { leBytes = recordBytes (lsSections snap)
    , leEtags = etagVector snap
    , leRoots = Map.fromList (map (\sec -> (lsecSlice sec, lsecRoot sec)) (lsSections snap))
    , leKeys = lsKeys snap
    }


-- ---------------------------------------------------------------------------
-- Subscribing
-- ---------------------------------------------------------------------------

-- | A live subscription as the HTTP layer consumes it.
data LiveSub = LiveSub
  { lsubSource :: IO (Maybe SseFrame)
  -- ^ Frame source for 'Network.HTTP.Client.SSE.sseResponseBodyFrames'.
  -- Runs the teardown itself when it yields end-of-stream.
  , lsubClose :: IO ()
  -- ^ Idempotent teardown: close the queue, unregister, retire the
  -- group when it empties. Wire it to whatever abort hook the
  -- transport offers.
  }


data LiveRefused e
  = LiveOverCapacity
  | LiveContention
  -- ^ The initial page snapshot could not be composed under a single
  -- storage snapshot after bounded retries (§12; the origin answers
  -- @503 lattice:snapshot-contention@).
  | LiveExecRefused e
  -- ^ The initial snapshot execution refused (the pull path's problem).


{- | Register a subscriber. The bus subscription is created before the
initial snapshot executes (no event gap); the snapshot burst is queued
before this returns, so the transport's first pops see
@: lattice live@, the manifest, records, and the end record.
-}
liveSubscribe ::
  LiveConfig ->
  LiveState ->
  LiveKey ->
  -- | Subscribe to the invalidation bus: a blocking read of
  -- (outbox cursor, purged keys).
  IO (STM (Word64, [SurrogateKey])) ->
  -- | Execute the plan once (the pull pipeline minus framing).
  -- @Right Nothing@ = transient single-snapshot contention on a page
  -- target: refused at subscribe time, skipped on a delta.
  IO (Either e (Maybe LiveSnapshot)) ->
  -- | The presented proof's expiry, when it carries one (§12 reauth).
  Maybe POSIXTime ->
  -- | The origin's clock.
  IO POSIXTime ->
  IO (Either (LiveRefused e) LiveSub)
liveSubscribe cfg st key subscribeBus execute mExpiry now = do
  admitted <- atomically $ do
    n <- readTVar (liveCount st)
    if n >= max 0 (liveMaxSubscribers cfg)
      then pure False
      else do
        writeTVar (liveCount st) (n + 1)
        pure True
  if not admitted
    then pure (Left LiveOverCapacity)
    else do
      res <- withMVar (liveJoinLock st) $ \_ -> do
        (grp, created) <- lookupOrCreateGroup
        joined <- withMVar (lgLock grp) $ \_ ->
          execute >>= \case
            Left e -> pure (Left (LiveExecRefused e))
            Right Nothing -> pure (Left LiveContention)
            Right (Just snap) -> Right <$> register grp created snap
        case joined of
          Left e -> do
            when created (retireGroup grp)
            pure (Left e)
          Right sub -> pure (Right sub)
      case res of
        Left e -> do
          atomically (modifyTVar' (liveCount st) (subtract 1))
          pure (Left e)
        Right sub -> pure (Right sub)
  where
    lookupOrCreateGroup = do
      groups <- readTVarIO (liveGroups st)
      case Map.lookup key groups of
        Just grp -> pure (grp, False)
        Nothing -> do
          busRead <- subscribeBus
          grp <-
            LiveGroup
              <$> newMVar ()
              <*> newTVarIO (LiveEmission Map.empty [] Map.empty Set.empty)
              <*> newTVarIO Map.empty
              <*> newTVarIO False
          atomically (modifyTVar' (liveGroups st) (Map.insert key grp))
          void (forkIO (groupWorker grp busRead))
          pure (grp, True)

    register grp created snap = do
      sid <- atomically (stateTVar (liveNextId st) (\i -> (i, i + 1)))
      sub <-
        Subscriber
          <$> newTVarIO Seq.empty
          <*> newTVarIO False
          <*> newTVarIO 0
      atomically $ do
        modifyTVar' (lgMembers grp) (Map.insert sid sub)
        if created
          then writeTVar (lgLast grp) (emissionOf snap)
          else modifyTVar' (lgLast grp) $ \e ->
            e {leKeys = leKeys e <> lsKeys snap}
        void (pushFrame sub (SseComment "lattice live"))
        for_ (snapshotFrames snap) (pushFrame sub)
      let close = unregister sid sub
      void (forkIO (pingLoop sub))
      for_ mExpiry (void . forkIO . reauthLoop sub)
      pure
        LiveSub
          { lsubSource = do
              mf <- atomically (popFrame sub)
              case mf of
                Nothing -> close >> pure Nothing
                justF -> pure justF
          , lsubClose = close
          }
      where
        unregister sid sub = withMVar (liveJoinLock st) $ \_ -> do
          emptied <- atomically $ do
            writeTVar (subClosed sub) True
            ms <- readTVar (lgMembers grp)
            let present = Map.member sid ms
                ms' = Map.delete sid ms
            writeTVar (lgMembers grp) ms'
            when present (modifyTVar' (liveCount st) (subtract 1))
            pure (present && Map.null ms')
          when emptied (retireGroup grp)

    retireGroup grp = do
      atomically $ do
        writeTVar (lgStop grp) True
        modifyTVar' (liveGroups st) (Map.delete key)

    -- The single-flight worker: drain every queued bus event, re-execute
    -- at most once for the batch, fan the delta out. Members whose queue
    -- refused a frame are unregistered after the execution lock drops
    -- (lock order: liveJoinLock before lgLock, never inside it).
    groupWorker grp busRead = loop
      where
        loop = do
          mEvs <- atomically $ do
            stop <- readTVar (lgStop grp)
            if stop
              then pure Nothing
              else do
                ev <- (Just <$> busRead) `orElse` pure Nothing
                case ev of
                  Nothing -> retry
                  Just e -> do
                    rest <- drainBus
                    pure (Just (e : rest))
          case mEvs of
            Nothing -> pure ()
            Just evs -> do
              dead <- process evs
              for_ dead (uncurry unregisterMember)
              loop

        drainBus = do
          m <- (Just <$> busRead) `orElse` pure Nothing
          case m of
            Nothing -> pure []
            Just e -> (e :) <$> drainBus

        process evs = withMVar (lgLock grp) $ \_ -> do
          prev <- readTVarIO (lgLast grp)
          let hit = any (any (`Set.member` leKeys prev) . snd) evs
              cursor = maximum (map fst evs)
          if not hit
            then pure []
            else
              try execute >>= \case
                Left (_ :: SomeException) -> closeMembers
                Right (Left _) -> closeMembers
                -- Transient page contention: keep every member; the
                -- intersecting write already queued the next trigger.
                Right (Right Nothing) -> pure []
                Right (Right (Just snap)) -> do
                  let frames = deltaFrames cursor prev snap
                  atomically $ do
                    writeTVar (lgLast grp) (emissionOf snap)
                    if null frames
                      then pure []
                      else do
                        ms <- readTVar (lgMembers grp)
                        fmap catMaybes . for (Map.toList ms) $ \(sid, s) -> do
                          oks <- for frames (pushFrame s)
                          pure (if and oks then Nothing else Just (sid, s))

        -- A failed re-execution cannot keep the stream honest: close
        -- every member; clients reconnect into a fresh snapshot (which
        -- surfaces the same problem as an ordinary response).
        closeMembers = atomically $ do
          ms <- readTVar (lgMembers grp)
          for_ ms (\s -> writeTVar (subClosed s) True)
          pure []

        unregisterMember sid sub = withMVar (liveJoinLock st) $ \_ -> do
          emptied <- atomically $ do
            writeTVar (subClosed sub) True
            ms <- readTVar (lgMembers grp)
            let present = Map.member sid ms
                ms' = Map.delete sid ms
            writeTVar (lgMembers grp) ms'
            when present (modifyTVar' (liveCount st) (subtract 1))
            pure (present && Map.null ms')
          when emptied (retireGroup grp)

    -- Keep-alive: a ": ping" comment after each idle period. Periods
    -- with dispatched traffic skip their ping; the ping itself is not
    -- traffic. Disabled at livePingMicros <= 0.
    pingLoop sub
      | livePingMicros cfg <= 0 = pure ()
      | otherwise = go =<< readTVarIO (subActivity sub)
      where
        go seen = do
          tv <- registerDelay (livePingMicros cfg)
          state <- atomically $ do
            closed <- readTVar (subClosed sub)
            if closed
              then pure Nothing
              else do
                expired <- readTVar tv
                if expired then Just <$> readTVar (subActivity sub) else retry
          case state of
            Nothing -> pure ()
            Just n -> do
              ok <-
                if n == seen
                  then atomically (pushFrame sub (SseComment "ping"))
                  else pure True
              when ok (go n)

    -- §12 reauth: at proof expiry (per the supplied clock) push the
    -- reauth record, then close after the grace window. registerDelay
    -- ticks are re-checked against the clock so a coarse timer never
    -- fires early; an already-lapsed proof fires immediately.
    reauthLoop sub expiry = do
      expired <- waitExpiry
      when expired $ do
        ok <- atomically (pushFrame sub (recordFrame Nothing RReauth))
        when ok $ do
          let grace = liveReauthGraceMicros cfg
          when (grace > 0) $ do
            tv <- registerDelay grace
            void . atomically $ do
              closed <- readTVar (subClosed sub)
              done <- readTVar tv
              unless (closed || done) retry
          atomically (writeTVar (subClosed sub) True)
      where
        waitExpiry = do
          t <- now
          let remain = ceiling ((expiry - t) * 1_000_000) :: Integer
          if remain <= 0
            then pure True
            else do
              tv <- registerDelay (fromIntegral (min remain 60_000_000))
              alive <- atomically $ do
                closed <- readTVar (subClosed sub)
                if closed
                  then pure False
                  else do
                    done <- readTVar tv
                    if done then pure True else retry
              if alive then waitExpiry else pure False
