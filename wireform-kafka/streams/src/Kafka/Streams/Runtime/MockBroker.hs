{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Streams.Runtime.MockBroker
Description : An in-memory Kafka cluster behind a 'StreamDriver'

Enough of a broker to run the streams runtime end to end in a test:

  * topics with partitions holding append-only logs (null values
    included, so changelog tombstones and table deletes work);
  * consumer groups with a real rejoin barrier: once a member joins
    or leaves, every member must rejoin (with fresh subscription user
    data) before anyone gets a new assignment, and until then the old
    generation can still commit, exactly like a group coordinator in
    @PreparingRebalance@. The first member to find the barrier
    complete is the leader: it runs its assignor (the
    'GroupAssignor' it subscribed with) over every member's
    subscription, and the result is installed unless the group moved
    on meanwhile;
  * transactions: records written in a transaction are invisible to
    read_committed readers until it commits and are skipped forever if
    it aborts; commits and aborts leave a control marker that takes an
    offset; offsets sent to a transaction are applied with it, and
    only if the sending member's generation is still current;
    @initTransactions@ fences the previous producer with the same
    transactional id;
  * fault injection for sends, transaction commits and offset commits.

Several drivers ('newMockDriver') on one broker are several streams
instances of the same application.
-}
module Kafka.Streams.Runtime.MockBroker (
  -- * Broker
  MockBroker,
  newMockBroker,
  mockCreateTopic,
  mockPartitionCount,
  mockProduce,
  mockProduceTo,
  mockRecords,
  mockVisibleRecords,
  mockCommittedOffsets,
  mockGroupMembers,
  mockGeneration,
  mockTopics,
  mockSetPartitionRacks,

  -- * Faults
  mockFailSends,
  mockFailTransactionCommits,
  mockFailOffsetCommits,

  -- * Drivers
  MockDriverOptions (..),
  defaultMockDriverOptions,
  newMockDriver,
  mockDriverFor,
) where

import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (toList)
import Data.IORef
import Data.Int (Int32, Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Streams.Internal.Text (decimalText)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Kafka.Client.Assignor (CustomAssignor (..), MemberAssignment (..), MemberSubscription (..))
import Kafka.Client.Consumer qualified as KC
import Kafka.Client.Internal.Murmur2 qualified as Murmur2
import Kafka.Client.Producer qualified as KP
import Kafka.Streams.Config (ProcessingGuarantee (..), StreamsConfig (..))
import Kafka.Streams.Runtime.Driver


----------------------------------------------------------------------
-- State
----------------------------------------------------------------------

data Entry
  = Data !StoredRecord
  | -- | A transaction's commit / abort marker: takes an offset, never read
    Marker


data StoredRecord = StoredRecord
  { srKey :: !(Maybe ByteString)
  , srValue :: !(Maybe ByteString)
  , srTimestamp :: !Int64
  , srHeaders :: ![(Text, ByteString)]
  , srTxn :: !(Maybe TxnRef)
  }


-- | One run of a transactional id: its id and a per-broker instance number.
data TxnRef = TxnRef !Text !Int
  deriving stock (Eq, Ord, Show)


data TxnStatus = TxnOpen | TxnCommitted | TxnAborted
  deriving stock (Eq, Show)


data Member = Member
  { mTopics :: ![Text]
  , mGeneration :: !Int
  -- ^ The generation whose assignment this member holds (@-1@: none yet)
  , mRejoined :: !Bool
  -- ^ Has rejoined for the pending rebalance
  , mTarget :: !(Set KC.TopicPartition)
  -- ^ Its share of the current generation, taken at its next poll
  , mAssignment :: !(Set KC.TopicPartition)
  , mPositions :: !(Map KC.TopicPartition Int64)
  , mPaused :: !(Set KC.TopicPartition)
  , mRejoinRequested :: !Bool
  , mAssignor :: !CustomAssignor
  , mUserData :: !ByteString
  -- ^ Its subscription user data, as of its last join
  , mTargetUserData :: !ByteString
  -- ^ The assignment user data the leader attached to 'mTarget'
  }


data Group = Group
  { gGeneration :: !Int
  , gPending :: !Bool
  -- ^ A rebalance has been triggered and not every member has rejoined
  , gMembers :: !(Map Text Member)
  , gOffsets :: !(Map KC.TopicPartition Int64)
  , gAssigning :: !Bool
  -- ^ Every member has rejoined and a leader is running the assignor
  , gEpoch :: !Int
  -- ^ Bumped by every trigger: a leader's result for an older epoch is dropped
  }


emptyGroup :: Group
emptyGroup = Group 0 False Map.empty Map.empty False 0


data Producer = Producer
  { pEpoch :: !Int
  , pOpen :: !(Maybe OpenTxn)
  }


data OpenTxn = OpenTxn
  { otRef :: !TxnRef
  , otPartitions :: !(Set KC.TopicPartition)
  , otOffsets :: !(Map Text (Map KC.TopicPartition Int64))
  }


data St = St
  { stTopics :: !(Map Text (Map Int32 (Seq Entry)))
  , stGroups :: !(Map Text Group)
  , stTxnStatus :: !(Map TxnRef TxnStatus)
  , stProducers :: !(Map Text Producer)
  , stNextTxnInstance :: !Int
  , stNextMember :: !Int
  , stVersion :: !Int
  -- ^ Bumped on every change a poller might be waiting for
  , stFailSends :: !(Map Text Int)
  , stFailTxnCommits :: !Int
  , stFailOffsetCommits :: !Int
  , stAutoCreatePartitions :: !(Maybe Int)
  , stRoundRobin :: !Int
  , stLogStart :: !(Map KC.TopicPartition Int64)
  -- ^ Records before these offsets are deleted
  , stRacks :: !(Map KC.TopicPartition (Set Text))
  -- ^ The racks of each partition's replicas ('mockSetPartitionRacks')
  }


newtype MockBroker = MockBroker (TVar St)


{- | A broker with no topics. Producing to a missing topic creates it
with the given number of partitions ('Nothing': the send fails).
-}
newMockBroker :: Maybe Int -> IO MockBroker
newMockBroker autoCreate =
  MockBroker
    <$> newTVarIO
      St
        { stTopics = Map.empty
        , stGroups = Map.empty
        , stTxnStatus = Map.empty
        , stProducers = Map.empty
        , stNextTxnInstance = 0
        , stNextMember = 0
        , stVersion = 0
        , stFailSends = Map.empty
        , stFailTxnCommits = 0
        , stFailOffsetCommits = 0
        , stAutoCreatePartitions = autoCreate
        , stRoundRobin = 0
        , stLogStart = Map.empty
        , stRacks = Map.empty
        }


bump :: St -> St
bump s = s {stVersion = stVersion s + 1}


-- | Create a topic (no-op if it exists).
mockCreateTopic :: MockBroker -> Text -> Int -> IO ()
mockCreateTopic (MockBroker v) t n = atomically $ modifyTVar' v (createTopicSt t n)


createTopicSt :: Text -> Int -> St -> St
createTopicSt t n s
  | Map.member t (stTopics s) = s
  | otherwise =
      bump
        s
          { stTopics =
              Map.insert t (Map.fromList (map (\p -> (p, Seq.empty)) [0 .. fromIntegral n - 1])) (stTopics s)
          }


mockPartitionCount :: MockBroker -> Text -> IO (Maybe Int)
mockPartitionCount (MockBroker v) t = fmap Map.size . Map.lookup t . stTopics <$> readTVarIO v


-- | Place @topic@'s partitions' replicas: the racks of partition @p@ are the @p@-th set.
mockSetPartitionRacks :: MockBroker -> Text -> [Set Text] -> IO ()
mockSetPartitionRacks (MockBroker v) topic racks =
  atomically $ modifyTVar' v $ \s ->
    s {stRacks = Map.union (Map.fromList (zipWith (\p rs -> (KC.TopicPartition topic p, rs)) [0 ..] racks)) (stRacks s)}


mockTopics :: MockBroker -> IO [Text]
mockTopics (MockBroker v) = Map.keys . stTopics <$> readTVarIO v


{- | Produce outside any transaction, partitioned by the key like the
real producer's default partitioner (murmur2; keyless records go to
partition 0).
-}
mockProduce :: MockBroker -> Text -> Maybe ByteString -> Maybe ByteString -> Int64 -> IO ()
mockProduce b t k v ts = () <$ mockProduceTo b t Nothing k v ts []


-- | Produce with an explicit partition and headers; returns the offset.
mockProduceTo
  :: MockBroker
  -> Text
  -> Maybe Int32
  -> Maybe ByteString
  -> Maybe ByteString
  -> Int64
  -> [(Text, ByteString)]
  -> IO (Either String (Int32, Int64))
mockProduceTo (MockBroker v) t mp k val ts hs = atomically $ do
  s <- readTVar v
  case appendSt t mp (StoredRecord k val ts hs Nothing) s of
    Left e -> pure (Left e)
    Right (s', p, off) -> do
      writeTVar v s'
      pure (Right (p, off))


appendSt :: Text -> Maybe Int32 -> StoredRecord -> St -> Either String (St, Int32, Int64)
appendSt t mp r s0 = do
  s <- case (Map.member t (stTopics s0), stAutoCreatePartitions s0) of
    (True, _) -> Right s0
    (False, Just n) -> Right (createTopicSt t n s0)
    (False, Nothing) -> Left ("unknown topic " <> T.unpack t)
  let parts = stTopics s Map.! t
      !n = fromIntegral (Map.size parts) :: Int32
  p <- case mp of
    Just p
      | p >= 0 && p < n -> Right p
      | otherwise -> Left ("partition " <> show p <> " out of range for " <> T.unpack t)
    Nothing -> Right (maybe 0 (`Murmur2.partitionForKey` n) (srKey r))
  let lg = parts Map.! p
      !off = fromIntegral (Seq.length lg)
      parts' = Map.insert p (lg Seq.|> Data r) parts
  Right (bump s {stTopics = Map.insert t parts' (stTopics s)}, p, off)


toDriverRecord :: Text -> Int32 -> Int64 -> StoredRecord -> DriverRecord
toDriverRecord t p off r =
  DriverRecord
    { drTopic = t
    , drPartition = p
    , drOffset = off
    , drTimestamp = srTimestamp r
    , drKey = srKey r
    , drValue = srValue r
    , drHeaders = srHeaders r
    }


-- | Every data record of a topic (transactional or not), partition by partition.
mockRecords :: MockBroker -> Text -> IO [DriverRecord]
mockRecords (MockBroker v) t = do
  s <- readTVarIO v
  pure $ case Map.lookup t (stTopics s) of
    Nothing -> []
    Just parts ->
      concatMap
        ( \(p, lg) ->
            mapMaybe
              (\(off, e) -> case e of Data r -> Just (toDriverRecord t p off r); Marker -> Nothing)
              (zip [0 ..] (toList lg))
        )
        (Map.toList parts)


-- | What a read_committed consumer would read from a topic, partition by partition.
mockVisibleRecords :: MockBroker -> Text -> IO [DriverRecord]
mockVisibleRecords (MockBroker v) t = do
  s <- readTVarIO v
  pure $ case Map.lookup t (stTopics s) of
    Nothing -> []
    Just parts ->
      concatMap
        (\p -> fst (readFrom s KC.ReadCommitted (KC.TopicPartition t p) 0 maxBound))
        (Map.keys parts)


mockCommittedOffsets :: MockBroker -> Text -> IO (Map KC.TopicPartition Int64)
mockCommittedOffsets (MockBroker v) g = maybe Map.empty gOffsets . Map.lookup g . stGroups <$> readTVarIO v


mockGroupMembers :: MockBroker -> Text -> IO [Text]
mockGroupMembers (MockBroker v) g = maybe [] (Map.keys . gMembers) . Map.lookup g . stGroups <$> readTVarIO v


mockGeneration :: MockBroker -> Text -> IO Int
mockGeneration (MockBroker v) g = maybe 0 gGeneration . Map.lookup g . stGroups <$> readTVarIO v


-- | The next @n@ sends to @topic@ fail (and are not written).
mockFailSends :: MockBroker -> Text -> Int -> IO ()
mockFailSends (MockBroker v) t n = atomically $ modifyTVar' v $ \s -> s {stFailSends = Map.insert t n (stFailSends s)}


-- | The next @n@ transaction commits fail; the transaction stays open.
mockFailTransactionCommits :: MockBroker -> Int -> IO ()
mockFailTransactionCommits (MockBroker v) n = atomically $ modifyTVar' v $ \s -> s {stFailTxnCommits = n}


-- | The next @n@ group offset commits fail.
mockFailOffsetCommits :: MockBroker -> Int -> IO ()
mockFailOffsetCommits (MockBroker v) n = atomically $ modifyTVar' v $ \s -> s {stFailOffsetCommits = n}


----------------------------------------------------------------------
-- Reading
----------------------------------------------------------------------

{- | Records readable from @tp@ starting at @from@ (at most @n@), and
the position after them: past markers, aborted records and (for
read_uncommitted) everything; for read_committed it stops at the
first record of a still-open transaction.
-}
readFrom :: St -> KC.IsolationLevel -> KC.TopicPartition -> Int64 -> Int -> ([DriverRecord], Int64)
readFrom s iso tp from n =
  case Map.lookup tp.topic (stTopics s) >>= Map.lookup tp.partition of
    Nothing -> ([], from)
    Just lg ->
      let !start = max from (Map.findWithDefault 0 tp (stLogStart s))
      in go [] start 0 (Seq.drop (fromIntegral start) lg)
  where
    go acc !off !k es
      | k >= n = (reverse acc, off)
      | otherwise = case Seq.viewl es of
          Seq.EmptyL -> (reverse acc, off)
          Marker Seq.:< rest -> go acc (off + 1) k rest
          Data r Seq.:< rest -> case (iso, srTxn r >>= (`Map.lookup` stTxnStatus s)) of
            (KC.ReadCommitted, Just TxnOpen) -> (reverse acc, off)
            (KC.ReadCommitted, Just TxnAborted) -> go acc (off + 1) k rest
            _ -> go (toDriverRecord tp.topic tp.partition off r : acc) (off + 1) (k + 1) rest


logEnd :: St -> KC.TopicPartition -> Int64
logEnd s tp = maybe 0 (fromIntegral . Seq.length) (Map.lookup tp.topic (stTopics s) >>= Map.lookup tp.partition)


-- | The last stable offset: the first offset of a still-open transaction, or the log end.
lastStable :: St -> KC.TopicPartition -> Int64
lastStable s tp = case Map.lookup tp.topic (stTopics s) >>= Map.lookup tp.partition of
  Nothing -> 0
  Just lg ->
    maybe
      (fromIntegral (Seq.length lg))
      fromIntegral
      ( Seq.findIndexL
          (\case Data r -> (srTxn r >>= (`Map.lookup` stTxnStatus s)) == Just TxnOpen; Marker -> False)
          lg
      )


endFor :: KC.IsolationLevel -> St -> KC.TopicPartition -> Int64
endFor KC.ReadCommitted = lastStable
endFor KC.ReadUncommitted = logEnd


-- | Block until the broker changes or @ms@ pass.
waitForChange :: TVar St -> Int -> Int -> IO ()
waitForChange v seen ms
  | ms <= 0 = pure ()
  | otherwise = do
      timer <- registerDelay (ms * 1000)
      atomically $ do
        s <- readTVar v
        fired <- readTVar timer
        unless (fired || stVersion s /= seen) retry


----------------------------------------------------------------------
-- Groups
----------------------------------------------------------------------

triggerRebalance :: Group -> Group
triggerRebalance g =
  g
    { gPending = True
    , gAssigning = False
    , gEpoch = gEpoch g + 1
    , gMembers = Map.map (\m -> m {mRejoined = False}) (gMembers g)
    }


{- | Every member has rejoined: the member that sees it first claims the
leadership and runs the assignor (outside STM), over this snapshot.
-}
claimLeadership :: Group -> Maybe Group
claimLeadership g
  | gPending g && not (gAssigning g) && all mRejoined (Map.elems (gMembers g)) = Just g {gAssigning = True}
  | otherwise = Nothing


-- | Install the leader's assignment, unless the group moved on meanwhile.
installAssignment :: Int -> Map Text MemberAssignment -> Group -> Group
installAssignment epoch asg g
  | gEpoch g /= epoch || not (gAssigning g) = g
  | otherwise =
      g
        { gGeneration = gGeneration g + 1
        , gPending = False
        , gAssigning = False
        , gMembers =
            Map.mapWithKey
              ( \mid m ->
                  let ma = Map.findWithDefault (MemberAssignment [] BS.empty) mid asg
                  in m
                       { mRejoinRequested = False
                       , mTarget = Set.fromList (concatMap (\(t, ps) -> map (KC.TopicPartition t) ps) (maPartitions ma))
                       , mTargetUserData = maUserData ma
                       }
              )
              (gMembers g)
        }


modifyGroup :: Text -> (Group -> Group) -> St -> St
modifyGroup gid f s =
  let g0 = Map.findWithDefault emptyGroup gid (stGroups s)
  in bump s {stGroups = Map.insert gid (f g0) (stGroups s)}


----------------------------------------------------------------------
-- Drivers
----------------------------------------------------------------------

data MockDriverOptions = MockDriverOptions
  { mdoGroupId :: !Text
  , mdoClientId :: !Text
  , mdoIsolation :: !KC.IsolationLevel
  , mdoTransactionalId :: !(Maybe Text)
  , mdoResetToEarliest :: !Bool
  -- ^ Where a partition with no committed offset starts (else: the log end)
  , mdoMaxPollRecords :: !Int
  }


defaultMockDriverOptions :: MockDriverOptions
defaultMockDriverOptions =
  MockDriverOptions
    { mdoGroupId = "app"
    , mdoClientId = "client"
    , mdoIsolation = KC.ReadUncommitted
    , mdoTransactionalId = Nothing
    , mdoResetToEarliest = True
    , mdoMaxPollRecords = 1000
    }


data PollStep
  = NeedRejoin
  | Lead !Int !CustomAssignor ![MemberSubscription] !(Map Text Int32)
  | Waiting !Int
  | Synced ![KC.TopicPartition] ![KC.TopicPartition] ![KC.TopicPartition] !ByteString
  | Fetched ![DriverRecord]
  | NothingNew !Int
  | Fenced


nowMs :: IO Int64
nowMs = (\t -> round (t * 1000)) <$> getPOSIXTime


{- | A driver for one streams instance on @broker@: its consumer joins
'mdoGroupId', its producer uses 'mdoTransactionalId'.
-}
newMockDriver :: MockBroker -> MockDriverOptions -> IO StreamDriver
newMockDriver (MockBroker v) opts = do
  memberRef <- newIORef (Nothing :: Maybe Text)
  cbRef <- newIORef noRebalanceCallbacks
  udRef <- newIORef (pure BS.empty)
  epochRef <- newIORef (-1 :: Int)
  let gid = mdoGroupId opts
      iso = mdoIsolation opts

      withMember :: (Text -> IO (Either String a)) -> IO (Either String a)
      withMember k = readIORef memberRef >>= maybe (pure (Left "not subscribed")) k

      onMember :: Text -> (Member -> Member) -> IO ()
      onMember mid f =
        atomically $ modifyTVar' v $ modifyGroup gid $ \g -> g {gMembers = Map.adjust f mid (gMembers g)}

      startPosition s g tp = case Map.lookup tp (gOffsets g) of
        Just off -> off
        Nothing
          | mdoResetToEarliest opts -> Map.findWithDefault 0 tp (stLogStart s)
          | otherwise -> logEnd s tp

      resolve s tp off
        | off == -2 = Map.findWithDefault 0 tp (stLogStart s)
        | off == -1 = endFor iso s tp
        | otherwise = off

      pollStep :: Text -> STM PollStep
      pollStep mid = do
        s <- readTVar v
        case Map.lookup gid (stGroups s) of
          Nothing -> pure Fenced
          Just g -> case Map.lookup mid (gMembers g) of
            Nothing -> pure Fenced
            Just m
              | gPending g && not (mRejoined m) -> pure NeedRejoin
              | Just g' <- claimLeadership g -> do
                  writeTVar v s {stGroups = Map.insert gid g' (stGroups s)}
                  let subs =
                        map
                          (\(i, mem) -> MemberSubscription i (mTopics mem) (mUserData mem) [])
                          (Map.toList (gMembers g))
                      topics = Set.toList (Set.fromList (concatMap mTopics (Map.elems (gMembers g))))
                      counts = Map.fromList (map (\t -> (t, maybe 0 (fromIntegral . Map.size) (Map.lookup t (stTopics s)))) topics)
                  pure (Lead (gEpoch g) (mAssignor m) subs counts)
              | gPending g -> pure (Waiting (stVersion s))
              | mGeneration m < gGeneration g -> do
                  let new = mTarget m
                      old = mAssignment m
                      added = Set.difference new old
                      revoked = Set.difference old new
                      positions =
                        Map.union
                          (Map.restrictKeys (mPositions m) new)
                          (Map.fromSet (startPosition s g) added)
                      m' =
                        m
                          { mGeneration = gGeneration g
                          , mAssignment = new
                          , mPositions = positions
                          , mPaused = Set.intersection (mPaused m) new
                          }
                  writeTVar v (modifyGroup gid (\g' -> g' {gMembers = Map.insert mid m' (gMembers g')}) s)
                  pure (Synced (Set.toList revoked) (Set.toList added) (Set.toList new) (mTargetUserData m))
              | mRejoinRequested m -> do
                  writeTVar v (modifyGroup gid triggerRebalance s)
                  pure NeedRejoin
              | otherwise -> do
                  let live = filter (`Set.notMember` mPaused m) (Set.toList (mAssignment m))
                      go acc positions _ [] = (reverse acc, positions)
                      go acc positions !budget (tp : tps)
                        | budget <= 0 = (reverse acc, positions)
                        | otherwise =
                            let from = Map.findWithDefault 0 tp positions
                                (rs, next) = readFrom s iso tp from budget
                            in go (reverse rs <> acc) (Map.insert tp next positions) (budget - length rs) tps
                      (recs, positions') = go [] (mPositions m) (mdoMaxPollRecords opts) live
                  -- positions move past markers even when nothing is read
                  writeTVar v (s {stGroups = Map.insert gid g {gMembers = Map.insert mid m {mPositions = positions'} (gMembers g)} (stGroups s)})
                  pure (if null recs then NothingNew (stVersion s) else Fetched recs)

      poll :: Int -> IO (Either String [DriverRecord])
      poll ms = withMember $ \mid -> do
        deadline <- (+ fromIntegral ms) <$> nowMs
        let loop waited = do
              step <- atomically (pollStep mid)
              cbs <- readIORef cbRef
              case step of
                Fenced -> pure (Left "member is no longer in the group")
                NeedRejoin -> do
                  rcBeforeRejoin cbs
                  ud <- readIORef udRef >>= id
                  atomically $ modifyTVar' v $
                    modifyGroup gid (\g -> g {gMembers = Map.adjust (\m -> m {mRejoined = True, mUserData = ud}) mid (gMembers g)})
                  loop waited
                Lead epoch ca subs counts -> do
                  r <- try (caAssign ca counts subs) :: IO (Either SomeException (Either String (Map Text MemberAssignment)))
                  let retry e = do
                        -- everyone joins again, and a leader retries
                        atomically $ modifyTVar' v $ modifyGroup gid triggerRebalance
                        pure (Left ("assignor: " <> e))
                  case r of
                    Right (Right asg) -> do
                      atomically $ modifyTVar' v $ modifyGroup gid (installAssignment epoch asg)
                      loop waited
                    Right (Left e) -> retry e
                    Left e -> retry (show e)
                Synced revoked added whole ud -> do
                  unless (null revoked) (rcRevoked cbs revoked)
                  unless (null added) (rcAssigned cbs added)
                  rcAssignment cbs whole ud
                  loop waited
                Fetched rs -> pure (Right rs)
                Waiting ver -> wait ver waited
                NothingNew ver -> wait ver waited
            wait ver waited
              | waited = pure (Right [])
              | otherwise = do
                  now <- nowMs
                  let left = fromIntegral (deadline - now)
                  if left <= 0
                    then pure (Right [])
                    else waitForChange v ver left >> loop True
        loop False

      checkEpoch :: St -> Either String (Text, Producer)
      checkEpoch s = case mdoTransactionalId opts of
        Nothing -> Left "the driver has no transactional id"
        Just tid -> case Map.lookup tid (stProducers s) of
          Nothing -> Left "transactions not initialised"
          Just p -> Right (tid, p)

      endTxn :: TxnStatus -> STM (Either String ())
      endTxn status = do
        s <- readTVar v
        case checkEpoch s of
          Left e -> pure (Left e)
          Right (tid, p) -> case pOpen p of
            Nothing -> pure (Left "no open transaction")
            Just ot -> do
              let markers st0 =
                    foldr
                      ( \tp st -> st
                          { stTopics =
                              Map.adjust (Map.adjust (Seq.|> Marker) tp.partition) tp.topic (stTopics st)
                          }
                      )
                      st0
                      (Set.toList (otPartitions ot))
                  applyOffsets st0
                    | status == TxnCommitted =
                        Map.foldrWithKey
                          (\g offs st -> modifyGroup g (\grp -> grp {gOffsets = Map.union offs (gOffsets grp)}) st)
                          st0
                          (otOffsets ot)
                    | otherwise = st0
                  s' =
                    bump
                      ( applyOffsets
                          ( markers
                              s
                                { stTxnStatus = Map.insert (otRef ot) status (stTxnStatus s)
                                , stProducers = Map.insert tid p {pOpen = Nothing} (stProducers s)
                                }
                          )
                      )
              writeTVar v s'
              pure (Right ())

      fencedUnless :: Int -> St -> Either String (Text, Producer)
      fencedUnless myEpoch s = do
        (tid, p) <- checkEpoch s
        if pEpoch p /= myEpoch then Left "ProducerFenced: a newer producer has this transactional id" else Right (tid, p)

  pure
    StreamDriver
      { sdSubscribe = \topics ga cbs -> do
          writeIORef cbRef cbs
          writeIORef udRef (gaSubscriptionUserData ga)
          ud <- gaSubscriptionUserData ga
          known <- readIORef memberRef
          mid <- atomically $ do
            s <- readTVar v
            let !n = stNextMember s
                mid = mdoClientId opts <> "-" <> decimalText n
                member =
                  Member
                    { mTopics = topics
                    , mGeneration = -1
                    , mRejoined = True
                    , mTarget = Set.empty
                    , mAssignment = Set.empty
                    , mPositions = Map.empty
                    , mPaused = Set.empty
                    , mRejoinRequested = False
                    , mAssignor = gaAssignor ga
                    , mUserData = ud
                    , mTargetUserData = BS.empty
                    }
                s1 = s {stNextMember = n + 1}
            case known of
              -- subscribing again (a re-plan): the same member starts over
              Just old -> do
                writeTVar v $
                  modifyGroup
                    gid
                    (\g -> let g' = triggerRebalance g in g' {gMembers = Map.insert old member (gMembers g')})
                    s
                pure old
              Nothing -> do
                writeTVar v $
                  modifyGroup
                    gid
                    ( \g ->
                        let g' = triggerRebalance g in g' {gMembers = Map.insert mid member (gMembers g')}
                    )
                    s1
                pure mid
          writeIORef memberRef (Just mid)
          pure (Right ())
      , sdPoll = poll
      , sdCommitOffsets = \offs -> withMember $ \mid -> atomically $ do
          s <- readTVar v
          let g = Map.findWithDefault emptyGroup gid (stGroups s)
          if stFailOffsetCommits s > 0
            then do
              writeTVar v s {stFailOffsetCommits = stFailOffsetCommits s - 1}
              pure (Left "injected offset-commit failure")
            else case Map.lookup mid (gMembers g) of
              Just m
                | mGeneration m == gGeneration g -> do
                    writeTVar v (modifyGroup gid (\grp -> grp {gOffsets = Map.union (Map.fromList offs) (gOffsets grp)}) s)
                    pure (Right ())
              _ -> pure (Left "ILLEGAL_GENERATION: the group has rebalanced")
      , sdCommitted = \tps -> do
          offs <- maybe Map.empty gOffsets . Map.lookup gid . stGroups <$> readTVarIO v
          pure (Right (Map.restrictKeys offs (Set.fromList tps)))
      , sdSeek = \tp off -> withMember $ \mid -> do
          s <- readTVarIO v
          onMember mid (\m -> m {mPositions = Map.insert tp (resolve s tp off) (mPositions m)})
          pure (Right ())
      , sdPause = \tps -> () <$ withMember (\mid -> Right <$> onMember mid (\m -> m {mPaused = Set.union (Set.fromList tps) (mPaused m)}))
      , sdResume = \tps -> () <$ withMember (\mid -> Right <$> onMember mid (\m -> m {mPaused = Set.difference (mPaused m) (Set.fromList tps)}))
      , sdRequestRejoin = () <$ withMember (\mid -> Right <$> onMember mid (\m -> m {mRejoinRequested = True}))
      , sdCloseConsumer = \_leave _ -> do
          mmid <- readIORef memberRef
          forM_ mmid $ \mid -> atomically $ modifyTVar' v $ \s ->
            modifyGroup
              gid
              ( \g ->
                  let g' = g {gMembers = Map.delete mid (gMembers g)}
                  in if Map.null (gMembers g') then g' else triggerRebalance g'
              )
              s
          writeIORef memberRef Nothing
      , sdNewReader = do
          posRef <- newIORef (Map.empty :: Map KC.TopicPartition Int64)
          pure . Right $
            Reader
              { rdAssign = \tps -> do
                  s <- readTVarIO v
                  writeIORef posRef (Map.fromList (map (\(tp, off) -> (tp, resolve s tp off)) tps))
                  pure (Right ())
              , rdPoll = \ms -> do
                  let attempt = readTVarIO v
                      once s = do
                        positions <- readIORef posRef
                        let step (acc, ps) (tp, from) =
                              let (rs, next) = readFrom s iso tp from 1000
                              in (acc <> rs, Map.insert tp next ps)
                            (recs, positions') = foldl step ([], positions) (Map.toList positions)
                        writeIORef posRef positions'
                        pure recs
                  s0 <- attempt
                  rs <- once s0
                  if null rs && ms > 0
                    then do
                      waitForChange v (stVersion s0) ms
                      s1 <- attempt
                      Right <$> once s1
                    else pure (Right rs)
              , rdPosition = \tp -> do
                  positions <- readIORef posRef
                  pure (maybe (Left "not assigned") Right (Map.lookup tp positions))
              , rdEndOffsets = \tps -> do
                  s <- readTVarIO v
                  pure (Right (Map.fromList (map (\tp -> (tp, endFor iso s tp)) tps)))
              , rdClose = pure ()
              }
      , sdSend = \rec cb -> do
          now <- nowMs
          myEpoch <- readIORef epochRef
          r <- atomically $ do
            s <- readTVar v
            let ts = fromMaybe now rec.timestamp
                failing = Map.findWithDefault 0 rec.topic (stFailSends s)
            if failing > 0
              then do
                writeTVar v s {stFailSends = Map.insert rec.topic (failing - 1) (stFailSends s)}
                pure (Left "injected send failure")
              else do
                txn <- case mdoTransactionalId opts of
                  Nothing -> pure (Right Nothing)
                  Just _ -> pure $ do
                    (tid, p) <- fencedUnless myEpoch s
                    case pOpen p of
                      Nothing -> Left "send outside a transaction"
                      Just ot -> Right (Just (tid, p, ot))
                case txn of
                  Left e -> pure (Left e)
                  Right mt -> case appendSt rec.topic rec.partition (StoredRecord rec.key rec.value ts rec.headers ((\(_, _, ot) -> otRef ot) <$> mt)) s of
                    Left e -> pure (Left e)
                    Right (s', p, off) -> do
                      writeTVar v $ case mt of
                        Nothing -> s'
                        Just (tid, pr, ot) ->
                          s'
                            { stProducers =
                                Map.insert
                                  tid
                                  pr {pOpen = Just ot {otPartitions = Set.insert (KC.TopicPartition rec.topic p) (otPartitions ot)}}
                                  (stProducers s')
                            }
                      pure (Right (KP.RecordMetadata {topic = rec.topic, partition = p, offset = off, timestamp = ts}))
          cb r
          pure (Right ())
      , sdFlush = pure (Right ())
      , sdPartitionsFor = \t -> maybe (Left ("unknown topic " <> T.unpack t)) Right <$> mockPartitionCount (MockBroker v) t
      , sdCloseProducer = pure ()
      , sdInitTransactions = case mdoTransactionalId opts of
          Nothing -> pure (Left "the driver has no transactional id")
          Just tid -> do
            ep <- atomically $ do
              s <- readTVar v
              let prev = Map.lookup tid (stProducers s)
                  !ep = maybe 0 ((+ 1) . pEpoch) prev
                  -- an open transaction of the fenced producer is aborted
                  s1 = case prev >>= pOpen of
                    Nothing -> s
                    Just ot ->
                      foldr
                        (\tp st -> st {stTopics = Map.adjust (Map.adjust (Seq.|> Marker) tp.partition) tp.topic (stTopics st)})
                        s {stTxnStatus = Map.insert (otRef ot) TxnAborted (stTxnStatus s)}
                        (Set.toList (otPartitions ot))
              writeTVar v (bump s1 {stProducers = Map.insert tid (Producer ep Nothing) (stProducers s1)})
              pure ep
            writeIORef epochRef ep
            pure (Right ())
      , sdBeginTransaction = do
          myEpoch <- readIORef epochRef
          atomically $ do
            s <- readTVar v
            case fencedUnless myEpoch s of
              Left e -> pure (Left e)
              Right (tid, p) -> case pOpen p of
                Just _ -> pure (Left "a transaction is already open")
                Nothing -> do
                  let !n = stNextTxnInstance s
                      ref = TxnRef tid n
                  writeTVar v $
                    bump
                      s
                        { stNextTxnInstance = n + 1
                        , stTxnStatus = Map.insert ref TxnOpen (stTxnStatus s)
                        , stProducers = Map.insert tid p {pOpen = Just (OpenTxn ref Set.empty Map.empty)} (stProducers s)
                        }
                  pure (Right ())
      , sdSendOffsetsToTransaction = \offs -> withMember $ \mid -> do
          myEpoch <- readIORef epochRef
          atomically $ do
            s <- readTVar v
            let g = Map.findWithDefault emptyGroup gid (stGroups s)
                current = maybe False ((== gGeneration g) . mGeneration) (Map.lookup mid (gMembers g))
            case fencedUnless myEpoch s of
              Left e -> pure (Left e)
              Right (tid, p) -> case pOpen p of
                Nothing -> pure (Left "no open transaction")
                Just ot
                  | not current -> pure (Left "ILLEGAL_GENERATION: fenced by a group rebalance")
                  | otherwise -> do
                      let ot' = ot {otOffsets = Map.insertWith Map.union gid (Map.fromList offs) (otOffsets ot)}
                      writeTVar v s {stProducers = Map.insert tid p {pOpen = Just ot'} (stProducers s)}
                      pure (Right ())
      , sdCommitTransaction = do
          myEpoch <- readIORef epochRef
          atomically $ do
            s <- readTVar v
            case fencedUnless myEpoch s of
              Left e -> pure (Left e)
              Right _
                | stFailTxnCommits s > 0 -> do
                    writeTVar v s {stFailTxnCommits = stFailTxnCommits s - 1}
                    pure (Left "injected transaction-commit failure")
                | otherwise -> endTxn TxnCommitted
      , sdAbortTransaction = do
          myEpoch <- readIORef epochRef
          atomically $ do
            s <- readTVar v
            case fencedUnless myEpoch s of
              Left e -> pure (Left e)
              Right _ -> endTxn TxnAborted
      , sdCreateTopics = \ts -> do
          atomically $ modifyTVar' v $ \s -> foldr (\t -> createTopicSt (nitName t) (nitPartitions t)) s ts
          pure (Right ())
      , sdDescribeTopics = \ts -> do
          s <- readTVarIO v
          pure (Right (Map.map Map.size (Map.restrictKeys (stTopics s) (Set.fromList ts))))
      , sdListTopics = Right . Map.keys . stTopics <$> readTVarIO v
      , sdPartitionRacks = \tps -> do
          s <- readTVarIO v
          pure (Right (Map.fromList (map (\tp -> (tp, Map.findWithDefault Set.empty tp (stRacks s))) tps)))
      , sdDeleteRecords = \offs -> do
          atomically $ modifyTVar' v $ \s ->
            bump s {stLogStart = Map.unionWith max (Map.fromList offs) (stLogStart s)}
          pure (Right ())
      , sdClose = pure ()
      }


{- | A driver for a streams instance configured by @cfg@: group
@application.id@, read_committed and a fresh transactional id under
exactly-once (as "Kafka.Streams.Runtime.NativeDriver" would).
-}
mockDriverFor :: MockBroker -> StreamsConfig -> IO StreamDriver
mockDriverFor b@(MockBroker v) cfg = do
  n <- atomically $ do
    s <- readTVar v
    writeTVar v s {stNextMember = stNextMember s + 1}
    pure (stNextMember s)
  let eos = processingGuarantee cfg == ExactlyOnceV2
  newMockDriver
    b
    defaultMockDriverOptions
      { mdoGroupId = applicationId cfg
      , mdoClientId = clientId cfg
      , mdoIsolation = if eos then KC.ReadCommitted else KC.ReadUncommitted
      , mdoTransactionalId = if eos then Just (applicationId cfg <> "-" <> decimalText n) else Nothing
      }
