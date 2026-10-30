{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Kafka.Client.Internal.Subscribe
Description : End-to-end consumer-group subscribe flow

Wires together the protocol primitives in
"Kafka.Client.Internal.ConsumerGroup" plus offset / metadata
machinery to implement the full @subscribe@ lifecycle for a
consumer group:

  1. FindCoordinator — pick the broker that manages this group.
  2. JoinGroup       — register as a member; one member is elected leader.
  3. Assignment      — leader runs the assignment strategy across the
                       members' subscriptions and broker metadata. Right
                       now we ship the @range@ assignor only (the
                       canonical default in the JVM client).
  4. SyncGroup       — broadcast assignments (leader) or receive our
                       own (follower).
  5. OffsetFetch     — for each assigned partition, pick the resume
                       point: committed offset if one exists, otherwise
                       fall back to the consumer's @auto.offset.reset@
                       policy (earliest \/ latest).

The state mutations land in the @Consumer@'s 'StmMap.Map' assignment
table, so 'Kafka.Client.Consumer.poll' picks them up immediately.
-}
module Kafka.Client.Internal.Subscribe (
  SubscribeError (..),
  ResetPolicy (..),
  TopicPartition (..),

  -- * Assignors
  Assignor (..),
  assignorName,
  runAssignor,
  rangeAssign,
  roundRobinAssign,
  stickyAssign,
  subscribeFlow,

  -- * Subscription metadata codec (exposed for testing)
  encodeSubscription,
  encodeSubscriptionWithOwned,
  decodeSubscription,
  decodeSubscriptionFull,
  decodeAssignment,
  decodeAssignmentFull,
) where

import Control.Concurrent.STM
import Control.Monad (forM)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.IORef (IORef, atomicModifyIORef', readIORef, writeIORef)
import Data.Int (Int16, Int32, Int64)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Word (Word8)
import Kafka.Client.Assignor (CustomAssignor (..), MemberAssignment (..), MemberSubscription (..))
import Kafka.Client.Internal.ConsumerGroup qualified as CG
import Kafka.Client.Internal.Heartbeat qualified as HB
import Kafka.Client.Internal.Request qualified as Req
import Kafka.Client.Metadata qualified as Meta
import Kafka.Network.Connection (BrokerAddress (..))
import Kafka.Network.Connection qualified as Conn
import Kafka.Protocol.ApiVersions qualified as AV
import Kafka.Protocol.VersionNegotiation qualified as VN
import "wireform-kafka-protocol" Kafka.Protocol.Generated.ConsumerProtocolAssignment qualified as CPA
import "wireform-kafka-protocol" Kafka.Protocol.Generated.ConsumerProtocolSubscription qualified as CPS
import "wireform-kafka-protocol" Kafka.Protocol.Generated.ListOffsetsRequest qualified as LOReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.ListOffsetsResponse qualified as LOResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.OffsetFetchRequest qualified as OFReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.OffsetFetchResponse qualified as OFResp
import "wireform-kafka-protocol" Kafka.Protocol.Primitives qualified as P
import "wireform-kafka-protocol" Kafka.Protocol.Wire.Codec qualified as WC


{- | Discriminated error type for the subscribe flow. Anything that
isn't a structured failure (network / decode / broker-error code)
bubbles up as 'SubscribeOther'.
-}
data SubscribeError
  = SubscribeNoBootstrap
  | SubscribeCoordinator !String
  | SubscribeJoin !String
  | SubscribeSync !String
  | SubscribeMetadata !String
  | SubscribeOffsetFetch !String
  | SubscribeOther !String
  deriving (Eq, Show)


{- | Which partition-assignment strategy a consumer should advertise to
the group coordinator. The assignor that wins the protocol
negotiation is the one every member of the group has in common; the
broker picks one and the elected /leader/ then runs that assignor
locally to compute the per-member assignments.

We support all three of the JVM client's defaults:

  * 'AssignorRange'      - same as the JVM "range" default. Sort
                           consumers + partitions, give each consumer
                           a contiguous slice of partitions per topic.
                           Good when the consumer count divides the
                           partition count evenly.
  * 'AssignorRoundRobin' - JVM "roundrobin". Sort all
                           (topic, partition) pairs and consumers,
                           then assign one partition per consumer in
                           round-robin order until the supply is
                           exhausted. Better balance than range when
                           consumers subscribe to different topic
                           sets.
  * 'AssignorCustom'     - an application's own assignor (its name,
                           and a function from every member's
                           subscription to its partitions and an
                           assignment user-data blob).
  * 'AssignorSticky'     - JVM "sticky". Tries to (a) balance the
                           count per consumer and (b) preserve the
                           previous generation's assignment so few
                           partitions move. Without previous-gen
                           state our impl degrades to a balanced
                           round-robin; with previous state passed
                           in, it preserves it greedily.
-}
data Assignor
  = AssignorRange
  | AssignorRoundRobin
  | AssignorSticky
  | -- | An application assignor ("Kafka.Client.Assignor")
    AssignorCustom !CustomAssignor
  deriving (Eq, Show)


{- | Wire-protocol name the broker negotiates on
('JoinGroupRequest.protocols.name'). Matches the JVM client.
-}
assignorName :: Assignor -> Text
assignorName AssignorRange = "range"
assignorName AssignorRoundRobin = "roundrobin"
assignorName AssignorSticky = "cooperative-sticky"
assignorName (AssignorCustom ca) = caName ca


{- | Topic + partition pair (mirror of the one in 'Kafka.Client.Consumer'
— we keep a local copy here to avoid the import cycle).
-}
data TopicPartition = TopicPartition
  { tpTopic :: !Text
  , tpPartition :: !Int32
  }
  deriving (Eq, Ord, Show)


{- | Run the full subscribe lifecycle.

The resulting list is the (topic-partition, fetch-position) pairs
assigned to this consumer; the caller is expected to insert them
into the consumer's assignment 'StmMap.Map' (or use it as the
starting point of a new assignment).
-}
subscribeFlow
  :: Conn.ConnectionManager
  -> Conn.ConnectionConfig
  -- ^ Used for any new broker connections (TLS / SASL)
  -> Meta.MetadataCache
  -> AV.ApiVersionCache
  -> HB.HeartbeatState
  {- ^ Updated with the discovered coordinator + assigned member id
  and generation id, so the heartbeat thread can take over.
  -}
  -> Text
  -- ^ client id
  -> Text
  -- ^ group id
  -> [Text]
  -- ^ topics to subscribe to
  -> Int32
  -- ^ session timeout (ms)
  -> Int32
  -- ^ rebalance / max-poll-interval (ms)
  -> ResetPolicy
  -- ^ what to do when no committed offset exists
  -> Assignor
  -- ^ partition assignor to advertise + run if elected leader
  -> IORef Int32
  -- ^ correlation id source
  -> IO BS.ByteString
  {- ^ subscription-userdata
  callback. Returns bytes to
  attach to the JoinGroup
  subscription-userdata blob
  (e.g. the encoded
  'SubscriptionInfo' for cross-
  instance IQ discovery).
  'pure BS.empty' disables.
  -}
  -> IO (Either SubscribeError ([(TopicPartition, Int64)], BS.ByteString))
  {- ^ Assigned partitions with their fetch positions, and the
  assignment user data the leader's assignor attached for this member
  -}
subscribeFlow
  connMgr
  connConfig
  metaCache
  versionCache
  hbState
  clientId
  groupId
  topics
  sessionTimeoutMs
  rebalanceTimeoutMs
  resetPolicy
  assignor
  corrIdVar
  fetchUserData = do
    -- 1. Make sure we have metadata for the topics we're about to subscribe
    --    to; we need the partition list locally so we (a) include accurate
    --    "what I want" subscriptions in JoinGroup metadata and (b) know how
    --    to compute the leader-side assignment if we get elected.
    ensureMetadata >>= \case
      Left err -> pure (Left (SubscribeMetadata err))
      Right () -> do
        -- 2. FindCoordinator using any known broker.
        withBootstrapBroker $ \bootAddr bootConn -> do
          cid <- nextCorrId
          coordResult <-
            CG.findGroupCoordinator
              versionCache
              connMgr
              bootAddr
              bootConn
              groupId
              cid
              clientId
          case coordResult of
            Left err -> pure (Left (SubscribeCoordinator err))
            Right coord -> joinAndSync coord
    where
      nextCorrId = atomicModifyIORef' corrIdVar (\v -> (v + 1, v))

      -- A connection whose ApiVersions are in the cache, so the
      -- requests sent on it pick a version the broker speaks
      -- (the coordinator is often a broker nobody has talked to
      -- yet). A failed handshake leaves the compiled-in defaults.
      connectNegotiated addr = do
        r <- Conn.getOrCreateConnection connMgr addr connConfig
        case r of
          Right conn -> VN.ensureVersionsNegotiated conn addr versionCache nextCorrId >> pure r
          Left _ -> pure r

      ensureMetadata = do
        brokersM <- atomically $ Meta.getAllBrokers metaCache
        case brokersM of
          Just (b : _) -> do
            let addr = Meta.brokerMetaAddress b
            connRes <- connectNegotiated addr
            case connRes of
              Left err -> pure (Left ("connect: " <> err))
              Right conn -> do
                cid <- nextCorrId
                Conn.withBrokerLock connMgr addr $
                  Meta.refreshTopicMetadata conn metaCache (Just topics) cid
          _ -> pure (Left "no brokers in metadata cache (call createConsumer first)")

      withBootstrapBroker k = do
        brokersM <- atomically $ Meta.getAllBrokers metaCache
        case brokersM of
          Just (b : _) -> do
            let addr = Meta.brokerMetaAddress b
            connRes <- connectNegotiated addr
            case connRes of
              Left err -> pure (Left (SubscribeOther ("bootstrap connect: " <> err)))
              Right conn -> k addr conn
          _ -> pure (Left SubscribeNoBootstrap)

      joinAndSync coord = do
        let coordAddr =
              BrokerAddress
                (T.unpack (CG.coordHost coord))
                (fromIntegral (CG.coordPort coord))
        coordConnR <- connectNegotiated coordAddr
        case coordConnR of
          Left err -> pure (Left (SubscribeCoordinator err))
          Right coordConn -> do
            writeIORef (HB.hbCoordinatorAddr hbState) (Just coordAddr)

            -- 3. JoinGroup. Advertise the chosen assignor by name; the
            --    coordinator picks one assignor that every member of the
            --    group has in common.
            --
            -- KIP-535: stamp the user's subscription-userdata blob
            -- (typically the streams 'SubscriptionInfo' carrying
            -- host:port + owned stores + owned partitions) so the
            -- assignor can use it for cross-instance IQ routing.
            userData <- fetchUserData
            let subMeta = encodeSubscriptionWithOwned topics userData []
                protocols = [(assignorName assignor, subMeta)]
            cid1 <- nextCorrId
            existingMember <- readIORef (HB.hbMemberId hbState)
            joinR <-
              CG.joinGroup
                versionCache
                connMgr
                coordAddr
                coordConn
                groupId
                existingMember
                clientId
                sessionTimeoutMs
                rebalanceTimeoutMs
                "consumer"
                protocols
                cid1
            case joinR of
              Left err -> pure (Left (SubscribeJoin err))
              Right join -> do
                writeIORef (HB.hbMemberId hbState) (CG.jgrMemberId join)
                writeIORef (HB.hbGenerationId hbState) (CG.jgrGenerationId join)

                -- 4. If we are the leader, decode every member's
                --    subscription, run the range assignor against the
                --    metadata cache, and prepare the per-member
                --    assignments. Followers send empty assignments.
                assignments <-
                  if CG.jgrMemberId join == CG.jgrLeaderId join
                    then buildLeaderAssignments (CG.jgrMembers join)
                    else pure (Right [])

                case assignments of
                  Left err -> pure (Left (SubscribeOther ("assignor: " <> err)))
                  Right asg -> syncWith coordConn coordAddr join asg

      syncWith coordConn coordAddr join assignments = do
                cid2 <- nextCorrId
                -- 'protocolType' is fixed by the caller ("consumer"); the
                -- 'protocolName' is whatever the broker picked from our
                -- 'protocols' list (KIP-559 SyncGroup v5 demands both).
                syncR <-
                  CG.syncGroup
                    versionCache
                    connMgr
                    coordAddr
                    coordConn
                    groupId
                    (CG.jgrGenerationId join)
                    (CG.jgrMemberId join)
                    clientId
                    "consumer"
                    (CG.jgrProtocolName join)
                    assignments
                    cid2
                case syncR of
                  Left err -> pure (Left (SubscribeSync err))
                  Right myAssignmentBytes -> do
                    case decodeAssignmentFull myAssignmentBytes of
                      Left err -> pure (Left (SubscribeSync ("decode assignment: " <> err)))
                      Right (myParts, ud) -> fmap (\tps -> (tps, ud)) <$> resolveOffsets coordConn coordAddr myParts

      -- Compute per-member assignment when we're the leader. We walk the
      -- raw subscription bytes published by every joined member, take the
      -- union of subscribed topics, look the partitions up in the
      -- metadata cache, and fan them out via 'runAssignor'.
      buildLeaderAssignments members = do
        let memberSubs = map (\m -> (CG.gmiMemberId m, decodeSubscriptionFull (CG.gmiMetadata m))) members
            subscribedTopics =
              Map.keys $
                Map.fromList
                  [(t, ()) | (_, Right (ts, _, _)) <- memberSubs, t <- ts]

        -- Look up partition counts for every subscribed topic. (We
        -- already refreshed metadata for our own topics; the leader may
        -- be subscribing on behalf of others, so refresh once more for
        -- the combined set so range assignment sees the right count.)
        topicParts <- forM subscribedTopics $ \t -> do
          psM <- atomically $ Meta.getTopicPartitions metaCache t
          let pids = case psM of
                Nothing -> []
                Just ps -> sortOn id (map Meta.partitionMetaId ps)
          pure (t, pids)

        -- Sticky / cooperative-sticky needs each member's previous
        -- assignment to keep partition ownership stable across
        -- rebalances (KIP-341 / KIP-429). We pull that out of the
        -- 'ownedPartitions' field every member published in its
        -- JoinGroup subscription metadata. For other assignors the
        -- list is ignored (see 'runAssignor').
        let prevGen :: [(Text, [(Text, [Int32])])]
            prevGen =
              [ (mid, owned)
              | (mid, Right (_topics, _user, owned)) <- memberSubs
              , not (null owned)
              ]
            mPrev = if null prevGen then Nothing else Just prevGen

        ePerMember <- case assignor of
          AssignorCustom ca ->
            fmap (map (\(mid, ma) -> (mid, (maPartitions ma, maUserData ma))) . Map.toList)
              <$> caAssign
                ca
                (Map.fromList (map (fmap (fromIntegral . length)) topicParts))
                ( map
                    ( \(mid, d) -> case d of
                        Right (ts, ud, owned) -> MemberSubscription mid ts ud owned
                        Left _ -> MemberSubscription mid [] BS.empty []
                    )
                    memberSubs
                )
          _ ->
            pure . Right . map (fmap (\byTopic -> (byTopic, BS.empty))) $
              runAssignor
                assignor
                [ (mid, topicsOf m)
                | (mid, m) <- memberSubs
                , let topicsOf (Right (ts, _, _)) = ts
                      topicsOf (Left _) = []
                ]
                (Map.fromList topicParts)
                mPrev

        -- Assignment payload mirrors the subscription on the wire:
        -- a two-byte big-endian version header followed by the
        -- ConsumerProtocolAssignment body. Followers / future
        -- generations decode the version off the header, so this
        -- has to be present even when there's only one member in
        -- the group.
        let assignmentVersion = 0 :: Int16
            assignmentBytes (byTopic, userData) =
              let !msg =
                    CPA.ConsumerProtocolAssignment
                      { CPA.consumerProtocolAssignmentAssignedPartitions =
                          P.mkKafkaArray $
                            V.fromList
                              [ CPA.TopicPartition
                                  { CPA.topicPartitionTopic = P.mkKafkaString t
                                  , CPA.topicPartitionPartitions =
                                      P.mkKafkaArray (V.fromList ps)
                                  }
                              | (t, ps) <- byTopic
                              ]
                      , CPA.consumerProtocolAssignmentUserData = P.mkKafkaBytes userData
                      }
                  !body = WC.runEncodeVer @CPA.ConsumerProtocolAssignment assignmentVersion msg
                  !vHi = fromIntegral (assignmentVersion `shiftR` 8) :: Word8
                  !vLo = fromIntegral (assignmentVersion .&. 0xff) :: Word8
              in BS.cons vHi (BS.cons vLo body)
        pure (map (fmap assignmentBytes) <$> ePerMember)

      -- After SyncGroup we know the partitions assigned to *us*. For
      -- each one, look up its committed offset; if missing, resolve the
      -- earliest / latest reset policy immediately with ListOffsets so
      -- the first poll starts at the broker's exact position.
      resolveOffsets coordConn coordAddr parts = do
        let tps = [TopicPartition t p | (t, ps) <- parts, p <- ps]
        case tps of
          [] -> pure (Right [])
          _ -> do
            cid <- nextCorrId
            fetchR <-
              offsetFetchAll
                versionCache
                connMgr
                coordAddr
                coordConn
                clientId
                groupId
                tps
                cid
            case fetchR of
              Left err -> pure (Left (SubscribeOffsetFetch err))
              Right committed -> do
                -- Partitions with a stored commit reuse it.  The
                -- rest fall back to the @auto.offset.reset@ policy:
                -- ResetEarliest/ResetLatest issue a ListOffsets
                -- query (so the consumer starts from the actual
                -- log-start / log-end offset, not the broker-
                -- rejected sentinel '0'), ResetNone defaults to 0
                -- (callers that care will get OffsetOutOfRange on
                -- first fetch and decide what to do).
                let needsReset = [tp | tp <- tps, not (Map.member tp committed)]
                resetMap <- case (resetPolicy, needsReset) of
                  (_, []) -> pure (Right Map.empty)
                  (ResetNone, _) -> pure (Right Map.empty)
                  (ResetEarliest, _) ->
                    resolveByListOffsets needsReset (-2 :: Int64)
                  (ResetLatest, _) ->
                    resolveByListOffsets needsReset (-1 :: Int64)
                case resetMap of
                  Left e -> pure (Left (SubscribeOther e))
                  Right rm -> do
                    let resolved =
                          [ ( tp
                            , fromMaybe
                                (Map.findWithDefault 0 tp rm)
                                (Map.lookup tp committed)
                            )
                          | tp <- tps
                          ]
                    pure (Right resolved)

      -- \| Issue a single ListOffsets request to any known broker
      -- (the broker forwards to the per-partition leader internally)
      -- to resolve a 'auto.offset.reset' fall-back to a real offset.
      resolveByListOffsets
        :: [TopicPartition]
        -> Int64
        -- \^ -2 for earliest, -1 for latest
        -> IO (Either String (Map.Map TopicPartition Int64))
      resolveByListOffsets tps timestamp = do
        brokersM <- atomically $ Meta.getAllBrokers metaCache
        case brokersM of
          Just (b : _) -> do
            let addr = Meta.brokerMetaAddress b
            connRes <- connectNegotiated addr
            case connRes of
              Left err -> pure (Left ("ListOffsets connect: " <> err))
              Right c -> doListOffsets addr c tps timestamp
          _ -> pure (Left "no brokers in metadata cache")

      doListOffsets brokerAddr conn tps timestamp = do
        cid <- nextCorrId
        let apiKey = 2 -- ListOffsets
        -- Cap at v8 like the Consumer.queryPartitionOffsets path,
        -- for the same reason: v9+ requires KIP-405 tiered storage
        -- which 3.7's default builds don't enable.
        verR <-
          VN.pickApiVersionForRange @LOReq.ListOffsetsRequest
            0
            8
            versionCache
            brokerAddr
            1
        let apiVersion = case verR of
              Right v -> v
              Left _ -> 1
            byTopic =
              Map.fromListWith
                (++)
                [(tpTopic tp, [tpPartition tp]) | tp <- tps]
            topics =
              V.fromList
                [ LOReq.ListOffsetsTopic
                    { LOReq.listOffsetsTopicName = P.mkKafkaString topic
                    , LOReq.listOffsetsTopicPartitions =
                        P.mkKafkaArray $
                          V.fromList
                            [ LOReq.ListOffsetsPartition
                                { LOReq.listOffsetsPartitionPartitionIndex = pid
                                , LOReq.listOffsetsPartitionCurrentLeaderEpoch = -1
                                , LOReq.listOffsetsPartitionTimestamp = timestamp
                                }
                            | pid <- pids
                            ]
                    }
                | (topic, pids) <- Map.toList byTopic
                ]
            request =
              LOReq.ListOffsetsRequest
                { LOReq.listOffsetsRequestReplicaId = -1
                , LOReq.listOffsetsRequestIsolationLevel = 0
                , LOReq.listOffsetsRequestTopics = P.mkKafkaArray topics
                , LOReq.listOffsetsRequestTimeoutMs = 30000
                }
            requestBody = WC.runEncodeVer @LOReq.ListOffsetsRequest apiVersion request
            clientIdK = P.mkKafkaString clientId
        r <-
          Req.sendRequestReceiveResponseLocked
            (Conn.withBrokerLock connMgr brokerAddr)
            conn
            apiKey
            apiVersion
            cid
            clientIdK
            requestBody
        case r of
          Left err -> pure (Left ("ListOffsets: " <> err))
          Right (_, body) ->
            case WC.runDecodeVer @LOResp.ListOffsetsResponse apiVersion body of
              Left err -> pure (Left ("decode ListOffsets: " <> err))
              Right resp ->
                let !pairs = case P.unKafkaArray (LOResp.listOffsetsResponseTopics resp) of
                      P.Null -> []
                      P.NotNull tvec ->
                        concatMap
                          ( \tr ->
                              let topic = case P.unKafkaString
                                    (LOResp.listOffsetsTopicResponseName tr) of
                                    P.Null -> T.empty
                                    P.NotNull t -> t
                                  partsVec = case P.unKafkaArray
                                    (LOResp.listOffsetsTopicResponsePartitions tr) of
                                    P.Null -> V.empty
                                    P.NotNull v -> v
                              in [ (TopicPartition topic pid, off)
                                 | pr <- V.toList partsVec
                                 , let pid = LOResp.listOffsetsPartitionResponsePartitionIndex pr
                                       off = LOResp.listOffsetsPartitionResponseOffset pr
                                       ec = LOResp.listOffsetsPartitionResponseErrorCode pr
                                 , ec == 0
                                 , off >= 0
                                 ]
                          )
                          (V.toList tvec)
                in pure (Right (Map.fromList pairs))


{- | Dispatch on 'Assignor'. The third argument is the previous
generation's assignment (if any), which only the sticky assignor
consults — the others ignore it.
-}
runAssignor
  :: Assignor
  -> [(Text, [Text])]
  -- ^ subscriptions: @(memberId, [topic])@
  -> Map.Map Text [Int32]
  -- ^ partition lists per topic
  -> Maybe [(Text, [(Text, [Int32])])]
  -- ^ previous-generation assignment
  -> [(Text, [(Text, [Int32])])]
runAssignor AssignorRange members topicParts _ = rangeAssign members topicParts
runAssignor AssignorRoundRobin members topicParts _ = roundRobinAssign members topicParts
runAssignor AssignorSticky members topicParts prev = stickyAssign members topicParts prev
-- run by the leader before it gets here ('subscribeFlow')
runAssignor (AssignorCustom _) members topicParts _ = rangeAssign members topicParts


{- | Naive range assignment, in the spirit of the JVM client's
@RangeAssignor@. For each topic, sort consumers and partitions
lexicographically, then chunk partitions into roughly-equal slices.

Inputs:

  * member subscriptions: @(memberId, [topic])@
  * topic partition lists: @topic -> [partitionId]@

Output: per-member topic→partitions mapping that the SyncGroup
payload can be built from.
-}
rangeAssign
  :: [(Text, [Text])]
  -> Map.Map Text [Int32]
  -> [(Text, [(Text, [Int32])])]
rangeAssign members topicParts =
  let memberOrder = sortOn fst members
      assignmentsByMember =
        foldr
          accumulate
          (Map.fromList [(mid, []) | (mid, _) <- memberOrder])
          (Map.toList topicParts)

      accumulate (topic, parts) acc =
        let interested = [mid | (mid, ts) <- memberOrder, topic `elem` ts]
            n = length interested
        in if n == 0
             then acc
             else
               let chunks = chunkRange parts n
                   pairs =
                     [ (mid, ps)
                     | (mid, ps) <- zip interested chunks
                     , not (null ps)
                     ]
               in foldr
                    ( \(mid, ps) m ->
                        Map.insertWith (++) mid [(topic, ps)] m
                    )
                    acc
                    pairs
  in [ (mid, byTopic)
     | (mid, _) <- memberOrder
     , let byTopic = fromMaybe [] (Map.lookup mid assignmentsByMember)
     ]


{- | Round-robin assignment in the spirit of the JVM client's
@RoundRobinAssignor@:

  1. Sort members lexicographically.
  2. Sort topics lexicographically; within a topic sort partitions
     ascending. Concatenate into one global list of
     @(topic, partition)@ pairs.
  3. Walk the global list, handing each partition to the next
     eligible consumer (one that is subscribed to that topic) in
     round-robin order.

Better than 'rangeAssign' when consumers subscribe to different
topic sets: it keeps total partitions-per-consumer within ±1 across
the whole subscription, not just within each individual topic.
-}
roundRobinAssign
  :: [(Text, [Text])]
  -> Map.Map Text [Int32]
  -> [(Text, [(Text, [Int32])])]
roundRobinAssign members topicParts =
  let memberOrder = sortOn fst members
      memberIds = map fst memberOrder
      memberSubs = Map.fromList memberOrder
      sortedTopics = sortOn fst (Map.toList topicParts)
      allPairs =
        [(t, p) | (t, ps) <- sortedTopics, p <- sortOn id ps]

      -- Cycle through consumers; skip those not subscribed to the
      -- current topic. We use a pure index counter so the rotation
      -- order is deterministic.
      assignLoop
        :: Int
        -- \^ rotation cursor
        -> [(Text, Int32)]
        -- \^ remaining (topic, partition) pairs
        -> Map.Map Text [(Text, Int32)]
        -- \^ accumulated assignments per member
        -> Map.Map Text [(Text, Int32)]
      assignLoop _ [] acc = acc
      assignLoop cur ((t, p) : rest) acc =
        case findEligible cur t of
          Nothing -> assignLoop cur rest acc -- no consumer wants this topic
          Just (i, mid) ->
            let acc' = Map.insertWith (++) mid [(t, p)] acc
            in assignLoop (i + 1) rest acc'

      n = length memberIds

      findEligible :: Int -> Text -> Maybe (Int, Text)
      findEligible start t =
        let go k
              | k == n = Nothing
              | otherwise =
                  let i = (start + k) `mod` n
                      mid = memberIds !! i
                  in case Map.lookup mid memberSubs of
                       Just ts | t `elem` ts -> Just (i, mid)
                       _ -> go (k + 1)
        in go 0

      finalAcc = assignLoop 0 allPairs Map.empty
  in [ (mid, groupByTopicSorted (fromMaybe [] (Map.lookup mid finalAcc)))
     | mid <- memberIds
     ]


{- | Sticky assignment in the spirit of the JVM client's
@StickyAssignor@:

  * If a previous generation's assignment is supplied, every
    partition that the same consumer already owned (and is still
    subscribed to that topic) stays put.
  * Any unassigned partitions are then handed out in round-robin
    order to whichever subscribed consumer currently owns the fewest
    partitions, preserving balance within ±1 across the group.

Without a previous-generation assignment ('Nothing') sticky behaves
like a balanced round-robin — which is the JVM client's behaviour on
the first generation too.
-}
stickyAssign
  :: [(Text, [Text])]
  -> Map.Map Text [Int32]
  -> Maybe [(Text, [(Text, [Int32])])]
  -> [(Text, [(Text, [Int32])])]
stickyAssign members topicParts mPrev =
  case mPrev of
    Nothing -> roundRobinAssign members topicParts
    Just prev ->
      let memberOrder = sortOn fst members
          memberIds = map fst memberOrder
          memberSubs = Map.fromList memberOrder

          -- All partitions that *should* exist this generation.
          allPairs :: Map.Map (Text, Int32) ()
          allPairs =
            Map.fromList
              [((t, p), ()) | (t, ps) <- Map.toList topicParts, p <- ps]

          -- Step 1: keep what the previous generation owned, modulo
          -- partitions that no longer exist or that the consumer no
          -- longer subscribes to.
          startAcc :: Map.Map Text [(Text, Int32)]
          startAcc =
            Map.fromList
              [ (mid, kept)
              | mid <- memberIds
              , let subs = fromMaybe [] (Map.lookup mid memberSubs)
                    prevByMember = fromMaybe [] (lookup mid prev)
                    kept =
                      [ (t, p)
                      | (t, ps) <- prevByMember
                      , t `elem` subs
                      , p <- ps
                      , Map.member (t, p) allPairs
                      ]
              ]

          -- Step 2: partitions that aren't yet assigned (because the
          -- member that owned them dropped them, or the partition is
          -- new this generation).
          owned :: Map.Map (Text, Int32) ()
          owned =
            foldr
              ( \(_, xs) m ->
                  foldr (\tp m' -> Map.insert tp () m') m xs
              )
              Map.empty
              (Map.toList startAcc)

          stillUnassigned :: [(Text, Int32)]
          stillUnassigned =
            [tp | tp <- Map.keys allPairs, not (Map.member tp owned)]

          -- Step 3: hand the unassigned out to the least-loaded
          -- eligible consumer (deterministic tie-break on memberId).
          afterHandout = handout startAcc stillUnassigned

          -- Step 4: rebalance. If any consumer is over the natural
          -- ceiling (ceil(total / n)), move its excess partitions to
          -- consumers below the floor (floor(total / n)) that are
          -- subscribed to the relevant topic. This is what makes
          -- "previous gen had c1 with everything, c2 just joined"
          -- end up with c1 and c2 sharing the load — the JVM sticky
          -- behaviour.
          totalParts = Map.size allPairs
          n = max 1 (length memberIds)
          ceilLoad = (totalParts + n - 1) `quot` n
          floorLoad = totalParts `quot` n

          rebalance acc = go acc
            where
              go a =
                let loads =
                      [ (mid, length (fromMaybe [] (Map.lookup mid a)))
                      | mid <- memberIds
                      ]
                    overs = [mid | (mid, l) <- loads, l > ceilLoad]
                    unders = [mid | (mid, l) <- loads, l < floorLoad]
                in if null overs || null unders
                     then a
                     else
                       let !donor = head overs
                           !donorPs = fromMaybe [] (Map.lookup donor a)
                           tryMove [] keep moved =
                             -- Couldn't find a movable partition for
                             -- any 'unders' consumer; bail to avoid
                             -- looping forever.
                             Map.insert donor (reverse keep ++ moved) a
                           tryMove (tp@(t, _) : rest) keep moved =
                             let willing =
                                   [ mid
                                   | mid <- unders
                                   , Just subs <- [Map.lookup mid memberSubs]
                                   , t `elem` subs
                                   ]
                             in case willing of
                                  [] -> tryMove rest (tp : keep) moved
                                  (taker : _) ->
                                    let a1 = Map.insert donor (reverse keep ++ rest ++ moved) a
                                        a2 = Map.insertWith (++) taker [tp] a1
                                    in go a2
                       in tryMove donorPs [] []

          finalAcc = rebalance afterHandout
      in [ (mid, groupByTopicSorted (fromMaybe [] (Map.lookup mid finalAcc)))
         | mid <- memberIds
         ]
  where
    -- Hand a list of unassigned partitions out to whichever subscribed
    -- consumer is currently carrying the lightest load.
    handout
      :: Map.Map Text [(Text, Int32)]
      -> [(Text, Int32)]
      -> Map.Map Text [(Text, Int32)]
    handout acc [] = acc
    handout acc ((t, p) : rest) =
      let memberIds = sortOn id (Map.keys acc)
          memberSubs = Map.fromList members
          candidates =
            [ (length (fromMaybe [] (Map.lookup mid acc)), mid)
            | mid <- memberIds
            , Just subs <- [Map.lookup mid memberSubs]
            , t `elem` subs
            ]
      in case sortOn id candidates of
           [] -> handout acc rest
           ((_, mid) : _) -> handout (Map.insertWith (++) mid [(t, p)] acc) rest


{- | Group an unsorted (topic, partition) list into a per-topic list
with partitions sorted ascending. Used by every assignor so the
output order is deterministic.
-}
groupByTopicSorted :: [(Text, Int32)] -> [(Text, [Int32])]
groupByTopicSorted xs =
  let m = Map.fromListWith (++) [(t, [p]) | (t, p) <- xs]
  in [(t, sortOn id ps) | (t, ps) <- Map.toAscList m]


{- | Split a list of partitions into @n@ contiguous chunks. The first
@r@ chunks get one extra partition where @r = length xs `mod` n@.
-}
chunkRange :: [a] -> Int -> [[a]]
chunkRange xs n
  | n <= 0 = []
  | otherwise =
      let total = length xs
          (q, r) = total `quotRem` n
          sizes = replicate r (q + 1) ++ replicate (n - r) q
      in go sizes xs
  where
    go [] _ = []
    go (s : ss) ys =
      let (here, rest) = splitAt s ys
      in here : go ss rest


{- | Encode the JoinGroup subscription metadata payload, with no
previous-generation owned partitions (the first time a member
joins a group, or for non-sticky assignors).
-}
encodeSubscription :: [Text] -> BS.ByteString
encodeSubscription topics = encodeSubscriptionWithOwned topics BS.empty []


{- | Like 'encodeSubscription' but stamps a previous-generation
@ownedPartitions@ list (KIP-341 / KIP-429) into the
subscription metadata. The leader extracts this back out via
'decodeSubscriptionFull' so the sticky / cooperative-sticky
assignors can preserve assignments across rebalances.

Uses consumer-protocol v1, the lowest version that carries the
@ownedPartitions@ field on the wire. Both 'encodeSubscription'
(no owned partitions) and 'decodeSubscriptionFull' agree on
v1; v0 silently drops the field.
-}
encodeSubscriptionWithOwned
  :: [Text]
  -- ^ subscribed topics
  -> BS.ByteString
  -- ^ user-supplied opaque metadata
  -> [(Text, [Int32])]
  -- ^ owned partitions: @(topic, [pid])@
  -> BS.ByteString
encodeSubscriptionWithOwned topics userData owned =
  -- The JoinGroup 'protocols[].metadata' field is the
  -- /serialised ConsumerProtocolSubscription/ as the JVM client
  -- writes it (org.apache.kafka.clients.consumer.internals.
  -- ConsumerProtocol#serializeSubscription) — a two-byte
  -- big-endian version header followed by the version-specific
  -- subscription body. The version header is what tells the
  -- broker (and any peer member that reads our subscription)
  -- which schema version to decode against; without it the
  -- broker reads the first two bytes of the topics-array
  -- length as the version, sees a non-existent vNNN, and silently
  -- ignores the rebalance-completion path — the JoinGroup just
  -- hangs in PreparingRebalance until the rebalance timeout.
  let !body =
        WC.runEncodeVer @CPS.ConsumerProtocolSubscription consumerProtocolVersion $
          CPS.ConsumerProtocolSubscription
            { CPS.consumerProtocolSubscriptionTopics =
                P.mkKafkaArray $ V.fromList (map P.mkKafkaString topics)
            , CPS.consumerProtocolSubscriptionUserData = P.mkKafkaBytes userData
            , CPS.consumerProtocolSubscriptionOwnedPartitions =
                P.mkKafkaArray $
                  V.fromList
                    [ CPS.TopicPartition
                        { CPS.topicPartitionTopic = P.mkKafkaString t
                        , CPS.topicPartitionPartitions = P.mkKafkaArray (V.fromList ps)
                        }
                    | (t, ps) <- owned
                    ]
            , CPS.consumerProtocolSubscriptionGenerationId = -1
            , CPS.consumerProtocolSubscriptionRackId = P.KafkaString P.Null
            }
      !versionHi = fromIntegral (consumerProtocolVersion `shiftR` 8) :: Word8
      !versionLo = fromIntegral (consumerProtocolVersion .&. 0xff) :: Word8
  in BS.cons versionHi (BS.cons versionLo body)


{- | Subscription metadata schema version we negotiate. v1
introduced @ownedPartitions@ (KIP-341), needed for sticky and
cooperative-sticky assignors.
-}
consumerProtocolVersion :: Int16
consumerProtocolVersion = 1


{- | Decode a JoinGroup subscription metadata payload back into
@(topics, userData)@. Note this drops the @ownedPartitions@ field;
'decodeSubscriptionFull' is the sticky-aware variant.
-}
decodeSubscription :: BS.ByteString -> Either String ([Text], BS.ByteString)
decodeSubscription bs =
  fmap (\(ts, ud, _) -> (ts, ud)) (decodeSubscriptionFull bs)


{- | Like 'decodeSubscription' but also returns the
@ownedPartitions@ field, which sticky / cooperative-sticky
assignors use to thread previous-generation state across
rebalances (KIP-341 / KIP-429). The third tuple component is
the list of @(topic, [partitionId])@ the member had assigned
before this rebalance.
-}
decodeSubscriptionFull
  :: BS.ByteString
  -> Either String ([Text], BS.ByteString, [(Text, [Int32])])
decodeSubscriptionFull rawBs =
  -- Symmetric to 'encodeSubscriptionWithOwned': skip the
  -- two-byte version header before handing the body to the
  -- ConsumerProtocolSubscription decoder. Use the embedded
  -- version (not 'consumerProtocolVersion') so a peer that
  -- speaks an older subscription schema still decodes
  -- correctly.
  let (verBs, body) = BS.splitAt 2 rawBs
      msgVer =
        if BS.length verBs == 2
          then
            fromIntegral (BS.index verBs 0) `shiftL` 8
              .|. fromIntegral (BS.index verBs 1)
          else consumerProtocolVersion
  in case WC.runDecodeVer @CPS.ConsumerProtocolSubscription msgVer body of
       Left err -> Left err
       Right s ->
         let topicsArr = case P.unKafkaArray (CPS.consumerProtocolSubscriptionTopics s) of
               P.NotNull v -> V.toList v
               P.Null -> []
             userDataBs = case P.unKafkaBytes (CPS.consumerProtocolSubscriptionUserData s) of
               P.NotNull v -> v
               P.Null -> BS.empty
             owned = case P.unKafkaArray (CPS.consumerProtocolSubscriptionOwnedPartitions s) of
               P.NotNull tv -> V.toList tv
               P.Null -> []
             ownedDecoded =
               [ ( kafkaStringToText (CPS.topicPartitionTopic tp)
                 , case P.unKafkaArray (CPS.topicPartitionPartitions tp) of
                     P.NotNull pv -> V.toList pv
                     P.Null -> []
                 )
               | tp <- owned
               ]
         in Right (map kafkaStringToText topicsArr, userDataBs, ownedDecoded)


-- | Decode the per-member SyncGroup assignment payload.
decodeAssignment :: BS.ByteString -> Either String [(Text, [Int32])]
decodeAssignment = fmap fst . decodeAssignmentFull


-- | The assignment payload's partitions and user data.
decodeAssignmentFull :: BS.ByteString -> Either String ([(Text, [Int32])], BS.ByteString)
decodeAssignmentFull rawBs =
  -- Symmetric to 'assignmentBytes' above: skip the two-byte
  -- version header, then decode the body at the embedded
  -- version.
  let (verBs, body) = BS.splitAt 2 rawBs
      msgVer =
        if BS.length verBs == 2
          then
            fromIntegral (BS.index verBs 0) `shiftL` 8
              .|. fromIntegral (BS.index verBs 1)
          else 0
  in case WC.runDecodeVer @CPA.ConsumerProtocolAssignment msgVer body of
       Left err -> Left err
       Right a ->
         let parts = case P.unKafkaArray (CPA.consumerProtocolAssignmentAssignedPartitions a) of
               P.NotNull v -> V.toList v
               P.Null -> []
             ud = case P.unKafkaBytes (CPA.consumerProtocolAssignmentUserData a) of
               P.NotNull b -> b
               P.Null -> BS.empty
         in Right
              ( map
                  ( \tp ->
                      ( kafkaStringToText (CPA.topicPartitionTopic tp)
                      , case P.unKafkaArray (CPA.topicPartitionPartitions tp) of
                          P.NotNull v -> V.toList v
                          P.Null -> []
                      )
                  )
                  parts
              , ud
              )


{- | OffsetFetch for a list of TopicPartitions; returns a map from
topic-partition to its committed offset (entries are omitted when
there is no committed offset, leaving the caller to use the
consumer's reset policy).
-}
offsetFetchAll
  :: AV.ApiVersionCache
  -> Conn.ConnectionManager
  -> BrokerAddress
  -> Conn.Connection
  -> Text
  -- ^ clientId
  -> Text
  -- ^ groupId
  -> [TopicPartition]
  -> Int32
  -- ^ correlation id
  -> IO (Either String (Map.Map TopicPartition Int64))
offsetFetchAll versionCache connMgr coordAddr conn clientId groupId tps corrId = do
  let apiKey = 9
      clientMaxVersion = 5
  brokerVersionM <- atomically $ AV.queryApiVersion versionCache coordAddr apiKey
  let apiVersion = case brokerVersionM of
        Nothing -> 0
        Just range -> fromMaybe 0 (AV.selectVersion clientMaxVersion range)

      byTopic =
        Map.fromListWith
          (++)
          [(tpTopic t, [tpPartition t]) | t <- tps]
      topics =
        V.fromList
          [ OFReq.OffsetFetchRequestTopic
              { OFReq.offsetFetchRequestTopicName = P.mkKafkaString topic
              , OFReq.offsetFetchRequestTopicPartitionIndexes =
                  P.mkKafkaArray (V.fromList parts)
              }
          | (topic, parts) <- Map.toList byTopic
          ]
      request =
        OFReq.OffsetFetchRequest
          { OFReq.offsetFetchRequestGroupId = P.mkKafkaString groupId
          , OFReq.offsetFetchRequestTopics = P.mkKafkaArray topics
          , OFReq.offsetFetchRequestGroups = P.mkKafkaArray V.empty
          , OFReq.offsetFetchRequestRequireStable = False
          }
      requestBody = WC.runEncodeVer @OFReq.OffsetFetchRequest apiVersion request
      clientIdK = P.mkKafkaString clientId
  result <-
    Req.sendRequestReceiveResponseLocked
      (Conn.withBrokerLock connMgr coordAddr)
      conn
      apiKey
      apiVersion
      corrId
      clientIdK
      requestBody
  case result of
    Left err -> pure (Left err)
    Right (_, body) ->
      case WC.runDecodeVer @OFResp.OffsetFetchResponse apiVersion body of
        Left err -> pure (Left ("decode OffsetFetch: " <> err))
        Right resp ->
          let topicsList = case P.unKafkaArray (OFResp.offsetFetchResponseTopics resp) of
                P.NotNull v -> V.toList v
                P.Null -> []
              entries =
                [ (TopicPartition topic pid, off)
                | tr <- topicsList
                , let topic = kafkaStringToText (OFResp.offsetFetchResponseTopicName tr)
                      partsList = case P.unKafkaArray (OFResp.offsetFetchResponseTopicPartitions tr) of
                        P.NotNull v -> V.toList v
                        P.Null -> []
                , pr <- partsList
                , let pid = OFResp.offsetFetchResponsePartitionPartitionIndex pr
                      off = OFResp.offsetFetchResponsePartitionCommittedOffset pr
                      errCd = OFResp.offsetFetchResponsePartitionErrorCode pr
                , errCd == 0
                , off >= 0
                ]
          in pure (Right (Map.fromList entries))


{- | Sentinel offset used when no committed offset exists. We thread
the policy through and let the regular Consumer.poll path treat -2 /
-1 as "earliest" / "latest" markers when it builds the FetchRequest.
(Kafka itself uses these timestamps in ListOffsets, and the Consumer
already understands them.)
-}
resetSentinel :: ResetPolicy -> Int64
resetSentinel ResetEarliest = 0 -- start of log; the broker rejects negatives in offsets
resetSentinel ResetLatest = 0 -- conservative fallback; tests / handler can re-seek
resetSentinel ResetNone = 0


{- | Mirror of 'Kafka.Client.Consumer.OffsetResetStrategy', kept local
to avoid the import cycle.
-}
data ResetPolicy = ResetEarliest | ResetLatest | ResetNone
  deriving (Eq, Show)


kafkaStringToText :: P.KafkaString -> Text
kafkaStringToText ks = case P.unKafkaString ks of
  P.NotNull t -> t
  P.Null -> T.empty
