{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Streams.Runtime.NativeDriver
Description : The 'StreamDriver' over the real Kafka clients

What a Kafka Streams instance runs against a cluster, configured from
its 'StreamsConfig' like the JVM client configures its embedded
clients:

  * the main consumer joins group @application.id@ with the runtime's
    assignor (the streams protocol: whole tasks, so co-partitioned
    partitions land together), never auto-commits, starts unseen
    partitions at the earliest offset, and reads committed data only
    under exactly-once;
  * readers (restore, global stores) are group-less consumers with the
    same isolation level;
  * under exactly-once the producer is transactional with
    @transactional.id = \<application.id\>-\<random process id\>@ (EOS v2:
    one producer per process, zombies are fenced by the group
    generation rather than by a fixed transactional id);
  * an admin client creates internal topics.

'consumerOverrides', 'producerOverrides' and 'adminOverrides' are
applied last.
-}
module Kafka.Streams.Runtime.NativeDriver (
  newNativeDriver,
) where

import Control.Exception (SomeException, try)
import Data.HashMap.Strict qualified as HashMap
import Data.IORef
import Data.List (isPrefixOf)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Kafka.Client.AdminClient qualified as KA
import Kafka.Client.Consumer qualified as KC
import Kafka.Client.Internal.TransactionCoordinator qualified as TC
import Kafka.Client.Producer qualified as KP
import Kafka.Client.Transaction qualified as KT
import Kafka.Streams.Config (ProcessingGuarantee (..), StreamsConfig (..))
import Kafka.Streams.Runtime.Driver


toDriverRecord :: KC.ConsumerRecord -> DriverRecord
toDriverRecord r =
  DriverRecord
    { drTopic = r.topic
    , drPartition = r.partition
    , drOffset = r.offset
    , drTimestamp = r.timestamp
    , drKey = r.key
    , drValue = r.value
    , drHeaders = r.headers
    }


showE :: Show e => Either e a -> Either String a
showE = either (Left . show) Right


newNativeDriver :: StreamsConfig -> IO (Either String StreamDriver)
newNativeDriver cfg = do
  let eos = processingGuarantee cfg == ExactlyOnceV2
      boot = bootstrapServers cfg
      iso = if eos then KC.ReadCommitted else KC.ReadUncommitted
      consumerBase gid =
        consumerOverrides cfg
          KC.defaultConsumerConfig
            { KC.consumerClientId = clientId cfg
            , KC.consumerGroupId = gid
            , KC.consumerIsolationLevel = iso
            , KC.consumerAutoOffsetReset = KC.Earliest
            , KC.consumerAutoCommit = False
            , KC.consumerAssignmentStrategy = KC.RangeAssignment
            }
  pid <- UUID.nextRandom
  let txnId = applicationId cfg <> "-" <> UUID.toText pid
      producerCfg =
        producerOverrides cfg
          KP.defaultProducerConfig
            { KP.producerClientId = clientId cfg
            , KP.producerTransactional = if eos then Just txnId else Nothing
            , KP.producerIdempotent = eos
            , KP.producerDelivery = if eos then KP.ExactlyOnce else KP.AtLeastOnce
            }
  eAdmin <- KA.createAdminClient boot (adminOverrides cfg KA.defaultAdminClientConfig {KA.adminClientId = clientId cfg <> "-admin"})
  ePr <- KP.createProducer boot producerCfg
  eCo <- KC.createConsumer boot (applicationId cfg) (consumerBase (applicationId cfg))
  case (eAdmin, ePr, eCo) of
    (Left e, _, _) -> pure (Left ("admin client: " <> e))
    (_, Left e, _) -> pure (Left ("producer: " <> e))
    (_, _, Left e) -> pure (Left ("consumer: " <> e))
    (Right admin, Right producer, Right consumer) -> do
      eTxn <-
        if eos
          then case boot of
            (b : _) -> fmap Just <$> KP.newBoundTransaction producer b
            [] -> pure (Left "no bootstrap servers")
          else pure (Right Nothing)
      case eTxn of
        Left e -> pure (Left ("transaction: " <> e))
        Right mTxn -> do
          readers <- newIORef []
          let withTxn :: (KT.Transaction -> IO (Either String ())) -> IO (Either String ())
              withTxn k = maybe (pure (Left "not transactional (processing.guarantee is at least once)")) k mTxn
          pure . Right $
            StreamDriver
              { sdSubscribe = \topics ga cbs -> do
                  KC.setAssignmentStrategy consumer (KC.CustomAssignment (gaAssignor ga))
                  KC.setSubscriptionUserDataHook consumer (gaSubscriptionUserData ga)
                  KC.setRebalanceListener consumer (rcAssigned cbs) (rcRevoked cbs) (rcLost cbs)
                  KC.setPreRejoinHook consumer (rcBeforeRejoin cbs)
                  KC.setAssignmentHook consumer (rcAssignment cbs)
                  KC.subscribe consumer topics
              , sdPoll = \ms -> fmap (map toDriverRecord) <$> KC.poll consumer ms
              , sdCommitOffsets = \offs ->
                  KC.commitSyncOffsets
                    consumer
                    (Map.fromList (map (\(tp, o) -> (tp, KC.OffsetAndMetadata o Nothing "")) offs))
              , sdCommitted = \tps -> fmap (Map.fromList . HashMap.toList) <$> KC.committedAll consumer tps
              , sdSeek = seekTo consumer
              , sdPause = KC.pause consumer
              , sdResume = KC.resume consumer
              , sdRequestRejoin = () <$ KC.requestRejoin consumer
              , sdCloseConsumer = \leave tmo ->
                  if leave
                    then KC.closeConsumerWithTimeout consumer tmo
                    else KC.closeConsumerWithoutLeavingGroup consumer tmo
              , sdNewReader = do
                  eR <- KC.createConsumer boot "" (consumerBase "")
                  case eR of
                    Left e -> pure (Left e)
                    Right rc -> do
                      modifyIORef' readers (rc :)
                      pure . Right $
                        Reader
                          { rdAssign = \tps -> do
                              r <- KC.assign rc (map fst tps)
                              case r of
                                Left e -> pure (Left e)
                                Right () -> sequence_ <$> mapM (uncurry (seekTo rc)) tps
                          , rdPoll = \ms -> fmap (map toDriverRecord) <$> KC.poll rc ms
                          , rdPosition = KC.position rc
                          , rdEndOffsets = \tps -> fmap (Map.fromList . HashMap.toList) <$> KC.endOffsets rc tps
                          , rdClose = KC.closeConsumer rc
                          }
              , sdSend = KP.sendRecordAsync producer
              , sdFlush = KP.flushProducer producer
              , sdPartitionsFor = \t -> fmap length <$> KP.partitionsFor producer t
              , sdCloseProducer = KP.closeProducer producer
              , sdInitTransactions = withTxn (fmap showE . KT.initTransactions)
              , sdBeginTransaction = withTxn (fmap showE . KT.beginTransaction)
              , sdSendOffsetsToTransaction = \offs -> withTxn $ \t -> do
                  mCoord <- KC.groupCoordinator consumer
                  meta <- KC.groupMetadata consumer
                  case mCoord of
                    Nothing -> pure (Left "no group coordinator known")
                    Just coord ->
                      showE
                        <$> KT.sendOffsetsToTransaction
                          t
                          coord
                          TC.TxnGroupMember
                            { TC.tgmGroupId = meta.cgmGroupId
                            , TC.tgmGenerationId = meta.cgmGenerationId
                            , TC.tgmMemberId = meta.cgmMemberId
                            , TC.tgmGroupInstanceId = meta.cgmGroupInstanceId
                            }
                          offs
              , sdCommitTransaction = withTxn (fmap showE . KT.commitTransaction)
              , sdAbortTransaction = withTxn (fmap showE . KT.abortTransaction)
              , sdCreateTopics = \nts -> do
                  r <-
                    KA.createTopics
                      admin
                      ( map
                          ( \nt ->
                              KA.NewTopic
                                { KA.ntName = nitName nt
                                , KA.ntNumPartitions = fromIntegral (nitPartitions nt)
                                , KA.ntReplicationFactor = fromIntegral (nitReplicationFactor nt)
                                , KA.ntConfigs = nitConfigs nt
                                }
                          )
                          nts
                      )
                  pure $ case r of
                    Left e -> Left e
                    Right results ->
                      -- TOPIC_ALREADY_EXISTS (36) is fine: the plan checks partition counts next
                      case filter (either (not . ("Error 36" `isPrefixOf`)) (const False) . snd) results of
                        [] -> Right ()
                        bad -> Left (unwords (map (\(t, e) -> T.unpack t <> ": " <> either id (const "") e) bad))
              , sdDescribeTopics = \ts ->
                  if null ts
                    then pure (Right Map.empty)
                    else do
                      r <- KA.describeTopics admin ts
                      pure $
                        Map.fromList
                          . map (\d -> (KA.tdName d, length (KA.tdPartitions d)))
                          . filter (not . null . KA.tdPartitions)
                          <$> r
              , sdDeleteRecords = \offs ->
                  fmap (const ())
                    <$> KA.deleteRecords admin (map (\(tp, o) -> (tp.topic, tp.partition, o)) offs)
              , sdClose = do
                  rs <- readIORef readers
                  mapM_ (\c -> try (KC.closeConsumer c) :: IO (Either SomeException ())) rs
                  KA.closeAdminClient admin
              }
  where
    seekTo c tp off
      | off == -2 = KC.seekToBeginning c [tp]
      | off == -1 = KC.seekToEnd c [tp]
      | otherwise = KC.seek c tp off

