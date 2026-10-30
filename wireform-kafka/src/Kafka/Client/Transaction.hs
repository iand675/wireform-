{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

{- |
Module: Kafka.Client.Transaction
Description: Group multiple producer sends + consumer offset commits into one atomic write.

A Kafka /transaction/ lets a producer publish to several partitions
and update one or more consumer offsets, then commit the lot
atomically — either every write lands, or none of them do. That is
what \"exactly once\" actually means in Kafka: a stream-processing
pipeline that reads from one topic and writes to another can
guarantee its outputs and its consumer offsets advance together.

= The five-step recipe

@
'withProducer' brokers
  'defaultProducerConfig'
    { 'producerTransactional' = Just \"my-app-1\"
    , 'producerIdempotent'    = True
    } $ \\p -> do
      txn <- 'createTransaction' (TransactionalId \"my-app-1\") connMgr vCache
               \"my-app-client\" bootstrap 60_000
      Right () <- 'initTransactions' txn           -- once per producer
      'bindTransaction' p txn

      Right () <- 'beginTransaction' txn
      _        <- 'sendMessage' p \"out\" Nothing payload
      Right () <- 'commitTransaction' txn
@

Five steps in order:

  1. Configure the producer with a 'producerTransactional' id and
     'producerIdempotent' on (these are required by the broker).
  2. 'createTransaction' for the coordinator handle, then
     'initTransactions' once — this fences any zombie producer
     using the same id from a previous run.
  3. 'bindTransaction' the producer to the transaction so subsequent
     sends participate in it.
  4. 'beginTransaction', send records via 'sendMessage' (and / or
     stage consumer offsets via 'commitOffsetsInTransaction'),
     then 'commitTransaction' or 'abortTransaction'.
  5. Repeat 4 as many times as you like — one initialised
     'Transaction' handle supports many begin/commit cycles.

The wire-level guarantees the broker provides:

  * /Atomicity/ — every record in the transaction either becomes
    visible or none of them do.
  * /Fencing/ — only one producer instance can hold a given
    transactional id at a time; an older instance is reliably
    locked out as soon as a newer one calls 'initTransactions'.
  * /Read isolation/ — a consumer configured with
    'consumerIsolationLevel = ReadCommitted' will only return
    records from committed transactions.

= Where the lower-level primitives live

Most user code only ever needs the five-step recipe above. The
'createTransaction' \/ 'getTransactionState' \/ 'transitionState'
exports are for the integration tests and the OpenTelemetry
instrumentation.
-}
module Kafka.Client.Transaction (
  -- * The transaction lifecycle

  --

  {- | Initialise once per producer process; then begin / commit /
  abort as many times as you like.
  -}
  initTransactions,
  beginTransaction,
  commitTransaction,
  abortTransaction,
  withTransaction,

  -- * Transactional sends
  sendInTransaction,
  registerPartition,
  sendOffsetsToTransaction,

  -- * Types
  Transaction (..),
  TransactionState (..),
  ProducerId (..),
  ProducerEpoch (..),
  TransactionalId (..),
  TransactionError (..),

  -- * Low-level state management

  --

  {- | Used by integration tests and the OpenTelemetry
  instrumentation. Most user code does not need to touch these.
  -}
  createTransaction,
  getTransactionState,
  transitionState,

  -- * Transactional-id helpers
  transactionalIdOptional,

  -- * Transactional-error classification
  TxnErrorRecovery (..),
  classifyTxnError,

  -- * Bounded txn-op deadlines
  TxnDeadline (..),
  effectiveTxnDeadlineMs,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Exception (Exception, SomeException, try)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.IORef (
  IORef,
  atomicModifyIORef',
  newIORef,
  readIORef,
  writeIORef,
 )
import Data.Int (Int16, Int32, Int64)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Clock (getMonotonicTimeNSec)
import Kafka.Client.Consumer (TopicPartition (..))
import Kafka.Client.Internal.TransactionCoordinator (TransactionCoordinator)
import Kafka.Client.Internal.TransactionCoordinator qualified as TC
import Kafka.Client.RetryClassifier qualified as RC
import Kafka.Network.Connection (BrokerAddress, ConnectionConfig, ConnectionManager)
import Kafka.Network.Connection qualified as Conn
import Kafka.Protocol.ApiVersions (ApiVersionCache)
import Kafka.Protocol.ApiVersions qualified as AV


-- | Producer ID assigned by the transaction coordinator
newtype ProducerId = ProducerId {unProducerId :: Int64}
  deriving (Show, Eq, Ord)


-- | Producer epoch for fencing
newtype ProducerEpoch = ProducerEpoch {unProducerEpoch :: Int16}
  deriving (Show, Eq, Ord)


-- | Transactional ID for EOS
newtype TransactionalId = TransactionalId {unTransactionalId :: Text}
  deriving (Show, Eq, Ord)


-- | Transaction state machine
data TransactionState
  = -- | Transaction not yet initialized
    Uninitialized
  | -- | Transaction initialized, ready to begin
    Ready
  | -- | Transaction in progress
    InTransaction
  | -- | Transaction committing
    Committing
  | -- | Transaction aborting
    Aborting
  | -- | Transaction aborted
    Aborted
  | -- | Producer has been fenced by a newer instance
    Fenced
  | -- | Fatal error state
    Error Text
  deriving (Show, Eq)


{- | Transaction handle with thread-safe state management.

Tier 3 of the STM-replacement work moves the per-transaction
handoff slots ('txnProducerId', 'txnProducerEpoch', 'txnState',
'txnCoordinator') to 'IORef': they are all single-writer (the
transaction-coordinator-driven state machine) / multi-reader
(sender thread + close path) handoffs that never composed
transactionally with anything else. 'txnPartitions' and
'txnSequenceNumbers' stay 'TVar' because the producer's
partition-registration path uses STM to compose
check-then-insert atomically with the transactional state.
-}
data Transaction = Transaction
  { txnTransactionalId :: !TransactionalId
  , txnProducerId :: !(IORef (Maybe ProducerId))
  , txnProducerEpoch :: !(IORef (Maybe ProducerEpoch))
  , txnState :: !(IORef TransactionState)
  , txnPartitions :: !(TVar (HashSet TopicPartition))
  -- ^ Partitions involved in the current transaction.
  , txnPendingPartitions :: !(TVar (HashSet TopicPartition))
  {- ^ Partitions whose @AddPartitionsToTxn@ is in flight
  ('registerPartition'); a concurrent first send to one waits for it.
  -}
  , txnStarted :: !(IORef Bool)
  {- ^ Whether the coordinator has this transaction open: some
  @AddPartitionsToTxn@ or @AddOffsetsToTxn@ succeeded since
  'beginTransaction'. Without one there is nothing to end, and the
  coordinator would answer @EndTxn@ with @INVALID_TXN_STATE@.
  -}
  , txnSequenceNumbers :: !(TVar (HashMap TopicPartition Int32))
  -- ^ Sequence numbers per partition for idempotency.
  , txnCoordinator :: !(IORef (Maybe TransactionCoordinator))
  -- ^ Cached transaction coordinator
  , txnConnectionManager :: !ConnectionManager
  , txnConnConfig :: !ConnectionConfig
  -- ^ TLS / SASL settings for coordinator connections
  -- ^ Connection manager for network operations
  , txnVersionCache :: !ApiVersionCache
  -- ^ API version cache for version negotiation
  , txnCorrelationId :: !(IORef Int32)
  {- ^ Correlation ID generator. Single source of monotonically
  increasing correlation IDs handed to TransactionCoordinator
  requests; never composed transactionally with anything else,
  so 'IORef' + 'atomicModifyIORef\'' suffices.
  -}
  , txnClientId :: !Text
  -- ^ Client ID for requests
  , txnBootstrapBroker :: !BrokerAddress
  -- ^ Bootstrap broker for coordinator discovery
  , txnTimeoutMs :: !Int32
  -- ^ Transaction timeout in milliseconds
  }


-- | Transaction errors
data TransactionError
  = TransactionNotInitialized Text
  | TransactionAlreadyInProgress Text
  | TransactionNotInProgress Text
  | InvalidStateTransition TransactionState TransactionState
  | ProducerFenced Text
  | CoordinatorNotAvailable Text
  | TransactionAborted Text
  | TransactionTimeout Text
  | UnknownTransactionError Text
  | ConsumerGroupFenced Text
  {- ^ The group coordinator rejected a transactional offset commit
  because the member's generation is stale or it is no longer a
  member (@ILLEGAL_GENERATION@, @UNKNOWN_MEMBER_ID@,
  @FENCED_INSTANCE_ID@): a rebalance moved its partitions. The
  transaction must be aborted.
  -}
  deriving (Show, Eq)


{- | Classify a coordinator-level error for callers of the public
'Transaction' API.
-}
coordinatorErrorToTxn :: TC.TransactionCoordinatorError -> TransactionError
coordinatorErrorToTxn = \case
  TC.ProducerFenced m -> ProducerFenced m
  TC.TransactionCoordinatorFenced m -> ProducerFenced m
  TC.InvalidProducerEpoch m -> ProducerFenced m
  TC.CoordinatorNotAvailable m -> CoordinatorNotAvailable m
  TC.CoordinatorLoadInProgress m -> CoordinatorNotAvailable m
  TC.NotCoordinator m -> CoordinatorNotAvailable m
  TC.ConcurrentTransactions m -> CoordinatorNotAvailable m
  TC.UnknownCoordinatorError code m
    | code `elem` [22, 25, 82] -> ConsumerGroupFenced m
  e -> UnknownTransactionError (T.pack (show e))


instance Exception TransactionError


-- | Create a new transaction handle with all required infrastructure
createTransaction
  :: MonadIO m
  => TransactionalId
  -> ConnectionManager
  -> ConnectionConfig
  -- ^ TLS / SASL settings for coordinator connections (the producer's)
  -> ApiVersionCache
  -> Text
  -- ^ Client ID
  -> BrokerAddress
  -- ^ Bootstrap broker
  -> Int32
  -- ^ Transaction timeout (ms)
  -> m Transaction
createTransaction transactionalId connMgr connConfig versionCache clientId bootstrapBroker timeoutMs = liftIO $ do
  producerId <- newIORef Nothing
  producerEpoch <- newIORef Nothing
  state <- newIORef Uninitialized
  partitions <- newTVarIO HashSet.empty
  pending <- newTVarIO HashSet.empty
  started <- newIORef False
  sequences <- newTVarIO HashMap.empty
  coordinator <- newIORef Nothing
  correlationId <- newIORef 0
  return
    Transaction
      { txnTransactionalId = transactionalId
      , txnProducerId = producerId
      , txnProducerEpoch = producerEpoch
      , txnState = state
      , txnPartitions = partitions
      , txnPendingPartitions = pending
      , txnStarted = started
      , txnSequenceNumbers = sequences
      , txnCoordinator = coordinator
      , txnConnectionManager = connMgr
      , txnConnConfig = connConfig
      , txnVersionCache = versionCache
      , txnCorrelationId = correlationId
      , txnClientId = clientId
      , txnBootstrapBroker = bootstrapBroker
      , txnTimeoutMs = timeoutMs
      }


-- | Get current transaction state
getTransactionState :: Transaction -> IO TransactionState
getTransactionState txn = readIORef (txnState txn)


-- | Attempt to transition to a new state, returns False if transition is invalid
transitionState :: Transaction -> TransactionState -> IO Bool
transitionState txn newState =
  atomicModifyIORef' (txnState txn) $ \currentState ->
    if isValidTransition currentState newState
      then (newState, True)
      else (currentState, False)


-- | Check if state transition is valid
isValidTransition :: TransactionState -> TransactionState -> Bool
isValidTransition from to = case (from, to) of
  -- Initialization
  (Uninitialized, Ready) -> True
  -- Begin transaction
  (Ready, InTransaction) -> True
  -- Commit/abort
  (InTransaction, Committing) -> True
  (InTransaction, Aborting) -> True
  (Committing, Ready) -> True
  (Aborting, Aborted) -> True
  (Aborted, Ready) -> True
  -- Error states
  (_, Error _) -> True
  (_, Fenced) -> True
  -- Same state is always valid (idempotent operations)
  _ | from == to -> True
  -- Everything else is invalid
  _ -> False


{- | Initialize transactions (KIP-98)
This must be called before any transactional operations.
It discovers the transaction coordinator and obtains a producer ID.
-}
initTransactions :: MonadIO m => Transaction -> m (Either TransactionError ())
initTransactions txn = liftIO $ do
  state <- getTransactionState txn
  case state of
    Uninitialized -> do
      let transactionalId = unTransactionalId $ txnTransactionalId txn

      -- Discover transaction coordinator
      coordResult <-
        TC.findTransactionCoordinator
          (txnConnectionManager txn)
          (txnConnConfig txn)
          (txnVersionCache txn)
          (txnCorrelationId txn)
          (txnBootstrapBroker txn)
          (txnClientId txn)
          transactionalId

      case coordResult of
        Left err -> return $ Left $ CoordinatorNotAvailable $ T.pack $ show err
        Right coordinator -> do
          -- Cache the coordinator
          writeIORef (txnCoordinator txn) (Just coordinator)

          -- Initialize producer ID, with retry for the broker's
          -- mid-transition error codes:
          --
          --   * @CONCURRENT_TRANSACTIONS@ (96) — the broker is
          --     in the middle of completing a previous
          --     transaction for this id; retry once the abort
          --     marker has landed.
          --   * @INVALID_PRODUCER_EPOCH@ (51) — when InitProducerId
          --     arrives against an @Ongoing@ transaction the
          --     broker has to drive the previous epoch through
          --     @PrepareEpochFence -> CompleteAbort@ before
          --     bumping; some Kafka 3.7 paths surface this as
          --     INVALID_PRODUCER_EPOCH instead of
          --     CONCURRENT_TRANSACTIONS while the transit is in
          --     flight. The JVM client retries on both.
          --
          -- Bounded retry: 5 attempts with 100ms exponential
          -- backoff (max ~1.6s total). Beyond that we surface
          -- the last error.
          pidResult <- initProducerIdWithRetry txn coordinator transactionalId 5

          case pidResult of
            Left err -> return $ Left $ CoordinatorNotAvailable $ T.pack $ show err
            Right (pid, epoch) -> do
              -- Store producer ID and epoch.
              -- Tier 3 of the STM-replacement work: 'txnProducerId',
              -- 'txnProducerEpoch' and 'txnState' moved to 'IORef';
              -- the cross-ref atomicity used to be provided by the
              -- enclosing 'atomically' block. The producer reads
              -- these three together via three independent
              -- 'readIORef's; an interleaved sender that observes
              -- (Just pid, Nothing epoch, Ready) for one
              -- instruction is harmless because the sender only
              -- consults these when the state is already Ready and
              -- it has its own pid/epoch snapshot via
              -- @senderTransactionalId@.
              writeIORef (txnProducerId txn) (Just $ ProducerId pid)
              writeIORef (txnProducerEpoch txn) (Just $ ProducerEpoch epoch)
              writeIORef (txnState txn) Ready

              return $ Right ()
    Ready -> return $ Right () -- Already initialized, idempotent
    Error msg -> return $ Left $ UnknownTransactionError msg
    Fenced -> return $ Left $ ProducerFenced "Producer has been fenced"
    _ -> return $ Left $ InvalidStateTransition state Ready


{- | InitProducerId with bounded retry for the broker's
mid-transition error codes. Mirrors the JVM client's
@TransactionManager.initializeTransactions@ loop: retry on
@CONCURRENT_TRANSACTIONS@ / @INVALID_PRODUCER_EPOCH@ /
@COORDINATOR_LOAD_IN_PROGRESS@ with exponential backoff, surface
everything else immediately.

The retry is bounded so a genuine producer-fenced situation
(e.g. a different client really did bump our epoch and we're
not allowed in) eventually surfaces as a hard error rather
than spinning forever.
-}
initProducerIdWithRetry
  :: Transaction
  -> TC.TransactionCoordinator
  -> Text
  -- ^ transactional id
  -> Int
  -- ^ remaining attempts
  -> IO (Either TC.TransactionCoordinatorError (Int64, Int16))
initProducerIdWithRetry txn coordinator transactionalId attemptsLeft = do
  pidResult <-
    TC.initProducerId
      (txnConnectionManager txn)
      (txnConnConfig txn)
      (txnVersionCache txn)
      (txnCorrelationId txn)
      (txnClientId txn)
      coordinator
      (Just transactionalId)
      (txnTimeoutMs txn)
      Nothing -- No existing producer ID for first init
      Nothing -- No existing epoch for first init
  case pidResult of
    Right ok -> pure (Right ok)
    Left err
      | attemptsLeft <= 1 -> pure (Left err)
      | retriable err -> do
          -- 5 attempts: 100ms, 200ms, 400ms, 800ms, give up.
          let !attempt = 5 - attemptsLeft
              !delayUs = 100_000 * (2 ^ max 0 attempt)
          threadDelay delayUs
          initProducerIdWithRetry txn coordinator transactionalId (attemptsLeft - 1)
      | otherwise -> pure (Left err)
  where
    retriable :: TC.TransactionCoordinatorError -> Bool
    retriable e = case e of
      TC.ConcurrentTransactions _ -> True
      TC.InvalidProducerEpoch _ -> True
      TC.CoordinatorLoadInProgress _ -> True
      TC.CoordinatorNotAvailable _ -> True
      _ -> False


-- | Begin a new transaction (KIP-98)
beginTransaction :: MonadIO m => Transaction -> m (Either TransactionError ())
beginTransaction txn = liftIO $ do
  state <- getTransactionState txn
  case state of
    Ready -> do
      success <- transitionState txn InTransaction
      if success
        then do
          -- Clear partition tracking for new transaction
          atomically $ writeTVar (txnPartitions txn) HashSet.empty
          writeIORef (txnStarted txn) False
          return $ Right ()
        else return $ Left $ InvalidStateTransition state InTransaction
    InTransaction -> return $ Left $ TransactionAlreadyInProgress "Transaction already in progress"
    Uninitialized -> return $ Left $ TransactionNotInitialized "Must call initTransactions first"
    Error msg -> return $ Left $ UnknownTransactionError msg
    Fenced -> return $ Left $ ProducerFenced "Producer has been fenced"
    _ -> return $ Left $ InvalidStateTransition state InTransaction


-- | Commit the current transaction (KIP-98)
commitTransaction :: MonadIO m => Transaction -> m (Either TransactionError ())
commitTransaction txn = liftIO $ do
  state <- getTransactionState txn
  case state of
    InTransaction -> do
      success <- transitionState txn Committing
      if success
        then do
          -- Get coordinator, producer ID, and epoch
          coordM <- readIORef (txnCoordinator txn)
          pidM <- readIORef (txnProducerId txn)
          epochM <- readIORef (txnProducerEpoch txn)
          started <- readIORef (txnStarted txn)

          case (coordM, pidM, epochM) of
            _ | not started -> do
              -- Nothing reached the coordinator: nothing to commit.
              writeIORef (txnState txn) Ready
              return $ Right ()
            (Just _, Just (ProducerId pid), Just (ProducerEpoch epoch)) -> do
              let transactionalId = unTransactionalId $ txnTransactionalId txn

              -- End transaction with commit=true
              endResult <- endTransactionWithRetry txn transactionalId pid epoch True
              case endResult of
                Left err -> failEnd txn InTransaction err
                Right () -> do
                  writeIORef (txnState txn) Ready
                  return $ Right ()
            _ -> return $ Left $ TransactionNotInitialized "Missing producer ID or coordinator"
        else return $ Left $ InvalidStateTransition state Committing
    Committing -> do
      -- Already committing, just return success (idempotent)
      return $ Right ()
    Ready -> return $ Left $ TransactionNotInProgress "No transaction in progress"
    Uninitialized -> return $ Left $ TransactionNotInitialized "Must call initTransactions first"
    Error msg -> return $ Left $ UnknownTransactionError msg
    Fenced -> return $ Left $ ProducerFenced "Producer has been fenced"
    _ -> return $ Left $ InvalidStateTransition state Committing


-- | Abort the current transaction (KIP-98)
abortTransaction :: MonadIO m => Transaction -> m (Either TransactionError ())
abortTransaction txn = liftIO $ do
  state <- getTransactionState txn
  case state of
    InTransaction -> do
      success <- transitionState txn Aborting
      if success
        then do
          -- Get coordinator, producer ID, and epoch
          coordM <- readIORef (txnCoordinator txn)
          pidM <- readIORef (txnProducerId txn)
          epochM <- readIORef (txnProducerEpoch txn)
          started <- readIORef (txnStarted txn)

          case (coordM, pidM, epochM) of
            _ | not started -> do
              -- Nothing reached the coordinator: nothing to abort.
              writeIORef (txnState txn) Ready
              return $ Right ()
            (Just _, Just (ProducerId pid), Just (ProducerEpoch epoch)) -> do
              let transactionalId = unTransactionalId $ txnTransactionalId txn

              -- End transaction with commit=false (abort)
              endResult <- endTransactionWithRetry txn transactionalId pid epoch False
              case endResult of
                Left err -> failEnd txn InTransaction err
                Right () -> do
                  writeIORef (txnState txn) Aborted
                  writeIORef (txnState txn) Ready
                  return $ Right ()
            _ -> return $ Left $ TransactionNotInitialized "Missing producer ID or coordinator"
        else return $ Left $ InvalidStateTransition state Aborting
    Aborting -> do
      -- Already aborting, just return success (idempotent)
      return $ Right ()
    Ready -> return $ Left $ TransactionNotInProgress "No transaction in progress"
    Uninitialized -> return $ Left $ TransactionNotInitialized "Must call initTransactions first"
    Error msg -> return $ Left $ UnknownTransactionError msg
    Fenced -> return $ Left $ ProducerFenced "Producer has been fenced"
    _ -> return $ Left $ InvalidStateTransition state Aborting


{- | Send a request to the transaction coordinator with the JVM
@TransactionManager@'s retry policy: the coordinator answers
@CONCURRENT_TRANSACTIONS@ while a previous transaction's markers are
still being written (so right after a commit, the next
transaction's first @AddPartitionsToTxn@ often gets it), and
@COORDINATOR_LOAD_IN_PROGRESS@ / @NOT_COORDINATOR@ /
@COORDINATOR_NOT_AVAILABLE@ while the transaction-log partition
moves. All clear up, so they are retried with exponential backoff
(20 ms doubling to 1 s, re-discovering the coordinator after a
move) until the transaction timeout has passed, and only then
reported.
-}
withCoordinatorRetry
  :: Transaction
  -> (TC.TransactionCoordinator -> IO (Either TC.TransactionCoordinatorError a))
  -> IO (Either TC.TransactionCoordinatorError a)
withCoordinatorRetry txn req = do
  coordM <- readIORef (txnCoordinator txn)
  case coordM of
    Nothing -> pure (Left (TC.CoordinatorNotAvailable "no transaction coordinator (initTransactions never completed?)"))
    Just coord0 -> do
      start <- getMonotonicTimeNSec
      go (start + fromIntegral (max 0 (txnTimeoutMs txn)) * 1_000_000) 20_000 coord0
  where
    go !deadline !delayUs coordinator = do
      r <- req coordinator
      case r of
        Left err | retriable err -> do
          now <- getMonotonicTimeNSec
          if now >= deadline
            then pure r
            else do
              threadDelay delayUs
              coordinator' <- case err of
                TC.ConcurrentTransactions _ -> pure coordinator
                _ -> rediscover coordinator
              go deadline (min 1_000_000 (2 * delayUs)) coordinator'
        _ -> pure r
    rediscover coordinator = do
      found <-
        TC.findTransactionCoordinator
          (txnConnectionManager txn)
          (txnConnConfig txn)
          (txnVersionCache txn)
          (txnCorrelationId txn)
          (txnBootstrapBroker txn)
          (txnClientId txn)
          (unTransactionalId (txnTransactionalId txn))
      case found of
        Right c -> do
          writeIORef (txnCoordinator txn) (Just c)
          pure c
        Left _ -> pure coordinator
    retriable e = case e of
      TC.ConcurrentTransactions _ -> True
      TC.CoordinatorLoadInProgress _ -> True
      TC.CoordinatorNotAvailable _ -> True
      TC.NotCoordinator _ -> True
      _ -> False


-- | @EndTxn@ for the open transaction, with 'withCoordinatorRetry'.
endTransactionWithRetry
  :: Transaction
  -> Text
  -> Int64
  -> Int16
  -> Bool
  -- ^ commit (True) or abort (False)
  -> IO (Either TC.TransactionCoordinatorError ())
endTransactionWithRetry txn transactionalId pid epoch commit =
  withCoordinatorRetry txn $ \coordinator ->
    TC.endTransaction
      (txnConnectionManager txn)
      (txnConnConfig txn)
      (txnVersionCache txn)
      (txnCorrelationId txn)
      (txnClientId txn)
      coordinator
      transactionalId
      pid
      epoch
      commit


{- | Record a failed @EndTxn@. Fencing errors make the producer
permanently 'Fenced' (another instance owns the transactional id);
anything retriable leaves the transaction in @back@ so the caller
can retry or abort; the rest is a fatal 'Error'.
-}
failEnd :: Transaction -> TransactionState -> TC.TransactionCoordinatorError -> IO (Either TransactionError ())
failEnd txn back err = do
  let !e = coordinatorErrorToTxn err
  writeIORef (txnState txn) $ case e of
    ProducerFenced _ -> Fenced
    CoordinatorNotAvailable _ -> back
    _ -> Error (T.pack (show err))
  pure (Left e)


{- | Execute an action within a transaction, automatically
committing on success and aborting on exception. The action
runs in the caller's monad; commit / abort always run in IO
(the broker calls don't need 'MonadIO' polymorphism inside
the bracket since they're internal to the lifecycle).
-}
withTransaction
  :: MonadUnliftIO m
  => Transaction
  -> m a
  -> m (Either TransactionError a)
withTransaction txn action =
  withRunInIO $ \run -> do
    beginResult <- beginTransaction txn
    case beginResult of
      Left err -> pure (Left err)
      Right () -> do
        result <- try (run action)
        case result of
          Right value -> do
            commitResult <- commitTransaction txn
            case commitResult of
              Left err -> pure (Left err)
              Right () -> pure (Right value)
          Left (ex :: SomeException) -> do
            _ <- abortTransaction txn
            pure (Left (TransactionAborted (T.pack (show ex))))
{-# INLINEABLE withTransaction #-}
{-# SPECIALIZE withTransaction :: Transaction -> IO a -> IO (Either TransactionError a) #-}


{- | Send a record within a transaction
This tracks which partitions are involved in the transaction
-}
sendInTransaction :: MonadIO m => Transaction -> TopicPartition -> m (Either TransactionError ())
sendInTransaction txn tp = liftIO $ do
  state <- getTransactionState txn
  case state of
    InTransaction -> do
      -- Add partition to transaction tracking
      atomically $ do
        partitions <- readTVar (txnPartitions txn)
        writeTVar (txnPartitions txn) (HashSet.insert tp partitions)

        -- Get and increment sequence number for this partition
        sequences <- readTVar (txnSequenceNumbers txn)
        let currentSeq = HashMap.lookupDefault 0 tp sequences
            nextSeq = currentSeq + 1
        writeTVar (txnSequenceNumbers txn) (HashMap.insert tp nextSeq sequences)

      -- Note: the actual @ProduceRequest@ goes through
      -- 'Kafka.Client.Producer.sendMessage' on the same
      -- transactional producer, which stamps the producer-id /
      -- epoch / sequence onto every batch via 'BA.batchProducerId'
      -- / 'BA.batchProducerEpoch' / 'BA.batchBaseSequence'. This
      -- function's role is to register the partition with the
      -- coordinator so it's included in the commit envelope; the
      -- bookkeeping above (txnPartitions + txnSequenceNumbers)
      -- and the upstream AddPartitionsToTxn call (issued lazily
      -- by the producer the first time a record lands on a new
      -- partition) cover that.
      return $ Right ()
    _ -> return $ Left $ TransactionNotInProgress "Must be in a transaction to send records"


{- | Add a partition to the open transaction with the coordinator
(@AddPartitionsToTxn@), once per partition per transaction, before
the producer's first record to it. A concurrent first send to the
same partition waits for the registration in flight rather than
producing ahead of it; a failed registration is forgotten, so the
next send tries again.
-}
registerPartition
  :: Transaction
  -> Int64
  -- ^ producer id
  -> Int16
  -- ^ producer epoch
  -> TopicPartition
  -> IO (Either String ())
registerPartition txn pid epoch tp = do
  fresh <- atomically $ do
    added <- readTVar (txnPartitions txn)
    if HashSet.member tp added
      then pure False
      else do
        pending <- readTVar (txnPendingPartitions txn)
        when (HashSet.member tp pending) retry
        writeTVar (txnPendingPartitions txn) (HashSet.insert tp pending)
        pure True
  if not fresh
    then pure (Right ())
    else do
      r <-
        either (\e -> Left ("AddPartitionsToTxn failed: " <> show e)) Right
          <$> withCoordinatorRetry txn (\coord ->
            TC.addPartitionsToTxn
              (txnConnectionManager txn)
              (txnConnConfig txn)
              (txnVersionCache txn)
              (txnCorrelationId txn)
              (txnClientId txn)
              coord
              (unTransactionalId (txnTransactionalId txn))
              pid
              epoch
              [tp])
      case r of
        Right () -> writeIORef (txnStarted txn) True
        Left _ -> pure ()
      atomically $ do
        modifyTVar' (txnPendingPartitions txn) (HashSet.delete tp)
        case r of
          Right () -> modifyTVar' (txnPartitions txn) (HashSet.insert tp)
          Left _ -> pure ()
      pure r


{- | Commit consumer-group offsets as part of the open transaction
(the JVM's @KafkaProducer.sendOffsetsToTransaction@, KIP-98 /
KIP-447): @AddOffsetsToTxn@ to the transaction coordinator, then
@TxnOffsetCommit@ to the group coordinator. The offsets become
visible only if the transaction commits, so consumed input and
produced output move together.

The member's generation and member id make the group coordinator
reject the commit from a member a rebalance has already replaced.
-}
sendOffsetsToTransaction
  :: MonadIO m
  => Transaction
  -> BrokerAddress
  -- ^ The consumer group's coordinator
  -> TC.TxnGroupMember
  -> [(TopicPartition, Int64)]
  -- ^ Next offset to consume, per partition
  -> m (Either TransactionError ())
sendOffsetsToTransaction _ _ _ [] = liftIO (pure (Right ()))
sendOffsetsToTransaction txn groupCoord member offsets = liftIO $ do
  state <- getTransactionState txn
  case state of
    InTransaction -> do
      coordM <- readIORef (txnCoordinator txn)
      pidM <- readIORef (txnProducerId txn)
      epochM <- readIORef (txnProducerEpoch txn)
      case (coordM, pidM, epochM) of
        (Just _, Just (ProducerId pid), Just (ProducerEpoch epoch)) -> do
          let transactionalId = unTransactionalId (txnTransactionalId txn)
          addR <- withCoordinatorRetry txn $ \coord ->
            TC.addOffsetsToTxn
              (txnConnectionManager txn)
              (txnConnConfig txn)
              (txnVersionCache txn)
              (txnCorrelationId txn)
              (txnClientId txn)
              coord
              transactionalId
              pid
              epoch
              (TC.tgmGroupId member)
          case addR of
            Left e -> pure (Left (coordinatorErrorToTxn e))
            Right () -> do
              writeIORef (txnStarted txn) True
              r <-
                TC.txnOffsetCommitWith
                  (txnConnectionManager txn)
                  (txnConnConfig txn)
                  (txnVersionCache txn)
                  (txnCorrelationId txn)
                  (txnClientId txn)
                  groupCoord
                  transactionalId
                  member
                  pid
                  epoch
                  offsets
              pure (either (Left . coordinatorErrorToTxn) Right r)
        _ -> pure (Left (TransactionNotInitialized "Missing producer ID or coordinator"))
    _ -> pure (Left (TransactionNotInProgress "Must be in a transaction to commit offsets"))


----------------------------------------------------------------------
-- Additional transactional ergonomics
--
-- Previously lived in @Kafka.Client.ProducerExtras@.
----------------------------------------------------------------------

{- | Optionally derive a @transactional.id@ from a base prefix +
a per-process suffix. Keeps the user-facing API symmetric with
the JVM client, where @transactional.id@ may be left blank and
the client picks one based on host/process ids.
-}
transactionalIdOptional
  :: Maybe Text
  -- ^ explicit override (matches the JVM client's @transactional.id@ property)
  -> Text
  -- ^ application prefix
  -> Text
  -- ^ per-process suffix (e.g. host\@pid)
  -> Text
transactionalIdOptional (Just t) _ _ = t
transactionalIdOptional Nothing prefix sf = prefix <> "-" <> sf


{- | Recovery action for a non-zero error code returned by the
transaction coordinator. Mirrors the abort / retry / fatal
partitioning the JVM client uses.
-}
data TxnErrorRecovery
  = -- | Abort the current transaction and let the producer continue.
    TxnRecoverByAbort
  | -- | Re-issue the same operation after a short backoff.
    TxnRecoverByRetry
  | -- | The producer must close.
    TxnRecoverFatal
  deriving stock (Eq, Show)


{- | Classify a Kafka error code into a 'TxnErrorRecovery' bucket.
Backed by 'Kafka.Client.RetryClassifier.classify' so the
mapping stays in sync with the producer's retry logic.
-}
classifyTxnError :: Int16 -> TxnErrorRecovery
classifyTxnError code = case RC.classify code of
  RC.ECNoError -> TxnRecoverByRetry
  RC.ECRetriable -> TxnRecoverByRetry
  RC.ECAbortable -> TxnRecoverByAbort
  RC.ECFatal -> TxnRecoverFatal


{- | Deadline supplied to @commitTransaction@ / @abortTransaction@.
Callers can bound the wait so a misbehaving coordinator can't
pin the producer open during shutdown.
-}
data TxnDeadline
  = -- | Fall back to the producer's @transaction.timeout.ms@.
    TxnUseProducerDefault
  | -- | Hard upper bound in ms.
    TxnDeadlineMs !Int
  deriving stock (Eq, Show)


effectiveTxnDeadlineMs
  :: Int64
  -- ^ now (ms)
  -> Int
  -- ^ producer's @transaction.timeout.ms@
  -> TxnDeadline
  -> Int64
effectiveTxnDeadlineMs now defaultMs = \case
  TxnUseProducerDefault -> now + fromIntegral defaultMs
  TxnDeadlineMs ms -> now + fromIntegral ms


----------------------------------------------------------------------
-- SPECIALIZE pragmas for the IO hot path
--
-- See "Kafka.Client.Producer" for the rationale.
----------------------------------------------------------------------

{-# INLINEABLE createTransaction #-}
{-# SPECIALIZE createTransaction :: TransactionalId -> ConnectionManager -> ConnectionConfig -> ApiVersionCache -> Text -> BrokerAddress -> Int32 -> IO Transaction #-}


{-# INLINEABLE initTransactions #-}
{-# SPECIALIZE initTransactions :: Transaction -> IO (Either TransactionError ()) #-}


{-# INLINEABLE beginTransaction #-}
{-# SPECIALIZE beginTransaction :: Transaction -> IO (Either TransactionError ()) #-}


{-# INLINEABLE commitTransaction #-}
{-# SPECIALIZE commitTransaction :: Transaction -> IO (Either TransactionError ()) #-}


{-# INLINEABLE abortTransaction #-}
{-# SPECIALIZE abortTransaction :: Transaction -> IO (Either TransactionError ()) #-}


{-# INLINEABLE sendInTransaction #-}
{-# SPECIALIZE sendInTransaction :: Transaction -> TopicPartition -> IO (Either TransactionError ()) #-}
