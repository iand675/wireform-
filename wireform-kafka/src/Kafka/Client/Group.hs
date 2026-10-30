{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Kafka.Client.Group
Description : Receive records, one handler per record, with offsets managed for you

Reading from Kafka is harder than writing because of three concerns
that have nothing to do with your business logic: joining a consumer
group, riding out rebalances, and committing offsets so that crash
recovery picks up where you left off. This module folds all three
into a single bracket.

= The 30-second tour

Use 'runConsumer' when you want \"call this handler once per record,
forever\":

@
import qualified Kafka.Client.Group as Kafka

main :: IO ()
main =
  Kafka.runConsumer
    Kafka.'defaultGroupConfig'
      { Kafka.'bootstrapBrokers' = [\"broker-1:9092\"]
      , Kafka.'groupId'          = \"my-service\"
      , Kafka.'topics'           = [\"events\"]
      }
    $ \\record -> do
        putStrLn $ \"got \" <> show record.key <> \" -> \" <> show record.value
@

That is the whole API for the common case. The bracket:

  1. opens a connection and joins the group as a member of @my-service@,
  2. asks the group coordinator for partition assignments,
  3. resumes from the last committed offsets,
  4. invokes @handler@ once per record,
  5. commits after each record (or batch, see 'runBatchedConsumer'),
  6. leaves the group cleanly on a normal exit or exception.

= Batches and custom loops

For throughput, use 'runBatchedConsumer' — your handler receives a
whole 'V.Vector' of records and commits run once per batch. For
custom control flow, drop to 'withGroupConsumer' and call 'pollOnce'
+ 'commit' from your own loop.

= Error handling

Each handler invocation runs inside a 'SomeException' catch. Pick a
policy by setting 'onError':

  * 'LogAndRaise' (default) — print and re-raise, terminating the loop.
  * 'SkipRecord' — log and continue with the next record.
  * 'StopLoop' — log and exit the loop cleanly.
  * 'CustomError' — your own predicate.

= Offsets

By default we commit synchronously after each handler call (i.e.
after each batch in the batched API). This trades a little
throughput for "at least once with the smallest possible duplicate
window on a crash". Set 'commitMode' to 'CommitAsync' for higher
throughput, or 'CommitManual' to take ownership yourself.
-}
module Kafka.Client.Group (
  -- * Run a consumer loop
  runConsumer,
  runBatchedConsumer,

  -- * Configuration
  GroupConfig (..),
  defaultGroupConfig,
  ErrorPolicy (..),
  CommitMode (..),

  -- * Bracket-style API for custom loops
  GroupConsumer,
  withGroupConsumer,
  pollOnce,
  commit,
  currentAssignment,
  underlyingConsumer,

  -- * Convenience re-exports
  C.ConsumerRecord (..),
  C.TopicPartition (..),
  C.AssignmentStrategy (..),
  C.OffsetResetStrategy (..),

  -- * Auth helpers (re-exported)
  SASL.SaslConfig (..),
  Scram.ScramAlgo (..),
  Iam.AwsCredentials (..),
  Iam.AwsCredentialsProvider (..),
  OAuth.OAuthToken (..),
  OAuth.OAuthTokenProvider (..),
  OAuth.OAuthBearerExtensions (..),
  OAuth.defaultOAuthBearerExtensions,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (
  SomeException,
  bracket,
  catch,
  throwIO,
 )
import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Kafka.Telemetry.Log qualified as KLog
import Kafka.Client.Consumer qualified as C
import Kafka.Errors qualified as Errors
import Kafka.Network.Auth.AwsMskIam qualified as Iam
import Kafka.Network.Auth.OAuthBearer qualified as OAuth
import Kafka.Network.Auth.SASL qualified as SASL
import Kafka.Network.Auth.Scram qualified as Scram
import Kafka.Network.Connection qualified as Conn
import Wireform.Network.TLS.Config qualified as TLS


-- | What to do when the user-provided handler throws.
data ErrorPolicy
  = {- | Print the exception to @stderr@ and re-raise it; the loop
    exits and 'runConsumer' rethrows so the caller can decide
    whether to restart.
    -}
    LogAndRaise
  | {- | Print the exception to @stderr@ and keep going on the next
    record. Offsets for the failing record are still committed
    (it counts as "delivered"); use a dead-letter queue or
    in-handler retry if you need different semantics.
    -}
    SkipRecord
  | {- | Print the exception to @stderr@ and exit the loop cleanly
    (does not re-raise).
    -}
    StopLoop
  | {- | Run a custom handler. Return 'True' to keep going, 'False'
    to stop the loop.
    -}
    CustomError !(SomeException -> IO Bool)


-- | When to commit offsets back to the broker.
data CommitMode
  = {- | After every successful handler invocation (or batch). Slowest
    but the safest default — guarantees at-least-once with the
    smallest possible duplicate window on crash.
    -}
    CommitSync
  | {- | Fire-and-forget after every successful handler invocation.
    Higher throughput; on a hard crash you may reprocess up to a
    commit-interval's worth of records.
    -}
    CommitAsync
  | {- | Don't commit at all from the loop. The caller owns offset
    commits via 'commit' or via the configured auto-commit timer.
    -}
    CommitManual


{- | What 'runConsumer' / 'withGroupConsumer' needs to know. Construct
with @'defaultGroupConfig' { 'bootstrapBrokers' = .., 'groupId' = .., 'topics' = .. }@.
-}
data GroupConfig = GroupConfig
  { bootstrapBrokers :: ![Text]
  -- ^ The Kafka cluster's bootstrap servers, e.g. @[\"broker-1:9092\"]@.
  , groupId :: !Text
  {- ^ Consumer group id. Members of the same group share the
  partitions of the subscribed topics; members of different
  groups each get their own copy of the stream.
  -}
  , topics :: ![Text]
  {- ^ Topics to subscribe to. Non-empty for 'runConsumer' /
  'runBatchedConsumer'; 'withGroupConsumer' will skip the
  subscription if empty so you can call 'C.subscribe' yourself.
  -}
  , clientId :: !Text
  {- ^ Identifier sent to the broker on every request. Useful for
  broker-side audit logs.
  -}
  , sessionTimeoutMs :: !Int
  {- ^ Group session timeout — the broker fences this member if it
  doesn't see a heartbeat for this long.
  -}
  , maxPollIntervalMs :: !Int
  {- ^ Maximum gap between successive 'pollOnce' calls before the
  broker considers this member dead.
  -}
  , maxPollRecords :: !Int
  -- ^ Cap on records returned per 'pollOnce'.
  , pollTimeoutMs :: !Int
  {- ^ How long a single 'C.poll' is allowed to block server-side
  when there are no records yet. Defaults to 1000 ms.
  -}
  , autoOffsetReset :: !C.OffsetResetStrategy
  {- ^ Where to start a brand-new group — 'C.Earliest' (replay
  from the start), 'C.Latest' (only see future records), or
  'C.None' (refuse to consume without a committed offset).
  -}
  , assignmentStrategy :: !C.AssignmentStrategy
  -- ^ How the group leader distributes partitions among members.
  , commitMode :: !CommitMode
  -- ^ When to write offsets back to the broker.
  , onError :: !ErrorPolicy
  -- ^ What happens when your handler throws.
  , onPollError :: !(String -> IO ())
  {- ^ What to do when the underlying poll itself fails (network
  blip, broker rejection, etc.). Default: log a warning
  ("Kafka.Telemetry.Log") and back off briefly. Returning normally signals
  "retry"; throw to stop the loop.
  -}
  , closeTimeoutMs :: !Int
  -- ^ How long to wait for in-flight work to drain on shutdown.
  , useTls :: !Bool
  {- ^ Whether to wrap the broker connection in TLS. Defaults to
  'False' for local development; flip to 'True' for any
  production / cloud broker. AWS MSK IAM (and Confluent
  Cloud's PLAIN \/ OAUTHBEARER) /require/ TLS.
  -}
  , tlsParams :: !(Maybe TLS.TlsClientConfig)
  {- ^ Custom TLS parameters. When 'Nothing' but 'useTls' is
  'True' we fall back to 'Conn.defaultTlsSettings' against the
  first bootstrap broker hostname (system trust store, strong
  ciphers, hostname verification on).
  -}
  , sasl :: !(Maybe SASL.SaslConfig)
  {- ^ SASL mechanism to use after the connection is up. 'Nothing'
  means \"no SASL\" (i.e. the broker is configured for
  PLAINTEXT or SSL-only auth).
  -}
  }


{- | Sensible defaults: localhost broker, no topics yet (the caller is
expected to fill at least 'groupId' and 'topics'), 10-second
session timeout, sync commits, plaintext.
-}
defaultGroupConfig :: GroupConfig
defaultGroupConfig =
  GroupConfig
    { bootstrapBrokers = ["localhost:9092"]
    , groupId = ""
    , topics = []
    , clientId = "wireform-kafka"
    , sessionTimeoutMs = 10000
    , maxPollIntervalMs = 300000
    , maxPollRecords = 500
    , pollTimeoutMs = 1000
    , autoOffsetReset = C.Latest
    , assignmentStrategy = C.RangeAssignment
    , commitMode = CommitSync
    , onError = LogAndRaise
    , onPollError = \msg -> do
        KLog.logWarn KLog.kafkaInstrumentationLibrary "consumer group: poll failed" [KLog.attrText "exception.message" (T.pack msg)]
        threadDelay 250000 -- back off 250ms
    , closeTimeoutMs = 30000
    , useTls = False
    , tlsParams = Nothing
    , sasl = Nothing
    }


{- | Opaque handle for the bracket-style API. Wraps the raw
'C.Consumer' along with the user's 'GroupConfig' so the helper
functions can read the right policies.
-}
data GroupConsumer = GroupConsumer
  { groupConsumer :: !C.Consumer
  , groupConfig :: !GroupConfig
  }


{- | Get the underlying low-level consumer for advanced use cases
(manual seeking, pausing partitions, etc.).
-}
underlyingConsumer :: GroupConsumer -> C.Consumer
underlyingConsumer = groupConsumer


{- | Bracket: open the consumer, join the group, run the body, leave
the group + close on the way out (commits a final batch on
'CommitSync', cancels the heartbeat thread, closes connections).

Throws an 'IOError' if the broker can't be reached or the
subscription fails — the caller can catch and retry the whole
bracket if needed.
-}
withGroupConsumer
  :: MonadUnliftIO m
  => GroupConfig
  -> (GroupConsumer -> m a)
  -> m a
withGroupConsumer cfg@GroupConfig {..} body = do
  validateConfig cfg
  withRunInIO $ \run ->
    bracket open close $ \gc -> do
      subResult <- C.subscribe (groupConsumer gc) topics
      case subResult of
        Left err ->
          throwIO $
            Errors.connectError
              (T.pack ("wireform-kafka: subscribe failed: " <> err))
        Right () -> run (body gc)
  where
    open = do
      let connBase =
            Conn.defaultConnectionConfig
              { Conn.connUseTls = useTls
              , Conn.connTlsSettings = case tlsParams of
                  Just p -> Just p
                  Nothing -> case bootstrapBrokers of
                    -- Fall back to a sensible default keyed off the
                    -- first bootstrap broker hostname. We strip the
                    -- ":port" if present.
                    (b : _)
                      | useTls ->
                          let hostOnly = T.unpack (T.takeWhile (/= ':') b)
                          in Just (Conn.defaultTlsSettings hostOnly)
                    _ -> Nothing
              , Conn.connSasl = sasl
              , Conn.connClientId = clientId
              }
          ccfg =
            C.defaultConsumerConfig
              { C.consumerClientId = clientId
              , C.consumerGroupId = groupId
              , C.consumerSessionTimeoutMs = sessionTimeoutMs
              , C.consumerMaxPollIntervalMs = maxPollIntervalMs
              , C.consumerMaxPollRecords = maxPollRecords
              , C.consumerAutoOffsetReset = autoOffsetReset
              , C.consumerAssignmentStrategy = assignmentStrategy
              , C.consumerAutoCommit = case commitMode of
                  CommitManual -> True -- let the broker-side timer drive it
                  _ -> False -- the loop owns commits
              , C.consumerConnectionConfig = connBase
              }
      r <- C.createConsumer bootstrapBrokers groupId ccfg
      case r of
        Left err ->
          throwIO $
            Errors.connectError
              (T.pack ("wireform-kafka: createConsumer failed: " <> err))
        Right con -> pure GroupConsumer {groupConsumer = con, groupConfig = cfg}

    close gc = C.closeConsumerWithTimeout (groupConsumer gc) ((groupConfig gc).closeTimeoutMs)


-- | A single 'C.poll' against the underlying consumer.
pollOnce :: MonadIO m => GroupConsumer -> m [C.ConsumerRecord]
pollOnce GroupConsumer {..} = liftIO $ do
  r <- C.poll groupConsumer (pollTimeoutMs groupConfig)
  case r of
    Right xs -> pure xs
    Left err -> do
      onPollError groupConfig err
      pure []


{- | Commit current offsets according to the configured 'CommitMode'.
'CommitManual' is a no-op (the caller is expected to drive their
own commits).
-}
commit :: MonadIO m => GroupConsumer -> m ()
commit gc@GroupConsumer {groupConfig = GroupConfig {..}} = liftIO $
  case commitMode of
    CommitManual -> pure ()
    CommitAsync -> do
      r <- C.commitAsync (groupConsumer gc)
      case r of
        Right () -> pure ()
        Left err -> onPollError ("commitAsync: " <> err)
    CommitSync -> do
      r <- C.commitSync (groupConsumer gc)
      case r of
        Right () -> pure ()
        Left err -> onPollError ("commitSync: " <> err)


{- | Currently-assigned partitions for this consumer. Useful for
rebalance-aware logging.
-}
currentAssignment :: MonadIO m => GroupConsumer -> m [C.TopicPartition]
currentAssignment GroupConsumer {..} = C.assignment groupConsumer


{- | Run a consumer loop: call the handler once per record, commit
after each (per the configured 'CommitMode'), back off briefly when
the broker hands us nothing, exit cleanly on graceful close.

The bracket guarantees we leave the group + close connections + run
a final commit (in 'CommitSync' mode) when the body returns or
throws.
-}
runConsumer
  :: MonadUnliftIO m
  => GroupConfig
  -> (C.ConsumerRecord -> m ())
  -> m ()
runConsumer cfg handler =
  withRunInIO $ \run -> do
    let ioHandler rec = run (handler rec)
    withGroupConsumer cfg $ \gc -> do
      keepGoing <- newIORef True
      loop ioHandler gc keepGoing
  where
    loop hio gc keepGoing = do
      go <- readIORef keepGoing
      when go $ do
        records <- pollOnce gc
        case records of
          [] -> do
            -- Nothing right now; tiny back-off so we don't spin.
            threadDelay 50000 -- 50 ms
            loop hio gc keepGoing
          rs -> do
            forM_ rs $ \rec -> do
              keep <- runHandler (groupConfig gc) hio rec
              unless keep $ writeIORef keepGoing False
            commit gc
            loop hio gc keepGoing
{-# INLINEABLE runConsumer #-}
{-# SPECIALIZE runConsumer :: GroupConfig -> (C.ConsumerRecord -> IO ()) -> IO () #-}


{- | Same as 'runConsumer' but the handler receives a whole batch at a
time (whatever 'C.poll' returned). Use this for higher-throughput
workloads where per-record work amortises well.

The whole batch is treated as one commit unit: if the handler
throws, no offsets are committed for that batch. The 'ErrorPolicy'
still controls whether the loop stops.
-}
runBatchedConsumer
  :: MonadUnliftIO m
  => GroupConfig
  -> (V.Vector C.ConsumerRecord -> m ())
  -> m ()
runBatchedConsumer cfg handler =
  withRunInIO $ \run -> do
    let ioHandler vec = run (handler vec)
    withGroupConsumer cfg $ \gc -> do
      keepGoing <- newIORef True
      loop ioHandler gc keepGoing
  where
    loop hio gc keepGoing = do
      go <- readIORef keepGoing
      when go $ do
        records <- pollOnce gc
        case records of
          [] -> do
            threadDelay 50000
            loop hio gc keepGoing
          rs -> do
            outcome <-
              (Right <$> hio (V.fromList rs))
                `catch` \(e :: SomeException) -> pure (Left e)
            case outcome of
              Right () -> commit gc >> loop hio gc keepGoing
              Left e -> do
                keep <- handleError (groupConfig gc) e
                if keep
                  then loop hio gc keepGoing
                  else pure ()
{-# INLINEABLE runBatchedConsumer #-}
{-# SPECIALIZE runBatchedConsumer :: GroupConfig -> (V.Vector C.ConsumerRecord -> IO ()) -> IO () #-}


runHandler
  :: GroupConfig
  -> (C.ConsumerRecord -> IO ())
  -> C.ConsumerRecord
  -> IO Bool
runHandler GroupConfig {onError = policy} h rec =
  (h rec >> pure True) `catch` \(e :: SomeException) ->
    handlePolicy policy e


handleError :: GroupConfig -> SomeException -> IO Bool
handleError GroupConfig {onError = policy} = handlePolicy policy


handlePolicy :: ErrorPolicy -> SomeException -> IO Bool
handlePolicy policy e = case policy of
  LogAndRaise -> do
    logErr e
    throwIO e
  SkipRecord -> do
    logErr e
    pure True
  StopLoop -> do
    logErr e
    pure False
  CustomError k -> k e
  where
    logErr ex =
      KLog.logError KLog.kafkaInstrumentationLibrary "consumer group: record handler threw" [KLog.attrShow "exception.message" ex]


-- | Cheap sanity checks before we even open a network connection.
validateConfig :: MonadIO m => GroupConfig -> m ()
validateConfig GroupConfig {..} = liftIO $ do
  let cfgErr msg = throwIO $ Errors.configurationError [msg]
  when (null bootstrapBrokers) $
    cfgErr "bootstrapBrokers must be non-empty"
  when (T.null groupId) $
    cfgErr "groupId must be non-empty"
  when (null topics) $
    cfgErr "topics must be non-empty"


----------------------------------------------------------------------
-- SPECIALIZE pragmas for the IO hot path
--
-- See "Kafka.Client.Producer" for the rationale.
----------------------------------------------------------------------

{-# INLINEABLE withGroupConsumer #-}
{-# SPECIALIZE withGroupConsumer :: GroupConfig -> (GroupConsumer -> IO a) -> IO a #-}


{-# INLINEABLE pollOnce #-}
{-# SPECIALIZE pollOnce :: GroupConsumer -> IO [C.ConsumerRecord] #-}


{-# INLINEABLE commit #-}
{-# SPECIALIZE commit :: GroupConsumer -> IO () #-}


{-# INLINEABLE currentAssignment #-}
{-# SPECIALIZE currentAssignment :: GroupConsumer -> IO [C.TopicPartition] #-}


{-# INLINEABLE validateConfig #-}
{-# SPECIALIZE validateConfig :: GroupConfig -> IO () #-}
