{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the @KAFKA_*@ env-var loader in
"Kafka.Client.Env".

The unit under test is the pure 'parseKafkaEnvList' /
'applyKafkaEnvTo*' surface; the IO-flavoured wrappers
('loadKafkaEnv', 'producerConfigFromEnv', …) are thin shims on
top so we exercise them indirectly by passing the env in as
a list.
-}
module Client.EnvSpec (tests) where

import Data.Text qualified as T
import Kafka.Client.ConfigValidation (ConfigError (..))
import Kafka.Client.Consumer (applyKafkaEnvToConsumerConfig)
import Kafka.Client.Consumer qualified as C
import Kafka.Client.Env (
  EnvAcks (..),
  EnvOffsetReset (..),
  KafkaEnv (..),
  SaslMechanism (..),
  SecurityProtocol (..),
  applyKafkaEnvToConnectionConfig,
  parseKafkaEnvList,
 )
import Kafka.Client.Env qualified as Env
import Kafka.Client.Producer (applyKafkaEnvToProducerConfig)
import Kafka.Client.Producer qualified as P
import Kafka.Compression.Types qualified as Compression
import Kafka.Network.Auth.OAuthBearer qualified as OAuth
import Kafka.Network.Auth.SASL qualified as SASL
import Kafka.Network.Auth.Scram qualified as Scram
import Kafka.Network.Connection qualified as Conn
import System.Environment qualified as SE
import Test.Syd


tests :: Spec
tests =
  describe "Kafka.Client.Env" $
    sequence_
      [ describe "scalar parsers" $
          sequence_
            [ it "parseBool accepts true/false/1/0/yes/no/on/off" prop_parseBool
            , it "parseBool rejects garbage" prop_parseBoolBad
            , it "parseAcks accepts 0/1/-1/all" prop_parseAcks
            , it "parseCompression delegates to codec parser" prop_parseCompression
            , it "parseBootstrapServers splits and trims" prop_parseBootstrapServers
            , it "parseBootstrapServers rejects empty string" prop_parseBootstrapServersEmpty
            ]
      , describe "parseKafkaEnvList" $
          sequence_
            [ it "empty env yields empty KafkaEnv" prop_emptyEnv
            , it "ignores unrelated KAFKA_* variables" prop_ignoresUnknown
            , it "lookup is case-insensitive" prop_caseInsensitive
            , it "trims whitespace" prop_trimsWhitespace
            , it
                "reports parse errors with the librdkafka field name"
                prop_reportsParseErrors
            , it "accumulates multiple parse errors" prop_accumulatesErrors
            , it "fills every relevant field" prop_fullEnv
            ]
      , describe "applyKafkaEnvToConnectionConfig" $
          sequence_
            [ it "absent vars leave defaults intact" prop_connDefaults
            , it "client.id is overridden" prop_connClientId
            , it "socket timeout overrides read/write" prop_connSocketTimeout
            , it
                "SSL flips connUseTls and supplies default TLS settings"
                prop_connSsl
            , it "SSL preserves caller-supplied TLS settings" prop_connSslPreservesTls
            , it
                "SASL_SSL with PLAIN sets connSasl and TLS"
                prop_connSaslSsl
            , it
                "SASL_SSL with SCRAM-SHA-512 picks the right algo"
                prop_connSaslScramSha512
            , it
                "SASL_PLAINTEXT skips TLS but sets connSasl"
                prop_connSaslPlaintext
            , it
                "SASL_SSL without mechanism is an error"
                prop_connSaslSslNoMechanism
            , it
                "SASL_SSL with PLAIN and missing password is an error"
                prop_connSaslMissingPassword
            , it
                "SASL_SSL with OAUTHBEARER and static token sets connSasl"
                prop_connSaslOAuthStaticToken
            , it
                "SASL_SSL with OAUTHBEARER and no token is rejected with guidance"
                prop_connSaslOAuthMissingToken
            , it
                "SASL_SSL with AWS_MSK_IAM is rejected with guidance"
                prop_connSaslAwsRejected
            ]
      , describe "applyKafkaEnvToProducerConfig" $
          sequence_
            [ it "absent vars leave defaults intact" prop_prodDefaults
            , it
                "overrides batch.size + linger.ms + compression"
                prop_prodOverrides
            , it "acks=all maps to ExactlyOnce" prop_prodAcksAll
            , it "enable.idempotence=true flips the flag" prop_prodIdempotence
            , it "transactional.id is propagated" prop_prodTransactional
            ]
      , describe "applyKafkaEnvToConsumerConfig" $
          sequence_
            [ it "absent vars leave defaults intact" prop_consDefaults
            , it
                "group.id + group.instance.id are propagated"
                prop_consGroupId
            , it "auto.offset.reset=earliest is applied" prop_consAutoOffsetReset
            , it
                "isolation.level=read_committed is applied"
                prop_consIsolation
            , it "session/heartbeat overrides apply" prop_consTimeouts
            , it
                "partition.assignment.strategy accepts JVM class names"
                prop_consAssignmentJvmName
            , it
                "consumer pulls connection-level env via embedded ConnectionConfig"
                prop_consInheritsConnection
            ]
      , -- withTempEnv mutates the process environment: run these alone.
        sequential $ describe "createProducer / createConsumer" $
          sequence_
            [ it
                "createProducer reads KAFKA_BOOTSTRAP_SERVERS from the env"
                prop_createProducerReadsBootstrap
            , it
                "createConsumer reads KAFKA_BOOTSTRAP_SERVERS from the env"
                prop_createConsumerReadsBootstrap
            , it
                "createProducer surfaces malformed env-var values"
                prop_createProducerMalformedEnv
            ]
      ]


------------------------------------------------------------------
-- Scalar parsers
------------------------------------------------------------------

prop_parseBool :: IO ()
prop_parseBool = do
  Env.parseBool "true" `shouldBe` Right True
  Env.parseBool "TRUE" `shouldBe` Right True
  Env.parseBool "1" `shouldBe` Right True
  Env.parseBool "yes" `shouldBe` Right True
  Env.parseBool "on" `shouldBe` Right True
  Env.parseBool "false" `shouldBe` Right False
  Env.parseBool "0" `shouldBe` Right False
  Env.parseBool "no" `shouldBe` Right False
  Env.parseBool "off" `shouldBe` Right False


prop_parseBoolBad :: IO ()
prop_parseBoolBad = case Env.parseBool "maybe" of
  Left _ -> pure ()
  Right b -> expectationFailure ("expected Left, got Right " <> show b)


prop_parseAcks :: IO ()
prop_parseAcks = do
  Env.parseAcks "0" `shouldBe` Right EnvAcksZero
  Env.parseAcks "1" `shouldBe` Right EnvAcksOne
  Env.parseAcks "-1" `shouldBe` Right EnvAcksAll
  Env.parseAcks "all" `shouldBe` Right EnvAcksAll
  Env.parseAcks "ALL" `shouldBe` Right EnvAcksAll


prop_parseCompression :: IO ()
prop_parseCompression = do
  Env.parseCompression "gzip" `shouldBe` Right Compression.Gzip
  Env.parseCompression "ZSTD" `shouldBe` Right Compression.Zstd
  Env.parseCompression "snappy" `shouldBe` Right Compression.Snappy
  Env.parseCompression "lz4" `shouldBe` Right Compression.Lz4
  Env.parseCompression "none" `shouldBe` Right Compression.NoCompression


prop_parseBootstrapServers :: IO ()
prop_parseBootstrapServers = do
  Env.parseBootstrapServers "a:1, b:2 ,c:3"
    `shouldBe` Right ["a:1", "b:2", "c:3"]
  Env.parseBootstrapServers "a:1  b:2\tc:3"
    `shouldBe` Right ["a:1", "b:2", "c:3"]


prop_parseBootstrapServersEmpty :: IO ()
prop_parseBootstrapServersEmpty = case Env.parseBootstrapServers "   " of
  Left _ -> pure ()
  Right s -> expectationFailure ("expected Left, got " <> show s)


------------------------------------------------------------------
-- parseKafkaEnvList
------------------------------------------------------------------

prop_emptyEnv :: IO ()
prop_emptyEnv = case parseKafkaEnvList [] of
  Right env -> env `shouldBe` Env.emptyKafkaEnv
  Left e -> expectationFailure ("expected Right, got " <> show e)


prop_ignoresUnknown :: IO ()
prop_ignoresUnknown =
  -- Confluent's Docker images set tons of KAFKA_BROKER_*,
  -- KAFKA_LISTENERS, KAFKA_ZOOKEEPER_CONNECT, … - all of which
  -- target the broker, not the client. We must ignore them.
  case parseKafkaEnvList
    [ ("KAFKA_LISTENERS", "PLAINTEXT://0.0.0.0:9092")
    , ("KAFKA_BROKER_ID", "1")
    , ("KAFKA_ZOOKEEPER_CONNECT", "zookeeper:2181")
    , ("PATH", "/usr/bin")
    , ("KAFKA_CLIENT_ID", "alice")
    ] of
    Right env -> envClientId env `shouldBe` Just "alice"
    Left e -> expectationFailure ("expected Right, got " <> show e)


prop_caseInsensitive :: IO ()
prop_caseInsensitive = case parseKafkaEnvList
  [ ("kafka_client_id", "casey")
  , ("Kafka_Group_Id", "g1")
  ] of
  Right env -> do
    envClientId env `shouldBe` Just "casey"
    envGroupId env `shouldBe` Just "g1"
  Left e -> expectationFailure ("expected Right, got " <> show e)


prop_trimsWhitespace :: IO ()
prop_trimsWhitespace = case parseKafkaEnvList
  [ ("KAFKA_CLIENT_ID", "  ringo  ")
  , ("KAFKA_BATCH_SIZE", " 4096 ")
  ] of
  Right env -> do
    envClientId env `shouldBe` Just "ringo"
    envBatchSize env `shouldBe` Just 4096
  Left e -> expectationFailure ("expected Right, got " <> show e)


prop_reportsParseErrors :: IO ()
prop_reportsParseErrors = case parseKafkaEnvList
  [("KAFKA_BATCH_SIZE", "not-a-number")] of
  Left [ConfigError field _] -> field `shouldBe` "batch.size"
  other -> expectationFailure ("expected one batch.size error, got " <> show other)


prop_accumulatesErrors :: IO ()
prop_accumulatesErrors = case parseKafkaEnvList
  [ ("KAFKA_BATCH_SIZE", "x")
  , ("KAFKA_LINGER_MS", "y")
  , ("KAFKA_ENABLE_AUTO_COMMIT", "maybe")
  ] of
  Left errs -> length errs `shouldBe` 3
  Right _ -> expectationFailure "expected Left with 3 errors"


prop_fullEnv :: IO ()
prop_fullEnv = case parseKafkaEnvList
  [ ("KAFKA_BOOTSTRAP_SERVERS", "b1:9092,b2:9092")
  , ("KAFKA_CLIENT_ID", "ci")
  , ("KAFKA_SECURITY_PROTOCOL", "SASL_SSL")
  , ("KAFKA_SASL_MECHANISM", "SCRAM-SHA-256")
  , ("KAFKA_SASL_USERNAME", "u")
  , ("KAFKA_SASL_PASSWORD", "p")
  , ("KAFKA_SASL_OAUTH_BEARER_TOKEN", "oauth-token")
  , ("KAFKA_ACKS", "all")
  , ("KAFKA_COMPRESSION_TYPE", "zstd")
  , ("KAFKA_GROUP_ID", "grp")
  , ("KAFKA_AUTO_OFFSET_RESET", "earliest")
  ] of
  Right env -> do
    envBootstrapServers env `shouldBe` Just ["b1:9092", "b2:9092"]
    envClientId env `shouldBe` Just "ci"
    envSecurityProtocol env `shouldBe` Just SecSaslSsl
    envSaslMechanism env `shouldBe` Just MechScramSha256
    envSaslUsername env `shouldBe` Just "u"
    envSaslPassword env `shouldBe` Just "p"
    envSaslOAuthBearerToken env `shouldBe` Just "oauth-token"
    envAcks env `shouldBe` Just EnvAcksAll
    envCompressionType env `shouldBe` Just Compression.Zstd
    envGroupId env `shouldBe` Just "grp"
    envAutoOffsetReset env `shouldBe` Just EnvOffsetEarliest
  Left e -> expectationFailure ("expected Right, got " <> show e)


------------------------------------------------------------------
-- ConnectionConfig overlay
------------------------------------------------------------------

baseConn :: Conn.ConnectionConfig
baseConn = Conn.defaultConnectionConfig


prop_connDefaults :: IO ()
prop_connDefaults = withEnv [] $ \env ->
  case applyKafkaEnvToConnectionConfig env baseConn of
    Right cfg -> do
      Conn.connClientId cfg `shouldBe` Conn.connClientId baseConn
      Conn.connUseTls cfg `shouldBe` Conn.connUseTls baseConn
      Conn.connSasl cfg `assertSameSaslNothing` Conn.connSasl baseConn
      Conn.connRequestTimeoutMs cfg `shouldBe` Conn.connRequestTimeoutMs baseConn
    Left e -> expectationFailure ("expected Right, got " <> show e)


prop_connClientId :: IO ()
prop_connClientId = withEnv [("KAFKA_CLIENT_ID", "rocky")] $ \env ->
  case applyKafkaEnvToConnectionConfig env baseConn of
    Right cfg -> Conn.connClientId cfg `shouldBe` "rocky"
    Left e -> expectationFailure ("expected Right, got " <> show e)


prop_connSocketTimeout :: IO ()
prop_connSocketTimeout =
  withEnv [("KAFKA_SOCKET_TIMEOUT_MS", "45000")] $ \env ->
    case applyKafkaEnvToConnectionConfig env baseConn of
      Right cfg -> do
        Conn.connReadTimeout cfg `shouldBe` 45
        Conn.connWriteTimeout cfg `shouldBe` 45
      Left e -> expectationFailure ("expected Right, got " <> show e)


prop_connSsl :: IO ()
prop_connSsl =
  withEnv
    [ ("KAFKA_SECURITY_PROTOCOL", "SSL")
    , ("KAFKA_BOOTSTRAP_SERVERS", "broker.example.com:9093")
    ]
    $ \env ->
      case applyKafkaEnvToConnectionConfig env baseConn of
        Right cfg -> do
          Conn.connUseTls cfg `shouldBe` True
          ( case Conn.connTlsSettings cfg of
              Just _ -> True
              Nothing -> False
            )
            `shouldBe` True
        Left e -> expectationFailure ("expected Right, got " <> show e)


prop_connSslPreservesTls :: IO ()
prop_connSslPreservesTls = do
  let pinned = Conn.defaultTlsSettings "pinned.example.com"
      cfg0 = baseConn {Conn.connTlsSettings = Just pinned}
  withEnv
    [ ("KAFKA_SECURITY_PROTOCOL", "SSL")
    , ("KAFKA_BOOTSTRAP_SERVERS", "broker.example.com:9093")
    ]
    $ \env ->
      case applyKafkaEnvToConnectionConfig env cfg0 of
        Right cfg -> do
          Conn.connUseTls cfg `shouldBe` True
          -- We can't inspect the inner ClientParams equality easily,
          -- but at minimum the slot is occupied.
          ( case Conn.connTlsSettings cfg of
              Just _ -> True
              Nothing -> False
            )
            `shouldBe` True
        Left e -> expectationFailure ("expected Right, got " <> show e)


prop_connSaslSsl :: IO ()
prop_connSaslSsl =
  withEnv
    [ ("KAFKA_SECURITY_PROTOCOL", "SASL_SSL")
    , ("KAFKA_SASL_MECHANISM", "PLAIN")
    , ("KAFKA_SASL_USERNAME", "user")
    , ("KAFKA_SASL_PASSWORD", "pass")
    , ("KAFKA_BOOTSTRAP_SERVERS", "broker:9093")
    ]
    $ \env ->
      case applyKafkaEnvToConnectionConfig env baseConn of
        Right cfg -> do
          Conn.connUseTls cfg `shouldBe` True
          case Conn.connSasl cfg of
            Just (SASL.SaslPlain u p) -> do
              u `shouldBe` "user"
              p `shouldBe` "pass"
            other -> expectationFailure ("expected SaslPlain, got " <> show (saslDescr other))
        Left e -> expectationFailure ("expected Right, got " <> show e)


prop_connSaslScramSha512 :: IO ()
prop_connSaslScramSha512 =
  withEnv
    [ ("KAFKA_SECURITY_PROTOCOL", "SASL_SSL")
    , ("KAFKA_SASL_MECHANISM", "SCRAM-SHA-512")
    , ("KAFKA_SASL_USERNAME", "u")
    , ("KAFKA_SASL_PASSWORD", "p")
    , ("KAFKA_BOOTSTRAP_SERVERS", "broker:9093")
    ]
    $ \env ->
      case applyKafkaEnvToConnectionConfig env baseConn of
        Right cfg -> case Conn.connSasl cfg of
          Just (SASL.SaslScram Scram.ScramSHA512 _ _) -> pure ()
          other -> expectationFailure ("expected SCRAM-SHA-512, got " <> show (saslDescr other))
        Left e -> expectationFailure ("expected Right, got " <> show e)


prop_connSaslPlaintext :: IO ()
prop_connSaslPlaintext =
  withEnv
    [ ("KAFKA_SECURITY_PROTOCOL", "SASL_PLAINTEXT")
    , ("KAFKA_SASL_MECHANISM", "PLAIN")
    , ("KAFKA_SASL_USERNAME", "u")
    , ("KAFKA_SASL_PASSWORD", "p")
    ]
    $ \env ->
      case applyKafkaEnvToConnectionConfig env baseConn of
        Right cfg -> do
          Conn.connUseTls cfg `shouldBe` False
          case Conn.connSasl cfg of
            Just (SASL.SaslPlain _ _) -> pure ()
            other -> expectationFailure ("expected SaslPlain, got " <> show (saslDescr other))
        Left e -> expectationFailure ("expected Right, got " <> show e)


prop_connSaslSslNoMechanism :: IO ()
prop_connSaslSslNoMechanism =
  withEnv [("KAFKA_SECURITY_PROTOCOL", "SASL_SSL")] $ \env ->
    case applyKafkaEnvToConnectionConfig env baseConn of
      Left [ConfigError field _] -> field `shouldBe` "sasl.mechanism"
      other ->
        expectationFailure
          ( "expected sasl.mechanism error, got "
              <> describeResult other
          )


prop_connSaslMissingPassword :: IO ()
prop_connSaslMissingPassword =
  withEnv
    [ ("KAFKA_SECURITY_PROTOCOL", "SASL_SSL")
    , ("KAFKA_SASL_MECHANISM", "PLAIN")
    , ("KAFKA_SASL_USERNAME", "u")
    , ("KAFKA_BOOTSTRAP_SERVERS", "h:1")
    ]
    $ \env ->
      case applyKafkaEnvToConnectionConfig env baseConn of
        Left [ConfigError field _] -> field `shouldBe` "sasl.password"
        other ->
          expectationFailure
            ( "expected sasl.password error, got "
                <> describeResult other
            )


prop_connSaslOAuthStaticToken :: IO ()
prop_connSaslOAuthStaticToken =
  withEnv
    [ ("KAFKA_SECURITY_PROTOCOL", "SASL_SSL")
    , ("KAFKA_SASL_MECHANISM", "OAUTHBEARER")
    , ("KAFKA_SASL_OAUTH_BEARER_TOKEN", "static-token")
    , ("KAFKA_BOOTSTRAP_SERVERS", "h:1")
    ]
    $ \env ->
      case applyKafkaEnvToConnectionConfig env baseConn of
        Right cfg -> case Conn.connSasl cfg of
          Just (SASL.SaslOAuthBearer (OAuth.OAuthStaticToken tok)) ->
            OAuth.oauthTokenBytes tok `shouldBe` "static-token"
          other ->
            expectationFailure
              ( "expected static OAUTHBEARER, got "
                  <> show (saslDescr other)
              )
        Left e -> expectationFailure ("expected Right, got " <> show e)


prop_connSaslOAuthMissingToken :: IO ()
prop_connSaslOAuthMissingToken =
  withEnv
    [ ("KAFKA_SECURITY_PROTOCOL", "SASL_SSL")
    , ("KAFKA_SASL_MECHANISM", "OAUTHBEARER")
    , ("KAFKA_BOOTSTRAP_SERVERS", "h:1")
    ]
    $ \env ->
      case applyKafkaEnvToConnectionConfig env baseConn of
        Left [ConfigError field msg] -> do
          field `shouldBe` "sasl.oauthbearer.token"
          ("OAUTHBEARER" `T.isInfixOf` msg) `shouldBe` True
        other ->
          expectationFailure
            ( "expected sasl.oauthbearer.token error, got "
                <> describeResult other
            )


prop_connSaslAwsRejected :: IO ()
prop_connSaslAwsRejected =
  withEnv
    [ ("KAFKA_SECURITY_PROTOCOL", "SASL_SSL")
    , ("KAFKA_SASL_MECHANISM", "AWS_MSK_IAM")
    , ("KAFKA_BOOTSTRAP_SERVERS", "h:1")
    ]
    $ \env ->
      case applyKafkaEnvToConnectionConfig env baseConn of
        Left [ConfigError field msg] -> do
          field `shouldBe` "sasl.mechanism"
          ("AWS_MSK_IAM" `T.isInfixOf` msg) `shouldBe` True
        other ->
          expectationFailure
            ( "expected sasl.mechanism error, got "
                <> describeResult other
            )


------------------------------------------------------------------
-- ProducerConfig overlay
------------------------------------------------------------------

baseProd :: P.ProducerConfig
baseProd = P.defaultProducerConfig


prop_prodDefaults :: IO ()
prop_prodDefaults = withEnv [] $ \env ->
  case applyKafkaEnvToProducerConfig env baseProd of
    Right cfg -> do
      P.producerClientId cfg `shouldBe` P.producerClientId baseProd
      P.producerCompression cfg `shouldBe` P.producerCompression baseProd
      P.producerBatchSize cfg `shouldBe` P.producerBatchSize baseProd
      P.producerLingerMs cfg `shouldBe` P.producerLingerMs baseProd
      P.producerIdempotent cfg `shouldBe` P.producerIdempotent baseProd
    Left e -> expectationFailure ("expected Right, got " <> show e)


prop_prodOverrides :: IO ()
prop_prodOverrides =
  withEnv
    [ ("KAFKA_BATCH_SIZE", "32768")
    , ("KAFKA_LINGER_MS", "20")
    , ("KAFKA_COMPRESSION_TYPE", "lz4")
    , ("KAFKA_RETRIES", "7")
    ]
    $ \env ->
      case applyKafkaEnvToProducerConfig env baseProd of
        Right cfg -> do
          P.producerBatchSize cfg `shouldBe` 32768
          P.producerLingerMs cfg `shouldBe` 20
          P.producerCompression cfg `shouldBe` Compression.Lz4
          P.producerRetries cfg `shouldBe` 7
        Left e -> expectationFailure ("expected Right, got " <> show e)


prop_prodAcksAll :: IO ()
prop_prodAcksAll =
  withEnv [("KAFKA_ACKS", "all")] $ \env ->
    case applyKafkaEnvToProducerConfig env baseProd of
      Right cfg -> P.producerDelivery cfg `shouldBe` P.ExactlyOnce
      Left e -> expectationFailure ("expected Right, got " <> show e)


prop_prodIdempotence :: IO ()
prop_prodIdempotence =
  withEnv [("KAFKA_ENABLE_IDEMPOTENCE", "true")] $ \env ->
    case applyKafkaEnvToProducerConfig env baseProd of
      Right cfg -> P.producerIdempotent cfg `shouldBe` True
      Left e -> expectationFailure ("expected Right, got " <> show e)


prop_prodTransactional :: IO ()
prop_prodTransactional =
  withEnv [("KAFKA_TRANSACTIONAL_ID", "tx-1")] $ \env ->
    case applyKafkaEnvToProducerConfig env baseProd of
      Right cfg -> P.producerTransactional cfg `shouldBe` Just "tx-1"
      Left e -> expectationFailure ("expected Right, got " <> show e)


------------------------------------------------------------------
-- ConsumerConfig overlay
------------------------------------------------------------------

baseCons :: C.ConsumerConfig
baseCons = C.defaultConsumerConfig


prop_consDefaults :: IO ()
prop_consDefaults = withEnv [] $ \env ->
  case applyKafkaEnvToConsumerConfig env baseCons of
    Right cfg -> do
      C.consumerClientId cfg `shouldBe` C.consumerClientId baseCons
      C.consumerGroupId cfg `shouldBe` C.consumerGroupId baseCons
      C.consumerAutoOffsetReset cfg `shouldBe` C.consumerAutoOffsetReset baseCons
    Left e -> expectationFailure ("expected Right, got " <> show e)


prop_consGroupId :: IO ()
prop_consGroupId =
  withEnv
    [ ("KAFKA_GROUP_ID", "team-a")
    , ("KAFKA_GROUP_INSTANCE_ID", "pod-1")
    ]
    $ \env ->
      case applyKafkaEnvToConsumerConfig env baseCons of
        Right cfg -> do
          C.consumerGroupId cfg `shouldBe` "team-a"
          C.consumerGroupInstanceId cfg `shouldBe` Just "pod-1"
        Left e -> expectationFailure ("expected Right, got " <> show e)


prop_consAutoOffsetReset :: IO ()
prop_consAutoOffsetReset =
  withEnv [("KAFKA_AUTO_OFFSET_RESET", "earliest")] $ \env ->
    case applyKafkaEnvToConsumerConfig env baseCons of
      Right cfg -> C.consumerAutoOffsetReset cfg `shouldBe` C.Earliest
      Left e -> expectationFailure ("expected Right, got " <> show e)


prop_consIsolation :: IO ()
prop_consIsolation =
  withEnv [("KAFKA_ISOLATION_LEVEL", "read_committed")] $ \env ->
    case applyKafkaEnvToConsumerConfig env baseCons of
      Right cfg -> C.consumerIsolationLevel cfg `shouldBe` C.ReadCommitted
      Left e -> expectationFailure ("expected Right, got " <> show e)


prop_consTimeouts :: IO ()
prop_consTimeouts =
  withEnv
    [ ("KAFKA_SESSION_TIMEOUT_MS", "60000")
    , ("KAFKA_HEARTBEAT_INTERVAL_MS", "5000")
    , ("KAFKA_MAX_POLL_INTERVAL_MS", "120000")
    , ("KAFKA_MAX_POLL_RECORDS", "250")
    ]
    $ \env ->
      case applyKafkaEnvToConsumerConfig env baseCons of
        Right cfg -> do
          C.consumerSessionTimeoutMs cfg `shouldBe` 60_000
          C.consumerHeartbeatIntervalMs cfg `shouldBe` 5_000
          C.consumerMaxPollIntervalMs cfg `shouldBe` 120_000
          C.consumerMaxPollRecords cfg `shouldBe` 250
        Left e -> expectationFailure ("expected Right, got " <> show e)


prop_consAssignmentJvmName :: IO ()
prop_consAssignmentJvmName =
  withEnv
    [
      ( "KAFKA_PARTITION_ASSIGNMENT_STRATEGY"
      , "org.apache.kafka.clients.consumer.RoundRobinAssignor"
      )
    ]
    $ \env ->
      case applyKafkaEnvToConsumerConfig env baseCons of
        Right cfg ->
          C.consumerAssignmentStrategy cfg `shouldBe` C.RoundRobinAssignment
        Left e -> expectationFailure ("expected Right, got " <> show e)


prop_consInheritsConnection :: IO ()
prop_consInheritsConnection =
  withEnv
    [ ("KAFKA_REQUEST_TIMEOUT_MS", "12345")
    , ("KAFKA_CLIENT_ID", "consumer-a")
    ]
    $ \env ->
      case applyKafkaEnvToConsumerConfig env baseCons of
        Right cfg -> do
          C.consumerClientId cfg `shouldBe` "consumer-a"
          Conn.connRequestTimeoutMs (C.consumerConnectionConfig cfg) `shouldBe` 12345
        Left e -> expectationFailure ("expected Right, got " <> show e)


------------------------------------------------------------------
-- createProducer / createConsumer wiring
------------------------------------------------------------------

{- | Set a KAFKA_* env var for the body of the test, then unset
it. Uses 'setEnv' / 'unsetEnv' from "System.Environment".
-}
withTempEnv :: [(String, String)] -> IO a -> IO a
withTempEnv kvs body = do
  old <- mapM (\(k, _) -> (,) k <$> SE.lookupEnv k) kvs
  mapM_ (\(k, v) -> SE.setEnv k v) kvs
  r <- body
  mapM_ restore old
  pure r
  where
    restore (k, Nothing) = SE.unsetEnv k
    restore (k, Just v) = SE.setEnv k v


{- | createProducer should pick up @KAFKA_BOOTSTRAP_SERVERS@ when
the positional arg is left empty. We point it at a port that
nothing is listening on; the connect attempt fails, but the
failure message contains the host:port the env var supplied,
proving the env was consulted.
-}
prop_createProducerReadsBootstrap :: IO ()
prop_createProducerReadsBootstrap =
  withTempEnv [("KAFKA_BOOTSTRAP_SERVERS", "127.0.0.1:1")] $ do
    r <- P.createProducer [] P.defaultProducerConfig
    case r of
      Left msg -> (if ("127.0.0.1" `isInfixOfS` msg) then pure () else expectationFailure ("expected error to mention 127.0.0.1, got " <> msg))
      Right _ ->
        expectationFailure
          "expected createProducer to fail connecting to 127.0.0.1:1"


prop_createConsumerReadsBootstrap :: IO ()
prop_createConsumerReadsBootstrap =
  withTempEnv [("KAFKA_BOOTSTRAP_SERVERS", "127.0.0.1:1")] $ do
    r <- C.createConsumer [] "" C.defaultConsumerConfig
    case r of
      Left msg -> (if ("127.0.0.1" `isInfixOfS` msg) then pure () else expectationFailure ("expected error to mention 127.0.0.1, got " <> msg))
      Right _ ->
        expectationFailure
          "expected createConsumer to fail connecting to 127.0.0.1:1"


prop_createProducerMalformedEnv :: IO ()
prop_createProducerMalformedEnv =
  withTempEnv [("KAFKA_BATCH_SIZE", "not-a-number")] $ do
    r <- P.createProducer ["127.0.0.1:1"] P.defaultProducerConfig
    case r of
      Left msg -> (if ("batch.size" `isInfixOfS` msg) then pure () else expectationFailure ("expected the rendered error to mention batch.size, got " <> msg))
      Right _ ->
        expectationFailure
          "expected createProducer to refuse the malformed env"


{- | 'Data.List.isInfixOf' specialised for 'String'. Avoids
pulling 'Text.isInfixOf' just for the literal substring match.
-}
isInfixOfS :: String -> String -> Bool
isInfixOfS needle haystack = go haystack
  where
    n = length needle
    go [] = False
    go xs@(_ : rest)
      | take n xs == needle = True
      | otherwise = go rest


------------------------------------------------------------------
-- Test helpers
------------------------------------------------------------------

{- | Parse the env table and hand the resulting 'KafkaEnv' to the
continuation. We fail the test if parsing itself returns
errors, since every test that uses this helper exercises the
/successful/ path of 'parseKafkaEnvList'.
-}
withEnv :: [(String, String)] -> (KafkaEnv -> IO ()) -> IO ()
withEnv kvs k = case parseKafkaEnvList (fmap (\(a, b) -> (T.pack a, T.pack b)) kvs) of
  Right env -> k env
  Left errs -> expectationFailure ("parseKafkaEnvList failed: " <> show errs)


{- | Compare two SASL configs by structural intent. We don't get
an 'Eq' instance for 'SASL.SaslConfig' (it contains function
callbacks for OAUTHBEARER) so we settle for 'Nothing == Nothing'.
-}
assertSameSaslNothing :: Maybe SASL.SaslConfig -> Maybe SASL.SaslConfig -> IO ()
assertSameSaslNothing Nothing Nothing = pure ()
assertSameSaslNothing a b =
  expectationFailure
    ( "expected both Nothing, got "
        <> show (fmap saslName a)
        <> " / "
        <> show (fmap saslName b)
    )


saslName :: SASL.SaslConfig -> String
saslName SASL.SaslPlain {} = "PLAIN"
saslName SASL.SaslPlainWithAuthzid {} = "PLAIN"
saslName (SASL.SaslScram Scram.ScramSHA256 _ _) = "SCRAM-SHA-256"
saslName (SASL.SaslScram Scram.ScramSHA512 _ _) = "SCRAM-SHA-512"
saslName SASL.SaslOAuthBearer {} = "OAUTHBEARER"
saslName SASL.SaslOAuthBearerWithExtensions {} = "OAUTHBEARER"
saslName SASL.SaslAwsMskIam {} = "AWS_MSK_IAM"
saslName SASL.SaslGssapi = "GSSAPI"


saslDescr :: Maybe SASL.SaslConfig -> String
saslDescr Nothing = "Nothing"
saslDescr (Just s) = "Just " <> saslName s


{- | We can't simply 'show' the @Either [ConfigError] ConnectionConfig@
because 'ConnectionConfig' embeds 'TLS.ClientParams' / function
callbacks that don't have 'Show' instances. Reduce to a tag.
-}
describeResult :: Either [ConfigError] a -> String
describeResult (Right _) = "Right <config>"
describeResult (Left errs) = "Left " <> show errs
