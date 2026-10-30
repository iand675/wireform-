{- |
Module      : Kafka.Producer
Description : Transitional @hw-kafka-client@ producer facade.

Module to produce messages to Kafka topics using the old
@hw-kafka-client@ surface, backed by "Kafka.Client.Producer".

This module is intended as a transitional bridge for applications that
currently import @Kafka.Producer@. It preserves the old constructor and
function names while routing sends through the native wireform producer.
New code should use "Kafka.Client.Producer" directly.

Example:

@
producerProps :: 'ProducerProperties'
producerProps = 'brokersList' ["localhost:9092"]
             <> 'logLevel' 'KafkaLogInfo'

targetTopic :: 'TopicName'
targetTopic = 'TopicName' "events"

mkMessage :: Maybe ByteString -> Maybe ByteString -> 'ProducerRecord'
mkMessage k v = 'ProducerRecord'
  { 'prTopic' = targetTopic
  , 'prPartition' = 'UnassignedPartition'
  , 'prKey' = k
  , 'prValue' = v
  , 'prHeaders' = mempty
  }
@
-}
module Kafka.Producer (
  KafkaProducer,
  module X,
  runProducer,
  newProducer,
  produceMessage,
  produceMessage',
  flushProducer,
  closeProducer,
  RdKafkaRespErrT (..),
) where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int32)
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Kafka.Client.Producer qualified as WF
import Kafka.Compression.Types qualified as WFC
import Kafka.Consumer.Types (Offset (..))
import Kafka.Internal.Callbacks (
  Callback,
  deliveryCallbacks,
  errorCallbacks,
 )
import Kafka.Internal.Compat (
  Kafka (..),
  RdKafkaRespErrT (..),
  kafkaConf,
  textDecimal,
  topicConf,
 )
import Kafka.Producer.ProducerProperties as X
import Kafka.Producer.Types (KafkaProducer (..))
import Kafka.Producer.Types as X hiding (KafkaProducer)
import Kafka.Types as X


{-# DEPRECATED runProducer "Use 'newProducer'/'closeProducer' instead" #-}


{- | Run a Kafka producer with bracketed acquisition and release.

Deprecated upstream in favour of calling 'newProducer' and
'closeProducer' directly.
-}
runProducer
  :: ProducerProperties
  -> (KafkaProducer -> IO (Either KafkaError a))
  -> IO (Either KafkaError a)
runProducer props f =
  bracket (newProducer props) closeResult runResult
  where
    closeResult (Left _) = pure ()
    closeResult (Right prod) = closeProducer prod

    runResult (Left err) = pure (Left err)
    runResult (Right prod) = f prod


{- | Create a new Kafka producer.

A newly created producer must be closed with 'closeProducer'.
-}
newProducer :: MonadIO m => ProducerProperties -> m (Either KafkaError KafkaProducer)
newProducer props = liftIO $ do
  kc <- kafkaConf (ppKafkaProps props)
  let tc = topicConf (ppTopicProps props)
      brokers = bootstrapServers (ppKafkaProps props)
      cfg = producerConfig props
  result <- WF.createProducer brokers cfg
  case result of
    Left err -> do
      let kafkaErr = KafkaError (T.pack err)
      mapM_ (\callback -> callback kafkaErr err) (errorCallbacks (ppCallbacks props))
      pure (Left kafkaErr)
    Right producer ->
      pure $
        Right
          KafkaProducer
            { kpKafkaPtr = KafkaProducerHandle producer
            , kpKafkaConf = kc
            , kpTopicConf = tc
            }


{- | Send a single message.

Like @hw-kafka-client@, this returns only immediate/pre-flight
errors. The native wireform implementation waits for the
acknowledgement through 'Kafka.Client.Producer.sendRecord'.
-}
produceMessage
  :: MonadIO m
  => KafkaProducer
  -> ProducerRecord
  -> m (Maybe KafkaError)
produceMessage producer record =
  produceMessage' producer record (const (pure ())) >>= \case
    Left (ImmediateError err) -> pure (Just err)
    Right () -> pure Nothing


{- | Send a single message with a delivery callback.

The callback receives a compatibility 'DeliveryReport' after the
native send completes or fails.
-}
produceMessage'
  :: MonadIO m
  => KafkaProducer
  -> ProducerRecord
  -> (DeliveryReport -> IO ())
  -> m (Either ImmediateError ())
produceMessage' KafkaProducer {kpKafkaPtr = KafkaProducerHandle producer} record callback =
  liftIO $ do
    result <- WF.sendRecord producer (toWireformRecord record)
    case result of
      Left err -> do
        let kafkaErr = KafkaError (T.pack err)
        callback (DeliveryFailure record kafkaErr)
        pure (Left (ImmediateError kafkaErr))
      Right metadata -> do
        callback (DeliverySuccess record (Offset (WF.offset metadata)))
        pure (Right ())
produceMessage' _ record callback = liftIO $ do
  let err = KafkaBadSpecification "KafkaProducer does not contain a producer handle"
  callback (DeliveryFailure record err)
  pure (Left (ImmediateError err))


-- | Drain the producer's outbound queue.
flushProducer :: MonadIO m => KafkaProducer -> m ()
flushProducer KafkaProducer {kpKafkaPtr = KafkaProducerHandle producer} =
  liftIO $ do
    _ <- WF.flushProducer producer
    pure ()
flushProducer _ = pure ()


-- | Close the producer after flushing pending messages.
closeProducer :: MonadIO m => KafkaProducer -> m ()
closeProducer KafkaProducer {kpKafkaPtr = KafkaProducerHandle producer} =
  WF.closeProducer producer
closeProducer _ = pure ()


producerConfig :: ProducerProperties -> WF.ProducerConfig
producerConfig ProducerProperties {..} =
  WF.defaultProducerConfig
    { WF.producerClientId = M.findWithDefault "hw-kafka-client" "client.id" ppKafkaProps
    , WF.producerCompression =
        maybe WFC.defaultCodec compressionFromText (M.lookup "compression.codec" ppKafkaProps)
    , WF.producerDeliveryTimeoutMs =
        maybe
          (WF.producerDeliveryTimeoutMs WF.defaultProducerConfig)
          id
          (textDecimal =<< (M.lookup "message.timeout.ms" ppTopicProps <|> M.lookup "message.timeout.ms" ppKafkaProps))
    , WF.producerIdempotent = False
    , WF.producerDelivery = WF.AtLeastOnce
    , WF.producerOnAcknowledgement = dispatchProducerCallbacks ppCallbacks
    }


bootstrapServers :: M.Map T.Text T.Text -> [T.Text]
bootstrapServers props =
  case M.lookup "bootstrap.servers" props of
    Nothing -> []
    Just brokers -> filter (not . T.null) (T.splitOn "," brokers)


compressionFromText :: T.Text -> WFC.CompressionCodec
compressionFromText raw =
  case WFC.parseCompressionCodec raw of
    Just codec -> codec
    Nothing -> WFC.defaultCodec


toWireformRecord :: ProducerRecord -> WF.ProducerRecord
toWireformRecord ProducerRecord {..} =
  WF.ProducerRecord
    { WF.topic = unTopicName prTopic
    , WF.key = prKey
    , WF.value = prValue
    , WF.headers = map convertHeader (headersToList prHeaders)
    , WF.partition = partitionToWireform prPartition
    , WF.timestamp = Nothing
    }


partitionToWireform :: ProducePartition -> Maybe Int32
partitionToWireform UnassignedPartition = Nothing
partitionToWireform (SpecifiedPartition p) = Just (fromIntegral p)


convertHeader :: (ByteString, ByteString) -> (T.Text, ByteString)
convertHeader (name, value) =
  (TE.decodeUtf8With TEE.lenientDecode name, value)


fromWireformRecord :: WF.ProducerRecord -> ProducerRecord
fromWireformRecord WF.ProducerRecord {..} =
  ProducerRecord
    { prTopic = TopicName topic
    , prPartition = maybe UnassignedPartition (SpecifiedPartition . fromIntegral) partition
    , prKey = key
    , prValue = value
    , prHeaders = headersFromList (map (\(name, headerValue) -> (TE.encodeUtf8 name, headerValue)) headers)
    }


dispatchProducerCallbacks
  :: [Callback]
  -> WF.ProducerRecord
  -> Either String WF.RecordMetadata
  -> IO ()
dispatchProducerCallbacks callbacks record outcome =
  case outcome of
    Left err -> do
      let kafkaErr = KafkaError (T.pack err)
          report = DeliveryFailure (fromWireformRecord record) kafkaErr
      mapM_ (\callback -> callback report) (deliveryCallbacks callbacks)
      mapM_ (\callback -> callback kafkaErr err) (errorCallbacks callbacks)
    Right metadata -> do
      let report = DeliverySuccess (fromWireformRecord record) (Offset (WF.offset metadata))
      mapM_ (\callback -> callback report) (deliveryCallbacks callbacks)


(<|>) :: Maybe a -> Maybe a -> Maybe a
Just x <|> _ = Just x
Nothing <|> y = y
