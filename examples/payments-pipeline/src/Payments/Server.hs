{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Payments.Server
Description : gRPC PaymentService server that appends to the Kafka event log.

The synchronous front door. 'CreatePayment' is a unary RPC whose handler:

  1. mints a transaction id and timestamp,
  2. turns the request into a canonical 'TransactionEvent'
     ('transactionEventFromRequest'),
  3. appends that event to the @transactions@ topic via the Kafka
     producer (this is the event-sourcing write), and
  4. acknowledges synchronously with a 'PaymentResponse'.

Everything downstream — risk features, bookkeeping entries — is produced
asynchronously by the Kafka Streams topology consuming that topic, so the
RPC path stays short.

Requires a reachable Kafka broker; see 'Payments.Demo' for a broker-free
view of the stream processing.
-}
module Payments.Server (
  runPaymentServer,
) where

import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Kafka qualified
import Kafka.Serde (textSerde)
import Kafka.Serde.Proto (protoSerde)
import Kafka.Topic qualified as Topic
import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Server
import Network.GRPC.Server.Run
import Network.GRPC.Server.Service (Handlers (..), Service, fromService, method, service)
import Payments.Domain (transactionEventFromRequest)
import Payments.Serdes (transactionsTopic)
import Proto.API.Payments


{- | Run the gRPC server on @port@, producing events to @brokers@. Blocks
until the server stops.
-}
runPaymentServer :: Int -> [Text] -> IO ()
runPaymentServer port brokers =
  Kafka.withProducer brokers Kafka.defaultProducerConfig $ \prod -> do
    putStrLn $ "PaymentService listening on 0.0.0.0:" <> show port
    putStrLn $ "  emitting events to topic '" <> show transactionsTopic <> "'"
    runServerWithHandlers def (serverConfig port) (fromService (paymentService prod))


{- | The transactions event log, typed as @Text@ keys (payer account) and
'TransactionEvent' protobuf values.
-}
transactionsLog :: Topic.Topic Text TransactionEvent
transactionsLog = Topic.topic transactionsTopic textSerde protoSerde


paymentService :: Kafka.Producer -> Service PaymentService IO
paymentService prod =
  service (method @CreatePayment (handleCreatePayment prod) :& Done)


handleCreatePayment
  :: Kafka.Producer
  -> Proto PaymentRequest
  -> IO (Proto PaymentResponse)
handleCreatePayment prod reqMsg = do
  let req = getProto reqMsg
  uuid <- UUID.nextRandom
  nowMillis <- round . (* 1000) <$> getPOSIXTime
  let transactionId = "txn-" <> UUID.toText uuid
      event = transactionEventFromRequest transactionId nowMillis req
  result <- Kafka.publish prod transactionsLog (Just (transactionEventPayerAccount event)) event
  let status = case result of
        Right _ -> PaymentStatus'PaymentStatusAccepted
        Left _ -> PaymentStatus'PaymentStatusRejected
  pure . Proto $
    defaultPaymentResponse
      { paymentResponseTransactionId = transactionId
      , paymentResponseStatus = status
      , paymentResponseCreatedAtMillis = nowMillis
      }


serverConfig :: Int -> ServerConfig
serverConfig port =
  ServerConfig
    { serverSecure = Nothing
    , serverInsecure =
        Just
          InsecureConfig
            { insecureHost = Just "0.0.0.0"
            , insecurePort = fromIntegral port
            }
    }
