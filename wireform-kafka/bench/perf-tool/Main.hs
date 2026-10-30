{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Stand-alone produce / consume perf tool. Mirrors the
librdkafka 'rdk_produce' / 'rdk_consume' tools so the three
numbers ('JVM' kafka-{producer,consumer}-perf-test.sh,
librdkafka /examples/, this tool) measure the same workload
end-to-end against a real broker.

Usage:

@
wireform-kafka-perf produce BROKER TOPIC N VSIZE ACKS BATCH LINGER COMPRESSION
wireform-kafka-perf consume BROKER TOPIC GROUP N
@
-}
module Main where

import Control.Monad (replicateM_, when)
import Data.ByteString qualified as BS
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as Text.IO
import Data.Time.Clock.POSIX qualified as Time
import Kafka.Client.Consumer qualified as WC
import Kafka.Client.Producer qualified as WP
import Kafka.Compression.Types qualified as Compression
import System.Environment (getArgs)
import System.Exit (die)
import System.IO qualified
import Text.Printf (printf)


main :: IO ()
main = do
  args <- getArgs
  case args of
    ("produce" : broker : topic : sN : sV : sAcks : sBatch : sLinger : compression : _) ->
      runProduce
        broker
        (T.pack topic)
        (read sN)
        (read sV)
        (read sAcks)
        (read sBatch)
        (read sLinger)
        compression
    ("consume" : broker : topic : group : sN : _) ->
      runConsume broker (T.pack topic) (T.pack group) (read sN)
    _ ->
      die $
        unlines
          [ "usage:"
          , "  wireform-kafka-perf produce BROKER TOPIC N VSIZE ACKS BATCH LINGER COMPRESSION"
          , "  wireform-kafka-perf consume BROKER TOPIC GROUP N"
          ]


runProduce :: String -> Text -> Int -> Int -> Int -> Int -> Int -> String -> IO ()
runProduce broker topic n vsize acks batch lingerMs compression = do
  ackRef <- newIORef (0 :: Int)
  let !payload = BS.replicate vsize 0x41
      !pcfg =
        WP.defaultProducerConfig
          { WP.producerLingerMs = lingerMs
          , WP.producerBatchSize = batch
          , WP.producerMaxInFlight = 32
          , WP.producerDelivery = case acks of
              0 -> WP.AtMostOnce
              _ -> WP.AtLeastOnce
          , WP.producerCompression = case compression of
              "lz4" -> Compression.Lz4
              "snappy" -> Compression.Snappy
              "gzip" -> Compression.Gzip
              "zstd" -> Compression.Zstd
              _ -> Compression.NoCompression
          , WP.producerOnAcknowledgement = \_record res -> case res of
              Right _ -> modifyIORef' ackRef (+ 1)
              Left _ -> pure ()
          }
  res <- WP.createProducer [T.pack broker] pcfg
  case res of
    Left err -> die ("createProducer: " ++ err)
    Right p -> do
      t0 <- Time.getPOSIXTime
      let go !i
            | i <= 0 = pure ()
            | otherwise = do
                r <- WP.sendMessageFastest_ p topic Nothing payload
                case r of
                  Left e -> die ("send: " ++ e)
                  Right _ -> go (i - 1)
      go n
      flushRes <- WP.flushProducer p
      case flushRes of
        Left e -> die ("flushProducer: " ++ e)
        Right _ -> pure ()
      ackN <- readIORef ackRef
      when (ackN /= n) $
        Text.IO.hPutStrLn
          System.IO.stderr
          (T.pack ("[warn] only " <> show ackN <> "/" <> show n <> " records acked"))
      t1 <- Time.getPOSIXTime
      let !secs = realToFrac (t1 - t0) :: Double
          !rps = fromIntegral n / secs
          !mbps = fromIntegral (n * vsize) / secs / 1024 / 1024
      printf
        "wireform-kafka produce: %d records in %.3f s = %.0f rec/s (%.1f MB/s) acks=%d batch=%d linger=%d comp=%s\n"
        n
        secs
        rps
        mbps
        acks
        batch
        lingerMs
        compression
      WP.closeProducer p


runConsume :: String -> Text -> Text -> Int -> IO ()
runConsume broker topic group n = do
  let !ccfg =
        WC.defaultConsumerConfig
          { WC.consumerAutoOffsetReset = WC.Earliest
          , WC.consumerAutoCommit = False
          , WC.consumerMaxPollRecords = max 1 (n `div` 100)
          }
  res <- WC.createConsumer [T.pack broker] group ccfg
  case res of
    Left err -> die ("createConsumer: " ++ err)
    Right c -> do
      sr <- WC.subscribe c [topic]
      case sr of
        Left e -> die ("subscribe: " ++ e)
        Right () -> pure ()
      countRef <- newIORef (0 :: Int)
      bytesRef <- newIORef (0 :: Int)
      tStartRef <- newIORef (0 :: Double)
      let go = do
            cnt <- readIORef countRef
            if cnt >= n
              then pure ()
              else do
                r <- WC.poll c 5000
                case r of
                  Right rs -> do
                    when (cnt == 0 && not (null rs)) $ do
                      ts <- realToFrac <$> Time.getPOSIXTime
                      writeIORef tStartRef ts
                    let !nrs = length rs
                        !nbs = sum (map (maybe 0 BS.length . (.value)) rs)
                    modifyIORef' countRef (+ nrs)
                    modifyIORef' bytesRef (+ nbs)
                    go
                  Left _ -> go
      go
      t1 <- realToFrac <$> Time.getPOSIXTime :: IO Double
      t0 <- readIORef tStartRef
      cnt <- readIORef countRef
      bytes <- readIORef bytesRef
      let !secs = t1 - t0
          !rps = fromIntegral cnt / secs :: Double
          !mbps = fromIntegral bytes / secs / 1024 / 1024 :: Double
      printf
        "wireform-kafka consume: %d records in %.3f s = %.0f rec/s (%.1f MB/s)\n"
        cnt
        secs
        rps
        mbps
      WC.closeConsumer c
