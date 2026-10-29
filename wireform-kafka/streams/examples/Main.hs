{- |
Module      : Main
Description : CLI dispatcher for the wireform-kafka-streams demos

One executable, many demos. Run with no args for the index, or
pass a demo name to run that demo against the in-process test
driver (no broker required).

Pass @--broker host:port@ (or set @WIREFORM_KAFKA_BROKER@) to
run the demo against a real Kafka broker instead. Not every
demo supports broker mode — those that don't fall back to the
in-process driver with a clear stderr warning.

The set of demos mirrors the canonical Apache Kafka Streams
examples shipped under @org.apache.kafka.streams.examples@:

  * @pipe@        — copy from one topic to another
  * @line-split@  — flatMap a line into words
  * @word-count@  — flatMap + groupBy + count
  * @page-views@  — KStream-KTable inner join
  * @temperature@ — tumbling-window max + KIP-328 suppress
  * @top-articles@— hopping-window count by industry
  * @orders@      — multi-stage KStream-KTable enrichment
  * @fraud@       — session windows
  * @fk-join@     — KIP-213 KTable-KTable foreign-key join
  * @iq@          — KIP-67 / KIP-796 interactive queries
  * @processor@   — Processor API + Punctuator
  * @branching@   — KIP-418 split on predicates
  * @global@      — KStream-GlobalKTable join
  * @cogroup@     — KIP-150 cogroup of streams with distinct value types

And a second set of /operational/ demos that exercise the
runtime — multi-instance cluster, deployments, crashes,
upgrades, warm failover, EOS:

  * @ops-bringup@ — multi-instance assignment over a shared broker
  * @ops-crash@   — one instance dies; peers inherit its partitions
  * @ops-rolling@ — rolling deploy that recycles instances under load
  * @ops-threads@ — KIP-663 in-process @num.stream.threads@ scaling
  * @ops-standby@ — warm a standby off the changelog and decide
                    when it's ready (KIP-441)
  * @ops-eos@     — EOS commit visibility under read-committed
                    and behaviour on commit fault
  * @ops-revoke@  — KIP-869 soft-revocation grace decisions
  * @ops-observe@ — observability utilities: topology stats, lag /
                    health reports, and the OpenTelemetry metrics
                    bridge
  * @ops-replay@  — offline replay: reprocess a captured log,
                    window / offset selection, headers, rate
                    control, capture round-trip
  * @ops-backfill@— state backfill: changelog, snapshot+tail, CDC
-}
module Main (main) where

import Data.List (intercalate)
import Data.Text qualified as T
import Kafka.Streams.Examples.Branching qualified as Branching
import Kafka.Streams.Examples.Cogroup qualified as Cogroup
import Kafka.Streams.Examples.FraudDetection qualified as FraudDetection
import Kafka.Streams.Examples.GlobalTable qualified as GlobalTable
import Kafka.Streams.Examples.IdiomaticPipeline qualified as IdiomaticPipeline
import Kafka.Streams.Examples.InteractiveQueries qualified as InteractiveQueries
import Kafka.Streams.Examples.InventoryFKJoin qualified as InventoryFKJoin
import Kafka.Streams.Examples.LineSplit qualified as LineSplit
import Kafka.Streams.Examples.Ops.Backfill qualified as OpsBackfill
import Kafka.Streams.Examples.Ops.ClusterBringup qualified as OpsBringup
import Kafka.Streams.Examples.Ops.CrashFailover qualified as OpsCrash
import Kafka.Streams.Examples.Ops.DynamicThreads qualified as OpsThreads
import Kafka.Streams.Examples.Ops.EOSCommit qualified as OpsEOS
import Kafka.Streams.Examples.Ops.Observability qualified as OpsObservability
import Kafka.Streams.Examples.Ops.Replay qualified as OpsReplay
import Kafka.Streams.Examples.Ops.RevocationGrace qualified as OpsRevGrace
import Kafka.Streams.Examples.Ops.RollingUpgrade qualified as OpsRolling
import Kafka.Streams.Examples.Ops.StandbyWarmup qualified as OpsStandby
import Kafka.Streams.Examples.OrdersEnrichment qualified as OrdersEnrichment
import Kafka.Streams.Examples.PageViewRegion qualified as PageViewRegion
import Kafka.Streams.Examples.Pipe qualified as Pipe
import Kafka.Streams.Examples.ProcessorAPI qualified as ProcessorAPI
import Kafka.Streams.Examples.Runner (
  RunMode (..),
  brokerOnlyWarning,
  parseRunMode,
 )
import Kafka.Streams.Examples.SideEffects qualified as SideEffects
import Kafka.Streams.Examples.Temperature qualified as Temperature
import Kafka.Streams.Examples.TopArticles qualified as TopArticles
import Kafka.Streams.Examples.WordCount qualified as WordCount
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)


{- | A demo is either @RunMode@-aware (broker-friendly) or
in-memory-only. Both shapes are runnable; the @InMemoryOnly@
ones print a clear stderr warning if the user asked for
broker mode.
-}
data Demo
  = ModeAware !(RunMode -> IO ())
  | InMemoryOnly !(IO ())


runDemoEntry :: String -> RunMode -> Demo -> IO ()
runDemoEntry _ mode (ModeAware r) = r mode
runDemoEntry name mode (InMemoryOnly body) = do
  brokerOnlyWarning name mode
  body


demos :: [(String, Demo)]
demos =
  [ -- DSL feature demos. The simple stateless / hash-shuffle ones
    -- run identically in either mode; the rest are still test-
    -- driver-only.
    ("pipe", ModeAware Pipe.runDemo)
  , ("line-split", ModeAware LineSplit.runDemo)
  , ("word-count", ModeAware WordCount.runDemo)
  , ("page-views", InMemoryOnly PageViewRegion.runDemo)
  , ("temperature", InMemoryOnly Temperature.runDemo)
  , ("top-articles", InMemoryOnly TopArticles.runDemo)
  , ("orders", InMemoryOnly OrdersEnrichment.runDemo)
  , ("fraud", InMemoryOnly FraudDetection.runDemo)
  , ("fk-join", InMemoryOnly InventoryFKJoin.runDemo)
  , ("iq", InMemoryOnly InteractiveQueries.runDemo)
  , ("processor", InMemoryOnly ProcessorAPI.runDemo)
  , ("side-effects", InMemoryOnly SideEffects.runDemo)
  , ("branching", InMemoryOnly Branching.runDemo)
  , ("global", InMemoryOnly GlobalTable.runDemo)
  , ("cogroup", InMemoryOnly Cogroup.runDemo)
  , ("idiomatic", InMemoryOnly IdiomaticPipeline.runDemo)
  , -- Operational demos: multi-instance cluster, deployments,
    -- crashes, upgrades, warm failover, EOS. They run the real
    -- runtime on the in-memory broker
    -- ("Kafka.Streams.Runtime.MockBroker"); ops-threads and
    -- ops-revoke show standalone libraries.
    ("ops-bringup", InMemoryOnly OpsBringup.runDemo)
  , ("ops-crash", InMemoryOnly OpsCrash.runDemo)
  , ("ops-rolling", InMemoryOnly OpsRolling.runDemo)
  , ("ops-threads", InMemoryOnly OpsThreads.runDemo)
  , ("ops-standby", InMemoryOnly OpsStandby.runDemo)
  , ("ops-eos", InMemoryOnly OpsEOS.runDemo)
  , ("ops-revoke", InMemoryOnly OpsRevGrace.runDemo)
  , ("ops-observe", InMemoryOnly OpsObservability.runDemo)
  , ("ops-replay", InMemoryOnly OpsReplay.runDemo)
  , ("ops-backfill", InMemoryOnly OpsBackfill.runDemo)
  ]


main :: IO ()
main = do
  rawArgs <- getArgs
  (mode, args) <- parseRunMode rawArgs
  case args of
    ["all"] -> do
      announceMode mode
      mapM_ (\(n, d) -> runDemoEntry n mode d) demos
    [name] -> case lookup name demos of
      Just d -> do
        announceMode mode
        runDemoEntry name mode d
      Nothing -> do
        hPutStrLn stderr $
          "unknown demo: "
            <> name
            <> "\nknown demos: "
            <> intercalate ", " (map fst demos)
    _ -> do
      putStrLn "wireform-kafka-streams-examples"
      putStrLn ""
      putStrLn "Usage:"
      putStrLn "  wireform-kafka-streams-examples [--broker host:port] <demo>"
      putStrLn "  wireform-kafka-streams-examples [--broker host:port] all"
      putStrLn ""
      putStrLn "Mode:"
      putStrLn "  Default mode runs every demo against the in-process"
      putStrLn "  TopologyTestDriver (no broker required)."
      putStrLn "  --broker host:port (or env WIREFORM_KAFKA_BROKER=host:port)"
      putStrLn "  runs broker-compatible demos against a real Kafka broker."
      putStrLn ""
      putStrLn "Available demos:"
      mapM_
        ( \(n, d) -> do
            let tag = case d of
                  ModeAware {} -> "  (broker + in-memory)"
                  InMemoryOnly {} -> "  (in-memory only)"
            putStrLn ("  " <> n <> tag)
        )
        demos


announceMode :: RunMode -> IO ()
announceMode = \case
  InMemory ->
    putStrLn "[wireform-kafka-streams-examples] mode: in-memory (no broker)"
  Broker b ->
    putStrLn $ "[wireform-kafka-streams-examples] mode: broker " <> T.unpack b
