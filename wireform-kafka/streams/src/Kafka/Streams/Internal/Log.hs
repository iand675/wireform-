{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Streams.Internal.Log
Description : The Streams runtime's diagnostics, as OpenTelemetry log records

What the runtime has to report (an aborted transaction, a failed
commit, a re-plan, a store that failed to close) goes out through
"Kafka.Telemetry.Log" under the @wireform-kafka-streams@ scope: to
whatever OpenTelemetry logger provider the application installed, and
nowhere at all without one. Nothing is printed.
-}
module Kafka.Streams.Internal.Log (
  logInfo,
  logWarn,
  logError,
) where

import Data.Text qualified as T
import Kafka.Telemetry.Log qualified as KLog


logInfo, logWarn, logError :: String -> IO ()
logInfo msg = KLog.logInfo KLog.streamsInstrumentationLibrary (T.pack msg) []
logWarn msg = KLog.logWarn KLog.streamsInstrumentationLibrary (T.pack msg) []
logError msg = KLog.logError KLog.streamsInstrumentationLibrary (T.pack msg) []
