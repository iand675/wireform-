{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}

{- | A traffic light with a pedestrian button, a power-outage mode, and a
history-based recovery — exercising the full vertical slice: type-level
spec, completeness-checked registries, pure stepping, snapshotting, and
restore with recovery.

Run with @cabal run example-traffic@.
-}
module Main (main) where

import Data.Aeson (FromJSON, ToJSON, encode)
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Generics (Generic)

import StateMachine

-- | Machine context: how many cycles the light has run, and whether a
-- pedestrian is waiting.
data TrafficCtx = TrafficCtx
  { cycles :: Int
  , pedestrianWaiting :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The chart. Illegal edits do not compile: misspell @\"yellow\"@ in a
-- target, drop a state, reference an undeclared event — all TypeErrors.
type Traffic =
  ChartWith
    "traffic"
    TrafficCtx
    Int -- output: total cycles once decommissioned
    '[ "TIMER" ::: ()
     , "PUSH" ::: ()
     , "POWER_OUT" ::: ()
     , "FIXED" ::: ()
     , "DECOMMISSION" ::: ()
     ]
    '[ Compound
        "operational"
        "green"
        '[ State
            "green"
            '[ On "TIMER" ==> To "yellow"
             , On "PUSH" ==> Stay ! '["notePedestrian"]
             ]
         , State "yellow" '[On "TIMER" ==> To "red" ! '["countCycle"]]
         , State
            "red"
            '[ On "TIMER" ?: "noPedestrian" ==> To "green"
             , On "TIMER" ==> To "walk"
             ]
         , State "walk" '[On "TIMER" ==> To "green" ! '["clearPedestrian"]]
         , Hist "opHist"
         ]
        '[On "POWER_OUT" ==> To "flashing"]
     , State "flashing" '[On "FIXED" ==> To "opHist"]
     , Final "off"
     ]
    "operational"
    '[On "DECOMMISSION" ==> To "off"]

impl :: ChartImpl IO Traffic
impl =
  chartImpl
    (mkGuard @"noPedestrian" (\ctx _ -> not (pedestrianWaiting ctx)) :& RNil)
    ( assign @"countCycle" (\ctx _ -> ctx{cycles = cycles ctx + 1})
        :& assign @"notePedestrian" (\ctx _ -> ctx{pedestrianWaiting = True})
        :& assign @"clearPedestrian" (\ctx _ -> ctx{pedestrianWaiting = False})
        :& RNil
    )
    RNil
    RNil
    cycles

main :: IO ()
main = do
  banner "initialize"
  Right s0 <- initialize impl (TrafficCtx 0 False)
  describe (sMachine s0)

  banner "a full cycle with a pedestrian"
  m1 <- feed (sMachine s0) ["PUSH", "TIMER", "TIMER", "TIMER", "TIMER"]
  describe m1

  banner "power outage, then fixed: history restores the phase"
  Right s2 <- step impl m1 (external "POWER_OUT")
  describe (sMachine s2)
  Right s3 <- step impl (sMachine s2) (external "FIXED")
  describe (sMachine s3)

  banner "snapshot / restore roundtrip"
  let snap = snapshot impl (sMachine s3)
  BL8.putStrLn (encode snap)
  case restore impl snap of
    Left err -> error ("roundtrip failed: " <> show err)
    Right restored -> describe (restoredMachine restored)

  banner "restoring a snapshot from a stale chart"
  let stale = snap{snapConfig = ["operational", "amber"], snapFingerprint = "0000000000000000"}
  case restore impl stale of
    Left err -> putStrLn ("refused, as it should: " <> show err)
    Right _ -> error "should not restore"

  banner "…but a Recovery policy can restart instead"
  out <- restoreWith impl (restartRecovery (TrafficCtx 0 False)) stale
  case out of
    Right (RecoveredByRestart stepped err) -> do
      putStrLn ("recovered from: " <> takeWhile (/= '{') (show err))
      describe (sMachine stepped)
    other -> error ("unexpected recovery outcome: " <> summarize other)

  banner "decommission (a global, root-level handler)"
  Right s4 <- step impl (sMachine s3) (external "DECOMMISSION")
  case status (sMachine s4) of
    Finished total -> putStrLn ("machine finished; total cycles: " <> show total)
    Running -> error "should have finished"

  banner "render: mermaid (paste into a README), xstate JSON (paste into stately.ai)"
  TIO.putStrLn (mermaidHighlight (config (sMachine s3)) (ciChart impl))
  TIO.putStrLn (xstateConfigText (ciChart impl))

  banner "render: self-contained HTML with the trace timeline"
  let page = htmlPage (ciChart impl) (Just (config (sMachine s3))) (sTrace s3)
  TIO.writeFile "traffic.html" page
  putStrLn ("wrote traffic.html (" <> show (T.length page) <> " chars); open it in a browser")

  banner "what just happened (pretty trace of the FIXED step)"
  TIO.putStrLn (prettyTrace (sTrace s3))
 where
  external :: Text -> StepEvent Traffic
  external = \case
    "TIMER" -> EvExternal (mkEvent_ @"TIMER")
    "PUSH" -> EvExternal (mkEvent_ @"PUSH")
    "POWER_OUT" -> EvExternal (mkEvent_ @"POWER_OUT")
    "FIXED" -> EvExternal (mkEvent_ @"FIXED")
    "DECOMMISSION" -> EvExternal (mkEvent_ @"DECOMMISSION")
    other -> error ("unknown event in demo: " <> T.unpack other)

  feed m [] = pure m
  feed m (e : es) = do
    r <- step impl m (external e)
    case r of
      Left fault -> error (show fault)
      Right stepped -> feed (sMachine stepped) es

  describe m =
    putStrLn
      ( "active: "
          <> show (activeStates m)
          <> "  ctx: "
          <> show (context m)
          <> "  walk? "
          <> show (matches @"walk" m)
      )

  summarize = \case
    Left err -> "Left " <> show err
    Right (Intact _) -> "Intact"
    Right (RecoveredByRestart _ _) -> "RecoveredByRestart"
    Right (RecoveredByResume _ _) -> "RecoveredByResume"

  banner t = putStrLn ("\n== " <> t)
