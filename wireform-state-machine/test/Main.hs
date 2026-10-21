module Main (main) where

import Test.Syd

import Hedgehog.Internal.Config (resolveSkip)

import Test.StateMachine.ChildBridge qualified
import Test.StateMachine.Completion qualified
import Test.StateMachine.Effects qualified
import Test.StateMachine.Eventless qualified
import Test.StateMachine.History qualified
import Test.StateMachine.Keys qualified
import Test.StateMachine.Ordering qualified
import Test.StateMachine.Parallel qualified
import Test.StateMachine.Persistence qualified
import Test.StateMachine.Properties qualified
import Test.StateMachine.Selection qualified
import Test.StateMachine.Surface qualified

main :: IO ()
main = do
  -- Force Hedgehog's "HEDGEHOG_SKIP" CAF before sydtest starts its workers.
  -- Every property forces it first; when many capabilities hit it at once,
  -- GHC 9.8.4's RTS can lose the wakeup of a thread blocked on it, leaving
  -- one property hung until the per-test timeout.
  _ <- resolveSkip Nothing
  sydTest $
    describe "wireform-state-machine" $
      sequence_
        [ Test.StateMachine.Ordering.tests
        , Test.StateMachine.Selection.tests
        , Test.StateMachine.Parallel.tests
        , Test.StateMachine.History.tests
        , Test.StateMachine.Completion.tests
        , Test.StateMachine.Eventless.tests
        , Test.StateMachine.Effects.tests
        , Test.StateMachine.Surface.tests
        , Test.StateMachine.Persistence.tests
        , Test.StateMachine.Properties.tests
        , Test.StateMachine.ChildBridge.tests
        , Test.StateMachine.Keys.tests
        ]
