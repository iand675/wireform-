{- | Shared machinery for the semantics tests: a pure monad with an
action log, self-logging registry actions, and step drivers that unwrap
'StepFault' loudly.

Every test runs charts over @'LogM' = 'State' ['Text']@, so entry\/exit\/
transition action ORDER is an exact, deterministic assertion — no clocks,
no IO. Each driver call runs in its own 'runState', so the returned log
covers exactly that macrostep.
-}
module Test.StateMachine.Support (
  -- * The pure log monad
  LogM,
  runLog,
  logAct,

  -- * Drivers
  orFault,
  bootLog,
  bootStepped,
  boot,
  stepLog,
  stepOnce,
  advance,
  feed,

  -- * Machine projections
  ctxOf,
) where

import Control.Monad.Trans.State.Strict (State, modify', runState)
import Data.List (foldl')
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Stack (HasCallStack)
import GHC.TypeLits (KnownSymbol, symbolVal)

import StateMachine hiding (State)

-- | The test monad: a strict state of action names, appended in
-- execution order.
type LogM = State [Text]

-- | Run one computation with an empty log; returns the result and the
-- actions it logged, in order.
runLog :: LogM a -> (a, [Text])
runLog act = runState act []

-- | A registry action that logs its own name and does nothing else:
-- @logAct \@\"enterA\"@.
logAct :: forall n spec. (KnownSymbol n) => ActionE LogM spec n
logAct = effect @n (\_ _ -> modify' (<> [T.pack (symbolVal (Proxy @n))]))

-- | Tests never expect step faults; make one fail loudly.
orFault :: (HasCallStack) => Either StepFault a -> a
orFault = either (\f -> error ("unexpected step fault: " <> show f)) id

-- | Initialize, returning the step result and the actions it ran.
bootLog :: (HasCallStack) => ChartImpl LogM spec -> Ctx spec -> (Stepped spec, [Text])
bootLog impl c =
  let (r, lg) = runLog (initialize impl c)
   in (orFault r, lg)

-- | Initialize, keeping the full step result.
bootStepped :: (HasCallStack) => ChartImpl LogM spec -> Ctx spec -> Stepped spec
bootStepped impl c = fst (bootLog impl c)

-- | Initialize, keeping only the machine.
boot :: (HasCallStack) => ChartImpl LogM spec -> Ctx spec -> Machine spec
boot impl c = sMachine (bootStepped impl c)

-- | One macrostep, returning the step result and the actions it ran
-- (this macrostep only).
stepLog ::
  (HasCallStack) =>
  ChartImpl LogM spec ->
  Machine spec ->
  StepEvent spec ->
  (Stepped spec, [Text])
stepLog impl m e =
  let (r, lg) = runLog (step impl m e)
   in (orFault r, lg)

-- | One macrostep, keeping the full step result.
stepOnce :: (HasCallStack) => ChartImpl LogM spec -> Machine spec -> StepEvent spec -> Stepped spec
stepOnce impl m e = fst (stepLog impl m e)

-- | One macrostep, keeping only the machine.
advance :: (HasCallStack) => ChartImpl LogM spec -> Machine spec -> StepEvent spec -> Machine spec
advance impl m e = sMachine (stepOnce impl m e)

-- | A sequence of macrosteps, keeping only the final machine.
feed :: (HasCallStack) => ChartImpl LogM spec -> Machine spec -> [StepEvent spec] -> Machine spec
feed impl = foldl' (advance impl)

-- | 'StateMachine.context' under a name that does not clash with
-- 'Test.Syd.context'.
ctxOf :: Machine spec -> Ctx spec
ctxOf = context
