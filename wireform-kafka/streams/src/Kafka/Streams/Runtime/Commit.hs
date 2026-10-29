{- |
Module      : Kafka.Streams.Runtime.Commit
Description : External systems that commit with the runtime (two-phase commit)

A 'CommitParticipant' is a system outside Kafka (a database, an object
store, an HTTP endpoint) whose writes commit and abort together with
the runtime's commits — the shape of
"Kafka.Streams.Sinks.TwoPhase" sinks. Every commit cycle has an id
(@\<instance\>/\<n\>@, @n@ increasing per instance):

  1. once every task has flushed, each participant /prepares/ the
     cycle (durable, but invisible downstream); a failure aborts the
     cycle;
  2. the runtime records the cycle as committed in its
     @\<application.id\>-twophase-commits@ topic — inside the Kafka
     transaction under exactly-once, so the record exists exactly when
     the transaction committed; at least once, before the offsets are
     committed (a crash in between replays the input: duplicates, never
     loss);
  3. each participant /commits/ the cycle; on any failure before that,
     it /aborts/ it.

When the runtime rewinds to the last commit (an aborted cycle, a
recovered stream thread, or under exactly-once a lost partition),
every participant /discards/ what it staged since its last prepare.

After a crash a participant may hold prepared cycles. At start the
runtime reads the topic (the last committed cycle per instance) and
commits each prepared cycle at or below it, aborting the rest
('cpRecover').
-}
module Kafka.Streams.Runtime.Commit (
  CommitParticipant (..),
  CycleId (..),
  renderCycleId,
  parseCycleId,
) where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR


-- | A commit cycle: the instance that ran it and its sequence number there.
data CycleId = CycleId
  { cycleInstance :: !Text
  , cycleNumber :: !Int64
  }
  deriving stock (Eq, Ord, Show)


renderCycleId :: CycleId -> Text
renderCycleId (CycleId i n) = i <> "/" <> T.pack (show n)


parseCycleId :: Text -> Maybe CycleId
parseCycleId t = case T.breakOnEnd "/" t of
  (pre, num)
    | not (T.null pre)
    , Right (n, rest) <- TR.decimal num
    , T.null rest ->
        Just (CycleId (T.dropEnd 1 pre) n)
  _ -> Nothing


data CommitParticipant = CommitParticipant
  { cpName :: !Text
  , cpPrepare :: !(Text -> IO (Either Text ()))
  -- ^ Make what was staged since the last cycle durable under this cycle id
  , cpCommit :: !(Text -> IO (Either Text ()))
  -- ^ Make the cycle visible (idempotent: may be repeated after a crash)
  , cpAbort :: !(Text -> IO ())
  -- ^ Discard the cycle (idempotent)
  , cpDiscard :: !(IO ())
  {- ^ Drop what was staged since the last prepare: the runtime is
  rewinding its input to the last commit and will reprocess it
  -}
  , cpRecover :: !(IO [Text])
  -- ^ Cycles prepared and neither committed nor aborted
  }
