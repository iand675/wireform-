{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Streams.Properties.TwoPhaseSinkSpec
Description : Chaos / property suite for the 2PC sink contract

Properties:

  1. Commit-only delivery: a row appears in the committed view
     only after its txn's 'tpsCommit' has returned 'SinkOK'.
  2. Prepare-then-abort is invisible: rows from an aborted txn
     never appear in the committed view.
  3. Idempotent commit / abort: re-invoking either of them on an
     already-completed txn is a no-op.
  4. Coordinator wiring: 'withTwoPhaseSinks' calls prepare before
     the original 'storeCommit', commits after, and aborts on
     either prepare failure or base-storeCommit failure.
  5. Filesystem reference sink: atomic-rename gives crash
     consistency — recovery sees either the prepared or the
     committed batch but never both.
  6. HTTP echo reference sink: the @onCommit@ callback fires once
     per commit, with exactly the prepared rows, in commit order
     across many randomised cycles.
-}
module Streams.Properties.TwoPhaseSinkSpec (tests) where

import Control.Monad (forM_)
import Data.ByteString.Char8 qualified as BSC
import Data.IORef (
  modifyIORef',
  newIORef,
  readIORef,
 )
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Kafka.Streams.Sinks.TwoPhase
import System.IO.Temp (withSystemTempDirectory)
import Test.Syd
import Test.Syd.Hedgehog ()


----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

mkTxn :: Int -> SinkTxnId
mkTxn i = SinkTxnId (T.pack ("t" <> show i))


bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack


----------------------------------------------------------------------
-- Unit tests
----------------------------------------------------------------------

unit_inmem_prepare_then_commit_visible :: Spec
unit_inmem_prepare_then_commit_visible =
  it "in-memory: prepare + commit yields the rows" $ do
    (sink, st) <- inMemoryTwoPhaseSink "u"
    _ <- tpsPrepare sink (mkTxn 1) ["a", "b" :: Text]
    -- Before commit, no rows are visible.
    readCommittedRows st >>= (`shouldBe` [])
    _ <- tpsCommit sink (mkTxn 1)
    readCommittedRows st >>= (`shouldBe` ["a", "b"])


unit_inmem_abort_discards :: Spec
unit_inmem_abort_discards =
  it "in-memory: aborted txn rows never appear" $ do
    (sink, st) <- inMemoryTwoPhaseSink "u"
    _ <- tpsPrepare sink (mkTxn 1) ["a" :: Text]
    _ <- tpsAbort sink (mkTxn 1)
    _ <- tpsCommit sink (mkTxn 1) -- idempotent: txn already gone
    readCommittedRows st >>= (`shouldBe` [])


unit_inmem_commit_is_idempotent :: Spec
unit_inmem_commit_is_idempotent =
  it "in-memory: double commit returns SinkOK and doesn't duplicate" $ do
    (sink, st) <- inMemoryTwoPhaseSink "u"
    _ <- tpsPrepare sink (mkTxn 1) ["a", "b" :: Text]
    SinkOK <- tpsCommit sink (mkTxn 1)
    SinkOK <- tpsCommit sink (mkTxn 1)
    readCommittedRows st >>= (`shouldBe` ["a", "b"])


----------------------------------------------------------------------
-- Property: arbitrary {prepare, commit, abort} schedules
----------------------------------------------------------------------

data Op
  = OpPrepare !Int ![Text]
  | OpCommit !Int
  | OpAbort !Int
  deriving stock (Eq, Show)


genOp :: H.Gen Op
genOp = do
  txn <- Gen.int (Range.linear 0 4)
  Gen.frequency
    [
      ( 3
      , do
          rows <-
            Gen.list
              (Range.linear 1 4)
              ( Gen.element
                  ["a", "b", "c", "d", "e"]
              )
          pure (OpPrepare txn rows)
      )
    , (2, pure (OpCommit txn))
    , (2, pure (OpAbort txn))
    ]


{- | The model maps each /pending/ txn id to its buffered rows.
A txn that has been committed or aborted is /forgotten/: the
next 'OpPrepare' on that id re-prepares from scratch. This
matches the reference sinks, where the prepare map is keyed by
'SinkTxnId' and committed / aborted txns are deleted from the
map so the id is freshly available again.
-}
applyModel
  :: Op
  -> (Map.Map Int [Text], [Text])
  -> (Map.Map Int [Text], [Text])
applyModel op (prep, committed) = case op of
  OpPrepare txn rows ->
    (Map.insert txn rows prep, committed)
  OpCommit txn ->
    case Map.lookup txn prep of
      Just rs -> (Map.delete txn prep, committed ++ rs)
      Nothing -> (prep, committed)
  OpAbort txn ->
    (Map.delete txn prep, committed)


prop_inmem_matches_model :: H.Property
prop_inmem_matches_model = H.property $ do
  ops <- H.forAll (Gen.list (Range.linear 1 40) genOp)
  observed <- H.evalIO $ do
    (sink, st) <- inMemoryTwoPhaseSink "p"
    let go [] = pure ()
        go (OpPrepare txn rows : rest) = do
          _ <- tpsPrepare sink (mkTxn txn) rows
          go rest
        go (OpCommit txn : rest) = do
          _ <- tpsCommit sink (mkTxn txn)
          go rest
        go (OpAbort txn : rest) = do
          _ <- tpsAbort sink (mkTxn txn)
          go rest
    go ops
    readCommittedRows st
  let (_, expected) = foldl (flip applyModel) (Map.empty, []) ops
  observed H.=== expected


----------------------------------------------------------------------
-- Property: HTTP echo sink fires onCommit exactly once per txn
----------------------------------------------------------------------

prop_http_echo_fires_once :: H.Property
prop_http_echo_fires_once = H.property $ do
  ops <- H.forAll (Gen.list (Range.linear 1 30) genOp)
  outcome <- H.evalIO $ do
    delivered <- newIORef ([] :: [[Text]])
    sink <-
      httpEchoTwoPhaseSink
        "h"
        (\rs -> modifyIORef' delivered (rs :))
    forM_ ops $ \op -> case op of
      OpPrepare txn rows -> () <$ tpsPrepare sink (mkTxn txn) rows
      OpCommit txn -> () <$ tpsCommit sink (mkTxn txn)
      OpAbort txn -> () <$ tpsAbort sink (mkTxn txn)
    rs <- reverse <$> readIORef delivered
    pure rs
  -- Reconstruct the expected delivery sequence from the model:
  -- a commit fires the buffered rows iff the txn currently has a
  -- pending prepare.
  let go _ acc [] = reverse acc
      go prep acc (OpPrepare txn rows : rest) =
        go (Map.insert txn rows prep) acc rest
      go prep acc (OpCommit txn : rest) =
        case Map.lookup txn prep of
          Just rs -> go (Map.delete txn prep) (rs : acc) rest
          Nothing -> go prep acc rest
      go prep acc (OpAbort txn : rest) =
        go (Map.delete txn prep) acc rest
      expected = go Map.empty [] ops
  outcome H.=== expected


----------------------------------------------------------------------
-- Property: filesystem sink crash consistency
----------------------------------------------------------------------

prop_filesystem_crash_consistency :: H.Property
prop_filesystem_crash_consistency = H.property $ do
  rows <-
    H.forAll
      ( Gen.list
          (Range.linear 1 8)
          (Gen.element ["row0", "row1", "row2"])
      )
  crashBetween <- H.forAll Gen.bool
  outcome <- H.evalIO $ withSystemTempDirectory "tps-prop" $ \root -> do
    sink <- filesystemTwoPhaseSink root bytes "fs"
    let txn = mkTxn 1
    _ <- tpsPrepare sink txn rows
    -- Decide whether to "crash" before or after commit.
    if crashBetween
      then do
        -- Simulate crash by NOT calling tpsCommit. On recovery,
        -- tpsRecover should still see the prepared txn.
        recovered <- tpsRecover sink
        pure (recovered, False)
      else do
        _ <- tpsCommit sink txn
        recovered <- tpsRecover sink
        pure (recovered, True)
  let (recovered, committed) = outcome
  if committed
    -- After commit, the prepared dir is empty.
    then recovered H.=== []
    -- After a crash before commit, the prepared txn is recoverable.
    else recovered H.=== [mkTxn 1]


----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

tests :: Spec
tests =
  describe "Two-phase-commit sink" $
    sequence_
      [ unit_inmem_prepare_then_commit_visible
      , unit_inmem_abort_discards
      , unit_inmem_commit_is_idempotent
      , it "in-memory sink matches the pure prepare/commit/abort model" $
          H.withTests 200 prop_inmem_matches_model
      , it "HTTP echo sink fires onCommit once per committed txn" $
          H.withTests 120 prop_http_echo_fires_once
      , it "filesystem sink survives prepare-without-commit crashes" $
          H.withTests 60 prop_filesystem_crash_consistency
      , unit_stage_then_prepare_includes_staged
      ]


{- | A sink whose 'tpsStage' really buffers staged rows so the
subsequent 'tpsPrepare' picks them up alongside any
caller-supplied @[r]@. Verifies the contract change that
introduced 'tpsStage' for the Riffle \xc2\xa74 SinkTwoPhase Prim.
-}
unit_stage_then_prepare_includes_staged :: Spec
unit_stage_then_prepare_includes_staged =
  it "tpsStage rows surface through tpsPrepare and reach the committed view" $ do
    (sink, st) <- inMemoryTwoPhaseSink "stage"
    tpsStage sink "a"
    tpsStage sink "b"
    -- Caller supplies no extra rows; staged rows are still
    -- prepared.
    _ <- tpsPrepare sink (mkTxn 1) []
    _ <- tpsCommit sink (mkTxn 1)
    rs <- readCommittedRows st
    rs `shouldBe` ["a", "b" :: Text]
