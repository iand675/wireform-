{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Systematic (dejafu) concurrency tests for the HPACK send pipeline.
--
-- HPACK's dynamic table is /stateful/: the encoder mutates it as it
-- produces header blocks, and the peer's decoder replays those mutations
-- **in the order the blocks arrive on the wire** (RFC 7541 §2.3.2). A
-- block can reference dynamic-table entries inserted by an earlier block,
-- so if the order in which blocks reach the wire differs from the order
-- in which they were encoded, the decoder desyncs and mis-decodes every
-- subsequent block.
--
-- This is a real bug class in the wireform-http2 senders. The HPACK
-- encoder is guarded by one lock (@connHpackEncoder@) and the wire by
-- another (@connSendLock@). If a sender encodes under the encoder lock,
-- releases it, then sends under the send lock as a /separate/ critical
-- section, two concurrent streams can encode in one order and reach the
-- wire in the other — desyncing the peer. The fix (and the contract these
-- tests pin) is to hold the send lock across /both/ the encode and the
-- send, as 'Network.HTTP2.Connection.encodeAndSendHeaderBlock' does.
--
-- These programs model exactly that: a shared real encoder, a "wire"
-- (frames in send order), and a single decoder that replays the wire in
-- order. The real 'encodeHeaderBlock' / 'decodeHeaderBlock' run via
-- 'liftIO'; dejafu controls only the order of the (atomic) critical
-- sections — precisely where the bug lives. Each schedule re-runs the
-- program from scratch, so the 'liftIO'-allocated tables are fresh per
-- run (the result is a pure function of the schedule).
--
--   * 'twoLockProgram' (the bug): encode under the encoder lock, then
--     append to the wire under a /separate/ send lock. dejafu finds a
--     schedule where the wire order diverges from the encode order and the
--     replay desyncs.
--
--   * 'oneLockProgram' (the fix): encode and append to the wire in /one/
--     send-lock critical section. dejafu proves every schedule replays
--     correctly.
module Test.HPACKDejaFu (tests) where

import qualified Control.Concurrent.Classy as C
import Control.Monad (forM_)
import Data.Either (rights)
import Data.List (sort)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Test.DejaFu (ConcIO, Condition, defaultMemType, defaultWay)
import Test.DejaFu.SCT (runSCT)
import Test.Syd

import Network.HTTP2.HPACK

-- | Number of concurrent senders. Kept small so dejafu's systematic
-- exploration stays exhaustive and fast.
nWorkers :: Int
nWorkers = 3

-- | Each sender encodes a header list that shares field /names/ with the
-- others but has distinct /values/. Shared names force later blocks to
-- reference dynamic-table entries inserted by earlier blocks, which is
-- what makes out-of-order delivery observable as a decode failure.
workerHeaders :: Int -> [(BS.ByteString, BS.ByteString)]
workerHeaders i =
  [ (":method", if even i then "GET" else "POST")
  , (":path", BS8.pack ("/worker/" <> show i))
  , (":scheme", "https")
  , (":authority", "example.com")
  , ("x-worker-id", BS8.pack (show i))
  ]

-- | The list of header lists every run must reproduce, in some order.
expected :: [[(BS.ByteString, BS.ByteString)]]
expected = map workerHeaders [1 .. nWorkers]

-- | Replay the recorded wire (in send order) through one fresh decoder and
-- decide whether it round-tripped: every block decoded, and the multiset
-- of decoded header lists equals the multiset of inputs. Any desync shows
-- up as a decode error or a wrong header list, breaking the equality.
checkWire :: [BS.ByteString] -> ConcIO Bool
checkWire frames = do
  dec <- liftIO (newDynamicTable 4096)
  decoded <- liftIO (mapM (decodeHeaderBlock dec) frames)
  pure (length (rights decoded) == nWorkers && sort (rights decoded) == sort expected)

-- | BUGGY pipeline: encode (encoder lock) and append-to-wire (send lock)
-- are two separate critical sections, so the wire order can diverge from
-- the encode order.
twoLockProgram :: ConcIO Bool
twoLockProgram = do
  enc <- liftIO (newDynamicTable 4096)
  encL <- C.newMVar ()
  wire <- C.newMVar []
  done <- C.newEmptyMVar
  forM_ [1 .. nWorkers] $ \i -> C.fork $ do
    blk <- C.withMVar encL $ \() ->
      liftIO (encodeHeaderBlock defaultEncodeStrategy enc (workerHeaders i))
    C.modifyMVar_ wire $ \w -> pure (blk : w)
    C.putMVar done ()
  forM_ [1 .. nWorkers] $ \_ -> C.takeMVar done
  frames <- reverse <$> C.takeMVar wire
  checkWire frames

-- | FIXED pipeline: encode and append-to-wire happen in ONE send-lock
-- critical section, so the wire order always matches the encode order.
oneLockProgram :: ConcIO Bool
oneLockProgram = do
  enc <- liftIO (newDynamicTable 4096)
  encL <- C.newMVar ()
  wire <- C.newMVar []
  done <- C.newEmptyMVar
  forM_ [1 .. nWorkers] $ \i -> C.fork $ do
    C.modifyMVar_ wire $ \w -> do
      blk <- C.withMVar encL $ \() ->
        liftIO (encodeHeaderBlock defaultEncodeStrategy enc (workerHeaders i))
      pure (blk : w)
    C.putMVar done ()
  forM_ [1 .. nWorkers] $ \_ -> C.takeMVar done
  frames <- reverse <$> C.takeMVar wire
  checkWire frames

-- | A schedule is consistent when it terminated normally and the wire
-- replayed correctly.
scheduleConsistent :: (Either Condition Bool, a) -> Bool
scheduleConsistent (Right ok, _) = ok
scheduleConsistent (Left _, _) = False

tests :: Spec
tests = describe "HPACK send pipeline (dejafu systematic schedules)" $ do
  it "encode + send in two separate locks: some schedule desyncs the decoder" $ do
    results <- runSCT defaultWay defaultMemType twoLockProgram
    not (null results) `shouldBe` True
    -- The split critical sections admit a schedule whose wire order differs
    -- from the encode order, desyncing the replay — so NOT every schedule
    -- is consistent.
    all scheduleConsistent results `shouldBe` False
  it "encode + send under one lock: every schedule replays correctly" $ do
    results <- runSCT defaultWay defaultMemType oneLockProgram
    not (null results) `shouldBe` True
    all scheduleConsistent results `shouldBe` True
