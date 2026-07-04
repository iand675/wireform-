{-# LANGUAGE ScopedTypeVariables #-}

-- | A controllable, fault-injecting in-memory link that plugs the
-- @wireform-dst@ fault vocabulary into the @wireform-network@ transport seam.
--
-- Every wireform networking layer (HTTP\/1, HTTP\/2, gRPC, Connect, Kafka,
-- WebSocket) funnels its bytes through a 'ReceiveFn' \/ 'SendFn' pair and
-- builds a 'DuplexTransport' from it ('newDuplexBufTransport'), or — for the
-- HTTP\/2 engine — consumes a raw @SendFn@ + @readN@ directly. 'newSimLink'
-- produces exactly those, connected by in-memory queues whose byte flow is
-- governed by a runtime-mutable fault policy. So the /real IO stack/ runs
-- unchanged over this link in "simulator mode", while the production
-- socket\/TLS path is untouched — no branch, no dependency, no runtime cost.
--
-- __What this delivers:__ controllable, seeded fault injection against the real
-- stack — directed partition\/heal (stall), @cut@ (peer death → EOF),
-- probabilistic drop, latency ('LatencyDist'), and bit-corruption. Fault
-- /decisions/ are drawn from a seeded splittable PRNG (reproducible under a
-- single-threaded driver). Full byte-for-byte determinism of the stack's own
-- thread scheduling is out of scope (that needs the stack rewritten over an
-- abstract monad, which would erode the hot path); assert liveness/safety with
-- your own probes as in @wireform-dst@.
module Sim.Net.Link
  ( -- * Building a fault link
    SimLink (..)
  , LinkEnd (..)
  , newSimLink
  , newSimLinkWith
  , closeSimLink
  , leReadExactN

    -- * Runtime fault controls
  , LinkControl
  , Direction (..)
  , partition
  , heal
  , cut
  , setDrop
  , setLatency
  , corruptNext

    -- * Re-exported fault vocabulary
  , LinkPolicy (..)
  , LatencyDist (..)
  , Seed (..)
  , SimTime (..)
  , defaultLink
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Data.Bits (shiftL, xor)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.IORef
import Data.Word (Word8)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr)
import Sim.Entropy (Gen, newGen, nextDouble, nextWord64, sampleLatency, splitGen)
import Sim.Types (Seed (..), SimTime (..))
import Sim.World (LatencyDist (..), LinkPolicy (..), defaultLink)
import Wireform.Network
  ( DuplexTransport (..)
  , ReceiveFn
  , SendFn
  , TransportConfig
  , closeDuplexTransport
  , defaultTransportConfig
  , newDuplexBufTransport
  )

-- | The direction a fault applies to, from the client's point of view.
data Direction = ClientToServer | ServerToClient
  deriving stock (Eq, Show)

-- | The mutable fault policy of one direction.
data DirPolicy = DirPolicy
  { dpPartitioned :: !Bool
  -- ^ delivery stalled (bytes held, released on 'heal')
  , dpDrop :: !Double
  -- ^ per-send-chunk drop probability
  , dpLatency :: !LatencyDist
  -- ^ per-send delay (order-preserving)
  , dpCorruptNext :: !Bool
  -- ^ flip a bit in the next delivered chunk
  }

-- One direction's mutable state: the byte buffer, closed flag, fault policy,
-- and a seeded PRNG for drop/corrupt/latency rolls.
data DirState = DirState
  { dsBuf :: !(TVar ByteString)
  , dsClosed :: !(TVar Bool)
  , dsPolicy :: !(TVar DirPolicy)
  , dsGen :: !(IORef Gen)
  }

-- | An opaque handle for injecting faults at runtime.
data LinkControl = LinkControl
  { lcC2S :: !DirState
  , lcS2C :: !DirState
  }

-- | One end of the link: the raw byte primitives plus the assembled
-- 'DuplexTransport'. @leReadN@ / @leSendFn@ feed the HTTP\/2 engine @Config@
-- (and thus gRPC); @leDuplex@ plugs into HTTP\/1 \/ Kafka \/ WebSocket.
data LinkEnd = LinkEnd
  { leReceiveFn :: !ReceiveFn
  , leSendFn :: !SendFn
  , leReadN :: !(Int -> IO ByteString)
  , leShutdown :: !(IO ())
  , leDuplex :: !DuplexTransport
  }

-- | A bidirectional fault link: a client end, a server end, and the control
-- handle. Bytes sent on @slClient@ arrive at @slServer@'s receive (subject to
-- the @ClientToServer@ policy) and vice versa.
data SimLink = SimLink
  { slClient :: !LinkEnd
  , slServer :: !LinkEnd
  , slControl :: !LinkControl
  }

-- | Build a healthy fault link seeded by @seed@ (default transport config, no
-- initial faults, zero latency). Inject faults via 'slControl'.
newSimLink :: Seed -> IO SimLink
newSimLink = newSimLinkWith defaultTransportConfig (defaultLink (Fixed (SimTime 0)))

-- | Build a fault link with an explicit transport config and an initial
-- 'LinkPolicy' applied to /both/ directions.
newSimLinkWith :: TransportConfig -> LinkPolicy -> Seed -> IO SimLink
newSimLinkWith cfg policy (Seed s) = do
  let (g0, g1) = splitGen (newGen s)
      p0 = policyToDir policy
  c2s <- newDirState p0 g0
  s2c <- newDirState p0 g1
  let control = LinkControl c2s s2c
      -- client sends into C2S, receives from S2C; server the mirror.
      clientRecv = mkReceiveFn s2c
      clientSend = mkSendFn c2s
      serverRecv = mkReceiveFn c2s
      serverSend = mkSendFn s2c
  clientDuplex <- newDuplexBufTransport cfg clientRecv clientSend (shutdownDir c2s)
  serverDuplex <- newDuplexBufTransport cfg serverRecv serverSend (shutdownDir s2c)
  let clientEnd =
        LinkEnd clientRecv clientSend (readNFrom s2c) (shutdownDir c2s) clientDuplex
      serverEnd =
        LinkEnd serverRecv serverSend (readNFrom c2s) (shutdownDir s2c) serverDuplex
  pure (SimLink clientEnd serverEnd control)

newDirState :: DirPolicy -> Gen -> IO DirState
newDirState p g =
  DirState <$> newTVarIO BS.empty <*> newTVarIO False <*> newTVarIO p <*> newIORef g

policyToDir :: LinkPolicy -> DirPolicy
policyToDir lp =
  DirPolicy
    { dpPartitioned = lpPartitioned lp
    , dpDrop = lpDrop lp
    , dpLatency = lpLatency lp
    , dpCorruptNext = False
    }

-- | Release the link's rings and mark both directions closed.
closeSimLink :: SimLink -> IO ()
closeSimLink sl = do
  cut (slControl sl)
  closeDuplexTransport (leDuplex (slClient sl))
  closeDuplexTransport (leDuplex (slServer sl))

-- * Byte plumbing ---------------------------------------------------------

-- Receive up to @want@ bytes from a direction's buffer. Blocks while the
-- direction is partitioned (stall) or the buffer is empty; returns 0 at EOF.
mkReceiveFn :: DirState -> ReceiveFn
mkReceiveFn ds dst want = do
  mbs <- atomically $ do
    closed <- readTVar (dsClosed ds)
    buf <- readTVar (dsBuf ds)
    if BS.null buf
      then if closed then pure Nothing else retry
      else do
        part <- dpPartitioned <$> readTVar (dsPolicy ds)
        if part
          then retry -- held: delivery stalled until healed
          else do
            let (h, t) = BS.splitAt want buf
            writeTVar (dsBuf ds) t
            pure (Just h)
  case mbs of
    Nothing -> pure 0
    Just bs -> copyBSInto dst bs >> pure (BS.length bs)

-- Read up to @n@ bytes as a 'ByteString' (EOF ⇒ empty) — the HTTP/2 @readN@.
readNFrom :: DirState -> Int -> IO ByteString
readNFrom ds n = atomically $ do
  closed <- readTVar (dsClosed ds)
  buf <- readTVar (dsBuf ds)
  if BS.null buf
    then if closed then pure BS.empty else retry
    else do
      part <- dpPartitioned <$> readTVar (dsPolicy ds)
      if part
        then retry
        else do
          let (h, t) = BS.splitAt n buf
          writeTVar (dsBuf ds) t
          pure h

-- Send @n@ bytes into a direction, applying latency, drop, and corruption.
-- Sends are non-blocking (Flow-style): a dropped or post-cut chunk still
-- reports success to the sender; the peer observes the loss / EOF.
mkSendFn :: DirState -> SendFn
mkSendFn ds src n = do
  closed <- readTVarIO (dsClosed ds)
  if closed
    then pure n -- silently discard after a cut
    else do
      policy <- readTVarIO (dsPolicy ds)
      applyLatency ds (dpLatency policy)
      dropped <- rollDrop ds (dpDrop policy)
      if dropped
        then pure n
        else do
          bs0 <- BS.packCStringLen (castPtr src, n)
          bs1 <- maybeCorrupt ds policy bs0
          atomically $ modifyTVar' (dsBuf ds) (<> bs1)
          pure n

-- Apply per-send latency (order-preserving: the sender blocks briefly). A
-- 'Fixed' 0 latency (the default) is a no-op and forks nothing.
applyLatency :: DirState -> LatencyDist -> IO ()
applyLatency _ (Fixed (SimTime 0)) = pure ()
applyLatency ds dist = do
  SimTime ns <- sampleLatencyIO (dsGen ds) dist
  let us = fromIntegral (ns `div` 1000)
  if us > 0 then threadDelay us else pure ()

-- Draw a latency sample, advancing the direction's PRNG.
sampleLatencyIO :: IORef Gen -> LatencyDist -> IO SimTime
sampleLatencyIO ref dist =
  atomicModifyIORef' ref $ \g ->
    let (t, g') = sampleLatency dist g in (g', t)

rollDrop :: DirState -> Double -> IO Bool
rollDrop ds p
  | p <= 0 = pure False
  | otherwise = atomicModifyIORef' (dsGen ds) $ \g ->
      let (d, g') = nextDouble g in (g', d < p)

-- If corruption is armed for this direction, flip one bit of the chunk (seeded
-- bit index) and disarm.
maybeCorrupt :: DirState -> DirPolicy -> ByteString -> IO ByteString
maybeCorrupt ds policy bs
  | not (dpCorruptNext policy) || BS.null bs = pure bs
  | otherwise = do
      bitIdx <- atomicModifyIORef' (dsGen ds) $ \g ->
        let (w, g') = nextWord64 g in (g', fromIntegral (w `mod` fromIntegral (BS.length bs * 8)))
      atomically $ modifyTVar' (dsPolicy ds) (\p -> p {dpCorruptNext = False})
      pure (flipBitBS bs bitIdx)

flipBitBS :: ByteString -> Int -> ByteString
flipBitBS bs bit =
  let byteI = bit `div` 8
      bitI = bit `mod` 8
      old = BS.index bs byteI
      new = old `xor` (1 `shiftL` bitI)
   in BS.concat [BS.take byteI bs, BS.singleton new, BS.drop (byteI + 1) bs]

shutdownDir :: DirState -> IO ()
shutdownDir ds = atomically $ writeTVar (dsClosed ds) True

copyBSInto :: Ptr Word8 -> ByteString -> IO ()
copyBSInto dst bs =
  BSU.unsafeUseAsCStringLen bs $ \(src, len) ->
    copyBytes dst (castPtr src) len

-- * Fault controls --------------------------------------------------------

dirOf :: LinkControl -> Direction -> DirState
dirOf lc ClientToServer = lcC2S lc
dirOf lc ServerToClient = lcS2C lc

-- | Stall delivery in a direction: bytes already sent (and any sent while
-- partitioned) are held until 'heal'. Models a network partition / clog.
partition :: LinkControl -> Direction -> IO ()
partition lc dir =
  atomically $ modifyTVar' (dsPolicy (dirOf lc dir)) (\p -> p {dpPartitioned = True})

-- | Heal a partition: held bytes become deliverable again.
heal :: LinkControl -> Direction -> IO ()
heal lc dir =
  atomically $ modifyTVar' (dsPolicy (dirOf lc dir)) (\p -> p {dpPartitioned = False})

-- | Kill the link in both directions: buffers cleared, receivers see EOF,
-- further sends are silently discarded. Models a peer crash / hard close.
cut :: LinkControl -> IO ()
cut lc = atomically $ do
  mapM_ closeDir [lcC2S lc, lcS2C lc]
  where
    closeDir ds = do
      writeTVar (dsClosed ds) True
      writeTVar (dsBuf ds) BS.empty

-- | Set the per-chunk drop probability for a direction.
setDrop :: LinkControl -> Direction -> Double -> IO ()
setDrop lc dir r =
  atomically $ modifyTVar' (dsPolicy (dirOf lc dir)) (\p -> p {dpDrop = r})

-- | Set the latency distribution for a direction (applied per send).
setLatency :: LinkControl -> Direction -> LatencyDist -> IO ()
setLatency lc dir d =
  atomically $ modifyTVar' (dsPolicy (dirOf lc dir)) (\p -> p {dpLatency = d})

-- | Arm one-shot bit corruption of the next chunk delivered in a direction.
corruptNext :: LinkControl -> Direction -> IO ()
corruptNext lc dir =
  atomically $ modifyTVar' (dsPolicy (dirOf lc dir)) (\p -> p {dpCorruptNext = True})

-- | Read exactly @n@ bytes from a 'LinkEnd' (blocking until they arrive),
-- returning fewer only at end-of-input. Matches the exactly-N contract the
-- HTTP\/2 /engine/ 'Config' expects of its @readN@ callback (unlike the field
-- 'leReadN', which returns whatever is currently buffered). Use it to build an
-- engine @Config@ over the link (via
-- @Network.HTTP2.Engine.Client.allocConfigForTransport@) for gRPC \/ Connect.
leReadExactN :: LinkEnd -> Int -> IO ByteString
leReadExactN end = go []
  where
    go acc remaining
      | remaining <= 0 = pure (BS.concat (reverse acc))
      | otherwise = do
          chunk <- leReadN end remaining
          if BS.null chunk
            then pure (BS.concat (reverse acc))
            else go (chunk : acc) (remaining - BS.length chunk)
