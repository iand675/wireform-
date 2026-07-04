-- | Deterministic, splittable entropy for the simulator, plus the fixed
-- fingerprint mixer and latency sampler. This is the /only/ sanctioned
-- source of randomness in the library — 'System.Random' and
-- 'Data.Hashable' are banned (see @docs@ / the determinism bans), because
-- reproducibility across runs and machines is the whole point.
--
-- Backed by @splitmix@'s 'SMGen'. Splitting yields two independent streams
-- so different fibers / search branches never share a stream.
module Sim.Entropy
  ( Gen (..)
  , newGen
  , splitGen
  , nextWord64
  , nextDouble
  , uniformR
  , mixKey
  , LatencyDist (..)
  , sampleLatency
  ) where

import Control.DeepSeq (NFData (..))
import Data.Bits (xor, shiftR)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Sim.Types (SimTime (..), StateKey (..))
import System.Random.SplitMix (SMGen)
import System.Random.SplitMix qualified as SM

-- | A splittable deterministic PRNG state.
newtype Gen = Gen SMGen
  deriving stock (Generic)

-- | 'SMGen' has no 'NFData'; it is a strict pair of 'Word64' internally, so
-- forcing it to WHNF fully evaluates it.
instance NFData Gen where
  rnf (Gen g) = g `seq` ()

-- | Seed a generator deterministically from a 'Seed'.
newGen :: Word64 -> Gen
newGen = Gen . SM.mkSMGen
{-# INLINE newGen #-}

-- | Split into two independent streams.
splitGen :: Gen -> (Gen, Gen)
splitGen (Gen g) = let (a, b) = SM.splitSMGen g in (Gen a, Gen b)
{-# INLINE splitGen #-}

-- | Draw a uniform 'Word64' and advance the stream.
nextWord64 :: Gen -> (Word64, Gen)
nextWord64 (Gen g) = let (w, g') = SM.nextWord64 g in (w, Gen g')
{-# INLINE nextWord64 #-}

-- | Draw a uniform 'Double' in @[0,1)@ and advance the stream.
nextDouble :: Gen -> (Double, Gen)
nextDouble (Gen g) = let (d, g') = SM.nextDouble g in (d, Gen g')
{-# INLINE nextDouble #-}

-- | Draw an 'Int' uniformly in the inclusive range @[lo, hi]@. If @lo > hi@
-- the range is treated as empty and @lo@ is returned unchanged (defensive;
-- callers should pass non-empty ranges).
uniformR :: (Int, Int) -> Gen -> (Int, Gen)
uniformR (lo, hi) gen
  | hi <= lo = (lo, gen)
  | otherwise =
      let !spanSz = fromIntegral (hi - lo) + 1 :: Word64
          (w, gen') = nextWord64 gen
          !off = fromIntegral (w `mod` spanSz)
       in (lo + off, gen')
{-# INLINE uniformR #-}

-- | Fixed splitmix-style finalizing mix of a list of 'Word64's into a
-- 'StateKey' fingerprint. Deterministic, order-sensitive, and independent of
-- 'Data.Hashable' so fingerprints are stable across runs and machines.
--
-- Uses the SplitMix64 finalizer (Stafford variant 13) folded over the
-- inputs with a fixed initial constant.
mixKey :: [Word64] -> StateKey
mixKey = StateKey . go 0x9E3779B97F4A7C15
  where
    go !acc [] = finalize acc
    go !acc (x : xs) = go (finalize (acc `xor` x) + 0x9E3779B97F4A7C15) xs
    finalize z0 =
      let z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xBF58476D1CE4E5B9
          z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94D049BB133111EB
       in z2 `xor` (z2 `shiftR` 31)
{-# INLINE mixKey #-}

-- | A latency distribution for a network link. Times are virtual ns.
data LatencyDist
  = Fixed !SimTime
  | UniformMs !Int !Int
  | ExponentialMs !Double
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Sample a latency from a distribution, advancing the stream. Results are
-- clamped to be non-negative.
sampleLatency :: LatencyDist -> Gen -> (SimTime, Gen)
sampleLatency dist gen = case dist of
  Fixed t -> (t, gen)
  UniformMs loMs hiMs ->
    let (ms, gen') = uniformR (loMs, hiMs) gen
     in (msToNs (max 0 ms), gen')
  ExponentialMs meanMs ->
    let (u, gen') = nextDouble gen
        -- inverse-CDF sample of Exp(1/mean); guard log(0)
        !u' = if u <= 0 then 1e-12 else u
        !ms = negate (log u') * meanMs
     in (msToNs (max 0 (round ms)), gen')
  where
    msToNs :: Int -> SimTime
    msToNs ms = SimTime (fromIntegral ms * 1000000)
{-# INLINE sampleLatency #-}
