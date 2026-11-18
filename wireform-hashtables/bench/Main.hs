{-# LANGUAGE ScopedTypeVariables #-}

{- | Mutable hash tables head to head.

Every competitor is driven by the same @INLINE@ loop over a pre-generated
unboxed key vector, so the probe loops are compiled at concrete types and
nobody pays an indirect call the others don't. Report names are
@<op>/<n>/<impl>@.

Key sets: @seq@ is @0 .. n-1@ (what vector-hashtables' own benchmark uses,
and the best case for identity-hash chaining tables); @rand@ is n
pseudo-random 63-bit keys; misses are drawn from a disjoint stream.
-}
module Main (main) where

import Control.DeepSeq (NFData (..), rwhnf)
import Control.Monad.Primitive (PrimState)
import Criterion.Main
import qualified Data.HashMap.Strict as HM
import qualified Data.HashTable.IO as HT
import qualified Data.Vector.Hashtables as VH
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Exts (RealWorld)
import qualified Hashtable as H

type Ours = H.Dictionary RealWorld H.Unboxed Int H.Unboxed Int

type VHT = VH.Dictionary (PrimState IO) UM.MVector Int UM.MVector Int

type Basic = HT.BasicHashTable Int Int

type Cuckoo = HT.CuckooHashTable Int Int

newtype WHNF a = WHNF a

instance NFData (WHNF a) where rnf = rwhnf

------------------------------------------------------------------------
-- Keys

-- | PCG-style LCG stream, top 63 bits: positive, well spread, no deps.
stream :: Int -> Int -> U.Vector Int
stream seed n = U.unfoldrExactN n step (fromIntegral seed :: Word)
 where
  step x =
    let x' = x * 6364136223846793005 + 1442695040888963407
     in (fromIntegral (x' `div` 2), x')

data Keys = Keys
  { kName :: String
  , kHit :: !(U.Vector Int)
  , kMiss :: !(U.Vector Int)
  }

keySets :: Int -> [Keys]
keySets n =
  [ Keys "seq" (U.enumFromN 0 n) (U.enumFromN n n)
  , Keys "rand" (stream 1 n) (stream 2 n)
  ]

forKeys :: U.Vector Int -> (Int -> IO ()) -> IO ()
forKeys ks f = go 0
 where
  n = U.length ks
  go i
    | i >= n = pure ()
    | otherwise = f (U.unsafeIndex ks i) >> go (i + 1)
{-# INLINE forKeys #-}

sumKeys :: U.Vector Int -> (Int -> IO Int) -> IO Int
sumKeys ks f = go 0 0
 where
  n = U.length ks
  go !i !acc
    | i >= n = pure acc
    | otherwise = f (U.unsafeIndex ks i) >>= \x -> go (i + 1) (acc + x)
{-# INLINE sumKeys #-}

------------------------------------------------------------------------
-- Builders (also the insert benchmarks)

buildOurs :: Int -> U.Vector Int -> IO Ours
buildOurs cap ks = do
  t <- H.initialize cap
  forKeys ks $ \k -> H.insert t k k
  pure t

buildVH :: Int -> U.Vector Int -> IO VHT
buildVH cap ks = do
  t <- VH.initialize cap
  forKeys ks $ \k -> VH.insert t k k
  pure t

buildBasic :: Int -> U.Vector Int -> IO Basic
buildBasic cap ks = do
  t <- HT.newSized (max 1 cap)
  forKeys ks $ \k -> HT.insert t k k
  pure t

buildCuckoo :: Int -> U.Vector Int -> IO Cuckoo
buildCuckoo cap ks = do
  t <- HT.newSized (max 1 cap)
  forKeys ks $ \k -> HT.insert t k k
  pure t

buildHM :: U.Vector Int -> HM.HashMap Int Int
buildHM = U.foldl' (\m k -> HM.insert k k m) HM.empty

------------------------------------------------------------------------
-- Suites

insertSuite :: String -> (Int -> Int) -> Int -> Keys -> Benchmark
insertSuite label capFor n ks =
  bgroup
    (label <> "/" <> kName ks <> "/" <> show n)
    [ bench "wireform-hashtables" $ whnfIO (WHNF <$> buildOurs (capFor n) (kHit ks))
    , bench "wireform-hashtables-batch" $ whnfIO $ do
        t <- H.initialize (capFor n) :: IO Ours
        H.insertBatch t (kHit ks) (kHit ks)
        pure (WHNF t)
    , bench "vector-hashtables" $ whnfIO (WHNF <$> buildVH (capFor n) (kHit ks))
    , bench "hashtables-basic" $ whnfIO (WHNF <$> buildBasic (capFor n) (kHit ks))
    , bench "hashtables-cuckoo" $ whnfIO (WHNF <$> buildCuckoo (capFor n) (kHit ks))
    , bench "unordered-containers" $ whnf buildHM (kHit ks)
    ]

data Built = Built !Ours !VHT !Basic !Cuckoo !(HM.HashMap Int Int)

instance NFData Built where rnf = rwhnf

buildAll :: Int -> Keys -> IO Built
buildAll n ks =
  Built
    <$> buildOurs n (kHit ks)
    <*> buildVH n (kHit ks)
    <*> buildBasic n (kHit ks)
    <*> buildCuckoo n (kHit ks)
    <*> pure (buildHM (kHit ks))

lookupSuite :: String -> (Keys -> U.Vector Int) -> Int -> Keys -> Benchmark
lookupSuite label probe n ks =
  env (buildAll n ks) $ \ ~(Built o v b c hm) ->
    let qs = probe ks
     in bgroup
          (label <> "/" <> kName ks <> "/" <> show n)
          [ bench "wireform-hashtables" $ whnfIO $ sumKeys qs (H.findWithDefault o 0)
          , bench "wireform-hashtables-batch" $ whnfIO $ U.sum <$> (H.lookupBatch o 0 qs :: IO (U.Vector Int))
          , bench "vector-hashtables" $ whnfIO $ sumKeys qs (\k -> maybe 0 id <$> VH.lookup v k)
          , bench "hashtables-basic" $ whnfIO $ sumKeys qs (\k -> maybe 0 id <$> HT.lookup b k)
          , bench "hashtables-cuckoo" $ whnfIO $ sumKeys qs (\k -> maybe 0 id <$> HT.lookup c k)
          , bench "unordered-containers" $ whnf (U.foldl' (\acc k -> acc + HM.lookupDefault 0 k hm) 0) qs
          ]

-- | Insert everything, then delete everything (presized).
churnSuite :: Int -> Keys -> Benchmark
churnSuite n ks =
  let xs = kHit ks
   in bgroup
        ("insert-delete/" <> kName ks <> "/" <> show n)
        [ bench "wireform-hashtables" $ whnfIO $ do
            t <- buildOurs n xs
            forKeys xs (H.delete t)
            WHNF <$> pure t
        , bench "vector-hashtables" $ whnfIO $ do
            t <- buildVH n xs
            forKeys xs (VH.delete t)
            WHNF <$> pure t
        , bench "hashtables-basic" $ whnfIO $ do
            t <- buildBasic n xs
            forKeys xs (HT.delete t)
            WHNF <$> pure t
        , bench "hashtables-cuckoo" $ whnfIO $ do
            t <- buildCuckoo n xs
            forKeys xs (HT.delete t)
            WHNF <$> pure t
        , bench "unordered-containers" $ whnf (\m -> HM.size (U.foldl' (flip HM.delete) m xs)) (buildHM xs)
        ]

-- | Histogram: n increments over n/8 distinct keys, from an empty table.
upsertSuite :: Int -> Keys -> Benchmark
upsertSuite n ks =
  let xs = U.map (`rem` max 1 (n `quot` 8)) (kHit ks)
   in bgroup
        ("upsert-count/" <> kName ks <> "/" <> show n)
        [ bench "wireform-hashtables" $ whnfIO $ do
            t <- H.new :: IO Ours
            forKeys xs (H.upsert t (maybe 1 (+ 1)))
            WHNF <$> pure t
        , bench "wireform-hashtables-batch" $ whnfIO $ do
            t <- H.new :: IO Ours
            H.upsertBatch t (maybe 1 (+ 1)) xs
            WHNF <$> pure t
        , bench "vector-hashtables" $ whnfIO $ do
            t <- VH.initialize 0 :: IO VHT
            forKeys xs (VH.upsert t (maybe 1 (+ 1)))
            WHNF <$> pure t
        , bench "hashtables-basic" $ whnfIO $ do
            t <- HT.new :: IO Basic
            forKeys xs (\k -> HT.mutate t k (\m -> (Just $! maybe 1 (+ 1) m, ())))
            WHNF <$> pure t
        , bench "hashtables-cuckoo" $ whnfIO $ do
            t <- HT.new :: IO Cuckoo
            forKeys xs (\k -> HT.mutate t k (\m -> (Just $! maybe 1 (+ 1) m, ())))
            WHNF <$> pure t
        , bench "unordered-containers" $ whnf (\v -> HM.size (U.foldl' (\m k -> HM.insertWith (+) k (1 :: Int) m) HM.empty v)) xs
        ]

toListSuite :: Int -> Keys -> Benchmark
toListSuite n ks =
  env (buildAll n ks) $ \ ~(Built o v b c hm) ->
    bgroup
      ("toList/" <> kName ks <> "/" <> show n)
      [ bench "wireform-hashtables" $ nfIO (H.toList o)
      , bench "vector-hashtables" $ nfIO (VH.toList v)
      , bench "hashtables-basic" $ nfIO (HT.toList b)
      , bench "hashtables-cuckoo" $ nfIO (HT.toList c)
      , bench "unordered-containers" $ nf HM.toList hm
      ]

main :: IO ()
main = defaultMain $ do
  n <- [1000, 100000, 1000000]
  ks <- keySets n
  [ insertSuite "insert" id n ks
    , insertSuite "insert-grow" (const 0) n ks
    , lookupSuite "lookup-hit" kHit n ks
    , lookupSuite "lookup-miss" kMiss n ks
    , churnSuite n ks
    , upsertSuite n ks
    , toListSuite n ks
    ]
