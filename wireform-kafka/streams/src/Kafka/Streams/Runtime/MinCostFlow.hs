{-# LANGUAGE BangPatterns #-}

{- |
Module      : Kafka.Streams.Runtime.MinCostFlow
Description : Min-cost max-flow, for the rack-aware task assignor

Successive shortest paths over the residual graph, each found with
Bellman-Ford (SPFA): costs may be negative on residual edges, and the
graphs the assignor builds (tasks x clients) are small enough that
the simple algorithm is fast. Integer capacities and costs.
-}
module Kafka.Streams.Runtime.MinCostFlow (
  Edge (..),
  minCostFlow,
) where

import Control.Monad (forM_, unless, when)
import Control.Monad.ST (runST)
import Data.Sequence (Seq, ViewL (..), viewl, (|>))
import Data.Sequence qualified as Seq
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Vector.Unboxed qualified as UV
import Data.Vector.Unboxed.Mutable qualified as UMV


-- | A directed edge: from, to, capacity, cost per unit of flow.
data Edge = Edge
  { eFrom :: {-# UNPACK #-} !Int
  , eTo :: {-# UNPACK #-} !Int
  , eCap :: {-# UNPACK #-} !Int
  , eCost :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Show)


{- | The flow on each of @edges@ (in order) of a maximum flow from @s@ to
@t@ of least total cost, over nodes @0 .. n - 1@.
-}
minCostFlow :: Int -> Int -> Int -> [Edge] -> [Int]
minCostFlow n s t edges = map (flows UV.!) (take (length edges) [0, 2 ..])
  where
    m = 2 * length edges
    flows :: UV.Vector Int
    flows = runST $ do
      -- edge 2i is the i-th edge, 2i + 1 its reverse
      to <- UV.thaw (UV.fromList (concatMap (\e -> [eTo e, eFrom e]) edges))
      cap <- UV.thaw (UV.fromList (concatMap (\e -> [eCap e, 0]) edges))
      cost <- UV.thaw (UV.fromList (concatMap (\e -> [eCost e, negate (eCost e)]) edges))
      flow <- UMV.replicate m (0 :: Int)
      -- adjacency as linked lists of edge indices
      headA <- UMV.replicate n (-1 :: Int)
      nextA <- UMV.replicate m (-1 :: Int)
      forM_ (zip [0 ..] edges) $ \(i, e) -> do
        let link from ix = do
              h <- UMV.unsafeRead headA from
              UMV.unsafeWrite nextA ix h
              UMV.unsafeWrite headA from ix
        link (eFrom e) (2 * i)
        link (eTo e) (2 * i + 1)
      dist <- UMV.replicate n (maxBound :: Int)
      inQ <- UMV.replicate n False
      via <- UMV.replicate n (-1 :: Int)
      let residual ix = (-) <$> UMV.unsafeRead cap ix <*> UMV.unsafeRead flow ix
          -- shortest paths from s in the residual graph
          spfa = do
            forM_ [0 .. n - 1] $ \v -> UMV.unsafeWrite dist v maxBound >> UMV.unsafeWrite via v (-1) >> UMV.unsafeWrite inQ v False
            UMV.unsafeWrite dist s 0
            q <- newSTRef (Seq.singleton s :: Seq Int)
            UMV.unsafeWrite inQ s True
            let loop = do
                  qs <- readSTRef q
                  case viewl qs of
                    EmptyL -> pure ()
                    u :< rest -> do
                      writeSTRef q rest
                      UMV.unsafeWrite inQ u False
                      du <- UMV.unsafeRead dist u
                      let walk ix
                            | ix < 0 = pure ()
                            | otherwise = do
                                r <- residual ix
                                when (r > 0) $ do
                                  v <- UMV.unsafeRead to ix
                                  c <- UMV.unsafeRead cost ix
                                  dv <- UMV.unsafeRead dist v
                                  let !nd = du + c
                                  when (nd < dv) $ do
                                    UMV.unsafeWrite dist v nd
                                    UMV.unsafeWrite via v ix
                                    queued <- UMV.unsafeRead inQ v
                                    unless queued $ do
                                      UMV.unsafeWrite inQ v True
                                      readSTRef q >>= writeSTRef q . (|> v)
                                UMV.unsafeRead nextA ix >>= walk
                      UMV.unsafeRead headA u >>= walk
                      loop
            loop
          -- the residual bottleneck along the path into t
          bottleneck v acc
            | v == s = pure acc
            | otherwise = do
                ix <- UMV.unsafeRead via v
                r <- residual ix
                from <- UMV.unsafeRead to (opposite ix)
                bottleneck from (min acc r)
          push v amt
            | v == s = pure ()
            | otherwise = do
                ix <- UMV.unsafeRead via v
                UMV.unsafeRead flow ix >>= UMV.unsafeWrite flow ix . (+ amt)
                UMV.unsafeRead flow (opposite ix) >>= UMV.unsafeWrite flow (opposite ix) . subtract amt
                UMV.unsafeRead to (opposite ix) >>= \from -> push from amt
          augment = do
            spfa
            dt <- UMV.unsafeRead dist t
            when (dt /= maxBound) $ do
              amt <- bottleneck t maxBound
              push t amt
              augment
      when (n > 0 && m > 0) augment
      UV.freeze flow
    opposite ix = if even ix then ix + 1 else ix - 1
