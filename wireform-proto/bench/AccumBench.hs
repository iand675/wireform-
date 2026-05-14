{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Criterion micro-benchmarks comparing repeated-field accumulator strategies.

The decode loop for @repeated@ non-packed fields threads an accumulator
as a parameter (since the Decoder monad is pure).  The accumulator must
support:

  * O(1) snoc during the field loop
  * One-shot materialisation to @V.Vector@ or @VU.Vector@ at the end

Strategies compared:

  1. __GrowList__ (current) — difference list (@[a] -> [a]@) with a
     count.  O(1) snoc, but allocates two heap objects per element:
     a lambda closure for @(.)@ and a partial application for @(x:)@.

  2. __RevList__ — plain reversed @[a]@ with a count.  O(1) snoc
     (one cons cell per element), @V.fromListN n (reverse xs)@ at
     the end.  No closure overhead.

  3. __Seq__ (@Data.Sequence.Seq@) — finger tree with O(1) amortised
     snoc.  Complex allocation pattern; measured to see if the
     implicit chunking pays off.

  4. __STLoop__ — tail-recursive loop inside @runST@, carrying the
     mutable vector + size + capacity as explicit parameters.  No
     @STRef@ wrapper; the recursive call is the "snoc".  This is the
     ST-based analogue of the existing pure decode loop — same
     structure, different accumulator.

  5. __STLoopU__ — same as STLoop but with an unboxed @MVU.MVector@,
     for scalar (Int-like) element types.

Benchmarks vary:

  * Element type: @Int@ (unboxable scalar) and @BS.ByteString@
    (heap-allocated, stands in for submessage values).

  * Count N: 4, 16, 64, 256 — trivial messages up to
    @FileDescriptorProto@-scale.

Run with:

> cabal bench accum-bench -- --output accum-bench.html
-}
module Main where

import Control.DeepSeq (force, rnf)
import Control.Exception (evaluate)
import Control.Monad.ST (runST)
import Criterion.Main
import Data.ByteString qualified as BS
import Data.List (foldl')
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as MVU

------------------------------------------------------------------------
-- Strategy 1: GrowList (current implementation, inlined for self-containment)
------------------------------------------------------------------------

data GrowList a = GrowList
  { glBuild :: !([a] -> [a])
  , glCount :: {-# UNPACK #-} !Int
  }

emptyGrowList :: GrowList a
emptyGrowList = GrowList id 0
{-# INLINE emptyGrowList #-}

snocGrowList :: GrowList a -> a -> GrowList a
snocGrowList (GrowList f n) x = GrowList (f . (x :)) (n + 1)
{-# INLINE snocGrowList #-}

growListToVector :: GrowList a -> V.Vector a
growListToVector (GrowList f n)
  | n == 0 = V.empty
  | otherwise = V.create $ do
      let !xs = f []
      mv <- MV.new n
      let fill !_ [] = pure ()
          fill !i (e : es) = MV.unsafeWrite mv i e >> fill (i + 1) es
      fill 0 xs
      pure mv
{-# INLINE growListToVector #-}

growListToVectorU :: VU.Unbox a => GrowList a -> VU.Vector a
growListToVectorU gl = VU.convert (growListToVector gl)
{-# INLINE growListToVectorU #-}


------------------------------------------------------------------------
-- Strategy 2: reversed cons list with count
------------------------------------------------------------------------

data RevList a = RevList {-# UNPACK #-} !Int ![a]

emptyRevList :: RevList a
emptyRevList = RevList 0 []
{-# INLINE emptyRevList #-}

snocRevList :: RevList a -> a -> RevList a
snocRevList (RevList n xs) x = RevList (n + 1) (x : xs)
{-# INLINE snocRevList #-}

revListToVector :: RevList a -> V.Vector a
revListToVector (RevList n xs) = V.fromListN n (reverse xs)
{-# INLINE revListToVector #-}

revListToVectorU :: VU.Unbox a => RevList a -> VU.Vector a
revListToVectorU (RevList n xs) = VU.fromListN n (reverse xs)
{-# INLINE revListToVectorU #-}


------------------------------------------------------------------------
-- Strategy 3: Seq
------------------------------------------------------------------------

seqToVector :: Seq a -> V.Vector a
seqToVector s = V.fromList (foldr (:) [] s)
{-# INLINE seqToVector #-}


------------------------------------------------------------------------
-- Strategy 4: ST tail-recursive loop, boxed
--
-- Carries (size, capacity, MV.MVector) as explicit loop params —
-- no STRef overhead.  Models what the generated decode loop would
-- look like if the Decoder monad were restructured to run in ST.
------------------------------------------------------------------------

stLoopAccum :: [a] -> V.Vector a
stLoopAccum xs = runST $ do
    mv0 <- MV.unsafeNew 4
    go 0 4 mv0 xs
  where
    go !sz !_ mv [] =
        V.unsafeFreeze (MV.unsafeSlice 0 sz mv)
    go !sz !cap mv (x : rest) = do
        mv' <- if sz < cap
                then pure mv
                else MV.unsafeGrow mv cap
        MV.unsafeWrite mv' sz x
        go (sz + 1) (if sz < cap then cap else cap * 2) mv' rest
{-# NOINLINE stLoopAccum #-}


------------------------------------------------------------------------
-- Strategy 5: ST tail-recursive loop, unboxed
------------------------------------------------------------------------

stLoopAccumU :: VU.Unbox a => [a] -> VU.Vector a
stLoopAccumU xs = runST $ do
    mv0 <- MVU.unsafeNew 4
    go 0 4 mv0 xs
  where
    go !sz !_ mv [] =
        VU.unsafeFreeze (MVU.unsafeSlice 0 sz mv)
    go !sz !cap mv (x : rest) = do
        mv' <- if sz < cap
                then pure mv
                else MVU.unsafeGrow mv cap
        MVU.unsafeWrite mv' sz x
        go (sz + 1) (if sz < cap then cap else cap * 2) mv' rest
{-# NOINLINE stLoopAccumU #-}


------------------------------------------------------------------------
-- Strategy 6: SmallList
--
-- Inline 0–4 elements as ADT constructors — zero allocation for the
-- dominant single-element case.  Falls back to a reversed cons list
-- (with count) for N > 4.  Purely threadable; no IO/ST needed.
------------------------------------------------------------------------

data SmallList a
    = SL0
    | SL1 a
    | SL2 a a
    | SL3 a a a
    | SL4 a a a a
    | SLN {-# UNPACK #-} !Int ![a]  -- reversed, N > 4

emptySL :: SmallList a
emptySL = SL0
{-# INLINE emptySL #-}

snocSL :: SmallList a -> a -> SmallList a
snocSL SL0           x = SL1 x
snocSL (SL1 a)       x = SL2 a x
snocSL (SL2 a b)     x = SL3 a b x
snocSL (SL3 a b c)   x = SL4 a b c x
snocSL (SL4 a b c d) x = SLN 5 [x, d, c, b, a]
snocSL (SLN n xs)    x = SLN (n + 1) (x : xs)
{-# INLINE snocSL #-}

slToVector :: SmallList a -> V.Vector a
slToVector SL0           = V.empty
slToVector (SL1 a)       = V.singleton a
slToVector (SL2 a b)     = V.fromListN 2 [a, b]
slToVector (SL3 a b c)   = V.fromListN 3 [a, b, c]
slToVector (SL4 a b c d) = V.fromListN 4 [a, b, c, d]
slToVector (SLN n xs)    = V.fromListN n (reverse xs)
{-# INLINE slToVector #-}

slToVectorU :: VU.Unbox a => SmallList a -> VU.Vector a
slToVectorU SL0           = VU.empty
slToVectorU (SL1 a)       = VU.singleton a
slToVectorU (SL2 a b)     = VU.fromListN 2 [a, b]
slToVectorU (SL3 a b c)   = VU.fromListN 3 [a, b, c]
slToVectorU (SL4 a b c d) = VU.fromListN 4 [a, b, c, d]
slToVectorU (SLN n xs)    = VU.fromListN n (reverse xs)
{-# INLINE slToVectorU #-}


------------------------------------------------------------------------
-- Strategy 7: ChunkVec
--
-- Buffer elements into small fixed-size chunks (chunkSz = 8).  When a
-- chunk fills, freeze it into an immutable V.Vector via runST and push
-- it onto a reversed list of frozen chunks.  At the end, reverse the
-- chunk list and V.concat.
--
-- Per-element cost: ~1 cons cell.  A vector alloc happens only every
-- chunkSz elements.  Materialization is a single V.concat (one copy).
------------------------------------------------------------------------

chunkSz :: Int
chunkSz = 8

data ChunkVec a = ChunkVec
    { cvTotal   :: {-# UNPACK #-} !Int
    , cvChunks  :: ![V.Vector a]    -- frozen completed chunks, reversed
    , cvPending :: ![a]             -- reversed current partial chunk
    , cvPendSz  :: {-# UNPACK #-} !Int
    }

emptyCV :: ChunkVec a
emptyCV = ChunkVec 0 [] [] 0
{-# INLINE emptyCV #-}

snocCV :: ChunkVec a -> a -> ChunkVec a
snocCV cv x
    | cvPendSz cv + 1 >= chunkSz =
        let !chunk = V.fromListN chunkSz (reverse (x : cvPending cv))
        in ChunkVec (cvTotal cv + 1) (chunk : cvChunks cv) [] 0
    | otherwise =
        ChunkVec (cvTotal cv + 1) (cvChunks cv) (x : cvPending cv) (cvPendSz cv + 1)
{-# INLINE snocCV #-}

cvToVector :: ChunkVec a -> V.Vector a
cvToVector cv =
    let !lastChunk = V.fromListN (cvPendSz cv) (reverse (cvPending cv))
        !allChunks = reverse (lastChunk : cvChunks cv)
    in V.concat allChunks
{-# INLINE cvToVector #-}

cvToVectorU :: VU.Unbox a => ChunkVec a -> VU.Vector a
cvToVectorU cv = VU.convert (cvToVector cv)
{-# INLINE cvToVectorU #-}


------------------------------------------------------------------------
-- Strategy 8: RevList with fill-backwards (no intermediate 'reverse')
--
-- Collects into a reversed [a] with a count, then V.create fills the
-- vector by writing index n-1-i for each element, traversing the
-- reversed list front-to-back.  Avoids the extra O(n) 'reverse' pass
-- that RevList pays.
------------------------------------------------------------------------

revFillToVector :: RevList a -> V.Vector a
revFillToVector (RevList n xs) = V.create $ do
    mv <- MV.unsafeNew n
    let go !i []       = pure ()
        go !i (e : es) = MV.unsafeWrite mv (n - 1 - i) e >> go (i + 1) es
    go 0 xs
    pure mv
{-# INLINE revFillToVector #-}

revFillToVectorU :: VU.Unbox a => RevList a -> VU.Vector a
revFillToVectorU (RevList n xs) = VU.create $ do
    mv <- MVU.unsafeNew n
    let go !i []       = pure ()
        go !i (e : es) = MVU.unsafeWrite mv (n - 1 - i) e >> go (i + 1) es
    go 0 xs
    pure mv
{-# INLINE revFillToVectorU #-}


------------------------------------------------------------------------
-- Benchmark groups
------------------------------------------------------------------------

benchN :: Int -> [Benchmark]
benchN n =
    let !ints = [1 .. n] :: [Int]
        !bss  = map (\i -> BS.replicate (i `rem` 16 + 1) 0x42) ints
    in
    [ bgroup ("Int/" <> show n)
        [ bench "GrowList"   $ nf (\es -> growListToVectorU (foldl' snocGrowList emptyGrowList es)) ints
        , bench "RevList"    $ nf (\es -> revListToVectorU  (foldl' snocRevList  emptyRevList  es)) ints
        , bench "RevFill"    $ nf (\es -> revFillToVectorU  (foldl' snocRevList  emptyRevList  es)) ints
        , bench "SmallList"  $ nf (\es -> slToVectorU       (foldl' snocSL       emptySL       es)) ints
        , bench "ChunkVec"   $ nf (\es -> cvToVectorU       (foldl' snocCV       emptyCV       es)) ints
        , bench "STLoop"     $ nf stLoopAccumU ints
        ]
    , bgroup ("BS/" <> show n)
        [ bench "GrowList"   $ nf (\es -> growListToVector  (foldl' snocGrowList emptyGrowList es)) bss
        , bench "RevList"    $ nf (\es -> revListToVector   (foldl' snocRevList  emptyRevList  es)) bss
        , bench "RevFill"    $ nf (\es -> revFillToVector   (foldl' snocRevList  emptyRevList  es)) bss
        , bench "SmallList"  $ nf (\es -> slToVector        (foldl' snocSL       emptySL       es)) bss
        , bench "ChunkVec"   $ nf (\es -> cvToVector        (foldl' snocCV       emptyCV       es)) bss
        , bench "STLoop"     $ nf stLoopAccum bss
        ]
    ]


main :: IO ()
main = do
    _ <- evaluate $ force ([1..256] :: [Int])
    evaluate $ rnf (map (\i -> BS.replicate (i `rem` 16 + 1) 0x42) ([1..256] :: [Int]))
    defaultMain $ concatMap benchN [4, 16, 64, 256]
