{-# LANGUAGE OverloadedStrings #-}

{- | Property tests for 'Columnar.Stream', 'Columnar.Predicate',
'Columnar.LZ4', 'Columnar.IO' and the 'Columnar.SIMD' kernels.

The Iter combinators are tiny but they're the iteration
backbone of the columnar formats; an off-by-one in
'iterTake' / 'iterDrop' / 'iterRowSlice' would silently corrupt
every Parquet / Arrow / ORC stream the facade decodes. Drive
them through Hedgehog with random list inputs.
-}
module Main (main) where

import Columnar.IO qualified as CIO
import Columnar.LZ4 qualified as LZ4
import Columnar.Predicate qualified as Pred
import Columnar.SIMD qualified as SIMD
import Columnar.Stream qualified as IS
import Control.Monad (replicateM)
import Data.Bits (shiftL, testBit, xor, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Unsafe qualified as BSU
import Data.Int (Int32, Int64)
import Data.List (findIndex, sort)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word32, Word8)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Hedgehog (forAll, property, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.Syd
import Test.Syd.Hedgehog ()


main :: IO ()
main =
  sydTest
    $ describe
      "wireform-columnar"
    $ sequence_
      [ iterProps
      , iterCombinatorProps
      , predicateProps
      , predicateUnits
      , columnarIOUnits
      , lz4Tests
      , simdKernelProps
      ]


-- ============================================================
-- Columnar.LZ4 — pure-Haskell raw block codec
-- ============================================================

lz4Tests :: Spec
lz4Tests =
  describe
    "Columnar.LZ4"
    $ sequence_
      [ it "decompress empty -> empty" $
          LZ4.decompress 0 BS.empty `shouldBe` Right BS.empty
      , it "decompress: single all-literals sequence" $
          -- token = (5 << 4) | 0 = 0x50, then 5 literal bytes.
          let !block = BS.pack [0x50, 0x68, 0x65, 0x6c, 0x6c, 0x6f] -- "hello"
          in LZ4.decompress 16 block `shouldBe` Right (BSC.pack "hello")
      , it "decompress: literal extension" $
          -- 20 literals: nibble=15, ext=[5], then 20 bytes.
          let !lits = BS.replicate 20 0x41 -- 'A' x 20
              !block = BS.pack (0xF0 : 5 : BS.unpack lits)
          in LZ4.decompress 32 block `shouldBe` Right (BS.replicate 20 0x41)
      , -- Back-reference + overlapping-match wire-byte tests are
        -- exercised through the round-trip property below; we
        -- can't hand-write a "minimal" block here because liblz4
        -- (rightly) enforces the spec's "last 5 bytes literal +
        -- match ends >= 12 bytes from end" rule and our minimal
        -- sequences violated those constraints.
        it "compress . decompress = id (short text)" $
          let !payload = BSC.pack "hello world hello world hello world"
              !compressed = LZ4.compress payload
          in LZ4.decompress (BS.length payload) compressed `shouldBe` Right payload
      , it "compress . decompress = id (highly repetitive)" $
          let !payload = BS.replicate 1024 0xAB
              !compressed = LZ4.compress payload
          in do
               BS.length compressed < BS.length payload `shouldBe` True -- did compress
               LZ4.decompress (BS.length payload) compressed `shouldBe` Right payload
      , it "compress . decompress = id (random bytes)" $ property $ do
          bs <-
            forAll
              (BS.pack <$> Gen.list (Range.linear 0 4096) (Gen.word8 Range.linearBounded))
          let !c = LZ4.compress bs
          LZ4.decompress (BS.length bs) c === Right bs
      , it "decompress refuses output > maxOutput" $ property $ do
          let !payload = BS.replicate 64 0x21
              !c = LZ4.compress payload
          -- maxOutput one byte too small
          case LZ4.decompress (BS.length payload - 1) c of
            Left _ -> H.success
            Right _ -> H.failure
      , it "decompress detects truncated blocks" $ property $ do
          bs <-
            forAll
              (BS.pack <$> Gen.list (Range.linear 16 256) (Gen.word8 Range.linearBounded))
          let !c = LZ4.compress bs
          -- Drop the last byte: result might or might not parse,
          -- but if it parses it must NOT equal the original.
          cutBy <- forAll (Gen.int (Range.linear 1 (max 1 (BS.length c - 1))))
          let !truncated = BS.take (BS.length c - cutBy) c
          case LZ4.decompress (BS.length bs) truncated of
            Left _ -> H.success
            Right back -> H.diff back (/=) bs
      ]


-- ============================================================
-- iterChunk / iterScan / iterMergeBy / iterPrefetch / iterParallelMap
-- ============================================================

iterCombinatorProps :: Spec
iterCombinatorProps =
  describe
    "Columnar.Stream new combinators"
    $ sequence_
      [ it "iterChunk n preserves concat" $ property $ do
          n <- forAll (Gen.int (Range.linear 1 10))
          xs <- forAll (Gen.list (Range.linear 0 50) (Gen.int (Range.linear 0 100)))
          case IS.iterToList (IS.iterChunk n (IS.iterFromList xs)) of
            Right chunks -> do
              concat chunks === xs
              -- All chunks except possibly the last have length n
              all (\c -> length c == n) (init1 chunks) === True
              -- Last chunk has length in [1..n] when xs nonempty
              case chunks of
                [] -> null xs === True
                _ ->
                  let !lastLen = length (last chunks)
                  in (lastLen >= 1 && lastLen <= n) === True
            Left e -> H.footnote e >> H.failure
      , it "iterChunk n=0 yields empty" $ property $ do
          xs <- forAll (Gen.list (Range.linear 0 30) (Gen.int (Range.linear 0 100)))
          IS.iterToList (IS.iterChunk 0 (IS.iterFromList xs)) === Right []
      , it "iterScan matches Data.List.scanl'" $ property $ do
          xs <- forAll (Gen.list (Range.linear 0 30) (Gen.int (Range.linear 0 100)))
          seed <- forAll (Gen.int (Range.linear 0 100))
          IS.iterToList (IS.iterScan (+) seed (IS.iterFromList xs))
            === Right (scanl' (+) seed xs)
      , it "iterMergeBy on sorted inputs == merge-sort union" $ property $ do
          xs <- sortedList
          ys <- sortedList
          zs <- sortedList
          let merged =
                IS.iterMergeBy
                  compare
                  [IS.iterFromList xs, IS.iterFromList ys, IS.iterFromList zs]
          case IS.iterToList merged of
            Right got -> got === sortedMerge3 xs ys zs
            Left e -> H.footnote e >> H.failure
      , it "iterMergeBy [] is empty" $
          property $
            IS.iterToList (IS.iterMergeBy compare ([] :: [IS.Iter Int])) === Right []
      , it "iterMergeBy [single] is identity" $ property $ do
          xs <- forAll (Gen.list (Range.linear 0 30) (Gen.int (Range.linear 0 100)))
          IS.iterToList (IS.iterMergeBy compare [IS.iterFromList xs])
            === Right xs
      , it "iterIOPrefetch preserves order and contents" $ property $ do
          xs <- forAll (Gen.list (Range.linear 0 30) (Gen.int (Range.linear 0 100)))
          depth <- forAll (Gen.int (Range.linear 1 8))
          got <- H.evalIO $ do
            prefetched <- IS.iterIOPrefetch depth (IS.iterIOFromIter (IS.iterFromList xs))
            IS.iterIOToList prefetched
          got === Right xs
      , it "iterParallelMap preserves order and applies the function" $ property $ do
          xs <- forAll (Gen.list (Range.linear 0 30) (Gen.int (Range.linear 0 100)))
          depth <- forAll (Gen.int (Range.linear 1 4))
          got <- H.evalIO $ do
            mapped <-
              IS.iterParallelMap
                depth
                (\x -> pure (x * 2))
                (IS.iterIOFromIter (IS.iterFromList xs))
            IS.iterIOToList mapped
          got === Right (map (* 2) xs)
      ]
  where
    sortedList =
      forAll
        ( sortAsc
            <$> Gen.list (Range.linear 0 20) (Gen.int (Range.linear 0 100))
        )
    sortAsc = foldr insertAsc []
    insertAsc x [] = [x]
    insertAsc x (y : ys)
      | x <= y = x : y : ys
      | otherwise = y : insertAsc x ys

    sortedMerge3 xs ys zs = sortAscMerge xs (sortAscMerge ys zs)
      where
        sortAscMerge as [] = as
        sortAscMerge [] bs = bs
        sortAscMerge (a : as) (b : bs)
          | a <= b = a : sortAscMerge as (b : bs)
          | otherwise = b : sortAscMerge (a : as) bs

    init1 [] = []
    init1 [_] = []
    init1 (x : xs) = x : init1 xs

    scanl' f = go
      where
        go !acc [] = [acc]
        go !acc (x : xs) = acc : go (f acc x) xs


-- ============================================================
-- Columnar.IO unit tests
-- ============================================================

columnarIOUnits :: Spec
columnarIOUnits =
  describe
    "Columnar.IO"
    $ sequence_
      [ it "loadFileEager + loadFileMmap return equal bytes" $
          withSystemTempFile "wfio.bin" $ \path h -> do
            let !payload = BS.replicate 200_000 0xAB
            BS.hPut h payload
            hClose h
            eager <- CIO.loadFileEager path
            mmaped <- CIO.loadFileMmap path
            eager `shouldBe` payload
            mmaped `shouldBe` payload
      , it "loadFile picks mmap above MmapAbove threshold" $
          withSystemTempFile "wfio-big.bin" $ \path h -> do
            let !payload = BS.replicate 200_000 0xCD
            BS.hPut h payload
            hClose h
            bs <- CIO.loadFile path
            BS.length bs `shouldBe` 200_000
      , it "loadFile uses eager path under threshold" $
          withSystemTempFile "wfio-small.bin" $ \path h -> do
            let !payload = BS.replicate 1024 0xEF
            BS.hPut h payload
            hClose h
            bs <- CIO.loadFile path
            BS.length bs `shouldBe` 1024
      ]


-- ============================================================
-- Iter properties
-- ============================================================

iterProps :: Spec
iterProps =
  describe
    "Columnar.Stream.Iter"
    $ sequence_
      [ it "iterToList . iterFromList = id" $ property $ do
          xs <- forAll (Gen.list (Range.linear 0 50) (Gen.int (Range.linear 0 100)))
          IS.iterToList (IS.iterFromList xs) === Right xs
      , it "iterMap = map" $ property $ do
          xs <- forAll (Gen.list (Range.linear 0 50) (Gen.int (Range.linear 0 100)))
          let f = (+ 1)
          IS.iterToList (IS.iterMap f (IS.iterFromList xs))
            === Right (map f xs)
      , it "iterFilter = filter" $ property $ do
          xs <- forAll (Gen.list (Range.linear 0 50) (Gen.int (Range.linear 0 100)))
          let p = even
          IS.iterToList (IS.iterFilter p (IS.iterFromList xs))
            === Right (filter p xs)
      , it "iterTake n = take n" $ property $ do
          xs <- forAll (Gen.list (Range.linear 0 50) (Gen.int (Range.linear 0 100)))
          n <- forAll (Gen.int (Range.linear 0 60))
          IS.iterToList (IS.iterTake n (IS.iterFromList xs))
            === Right (take n xs)
      , it "iterDrop n = drop n" $ property $ do
          xs <- forAll (Gen.list (Range.linear 0 50) (Gen.int (Range.linear 0 100)))
          n <- forAll (Gen.int (Range.linear 0 60))
          IS.iterToList (IS.iterDrop n (IS.iterFromList xs))
            === Right (drop n xs)
      , it "iterAppend = (++)" $ property $ do
          xs <- forAll (Gen.list (Range.linear 0 30) (Gen.int (Range.linear 0 100)))
          ys <- forAll (Gen.list (Range.linear 0 30) (Gen.int (Range.linear 0 100)))
          IS.iterToList (IS.iterAppend (IS.iterFromList xs) (IS.iterFromList ys))
            === Right (xs ++ ys)
      , it "iterConcat = concat" $ property $ do
          xss <-
            forAll
              ( Gen.list
                  (Range.linear 0 5)
                  (Gen.list (Range.linear 0 10) (Gen.int (Range.linear 0 100)))
              )
          IS.iterToList
            (IS.iterConcat (IS.iterFromList (map IS.iterFromList xss)))
            === Right (concat xss)
      , it "iterFold = foldl'" $ property $ do
          xs <- forAll (Gen.list (Range.linear 0 50) (Gen.int (Range.linear 0 100)))
          IS.iterFold (+) 0 (IS.iterFromList xs)
            === Right (sum xs)
      , it "iterLength = length" $ property $ do
          xs <- forAll (Gen.list (Range.linear 0 50) (Gen.int (Range.linear 0 100)))
          IS.iterLength (IS.iterFromList xs)
            === Right (length xs)
      , it "iterFromIndexed n f matches map f [0..n-1]" $ property $ do
          n <- forAll (Gen.int (Range.linear 0 30))
          let f i = Right (i * 2)
          IS.iterToList (IS.iterFromIndexed n f)
            === Right [i * 2 | i <- [0 .. n - 1]]
      , it "iterMapM threads errors" $ property $ do
          xs <- forAll (Gen.list (Range.linear 1 20) (Gen.int (Range.linear 0 100)))
          let f x = if x == 7 then Left "boom" else Right x
              expected = case break (== 7) xs of
                (pre, []) -> Right pre
                (_, _ : _) -> Left "boom"
          IS.iterToList (IS.iterMapM f (IS.iterFromList xs))
            === expected
      , it "iterRowSlice respects offset+len" $ property $ do
          -- Each element is a list of ints (its 'row count' is the
          -- list length). Cross-element slicing should match plain
          -- list slicing of the flattened stream.
          xss <-
            forAll
              ( Gen.list
                  (Range.linear 0 6)
                  (Gen.list (Range.linear 0 5) (Gen.int (Range.linear 0 100)))
              )
          offset <- forAll (Gen.int (Range.linear 0 30))
          taken <- forAll (Gen.int (Range.linear 0 30))
          let source = IS.iterFromList xss
              sliced =
                IS.iterRowSlice
                  length
                  (\s l xs -> take l (drop s xs))
                  offset
                  taken
                  source
              expected = take taken (drop offset (concat xss))
          case IS.iterToList sliced of
            Left e -> H.footnote e >> H.failure
            Right got -> concat got === expected
      ]


-- ============================================================
-- Predicate properties
-- ============================================================

predicateProps :: Spec
predicateProps =
  describe
    "Columnar.Predicate.evalRange"
    $ sequence_
      [ -- Soundness: PSkip is only returned when no value in
        -- [mn, mx] satisfies the predicate. Generate a triple
        -- (mn, mx, v) and a leaf predicate, ask evalRange, then
        -- \*exhaustively* check whether any integer in [mn, mx]
        -- satisfies the predicate. If evalRange says PSkip but
        -- some integer satisfies, that's a false negative —
        -- the soundness violation we care about.
        it "PSkip => no integer in [mn,mx] satisfies the predicate (Int64)" $
          property $ do
            mn <- forAll (Gen.int (Range.linear (-50) 50))
            mx <- forAll (Gen.int (Range.linear mn 60))
            op <-
              forAll
                ( Gen.choice
                    [ pure Pred.PEq
                    , pure Pred.PNeq
                    , pure Pred.PLt
                    , pure Pred.PLtEq
                    , pure Pred.PGt
                    , pure Pred.PGtEq
                    ]
                    <*> (Pred.PVInt64 . fromIntegral <$> Gen.int (Range.linear (-100) 100))
                )
            let !decision =
                  Pred.evalRange
                    (Pred.PVInt64 (fromIntegral mn))
                    (Pred.PVInt64 (fromIntegral mx))
                    op
                range64 = map fromIntegral [mn .. mx] :: [Int64]
                !satisfies = case op of
                  Pred.PEq (Pred.PVInt64 v) -> v `elem` range64
                  Pred.PNeq (Pred.PVInt64 v) -> any (/= v) range64
                  Pred.PLt (Pred.PVInt64 v) -> any (< v) range64
                  Pred.PLtEq (Pred.PVInt64 v) -> any (<= v) range64
                  Pred.PGt (Pred.PVInt64 v) -> any (> v) range64
                  Pred.PGtEq (Pred.PVInt64 v) -> any (>= v) range64
                  _ -> True
            case decision of
              Pred.PSkip -> satisfies === False
              Pred.PMaybeKeep -> H.success -- always sound
      , it "PSkip soundness for Int32 ranges" $ property $ do
          mn <- forAll (Gen.int32 (Range.linear (-50) 50))
          mx <- forAll (Gen.int32 (Range.linear mn 60))
          v <- forAll (Gen.int32 (Range.linear (-100) 100))
          let !decision =
                Pred.evalRange
                  (Pred.PVInt32 mn)
                  (Pred.PVInt32 mx)
                  (Pred.PEq (Pred.PVInt32 v))
          case decision of
            Pred.PSkip -> (v >= mn && v <= mx) === False
            _ -> H.success
      , it "PSkip soundness for Double ranges" $ property $ do
          mn <- forAll (Gen.double (Range.linearFrac (-50.0) 50.0))
          mx <- forAll (Gen.double (Range.linearFrac mn 60.0))
          v <- forAll (Gen.double (Range.linearFrac (-100.0) 100.0))
          let !decision =
                Pred.evalRange
                  (Pred.PVDouble mn)
                  (Pred.PVDouble mx)
                  (Pred.PEq (Pred.PVDouble v))
          case decision of
            Pred.PSkip -> (v >= mn && v <= mx) === False
            _ -> H.success
      , it "PSkip soundness for Text ranges (UTF-8 byte order)" $
          property $ do
            let alpha = Gen.text (Range.linear 1 4) Gen.alpha
            mn <- forAll alpha
            mx <- forAll (Gen.filter (>= mn) alpha)
            v <- forAll alpha
            let !decision =
                  Pred.evalRange
                    (Pred.PVText mn)
                    (Pred.PVText mx)
                    (Pred.PEq (Pred.PVText v))
            case decision of
              Pred.PSkip -> (v >= mn && v <= mx) === False
              _ -> H.success
      , it "PIn rejects only when every member is outside the range" $
          property $ do
            mn <- forAll (Gen.int (Range.linear (-50) 50))
            mx <- forAll (Gen.int (Range.linear mn 60))
            ks <- forAll (Gen.list (Range.linear 1 5) (Gen.int (Range.linear (-100) 100)))
            let !decision =
                  Pred.evalRange
                    (Pred.PVInt64 (fromIntegral mn))
                    (Pred.PVInt64 (fromIntegral mx))
                    (Pred.PIn (map (Pred.PVInt64 . fromIntegral) ks))
                !anyInside = any (\k -> k >= mn && k <= mx) ks
            if anyInside
              then decision === Pred.PMaybeKeep
              else decision === Pred.PSkip
      , it "PIsNull always returns PMaybeKeep (range-only stats)" $
          property $ do
            mn <- forAll (Gen.int (Range.linear (-100) 100))
            mx <- forAll (Gen.int (Range.linear mn 100))
            Pred.evalRange
              (Pred.PVInt64 (fromIntegral mn))
              (Pred.PVInt64 (fromIntegral mx))
              Pred.PIsNull
              === Pred.PMaybeKeep
      , it "PIsNotNull always returns PMaybeKeep" $ property $ do
          mn <- forAll (Gen.int (Range.linear (-100) 100))
          mx <- forAll (Gen.int (Range.linear mn 100))
          Pred.evalRange
            (Pred.PVInt64 (fromIntegral mn))
            (Pred.PVInt64 (fromIntegral mx))
            Pred.PIsNotNull
            === Pred.PMaybeKeep
      , it "PNeq always returns PMaybeKeep (can't prove from range alone)" $
          property $ do
            mn <- forAll (Gen.int (Range.linear (-100) 100))
            mx <- forAll (Gen.int (Range.linear mn 100))
            v <- forAll (Gen.int (Range.linear (-200) 200))
            Pred.evalRange
              (Pred.PVInt64 (fromIntegral mn))
              (Pred.PVInt64 (fromIntegral mx))
              (Pred.PNeq (Pred.PVInt64 (fromIntegral v)))
              === Pred.PMaybeKeep
      , it "Cross-type comparison degrades to PMaybeKeep" $ property $ do
          v <- forAll (Gen.int (Range.linear (-100) 100))
          txt <- forAll (Gen.text (Range.linear 0 5) Gen.alpha)
          let !decision =
                Pred.evalRange
                  (Pred.PVInt64 (fromIntegral v))
                  (Pred.PVInt64 (fromIntegral v))
                  (Pred.PEq (Pred.PVText txt))
          decision === Pred.PMaybeKeep
      ]


predicateUnits :: Spec
predicateUnits =
  describe
    "Columnar.Predicate units"
    $ sequence_
      [ it "combineDecisions PSkip _ = PSkip" $
          Pred.combineDecisions Pred.PSkip Pred.PMaybeKeep `shouldBe` Pred.PSkip
      , it "combineDecisions PMaybeKeep PMaybeKeep = PMaybeKeep" $
          Pred.combineDecisions Pred.PMaybeKeep Pred.PMaybeKeep `shouldBe` Pred.PMaybeKeep
      , it "pvLess on incomparable returns False" $
          Pred.pvLess (Pred.PVInt32 1) (Pred.PVText "x") `shouldBe` False
      ]


-- ============================================================
-- Columnar.SIMD kernels against list models
-- ============================================================

{- | Every kernel is compared with a direct list model. Inputs are
mostly valid and then optionally corrupted at one or two positions,
so both the @-1@ and the failing-index paths are exercised. Each
buffer is placed at a random byte offset (0..15) inside a larger
allocation and bitmaps start at random bit offsets (0..70), so no
kernel can rely on alignment.
-}
simdKernelProps :: Spec
simdKernelProps =
  modifyMaxSuccess (const 300) $
    describe
      "Columnar.SIMD kernels"
      $ sequence_
        [ describe "offsets" $
            sequence_
              [ it "i32 matches the list model" $
                  offsetsProp wI32 (\p -> SIMD.offsetsCheckI32 (castPtr p))
              , it "i64 matches the list model" $
                  offsetsProp wI64 (\p -> SIMD.offsetsCheckI64 (castPtr p))
              ]
        , it "utf8 boundaries (i32 and i64) match the list model" utf8BoundariesProp
        , it "popCountBits matches the list model" popCountBitsProp
        , describe "keysInRange" $
            sequence_
              [ it "i8" $ keysProp wI8 (\p -> SIMD.keysInRangeI8 (castPtr p))
              , it "i16" $ keysProp wI16 (\p -> SIMD.keysInRangeI16 (castPtr p))
              , it "i32" $ keysProp wI32 (\p -> SIMD.keysInRangeI32 (castPtr p))
              , it "i64" $ keysProp wI64 (\p -> SIMD.keysInRangeI64 (castPtr p))
              , it "u8" $ keysProp wU8 SIMD.keysInRangeU8
              , it "u16" $ keysProp wU16 (\p -> SIMD.keysInRangeU16 (castPtr p))
              , it "u32" $ keysProp wU32 (\p -> SIMD.keysInRangeU32 (castPtr p))
              , it "u64" $ keysProp wU64 (\p -> SIMD.keysInRangeU64 (castPtr p))
              ]
        , it "copyBits writes the range and preserves every other bit" copyBitsProp
        , describe "rebaseOffsets" $
            sequence_
              [ it "i32" $
                  rebaseProp wI32 (pow2 29) (\d s -> SIMD.rebaseOffsetsI32 (castPtr d) (castPtr s))
              , it "i64" $
                  rebaseProp wI64 (pow2 61) (\d s -> SIMD.rebaseOffsetsI64 (castPtr d) (castPtr s))
              ]
        , describe "runEnds" $
            sequence_
              [ it "i16" $ runEndsProp wI16 (\p -> SIMD.runEndsCheckI16 (castPtr p))
              , it "i32" $ runEndsProp wI32 (\p -> SIMD.runEndsCheckI32 (castPtr p))
              , it "i64" $ runEndsProp wI64 (\p -> SIMD.runEndsCheckI64 (castPtr p))
              ]
        , describe "listView" $
            sequence_
              [ it "i32" $
                  listViewProp wI32 (\o s -> SIMD.listViewCheckI32 (castPtr o) (castPtr s))
              , it "i64" $
                  listViewProp wI64 (\o s -> SIMD.listViewCheckI64 (castPtr o) (castPtr s))
              ]
        , it "denseUnionCheck matches the list model" denseUnionProp
        , it "viewRefsCheck (binary and utf8) matches the list model" viewRefsProp
        , describe "gather" $
            sequence_
              [ it "width 1" $ gatherProp 1 SIMD.gather1
              , it "width 2" $ gatherProp 2 SIMD.gather2
              , it "width 4" $ gatherProp 4 SIMD.gather4
              , it "width 8" $ gatherProp 8 SIMD.gather8
              , it "width 16" $ gatherProp 16 SIMD.gather16
              ]
        , it "gatherBits matches the list model" gatherBitsProp
        , it "andBits matches the list model" andBitsProp
        , describe "takeOffsets" $
            sequence_
              [ it "i32 (with overflow)" $
                  takeOffsetsProp wI32 (pow2 28) (\d s -> SIMD.takeOffsetsI32 (castPtr d) (castPtr s))
              , it "i64 (with overflow)" $
                  takeOffsetsProp wI64 (pow2 60) (\d s -> SIMD.takeOffsetsI64 (castPtr d) (castPtr s))
              ]
        , it "takeBytes (i32 and i64) concatenates the selected rows" takeBytesProp
        ]


-- ---------- buffers ----------

-- | Copy the bytes to byte offset @off@ of a fresh 16-aligned buffer.
withBytesAt :: Int -> BS.ByteString -> (Ptr a -> IO r) -> IO r
withBytesAt off bs k =
  allocaBytesAligned (off + BS.length bs + 16) 16 $ \(base :: Ptr Word8) -> do
    let p = base `plusPtr` off
    BSU.unsafeUseAsCStringLen bs $ \(src, len) -> copyBytes p (castPtr src) len
    k (castPtr p)


withAllAt :: [(Int, BS.ByteString)] -> ([Ptr Word8] -> IO r) -> IO r
withAllAt [] k = k []
withAllAt ((off, bs) : rest) k = withBytesAt off bs $ \p -> withAllAt rest (\ps -> k (p : ps))


readBytes :: Ptr a -> Int -> IO BS.ByteString
readBytes p n = BS.packCStringLen (castPtr p, n)


genAlign :: H.Gen Int
genAlign = Gen.int (Range.constant 0 15)


-- ---------- fixed-width encodings ----------

data Width = Width
  { wSize :: Int
  , wLo :: Integer
  , wHi :: Integer
  , wEnc :: Integer -> BB.Builder
  }


pow2 :: Int -> Integer
pow2 k = 2 ^ k


signedW :: Int -> (Integer -> BB.Builder) -> Width
signedW bytes = Width bytes (negate (pow2 (8 * bytes - 1))) (pow2 (8 * bytes - 1) - 1)


unsignedW :: Int -> (Integer -> BB.Builder) -> Width
unsignedW bytes = Width bytes 0 (pow2 (8 * bytes) - 1)


wI8, wI16, wI32, wI64, wU8, wU16, wU32, wU64 :: Width
wI8 = signedW 1 (BB.int8 . fromInteger)
wI16 = signedW 2 (BB.int16LE . fromInteger)
wI32 = signedW 4 (BB.int32LE . fromInteger)
wI64 = signedW 8 (BB.int64LE . fromInteger)
wU8 = unsignedW 1 (BB.word8 . fromInteger)
wU16 = unsignedW 2 (BB.word16LE . fromInteger)
wU32 = unsignedW 4 (BB.word32LE . fromInteger)
wU64 = unsignedW 8 (BB.word64LE . fromInteger)


encW :: Width -> [Integer] -> BS.ByteString
encW w = BL.toStrict . BB.toLazyByteString . foldMap (wEnc w)


encIdx :: [Int] -> BS.ByteString
encIdx = encW wI64 . map toInteger


le32 :: Int -> BS.ByteString
le32 x = encW wI32 [toInteger x]


-- | Signed little-endian int32 at byte offset @at@.
leI32 :: BS.ByteString -> Int -> Int
leI32 bs at = fromIntegral (fromIntegral w :: Int32)
  where
    w = foldr (\k acc -> (acc `shiftL` 8) .|. fromIntegral (BS.index bs (at + k))) (0 :: Word32) [0 .. 3]


setI32 :: Int -> Int -> BS.ByteString -> BS.ByteString
setI32 at x v = BS.take at v <> le32 x <> BS.drop (at + 4) v


-- | Any value of the width, biased towards the extremes and small values.
genCorrupt :: Width -> H.Gen Integer
genCorrupt w =
  Gen.choice
    [ Gen.integral (Range.linearFrom 0 (wLo w) (wHi w))
    , Gen.element (filter (\x -> x >= wLo w && x <= wHi w) [wLo w, wHi w, -1, 0, 1, 2])
    ]


-- ---------- lists and bits ----------

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i v xs = take i xs ++ [v] ++ drop (i + 1) xs


-- | With probability 1/2, overwrite one random position.
corrupt :: H.Gen a -> [a] -> H.Gen [a]
corrupt g xs
  | null xs = pure xs
  | otherwise =
      Gen.frequency
        [ (1, pure xs)
        , ( 1
          , do
              i <- Gen.int (Range.constant 0 (length xs - 1))
              v <- g
              pure (replaceAt i v xs)
          )
        ]


corrupt2 :: H.Gen a -> [a] -> H.Gen [a]
corrupt2 g xs = corrupt g xs >>= corrupt g


lastOr :: a -> [a] -> a
lastOr d = foldl (\_ x -> x) d


firstFail :: [Bool] -> Int
firstFail = fromMaybe (-1) . findIndex id


ceil8 :: Int -> Int
ceil8 x = (x + 7) `quot` 8


packBits :: [Bool] -> BS.ByteString
packBits [] = BS.empty
packBits bits = BS.cons (byteOf h) (packBits t)
  where
    (h, t) = splitAt 8 bits
    byteOf = foldr (\(i, b) acc -> if b then acc .|. (1 `shiftL` i) else acc) (0 :: Word8) . zip [0 ..]


unpackBits :: BS.ByteString -> [Bool]
unpackBits = concatMap (\w -> map (testBit w) [0 .. 7]) . BS.unpack


-- | Random bits covering at least @n@, rounded up to whole bytes.
genBitsFor :: Int -> H.Gen [Bool]
genBitsFor n = Gen.list (Range.singleton (8 * ceil8 n)) Gen.bool


genBitOff :: H.Gen Int
genBitOff =
  Gen.frequency
    [ (3, Gen.int (Range.constant 0 70))
    , (1, (* 8) <$> Gen.int (Range.constant 0 8))
    ]


type Validity = Maybe (Int, [Bool])


genValidity :: Int -> H.Gen Validity
genValidity n =
  Gen.frequency
    [ (1, pure Nothing)
    , ( 3
      , do
          off <- genBitOff
          bits <- Gen.list (Range.singleton (8 * ceil8 (off + n))) (Gen.frequency [(4, pure True), (1, pure False)])
          pure (Just (off, bits))
      )
    ]


isValid :: Validity -> Int -> Bool
isValid Nothing _ = True
isValid (Just (off, bits)) i = bits !! (off + i)


withValidity :: Int -> Validity -> (Ptr Word8 -> Int -> IO r) -> IO r
withValidity _ Nothing k = k nullPtr 0
withValidity al (Just (off, bits)) k = withBytesAt al (packBits bits) $ \p -> k p off


-- ---------- validators ----------

modelOffsets :: [Integer] -> Integer -> Int
modelOffsets [] _ = -1
modelOffsets xs@(x0 : rest) limit
  | x0 < 0 = 0
  | Just i <- findIndex id (zipWith (>) xs rest) = i + 1
  | lastOr x0 rest > limit = length xs - 1
  | otherwise = -1


offsetsProp :: Width -> (Ptr Word8 -> Int -> Int64 -> IO Int) -> H.Property
offsetsProp w kernel = property $ do
  start <- forAll (Gen.integral (Range.linear 0 50))
  steps <- forAll (Gen.list (Range.linear 0 80) (Gen.integral (Range.linear 0 20)))
  isEmpty <- forAll (Gen.frequency [(1, pure True), (12, pure False)])
  let valid = if isEmpty then [] else scanl (+) start steps
  offs <- forAll (corrupt2 (genCorrupt w) valid)
  slack <- forAll (Gen.integral (Range.linearFrom 0 (-3) 3))
  al <- forAll genAlign
  let limit = lastOr 0 valid + slack
  r <- H.evalIO (withBytesAt al (encW w offs) (\p -> kernel p (length offs) (fromInteger limit)))
  let model = modelOffsets offs limit
  coverPaths model
  r === model


modelRunEnds :: [Integer] -> Integer -> Int
modelRunEnds [] minEnd = if minEnd <= 0 then -1 else 0
modelRunEnds xs@(x0 : rest) minEnd
  | x0 <= 0 = 0
  | Just i <- findIndex id (zipWith (>=) xs rest) = i + 1
  | lastOr x0 rest < minEnd = length xs - 1
  | otherwise = -1


runEndsProp :: Width -> (Ptr Word8 -> Int -> Int64 -> IO Int) -> H.Property
runEndsProp w kernel = property $ do
  steps <- forAll (Gen.list (Range.linear 0 60) (Gen.integral (Range.linear 1 300)))
  let ends0 = drop 1 (scanl (+) 0 steps)
  ends <- forAll (corrupt2 (genCorrupt w) ends0)
  slack <- forAll (Gen.integral (Range.linearFrom 0 (-3) 3))
  al <- forAll genAlign
  let minEnd = lastOr 0 ends0 + slack
  r <- H.evalIO (withBytesAt al (encW w ends) (\p -> kernel p (length ends) (fromInteger minEnd)))
  let model = modelRunEnds ends minEnd
  coverPaths model
  r === model


utf8BoundariesProp :: H.Property
utf8BoundariesProp = property $ do
  txt <- forAll (Gen.text (Range.linear 0 40) Gen.unicode)
  let dat = TE.encodeUtf8 txt
      len = BS.length dat
      boundaries = scanl (+) 0 (map (BS.length . TE.encodeUtf8 . T.singleton) (T.unpack txt))
  picked <- forAll (Gen.subsequence boundaries)
  offs <- forAll (sort <$> corrupt2 (Gen.int (Range.constant 0 len)) picked)
  alO <- forAll genAlign
  alD <- forAll genAlign
  let n = length offs
      model = firstFail (map (\o -> o < len && BS.index dat o .&. 0xC0 == 0x80) offs)
      run w kernel =
        withBytesAt alO (encW w (map toInteger offs)) $ \op ->
          withBytesAt alD dat $ \dp -> kernel op n dp len
  r32 <- H.evalIO (run wI32 (\o -> SIMD.utf8BoundariesI32 (castPtr o)))
  r64 <- H.evalIO (run wI64 (\o -> SIMD.utf8BoundariesI64 (castPtr o)))
  coverPaths model
  (r32, r64) === (model, model)


keysProp :: Width -> (Ptr Word8 -> Int -> Ptr Word8 -> Int -> Int64 -> IO Int) -> H.Property
keysProp w kernel = property $ do
  mx <-
    forAll
      ( Gen.frequency
          [ (12, Gen.integral (Range.linear 1 (min 200 (wHi w))))
          , (1, Gen.integral (Range.linearFrom 0 (-3) 0))
          , (1, pure (pow2 63 - 1))
          ]
      )
  n <- forAll (Gen.int (Range.linear 0 150))
  keys0 <- forAll (Gen.list (Range.singleton n) (Gen.integral (Range.constant 0 (max 0 (min (wHi w) (mx - 1))))))
  keys <- forAll (corrupt2 (genCorrupt w) keys0)
  validity <- forAll (genValidity n)
  alK <- forAll genAlign
  alV <- forAll genAlign
  r <-
    H.evalIO $
      withBytesAt alK (encW w keys) $ \kp ->
        withValidity alV validity $ \vp vo -> kernel kp n vp vo (fromInteger mx)
  let model = firstFail (zipWith (\i k -> isValid validity i && not (0 <= k && k < mx)) [0 ..] keys)
  coverPaths model
  r === model


listViewProp :: Width -> (Ptr Word8 -> Ptr Word8 -> Int -> Ptr Word8 -> Int -> Int64 -> IO Int) -> H.Property
listViewProp w kernel = property $ do
  childLen <- forAll (Gen.integral (Range.linear 0 50))
  n <- forAll (Gen.int (Range.linear 0 100))
  rows <-
    forAll $
      Gen.list (Range.singleton n) $ do
        o <- Gen.integral (Range.constant 0 childLen)
        s <- Gen.integral (Range.constant 0 (childLen - o))
        pure (o, s)
  offs <- forAll (corrupt2 (genCorrupt w) (map fst rows))
  sizes <- forAll (corrupt2 (genCorrupt w) (map snd rows))
  validity <- forAll (genValidity n)
  alO <- forAll genAlign
  alS <- forAll genAlign
  alV <- forAll genAlign
  r <-
    H.evalIO $
      withBytesAt alO (encW w offs) $ \op ->
        withBytesAt alS (encW w sizes) $ \sp ->
          withValidity alV validity $ \vp vo -> kernel op sp n vp vo (fromInteger childLen)
  let model =
        firstFail
          ( zipWith3
              (\i o s -> isValid validity i && not (o >= 0 && s >= 0 && o + s <= childLen))
              [0 ..]
              offs
              sizes
          )
  coverPaths model
  r === model


denseUnionProp :: H.Property
denseUnionProp = property $ do
  lens <- forAll (Gen.list (Range.linear 0 5) (Gen.integral (Range.linear 0 10)))
  n <- forAll (Gen.int (Range.linear 0 60))
  let nonEmpty = filter (\(_, l) -> l > 0) (zip [0 ..] lens)
      nch = length lens
  rows <-
    forAll $
      Gen.list (Range.singleton n) $
        if null nonEmpty
          then (,) <$> Gen.integral (Range.constant (-2) 6) <*> Gen.integral (Range.constant (-2) 10)
          else do
            (t, l) <- Gen.element nonEmpty
            o <- Gen.integral (Range.constant 0 (l - 1))
            pure (t, o)
  types <- forAll (corrupt (genCorrupt wI8) (map fst rows))
  offs <- forAll (corrupt (genCorrupt wI32) (map snd rows))
  alT <- forAll genAlign
  alO <- forAll genAlign
  alL <- forAll genAlign
  r <-
    H.evalIO $
      withBytesAt alT (encW wI8 types) $ \tp ->
        withBytesAt alO (encW wI32 offs) $ \op ->
          withBytesAt alL (encW wI64 lens) $ \lp -> SIMD.denseUnionCheck tp op n lp nch
  let bad t o = t < 0 || t >= toInteger nch || o < 0 || o >= lens !! fromInteger t
  let model = firstFail (zipWith bad types offs)
  coverPaths model
  r === model


-- ---------- views ----------

invalidUtf8Seqs :: [BS.ByteString]
invalidUtf8Seqs =
  map
    BS.pack
    [ [0xC0, 0x80] -- overlong NUL
    , [0xC1, 0xBF] -- overlong
    , [0xE0, 0x80, 0x80] -- overlong 3-byte
    , [0xF0, 0x80, 0x80, 0x80] -- overlong 4-byte
    , [0xED, 0xA0, 0x80] -- surrogate D800
    , [0xED, 0xBF, 0xBF] -- surrogate DFFF
    , [0xF4, 0x90, 0x80, 0x80] -- above U+10FFFF
    , [0xF5, 0x80, 0x80, 0x80] -- invalid lead
    , [0xE2, 0x82] -- truncated 3-byte
    , [0xF0, 0x9F, 0x98] -- truncated 4-byte
    , [0xC3] -- truncated 2-byte
    , [0x80] -- stray continuation
    , [0xFF]
    ]


genPayload :: Bool -> H.Gen BS.ByteString
genPayload utf8Mode =
  Gen.frequency
    [ (if utf8Mode then 8 else 2, validText)
    , ( 2
      , do
          a <- validText
          b <- Gen.element invalidUtf8Seqs
          c <- validText
          pure (a <> b <> c)
      )
    , (if utf8Mode then 1 else 4, Gen.bytes (Range.linear 0 30))
    ]
  where
    validText = TE.encodeUtf8 <$> Gen.text (Range.linear 0 15) Gen.unicode


-- | Data buffers and 16-byte views for the rows ('Nothing' rows get garbage views).
buildViews :: Int -> [Maybe BS.ByteString] -> H.Gen ([BS.ByteString], [BS.ByteString])
buildViews nb rows = do
  initial <- replicateM nb (Gen.bytes (Range.linear 0 5))
  go initial rows
  where
    go bufs [] = pure (bufs, [])
    go bufs (Nothing : rest) = do
      junk <- Gen.bytes (Range.singleton 16)
      (bufs', vs) <- go bufs rest
      pure (bufs', junk : vs)
    go bufs (Just p : rest)
      | BS.length p <= 12 = do
          pad <- Gen.bytes (Range.singleton (12 - BS.length p))
          (bufs', vs) <- go bufs rest
          pure (bufs', (le32 (BS.length p) <> p <> pad) : vs)
      | otherwise = do
          b <- Gen.int (Range.constant 0 (nb - 1))
          gap <- Gen.bytes (Range.linear 0 3)
          let prefix = (bufs !! b) <> gap
              view = le32 (BS.length p) <> BS.take 4 p <> le32 b <> le32 (BS.length prefix)
          (bufs', vs) <- go (replaceAt b (prefix <> p) bufs) rest
          pure (bufs', view : vs)


-- | With probability 1/2, break one view: bad index, bad offset, wrong prefix or bad length.
mutateViews :: Int -> [BS.ByteString] -> [BS.ByteString] -> H.Gen [BS.ByteString]
mutateViews nb bufs views
  | null views = pure views
  | otherwise =
      Gen.frequency
        [ (1, pure views)
        , ( 1
          , do
              i <- Gen.int (Range.constant 0 (length views - 1))
              let v = views !! i
                  len = leI32 v 0
                  bi = leI32 v 8
                  bufLen = if bi >= 0 && bi < nb then BS.length (bufs !! bi) else 0
              v' <-
                Gen.choice
                  [ (\x -> setI32 8 x v) <$> Gen.element [nb, nb + 7, -1]
                  , (\x -> setI32 12 x v) <$> Gen.element [-1, bufLen - len + 1, bufLen, 2147483647]
                  , (\x -> setI32 12 x v) <$> Gen.int (Range.constant 0 bufLen)
                  , pure (BS.take 4 v <> BS.map (xor 0x01) (BS.take 1 (BS.drop 4 v)) <> BS.drop 5 v)
                  , (\x -> setI32 0 x v) <$> Gen.element [-1, -100, -2147483648]
                  , (\x -> setI32 0 (len + x) v) <$> Gen.int (Range.constant 1 20)
                  ]
              pure (replaceAt i v' views)
          )
        ]


viewValidity :: [Maybe a] -> H.Gen Validity
viewValidity rows = do
  useMask <- if any isNothing rows then pure True else Gen.bool
  if not useMask
    then pure Nothing
    else do
      off <- genBitOff
      pre <- Gen.list (Range.singleton off) Gen.bool
      let total = off + length rows
      post <- Gen.list (Range.singleton (8 * ceil8 total - total)) Gen.bool
      pure (Just (off, pre ++ map isJust rows ++ post))


modelViews :: Bool -> [BS.ByteString] -> [BS.ByteString] -> Validity -> Int
modelViews utf8 bufs views validity = firstFail (zipWith bad [0 ..] views)
  where
    nb = length bufs
    okUtf8 s = not utf8 || BS.isValidUtf8 s
    bad i v
      | not (isValid validity i) = False
      | len < 0 = True
      | len <= 12 = not (okUtf8 (BS.take len (BS.drop 4 v)))
      | bi < 0 || bi >= nb || off < 0 = True
      | off + len > BS.length buf = True
      | BS.take 4 (BS.drop 4 v) /= BS.take 4 payload = True
      | otherwise = not (okUtf8 payload)
      where
        len = leI32 v 0
        bi = leI32 v 8
        off = leI32 v 12
        buf = bufs !! bi
        payload = BS.take len (BS.drop off buf)


viewRefsProp :: H.Property
viewRefsProp = property $ do
  utf8Mode <- forAll Gen.bool
  nb <- forAll (Gen.int (Range.constant 1 3))
  rows <- forAll (Gen.list (Range.linear 0 30) (Gen.frequency [(6, Just <$> genPayload utf8Mode), (1, pure Nothing)]))
  (bufs, views0) <- forAll (buildViews nb rows)
  views <- forAll (mutateViews nb bufs views0)
  validity <- forAll (viewValidity rows)
  alV <- forAll genAlign
  alM <- forAll genAlign
  alBs <- forAll (Gen.list (Range.singleton nb) genAlign)
  let n = length views
  rs <-
    H.evalIO $
      withBytesAt alV (mconcat views) $ \vp ->
        withValidity alM validity $ \mp mo ->
          withAllAt (zip alBs bufs) $ \bps ->
            withArray bps $ \bpp ->
              withArray (map (fromIntegral . BS.length) bufs) $ \lp ->
                traverse (SIMD.viewRefsCheck vp n mp mo bpp lp nb) [False, True]
  let models = map (\u -> modelViews u bufs views validity) [False, True]
  mapM_ coverPaths (drop 1 models)
  rs === models


-- ---------- bit kernels ----------

popCountBitsProp :: H.Property
popCountBitsProp = property $ do
  bitoff <- forAll genBitOff
  nbits <- forAll (Gen.int (Range.linear 0 400))
  bits <- forAll (genBitsFor (bitoff + nbits))
  al <- forAll genAlign
  r <- H.evalIO (withBytesAt al (packBits bits) (\p -> SIMD.popCountBits p bitoff nbits))
  r === length (filter id (take nbits (drop bitoff bits)))


copyBitsProp :: H.Property
copyBitsProp = property $ do
  srcoff <- forAll genBitOff
  dstoff <- forAll genBitOff
  nbits <- forAll (Gen.int (Range.linear 0 300))
  srcBits <- forAll (genBitsFor (srcoff + nbits))
  guardLen <- forAll (Gen.int (Range.constant 0 9))
  dstInit <- forAll (Gen.bytes (Range.singleton (ceil8 (dstoff + nbits) + guardLen)))
  alS <- forAll genAlign
  alD <- forAll genAlign
  out <-
    H.evalIO $
      withBytesAt alS (packBits srcBits) $ \sp ->
        withBytesAt alD dstInit $ \dp -> do
          SIMD.copyBits dp dstoff sp srcoff nbits
          readBytes dp (BS.length dstInit)
  let dstBits = unpackBits dstInit
  out
    === packBits
      (take dstoff dstBits ++ take nbits (drop srcoff srcBits) ++ drop (dstoff + nbits) dstBits)


andBitsProp :: H.Property
andBitsProp = property $ do
  aoff <- forAll genBitOff
  boff <- forAll genBitOff
  nbits <- forAll (Gen.int (Range.linear 0 300))
  aBits <- forAll (genBitsFor (aoff + nbits))
  bBits <- forAll (genBitsFor (boff + nbits))
  dstInit <- forAll (Gen.bytes (Range.singleton (ceil8 nbits)))
  guardBytes <- forAll (Gen.bytes (Range.linear 0 9))
  alA <- forAll genAlign
  alB <- forAll genAlign
  alD <- forAll genAlign
  (cnt, out) <-
    H.evalIO $
      withBytesAt alA (packBits aBits) $ \ap ->
        withBytesAt alB (packBits bBits) $ \bp ->
          withBytesAt alD (dstInit <> guardBytes) $ \dp -> do
            c <- SIMD.andBits dp ap aoff bp boff nbits
            o <- readBytes dp (BS.length dstInit + BS.length guardBytes)
            pure (c, o)
  let res = zipWith (&&) (take nbits (drop aoff aBits)) (take nbits (drop boff bBits))
  (cnt, out) === (length (filter id res), packBits res <> guardBytes)


gatherBitsProp :: H.Property
gatherBitsProp = property $ do
  srcoff <- forAll genBitOff
  m <- forAll (Gen.int (Range.linear 1 200))
  srcBits <- forAll (genBitsFor (srcoff + m))
  idx <- forAll (Gen.list (Range.linear 0 200) (Gen.int (Range.constant 0 (m - 1))))
  let n = length idx
  dstInit <- forAll (Gen.bytes (Range.singleton (ceil8 n)))
  guardBytes <- forAll (Gen.bytes (Range.linear 0 9))
  alS <- forAll genAlign
  alI <- forAll genAlign
  alD <- forAll genAlign
  (cnt, out) <-
    H.evalIO $
      withBytesAt alS (packBits srcBits) $ \sp ->
        withBytesAt alI (encIdx idx) $ \ip ->
          withBytesAt alD (dstInit <> guardBytes) $ \dp -> do
            c <- SIMD.gatherBits dp sp srcoff ip n
            o <- readBytes dp (BS.length dstInit + BS.length guardBytes)
            pure (c, o)
  let picked = map (\i -> srcBits !! (srcoff + i)) idx
  (cnt, out) === (length (filter id picked), packBits picked <> guardBytes)


-- ---------- gathers, takes, rebases ----------

gatherProp :: Int -> (Ptr Word8 -> Ptr Word8 -> Ptr Int -> Int -> IO ()) -> H.Property
gatherProp width kernel = property $ do
  m <- forAll (Gen.int (Range.linear 1 40))
  src <- forAll (Gen.bytes (Range.singleton (m * width)))
  idx <- forAll (Gen.list (Range.linear 0 80) (Gen.int (Range.constant 0 (m - 1))))
  let n = length idx
  dstInit <- forAll (Gen.bytes (Range.singleton (n * width + 9)))
  alS <- forAll genAlign
  alI <- forAll genAlign
  alD <- forAll genAlign
  out <-
    H.evalIO $
      withBytesAt alS src $ \sp ->
        withBytesAt alI (encIdx idx) $ \ip ->
          withBytesAt alD dstInit $ \dp -> do
            kernel dp sp ip n
            readBytes dp (BS.length dstInit)
  out
    === mconcat (map (\i -> BS.take width (BS.drop (i * width) src)) idx)
    <> BS.drop (n * width) dstInit


rebaseProp :: Width -> Integer -> (Ptr Word8 -> Ptr Word8 -> Int -> Int64 -> IO ()) -> H.Property
rebaseProp w bound kernel = property $ do
  xs <- forAll (Gen.list (Range.linear 0 100) (Gen.integral (Range.linearFrom 0 (negate bound) bound)))
  delta <- forAll (Gen.integral (Range.linearFrom 0 (negate bound) bound))
  inPlace <- forAll Gen.bool
  guardBytes <- forAll (Gen.bytes (Range.linear 0 9))
  dstInit <- forAll (Gen.bytes (Range.singleton (wSize w * length xs)))
  alS <- forAll genAlign
  alD <- forAll genAlign
  let n = length xs
      srcBytes = encW w xs
      total = BS.length srcBytes + BS.length guardBytes
  out <-
    H.evalIO $
      if inPlace
        then withBytesAt alS (srcBytes <> guardBytes) $ \p -> do
          kernel p p n (fromInteger delta)
          readBytes p total
        else withBytesAt alS srcBytes $ \sp ->
          withBytesAt alD (dstInit <> guardBytes) $ \dp -> do
            kernel dp sp n (fromInteger delta)
            readBytes dp total
  out === encW w (map (+ delta) xs) <> guardBytes


takeOffsetsProp :: Width -> Integer -> (Ptr Word8 -> Ptr Word8 -> Ptr Int -> Int -> IO Int) -> H.Property
takeOffsetsProp w maxLen kernel = property $ do
  lens <-
    forAll
      ( Gen.list
          (Range.linear 1 6)
          (Gen.frequency [(1, Gen.integral (Range.linear 0 20)), (1, Gen.integral (Range.constant (maxLen `quot` 2) maxLen))])
      )
  start <- forAll (Gen.integral (Range.linear 0 5))
  idx <- forAll (Gen.list (Range.linear 0 60) (Gen.int (Range.constant 0 (length lens - 1))))
  alS <- forAll genAlign
  alI <- forAll genAlign
  alD <- forAll genAlign
  let n = length idx
      sel = map (lens !!) idx
      total = sum sel
      outLen = (n + 1) * wSize w
  (r, out) <-
    H.evalIO $
      withBytesAt alS (encW w (scanl (+) start lens)) $ \sp ->
        withBytesAt alI (encIdx idx) $ \ip ->
          withBytesAt alD (BS.replicate outLen 0xAA) $ \dp -> do
            r <- kernel dp sp ip n
            o <- readBytes dp outLen
            pure (r, o)
  H.cover 10 "overflow" (total > wHi w)
  H.cover 10 "fits" (total <= wHi w)
  if total > wHi w
    then r === -1
    else (r, out) === (fromInteger total, encW w (scanl (+) 0 sel))


-- | Require both outcomes of a validator to be generated often.
coverPaths :: Int -> H.PropertyT IO ()
coverPaths model = do
  H.cover 15 "valid" (model == -1)
  H.cover 15 "failing index" (model /= -1)


takeBytesProp :: H.Property
takeBytesProp = property $ do
  rows <- forAll (Gen.list (Range.linear 1 20) (Gen.bytes (Range.linear 0 12)))
  junk <- forAll (Gen.bytes (Range.linear 0 5))
  idx <- forAll (Gen.list (Range.linear 0 40) (Gen.int (Range.constant 0 (length rows - 1))))
  guardBytes <- forAll (Gen.bytes (Range.linear 0 9))
  alO <- forAll genAlign
  alS <- forAll genAlign
  alI <- forAll genAlign
  alD <- forAll genAlign
  let n = length idx
      dat = junk <> mconcat rows <> junk
      offs = scanl (+) (toInteger (BS.length junk)) (map (toInteger . BS.length) rows)
      expected = mconcat (map (rows !!) idx)
      total = BS.length expected
      run :: Width -> (Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Int -> Int -> IO ()) -> IO BS.ByteString
      run w kernel =
        withBytesAt alO (encW w offs) $ \op ->
          withBytesAt alS dat $ \sp ->
            withBytesAt alI (encIdx idx) $ \ip ->
              withBytesAt alD (BS.replicate total 0x55 <> guardBytes) $ \dp -> do
                kernel dp op sp ip n
                readBytes dp (total + BS.length guardBytes)
  outs <-
    H.evalIO $
      sequence
        [ run wI32 (\d o -> SIMD.takeBytesI32 d (castPtr o))
        , run wI64 (\d o -> SIMD.takeBytesI64 d (castPtr o))
        ]
  outs === replicate 2 (expected <> guardBytes)
