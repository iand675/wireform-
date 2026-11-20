{-# LANGUAGE OverloadedStrings #-}

module AlpRegression (run) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, tryReadMVar, yield)
import Control.Exception (evaluate, finally)
import Control.Monad (unless)
import Data.Bits (setBit, shiftL, testBit, (.&.))
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as L
import Data.Either (isLeft)
import Data.Int (Int32, Int64)
import Data.Vector.Primitive qualified as P
import Data.Word (Word32, Word64, Word8)
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Parquet.Alp qualified as Alp
import System.Mem (performGC)


data ExceptionValue = ExceptionValue
  { exceptionPosition :: !Int
  , exceptionBits :: !Word64
  }


data TestVector = TestVector
  { vectorCount :: !Int
  , vectorExponent :: !Int
  , vectorFactor :: !Int
  , vectorWidth :: !Int
  , vectorFrame :: !Word64
  , vectorDeltas :: !(P.Vector Word64)
  , vectorExceptions :: ![ExceptionValue]
  }


data PageFixture = PageFixture
  { fixtureBytes :: !BS.ByteString
  , fixtureVectors :: ![TestVector]
  , fixtureCount :: !Int
  }


run :: IO ()
run = do
  mapM_ (\width -> mapM_ (fixed width) [0 .. width * 8]) [4, 8]
  mapM_ decimalParameters [4, 8]
  mapM_ (\width -> mapM_ (sizing width) [3 .. 15]) [4, 8]
  mapM_ malformed [4, 8]
  ok <- H.check $ H.withTests 300 $ H.property $ do
    width <- H.forAll (Gen.element [4, 8])
    bits <- H.forAll (Gen.int (Range.linear 0 (width * 8)))
    count <- H.forAll (Gen.int (Range.linear 0 257))
    logSize <- H.forAll (Gen.int (Range.linear 3 8))
    decimalExponent <- H.forAll (Gen.int (Range.linear 0 (if width == 4 then 10 else 18)))
    factor <- H.forAll (Gen.int (Range.linear 0 decimalExponent))
    seed <- H.forAll (Gen.word64 Range.constantBounded)
    let page = fixture width logSize count bits decimalExponent factor seed
    H.assert (matches width page)
  unless ok (fail "ALP generated page regression")
  gcPressure
  putStrLn "ALP: full bit widths, vector sizes, malformed pages, IEEE exceptions and 300 generated pages passed"
  where
    fixed width bits = do
      let page = fixture width 3 19 bits (if width == 4 then 10 else 18) 3 0x9876543210
      unless (matches width page) (fail ("ALP full-width model mismatch: " <> show (width, bits)))
      mapM_
        ( \offset ->
            unless
              (matches width (page {fixtureBytes = BS.drop offset (BS.replicate offset 0xff <> fixtureBytes page)}))
              (fail "ALP misaligned input differs")
        )
        [1, 3, 7, 15]
    decimalParameters width =
      mapM_
        ( \e ->
            mapM_
              ( \f ->
                  unless
                    (matches width (fixture width 3 19 (width * 8) e f 9191))
                    (fail ("ALP decimal parameter mismatch: " <> show (width, e, f)))
              )
              [0 .. e]
        )
        [0 .. if width == 4 then 10 else 18]
    sizing width logSize = do
      let page = fixture width logSize ((1 `shiftL` logSize) + 1) 7 4 3 31337
      unless (matches width page) (fail ("ALP vector-size mismatch: " <> show (width, logSize)))


-- Multiple 32768-row vectors cross the bounded native-call limits. Keep a
-- collector active until every fresh result has been checked, exercising
-- ownership of the movable output between chunks and exception patches.
gcPressure :: IO ()
gcPressure = do
  stop <- newEmptyMVar
  done <- newEmptyMVar
  let collect = do
        finished <- tryReadMVar stop
        case finished of
          Just () -> pure ()
          Nothing -> performGC >> yield >> collect
  _ <- forkIO (collect `finally` putMVar done ())
  let check width seed = do
        let page = fixture width 15 65537 (width * 8) (if width == 4 then 10 else 18) 3 seed
        correct <- evaluate (matches width page)
        unless correct (fail "ALP large page changed under concurrent GC")
  mapM_ (\seed -> check 4 seed >> check 8 seed) [1 .. 8]
    `finally` (putMVar stop () >> takeMVar done)


matches :: Int -> PageFixture -> Bool
matches width page
  | width == 4 =
      fmap (P.map castFloatToWord32) (Alp.decodeAlpFloat (fixtureCount page) (fixtureBytes page))
        == Right (P.concat (map expectedFloat (fixtureVectors page)))
  | otherwise =
      fmap (P.map castDoubleToWord64) (Alp.decodeAlpDouble (fixtureCount page) (fixtureBytes page))
        == Right (P.concat (map expectedDouble (fixtureVectors page)))


expectedFloat :: TestVector -> P.Vector Word32
expectedFloat v = P.generate (vectorCount v) $ \i ->
  fromIntegral $
    expectedBits i (vectorExceptions v) $
      fromIntegral $
        castFloatToWord32 $
          (fromIntegral (fromIntegral (vectorFrame v + vectorDeltas v P.! i) :: Int32) * (positive !! vectorFactor v) :: Float)
            * (negative !! vectorExponent v)


expectedDouble :: TestVector -> P.Vector Word64
expectedDouble v = P.generate (vectorCount v) $ \i ->
  expectedBits i (vectorExceptions v) $
    castDoubleToWord64 $
      (fromIntegral (fromIntegral (vectorFrame v + vectorDeltas v P.! i) :: Int64) * (positive !! vectorFactor v) :: Double)
        * (negative !! vectorExponent v)


expectedBits :: Int -> [ExceptionValue] -> Word64 -> Word64
expectedBits i exceptions original = foldl' (\value e -> if exceptionPosition e == i then exceptionBits e else value) original exceptions


positive :: Fractional a => [a]
positive = [1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10, 1e11, 1e12, 1e13, 1e14, 1e15, 1e16, 1e17, 1e18]


negative :: Fractional a => [a]
negative = [1e0, 1e-1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7, 1e-8, 1e-9, 1e-10, 1e-11, 1e-12, 1e-13, 1e-14, 1e-15, 1e-16, 1e-17, 1e-18]


fixture :: Int -> Int -> Int -> Int -> Int -> Int -> Word64 -> PageFixture
fixture width logSize count bits decimalExponent factor seed = PageFixture bytes vectors count
  where
    vectorSize = 1 `shiftL` logSize
    vectors = makeVectors 0 count
    makeVectors offset remaining
      | remaining == 0 = []
      | otherwise =
          let n = min vectorSize remaining
              mask = if bits == 64 then maxBound else (1 `shiftL` bits) - 1
              deltas = P.generate n (\i -> if i == 0 then 0 else if i == 1 then mask else (seed + fromIntegral (offset + i) * 6364136223846793005) .&. mask)
              exceptions =
                if width == 4
                  then [ExceptionValue (n - 1) 0x7fc0dead, ExceptionValue 0 0x80000000]
                  else [ExceptionValue (n - 1) 0x7ff800deadbeef00, ExceptionValue 0 0x8000000000000000]
          in TestVector n decimalExponent factor bits (1 `shiftL` (width * 8 - 1)) deltas exceptions : makeVectors (offset + n) (remaining - n)
    encoded = map (encodeVector width) vectors
    offsets = init (scanl (+) (length vectors * 4) (map BS.length encoded))
    bytes =
      strict $
        B.word8 0
          <> B.word8 0
          <> B.word8 (fromIntegral logSize)
          <> B.int32LE (fromIntegral count)
          <> foldMap (B.word32LE . fromIntegral) offsets
          <> foldMap B.byteString encoded


encodeVector :: Int -> TestVector -> BS.ByteString
encodeVector width v =
  strict $
    B.word8 (fromIntegral (vectorExponent v))
      <> B.word8 (fromIntegral (vectorFactor v))
      <> B.word16LE (fromIntegral (length (vectorExceptions v)))
      <> word width (vectorFrame v)
      <> B.word8 (fromIntegral (vectorWidth v))
      <> packed
      <> foldMap (B.word16LE . fromIntegral . exceptionPosition) (vectorExceptions v)
      <> foldMap (word width . exceptionBits) (vectorExceptions v)
  where
    bitCount = vectorCount v * vectorWidth v
    packed = foldMap (B.word8 . byte) [0 .. (bitCount + 7) `quot` 8 - 1]
    byte offset =
      foldl'
        ( \acc b ->
            let global = offset * 8 + b
            in if global < bitCount && testBit (vectorDeltas v P.! (global `quot` vectorWidth v)) (global `rem` vectorWidth v)
                 then setBit acc b
                 else acc
        )
        (0 :: Word8)
        [0 .. 7]


word :: Int -> Word64 -> B.Builder
word 4 = B.word32LE . fromIntegral
word _ = B.word64LE


strict :: B.Builder -> BS.ByteString
strict = L.toStrict . B.toLazyByteString


malformed :: Int -> IO ()
malformed width = do
  let page = fixture width 3 3 3 4 3 42
      bytes = fixtureBytes page
      decode n raw = if width == 4 then (() <$ Alp.decodeAlpFloat n raw) else (() <$ Alp.decodeAlpDouble n raw)
      replace offset values = BS.take offset bytes <> values <> BS.drop (offset + BS.length values) bytes
      mutants =
        [ replace 0 (BS.singleton 1)
        , replace 1 (BS.singleton 1)
        , replace 2 (BS.singleton 2)
        , replace 2 (BS.singleton 16)
        , replace 3 (strict (B.int32LE (-1)))
        , replace 7 (strict (B.word32LE 5))
        , replace 11 (BS.singleton 255)
        , replace 12 (BS.singleton 5)
        , replace (15 + width) (BS.singleton (fromIntegral (width * 8 + 1)))
        , replace (16 + width) (BS.pack [0, 0x80])
        , replace (18 + width) (strict (B.word16LE 3))
        , bytes <> BS.singleton 0
        ]
  mapM_ (\raw -> unless (isLeft (decode 3 raw)) (fail "ALP accepted a malformed page")) mutants
  mapM_ (\n -> unless (isLeft (decode 3 (BS.take n bytes))) (fail "ALP accepted a truncated page")) [0 .. BS.length bytes - 1]
  mapM_ (\n -> unless (isLeft (decode n bytes)) (fail "ALP accepted a wrong count")) [-1, 0, 2, 4, maxBound]
  unless (matches width (fixture width 3 0 0 0 0 0)) (fail "ALP empty page")
