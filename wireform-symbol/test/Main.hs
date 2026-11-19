{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (evaluate)
import Control.Monad (forM, forM_, replicateM, unless)
import qualified Data.Binary as Binary
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Unsafe as BSU
import Data.Hashable (hash)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Exts (Int (..), Ptr (..))
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Symbol
import Symbol.SipHash
import System.Mem (performMajorGC)
import Test.Syd
import Test.Syd.Hedgehog ()

refKey :: SipKey
refKey = SipKey 0x0706050403020100 0x0f0e0d0c0b0a0908

-- | Hash a ByteString through each 'Bytes' view: raw memory, and a
-- ByteArray# slice at a non-zero offset (a Text that is a suffix of a
-- longer one).
viaAddr :: (Bytes -> a) -> ByteString -> IO a
viaAddr f bs = BSU.unsafeUseAsCStringLen bs $ \(Ptr a, I# n) -> evaluate (f (BytesAddr a n))

viaSlice :: (Bytes -> a) -> ByteString -> a
viaSlice f bs = case SBS.toShort ("pad" <> bs) of
  SBS.SBS ba -> case BS.length bs of I# n -> f (BytesBA ba 3# n)

genUtf8 :: Gen Text
genUtf8 = Gen.text (Range.linear 0 40) (Gen.frequency [(5, Gen.alphaNum), (1, Gen.unicode)])

-- | Strings no other test produces, built at run time so no CAF keeps them.
fresh :: String -> Int -> [Text]
fresh tag n = map (\i -> T.pack (tag <> "-" <> show i)) [1 .. n]

main :: IO ()
main = sydTest $ do
  describe "SipHash" $ do
    it "SipHash-2-4 matches the reference vectors" $ do
      let msg n = BS.pack (map fromIntegral [0 .. n - 1 :: Int])
      viaSlice (sipHash24 refKey) (msg 0) `shouldBe` 0x726fdb47dd0e0e31
      viaSlice (sipHash24 refKey) (msg 15) `shouldBe` 0xa129ca6149be45e5
      viaAddr (sipHash24 refKey) (msg 15) `shouldReturn` 0xa129ca6149be45e5
    it "raw memory and array slices hash alike (1-3 and 2-4)" $ property $ do
      bs <- forAll (Gen.bytes (Range.linear 0 100))
      a13 <- evalIO (viaAddr (sipHash13 refKey) bs)
      a13 === viaSlice (sipHash13 refKey) bs
      a24 <- evalIO (viaAddr (sipHash24 refKey) bs)
      a24 === viaSlice (sipHash24 refKey) bs

  describe "interning" $ do
    it "every Textual view of the same characters is the same symbol" $ property $ do
      t <- forAll genUtf8
      let s = Symbol.intern t
          others =
            [ Symbol.intern (TE.encodeUtf8 t)
            , Symbol.intern (SBS.toShort (TE.encodeUtf8 t))
            , Symbol.intern (T.unpack t)
            , Symbol.intern (T.drop 1 (T.cons 'x' t)) -- a Text at a non-zero offset
            ]
      forM_ others $ \o -> do
        o === s
        hash o === hash s
      Symbol.unintern s === t
      Symbol.unintern s === TE.encodeUtf8 t
      Symbol.unintern s === T.unpack t
      Symbol.symbolLength s === BS.length (TE.encodeUtf8 t)
    it "equality is content equality; order is Text order" $ property $ do
      a <- forAll genUtf8
      b <- forAll (Gen.choice [genUtf8, pure a])
      let sa = Symbol.intern a
          sb = Symbol.intern b
      (sa == sb) === (a == b)
      compare sa sb === compare a b
    it "invalid UTF-8 is replaced, like decodeUtf8Lenient" $ do
      let bad = BS.pack [0x61, 0xff, 0x62, 0xc3]
      Symbol.intern bad `shouldBe` Symbol.intern (TE.decodeUtf8Lenient bad)
      Symbol.intern (SBS.toShort bad) `shouldBe` Symbol.intern (TE.decodeUtf8Lenient bad)
    it "Binary and Show/Read round-trip" $ property $ do
      s <- Symbol.intern <$> forAll genUtf8
      Binary.decode (Binary.encode s) === s
      read (show s) === s
    it "lookup finds live symbols and never creates one" $ do
      let t = T.pack "lookup-probe-only"
      Symbol.lookup t `shouldReturn` Nothing
      s <- Symbol.internIO t
      Symbol.lookup t `shouldReturn` Just s
      Symbol.lookup (TE.encodeUtf8 t) `shouldReturn` Just s

  sequential . describe "garbage collection" $ do
    it "collecting other symbols never evicts a live one" $ do
      -- The failure mode of a finalizer-driven interner over a table whose
      -- delete drops colliding keys: symbols dying evicts live neighbours,
      -- and re-interning a live symbol's text then mints a second copy.
      let liveTexts = fresh "live" 4000
      kept <- forM liveTexts Symbol.internIO
      forM_ [1 .. 3 :: Int] $ \gen -> do
        forM_ (fresh ("dead" <> show gen) 40000) (\t -> Symbol.internIO t >>= evaluate)
        performMajorGC
      -- New keys after the collection: every shard sweeps its dead entries
      -- as it reaches capacity.
      forM_ (fresh "after" 40000) (\t -> Symbol.internIO t >>= evaluate)
      again <- forM liveTexts Symbol.internIO
      let broken = length (filter not (zipWith (==) kept again))
      broken `shouldBe` 0
      missing <- length . filter (== Nothing) <$> mapM Symbol.lookup liveTexts
      missing `shouldBe` 0
    it "collected symbols are dropped" $ do
      let texts = fresh "dropped" 20000
      forM_ texts (\t -> Symbol.internIO t >>= evaluate)
      performMajorGC
      stillThere <- length . filter (/= Nothing) <$> mapM Symbol.lookup texts
      stillThere `shouldBe` 0
      Symbol.sweepSymbolTable
      n <- Symbol.symbolTableSize
      unless (n < 20000) $ expectationFailure ("table still holds " <> show n <> " live symbols")

  describe "concurrency" $ do
    it "threads interning the same strings agree" $ do
      let texts = fresh "shared" 3000
          threads = 8 :: Int
      dones <- replicateM threads newEmptyMVar
      forM_ (zip [0 :: Int ..] dones) $ \(i, done) -> forkIO $ do
        -- Each thread interns in a different order, with churn in between.
        let order = sortBy (comparing (\t -> hash (T.pack (show i) <> t))) texts
        syms <- forM order $ \t -> do
          _ <- Symbol.internIO (t <> "-churn-" <> T.pack (show i)) >>= evaluate
          (,) t <$> Symbol.internIO t
        putMVar done (sortBy (comparing fst) syms)
      results <- mapM takeMVar dones
      case results of
        [] -> pure ()
        (r0 : rest) -> forM_ rest $ \r -> map snd r `shouldBe` map snd r0
      _ <- evaluate (length results)
      pure ()
