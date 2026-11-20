{-# LANGUAGE OverloadedStrings #-}

module LargeOffsetRegression (tests) where

import Arrow.Column qualified as A
import Arrow.Types qualified as T
import Control.Exception (evaluate)
import Control.Monad (forM_, unless)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Either (isLeft)
import Data.List (isInfixOf)
import Data.Vector qualified as V
import Data.Vector.Primitive qualified as P
import Data.Vector.Storable qualified as S
import GHC.Conc (getAllocationCounter)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Parquet.Arrow qualified as Arrow
import Parquet.HighLevel qualified as High
import Parquet.Page qualified as Page
import Parquet.Predicate qualified as Pred
import Parquet.Read qualified as Read
import Parquet.Types qualified as Types
import Parquet.Write qualified as Write
import System.Mem (performGC)


check :: String -> Bool -> IO ()
check name ok = unless ok (fail ("Large-offset regression: " ++ name))


plain :: [BS.ByteString] -> BS.ByteString
plain = BL.toStrict . B.toLazyByteString . foldMap (\bytes -> B.word32LE (fromIntegral (BS.length bytes)) <> B.byteString bytes)


page :: Page.PageType -> BS.ByteString -> BS.ByteString
page typ body = Write.encodePageHeader (Page.PageHeader typ (Just (fromIntegral (BS.length body))) (Just (fromIntegral (BS.length body)))) <> body


tests :: IO ()
tests = do
  valid <- H.check $ H.withTests 300 $ H.property $ do
    values <- H.forAll (Gen.list (Range.linear 0 64) (BS.pack <$> Gen.list (Range.linear 0 256) (Gen.word8 Range.constantBounded)))
    let bytes = plain values
        count = length values
        expected = BS.concat values
    small <- H.evalEither (Read.decodePlainBinaryBuffers count bytes)
    large <- H.evalEither (Read.decodePlainLargeBinaryBuffers count bytes)
    Read.binaryData small H.=== expected
    Read.binaryData large H.=== expected
    Read.binaryOffsets large H.=== S.map fromIntegral (Read.binaryOffsets small)
  check "32/64-bit PLAIN buffers preserve identical bytes and offsets" valid
  forM_ [(-1, BS.empty), (maxBound, BS.empty), (1, BS.pack [0, 0, 0]), (1, BS.pack [255, 255, 255, 255]), (1, BS.pack [4, 0, 0, 0, 1]), (0, "trailing")] $ \(count, bytes) ->
    check "large PLAIN bounds reject malformed inputs" (isLeft (Read.decodePlainLargeBinaryBuffers count bytes))
  let values = V.fromList ["", "alpha", "\NUL\255"]
      dictionary = Write.Dictionary (Write.ColByteArray values) (P.fromList [0, 1, 2, 1, 0])
      mixed = Write.encodeDictPage dictionary <> Write.encodeDictDataPage dictionary <> page (Page.PtDataPage (Page.DataPageHeader 2 0)) (plain ["tail", ""])
  small <- either fail pure (Read.readBinaryBuffersColumnChunk Types.Uncompressed mixed)
  large <- either fail pure (Read.readLargeBinaryBuffersColumnChunk Types.Uncompressed mixed)
  check "mixed dictionary/plain pages retain64-bit offsets" (Read.binaryData large == Read.binaryData small && Read.binaryOffsets large == S.map fromIntegral (Read.binaryOffsets small))
  -- A small shared dictionary requests exactly2GiB of output. The32-bit
  -- reader must reject before allocating indices/payload; the64-bit reader
  -- must get past that prefix and reject the subsequent invalid page without
  -- materializing the2GiB payload. The full valid file has a separate external
  -- resource test, keeping the ordinary regression suite lightweight.
  let expansion = Write.Dictionary (Write.ColByteArray (V.singleton (BS.replicate 32768 97))) (P.replicate 65536 0)
      oversized = Write.encodeDictPage expansion <> Write.encodeDictDataPage expansion
      invalidTail = page (Page.PtDataPage (Page.DataPageHeader 1 99)) BS.empty
      complete = oversized <> invalidTail
  _ <- evaluate (BS.length complete)
  _ <- evaluate (BS.length oversized)
  performGC
  before <- getAllocationCounter
  narrowRejected <- evaluate (isLeft (Read.readBinaryBuffersColumnChunk Types.Uncompressed oversized))
  wideFailure <- evaluate (case Read.readLargeBinaryBuffersColumnChunk Types.Uncompressed complete of Left message -> "99" `isInfixOf` message; Right _ -> False)
  after <- getAllocationCounter
  check "large prefix preflight keeps width and bounds without2GiB allocation" (narrowRejected && wideFailure && before - after < 4 * 1024 * 1024)
  forM_ [False, True] $ \indexed -> do
    let root = Types.SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing Nothing Nothing
        leaf = Types.SchemaElement "v" (Just Types.Required) (Just Types.PTByteArray) Nothing (Just Types.CTUtf8) (Just Types.LTString) Nothing Nothing
        source = High.encodeParquet High.defaultWriteOptions {High.writeCompression = Types.Uncompressed, High.writePageIndex = indexed} (V.fromList [root, leaf]) [V.singleton (Write.ColByteArray (V.fromList ["alpha", "beta"]))]
    file <- either fail pure (High.decodeParquet High.defaultReadOptions source)
    native <- either fail pure (Arrow.parquetFileArrowSchemaChecked file)
    let target = (V.head (T.arrowFields native)) {T.fieldType = T.ALargeUtf8}
    direct <- either fail pure (Arrow.readParquetColumn file 0 0 target)
    check "direct large string read" (largeStrings direct == Just ["alpha", "beta"])
    projected <- either (fail . show) pure (Arrow.parquetRowGroupToArrow native {T.arrowFields = V.singleton target} file 0)
    check "flat schema large string projection" (largeStrings (V.head projected) == Just ["alpha", "beta"])
    (_, kept) <- either fail pure (Arrow.readParquetColumnWithPagePruning file 0 0 target (Pred.PEq (Pred.PVText "alpha")))
    check "indexed/fallback large string projection" (largeStrings kept == Just ["alpha", "beta"])
    (_, skipped) <- either fail pure (Arrow.readParquetColumnWithPagePruning file 0 0 target (Pred.PGt (Pred.PVText "zzz")))
    check "empty indexed selection retains64-bit storage" (largeStrings skipped == Just (if indexed then [] else ["alpha", "beta"]))
  where
    largeStrings (A.ColLargeUtf8 _ offsets bytes) = Just (map (\i -> BS.take (fromIntegral (offsets S.! (i + 1) - offsets S.! i)) (BS.drop (fromIntegral (offsets S.! i)) bytes)) [0 .. S.length offsets - 2])
    largeStrings _ = Nothing
