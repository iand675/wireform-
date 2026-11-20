{-# LANGUAGE OverloadedStrings #-}

module NestedStatisticsRegression (tests) where

import Arrow.Types qualified as A
import Control.Monad (forM_, unless)
import Data.Aeson qualified as J
import Data.Bits (shiftR)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int32, Int64)
import Data.Ratio ((%))
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import GHC.Float (castDoubleToWord64, castFloatToWord32, castWord32ToFloat, castWord64ToDouble)
import Numeric (showHex)
import Parquet.Arrow qualified as Arrow
import Parquet.Compress qualified as Compress
import Parquet.HighLevel qualified as H
import Parquet.Nested qualified as N
import Parquet.PageIndex qualified as Index
import Parquet.Predicate qualified as Pred
import Parquet.Read qualified as R
import Parquet.Types
import Parquet.Write qualified as W
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.FilePath ((</>))


check :: String -> Bool -> IO ()
check label okay = unless okay (fail ("nested statistics: " ++ label))


right :: Either String a -> IO a
right = either fail pure


data Case = Case
  { caseName :: !String
  , caseType :: !N.LeafType
  , caseValues :: !(V.Vector N.LeafValue)
  , caseMin :: !(Maybe N.LeafValue)
  , caseMax :: !(Maybe N.LeafValue)
  , caseOrder :: !ColumnOrder
  , caseNans :: !(Maybe Int64)
  , caseQueries :: ![Pred.PValue]
  }


cases :: V.Vector Case
cases =
  V.fromList
    [ normal "i32" N.LtInt32 [N.LvInt32 minBound, N.LvInt32 7, N.LvInt32 maxBound, N.LvInt32 (-1)] (N.LvInt32 minBound) (N.LvInt32 maxBound) [Pred.PVInt32 minBound, Pred.PVInt32 7, Pred.PVInt32 maxBound, Pred.PVInt32 (-1)]
    , normal "u32" (N.LtInteger 32 False) [N.LvInt32 (-1), N.LvInt32 0, N.LvInt32 minBound] (N.LvInt32 0) (N.LvInt32 (-1)) [Pred.PVUnsigned 4294967295, Pred.PVUnsigned 0, Pred.PVUnsigned 2147483648]
    , normal "i64" N.LtInt64 [N.LvInt64 minBound, N.LvInt64 7, N.LvInt64 maxBound] (N.LvInt64 minBound) (N.LvInt64 maxBound) [Pred.PVInt64 minBound, Pred.PVInt64 7, Pred.PVInt64 maxBound]
    , normal "u64" (N.LtInteger 64 False) [N.LvInt64 (-1), N.LvInt64 0, N.LvInt64 minBound] (N.LvInt64 0) (N.LvInt64 (-1)) [Pred.PVUnsigned maxBound, Pred.PVUnsigned 0, Pred.PVUnsigned 9223372036854775808]
    , normal "decimal9" (N.LtDecimal 9 2) [N.LvInt32 17, N.LvInt32 (-99), N.LvInt32 0] (N.LvInt32 (-99)) (N.LvInt32 17) [Pred.PVDecimal ((-99) % 100), Pred.PVDecimal (17 % 100), Pred.PVDecimal 0]
    , normal "decimal18" (N.LtDecimal 18 2) [N.LvInt64 17, N.LvInt64 (-100000000000000000), N.LvInt64 0] (N.LvInt64 (-100000000000000000)) (N.LvInt64 17) [Pred.PVDecimal ((-100000000000000000) % 100), Pred.PVDecimal (17 % 100), Pred.PVDecimal 0]
    , wideDecimal 38 16
    , wideDecimal 76 32
    , normal "binary" N.LtBinary (map N.LvBinary ["a\0", "", "\xff", "a", "\0"]) (N.LvBinary "") (N.LvBinary "\xff") (map Pred.PVBinary ["a\0", "", "\xff", "a", "\0"])
    , normal "text" N.LtString (map N.LvString ["α", "", "\0", "aaa", "日本"]) (N.LvString "") (N.LvString "日本") (map Pred.PVText ["α", "", "\0", "aaa", "日本"])
    , normal "fixed9" (N.LtFixedBinary 9) (map (N.LvFixedBinary . BS.replicate 9) [128, 0, 255, 1]) (N.LvFixedBinary (BS.replicate 9 0)) (N.LvFixedBinary (BS.replicate 9 255)) []
    , normal "uuid" N.LtUuid (map (N.LvFixedBinary . BS.replicate 16) [128, 0, 255, 1]) (N.LvFixedBinary (BS.replicate 16 0)) (N.LvFixedBinary (BS.replicate 16 255)) []
    , floating "float-mixed" N.LtFloat (map (N.LvFloat . castWord32ToFloat) [0x7fc00001, 0x80000000, 0, 0xff800000, 0x7f800000]) (N.LvFloat (-1 / 0)) (N.LvFloat (1 / 0)) 1 [Pred.PVFloat (-1 / 0), Pred.PVFloat (-0), Pred.PVFloat 0, Pred.PVFloat (1 / 0)]
    , floating "double-mixed" N.LtDouble [N.LvDouble (castWord64ToDouble 0xfff8000000000001), N.LvDouble (castWord64ToDouble 0x7ff8000000000002), N.LvDouble 0, N.LvDouble (-0), N.LvDouble (-1.5), N.LvDouble 7.5, N.LvDouble (castWord64ToDouble 0x7ff0000000000001)] (N.LvDouble (-1.5)) (N.LvDouble 7.5) 3 [Pred.PVDouble 0, Pred.PVDouble (-0), Pred.PVDouble (-1.5), Pred.PVDouble 7.5]
    , floating "float-nans" N.LtFloat (map (N.LvFloat . castWord32ToFloat) [0x7fc00001, 0x7f800001, 0xffc00007, 0xff800002]) (N.LvFloat (castWord32ToFloat 0xffc00007)) (N.LvFloat (castWord32ToFloat 0x7fc00001)) 4 []
    , floating "double-nans" N.LtDouble (map (N.LvDouble . castWord64ToDouble) [0x7ff8000000000011, 0xfff8000000000022, 0x7ff0000000000001]) (N.LvDouble (castWord64ToDouble 0xfff8000000000022)) (N.LvDouble (castWord64ToDouble 0x7ff8000000000011)) 3 []
    , floating "float-minus-zero" N.LtFloat [N.LvFloat (-0)] (N.LvFloat (-0)) (N.LvFloat (-0)) 0 [Pred.PVFloat 0, Pred.PVFloat (-0)]
    , floating "float-plus-zero" N.LtFloat [N.LvFloat 0] (N.LvFloat 0) (N.LvFloat 0) 0 [Pred.PVFloat 0, Pred.PVFloat (-0)]
    , floating "double-zeros" N.LtDouble [N.LvDouble 0, N.LvDouble (-0)] (N.LvDouble (-0)) (N.LvDouble 0) 0 [Pred.PVDouble 0, Pred.PVDouble (-0)]
    , floating "half-nans" N.LtFloat16 (map N.LvFloat16 [0x7e01, 0x7c01, 0xfe07, 0xfc02]) (N.LvFloat16 0xfe07) (N.LvFloat16 0x7e01) 4 []
    , floating "half-values" N.LtFloat16 (map N.LvFloat16 [0, 0x8000, 0x3c00, 0xbc00, 0x0001]) (N.LvFloat16 0xbc00) (N.LvFloat16 0x3c00) 0 [Pred.PVDouble 0, Pred.PVDouble (-0), Pred.PVDouble (-1), Pred.PVDouble 1]
    , normal "bool" N.LtBool (map N.LvBool [True, False, True, False, True, False, True, False, True]) (N.LvBool False) (N.LvBool True) [Pred.PVBool False, Pred.PVBool True]
    , (normal "int96" N.LtInt96 [clock 0 0, clock minBound 0, clock maxBound (-1), clock minBound maxBound, clock maxBound maxBound, clock 0 minBound] (clock 0 minBound) (clock maxBound maxBound) []) {caseOrder = Int96TimestampOrder}
    , (normal "int96-counterexample" N.LtInt96 [clock minBound 0, clock 0 0] (clock minBound 0) (clock 0 0) []) {caseOrder = Int96TimestampOrder}
    , normal "timestamp96-counterexample" (N.LtTimestamp96 True LtNanos) [clock minBound 0, clock 0 0] (clock 0 0) (clock minBound 0) []
    , normal "timestamp96-wide" (N.LtTimestamp96 False LtMicros) (map (N.LvFixedBinary . signedLittle12) [-(2 ^ 95), -1, 0, 2 ^ 63, 2 ^ 95 - 1]) (N.LvFixedBinary (signedLittle12 (-(2 ^ 95)))) (N.LvFixedBinary (signedLittle12 (2 ^ 95 - 1))) []
    , Case "all-null-double" N.LtDouble V.empty Nothing Nothing IEEE754TotalOrder (Just 0) []
    , Case "all-null-int96" N.LtInt96 V.empty Nothing Nothing Int96TimestampOrder Nothing []
    ]
  where
    normal name typ vals low high queries = Case name typ (V.fromList vals) (Just low) (Just high) TypeDefinedOrder Nothing queries
    floating name typ vals low high nans queries = (normal name typ vals low high queries) {caseOrder = IEEE754TotalOrder, caseNans = Just nans}
    wideDecimal precision width =
      let bound = 10 ^ precision - 1; value = N.LvFixedBinary . signedBig width
      in normal ("decimal" ++ show precision) (N.LtDecimal (fromIntegral precision) 2) (map value [0, -1, bound, -bound]) (value (-bound)) (value bound) (map (Pred.PVDecimal . (% 100)) [0, -1, bound, -bound])


clock :: Int64 -> Int32 -> N.LeafValue
clock nanos day = N.LvFixedBinary (encode (B.int64LE nanos <> B.int32LE day))


signedBig :: Int -> Integer -> BS.ByteString
signedBig width value = BS.pack (map (\index -> fromInteger (value `shiftR` (8 * (width - 1 - index)))) [0 .. width - 1])


signedLittle12 :: Integer -> BS.ByteString
signedLittle12 value = BS.pack (map (\index -> fromInteger (value `shiftR` (8 * index))) [0 .. 11])


encode :: B.Builder -> BS.ByteString
encode = BL.toStrict . B.toLazyByteString


plain :: N.LeafValue -> BS.ByteString
plain value = case value of
  N.LvInt32 x -> encode (B.int32LE x)
  N.LvInt64 x -> encode (B.int64LE x)
  N.LvFloat16 x -> encode (B.word16LE x)
  N.LvFloat x -> encode (B.word32LE (castFloatToWord32 x))
  N.LvDouble x -> encode (B.word64LE (castDoubleToWord64 x))
  N.LvBool x -> BS.singleton (if x then 1 else 0)
  N.LvString x -> TE.encodeUtf8 x
  N.LvBinary x -> x
  N.LvFixedBinary x -> x


hex :: BS.ByteString -> String
hex = concatMap (\byte -> let text = showHex byte "" in if length text == 1 then '0' : text else text) . BS.unpack


tests :: IO ()
tests = do
  output <- lookupEnv "WIREFORM_PARQUET_PRIMITIVE_OUTPUT"
  forM_ output $ \directory -> do
    createDirectoryIfMissing True directory
    BL.writeFile (directory </> "nested-stats-models.json") $
      J.encode $
        V.map
          ( \config ->
              J.object
                [ "name" J..= caseName config
                , "type" J..= show (caseType config)
                , "values" J..= V.map (hex . plain) (caseValues config)
                , "min" J..= fmap (hex . plain) (caseMin config)
                , "max" J..= fmap (hex . plain) (caseMax config)
                , "order" J..= show (caseOrder config)
                , "nans" J..= caseNans config
                ]
          )
          cases
  forM_ [Uncompressed, GZip, Snappy, ZSTD, LZ4Raw, LZO, Brotli] $ \codec -> case Compress.validateCompression codec of
    Left _ -> pure ()
    Right () -> forM_ [W.PageV1, W.PageV2] $ \version -> forM_ [False, True] $ \indexed -> V.forM_ cases $ \config -> do
      let schema = V.singleton ("v", N.NSOptional (N.NSList (N.NSOptional (N.NSPrimitive (caseType config)))))
          rows = V.singleton (V.fromList [N.NRNull, N.NRList V.empty, N.NRList (V.cons N.NRNull (V.map N.NRLeaf (caseValues config)))])
          options = H.defaultWriteOptions {H.writeCompression = codec, H.writePageVersion = version, H.writePageIndex = indexed, H.writeBloomFilters = ["v"]}
      bytes <- right (H.encodeParquetNestedWith options schema rows)
      file <- right (R.loadParquetFile bytes)
      let metadata = R.pfFooter file; chunk = V.head (rgColumns (V.head (fmRowGroups metadata)))
      cm <- maybe (fail "missing column metadata") pure (ccMetadata chunk)
      stats <- maybe (fail "missing statistics") pure (cmStatistics cm)
      context <- maybe (fail "missing statistics context") pure (Pred.statisticsContext metadata 0)
      case caseType config of
        N.LtFloat16 -> do
          arrowSchema <- right (Arrow.parquetFileArrowSchemaChecked file)
          arrowColumns <- either (fail . show) pure (Arrow.parquetRowGroupToArrow arrowSchema file 0)
          actualRows <- right (Arrow.columnArrayToNestedRows (V.head (A.arrowFields arrowSchema)) (V.head arrowColumns))
          check "binary16 nested Arrow round-trip retains all raw bits and nulls" (actualRows == V.head rows)
        _ -> pure ()
      check (caseName config ++ " min") (statMinValue stats == fmap plain (caseMin config))
      check (caseName config ++ " max") (statMaxValue stats == fmap plain (caseMax config))
      check "truthful null count" (statNullCount stats == Just 3)
      check "truthful NaN count" (statNanCount stats == caseNans config)
      check "explicit ordering survives footer" (fmColumnOrders metadata == Just (V.singleton (caseOrder config)))
      ci <- right (Index.readColumnIndex file 0 0)
      oi <- right (Index.readOffsetIndex file 0 0)
      check "requested index presence" ((ci /= Nothing) == indexed && (oi /= Nothing) == indexed)
      forM_ ci $ \index -> do
        check "page bounds equal chunk bounds" (ciMinValues index == V.singleton (maybe BS.empty plain (caseMin config)) && ciMaxValues index == V.singleton (maybe BS.empty plain (caseMax config)))
        check "page null and NaN counts" (ciNullCounts index == Just (V.singleton 3) && ciNanCounts index == fmap V.singleton (caseNans config))
        check "null page marker" (ciNullPages index == V.singleton (V.null (caseValues config)))
      forM_ (caseQueries config) $ \query -> do
        check "chunk statistics must retain every matching value" (Pred.evalColumnChunk context chunk (Pred.PEq query) /= Pred.PSkip)
        forM_ ci $ \index -> check "page statistics must retain every matching value" (V.all (/= Pred.PSkip) (Pred.evalPagesByColumnIndex context index (Pred.PEq query)))
      forM_ output $ \directory -> BS.writeFile (directory </> ("nested-stats-" ++ caseName config ++ "-" ++ show codec ++ "-" ++ show version ++ "-" ++ show indexed ++ ".parquet")) bytes
  putStrLn "Nested signed/unsigned/decimal/IEEE/INT96/signed96 statistics and pruning regressions passed."
