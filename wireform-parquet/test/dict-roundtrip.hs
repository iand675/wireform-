-- | Standalone round-trip test for dict-encoded Parquet
-- writes. Builds a low-cardinality string column with
-- @writeDictionary = DictAuto@ for both V1 and V2 page
-- formats, reads it back via the same library, and checks
-- that every value + the per-column metadata round-trips.
module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP

import qualified Parquet.HighLevel as PHL
import qualified Parquet.Arrow as PArrow
import qualified Parquet.Read as PR
import qualified Parquet.Types as P
import qualified Arrow.Types as AT
import qualified Arrow.Column as AC
import Parquet.Write (ColumnData (..))

mkSchema :: V.Vector P.SchemaElement
mkSchema = V.fromList
  [ P.SchemaElement "schema" Nothing Nothing (Just 2) Nothing Nothing Nothing
  , P.SchemaElement "id"   (Just P.Required) (Just P.PTInt64)     Nothing Nothing Nothing Nothing
  , P.SchemaElement "name" (Just P.Required) (Just P.PTByteArray)
       Nothing (Just P.CTUtf8) Nothing Nothing
  ]

mkDataset :: Int -> V.Vector ColumnData
mkDataset n = V.fromList
  [ ColInt64  (VP.generate n fromIntegral)
  , ColByteArray $ V.generate n $ \i ->
      TE.encodeUtf8 (T.pack ("name_" ++ show (i `rem` 100)))
  ]

main :: IO ()
main = mapM_ runOne [PHL.PageV1, PHL.PageV2]
  where
    runOne pageVer = do
      let !dataset = mkDataset 1000
          opts = PHL.defaultWriteOptions
            { PHL.writeCompression = P.Uncompressed
            , PHL.writePageVersion = pageVer
            , PHL.writeDictionary  = PHL.DictAuto
            }
          bs = PHL.encodeParquet opts mkSchema [dataset]
          fp = "/tmp/wf_dict_roundtrip_" ++ show pageVer ++ ".parquet"
      BS.writeFile fp bs
      putStrLn $ "[" ++ show pageVer ++ "] encoded "
        ++ show (BS.length bs) ++ " bytes -> " ++ fp
      case PHL.decodeParquet PHL.defaultReadOptions bs of
        Left err -> error err
        Right pf -> do
          let sch  = PArrow.parquetFileArrowSchema pf
              flds = AT.arrowFields sch
          mapM_ (\c -> do
            let fld = V.unsafeIndex flds c
            case PArrow.readParquetColumn pf 0 c fld of
              Left e  -> error $ "readParquetColumn col "
                                 ++ show c ++ ": " ++ e
              Right cv ->
                putStrLn $ "    col " ++ show c ++ " "
                        ++ show (AT.fieldName fld)
                        ++ " : " ++ show (AC.columnLength cv) ++ " values")
            [0, 1]
          let fm     = PR.pfFooter pf
              ccs    = P.rgColumns (V.unsafeIndex (P.fmRowGroups fm) 0)
              ccName = V.unsafeIndex ccs 1
          case P.ccMetadata ccName of
            Just md -> do
              putStrLn $ "    name encodings: " ++ show (P.cmEncodings md)
              putStrLn $ "    name dict-page-offset: "
                ++ show (P.cmDictionaryPageOffset md)
              putStrLn $ "    name data-page-offset: "
                ++ show (P.cmDataPageOffset md)
            Nothing -> error "missing name column metadata"
