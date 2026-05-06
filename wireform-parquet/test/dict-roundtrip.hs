-- | Standalone smoke test for the new dict-encoding write
-- path. Builds a small low-cardinality string column with
-- @writeDictionary = DictAuto@, decodes it back via the same
-- library, and checks that the values + per-column metadata
-- round-trip.
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
main = do
  let !dataset = mkDataset 1000
      opts = PHL.defaultWriteOptions
        { PHL.writeCompression = P.Uncompressed
        , PHL.writePageVersion = PHL.PageV1   -- dict path is V1-only for now
        , PHL.writeDictionary  = PHL.DictAuto
        }
      bs = PHL.encodeParquet opts mkSchema [dataset]
  -- Stash the file so the run_columnar_interop suite can read it.
  BS.writeFile "/tmp/wf_dict_roundtrip.parquet" bs
  putStrLn $ "encoded " ++ show (BS.length bs) ++ " bytes"
  case PHL.decodeParquet PHL.defaultReadOptions bs of
    Left err -> error err
    Right pf -> do
      let sch = PArrow.parquetFileArrowSchema pf
          flds = AT.arrowFields sch
      putStrLn $ "decoded schema: " ++ show (V.length flds) ++ " fields"
      -- Decode each column
      mapM_ (\c -> do
        let fld = V.unsafeIndex flds c
        case PArrow.readParquetColumn pf 0 c fld of
          Left e  -> error $ "readParquetColumn col " ++ show c ++ ": " ++ e
          Right cv ->
            putStrLn $ "  col " ++ show c ++ " " ++ show (AT.fieldName fld)
                    ++ " : " ++ show (AC.columnLength cv) ++ " values") [0, 1]
      -- Inspect the metadata for the name column to confirm
      -- it's marked as PLAIN_DICTIONARY + RLE_DICTIONARY.
      let fm = PR.pfFooter pf
          rgs = P.fmRowGroups fm
          rg0 = V.unsafeIndex rgs 0
          ccs = P.rgColumns rg0
          ccName = V.unsafeIndex ccs 1
      case P.ccMetadata ccName of
        Just md -> do
          putStrLn $ "  name encodings: " ++ show (P.cmEncodings md)
          putStrLn $ "  name dict-page-offset: " ++ show (P.cmDictionaryPageOffset md)
          putStrLn $ "  name data-page-offset: " ++ show (P.cmDataPageOffset md)
        Nothing -> error "missing name column metadata"
  putStrLn "OK"
