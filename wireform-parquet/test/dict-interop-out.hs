module Main where

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP

import qualified Parquet.HighLevel as PHL
import qualified Parquet.Types as P
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
        , PHL.writePageVersion = PHL.PageV1
        , PHL.writeDictionary  = PHL.DictAuto
        }
      bs = PHL.encodeParquet opts mkSchema [dataset]
  BS.writeFile "/tmp/wf_dict.parquet" bs
  putStrLn $ "wrote " ++ show (BS.length bs) ++ " bytes"
