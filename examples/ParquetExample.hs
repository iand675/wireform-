{- | This format uses schema-driven codegen. For real usage:
wireform-gen parquet -i schema.thrift -o src/Gen/
Then use the generated types directly.

Example: create Parquet FileMetadata, write as footer, and read back.

Run with: cabal run example-parquet
-}
module Main where

import Data.ByteString qualified as BS
import Data.Vector qualified as V
import Parquet.Footer qualified as PF
import Parquet.Types qualified as P


main :: IO ()
main = do
  let schema =
        V.fromList
          [ P.SchemaElement "schema" Nothing Nothing (Just 2) Nothing Nothing Nothing Nothing
          , P.SchemaElement "id" (Just P.Required) (Just P.PTInt64) Nothing Nothing Nothing Nothing Nothing
          , P.SchemaElement "name" (Just P.Optional) (Just P.PTByteArray) Nothing Nothing Nothing Nothing Nothing
          ]

  let metadata =
        P.FileMetadata
          { P.fmVersion = 2
          , P.fmSchema = schema
          , P.fmNumRows = 1000
          , P.fmRowGroups = V.empty
          , P.fmCreatedBy = Just "wireform"
          , P.fmKeyValueMetadata = Nothing
          , P.fmColumnOrders = Nothing
          }

  let footer = PF.writeFooter metadata
  putStrLn $ "Parquet footer: " ++ show (BS.length footer) ++ " bytes"

  case PF.readFooter footer of
    Right fm' ->
      putStrLn $
        "Read back: version="
          ++ show (P.fmVersion fm')
          ++ ", rows="
          ++ show (P.fmNumRows fm')
          ++ ", schema elements="
          ++ show (V.length (P.fmSchema fm'))
    Left err -> putStrLn $ "Error: " ++ err
