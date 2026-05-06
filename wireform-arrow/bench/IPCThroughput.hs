{-# LANGUAGE BangPatterns #-}
-- | Arrow IPC stream write/read throughput baseline.
module Main (main) where

import Control.DeepSeq (NFData (..))
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T

import Criterion.Main (defaultMain, bench, bgroup, env, nf, whnf)

import qualified Arrow.Types as AT
import qualified Arrow.Column as AC
import qualified Arrow.Stream as AS

nRows :: Int
nRows = 100_000

mkField :: T.Text -> AT.ArrowType -> AT.Field
mkField name ty = AT.Field
  { AT.fieldName       = name
  , AT.fieldNullable   = False
  , AT.fieldType       = ty
  , AT.fieldChildren   = V.empty
  , AT.fieldDictionary = Nothing
  , AT.fieldMetadata   = V.empty
  }

mkSchema :: AT.Schema
mkSchema = AT.Schema
  { AT.arrowFields = V.fromList
      [ mkField "id"     (AT.AInt 64 True)
      , mkField "score"  (AT.AFloatingPoint AT.DoublePrecision)
      , mkField "name"   AT.AUtf8
      , mkField "active" AT.ABool
      ]
  , AT.arrowEndianness = AT.Little
  , AT.arrowFeatures   = V.empty
  , AT.arrowMetadata   = V.empty
  }

mkBatch :: Int -> V.Vector AC.ColumnArray
mkBatch n = V.fromList
  [ AC.ColInt64  (VP.generate n fromIntegral)
  , AC.ColDouble (VP.generate n (\i -> fromIntegral i * 0.5))
  , AC.ColUtf8   (V.generate n (\i -> T.pack ("name_" ++ show (i `rem` 1000))))
  , AC.ColBool   (V.generate n (\i -> i `rem` 2 == 0))
  ]

writeStream :: V.Vector AC.ColumnArray -> BS.ByteString
writeStream batch =
  AS.encodeArrowStream AS.defaultWriteOptions mkSchema [batch]

readStream :: BS.ByteString -> Int
readStream bs = case AS.decodeArrowStream bs of
  Left _ -> 0
  Right (_, batches) ->
    sum [ V.foldl' (\a c -> a + AC.columnLength c) 0 b | b <- batches ]

instance NFData AC.ColumnArray where
  rnf c = c `seq` ()

main :: IO ()
main =
  let !batch = mkBatch nRows
  in defaultMain
       [ env (pure batch) $ \b ->
           bgroup ("write 100k x 4 cols (Arrow IPC stream)")
             [ bench "encode" $ whnf (BS.length . writeStream) b
             ]
       , env (pure (writeStream batch)) $ \bs ->
           bgroup ("read 100k x 4 cols (Arrow IPC stream)")
             [ bench "decode" $ nf readStream bs
             ]
       ]
