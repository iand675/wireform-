module Main (main) where

import Control.Monad (forM_)
import Criterion.Main
import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.Vector qualified as V
import Data.Vector.Primitive qualified as VP
import Parquet.Encryption qualified as E
import Parquet.HighLevel qualified as H
import Parquet.Types
import Parquet.Write qualified as W
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath ((</>))


data Workload = Workload
  { workloadName :: !String
  , workloadSize :: {-# UNPACK #-} !Int
  , workloadAction :: !(H.FooterMode -> IO BS.ByteString)
  }


main :: IO ()
main = do
  let workloads = concatMap (\n -> [mixed n, nested n]) [64, 8192]
      modes = [H.EncryptedFooter, H.SignedPlaintextFooter]
  -- Materialize every input before Criterion starts: NestedRow elements are
  -- intentionally shared across iterations, just like the Rust Arrow arrays.
  forM_ workloads $ \work -> workloadAction work H.EncryptedFooter >>= \bytes -> BS.length bytes `seq` pure ()
  getArgs >>= \case
    ["--write-fixtures", directory] -> do
      createDirectoryIfMissing True directory
      forM_ workloads $ \work -> forM_ modes $ \mode -> do
        bytes <- workloadAction work mode
        BS.writeFile (directory </> (workloadName work ++ "-" ++ show (workloadSize work) ++ "-" ++ modeName mode ++ ".parquet")) bytes
    _ -> defaultMain $ map (\work -> bgroup (workloadName work ++ "/" ++ show (workloadSize work)) (map (\mode -> bench (modeName mode) (nfIO (workloadAction work mode))) modes)) workloads


modeName :: H.FooterMode -> String
modeName H.EncryptedFooter = "encrypted"
modeName H.SignedPlaintextFooter = "signed"


options :: H.FooterMode -> IO H.WriteOptions
options mode = do
  footer <- W.newFooterEncryption (BS.replicate 16 0x42) "writer-regression" "kf"
  let secret = W.ColumnEncryption E.AesGcmV1 (BS.replicate 16 0x44) (W.feFileId footer) "writer-regression" "kc2" 1
  pure
    H.defaultWriteOptions
      { H.writeCompression = Uncompressed
      , H.writePageVersion = W.PageV2
      , H.writePageIndex = True
      , H.writeFooterMode = mode
      , H.writeFooterEncryption = Just footer
      , H.writeColumnEncryption = V.fromList [Nothing, Just secret]
      }


mixed :: Int -> Workload
mixed n =
  let schema =
        V.fromList
          [ SchemaElement "schema" Nothing Nothing (Just 2) Nothing Nothing Nothing Nothing
          , SchemaElement "public" (Just Required) (Just PTInt64) Nothing Nothing Nothing Nothing Nothing
          , SchemaElement "secret" (Just Optional) (Just PTInt64) Nothing Nothing Nothing Nothing Nothing
          ]
      rows = [V.fromList [W.PCRequired (W.ColInt64 (VP.generate n fromIntegral)), W.PCOptional (W.OptInt64 (V.generate n (\i -> if i `rem` 5 == 0 then Nothing else Just (fromIntegral i * 3))))]]
  in Workload "mixed" n (\mode -> options mode >>= \opts -> H.encodeParquetMixedIO opts schema rows >>= either fail pure)


nested :: Int -> Workload
nested n =
  let schema =
        V.fromList
          [ ("public", H.NSRequired (H.NSPrimitive H.LtInt64))
          , ("secret", H.NSOptional (H.NSList (H.NSOptional (H.NSPrimitive H.LtInt64))))
          ]
      rows =
        V.fromList
          [ V.generate n (H.NRLeaf . H.LvInt64 . fromIntegral)
          , V.generate n $ \i ->
              if i `rem` 11 == 0
                then H.NRNull
                else
                  if i `rem` 7 == 0
                    then H.NRList V.empty
                    else H.NRList (V.generate 3 (\j -> if j == 0 && i `rem` 5 == 0 then H.NRNull else H.NRLeaf (H.LvInt64 (fromIntegral i * 3 + fromIntegral j :: Int64))))
          ]
  in Workload "nested" n (\mode -> options mode >>= \opts -> H.encodeParquetNestedIO opts schema rows >>= either fail pure)
