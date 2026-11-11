{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Equality for the raw protobuf records preserved by CEL message values.
module CEL.Internal.UnknownFields (equalUnknownFields) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IntMap.Strict qualified as IntMap
import GHC.Exts (Int (I#))
import Proto.Decode (decodeFail)
import Proto.Internal.Wire.Decode qualified as Wire


-- | Compare the original wire records for each unknown field number.
-- Interleaving records with other field numbers has no effect.
equalUnknownFields :: ByteString -> ByteString -> Bool
equalUnknownFields left right
  | BS.length left /= BS.length right = False
  | left == right = True
  | otherwise = case (groupUnknownFields left, groupUnknownFields right) of
      (Right x, Right y) -> x == y
      _ -> False


groupUnknownFields :: ByteString -> Either Wire.DecodeError (IntMap.IntMap [ByteString])
groupUnknownFields bytes = Wire.runDecoder (go 0 IntMap.empty) bytes
  where
    go start fields = Wire.withTagM (pure fields) $ \number wireType -> do
      skipField number wireType
      end <- Wire.Decoder (\_ offset -> (# (# I# offset, offset #) | #))
      let record = BS.take (end - start) (BS.drop start bytes)
          -- Both sides accumulate occurrences in reverse wire order.
          updated = IntMap.insertWith (<>) number [record] fields
      go end updated


skipField :: Int -> Int -> Wire.Decoder ()
skipField number 3 = skipGroup number []
skipField _ 2 = do
  size <- Wire.getVarint
  -- Check the remaining input before narrowing the unsigned wire length.
  Wire.Decoder $ \bytes offset ->
    if size > fromIntegral (BS.length bytes - I# offset)
      then (# | Wire.UnexpectedEnd #)
      else case I# offset + fromIntegral size of
        I# end -> (# (# (), end #) | #)
skipField _ wireType = Wire.skipWireType wireType


skipGroup :: Int -> [Int] -> Wire.Decoder ()
skipGroup expected parents = Wire.withTagM (decodeFail Wire.UnexpectedEnd) $ \number wireType ->
  case wireType of
    3 -> skipGroup number (expected : parents)
    4
      | number /= expected -> decodeFail (Wire.InvalidWireType wireType)
      | parent : rest <- parents -> skipGroup parent rest
      | otherwise -> pure ()
    _ -> skipField number wireType >> skipGroup expected parents
