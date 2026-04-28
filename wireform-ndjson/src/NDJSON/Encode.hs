-- | NDJSON (Newline-Delimited JSON) encoder.
module NDJSON.Encode
  ( encode
  , encodeRecords
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Vector (Vector)
import qualified Data.Vector as V

import qualified Data.Aeson as Aeson

encode :: Vector Aeson.Value -> ByteString
encode vals = BL.toStrict $ B.toLazyByteString $ buildNDJSON vals

encodeRecords :: Aeson.ToJSON a => Vector a -> ByteString
encodeRecords = encode . V.map Aeson.toJSON

-- | Build NDJSON output. Per the NDJSON spec
-- (<https://github.com/ndjson/ndjson-spec>), each record is followed by
-- a single LF (0x0A); the last record's trailing newline is REQUIRED so
-- consumers can append more records by simple byte concatenation.
buildNDJSON :: Vector Aeson.Value -> B.Builder
buildNDJSON =
  V.foldl' (\acc val -> acc <> B.lazyByteString (Aeson.encode val) <> B.word8 0x0A) mempty
