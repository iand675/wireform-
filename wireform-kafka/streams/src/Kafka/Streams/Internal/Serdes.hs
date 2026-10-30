{- |
Module      : Kafka.Streams.Internal.Serdes
Description : Serdes for the DSL's internal state (changelogs, internal topics)

The DSL keeps some state in shapes user code never names (a debounce
deadline next to a value, a subscription map, a hash token). Their
changelogs still need bytes, so they get compact, self-delimiting
encodings here. They are internal: both ends are always this library.
-}
module Kafka.Streams.Internal.Serdes (
  int64BE,
  readInt64BE,
  lengthPrefixed,
  splitLengthPrefixed,
  mapSerde,
  listSerde,
  eitherSerde,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BSL
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Kafka.Streams.Serde (Serde (..))


-- | 8-byte big-endian encoding.
int64BE :: Int64 -> ByteString
int64BE = BSL.toStrict . BB.toLazyByteString . BB.int64BE


-- | Inverse of 'int64BE' on the first 8 bytes; 'Nothing' if shorter.
readInt64BE :: ByteString -> Maybe Int64
readInt64BE bs
  | BS.length bs < 8 = Nothing
  | otherwise = Just (BS.foldl' (\acc w -> acc * 256 + fromIntegral w) 0 (BS.take 8 bs))


-- | A 4-byte big-endian length followed by the bytes.
lengthPrefixed :: ByteString -> BB.Builder
lengthPrefixed b = BB.int32BE (fromIntegral (BS.length b)) <> BB.byteString b


-- | Split one 'lengthPrefixed' chunk off the front.
splitLengthPrefixed :: ByteString -> Either Text (ByteString, ByteString)
splitLengthPrefixed bs
  | BS.length bs < 4 = Left "truncated length prefix"
  | otherwise =
      let !n = BS.foldl' (\acc w -> acc * 256 + fromIntegral w) (0 :: Int) (BS.take 4 bs)
          !rest = BS.drop 4 bs
      in if BS.length rest < n
           then Left "truncated length-prefixed chunk"
           else Right (BS.splitAt n rest)


-- | A map as a sequence of length-prefixed key / value pairs.
mapSerde :: Ord k => Serde k -> Serde v -> Serde (Map k v)
mapSerde ks vs =
  Serde
    { serialize = \m ->
        BSL.toStrict
          ( BB.toLazyByteString
              ( Map.foldrWithKey
                  (\k v acc -> lengthPrefixed (serialize ks k) <> lengthPrefixed (serialize vs v) <> acc)
                  mempty
                  m
              )
          )
    , deserialize = go Map.empty
    , serializeHeaders = const mempty
    }
  where
    go !acc bs
      | BS.null bs = Right acc
      | otherwise = do
          (kb, r1) <- splitLengthPrefixed bs
          (vb, r2) <- splitLengthPrefixed r1
          k <- deserialize ks kb
          v <- deserialize vs vb
          go (Map.insert k v acc) r2


-- | A list as a sequence of length-prefixed elements.
listSerde :: Serde v -> Serde [v]
listSerde vs =
  Serde
    { serialize = \xs -> BSL.toStrict (BB.toLazyByteString (foldMap (lengthPrefixed . serialize vs) xs))
    , deserialize = go []
    , serializeHeaders = const mempty
    }
  where
    go acc bs
      | BS.null bs = Right (reverse acc)
      | otherwise = do
          (vb, rest) <- splitLengthPrefixed bs
          v <- deserialize vs vb
          go (v : acc) rest


-- | A tag byte (0: 'Left', 1: 'Right') before the value.
eitherSerde :: Serde a -> Serde b -> Serde (Either a b)
eitherSerde as bs =
  Serde
    { serialize = \case
        Left a -> BS.cons 0 (serialize as a)
        Right b -> BS.cons 1 (serialize bs b)
    , deserialize = \x -> case BS.uncons x of
        Just (0, rest) -> Left <$> deserialize as rest
        Just (1, rest) -> Right <$> deserialize bs rest
        _ -> Left "either: bad tag"
    , serializeHeaders = const mempty
    }
