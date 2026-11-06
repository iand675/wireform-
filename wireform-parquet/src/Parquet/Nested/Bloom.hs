{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Bloom construction from the present values of a shredded PLAIN leaf.
module Parquet.Nested.Bloom (buildBloomFilter) where

import Control.Monad.ST (ST)
import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Parquet.BloomFilter qualified as Bloom
import Parquet.Nested.Plain (PlainLayout (..), lengthAt, plainLayout, validPlainValues)
import Parquet.Types (SchemaElement)


-- Validate once before the ST insertion loop. BYTE_ARRAY lengths belong to
-- PLAIN framing and must not be included in the value's hash. BOOLEAN values
-- are hashed individually as 0/1 bytes, rather than as packed page bits.
buildBloomFilter :: SchemaElement -> Int -> ByteString -> Either String Bloom.Sbbf
buildBloomFilter field count bytes = do
  layout <- plainLayout field
  if not (validPlainValues layout count bytes)
    then Left "Parquet.Nested: bloom values disagree with the physical PLAIN layout"
    else pure $ case layout of
      BooleanValues -> build (insertBooleans count bytes)
      VariableValues -> build (insertByteArrays count bytes)
      FixedValues width -> build (insertFixed count width bytes)
  where
    build :: (forall s. Bloom.SbbfBuilder s -> ST s ()) -> Bloom.Sbbf
    build action = Bloom.buildSbbf (Bloom.optimalNumBytes (max 1 count) 0.01) action


insertFixed :: forall s. Int -> Int -> ByteString -> Bloom.SbbfBuilder s -> ST s ()
insertFixed count width bytes builder = go 0 0
  where
    go :: Int -> Int -> ST s ()
    go !index !offset
      | index == count = pure ()
      | otherwise = do
          Bloom.sbbfBuilderInsert builder (BS.take width (BS.drop offset bytes))
          go (index + 1) (offset + width)


insertByteArrays :: forall s. Int -> ByteString -> Bloom.SbbfBuilder s -> ST s ()
insertByteArrays count bytes builder = go 0 0
  where
    go :: Int -> Int -> ST s ()
    go !index !offset
      | index == count = pure ()
      | otherwise = do
          let !length_ = lengthAt bytes offset
          Bloom.sbbfBuilderInsert builder (BS.take length_ (BS.drop (offset + 4) bytes))
          go (index + 1) (offset + 4 + length_)


insertBooleans :: forall s. Int -> ByteString -> Bloom.SbbfBuilder s -> ST s ()
insertBooleans count bytes builder = go 0
  where
    go :: Int -> ST s ()
    go !index
      | index == count = pure ()
      | otherwise = do
          let !value = BSU.unsafeIndex bytes (index `quot` 8) `shiftR` (index `rem` 8) .&. 1
          Bloom.sbbfBuilderInsert builder (if value == 0 then falseByte else trueByte)
          go (index + 1)
    falseByte = BS.singleton 0
    trueByte = BS.singleton 1
