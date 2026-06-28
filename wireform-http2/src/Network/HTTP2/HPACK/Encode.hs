module Network.HTTP2.HPACK.Encode
  ( encodeHeaderBlock
  , encodeHeader
  ) where

import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word
import Wireform.Builder

import Network.HTTP2.HPACK.Huffman
import Network.HTTP2.HPACK.Table
import Network.HTTP2.HPACK.Types
import Network.HTTP2.Types (Header)


encodeHeaderBlock :: EncodeStrategy -> DynamicTable -> [Header] -> IO ByteString
encodeHeaderBlock strategy dt headers = do
  builders <- traverse (encodeHeader strategy dt) headers
  let builder = mconcat builders
  pure (toStrictByteString builder)

encodeHeader :: EncodeStrategy -> DynamicTable -> Header -> IO Builder
encodeHeader strategy dt hdr@(name, value) = do
  match <- lookupNameValue dt hdr
  case match of
    Just (idx, True) ->
      pure (encodeIndexed idx)
    Just (idx, False)
      | useDynamicTable strategy -> do
          insertEntry dt hdr
          pure (encodeLiteralIncremental (Just idx) name value (useHuffman strategy))
      | otherwise ->
          pure (encodeLiteralNoIndex (Just idx) name value (useHuffman strategy))
    Nothing
      | useDynamicTable strategy -> do
          insertEntry dt hdr
          pure (encodeLiteralIncremental Nothing name value (useHuffman strategy))
      | otherwise ->
          pure (encodeLiteralNoIndex Nothing name value (useHuffman strategy))

encodeIndexed :: Int -> Builder
encodeIndexed idx = encodeInteger 7 0x80 (fromIntegral idx)

encodeLiteralIncremental :: Maybe Int -> ByteString -> ByteString -> Bool -> Builder
encodeLiteralIncremental midx name value huff =
  case midx of
    Just idx -> encodeInteger 6 0x40 (fromIntegral idx)
             <> encodeStringLiteral huff value
    Nothing -> word8 0x40
            <> encodeStringLiteral huff name
            <> encodeStringLiteral huff value

encodeLiteralNoIndex :: Maybe Int -> ByteString -> ByteString -> Bool -> Builder
encodeLiteralNoIndex midx name value huff =
  case midx of
    Just idx -> encodeInteger 4 0x00 (fromIntegral idx)
             <> encodeStringLiteral huff value
    Nothing -> word8 0x00
            <> encodeStringLiteral huff name
            <> encodeStringLiteral huff value

encodeStringLiteral :: Bool -> ByteString -> Builder
encodeStringLiteral False bs =
  encodeInteger 7 0x00 (fromIntegral (BS.length bs)) <> byteString bs
encodeStringLiteral True bs =
  let encoded = huffmanEncode bs
      encodedLen = BS.length encoded
  in encodeInteger 7 0x80 (fromIntegral encodedLen) <> byteString encoded

encodeInteger :: Int -> Word8 -> Word64 -> Builder
encodeInteger n prefix value
  | value < mask = word8 (prefix .|. fromIntegral value)
  | otherwise =
      word8 (prefix .|. fromIntegral mask)
      <> encodeContinuation (value - fromIntegral mask)
  where
    mask :: Word64
    mask = (1 `unsafeShiftL` n) - 1

encodeContinuation :: Word64 -> Builder
encodeContinuation value
  | value < 128 = word8 (fromIntegral value)
  | otherwise =
      word8 (fromIntegral (value .&. 0x7F) .|. 0x80)
      <> encodeContinuation (value `unsafeShiftR` 7)
