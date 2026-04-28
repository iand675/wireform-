{-# LANGUAGE BangPatterns #-}

-- | ORC column and file encoding.
--
-- Provides encoders for primitive column types (integer, boolean, float,
-- double, string) and a minimal ORC file builder that concatenates streams
-- into stripes with a proper footer + postscript.
module ORC.Write
  ( -- * Stream encoders
    encodeRLEv2Direct
  , encodeBooleanRLE
  , encodeIntColumn
  , encodeStringDirectColumn
  , encodeFloatColumn
  , encodeDoubleColumn
    -- * File assembly
  , buildStripe
  , buildORCFile
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import Data.Word (Word64, Word8)
import GHC.Float (castDoubleToWord64, castFloatToWord32)

import ORC.Footer (orcMagic, writeORCFooter)
import ORC.RLE
  ( bitWidth
  , closestWidth
  , encodeByteRLE
  , encodeWidth
  , packBitsMSB
  , putVulong
  , zigzagEncode
  )
import ORC.Stripe (encodeStripeFooter, encodeStream, Stream (..), StripeFooter (..))
import ORC.Types

------------------------------------------------------------------------
-- RLE v2 Direct encoding
------------------------------------------------------------------------

-- | Encode values as RLE v2 Direct.
--
-- Handles up to 512 values per run. For longer inputs, emits multiple
-- Direct runs.
encodeRLEv2Direct :: VP.Vector Int64 -> Bool -> ByteString
encodeRLEv2Direct vals signed =
  BL.toStrict $ B.toLazyByteString $ goChunks 0
  where
    !n = VP.length vals
    goChunks :: Int -> B.Builder
    goChunks !off
      | off >= n = mempty
      | otherwise =
          let !remaining = n - off
              !chunkLen = min 512 remaining
              !chunk = VP.slice off chunkLen vals
          in encodeDirectChunk chunk signed <> goChunks (off + chunkLen)

encodeDirectChunk :: VP.Vector Int64 -> Bool -> B.Builder
encodeDirectChunk vals signed =
  let !n = VP.length vals
      !transformed = VP.generate n $ \i ->
        let !v = VP.unsafeIndex vals i
        in if signed then zigzagEncode v else fromIntegral v :: Word64
      !maxVal = VP.foldl' max 0 transformed
      !rawW = bitWidth maxVal
      !w = closestWidth rawW
      !encodedW = encodeWidth w
      -- Header: [01][encodedW 5 bits][lenHigh 1 bit]
      !len1 = n - 1
      !lenHigh = (len1 `shiftR` 8) .&. 1
      !lenLow  = len1 .&. 0xFF
      !byte0 = (1 `shiftL` 6) .|. (encodedW `shiftL` 1) .|. lenHigh
      !byte1 = lenLow
      !packed = packBitsMSB transformed w
  in B.word8 (fromIntegral byte0)
     <> B.word8 (fromIntegral byte1)
     <> B.byteString packed

------------------------------------------------------------------------
-- Boolean stream encoding
------------------------------------------------------------------------

-- | Encode a boolean vector as ORC boolean stream (byte-RLE of bit-packed bytes).
encodeBooleanRLE :: V.Vector Bool -> ByteString
encodeBooleanRLE vals =
  let !n = V.length vals
      !numBytes = (n + 7) `quot` 8
      !bytes = VP.generate numBytes $ \bi ->
        let buildByte !bit !acc
              | bit >= 8 = acc
              | otherwise =
                  let !idx = bi * 8 + bit
                      !bitVal = if idx < n && V.unsafeIndex vals idx then 1 else 0 :: Word8
                      !acc' = acc .|. (bitVal `shiftL` (7 - bit))
                  in buildByte (bit + 1) acc'
        in buildByte 0 0
  in encodeByteRLE bytes

------------------------------------------------------------------------
-- Column encoders
------------------------------------------------------------------------

-- | Encode an integer column's DATA stream using RLE v2 Direct.
encodeIntColumn :: VP.Vector Int64 -> Bool -> ByteString
encodeIntColumn = encodeRLEv2Direct

-- | Encode a string column with DIRECT encoding.
-- Returns (DATA stream, LENGTH stream).
encodeStringDirectColumn :: V.Vector T.Text -> (ByteString, ByteString)
encodeStringDirectColumn texts =
  let !encodedTexts = V.map TE.encodeUtf8 texts
      !dataBs = BS.concat (V.toList encodedTexts)
      !lengths = VP.generate (V.length texts) $ \i ->
        fromIntegral (BS.length (V.unsafeIndex encodedTexts i)) :: Int64
      !lengthBs = encodeRLEv2Direct lengths False
  in (dataBs, lengthBs)

-- | Encode a float column (IEEE 754 single, little-endian).
encodeFloatColumn :: VP.Vector Float -> ByteString
encodeFloatColumn vals =
  BL.toStrict $ B.toLazyByteString $ VP.foldl' (\acc v -> acc <> writeFloatLE v) mempty vals

-- | Encode a double column (IEEE 754 double, little-endian).
encodeDoubleColumn :: VP.Vector Double -> ByteString
encodeDoubleColumn vals =
  BL.toStrict $ B.toLazyByteString $ VP.foldl' (\acc v -> acc <> writeDoubleLE v) mempty vals

{-# INLINE writeFloatLE #-}
writeFloatLE :: Float -> B.Builder
writeFloatLE !f =
  let !w = castFloatToWord32 f
  in B.word8 (fromIntegral (w .&. 0xFF))
     <> B.word8 (fromIntegral ((w `shiftR` 8) .&. 0xFF))
     <> B.word8 (fromIntegral ((w `shiftR` 16) .&. 0xFF))
     <> B.word8 (fromIntegral ((w `shiftR` 24) .&. 0xFF))

{-# INLINE writeDoubleLE #-}
writeDoubleLE :: Double -> B.Builder
writeDoubleLE !d =
  let !w = castDoubleToWord64 d
  in B.word8 (fromIntegral (w .&. 0xFF))
     <> B.word8 (fromIntegral ((w `shiftR` 8) .&. 0xFF))
     <> B.word8 (fromIntegral ((w `shiftR` 16) .&. 0xFF))
     <> B.word8 (fromIntegral ((w `shiftR` 24) .&. 0xFF))
     <> B.word8 (fromIntegral ((w `shiftR` 32) .&. 0xFF))
     <> B.word8 (fromIntegral ((w `shiftR` 40) .&. 0xFF))
     <> B.word8 (fromIntegral ((w `shiftR` 48) .&. 0xFF))
     <> B.word8 (fromIntegral ((w `shiftR` 56) .&. 0xFF))

------------------------------------------------------------------------
-- File assembly
------------------------------------------------------------------------

-- | Build a stripe from stream payloads and stream metadata.
--
-- Concatenates the DATA streams and appends a protobuf stripe footer.
buildStripe :: V.Vector (Word64, Word64, ByteString) -> ByteString
buildStripe streamInfos =
  let !streams = V.map (\(kind, col, bs) ->
        Stream { stKind = kind, stColumn = col, stLength = fromIntegral (BS.length bs) }) streamInfos
      !footer = StripeFooter streams V.empty
      !footerBs = encodeStripeFooter footer
      !dataParts = V.toList (V.map (\(_, _, bs) -> bs) streamInfos)
  in BS.concat (dataParts ++ [footerBs])

-- | Build a complete ORC file from type info and stripe data.
--
-- @types@: column types for the schema
-- @stripeData@: for each stripe, a vector of (streamKind, columnId, payload)
buildORCFile :: V.Vector ORCType -> V.Vector (V.Vector (Word64, Word64, ByteString)) -> ByteString
buildORCFile types stripeData =
  let !headerMagic = orcMagic
      !headerLen   = fromIntegral (BS.length headerMagic) :: Word64

      buildStripes :: Int -> Word64 -> V.Vector StripeInformation -> [ByteString]
                   -> (V.Vector StripeInformation, [ByteString])
      buildStripes !i !off !siAcc !bsAcc
        | i >= V.length stripeData = (siAcc, reverse bsAcc)
        | otherwise =
            let !sdata = V.unsafeIndex stripeData i
                !streams = V.map (\(kind, col, bs) ->
                  Stream { stKind = kind, stColumn = col, stLength = fromIntegral (BS.length bs) }) sdata
                !footer = StripeFooter streams V.empty
                !footerBs = encodeStripeFooter footer
                !dataLen = V.foldl' (\a (_, _, bs) -> a + fromIntegral (BS.length bs)) 0 sdata :: Word64
                !ftrLen = fromIntegral (BS.length footerBs) :: Word64
                !nRows = 0 -- caller should set proper row counts
                !si = StripeInformation
                  { siOffset = off
                  , siIndexLength = 0
                  , siDataLength = dataLen
                  , siFooterLength = ftrLen
                  , siNumberOfRows = nRows
                  }
                !stripeBs = BS.concat (V.toList (V.map (\(_, _, bs) -> bs) sdata) ++ [footerBs])
                !stripeLen = fromIntegral (BS.length stripeBs) :: Word64
            in buildStripes (i + 1) (off + stripeLen) (V.snoc siAcc si) (stripeBs : bsAcc)

      (!stripeInfos, !stripeBss) = buildStripes 0 headerLen V.empty []
      !contentLen = V.foldl' (\a si -> a + siIndexLength si + siDataLength si + siFooterLength si) 0 stripeInfos

      !footer = ORCFooter
        { orcHeaderLength  = headerLen
        , orcContentLength = contentLen
        , orcStripes       = stripeInfos
        , orcTypes         = types
        , orcMetadata      = V.empty
        , orcNumberOfRows  = V.foldl' (\a si -> a + siNumberOfRows si) 0 stripeInfos
        , orcStatistics    = V.empty
        }
      !footerBytes = writeORCFooter footer
  in BS.concat ([headerMagic] ++ stripeBss ++ [footerBytes])
