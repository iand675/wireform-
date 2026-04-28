{-# LANGUAGE BangPatterns #-}
-- | gRPC message framing for HTTP\/2.
--
-- The gRPC wire format wraps each serialized protobuf message in a 5-byte
-- header: 1 byte compression flag + 4 bytes big-endian message length.
-- This module provides framing and unframing for both single and streaming
-- messages.
module Proto.GRPC
  ( grpcFrame
  , grpcUnframe
  , grpcFrameMany
  , grpcUnframeMany
    -- * Compressed frames
  , grpcFrameCompressed
  , grpcUnframeWith
  , grpcUnframeManyWith
  , GrpcDecompress
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word32)

-- | A decompressor for gRPC compressed frames. The single argument is
-- the compressed payload (after the 5-byte header). 'Left' values
-- propagate as decode errors. Pass a function specific to the
-- @grpc-encoding@ negotiated with the peer (typically @gzip@ for the
-- wire defaults).
type GrpcDecompress = ByteString -> Either String ByteString

-- | Wrap a serialized message in a gRPC frame (compression=0).
grpcFrame :: ByteString -> ByteString
grpcFrame !msg = BL.toStrict $ B.toLazyByteString $
  B.word8 0x00 <> putBE32 (fromIntegral (BS.length msg)) <> B.byteString msg
{-# INLINE grpcFrame #-}

-- | Wrap an /already-compressed/ payload in a gRPC frame
-- (compression=1). Callers run the compression themselves and pass the
-- compressed bytes; this matches the spec exactly because the framing
-- layer has no knowledge of which codec the peer negotiated.
grpcFrameCompressed :: ByteString -> ByteString
grpcFrameCompressed !msg = BL.toStrict $ B.toLazyByteString $
  B.word8 0x01 <> putBE32 (fromIntegral (BS.length msg)) <> B.byteString msg
{-# INLINE grpcFrameCompressed #-}

-- | Extract the payload from a single gRPC frame, rejecting any frame
-- with the compression flag set. Use 'grpcUnframeWith' to handle
-- compressed frames.
grpcUnframe :: ByteString -> Either String ByteString
grpcUnframe = grpcUnframeWith
  (\_ -> Left "grpcUnframe: compressed frame received but no codec configured")

-- | Like 'grpcUnframe' but invokes @decomp@ on the payload of every
-- frame whose compression flag is set.
grpcUnframeWith :: GrpcDecompress -> ByteString -> Either String ByteString
grpcUnframeWith decomp !bs
  | BS.length bs < 5 =
      Left "grpcUnframe: insufficient data for frame header"
  | otherwise =
      let !compFlag = BS.index bs 0
          !len = decodeBE32 bs 1
          !lenI = fromIntegral len :: Int
          !totalLen = 5 + lenI
      in if compFlag > 1
         then Left $ "grpcUnframe: invalid compression flag: " ++ show compFlag
         else if lenI < 0
         then Left "grpcUnframe: declared length overflows Int"
         else if BS.length bs < totalLen
         then Left "grpcUnframe: payload shorter than declared length"
         else if BS.length bs > totalLen
         then Left "grpcUnframe: trailing data after frame"
         else
           let !payload = BS.take lenI (BS.drop 5 bs)
           in if compFlag == 1
                then decomp payload
                else Right payload

-- | Frame multiple messages (for streaming).
grpcFrameMany :: [ByteString] -> ByteString
grpcFrameMany !msgs = BL.toStrict $ B.toLazyByteString $ mconcat
  [ B.word8 0x00 <> putBE32 (fromIntegral (BS.length m)) <> B.byteString m
  | m <- msgs
  ]

-- | Extract multiple messages from a concatenated gRPC frame stream,
-- rejecting any frame with the compression flag set. Use
-- 'grpcUnframeManyWith' to handle compressed frames.
grpcUnframeMany :: ByteString -> Either String [ByteString]
grpcUnframeMany = grpcUnframeManyWith
  (\_ -> Left "grpcUnframeMany: compressed frame received but no codec configured")

-- | Like 'grpcUnframeMany' but invokes @decomp@ on the payload of every
-- frame whose compression flag is set.
grpcUnframeManyWith :: GrpcDecompress -> ByteString -> Either String [ByteString]
grpcUnframeManyWith decomp !bs = go 0 []
  where
    !bsLen = BS.length bs
    go !off !acc
      | off >= bsLen = Right (reverse acc)
      | off + 5 > bsLen =
          Left "grpcUnframeMany: truncated frame header"
      | otherwise =
          let !compFlag = BS.index bs off
              !len = decodeBE32 bs (off + 1)
              !lenI = fromIntegral len :: Int
              !payloadStart = off + 5
              !payloadEnd = payloadStart + lenI
          in if compFlag > 1
             then Left $ "grpcUnframeMany: invalid compression flag: " ++ show compFlag
             else if lenI < 0
             then Left "grpcUnframeMany: declared length overflows Int"
             else if payloadEnd > bsLen
             then Left "grpcUnframeMany: payload shorter than declared length"
             else
               let !payload = BS.take lenI (BS.drop payloadStart bs)
               in if compFlag == 1
                    then case decomp payload of
                      Left e -> Left e
                      Right decoded -> go payloadEnd (decoded : acc)
                    else go payloadEnd (payload : acc)

--------------------------------------------------------------------------------
-- Internal helpers
--------------------------------------------------------------------------------

putBE32 :: Word32 -> B.Builder
putBE32 !w =
  B.word8 (fromIntegral (w `shiftR` 24)) <>
  B.word8 (fromIntegral ((w `shiftR` 16) .&. 0xFF)) <>
  B.word8 (fromIntegral ((w `shiftR` 8) .&. 0xFF)) <>
  B.word8 (fromIntegral (w .&. 0xFF))
{-# INLINE putBE32 #-}

decodeBE32 :: ByteString -> Int -> Word32
decodeBE32 !bs !off =
  let !b0 = fromIntegral (BS.index bs off) :: Word32
      !b1 = fromIntegral (BS.index bs (off + 1)) :: Word32
      !b2 = fromIntegral (BS.index bs (off + 2)) :: Word32
      !b3 = fromIntegral (BS.index bs (off + 3)) :: Word32
  in (b0 `shiftL` 24) .|. (b1 `shiftL` 16) .|. (b2 `shiftL` 8) .|. b3
{-# INLINE decodeBE32 #-}
