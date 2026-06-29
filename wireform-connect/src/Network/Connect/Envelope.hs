{-# LANGUAGE OverloadedStrings #-}

-- | Connect streaming envelope: the 5-byte frame prefix (1 flag byte +
-- 4-byte big-endian length) and the EndStreamResponse JSON trailer.
--
-- The Connect envelope is structurally the gRPC length-prefix but with a
-- different flag byte: bit 0 = compressed, bit 1 = end-stream (gRPC's
-- single-bit compressed flag cannot represent Connect semantics), so this is
-- reimplemented rather than bending grpc-spec's internal 'MessagePrefix'.
module Network.Connect.Envelope (
  -- * Frame flags
  EnvelopeFlags (..),
  flagCompressedBit,
  flagEndStreamBit,
  flagsToByte,
  flagsFromByte,

  -- * Frame construction
  buildFrame,
  buildFrameLazy,

  -- * Frame reading
  FrameReader,
  newFrameReader,
  readFrame,

  -- * EndStreamResponse
  EndStreamResponse (..),
  encodeEndStream,
  decodeEndStream,
  decodeEndStreamValue,
) where

import Control.Exception (throwIO)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Bits ((.&.), (.|.), shiftL)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word32, Word8)
import Network.Connect.Error
  ( ConnectError (..)
  , ConnectException (..)
  , decodeConnectError
  , encodeConnectError
  )
import Network.Connect.Metadata (metadataFromJSON, metadataToJSON)
import Network.GRPC.Spec (CustomMetadata, GrpcError (..))

------------------------------------------------------------------------
-- Frame flags
------------------------------------------------------------------------

flagCompressedBit :: Word8
flagCompressedBit = 0x01

flagEndStreamBit :: Word8
flagEndStreamBit = 0x02

-- | The two Connect envelope flag bits.
data EnvelopeFlags = EnvelopeFlags
  { efCompressed :: !Bool
  , efEndStream :: !Bool
  }
  deriving stock (Eq, Show)

flagsToByte :: EnvelopeFlags -> Word8
flagsToByte f =
  (if efCompressed f then flagCompressedBit else 0)
    .|. (if efEndStream f then flagEndStreamBit else 0)

flagsFromByte :: Word8 -> EnvelopeFlags
flagsFromByte b =
  EnvelopeFlags
    { efCompressed = b .&. flagCompressedBit /= 0
    , efEndStream = b .&. flagEndStreamBit /= 0
    }

------------------------------------------------------------------------
-- Frame construction
------------------------------------------------------------------------

-- | Build a single frame: 1 flag byte + 4-byte big-endian length + payload.
buildFrame :: EnvelopeFlags -> ByteString -> Builder
buildFrame flags payload =
  B.word8 (flagsToByte flags)
    <> B.word32BE (fromIntegral (BS.length payload))
    <> B.byteString payload

-- | Lazy 'BL.ByteString' version of 'buildFrame'.
buildFrameLazy :: EnvelopeFlags -> ByteString -> BL.ByteString
buildFrameLazy flags payload = B.toLazyByteString (buildFrame flags payload)

------------------------------------------------------------------------
-- Frame reading
------------------------------------------------------------------------

-- | A pull-based frame reader over a chunk producer. Each 'readFrame' call
-- yields the next decoded @(flags, payload)@, or 'Nothing' at a clean
-- end-of-stream. A truncated frame throws a 'ConnectException' with code
-- @internal@.
data FrameReader = FrameReader
  { frProduce :: !(IO (Maybe ByteString))
  , frBuffer :: !(IORef ByteString)
  }

-- | Construct a 'FrameReader' from a body chunk producer (@'Nothing' = EOF@).
newFrameReader :: IO (Maybe ByteString) -> IO FrameReader
newFrameReader produce = do
  buf <- newIORef BS.empty
  pure (FrameReader produce buf)

-- | Read the next frame, or 'Nothing' at clean EOF.
readFrame :: FrameReader -> IO (Maybe (EnvelopeFlags, ByteString))
readFrame (FrameReader produce bufRef) = go
  where
    go = do
      buf <- readIORef bufRef
      if BS.null buf
        then do
          mchunk <- produce
          case mchunk of
            Nothing -> pure Nothing
            Just chunk
              | BS.null chunk -> go
              | otherwise -> do
                  writeIORef bufRef chunk
                  go
        else tryDecode buf
    tryDecode buf
      | BS.length buf < 5 = fill buf
      | otherwise =
          let flagsByte = BS.index buf 0
              len = fromIntegral (readBE32 (BS.drop 1 buf)) :: Int
              totalNeeded = 5 + len
           in if BS.length buf >= totalNeeded
                then do
                  let payload = BS.take len (BS.drop 5 buf)
                      rest = BS.drop totalNeeded buf
                  writeIORef bufRef rest
                  pure (Just (flagsFromByte flagsByte, payload))
                else fill buf
    fill buf = do
      mchunk <- produce
      case mchunk of
        Nothing -> throwIO (ConnectException internalTruncatedError)
        Just chunk -> do
          writeIORef bufRef (buf <> chunk)
          go

internalTruncatedError :: ConnectError
internalTruncatedError =
  ConnectError
    { ceCode = GrpcInternal
    , ceMessage = Just "connect: truncated streaming frame"
    , ceDetails = []
    }

-- Read 4 bytes as a big-endian Word32.
readBE32 :: ByteString -> Word32
readBE32 bs =
  (fromIntegral (BS.index bs 0) `shiftL` 24)
    .|. (fromIntegral (BS.index bs 1) `shiftL` 16)
    .|. (fromIntegral (BS.index bs 2) `shiftL` 8)
    .|. fromIntegral (BS.index bs 3)

------------------------------------------------------------------------
-- EndStreamResponse
------------------------------------------------------------------------

-- | The final message of a streaming response: an optional error (which must
-- be present iff the RPC failed) and optional trailing metadata.
data EndStreamResponse = EndStreamResponse
  { esError :: !(Maybe ConnectError)
  , esMetadata :: ![CustomMetadata]
  }
  deriving stock (Eq, Show)

-- | Encode an 'EndStreamResponse' as the Connect JSON object (suitable for
-- the payload of the final frame). @{"error"?: …, "metadata"?: …}@.
encodeEndStream :: EndStreamResponse -> ByteString
encodeEndStream esr =
  BL.toStrict (Aeson.encode (Aeson.object (errorPair <> metadataPair)))
  where
    errorPair =
      case esError esr of
        Just err -> ["error" Aeson..= encodeConnectError err]
        Nothing -> []
    metadataPair =
      case esMetadata esr of
        [] -> []
        ms -> ["metadata" Aeson..= metadataToJSON ms]

-- | Parse a Connect EndStreamResponse JSON object. Rejects
-- @{"error": null}@, @{"error": {}}@, and @{"error": {"code": null}}@;
-- absent @error@ means success.
decodeEndStream :: ByteString -> Either String EndStreamResponse
decodeEndStream bs =
  case Aeson.decode (BL.fromStrict bs) :: Maybe Aeson.Value of
    Nothing -> Left "EndStreamResponse: malformed JSON"
    Just v -> decodeEndStreamValue v

decodeEndStreamValue :: Aeson.Value -> Either String EndStreamResponse
decodeEndStreamValue (Aeson.Object o) = do
  err <- case KeyMap.lookup "error" o of
    Nothing -> Right Nothing
    Just Aeson.Null -> Left "EndStreamResponse: \"error\" must not be null"
    Just v@(Aeson.Object _) -> Just <$> decodeConnectError v
    Just _ -> Left "EndStreamResponse: \"error\" must be an object"
  meta <- case KeyMap.lookup "metadata" o of
    Nothing -> Right []
    Just v -> metadataFromJSON v
  Right (EndStreamResponse err meta)
decodeEndStreamValue _ = Left "EndStreamResponse: expected a JSON object"
