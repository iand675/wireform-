{-# LANGUAGE BangPatterns #-}

{- | Conformance test harness for the official protobuf conformance suite.

The conformance runner sends 'ConformanceRequest' messages via stdin
(length-prefixed) and expects 'ConformanceResponse' messages back.

To run:

@
conformance-test-runner --enforce_recommended ./wireform-conformance
@
-}
module Proto.Conformance (
  -- * Conformance types
  ConformanceRequest (..),
  defaultConformanceRequest,
  ConformanceResponse (..),
  defaultConformanceResponse,
  WireFormat (..),

  -- * Conformance runner
  conformanceMain,
) where

import Control.DeepSeq (NFData)
import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int32)
import Data.Text (Text)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Proto.Internal.Decode
import Proto.Internal.Encode
import Proto.Internal.Wire (Tag (..))
import System.IO (BufferMode (..), hFlush, hSetBinaryMode, hSetBuffering, isEOF, stdin, stdout)


data WireFormat = Protobuf | JSON | Jspb | TextFormat
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (NFData)


data ConformanceRequest = ConformanceRequest
  { crPayload :: !ByteString
  , crRequestedOutputFormat :: !Int32
  , crMessageType :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


defaultConformanceRequest :: ConformanceRequest
defaultConformanceRequest = ConformanceRequest "" 0 ""


instance MessageEncode ConformanceRequest where
  buildSized cr =
    (if BS.null (crPayload cr) then mempty else fieldBytes 1 (crPayload cr))
      <> ( if crRequestedOutputFormat cr == 0
            then mempty
            else fieldVarint 3 (fromIntegral (crRequestedOutputFormat cr))
         )
      <> (if crMessageType cr == "" then mempty else fieldString 4 (crMessageType cr))


instance MessageDecode ConformanceRequest where
  messageDecoder = loop defaultConformanceRequest
    where
      loop !cr = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure cr
          UJust (Tag 1 _) -> do v <- decodeFieldBytes; loop cr {crPayload = v}
          UJust (Tag 2 _) -> do _jsonPayload <- decodeFieldString; loop cr {crPayload = BS.empty}
          UJust (Tag 3 _) -> do v <- getVarint; loop cr {crRequestedOutputFormat = fromIntegral v}
          UJust (Tag 4 _) -> do v <- decodeFieldString; loop cr {crMessageType = v}
          UJust (Tag _ wt) -> skipField wt >> loop cr


newtype ConformanceResponse = ConformanceResponse
  { crsResult :: ConformanceResult
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data ConformanceResult
  = ParseError !Text
  | SerializeError !Text
  | RuntimeError !Text
  | ProtobufPayload !ByteString
  | JsonPayload !Text
  | Skipped !Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


defaultConformanceResponse :: ConformanceResponse
defaultConformanceResponse = ConformanceResponse (Skipped "")


instance MessageEncode ConformanceResponse where
  buildSized (ConformanceResponse r) = case r of
    ParseError t -> fieldString 1 t
    RuntimeError t -> fieldString 2 t
    ProtobufPayload b -> fieldBytes 3 b
    JsonPayload t -> fieldString 4 t
    Skipped t -> fieldString 5 t
    SerializeError t -> fieldString 6 t


instance MessageDecode ConformanceResponse where
  messageDecoder = loop defaultConformanceResponse
    where
      loop !cr = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure cr
          UJust (Tag 1 _) -> ConformanceResponse . ParseError <$> decodeFieldString
          UJust (Tag 2 _) -> ConformanceResponse . RuntimeError <$> decodeFieldString
          UJust (Tag 3 _) -> ConformanceResponse . ProtobufPayload <$> decodeFieldBytes
          UJust (Tag 4 _) -> ConformanceResponse . JsonPayload <$> decodeFieldString
          UJust (Tag 5 _) -> ConformanceResponse . Skipped <$> decodeFieldString
          UJust (Tag 6 _) -> ConformanceResponse . SerializeError <$> decodeFieldString
          UJust (Tag _ wt) -> skipField wt >> loop cr


{- | Main loop for conformance testing.
Reads length-delimited ConformanceRequest from stdin,
processes them, and writes length-delimited ConformanceResponse to stdout.
-}
conformanceMain :: (ConformanceRequest -> IO ConformanceResponse) -> IO ()
conformanceMain handler = do
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  hSetBuffering stdout NoBuffering
  go
  where
    go = do
      eof <- isEOF
      if eof
        then pure ()
        else do
          lenBytes <- BS.hGet stdin 4
          if BS.length lenBytes < 4
            then pure ()
            else do
              let len = fromIntegral (readLE32 lenBytes)
              payload <- BS.hGet stdin len
              case decodeMessage payload of
                Left _err -> do
                  let resp = ConformanceResponse (ParseError "Failed to decode ConformanceRequest")
                  writeResponse resp
                  go
                Right req -> do
                  resp <- handler req
                  writeResponse resp
                  go

    writeResponse resp = do
      let encoded = encodeMessage resp
          lenBytes = encodeLE32 (fromIntegral (BS.length encoded))
      BS.hPut stdout lenBytes
      BS.hPut stdout encoded
      hFlush stdout


readLE32 :: ByteString -> Word32
readLE32 bs =
  let b0 = fromIntegral (BS.index bs 0) :: Word32
      b1 = fromIntegral (BS.index bs 1) :: Word32
      b2 = fromIntegral (BS.index bs 2) :: Word32
      b3 = fromIntegral (BS.index bs 3) :: Word32
  in b0 + b1 * 256 + b2 * 65536 + b3 * 16777216


encodeLE32 :: Word32 -> ByteString
encodeLE32 n =
  BS.pack
    [ fromIntegral (n .&. 0xFF)
    , fromIntegral ((n `shiftR` 8) .&. 0xFF)
    , fromIntegral ((n `shiftR` 16) .&. 0xFF)
    , fromIntegral ((n `shiftR` 24) .&. 0xFF)
    ]


