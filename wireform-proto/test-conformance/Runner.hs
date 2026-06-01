{-# LANGUAGE OverloadedStrings #-}

{- | The @wireform-conformance-runner@ binary.

The upstream protobuf conformance_test_runner pipes
length-prefixed 'ConformanceRequest' messages over our stdin
and reads length-prefixed 'ConformanceResponse' messages back
from our stdout. This module is the IO loop; the per-request
logic lives in "Test.Conformance.Handler".

Length prefix is little-endian uint32 (NOT a varint — that's a
protocol detail of the runner, not the wire format).

== Manual smoke test

@
echo \-n "" | wireform-conformance-runner   # exits silently on EOF
@

Real use is via 'Test.Conformance.Driver', which builds the
upstream runner and pipes requests through this binary.
-}
module Main (main) where

import Control.Exception (SomeException, catch, evaluate)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as AesonT
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Word (Word32)
import Proto.Decode qualified as PD
import Proto.Encode qualified as PE
import Proto.Internal.JSON.WellKnown qualified as WK
import Proto.Registry (AnyCodec (..), TypeRegistry, registerCodec)
import System.IO (
  BufferMode (..),
  Handle,
  hFlush,
  hPutStrLn,
  hSetBinaryMode,
  hSetBuffering,
  isEOF,
  stderr,
  stdin,
  stdout,
 )
import Proto.TH.Metadata (withExtensionRegistry)
import Test.Conformance.Handler (handleRequest)
import Test.Conformance.Schema (
  ConformanceResponse,
  ConformanceResponse'Result (..),
  TestAllTypesProto2,
  TestAllTypesProto3,
  conformanceResponseResult,
  defaultConformanceResponse,
  registerExt_protobuf_test_messages_proto2_extension_int32,
 )


{- | The type registry used by the conformance runner: standard
WKT codecs plus the conformance suite's reference messages.
-}
conformanceRegistry :: TypeRegistry
conformanceRegistry =
  registerCodec
    "protobuf_test_messages.proto3.TestAllTypesProto3"
    testAllTypesProto3AnyCodec
    . registerCodec
      "protobuf_test_messages.proto2.TestAllTypesProto2"
      testAllTypesProto2AnyCodec
    $ WK.standardWktRegistry


main :: IO ()
main = do
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr LineBuffering
  -- Proto2 extension JSON codecs: the loadProto splice generates
  -- an ExtensionRegistry value per extend block; pass it via
  -- Data.Reflection.give so the Generated ToJSON/FromJSON instances
  -- can pick it up.
  let extReg = registerExt_protobuf_test_messages_proto2_extension_int32
  withExtensionRegistry extReg loop
  where
    loop = do
      eof <- isEOF
      if eof
        then pure ()
        else do
          mLen <- readLE32 stdin
          case mLen of
            Nothing -> pure () -- short read at boundary; treat as EOF
            Just len -> do
              payload <- BS.hGet stdin (fromIntegral len)
              if BS.length payload /= fromIntegral len
                then pure () -- truncated payload; the runner exited mid-message
                else do
                  resp <- case PD.decodeMessage payload of
                    Left e ->
                      pure
                        ( runtimeErr
                            ( "decode ConformanceRequest: "
                                <> T.pack (show e)
                            )
                        )
                    Right req -> evaluateOrCatch (handleRequest req)
                  writeResponseSafe resp
                  loop


{- | Catch every exception inside the handler so a single
malformed test case can't bring the runner down. The runner
treats a hung / killed test program as a hard failure across
the entire suite, which is far worse than a single 'runtime_error'.
-}
evaluateOrCatch :: IO ConformanceResponse -> IO ConformanceResponse
evaluateOrCatch act =
  (act >>= evaluate)
    `catch` \e -> do
      hPutStrLn stderr ("wireform-conformance-runner: " <> show (e :: SomeException))
      pure (runtimeErr (T.pack (show e)))


runtimeErr :: T.Text -> ConformanceResponse
runtimeErr t =
  defaultConformanceResponse
    { conformanceResponseResult = Just (ConformanceResponse'Result'RuntimeError t)
    }


{- | The non-WKT codec for the conformance suite's reference
message. The Any envelope inlines the message's JSON object
alongside @\@type@ rather than wrapping under @"value"@.
-}
testAllTypesProto3AnyCodec :: AnyCodec
testAllTypesProto3AnyCodec =
  AnyCodec
    { acToJSON = \bs ->
        case PD.decodeMessage bs :: Either PD.DecodeError TestAllTypesProto3 of
          Left e -> Left ("Any embedded TestAllTypesProto3: " <> show e)
          Right m -> Right (Aeson.toJSON m)
    , acFromJSON = \v ->
        case AesonT.parseEither (Aeson.parseJSON @TestAllTypesProto3) v of
          Left e -> Left e
          Right m -> Right (PE.encodeMessage m)
    , acIsWkt = False
    }


testAllTypesProto2AnyCodec :: AnyCodec
testAllTypesProto2AnyCodec =
  AnyCodec
    { acToJSON = \bs ->
        case PD.decodeMessage bs :: Either PD.DecodeError TestAllTypesProto2 of
          Left e -> Left ("Any embedded TestAllTypesProto2: " <> show e)
          Right m -> Right (Aeson.toJSON m)
    , acFromJSON = \v ->
        case AesonT.parseEither (Aeson.parseJSON @TestAllTypesProto2) v of
          Left e -> Left e
          Right m -> Right (PE.encodeMessage m)
    , acIsWkt = False
    }


writeResponse :: ConformanceResponse -> IO ()
writeResponse resp = do
  let encoded = PE.encodeMessage resp
      lenBytes = encodeLE32 (fromIntegral (BS.length encoded))
  BS.hPut stdout lenBytes
  BS.hPut stdout encoded
  hFlush stdout


{- | Force-evaluate the encoded bytes so any lazy exception
thrown by the JSON / TextFormat encoders (e.g. WKT range
check 'error' calls) is caught here rather than killing the
process mid-write. Falls back to a 'runtime_error' response
so the upstream runner sees a clean reply.
-}
writeResponseSafe :: ConformanceResponse -> IO ()
writeResponseSafe resp =
  writeResponse resp `catch` \e -> do
    hPutStrLn
      stderr
      ("wireform-conformance-runner (encode): " <> show (e :: SomeException))
    writeResponse (runtimeErr (T.pack (show e)))


-- ---------------------------------------------------------------------------
-- Tiny LE32 helpers; matches what the upstream runner sends.
-- ---------------------------------------------------------------------------

readLE32 :: Handle -> IO (Maybe Word32)
readLE32 h = do
  bs <- BS.hGet h 4
  if BS.length bs /= 4
    then pure Nothing
    else
      let b0 = fromIntegral (BS.index bs 0) :: Word32
          b1 = fromIntegral (BS.index bs 1) :: Word32
          b2 = fromIntegral (BS.index bs 2) :: Word32
          b3 = fromIntegral (BS.index bs 3) :: Word32
      in pure (Just (b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)))


encodeLE32 :: Word32 -> BS.ByteString
encodeLE32 n =
  BS.pack
    [ fromIntegral (n .&. 0xFF)
    , fromIntegral ((n `shiftR` 8) .&. 0xFF)
    , fromIntegral ((n `shiftR` 16) .&. 0xFF)
    , fromIntegral ((n `shiftR` 24) .&. 0xFF)
    ]
