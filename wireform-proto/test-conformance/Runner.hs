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
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Word (Word32)
import Proto.Decode qualified as PD
import Proto.Encode qualified as PE
import Proto.Registry (TypeRegistry, emptyRegistry, registerGlobalAnyCodecs, registerMessage)
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
import Test.Conformance.Handler (handleRequest)
import Test.Conformance.EditionUnstable qualified as Unstable
import Test.Conformance.MigrationProto2 qualified as Migration2
import Test.Conformance.MigrationProto3 qualified as Migration3
import Test.Conformance.Schema (
  ConformanceResponse,
  ConformanceResponse'Result (..),
  TestAllTypesProto2,
  TestAllTypesProto3,
  TestAllTypesEdition2023,
  conformanceResponseResult,
  defaultConformanceResponse,
 )


-- | Application codecs for Any payloads in the conformance suite.
conformanceRegistry :: TypeRegistry
conformanceRegistry =
  registerMessage (Proxy @TestAllTypesProto3)
    . registerMessage (Proxy @TestAllTypesProto2)
    . registerMessage (Proxy @TestAllTypesEdition2023)
    . registerMessage (Proxy @Migration2.TestAllTypesProto2)
    . registerMessage (Proxy @Migration3.TestAllTypesProto3)
    . registerMessage (Proxy @Unstable.TestAllTypesEditionUnstable)
    $ emptyRegistry


main :: IO ()
main = do
  registerGlobalAnyCodecs conformanceRegistry
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr LineBuffering
  loop
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
