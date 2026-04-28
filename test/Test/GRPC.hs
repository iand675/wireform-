module Test.GRPC (grpcTests) where

import qualified Data.ByteString as BS
import Test.Tasty
import Test.Tasty.HUnit

import Proto.GRPC

grpcTests :: TestTree
grpcTests = testGroup "gRPC Framing"
  [ singleMessageRoundtrip
  , emptyMessageRoundtrip
  , multiMessageRoundtrip
  , errorCases
  , frameStructure
  , compressedFrames
  ]

compressedFrames :: TestTree
compressedFrames = testGroup "Compressed frames"
  [ testCase "grpcFrameCompressed sets the flag byte" $ do
      let framed = grpcFrameCompressed (BS.pack [0x12, 0x34, 0x56])
      BS.index framed 0 @?= 0x01
      BS.length framed @?= 8
  , testCase "grpcUnframe rejects compressed frames without a codec" $ do
      let framed = grpcFrameCompressed (BS.pack [0xCC, 0xDD])
      case grpcUnframe framed of
        Left _ -> return ()
        Right _ -> assertFailure "expected error for unconfigured compressed frame"
  , testCase "grpcUnframeWith invokes the codec" $ do
      let payload = BS.pack [0xDE, 0xAD]
          framed = grpcFrameCompressed payload
          identity bs = Right bs
      grpcUnframeWith identity framed @?= Right payload
  , testCase "grpcUnframeWith accepts uncompressed frames untouched" $ do
      let payload = BS.pack [0x01, 0x02, 0x03]
          framed = grpcFrame payload
          rotate bs = Right (BS.reverse bs)
      grpcUnframeWith rotate framed @?= Right payload
  , testCase "grpcUnframeManyWith mixes compressed and plain" $ do
      let m1 = BS.pack [1, 2]
          m2 = BS.pack [3, 4]
          framed = grpcFrame m1 <> grpcFrameCompressed m2
          decoder = Right
      grpcUnframeManyWith decoder framed @?= Right [m1, m2]
  , testCase "grpcUnframeWith propagates codec errors" $ do
      let framed = grpcFrameCompressed (BS.pack [0xAB])
          fail_ _ = Left "kaboom"
      case grpcUnframeWith fail_ framed of
        Left "kaboom" -> return ()
        x -> assertFailure $ "expected codec error, got " <> show x
  ]

singleMessageRoundtrip :: TestTree
singleMessageRoundtrip = testGroup "Single message roundtrip"
  [ testCase "simple payload" $ do
      let payload = BS.pack [0x08, 0x96, 0x01]
          framed = grpcFrame payload
      grpcUnframe framed @?= Right payload

  , testCase "large payload" $ do
      let payload = BS.replicate 10000 0xAB
          framed = grpcFrame payload
      grpcUnframe framed @?= Right payload

  , testCase "single byte payload" $ do
      let payload = BS.singleton 0xFF
          framed = grpcFrame payload
      grpcUnframe framed @?= Right payload
  ]

emptyMessageRoundtrip :: TestTree
emptyMessageRoundtrip = testCase "Empty message roundtrip" $ do
  let payload = BS.empty
      framed = grpcFrame payload
  BS.length framed @?= 5
  grpcUnframe framed @?= Right payload

multiMessageRoundtrip :: TestTree
multiMessageRoundtrip = testGroup "Multiple messages"
  [ testCase "three messages" $ do
      let msgs = [BS.pack [1,2,3], BS.pack [4,5], BS.pack [6]]
          framed = grpcFrameMany msgs
      grpcUnframeMany framed @?= Right msgs

  , testCase "empty list" $ do
      let framed = grpcFrameMany []
      grpcUnframeMany framed @?= Right []

  , testCase "single in many" $ do
      let msgs = [BS.pack [0xDE, 0xAD]]
          framed = grpcFrameMany msgs
      grpcUnframeMany framed @?= Right msgs

  , testCase "mixed sizes including empty" $ do
      let msgs = [BS.empty, BS.pack [1], BS.replicate 256 0x42, BS.empty]
          framed = grpcFrameMany msgs
      grpcUnframeMany framed @?= Right msgs
  ]

errorCases :: TestTree
errorCases = testGroup "Error cases"
  [ testCase "too short for header" $ do
      case grpcUnframe (BS.pack [0x00, 0x00]) of
        Left _ -> return ()
        Right _ -> assertFailure "expected error for truncated header"

  , testCase "truncated payload" $ do
      case grpcUnframe (BS.pack [0x00, 0x00, 0x00, 0x05, 0x01, 0x02]) of
        Left _ -> return ()
        Right _ -> assertFailure "expected error for truncated payload"

  , testCase "trailing data" $ do
      let framed = grpcFrame (BS.pack [1,2,3])
          withTrailing = framed <> BS.singleton 0xFF
      case grpcUnframe withTrailing of
        Left _ -> return ()
        Right _ -> assertFailure "expected error for trailing data"

  , testCase "empty input to unframeMany" $ do
      grpcUnframeMany BS.empty @?= Right []
  ]

frameStructure :: TestTree
frameStructure = testCase "Frame header structure" $ do
  let payload = BS.pack [0x08, 0x96, 0x01]
      framed = grpcFrame payload
  BS.length framed @?= 8
  BS.index framed 0 @?= 0x00
  BS.index framed 1 @?= 0x00
  BS.index framed 2 @?= 0x00
  BS.index framed 3 @?= 0x00
  BS.index framed 4 @?= 0x03
