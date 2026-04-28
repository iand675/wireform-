module Test.MsgPackRPC (msgPackRPCTests) where

import qualified Data.ByteString as BS
import qualified Data.Vector as V

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog

import MsgPack.Encode (encode)
import MsgPack.RPC (RPCMessage(..), encodeRPC, decodeRPC)
import qualified MsgPack.Value as MV

msgPackRPCTests :: TestTree
msgPackRPCTests = testGroup "MsgPack RPC"
  [ requestRoundtrip
  , responseRoundtrip
  , notificationRoundtrip
  , unitTests
  , errorTests
  ]

requestRoundtrip :: TestTree
requestRoundtrip = testGroup "Request roundtrip"
  [ testProperty "request roundtrip" $ property $ do
      msgid <- forAll $ Gen.word32 Range.linearBounded
      method <- forAll $ Gen.text (Range.linear 1 50) Gen.alphaNum
      nparams <- forAll $ Gen.int (Range.linear 0 10)
      params <- forAll $ V.replicateM nparams genSimpleValue
      let msg = RPCRequest msgid method params
      decodeRPC (encodeRPC msg) === Right msg
  ]

responseRoundtrip :: TestTree
responseRoundtrip = testGroup "Response roundtrip"
  [ testProperty "response with result" $ property $ do
      msgid <- forAll $ Gen.word32 Range.linearBounded
      result <- forAll genNonNilValue
      let msg = RPCResponse msgid Nothing (Just result)
      decodeRPC (encodeRPC msg) === Right msg

  , testProperty "response with error" $ property $ do
      msgid <- forAll $ Gen.word32 Range.linearBounded
      err <- forAll genNonNilValue
      let msg = RPCResponse msgid (Just err) Nothing
      decodeRPC (encodeRPC msg) === Right msg

  , testProperty "response with both nil" $ property $ do
      msgid <- forAll $ Gen.word32 Range.linearBounded
      let msg = RPCResponse msgid Nothing Nothing
      decodeRPC (encodeRPC msg) === Right msg
  ]

notificationRoundtrip :: TestTree
notificationRoundtrip = testGroup "Notification roundtrip"
  [ testProperty "notification roundtrip" $ property $ do
      method <- forAll $ Gen.text (Range.linear 1 50) Gen.alphaNum
      nparams <- forAll $ Gen.int (Range.linear 0 10)
      params <- forAll $ V.replicateM nparams genSimpleValue
      let msg = RPCNotification method params
      decodeRPC (encodeRPC msg) === Right msg
  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "request with no params" $ do
      let msg = RPCRequest 1 "add" V.empty
      decodeRPC (encodeRPC msg) @?= Right msg

  , testCase "request with params" $ do
      let msg = RPCRequest 42 "echo" (V.fromList [MV.String "hello", MV.Word 123])
      decodeRPC (encodeRPC msg) @?= Right msg

  , testCase "response success" $ do
      let msg = RPCResponse 1 Nothing (Just (MV.Word 42))
      decodeRPC (encodeRPC msg) @?= Right msg

  , testCase "response error" $ do
      let msg = RPCResponse 1 (Just (MV.String "oops")) Nothing
      decodeRPC (encodeRPC msg) @?= Right msg

  , testCase "notification" $ do
      let msg = RPCNotification "update" (V.fromList [MV.Bool True])
      decodeRPC (encodeRPC msg) @?= Right msg
  ]

errorTests :: TestTree
errorTests = testGroup "Error cases"
  [ testCase "empty input" $
      case decodeRPC BS.empty of
        Left _ -> pure ()
        Right _ -> assertFailure "expected error on empty input"

  , testCase "non-array top level" $
      case decodeRPC (encode (MV.Word 42)) of
        Left _ -> pure ()
        Right _ -> assertFailure "expected error on non-array"

  , testCase "wrong array length" $
      case decodeRPC (encode (MV.Array (V.fromList [MV.Word 0, MV.Word 1]))) of
        Left _ -> pure ()
        Right _ -> assertFailure "expected error on wrong array length"

  , testCase "response with both error and result is rejected (msgpack-rpc spec)" $ do
      -- Spec: exactly one of `error` / `result` may be non-Nil.
      let bad = encode (MV.Array (V.fromList
            [ MV.Word 1
            , MV.Word 7
            , MV.String "boom"
            , MV.Word 42
            ]))
      case decodeRPC bad of
        Left  _ -> pure ()
        Right v -> assertFailure $
          "expected rejection of both-error-and-result, got: " ++ show v
  ]

genSimpleValue :: Gen MV.Value
genSimpleValue = Gen.choice
  [ pure MV.Nil
  , MV.Bool <$> Gen.bool
  , MV.Word <$> Gen.word64 (Range.linear 0 10000)
  , MV.Int <$> Gen.int64 (Range.linear (-10000) (-1))
  , MV.String <$> Gen.text (Range.linear 0 50) Gen.alphaNum
  ]

genNonNilValue :: Gen MV.Value
genNonNilValue = Gen.choice
  [ MV.Bool <$> Gen.bool
  , MV.Word <$> Gen.word64 (Range.linear 0 10000)
  , MV.Int <$> Gen.int64 (Range.linear (-10000) (-1))
  , MV.String <$> Gen.text (Range.linear 0 50) Gen.alphaNum
  ]
