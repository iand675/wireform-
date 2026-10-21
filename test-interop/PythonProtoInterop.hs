{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Cross-language conformance tests.

Hedgehog generates random protobuf messages in Haskell, encodes them,
pipes the bytes to Python's official protobuf library for decode+re-encode,
then decodes the Python output back in Haskell and checks equality.

This proves wire-format compatibility between wireform and the reference
Python implementation for all scalar types, nested messages, repeated
fields, and enums.
-}
module Main where

import Data.ByteString qualified as BS
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Config (resolveSkip)
import Hedgehog.Range qualified as Range
import InteropTypes
import Proto.Decode
import Proto.Encode
import System.Directory (getCurrentDirectory)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Test.Syd
import Test.Syd.Hedgehog ()


main :: IO ()
main = do
  -- Force Hedgehog's "HEDGEHOG_SKIP" CAF before sydtest starts its workers.
  -- Every property forces it first; when many capabilities hit it at once,
  -- GHC 9.8.4's RTS can lose the wakeup of a thread blocked on it, leaving
  -- one property hung until the per-test timeout.
  _ <- resolveSkip Nothing
  -- Verify Python oracle is functional before running tests
  pd <- protoDir
  (ec, _, err) <-
    readProcessWithExitCode
      "python3"
      ["-c", "import sys; sys.path.insert(0,'" <> pd <> "'); import interop_pb2"]
      ""
  case ec of
    ExitSuccess -> sydTest tests
    ExitFailure _ -> do
      putStrLn ("Python interop_pb2 not available: " <> err)
      putStrLn ("Ensure protoc has been run on " <> pd <> "/interop.proto")
      putStrLn "Skipping interop tests."


tests :: Spec
tests =
  describe "Python Interop Conformance" $
    sequence_
      [ describe "Haskell-encode → Python-decode → re-encode → Haskell-decode" $
          sequence_
            [ it "Scalars" $ withTests 200 $ property $ do
                msg <- forAll genScalars
                roundtripViaPython "Scalars" msg
            , it "Nested" $ withTests 100 $ property $ do
                msg <- forAll genNested
                roundtripViaPython "Nested" msg
            , it "Repeated" $ withTests 100 $ property $ do
                msg <- forAll genRepeated
                roundtripViaPython "Repeated" msg
            ]
      , describe "Python-encode → Haskell-decode" $
          sequence_
            [ it "Scalars with all fields" $ do
                bs <-
                  pythonEncode "Scalars" $
                    "{\"fDouble\":3.14,\"fFloat\":1.5,\"fInt32\":42,\"fInt64\":\"100\""
                      <> ",\"fUint32\":7,\"fUint64\":\"8\",\"fSint32\":-5,\"fSint64\":\"-10\""
                      <> ",\"fFixed32\":99,\"fFixed64\":\"200\",\"fSfixed32\":-30,\"fSfixed64\":\"-40\""
                      <> ",\"fBool\":true,\"fString\":\"hello\",\"fBytes\":\"AQID\"}"
                case decodeMessage bs of
                  Left err -> expectationFailure (show err)
                  Right (s :: Scalars) -> do
                    sfInt32 s `shouldBe` 42
                    sfString s `shouldBe` "hello"
                    sfBool s `shouldBe` True
                    sfSint32 s `shouldBe` (-5)
                    sfFixed32 s `shouldBe` 99
            , it "Nested with submessage" $ do
                bs <- pythonEncode "Nested" "{\"label\":\"test\",\"payload\":{\"fInt32\":99},\"color\":\"COLOR_BLUE\"}"
                case decodeMessage bs of
                  Left err -> expectationFailure (show err)
                  Right (n :: Nested) -> do
                    nLabel n `shouldBe` "test"
                    nColor n `shouldBe` ColorBlue
                    case nPayload n of
                      Nothing -> expectationFailure "Expected payload"
                      Just s -> sfInt32 s `shouldBe` 99
            , it "Repeated with packed ints" $ do
                bs <- pythonEncode "Repeated" "{\"ints\":[1,2,3],\"strings\":[\"a\",\"b\"]}"
                case decodeMessage bs of
                  Left err -> expectationFailure (show err)
                  Right (r :: Repeated) -> do
                    V.toList (rInts r) `shouldBe` [1, 2, 3]
                    V.toList (rStrings r) `shouldBe` ["a", "b"]
            , it "Empty message" $ do
                bs <- pythonEncode "Scalars" "{}"
                case decodeMessage bs of
                  Left err -> expectationFailure (show err)
                  Right (s :: Scalars) -> s `shouldBe` defaultScalars
            ]
      , describe "Edge cases" $
          sequence_
            [ it "Max int32" $ do
                let msg = defaultScalars {sfInt32 = maxBound}
                roundtripViaPythonIO "Scalars" msg
            , it "Min int32" $ do
                let msg = defaultScalars {sfInt32 = minBound}
                roundtripViaPythonIO "Scalars" msg
            , it "Max uint64" $ do
                let msg = defaultScalars {sfUint64 = maxBound}
                roundtripViaPythonIO "Scalars" msg
            , it "Empty string" $ do
                let msg = defaultScalars {sfString = ""}
                roundtripViaPythonIO "Scalars" msg
            , it "Unicode string" $ do
                let msg = defaultScalars {sfString = "こんにちは世界"}
                roundtripViaPythonIO "Scalars" msg
            , it "Large bytes" $ do
                let msg = defaultScalars {sfBytes = BS.replicate 1000 0xAB}
                roundtripViaPythonIO "Scalars" msg
            , it "Negative sint32" $ do
                let msg = defaultScalars {sfSint32 = -12345}
                roundtripViaPythonIO "Scalars" msg
            , it "Nested with no payload" $ do
                let msg = Nested "label" Nothing ColorRed
                roundtripViaPythonIO "Nested" msg
            , it "Repeated empty" $ do
                let msg = Repeated V.empty V.empty V.empty
                roundtripViaPythonIO "Repeated" msg
            ]
      ]


-- Generators

genScalars :: Gen Scalars
genScalars =
  Scalars
    <$> Gen.double (Range.linearFrac (-1e6) 1e6)
    <*> Gen.float (Range.linearFrac (-1e3) 1e3)
    <*> Gen.int32 (Range.linear (-100000) 100000)
    <*> Gen.int64 (Range.linear (-100000000) 100000000)
    <*> Gen.word32 (Range.linear 0 100000)
    <*> Gen.word64 (Range.linear 0 100000000)
    <*> Gen.int32 (Range.linear (-100000) 100000)
    <*> Gen.int64 (Range.linear (-100000000) 100000000)
    <*> Gen.word32 (Range.linear 0 100000)
    <*> Gen.word64 (Range.linear 0 100000000)
    <*> Gen.int32 (Range.linear (-100000) 100000)
    <*> Gen.int64 (Range.linear (-100000000) 100000000)
    <*> Gen.bool
    <*> Gen.text (Range.linear 0 50) Gen.alphaNum
    <*> Gen.bytes (Range.linear 0 50)


genColor :: Gen Color
genColor = Gen.element [ColorUnspecified, ColorRed, ColorGreen, ColorBlue]


genNested :: Gen Nested
genNested =
  Nested
    <$> Gen.text (Range.linear 0 30) Gen.alphaNum
    <*> Gen.maybe genScalars
    <*> genColor


genRepeated :: Gen Repeated
genRepeated =
  Repeated
    <$> (V.fromList <$> Gen.list (Range.linear 0 20) (Gen.int32 (Range.linear (-1000) 1000)))
    <*> (V.fromList <$> Gen.list (Range.linear 0 10) (Gen.text (Range.linear 0 20) Gen.alphaNum))
    <*> (V.fromList <$> Gen.list (Range.linear 0 3) genScalars)


-- Python oracle interaction.
-- Uses temp files for binary I/O to avoid encoding issues with process pipes.

callPythonRoundtrip :: String -> BS.ByteString -> IO (Either String BS.ByteString)
callPythonRoundtrip msgType inputBytes = do
  pd <- protoDir
  let tmpIn = "/tmp/wireform-interop-in.bin"
      tmpOut = "/tmp/wireform-interop-out.bin"
  BS.writeFile tmpIn inputBytes
  (exitCode, _, stderr) <-
    readProcessWithExitCode
      "bash"
      [ "-c"
      , "PYTHONPATH="
          <> pd
          <> ":$PYTHONPATH python3 "
          <> pd
          <> "/oracle.py roundtrip "
          <> msgType
          <> " < "
          <> tmpIn
          <> " > "
          <> tmpOut
      ]
      ""
  case exitCode of
    ExitSuccess -> Right <$> BS.readFile tmpOut
    ExitFailure c -> pure (Left ("Python exit " <> show c <> ": " <> stderr))


pythonEncode :: String -> String -> IO BS.ByteString
pythonEncode msgType json = do
  pd <- protoDir
  let tmpOut = "/tmp/wireform-interop-encode.bin"
  (exitCode, _, stderr) <-
    readProcessWithExitCode
      "bash"
      [ "-c"
      , "PYTHONPATH="
          <> pd
          <> ":$PYTHONPATH python3 "
          <> pd
          <> "/oracle.py encode "
          <> msgType
          <> " '"
          <> json
          <> "' > "
          <> tmpOut
      ]
      ""
  case exitCode of
    ExitSuccess -> BS.readFile tmpOut
    ExitFailure c -> error ("Python encode exit " <> show c <> ": " <> stderr)


-- The Python oracle and its generated module live alongside this
-- test under test-interop/proto/. Resolve relative to the cwd
-- (cabal runs the test suite from the project root) so the suite
-- works on any developer machine, not just the cloud-agent VM
-- whose absolute path used to be hard-coded here.
protoDir :: IO FilePath
protoDir = do
  cwd <- getCurrentDirectory
  pure (cwd </> "test-interop" </> "proto")


-- Property: encode in Haskell, roundtrip through Python, decode matches.
roundtripViaPython
  :: (MessageEncode a, MessageDecode a, Show a, Eq a)
  => String -> a -> PropertyT IO ()
roundtripViaPython msgType msg = do
  let encoded = encodeMessage msg
  result <- evalIO $ callPythonRoundtrip msgType encoded
  case result of
    Left err -> do
      annotate ("Python error: " <> err)
      annotate ("Encoded " <> show (BS.length encoded) <> " bytes")
      failure
    Right reencoded ->
      case decodeMessage reencoded of
        Left decErr -> do
          annotate ("Decode of Python output failed: " <> show decErr)
          failure
        Right decoded -> decoded === msg


-- Same thing but for HUnit it.
roundtripViaPythonIO
  :: (MessageEncode a, MessageDecode a, Show a, Eq a)
  => String -> a -> IO ()
roundtripViaPythonIO msgType msg = do
  let encoded = encodeMessage msg
  result <- callPythonRoundtrip msgType encoded
  case result of
    Left err -> expectationFailure ("Python error: " <> err)
    Right reencoded ->
      case decodeMessage reencoded of
        Left decErr -> expectationFailure ("Decode failed: " <> show decErr)
        Right decoded -> decoded `shouldBe` msg
