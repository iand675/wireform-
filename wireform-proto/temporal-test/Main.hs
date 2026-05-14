{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Exception (try, SomeException)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

import Proto (encodeMessage)
import Proto (decodeMessage, DecodeError)

import Proto.Temporalio.Api.Common.V1.Message
import Proto.Temporalio.Api.Enums.V1.Common

main :: IO ()
main = do
  hPutStrLn stderr "=== Temporal API codegen validation ==="
  ok <- sequence
    [ testBinaryRoundTrip
    , testJsonAnnotations
    , testPythonInterop
    ]
  if and ok
    then hPutStrLn stderr "\n=== All tests passed ===" >> exitSuccess
    else hPutStrLn stderr "\n=== SOME TESTS FAILED ===" >> exitFailure

testBinaryRoundTrip :: IO Bool
testBinaryRoundTrip = do
  hPutStrLn stderr "\n--- Binary round-trip tests ---"

  let blob = defaultDataBlob
        { dataBlobEncodingType = EncodingType'EncodingTypeProto3
        , dataBlobData = "hello world"
        }
  let encoded = encodeMessage blob
  hPutStrLn stderr ("  DataBlob: encoded " <> show (BS.length encoded) <> " bytes")
  case decodeMessage encoded of
    Left err -> do
      hPutStrLn stderr ("  FAIL: DataBlob decode: " <> show err)
      pure False
    Right (decoded :: DataBlob) ->
      if decoded.dataBlobEncodingType == blob.dataBlobEncodingType
         && decoded.dataBlobData == blob.dataBlobData
        then do hPutStrLn stderr "  OK: DataBlob round-trip matches"; pure True
        else do hPutStrLn stderr "  FAIL: DataBlob mismatch"; pure False

testJsonAnnotations :: IO Bool
testJsonAnnotations = do
  hPutStrLn stderr "\n--- JSON annotation tests ---"

  let wfExec = defaultWorkflowExecution
        { workflowExecutionWorkflowId = "my-wf"
        , workflowExecutionRunId = "my-run"
        }
  let json = show (Aeson.encode wfExec)
  hPutStrLn stderr ("  WorkflowExecution JSON: " <> json)

  let enumJson = Aeson.toJSON EncodingType'EncodingTypeJson
  hPutStrLn stderr ("  EncodingType JSON: " <> show enumJson)
  case enumJson of
    Aeson.String "ENCODING_TYPE_JSON" -> do
      hPutStrLn stderr "  OK: enum JSON name correct"
      pure True
    other -> do
      hPutStrLn stderr ("  FAIL: unexpected enum JSON: " <> show other)
      pure False

testPythonInterop :: IO Bool
testPythonInterop = do
  hPutStrLn stderr "\n--- Python interop tests ---"

  let hsBlob = defaultDataBlob
        { dataBlobEncodingType = EncodingType'EncodingTypeJson
        , dataBlobData = "hello from haskell"
        }
  BS.writeFile "/tmp/interop_hs_datablob.bin" (encodeMessage hsBlob)
  hPutStrLn stderr "  Wrote /tmp/interop_hs_datablob.bin"

  let hsWf = defaultWorkflowExecution
        { workflowExecutionWorkflowId = "hs-workflow-999"
        , workflowExecutionRunId = "hs-run-888"
        }
  BS.writeFile "/tmp/interop_hs_wfexec.bin" (encodeMessage hsWf)
  hPutStrLn stderr "  Wrote /tmp/interop_hs_wfexec.bin"

  result <- try (BS.readFile "/tmp/interop_datablob.bin")
  case result of
    Left (_ :: SomeException) -> do
      hPutStrLn stderr "  SKIP: Python data not found (run interop_gen.py first)"
      pure True
    Right pyBlobData -> do
      hPutStrLn stderr ("  Read Python DataBlob: " <> show (BS.length pyBlobData) <> " bytes")
      case decodeMessage pyBlobData of
        Left err -> do
          hPutStrLn stderr ("  FAIL: decode Python DataBlob: " <> show err)
          pure False
        Right (decoded :: DataBlob) -> do
          let ok1 = decoded.dataBlobEncodingType == EncodingType'EncodingTypeProto3
              ok2 = decoded.dataBlobData == "hello from python"
          if ok1 && ok2
            then hPutStrLn stderr "  OK: Python DataBlob decoded correctly"
            else hPutStrLn stderr ("  FAIL: Python DataBlob mismatch: enc=" <> show decoded.dataBlobEncodingType <> " data=" <> show decoded.dataBlobData)

          pyWfData <- BS.readFile "/tmp/interop_wfexec.bin"
          hPutStrLn stderr ("  Read Python WorkflowExecution: " <> show (BS.length pyWfData) <> " bytes")
          case decodeMessage pyWfData of
            Left err -> do
              hPutStrLn stderr ("  FAIL: decode Python WF: " <> show err)
              pure False
            Right (wf :: WorkflowExecution) -> do
              let wok1 = wf.workflowExecutionWorkflowId == "wf-python-test-123"
                  wok2 = wf.workflowExecutionRunId == "run-python-abc-456"
              if wok1 && wok2
                then do hPutStrLn stderr "  OK: Python WorkflowExecution decoded correctly"; pure True
                else do hPutStrLn stderr ("  FAIL: Python WF mismatch"); pure False
