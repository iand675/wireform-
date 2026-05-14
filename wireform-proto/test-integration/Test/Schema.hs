{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Test.Schema (schemaTests) where

import Data.Int (Int32, Int64)
import Test.Tasty
import Test.Tasty.HUnit

import Proto.Google.Protobuf.WellKnownTypes.Timestamp

schemaTests :: TestTree
schemaTests = testGroup "Schema (generated record fields)"
  [ testCase "generated field names" $ do
      let ts = defaultTimestamp { timestampSeconds = 42, timestampNanos = 99 }
      timestampSeconds ts @?= (42 :: Int64)
      timestampNanos ts @?= (99 :: Int32)

  , testCase "default value" $ do
      timestampSeconds defaultTimestamp @?= (0 :: Int64)
      timestampNanos defaultTimestamp @?= (0 :: Int32)

  , testCase "record update" $ do
      let ts = defaultTimestamp { timestampSeconds = 100, timestampNanos = 200 }
          ts' = ts { timestampSeconds = 300 }
      timestampSeconds ts' @?= (300 :: Int64)
      timestampNanos ts' @?= (200 :: Int32)
  ]
