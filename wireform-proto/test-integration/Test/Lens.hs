{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Test.Lens (lensTests) where

import Data.Int (Int32, Int64)
import Test.Tasty
import Test.Tasty.HUnit

import Proto.Google.Protobuf.WellKnownTypes.Timestamp (Timestamp(..), defaultTimestamp)

lensTests :: TestTree
lensTests = testGroup "Proto.Lens (record field access)"
  [ testCase "read timestampSeconds" $ do
      let ts = defaultTimestamp { timestampSeconds = 42, timestampNanos = 99 }
      timestampSeconds ts @?= (42 :: Int64)

  , testCase "read timestampNanos" $ do
      let ts = defaultTimestamp { timestampSeconds = 42, timestampNanos = 99 }
      timestampNanos ts @?= (99 :: Int32)

  , testCase "update via record syntax" $ do
      let ts = defaultTimestamp { timestampSeconds = 42, timestampNanos = 99 }
          ts' = ts { timestampSeconds = 100, timestampNanos = 200 }
      timestampSeconds ts' @?= 100
      timestampNanos ts' @?= 200

  , testCase "default values" $ do
      timestampSeconds defaultTimestamp @?= 0
      timestampNanos defaultTimestamp @?= 0
  ]
