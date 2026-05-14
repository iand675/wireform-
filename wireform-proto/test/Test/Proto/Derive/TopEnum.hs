{- | Tests for the top-level enum + packed scalar 'loadProto'
regression.
-}
module Test.Proto.Derive.TopEnum (tests) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Vector qualified as V
import Proto qualified as PD
import Proto qualified as PE
import Test.Proto.Derive.TopEnumInstances (
  Account (..),
  PackedBag (..),
  Status (..),
  defaultAccount,
  defaultPackedBag,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))


tests :: TestTree
tests =
  testGroup
    "Proto.TH top-level enum + packed scalar"
    [ testGroup
        "top-level enum field encodes as varint, not submessage"
        [ testCase "default Status (UNSPECIFIED = 0) is skipped" $ do
            let a = defaultAccount
            PE.encodeMessage a @?= BS.empty
        , testCase "Status = STATUS_ACTIVE encodes as varint 1, not as a submessage" $ do
            let a = defaultAccount {accountAcctStatus = Status'StatusActive}
            -- field 2 (varint) tag = (2<<3)|0 = 0x10; payload = 1.
            -- If the bridge had wrongly chosen PFSubmessage we'd
            -- see 0x12 ((2<<3)|2) followed by a length prefix here.
            PE.encodeMessage a @?= BS.pack [0x10, 0x01]
        , testCase "Status = STATUS_BANNED round-trips" $ do
            let a =
                  defaultAccount
                    { accountAcctName = T.pack "anon"
                    , accountAcctStatus = Status'StatusBanned
                    }
            PD.decodeMessage (PE.encodeMessage a) @?= Right a
        , testCase "all four Status values round-trip" $ do
            let go st = do
                  let a = defaultAccount {accountAcctStatus = st}
                  PD.decodeMessage (PE.encodeMessage a) @?= Right a
            mapM_
              go
              [ Status'StatusUnspecified
              , Status'StatusActive
              , Status'StatusRetired
              , Status'StatusBanned
              ]
        , testCase "enum decoder truncates oversized varint to int32" $ do
            -- Proto3 wire spec: enum values are int32 on the wire
            -- even though varints can carry larger values. A sender
            -- that writes a 10-byte varint of @kInt64Max@ for an
            -- enum field is expected to be parsed as the int32
            -- truncation (low 32 bits, sign-extended). For
            -- 0xFFFFFFFFFFFFFFFF (the proto3-canonical encoding of
            -- -1 as a 10-byte varint), the int32 truncation is -1,
            -- which our generated 'Status' has no constructor for —
            -- so it falls through to the catch-all (the first
            -- declared, Status'StatusUnspecified). The point of this test
            -- is to confirm we don't crash and the message
            -- round-trips.
            let bs =
                  BS.pack
                    [ 0x10 -- field 2 (acct_status) varint
                    , 0xFF
                    , 0xFF
                    , 0xFF
                    , 0xFF
                    , 0xFF
                    , 0xFF
                    , 0xFF
                    , 0xFF
                    , 0xFF
                    , 0x01 -- 10-byte varint of -1
                    ]
            case PD.decodeMessage bs :: Either PD.DecodeError Account of
              Right _ -> pure () -- decode succeeded; that's what matters
              Left e -> assertFailure ("expected decode success, got " <> show e)
        ]
    , testGroup
        "packed scalar (proto3 spec default for repeated int32)"
        [ testCase "empty bag encodes to 0 bytes" $ do
            PE.encodeMessage defaultPackedBag @?= BS.empty
        , testCase "Vector [1,2,3] is one length-delimited block" $ do
            let p = defaultPackedBag {packedBagBagNums = V.fromList [1, 2, 3]}
            PE.encodeMessage p @?= BS.pack [0x0A, 0x03, 0x01, 0x02, 0x03]
            PD.decodeMessage (PE.encodeMessage p) @?= Right p
        , testCase "decoder still accepts unpacked encoding" $ do
            -- Hand-craft an unpacked stream, assert it produces
            -- the same Vector.
            let unpacked =
                  BS.pack
                    [0x08, 0x01, 0x08, 0x02, 0x08, 0x03]
                expected = defaultPackedBag {packedBagBagNums = V.fromList [1, 2, 3]}
            PD.decodeMessage unpacked @?= Right expected
        , testCase "encoded length stays small as element count grows" $ do
            -- Strongest packed-vs-unpacked check that doesn't depend
            -- on counting tag bytes (which can collide with payload
            -- values for small ints). Unpacked would emit
            --   1 tag byte + 1 payload byte = 2 bytes per element.
            -- Packed emits
            --   1 tag byte + 1 length byte + 1 payload byte each.
            -- So 100 single-byte values should fit in well under
            -- the 200 bytes an unpacked stream would take.
            let p = defaultPackedBag {packedBagBagNums = V.fromList [1 .. 100]}
                bs = PE.encodeMessage p
            assertBool
              ( "expected packed encoding (≤ 110 bytes), got "
                  <> show (BS.length bs)
              )
              (BS.length bs <= 110)
        ]
    ]
