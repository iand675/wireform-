{- | Hand-computed golden byte vectors for the protobuf wire
format. These exist to guard the byte-equivalence regression in
"Test.Proto.Derive": before this file the regression compared
two implementations against each other ('loadProto' vs.
'deriveProtoFromTranslated'), which after the bridge rewire
both go through the same body builders in
'Proto.TH.Derive.Internal' — so the assertion had degraded to
"the bridge agrees with itself".

Adding hand-coded reference bytes (computed below from the
proto3 wire spec) restores the meaningful guarantee: if the
bridge ever drifts off-spec the assertion will catch it
immediately, no matter how many implementations agree with
each other.
-}
module Test.Proto.Derive.Golden (tests) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Word (Word8)
import Proto qualified as PE
import Test.Proto.Derive.RegressionInstances (
  RegInventory (..),
  RegItem (..),
  defaultRegInventory,
  defaultRegItem,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))


tests :: TestTree
tests =
  testGroup
    "Proto.TH.Derive byte-exact golden vectors"
    [ testCase "empty RegItem encodes to 0 bytes" $ do
        let p = defaultRegItem
        PE.encodeMessage p @?= BS.empty
    , testCase "RegItem { regi_name = \"widget\", regi_count = 99 }" $ do
        let p =
              defaultRegItem
                { regItemRegiName = T.pack "widget"
                , regItemRegiCount = 99
                }
        PE.encodeMessage p
          @?= bytes
            [ 0x0A
            , 0x06 -- field 1 (string), len 6
            , 0x77
            , 0x69
            , 0x64 -- "wid"
            , 0x67
            , 0x65
            , 0x74 -- "get"
            , 0x10
            , 0x63 -- field 2 (varint int32), 99
            ]
    , testCase "RegItem { regi_count = 1 } skips empty name" $ do
        let p = defaultRegItem {regItemRegiCount = 1}
        PE.encodeMessage p
          @?= bytes
            [0x10, 0x01]
    , testCase "empty RegInventory encodes to 0 bytes" $ do
        let p = defaultRegInventory
        PE.encodeMessage p @?= BS.empty
    , testCase "RegInventory { name = \"warehouse-7\" }" $ do
        let p = defaultRegInventory {regInventoryReginvName = T.pack "warehouse-7"}
        PE.encodeMessage p
          @?= bytes
            [ 0x0A
            , 0x0B
            , 0x77
            , 0x61
            , 0x72
            , 0x65
            , 0x68
            , 0x6F
            , 0x75
            , 0x73
            , 0x65
            , 0x2D
            , 0x37
            ]
    , testCase "RegInventory { name = \"depot\", items = 3 entries }" $ do
        let p =
              defaultRegInventory
                { regInventoryReginvName = T.pack "depot"
                , regInventoryReginvItems =
                    V.fromList
                      [ defaultRegItem {regItemRegiName = T.pack "alpha", regItemRegiCount = 1}
                      , defaultRegItem {regItemRegiName = T.pack "beta", regItemRegiCount = 2}
                      , defaultRegItem {regItemRegiName = T.pack "gamma", regItemRegiCount = 3}
                      ]
                }
        PE.encodeMessage p
          @?= bytes
            -- field 1 (name): tag, len, "depot"
            [ 0x0A
            , 0x05
            , 0x64
            , 0x65
            , 0x70
            , 0x6F
            , 0x74
            , -- field 2 (item): tag, len, payload  -- "alpha" + 1
              0x12
            , 0x09
            , 0x0A
            , 0x05
            , 0x61
            , 0x6C
            , 0x70
            , 0x68
            , 0x61
            , 0x10
            , 0x01
            , -- field 2 (item): tag, len, payload  -- "beta" + 2
              0x12
            , 0x08
            , 0x0A
            , 0x04
            , 0x62
            , 0x65
            , 0x74
            , 0x61
            , 0x10
            , 0x02
            , -- field 2 (item): tag, len, payload  -- "gamma" + 3
              0x12
            , 0x09
            , 0x0A
            , 0x05
            , 0x67
            , 0x61
            , 0x6D
            , 0x6D
            , 0x61
            , 0x10
            , 0x03
            ]
    ]


bytes :: [Word8] -> BS.ByteString
bytes = BS.pack
