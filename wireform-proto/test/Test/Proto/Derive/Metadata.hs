{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the metadata satellite instances 'loadProto' now
emits in addition to the wire codecs:

  * 'ProtoMessage' — schema metadata.
  * 'Aeson.ToJSON' \/ 'Aeson.FromJSON' — proto3 canonical JSON.
  * 'Hashable' — recursive structural hash.
  * 'ProtoEnum' — enum metadata + numeric \<-\> name conversion.

The fixtures we exercise here are the same ones the
'topenum_regression.proto' splice already generates for the
top-level enum tests, plus a couple of small derived messages.
-}
module Test.Proto.Derive.Metadata (tests) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as AesonKM
import Data.Hashable (hash, hashWithSalt)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Vector qualified as V
import Proto.Schema qualified as PS
import Test.Proto.Derive.TopEnumInstances (
  Account (..),
  PackedBag (..),
  Status (..),
  defaultAccount,
  defaultPackedBag,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))


tests :: TestTree
tests =
  testGroup
    "loadProto satellite instances"
    [ testGroup
        "ProtoMessage"
        [ testCase "Account name + package + default" $ do
            PS.protoMessageName (Proxy :: Proxy Account) @?= "Account"
            PS.protoPackageName (Proxy :: Proxy Account) @?= ""
            PS.protoDefaultValue @?= defaultAccount
        , testCase "Account field descriptors expose names + numbers" $ do
            let fields = PS.protoFieldDescriptors (Proxy :: Proxy Account)
            Map.keys fields @?= [1, 2]
            PS.messageFieldNumbers (Proxy :: Proxy Account) @?= [1, 2]
            let names = PS.messageFieldNames (Proxy :: Proxy Account)
            names @?= ["acct_name", "acct_status"]
        , testCase "Account field descriptor by name returns acct_status" $ do
            case PS.lookupFieldDescriptor "acct_status" (Proxy :: Proxy Account) of
              Just (PS.SomeField fd) -> do
                PS.fdNumber fd @?= 2
                -- Top-level enum fields appear as EnumType in the
                -- schema descriptor (not MessageType — that was the
                -- whole point of the top-level-enum loadProto fix).
                PS.fdTypeDesc fd @?= PS.EnumType "Status"
              Nothing -> error "lookupFieldDescriptor returned Nothing"
        , testCase "PackedBag schema describes a repeated int32" $ do
            case PS.lookupFieldDescriptor "bag_nums" (Proxy :: Proxy PackedBag) of
              Just (PS.SomeField fd) -> do
                PS.fdLabel fd @?= PS.LabelRepeated
                PS.fdTypeDesc fd @?= PS.ScalarType PS.Int32Field
              Nothing -> error "PackedBag bag_nums descriptor missing"
        ]
    , testGroup
        "ProtoEnum (Status)"
        [ testCase "primary names round-trip through to/fromProtoEnumValue" $ do
            PS.toProtoEnumValue Status'StatusUnspecified @?= 0
            PS.toProtoEnumValue Status'StatusActive @?= 1
            PS.toProtoEnumValue Status'StatusRetired @?= 2
            PS.toProtoEnumValue Status'StatusBanned @?= 3
            PS.fromProtoEnumValue 0 @?= Just Status'StatusUnspecified
            PS.fromProtoEnumValue 1 @?= Just Status'StatusActive
            -- Open-enum representation: an unknown wire value
            -- now round-trips through the synthetic 'Unrecognized'
            -- variant rather than disappearing as 'Nothing'.
            PS.fromProtoEnumValue 99 @?= Just (Status''Unrecognized 99)
        , testCase "protoEnumValues lists every declared value" $ do
            let values = PS.protoEnumValues (Proxy :: Proxy Status)
            values
              @?= [ ("STATUS_UNSPECIFIED", 0)
                  , ("STATUS_ACTIVE", 1)
                  , ("STATUS_RETIRED", 2)
                  , ("STATUS_BANNED", 3)
                  ]
        , testCase "fully qualified protoEnumName" $ do
            PS.protoEnumName (Proxy :: Proxy Status) @?= "Status"
        ]
    , testGroup
        "Aeson.ToJSON / FromJSON for messages (camelCase keys)"
        [ testCase "default Account encodes to {} (proto3 canonical: defaults skipped)" $ do
            let a = defaultAccount
            let v = Aeson.toJSON a
            case v of
              Aeson.Object km -> do
                -- Proto3 canonical JSON omits fields at default value
                -- (empty string for acctName, STATUS_UNSPECIFIED for
                -- acctStatus). The result is an empty object.
                assertBool
                  "acctName key absent (default empty string)"
                  (not (hasKey "acctName" km))
                assertBool
                  "acctStatus key absent (default STATUS_UNSPECIFIED)"
                  (not (hasKey "acctStatus" km))
              _ -> error "expected Object"
        , testCase "non-default Account emits both fields with camelCase keys" $ do
            let a =
                  defaultAccount
                    { accountAcctName = T.pack "ada"
                    , accountAcctStatus = Status'StatusActive
                    }
            let v = Aeson.toJSON a
            case v of
              Aeson.Object km -> do
                assertBool "acctName key present" (hasKey "acctName" km)
                assertBool "acctStatus key present" (hasKey "acctStatus" km)
              _ -> error "expected Object"
        , testCase "Account ToJSON / FromJSON round-trip" $ do
            let a =
                  defaultAccount
                    { accountAcctName = T.pack "alice"
                    , accountAcctStatus = Status'StatusActive
                    }
            let v = Aeson.toJSON a
            case Aeson.fromJSON v of
              Aeson.Success a' -> a' @?= a
              Aeson.Error e -> error ("fromJSON failed: " <> e)
        , testCase "Status enum encodes as its primary name string" $ do
            Aeson.toJSON Status'StatusActive @?= Aeson.String "STATUS_ACTIVE"
            Aeson.toJSON Status'StatusBanned @?= Aeson.String "STATUS_BANNED"
        , testCase "Status enum FromJSON accepts both name and number" $ do
            Aeson.fromJSON (Aeson.String "STATUS_RETIRED") @?= Aeson.Success Status'StatusRetired
            Aeson.fromJSON (Aeson.Number 1) @?= Aeson.Success Status'StatusActive
        , testCase "PackedBag ToJSON / FromJSON round-trip" $ do
            let p = defaultPackedBag {packedBagBagNums = V.fromList [1, 2, 3, 4, 5]}
            case Aeson.fromJSON (Aeson.toJSON p) of
              Aeson.Success p' -> p' @?= p
              Aeson.Error e -> error ("fromJSON failed: " <> e)
        , testCase "PackedBag with empty bagNums encodes to {} (proto3-canonical)" $ do
            let p = defaultPackedBag
            case Aeson.toJSON p of
              Aeson.Object km -> assertBool "no bagNums key" (not (hasKey "bagNums" km))
              _ -> error "expected Object"
        ]
    , testGroup
        "Hashable"
        [ testCase "equal Accounts hash equal" $ do
            let a1 = defaultAccount {accountAcctName = T.pack "x", accountAcctStatus = Status'StatusActive}
                a2 = defaultAccount {accountAcctName = T.pack "x", accountAcctStatus = Status'StatusActive}
            hash a1 @?= hash a2
        , testCase "different Accounts hash differently (sanity)" $ do
            let a1 = defaultAccount {accountAcctName = T.pack "x", accountAcctStatus = Status'StatusActive}
                a2 = defaultAccount {accountAcctName = T.pack "y", accountAcctStatus = Status'StatusActive}
            assertBool
              "names differ -> hashes should differ"
              (hash a1 /= hash a2)
        , testCase "Status hashes match its proto wire number" $ do
            -- The enum Hashable implementation hashes the proto
            -- wire number, so this is observable.
            hashWithSalt 0 Status'StatusUnspecified @?= hashWithSalt 0 (0 :: Int)
            hashWithSalt 0 Status'StatusActive @?= hashWithSalt 0 (1 :: Int)
        , testCase "PackedBag hashes survive vector permutation differences" $ do
            let p1 = defaultPackedBag {packedBagBagNums = V.fromList [1, 2, 3]}
                p2 = defaultPackedBag {packedBagBagNums = V.fromList [3, 2, 1]}
            assertBool
              "vector order matters in the hash"
              (hash p1 /= hash p2)
        ]
    ]


hasKey :: T.Text -> AesonKM.KeyMap Aeson.Value -> Bool
hasKey k = AesonKM.member (AesonKey.fromText k)
