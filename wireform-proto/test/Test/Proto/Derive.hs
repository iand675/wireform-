{-# OPTIONS_GHC -Wno-unused-imports #-}

{- | Round-trip tests for 'Proto.TH.Derive': encode each fixture to bytes,
decode the bytes back, and compare. Exercises bare scalars, the
proto3 default-skip rule, ZigZag and fixed-width 'wireOverride',
an optional nested submessage, and the IDL-bridge entry point
'deriveProtoFromTranslated' for repeated, map, oneof, and enum
shapes.
-}
module Test.Proto.Derive (tests) where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Proto qualified as PD
import Proto qualified as PE
import Test.Proto.Derive.Instances ()
import Test.Proto.Derive.RegressionInstances (
  RegInventory (..),
  RegItem (..),
  defaultRegInventory,
  defaultRegItem,
 )
import Test.Proto.Derive.RegressionTypes (
  BridgeRegInventory (..),
  BridgeRegItem (..),
 )
import Test.Proto.Derive.RichInstances ()
import Test.Proto.Derive.RichTypes (
  Avatar (..),
  Color (..),
  Inventory (..),
  Item (..),
  LooseInventory (..),
  Painting (..),
  Profile (..),
  Tagged (..),
 )
import Test.Proto.Derive.TranslatedInstances ()
import Test.Proto.Derive.TranslatedTypes (AddressT (..), UserT (..))
import Test.Proto.Derive.Types (Address (..), User (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))


tests :: TestTree
tests =
  testGroup
    "Proto.TH.Derive"
    [ testCase "default User round-trips" $ do
        let u = defaultUser
        let bs = PE.encodeMessage u
        bs @?= BS.empty -- proto3 default: every field skipped
        PD.decodeMessage bs @?= Right u
    , testCase "scalar fields round-trip" $ do
        let u =
              defaultUser
                { userId = 42
                , userName = T.pack "alice"
                , userActive = True
                , userScore = 3.14
                , userTagBits = 0xDEADBEEFCAFEBABE
                , userBlob = BS.pack [0, 1, 2, 3, 254, 255]
                }
        roundTrip u
    , testCase "ZigZag override on negative sint32" $ do
        let u = defaultUser {userOffset = -123456}
        roundTrip u
    , testCase "fixed32 override on uint32" $ do
        let u = defaultUser {userPort = 0xCAFEBABE}
        roundTrip u
        -- The fixed32 wire payload always occupies 4 bytes plus 1 tag byte;
        -- assert the encoded length to confirm we picked the right encoding.
        BS.length (PE.encodeMessage u) @?= 5
    , testCase "Maybe Address present round-trips" $ do
        let addr =
              Address
                { addrStreet = T.pack "1 Wireform Way"
                , addrCity = T.pack "Berlin"
                , addrZip = 10119
                }
        let u = defaultUser {userId = 7, userAddr = Just addr}
        roundTrip u
    , testCase "Maybe Address absent skips field" $ do
        let u = defaultUser {userId = 7}
        let bs = PE.encodeMessage u
        -- Tag for field 9 is (9 << 3) | 2 = 74; assert it's NOT in the stream.
        assertBool "address tag should be absent" (74 `BS.notElem` bs)
        PD.decodeMessage bs @?= Right u
    , testCase "deriveProtoFromTranslated: round-trip preserves UserT" $ do
        let u =
              (defaultUserT :: UserT)
                { tuserId = 99
                , tuserName = T.pack "carol"
                , tuserActive = True
                , tuserScore = 2.71
                , tuserTagBits = 0xFEEDFACECAFEBEEF
                , tuserBlob = BS.pack [0xDE, 0xAD, 0xBE, 0xEF]
                , tuserOffset = -98765
                , tuserPort = 0xCAFEBABE
                , tuserAddr =
                    Just
                      AddressT
                        { taddrStreet = T.pack "1 Wireform Way"
                        , taddrCity = T.pack "Berlin"
                        , taddrZip = 10119
                        }
                }
        PD.decodeMessage (PE.encodeMessage u) @?= Right u
    , testCase "deriveProtoFromTranslated: byte-identical to deriveProto" $ do
        let u =
              defaultUser
                { userId = 99
                , userName = T.pack "carol"
                , userActive = True
                , userScore = 2.71
                , userTagBits = 0xFEEDFACECAFEBEEF
                , userBlob = BS.pack [0xDE, 0xAD, 0xBE, 0xEF]
                , userOffset = -98765
                , userPort = 0xCAFEBABE
                , userAddr =
                    Just
                      Address
                        { addrStreet = T.pack "1 Wireform Way"
                        , addrCity = T.pack "Berlin"
                        , addrZip = 10119
                        }
                }
            uT =
              (defaultUserT :: UserT)
                { tuserId = userId u
                , tuserName = userName u
                , tuserActive = userActive u
                , tuserScore = userScore u
                , tuserTagBits = userTagBits u
                , tuserBlob = userBlob u
                , tuserOffset = userOffset u
                , tuserPort = userPort u
                , tuserAddr =
                    fmap
                      ( \a ->
                          AddressT
                            { taddrStreet = addrStreet a
                            , taddrCity = addrCity a
                            , taddrZip = addrZip a
                            }
                      )
                      (userAddr u)
                }
        PE.encodeMessage uT @?= PE.encodeMessage u
    , -- ---------------------------------------------------------------
      -- Enum
      -- ---------------------------------------------------------------
      testCase "Painting (enum field): default round-trips" $ do
        let p = Painting {pTitle = T.empty, pColor = ColRed}
        let bs = PE.encodeMessage p
        bs @?= BS.empty
        PD.decodeMessage bs @?= Right p
    , testCase "Painting (enum field): non-default round-trips" $ do
        let p = Painting {pTitle = T.pack "Composition VIII", pColor = ColBlue}
        PD.decodeMessage (PE.encodeMessage p) @?= Right p
    , testCase "Painting (enum field): zero-valued enum is skipped" $ do
        let p = Painting {pTitle = T.pack "X", pColor = ColRed}
        let bs = PE.encodeMessage p
        -- field 2 (color) has tag byte (2 << 3) | 0 = 16; assert it's
        -- absent because ColRed = 0 is the proto default.
        assertBool
          "color tag should be absent for default enum"
          (16 `BS.notElem` bs)
    , -- ---------------------------------------------------------------
      -- Repeated
      -- ---------------------------------------------------------------
      testCase "Inventory (Vector-repeated submessages): empty round-trips" $ do
        let inv = Inventory {invName = T.empty, invItems = V.empty}
        PD.decodeMessage (PE.encodeMessage inv) @?= Right inv
    , testCase "Inventory (Vector-repeated submessages): three elements" $ do
        let items =
              V.fromList
                [ Item {iName = T.pack "alpha", iCount = 1}
                , Item {iName = T.pack "beta", iCount = 2}
                , Item {iName = T.pack "gamma", iCount = 3}
                ]
            inv = Inventory {invName = T.pack "warehouse", invItems = items}
        PD.decodeMessage (PE.encodeMessage inv) @?= Right inv
    , testCase "LooseInventory (list-repeated strings): preserves order" $ do
        let li =
              LooseInventory
                { liId = 99
                , liTags = [T.pack "a", T.pack "b", T.pack "c"]
                }
        PD.decodeMessage (PE.encodeMessage li) @?= Right li
    , -- ---------------------------------------------------------------
      -- Map
      -- ---------------------------------------------------------------
      testCase "Tagged (map<string, string>): empty map round-trips" $ do
        let t = Tagged {tagName = T.empty, tagAttrs = Map.empty}
        let bs = PE.encodeMessage t
        bs @?= BS.empty
        PD.decodeMessage bs @?= Right t
    , testCase "Tagged (map<string, string>): three entries round-trip" $ do
        let attrs =
              Map.fromList
                [ (T.pack "color", T.pack "red")
                , (T.pack "size", T.pack "large")
                , (T.pack "shape", T.pack "round")
                ]
            t = Tagged {tagName = T.pack "demo", tagAttrs = attrs}
        PD.decodeMessage (PE.encodeMessage t) @?= Right t
    , -- ---------------------------------------------------------------
      -- Oneof
      -- ---------------------------------------------------------------
      testCase "Profile (oneof): unset oneof round-trips" $ do
        let p = Profile {profName = T.empty, profAvatar = Nothing}
        let bs = PE.encodeMessage p
        bs @?= BS.empty
        PD.decodeMessage bs @?= Right p
    , testCase "Profile (oneof): AvatarUrl variant round-trips" $ do
        let p =
              Profile
                { profName = T.pack "ada"
                , profAvatar = Just (AvatarUrl (T.pack "https://example.test/x.png"))
                }
        PD.decodeMessage (PE.encodeMessage p) @?= Right p
    , testCase "Profile (oneof): AvatarBlob variant round-trips" $ do
        let p =
              Profile
                { profName = T.pack "grace"
                , profAvatar = Just (AvatarBlob (BS.pack [0xDE, 0xAD, 0xBE, 0xEF]))
                }
        PD.decodeMessage (PE.encodeMessage p) @?= Right p
    , testCase "Profile (oneof): AvatarSeed variant round-trips" $ do
        let p =
              Profile
                { profName = T.pack "joan"
                , profAvatar = Just (AvatarSeed 42)
                }
        PD.decodeMessage (PE.encodeMessage p) @?= Right p
    , testCase "Profile (oneof): later variant wins on the wire" $ do
        -- Concatenate two oneof field encodings; per proto3 spec the
        -- last-wins, so encoding a Seed-only profile and decoding a
        -- Url+Seed concatenation should yield the Seed variant.
        let pUrl =
              Profile
                { profName = T.empty
                , profAvatar = Just (AvatarUrl (T.pack "old"))
                }
            pSeed =
              Profile
                { profName = T.empty
                , profAvatar = Just (AvatarSeed 7)
                }
            combined = PE.encodeMessage pUrl `BS.append` PE.encodeMessage pSeed
        PD.decodeMessage combined @?= Right pSeed
    , -- ---------------------------------------------------------------
      -- Byte-equivalence regression: deriveProtoFromTranslated vs. loadProto
      -- ---------------------------------------------------------------
      --
      -- 'Proto.TH.loadProto' is the long-standing proto code generator
      -- and the implementation of record. The new IDL bridge
      -- ('deriveProtoFromTranslated') must produce wire bytes that
      -- match it for the same logical message; otherwise downstream
      -- consumers that switch from one to the other would observe
      -- silent corruption.

      testCase "regression: empty RegInventory matches BridgeRegInventory bytes" $ do
        let pBridge = BridgeRegInventory {briName = T.empty, briItems = V.empty}
            pProto = defaultRegInventory
        PE.encodeMessage pBridge @?= PE.encodeMessage pProto
    , testCase "regression: name-only RegInventory matches" $ do
        let pBridge =
              BridgeRegInventory
                { briName = T.pack "warehouse-7"
                , briItems = V.empty
                }
            pProto = defaultRegInventory {regInventoryReginvName = T.pack "warehouse-7"}
        PE.encodeMessage pBridge @?= PE.encodeMessage pProto
    , testCase "regression: repeated submessages produce identical bytes" $ do
        let bridgeItems =
              V.fromList
                [ BridgeRegItem (T.pack "alpha") 1
                , BridgeRegItem (T.pack "beta") 2
                , BridgeRegItem (T.pack "gamma") 3
                ]
            protoItems =
              V.fromList
                [ defaultRegItem {regItemRegiName = T.pack "alpha", regItemRegiCount = 1}
                , defaultRegItem {regItemRegiName = T.pack "beta", regItemRegiCount = 2}
                , defaultRegItem {regItemRegiName = T.pack "gamma", regItemRegiCount = 3}
                ]
            pBridge =
              BridgeRegInventory
                { briName = T.pack "depot"
                , briItems = bridgeItems
                }
            pProto =
              defaultRegInventory
                { regInventoryReginvName = T.pack "depot"
                , regInventoryReginvItems = protoItems
                }
        PE.encodeMessage pBridge @?= PE.encodeMessage pProto
    , testCase "regression: single RegItem matches BridgeRegItem bytes" $ do
        let pBridge = BridgeRegItem (T.pack "widget") 99
            pProto = defaultRegItem {regItemRegiName = T.pack "widget", regItemRegiCount = 99}
        PE.encodeMessage pBridge @?= PE.encodeMessage pProto
    ]


defaultUserT :: UserT
defaultUserT =
  UserT
    { tuserId = 0
    , tuserName = T.empty
    , tuserActive = False
    , tuserScore = 0
    , tuserTagBits = 0
    , tuserBlob = BS.empty
    , tuserOffset = 0
    , tuserPort = 0
    , tuserAddr = Nothing
    }


defaultUser :: User
defaultUser =
  User
    { userId = 0
    , userName = T.empty
    , userActive = False
    , userScore = 0
    , userTagBits = 0
    , userBlob = BS.empty
    , userOffset = 0
    , userPort = 0
    , userAddr = Nothing
    }


roundTrip :: User -> IO ()
roundTrip u = PD.decodeMessage (PE.encodeMessage u) @?= Right u
