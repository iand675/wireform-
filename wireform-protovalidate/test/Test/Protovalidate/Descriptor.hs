{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Tests for reading @buf.validate@ rules out of a compiled descriptor: build
a 'FileDescriptorProto' carrying the buf.validate extension (#1159) on field
and message options, then extract and run the rules.
-}
module Test.Protovalidate.Descriptor (tests) where

import CEL (Duration (..), Timestamp (..), Value (..), celMapFromList, celMapLookup)
import Data.Bits (shiftL, shiftR, (.|.))
import Data.ByteString qualified as BS
import Data.Int (Int32)
import Data.List (sort)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Word (Word32, Word64, Word8)
import GHC.Generics (Generic)
import Language.Haskell.TH (recover)
import Proto.Decode (UnknownField (..))
import Proto.Dynamic (DynamicValue (DynGroup), decodeDynamic, emptyDynamic, setDynamicField)
import Proto.Google.Protobuf.Descriptor
import Proto.IDL.Parser (parseProtoFile)
import Proto.IDL.Print (printProtoFile)
import Protovalidate
import Protovalidate.TH (compileMessageValidator)
import Test.Protovalidate.UserProto (descriptorCollectionProto, descriptorContextProto, descriptorScalarProto, editionPresenceProto, explicitDefaultsProto, messageCelProto, messageOneofProto, presenceProto, recursiveProto, shorthandProto)
import Test.Syd


----------------------------------------------------------------------
-- Tiny protobuf wire encoder (to synthesize buf.validate extension bytes)
----------------------------------------------------------------------

varint :: Word64 -> [Word8]
varint n
  | n < 0x80 = [fromIntegral n]
  | otherwise = fromIntegral (n .|. 0x80) : varint (n `shiftR` 7)


tag :: Int -> Word64 -> [Word8]
tag fn wt = varint (fromIntegral fn `shiftL` 3 .|. wt)


lenField :: Int -> [Word8] -> [Word8]
lenField fn bs = tag fn 2 ++ varint (fromIntegral (length bs)) ++ bs


varintField :: Int -> Word64 -> [Word8]
varintField fn v = tag fn 0 ++ varint v


fixed32Field :: Int -> Word32 -> [Word8]
fixed32Field fn v = tag fn 5 ++ fixed32 v


fixed32 :: Word32 -> [Word8]
fixed32 v = map (fromIntegral . shiftR v) [0, 8, 16, 24]


strField :: Int -> Text -> [Word8]
strField fn s = lenField fn (BS.unpack (TE.encodeUtf8 s))


-- Constraint { id=1, message=2, expression=3 }
constraint :: Text -> Text -> [Word8]
constraint cid expr = strField 1 cid ++ strField 3 expr


-- FieldConstraints for a string field with min_len=3, required, and one custom CEL.
stringFieldConstraints :: [Word8]
stringFieldConstraints =
  lenField 14 (varintField 2 3) -- string -> StringRules { min_len = 3 }
    ++ varintField 25 1 -- required = true
    ++ lenField 23 (constraint "f.prefix_x" "this.startsWith('x')") -- cel


messageConstraintsBytes :: [Word8]
messageConstraintsBytes = lenField 3 (constraint "m.always" "false") -- cel


----------------------------------------------------------------------
-- Build a descriptor
----------------------------------------------------------------------

userDescriptor :: FileDescriptorProto
userDescriptor =
  defaultFileDescriptorProto
    { fdpName = "user.proto"
    , fdpMessageType = V.singleton userMsg
    }
  where
    userMsg =
      defaultDescriptorProto
        { dpName = "User"
        , dpField = V.singleton nameField
        , dpOptions =
            Just defaultMessageOptions {moUnknownFields = [UnknownLenDelim 1159 (BS.pack messageConstraintsBytes)]}
        }
    nameField =
      defaultFieldDescriptorProto
        { fdpFieldName = "name"
        , fdpFieldNumber = 1
        , fdpFieldType = 9 -- TYPE_STRING
        , fdpFieldOptions =
            Just defaultFieldOptions {fldoUnknownFields = [UnknownLenDelim 1159 (BS.pack stringFieldConstraints)]}
        }


userRules :: MessageRules
userRules = either (error . show) id (messageRulesFromDescriptor userDescriptor "User")


scalarDescriptor :: FileDescriptorProto
scalarDescriptor =
  defaultFileDescriptorProto
    { fdpSyntax = "proto3"
    , fdpMessageType = V.singleton defaultDescriptorProto
        { dpName = "ScalarRules"
        , dpField = V.fromList
            [ field "at" 1 11 22 (lenField 5 (varintField 1 1000 ++ varintField 2 500))
            , field "ttl" 2 11 21 (lenField 4 (varintField 1 (signed (-60)) ++ varintField 2 (signed (-10))))
            , field "recent" 3 11 22 (lenField 9 (varintField 2 50))
            , field "choice" 4 14 16 (lenField 3 [1] ++ varintField 4 2)
            , field "payload" 5 12 15 (strField 5 "a" ++ strField 8 "ab" ++ strField 8 "ax" ++ strField 9 "ax")
            , field "delta" 6 15 11
                (fixed32Field 3 (word32 (-2)) ++ lenField 6 (fixed32 (word32 (-2)) ++ fixed32 (word32 (-3))))
            , field "fraction" 7 2 1 (fixed32Field 1 0x3dcccccd)
            , field "real" 8 1 2 (varintField 8 1)
            , field "label" 9 9 14 (varintField 20 2)
            , field "network" 10 9 14 (varintField 27 1)
            ]
        }
    }
  where
    signed :: Int -> Word64
    signed = fromIntegral
    word32 :: Int32 -> Word32
    word32 = fromIntegral
    field name number fieldType kind bytes =
      defaultFieldDescriptorProto
        { fdpFieldName = name
        , fdpFieldNumber = number
        , fdpFieldLabel = 1
        , fdpFieldType = fieldType
        , fdpFieldOptions =
            Just defaultFieldOptions {fldoUnknownFields = [UnknownLenDelim 1159 (BS.pack (lenField kind bytes))]}
        }


scalarValidator :: Value -> [Violation]
scalarValidator = $(compileMessageValidator descriptorScalarProto "ScalarRules")


collectionDescriptor :: FileDescriptorProto
collectionDescriptor =
  defaultFileDescriptorProto
    { fdpPackage = "fixture.v1"
    , fdpSyntax = "proto3"
    , fdpMessageType = V.fromList [child, collection]
    }
  where
    child = defaultDescriptorProto
      { dpName = "Child"
      , dpField = V.singleton (field "code" 1 1 9 "" [varintField 25 1])
      }
    collection = defaultDescriptorProto
      { dpName = "CollectionRules"
      , dpOneofDecl = V.singleton defaultOneofDescriptorProto
          { odpName = "identity"
          , odpOptions = Just defaultOneofOptions
              { oneofoUnknownFields = [UnknownLenDelim 1159 (BS.pack (varintField 1 1))] }
          }
      , dpNestedType = V.fromList
          [mapEntry "ScoresEntry" 5 "", mapEntry "LookupEntry" 11 childType, mapEntry "IgnoredValuesEntry" 11 childType]
      , dpField = V.fromList
          [ field "labels" 1 3 9 ""
              [ lenField 18 (varintField 1 1)
              , lenField 18 (lenField 4 (lenField 14 (varintField 2 2)))
              , lenField 18 (lenField 4 (lenField 23 itemCel))
              ]
          , field "scores" 2 3 11 (entryType "ScoresEntry")
              [lenField 19 (varintField 1 1 ++ lenField 4 (lenField 14 (varintField 2 2)) ++ lenField 5 (lenField 3 (varintField 4 0)))]
          , field "children" 3 3 11 childType []
          , field "lookup" 4 3 11 (entryType "LookupEntry") []
          , field "ignored_items" 5 3 11 childType [lenField 18 (lenField 4 (varintField 27 3))]
          , field "ignored_values" 6 3 11 (entryType "IgnoredValuesEntry")
              [lenField 19 (lenField 5 (varintField 27 3 ++ lenField 23 (constraint "ignored" "this +")))]
          , field "tail" 7 1 9 "" [lenField 14 (varintField 2 2)]
          , (field "alias" 8 1 9 "" []) {fdpFieldOneofIdx = 0}
          , (field "code" 9 1 5 "" []) {fdpFieldOneofIdx = 0}
          ]
      }
    childType = ".fixture.v1.Child"
    entryType name = ".fixture.v1.CollectionRules." <> name
    itemCel = constraint "item.prefix" "this.startsWith('a')" ++ strField 2 "item must start with a"
    mapEntry name valueType typeName = defaultDescriptorProto
      { dpName = name
      , dpOptions = Just defaultMessageOptions {moMapEntry = True}
      , dpField = V.fromList [field "key" 1 1 9 "" [], field "value" 2 1 valueType typeName []]
      }
    field name number label fieldType typeName options = defaultFieldDescriptorProto
      { fdpFieldName = name
      , fdpFieldNumber = number
      , fdpFieldLabel = label
      , fdpFieldType = fieldType
      , fdpFieldTypeName = typeName
      , fdpFieldOptions =
          Just defaultFieldOptions {fldoUnknownFields = map (UnknownLenDelim 1159 . BS.pack) options}
      }


collectionValidator :: Value -> [Violation]
collectionValidator = $(compileMessageValidator descriptorCollectionProto "CollectionRules")


contextDescriptor :: FileDescriptorProto
contextDescriptor =
  defaultFileDescriptorProto
    { fdpPackage = "fixture.v1"
    , fdpSyntax = "proto3"
    , fdpEnumType = V.singleton (kind 1)
    , fdpMessageType = V.singleton defaultDescriptorProto
        { dpName = "ContextRules"
        , dpEnumType = V.singleton (kind (-2))
        , dpNestedType = V.fromList
            [entry "LookupEntry" 14 local, entry "HeadersEntry" 9 ""]
        , dpField = V.fromList
            [ field "local" 1 1 14 local defined
            , field "global" 2 1 14 ".fixture.v1.Kind" defined
            , field "kinds" 3 3 14 local (lenField 18 (lenField 4 defined))
            , field "lookup" 4 3 11 (scope <> "LookupEntry") (lenField 19 (lenField 5 defined))
            , field "header" 5 1 9 "" (regex 1 [])
            , field "header_value" 6 1 9 "" (regex 2 [])
            , field "loose" 7 3 9 "" (lenField 18 (lenField 4 (regex 1 relaxed)))
            , field "headers" 8 3 11 (scope <> "HeadersEntry")
                (lenField 19 (lenField 4 (regex 1 []) ++ lenField 5 (regex 2 relaxed)))
            ]
        }
    }
  where
    scope = ".fixture.v1.ContextRules."
    local = scope <> "Kind"
    defined = lenField 16 (varintField 2 1)
    relaxed = varintField 25 0
    regex code flags = lenField 14 (varintField 24 code ++ flags)
    kind number = defaultEnumDescriptorProto
      { edpName = "Kind"
      , edpValue = V.fromList [defaultEnumValueDescriptorProto, defaultEnumValueDescriptorProto {evdpNumber = number}]
      }
    entry name valueType typeName = defaultDescriptorProto
      { dpName = name
      , dpOptions = Just defaultMessageOptions {moMapEntry = True}
      , dpField = V.fromList [field "key" 1 1 9 "" [], field "value" 2 1 valueType typeName []]
      }
    field name number label fieldType typeName bytes = defaultFieldDescriptorProto
      { fdpFieldName = name
      , fdpFieldNumber = number
      , fdpFieldLabel = label
      , fdpFieldType = fieldType
      , fdpFieldTypeName = typeName
      , fdpFieldOptions = Just defaultFieldOptions {fldoUnknownFields = [UnknownLenDelim 1159 (BS.pack bytes)]}
      }


contextValidator :: Value -> [Violation]
contextValidator = $(compileMessageValidator descriptorContextProto "ContextRules")


shorthandDescriptor :: FileDescriptorProto
shorthandDescriptor =
  defaultFileDescriptorProto
    { fdpPackage = "fixture.v1"
    , fdpSyntax = "proto3"
    , fdpMessageType = V.singleton defaultDescriptorProto
        { dpName = "Shorthand"
        , dpNestedType = V.singleton defaultDescriptorProto
            { dpName = "ScoresEntry"
            , dpOptions = Just defaultMessageOptions {moMapEntry = True}
            , dpField = V.fromList [field "key" 1 1 9 "" [], field "value" 2 1 5 "" []]
            }
        , dpField = V.fromList
            [ field "code" 1 1 5 ""
                (lenField 23 (constraint "code.explicit" "this >= 1" ++ strField 2 "code is too small")
                  ++ expr "this > 0" ++ expr "this == 2 ? '' : 'code must equal 2'")
            , field "labels" 2 3 9 "" (lenField 18 (lenField 4 (expr "this.size() > 1")))
            , field "scores" 3 3 11 ".fixture.v1.Shorthand.ScoresEntry"
                (lenField 19 (lenField 4 (expr "this != 'bad'") ++ lenField 5 (expr "this > 0")))
            , field "ignored" 4 1 5 "" (varintField 27 3 ++ expr "this +")
            ]
        , dpOptions = Just defaultMessageOptions
            { moUnknownFields =
                [ option
                    (lenField 3 (constraint "message.explicit" "this.code == 2")
                      ++ strField 5 "this.code != 0" ++ strField 5 "this.code >= 2")
                ]
            }
        }
    }
  where
    expr = strField 29
    option = UnknownLenDelim 1159 . BS.pack
    field name number label fieldType typeName bytes = defaultFieldDescriptorProto
      { fdpFieldName = name
      , fdpFieldNumber = number
      , fdpFieldLabel = label
      , fdpFieldType = fieldType
      , fdpFieldTypeName = typeName
      , fdpFieldOptions = Just defaultFieldOptions {fldoUnknownFields = [option bytes]}
      }


shorthandValidator :: Value -> [Violation]
shorthandValidator = $(compileMessageValidator shorthandProto "Shorthand")


presenceDescriptor :: FileDescriptorProto
presenceDescriptor =
  defaultFileDescriptorProto
    { fdpSyntax = "proto3"
    , fdpMessageType = V.singleton defaultDescriptorProto
        { dpName = "Presence"
        , dpOneofDecl = V.fromList
            [defaultOneofDescriptorProto {odpName = "selection"}, defaultOneofDescriptorProto {odpName = "_explicit"}]
        , dpField = V.fromList
            [ presenceField "implicit" 1 9 required
            , (presenceField "explicit" 2 9 required) {fdpFieldProto3Optional = True, fdpFieldOneofIdx = 1}
            , (presenceField "entries" 3 9 required) {fdpFieldLabel = 3}
            , (presenceField "counts" 4 11 required) {fdpFieldLabel = 3, fdpFieldTypeName = ".Presence.CountsEntry"}
            , presenceField "ordinary" 5 9 minLength
            , (presenceField "selected" 6 9 minLength) {fdpFieldOneofIdx = 0}
            , presenceField "signed_zero" 7 1 required
            ]
        , dpNestedType = V.singleton defaultDescriptorProto
            { dpName = "CountsEntry"
            , dpOptions = Just defaultMessageOptions {moMapEntry = True}
            , dpField = V.fromList
                [ presenceField "key" 1 9 []
                , presenceField "value" 2 5 []
                ]
            }
        }
    }
  where
    required = varintField 25 1
    minLength = lenField 14 (varintField 2 1)


editionPresenceDescriptor :: FileDescriptorProto
editionPresenceDescriptor =
  defaultFileDescriptorProto
    { fdpSyntax = "editions"
    , fdpEdition = "2023"
    , fdpOptions = Just defaultFileOptions {foUnknownFields = [UnknownLenDelim 50 (BS.pack [8, 2])]}
    , fdpMessageType = V.singleton defaultDescriptorProto
        { dpName = "EditionPresence"
        , dpField = V.fromList
            [ presenceField "inherited" 1 5 required
            , explicit
                { fdpFieldOptions =
                    fmap
                      (\o -> o {fldoUnknownFields = UnknownLenDelim 21 (BS.pack [8, 1]) : fldoUnknownFields o})
                      (fdpFieldOptions explicit)
                }
            , presenceField "ordinary" 3 5 (lenField 3 (varintField 4 0))
            ]
        }
    }
  where
    required = varintField 25 1
    explicit = presenceField "explicit" 2 5 required


presenceField :: Text -> Int32 -> Int32 -> [Word8] -> FieldDescriptorProto
presenceField name number fieldType bytes =
  defaultFieldDescriptorProto
    { fdpFieldName = name
    , fdpFieldNumber = number
    , fdpFieldType = fieldType
    , fdpFieldOptions = Just defaultFieldOptions
        {fldoUnknownFields = [UnknownLenDelim 1159 (BS.pack bytes)]}
    }


presenceValidator :: Value -> [Violation]
presenceValidator = $(compileMessageValidator presenceProto "Presence")


editionPresenceValidator :: Value -> [Violation]
editionPresenceValidator = $(compileMessageValidator editionPresenceProto "EditionPresence")


projectionDescriptor :: FileDescriptorProto
projectionDescriptor =
  defaultFileDescriptorProto
    { fdpPackage = "fixture.v1"
    , fdpSyntax = "proto3"
    , fdpMessageType = V.fromList
        [ defaultDescriptorProto
            { dpName = "Projection"
            , dpOptions = messageCelOptions
                [ "this.code == 0 && !has(this.code)"
                , "this.optional_code == 0 && has(this.optional_code) == this.expect_present"
                , "this.chosen == 0 && has(this.chosen) == this.expect_present"
                , "this.child.next.next.count == 0 && this.child.count == 0"
                , "has(this.children) == (this.children.size() > 0) && this.children.all(c, c.count == 0)"
                , "has(this.lookup) == (this.lookup.size() > 0) && this.lookup.all(k, this.lookup[k].count == 0)"
                ]
            , dpField = V.fromList
                [ presenceField "code" 1 5 (varintField 27 3)
                , (presenceField "optional_code" 2 5 []) {fdpFieldProto3Optional = True, fdpFieldOneofIdx = 1}
                , (childField "children" 3 (lenField 18 (lenField 4 (strField 29 "this.count == 0")))) {fdpFieldLabel = 3}
                , (presenceField "lookup" 4 11 []) {fdpFieldLabel = 3, fdpFieldTypeName = ".fixture.v1.Projection.LookupEntry"}
                , childField "child" 5 (strField 29 "this.count == 0")
                , (presenceField "chosen" 6 5 []) {fdpFieldOneofIdx = 0}
                , presenceField "expect_present" 7 8 []
                ]
            , dpOneofDecl = V.fromList
                [defaultOneofDescriptorProto {odpName = "selection"}, defaultOneofDescriptorProto {odpName = "_optional_code"}]
            , dpNestedType = V.singleton defaultDescriptorProto
                { dpName = "LookupEntry"
                , dpOptions = Just defaultMessageOptions {moMapEntry = True}
                , dpField = V.fromList [presenceField "key" 1 9 [], childField "value" 2 []]
                }
            }
        , defaultDescriptorProto
            { dpName = "Child"
            , dpField = V.fromList [presenceField "count" 1 5 [], childField "next" 2 []]
            }
        ]
    }
  where
    childField name number bytes = (presenceField name number 11 bytes) {fdpFieldTypeName = ".fixture.v1.Child"}


explicitDefaultsDescriptor :: FileDescriptorProto
explicitDefaultsDescriptor =
  defaultFileDescriptorProto
    { fdpMessageType = V.singleton defaultDescriptorProto
        { dpName = "ExplicitDefaults"
        , dpOptions = messageCelOptions
            [ "this.count == 7 && this.state == 2 && this.label == 'é😀' && this.data == b'\\377\\303\\251'"
            , "has(this.count) == this.expect_present && has(this.state) == this.expect_present"
            , "1.0 / this.zero < 0.0 && this.limit > 1e300"
            , "this.chosen == 2 && has(this.chosen) == this.expect_present"
            ]
        , dpField = V.fromList
            [ field "count" 1 5 "7"
            , (field "state" 2 14 "SECOND") {fdpFieldTypeName = ".ExplicitDefaults.State"}
            , field "label" 3 9 "é😀"
            , (presenceField "data" 4 12 (lenField 15 (lenField 5 [255])))
                {fdpFieldDefault = "\\377\\303\\251", fdpFieldLabel = 1}
            , field "expect_present" 5 8 ""
            , field "zero" 6 1 "-0"
            , field "limit" 7 1 "inf"
            , (field "chosen" 8 14 "SECOND")
                {fdpFieldTypeName = ".ExplicitDefaults.State", fdpFieldOneofIdx = 0}
            ]
        , dpOneofDecl = V.singleton defaultOneofDescriptorProto {odpName = "selection"}
        , dpEnumType = V.singleton defaultEnumDescriptorProto
            { edpName = "State"
            , edpValue = V.fromList
                [ defaultEnumValueDescriptorProto {evdpName = "FIRST", evdpNumber = 1}
                , defaultEnumValueDescriptorProto {evdpName = "SECOND", evdpNumber = 2}
                ]
            }
        }
    }
  where
    field name number fieldType defaultValue =
      (presenceField name number fieldType []) {fdpFieldDefault = defaultValue, fdpFieldLabel = 1}


messageCelOptions :: [Text] -> Maybe MessageOptions
messageCelOptions expressions =
  Just defaultMessageOptions
    {moUnknownFields = [UnknownLenDelim 1159 (BS.pack (concatMap (strField 5) expressions))]}


projectionValidator :: Value -> [Violation]
projectionValidator = $(compileMessageValidator messageCelProto "Projection")


explicitDefaultsValidator :: Value -> [Violation]
explicitDefaultsValidator = $(compileMessageValidator explicitDefaultsProto "ExplicitDefaults")


recursiveDescriptor :: FileDescriptorProto
recursiveDescriptor =
  defaultFileDescriptorProto
    { fdpPackage = "fixture.recursive"
    , fdpSyntax = "proto3"
    , fdpMessageType = V.fromList [node, peer]
    }
  where
    node = defaultDescriptorProto
      { dpName = "Node"
      , dpOneofDecl = V.singleton defaultOneofDescriptorProto {odpName = "edge"}
      , dpField = V.fromList
          [ presenceField "code" 1 5 (lenField 3 (varintField 5 0))
          , messageField "next" 2 "Node" []
          , (messageField "children" 3 "Node" []) {fdpFieldLabel = 3}
          , (messageField "lookup" 4 "Node.LookupEntry" []) {fdpFieldLabel = 3}
          , messageField "peer" 5 "Peer" []
          , messageField "ignored" 6 "Node" ignored
          , (messageField "ignored_items" 7 "Node" (lenField 18 (lenField 4 ignored))) {fdpFieldLabel = 3}
          , (messageField "ignored_values" 8 "Node.IgnoredValuesEntry" (lenField 19 (lenField 5 ignored))) {fdpFieldLabel = 3}
          , (messageField "selected" 9 "Node" []) {fdpFieldOneofIdx = 0}
          ]
      , dpNestedType = V.fromList [mapEntry "LookupEntry", mapEntry "IgnoredValuesEntry"]
      }
    peer = defaultDescriptorProto
      { dpName = "Peer"
      , dpField = V.singleton (messageField "node" 1 "Node" [])
      }
    mapEntry name = defaultDescriptorProto
      { dpName = name
      , dpOptions = Just defaultMessageOptions {moMapEntry = True}
      , dpField = V.fromList [presenceField "key" 1 9 [], messageField "value" 2 "Node" []]
      }
    messageField name number ty options =
      (presenceField name number 11 options) {fdpFieldTypeName = ".fixture.recursive." <> ty}
    ignored = varintField 27 3


recursiveNodeValidator :: Value -> [Violation]
recursiveNodeValidator = $(compileMessageValidator recursiveProto "Node")


recursivePeerValidator :: Value -> [Violation]
recursivePeerValidator = $(compileMessageValidator recursiveProto "Peer")


messageOneofDescriptor :: [([Text], Bool)] -> FileDescriptorProto
messageOneofDescriptor groups =
  defaultFileDescriptorProto
    { fdpSyntax = "proto3"
    , fdpMessageType = V.singleton defaultDescriptorProto
        { dpName = "Exclusive"
        , dpOptions = Just defaultMessageOptions
            { moUnknownFields = [UnknownLenDelim 1159 (BS.pack (concatMap groupOption groups))]
            }
        , dpField = V.fromList
            [ presenceField "name" 1 9 (lenField 14 (varintField 2 2))
            , (presenceField "labels" 2 9 (lenField 18 (varintField 1 1))) {fdpFieldLabel = 3}
            , (presenceField "flag" 3 8 []) {fdpFieldProto3Optional = True, fdpFieldOneofIdx = 0}
            , (presenceField "entries" 4 11 (lenField 19 (varintField 1 1))) {fdpFieldLabel = 3, fdpFieldTypeName = ".Exclusive.EntriesEntry"}
            , presenceField "status" 5 9 (lenField 14 (varintField 2 1) ++ varintField 27 0)
            , presenceField "region" 6 13 []
            ]
        , dpOneofDecl = V.singleton defaultOneofDescriptorProto {odpName = "_flag"}
        , dpNestedType = V.singleton defaultDescriptorProto
            { dpName = "EntriesEntry"
            , dpOptions = Just defaultMessageOptions {moMapEntry = True}
            , dpField = V.fromList [presenceField "key" 1 9 [], presenceField "value" 2 5 []]
            }
        }
    }
  where
    groupOption (members, required) =
      lenField 4 (concatMap (strField 1) members ++ varintField 2 (if required then 1 else 0))


messageOneofValidator :: Value -> [Violation]
messageOneofValidator = $(compileMessageValidator messageOneofProto "Exclusive")


malformedMessageOneofRejected :: Bool
malformedMessageOneofRejected =
  $( recover [|True|] $
      compileMessageValidator
        "syntax = \"proto3\"; message Bad { string x = 1; option (buf.validate.message).oneof = { fields: [\"x\", \"x\"] }; }"
        "Bad"
        >> [|False|]
   )


newtype OptionalInput = OptionalInput {optionalValue :: Maybe Value}
  deriving stock (Generic)
  deriving anyclass (ToCel)


malformedShorthandRejected :: Bool
malformedShorthandRejected =
  $( recover [|True|] $
      compileMessageValidator
        "syntax = \"proto3\"; message Bad { int32 code = 1 [(buf.validate.field).cel_expression = \"this +\"]; }"
        "Bad"
        >> [|False|]
   )


msg :: [(Text, Value)] -> Value
msg fs = VMap (celMapFromList [(VString k, v) | (k, v) <- fs])


ids :: [Violation] -> [Text]
ids = sort . map violationConstraintId


tests :: Spec
tests =
  describe
    "descriptor (buf.validate extension #1159)"
    $ sequence_
      [ it "rules are extracted from FieldOptions/MessageOptions" $
          (any ((== "name") . fst) (mrFields userRules)) `shouldBe` True
      , it "standard rule + field CEL + message CEL all fire" $
          ids (validate (msg [("name", VString "ab")]) userRules)
            `shouldBe` sort ["string.min_len", "f.prefix_x", "m.always"]
      , it "a conforming value only trips the always-false message rule" $
          ids (validate (msg [("name", VString "xander")]) userRules)
            `shouldBe` ["m.always"]
      , it "scalar bounds and formats agree across descriptor, source, and TH" $ do
          descriptor <- either (fail . show) pure (messageRulesFromDescriptor scalarDescriptor "ScalarRules")
          textRules <- either (fail . show) pure (parseProtoRules descriptorScalarProto)
          source <- maybe (fail "missing ScalarRules") pure (lookup "ScalarRules" textRules)
          let validFields =
                  [ ("at", VTimestamp (Timestamp 1000 501))
                  , ("ttl", VDuration (Duration (-60) (-10)))
                  , ("choice", VInt 1)
                  , ("payload", VBytes "ab")
                  , ("delta", VInt (-2))
                  , ("fraction", VDouble 0.10000000149011612)
                  , ("real", VDouble 0)
                  , ("label", VString "é")
                  , ("network", VString "127.0.0.1/24")
                  ]
              valid = msg validFields
              invalid =
                msg
                  [ ("at", VTimestamp (Timestamp 1000 500))
                  , ("ttl", VDuration (Duration (-60) (-9)))
                  , ("choice", VInt 2)
                  , ("payload", VBytes "ax")
                  , ("delta", VInt (-1))
                  , ("fraction", VDouble 0.1)
                  , ("real", VDouble (1 / 0))
                  , ("label", VString "a")
                  , ("network", VString "127.0.0.1")
                  ]
              check validator = do
                validator valid `shouldBe` []
                validator invalid `shouldBe` validate invalid source
              now = Timestamp 2000 0
              validateRecent rules nanos =
                validateAt now (msg (("recent", VTimestamp (Timestamp 2000 nanos)) : validFields)) rules
          ids (validate invalid source)
            `shouldBe` sort
              [ "timestamp.gt", "duration.lte", "enum.in", "enum.not_in", "bytes.not_in"
              , "sfixed32.lte", "sfixed32.in", "float.const", "double.finite", "string.len_bytes"
              , "string.ipv4_with_prefixlen"
              ]
          map (frRules . snd) (mrFields descriptor) `shouldBe` map (frRules . snd) (mrFields source)
          mapM_ check [(\input -> validate input source), (\input -> validate input descriptor), scalarValidator]
          validateRecent descriptor 50 `shouldBe` []
          validateRecent descriptor 51 `shouldBe` validateRecent source 51
          ids (validateRecent descriptor 51) `shouldBe` ["timestamp.within"]
      , it "validates collections, nested values, and required oneofs across descriptor, source, and TH" $ do
          descriptor <- either (fail . show) pure (messageRulesFromDescriptor collectionDescriptor "fixture.v1.CollectionRules")
          sourceRules <- either (fail . show) pure (parseProtoRules descriptorCollectionProto)
          source <- maybe (fail "missing CollectionRules") pure (lookup "CollectionRules" sourceRules)
          let ignored =
                [ ("ignored_items", VList (V.singleton (msg [])))
                , ("ignored_values", msg [("a", msg [])])
                ]
              child = msg [("code", VString "ok")]
              valid =
                msg $
                  [ ("labels", VList (V.singleton (VString "ab")))
                  , ("scores", msg [("ab", VInt 1)])
                  , ("children", VList (V.singleton child))
                  , ("lookup", msg [("a", child)])
                  , ("tail", VString "ok")
                  , ("alias", VString "")
                  ] ++ ignored
              invalid =
                msg $
                  [ ("labels", VList (V.singleton (VString "x")))
                  , ("scores", msg [("a", VInt 0)])
                  , ("children", VList (V.singleton (msg [])))
                  , ("lookup", msg [("a", msg [])])
                  , ("tail", VString "x")
                  ] ++ ignored
              absentCollections = msg [("code", VInt 0), ("tail", VString "ok")]
              runtime = validate invalid source
              check validator = do
                validator valid `shouldBe` []
                validator invalid `shouldBe` runtime
                ids (validator absentCollections) `shouldBe` ["map.min_pairs", "repeated.min_items"]
          map (\v -> (violationFieldPath v, violationConstraintId v)) runtime
            `shouldBe` [ ("labels[0]", "string.min_len")
                       , ("labels[0]", "item.prefix")
                       , ("children[0].code", "required")
                       , ("tail", "string.min_len")
                       , ("scores[a]", "string.min_len")
                       , ("scores[a]", "int32.gt")
                       , ("lookup[a].code", "required")
                       , ("", "identity")
                       ]
          mapM_ check [(flip validate source), (flip validate descriptor), collectionValidator]
      , it "resolves scoped enums and honors strict header rules in scalar and collection fields" $ do
          descriptor <- either (fail . show) pure (messageRulesFromDescriptor contextDescriptor "fixture.v1.ContextRules")
          sourceRules <- either (fail . show) pure (parseProtoRules descriptorContextProto)
          source <- maybe (fail "missing ContextRules") pure (lookup "ContextRules" sourceRules)
          let valid =
                msg
                  [ ("local", VInt (-2))
                  , ("global", VInt 1)
                  , ("kinds", VList (V.singleton (VInt (-2))))
                  , ("lookup", msg [("a", VInt (-2))])
                  , ("header", VString ":content-type")
                  , ("header_value", VString "é\t")
                  , ("loose", VList (V.singleton (VString "space allowed")))
                  , ("headers", msg [("content-type", VString "\DEL")])
                  ]
              invalid =
                msg
                  [ ("local", VInt 1)
                  , ("global", VInt (-2))
                  , ("kinds", VList (V.singleton (VInt 1)))
                  , ("lookup", msg [("a", VInt 1)])
                  , ("header", VString "bad header")
                  , ("header_value", VString "\DEL")
                  , ("loose", VList (V.singleton (VString "bad\rname")))
                  , ("headers", msg [("bad header", VString "\NUL")])
                  ]
              violations = validate invalid source
              check validator = do
                validator valid `shouldBe` []
                validator invalid `shouldBe` violations
                ids (validator (msg [("loose", VList (V.singleton (VString ""))), ("header", VString "ok")]))
                  `shouldBe` ["string.well_known_regex"]
          map violationFieldPath violations `shouldBe`
            ["local", "global", "kinds[0]", "header", "header_value", "loose[0]", "lookup[a]", "headers[bad header]", "headers[bad header]"]
          mapM_ check [(flip validate source), (flip validate descriptor), contextValidator]
      , it "reports an unavailable enum definition instead of dropping the rule" $ do
          let missingEnum = contextDescriptor {fdpEnumType = V.empty}
              source = "syntax = \"proto3\"; message Missing { External value = 1 [(buf.validate.field).enum.defined_only = true]; }"
              errorText = "buf.validate enum.defined_only needs the enum definition"
              check = either (`shouldBe` errorText) (const (expectationFailure "Accepted unresolved enum rule"))
          check (messageRulesFromDescriptor missingEnum "ContextRules")
          either (`shouldBe` errorText) (const (expectationFailure "Accepted unresolved source enum rule")) (parseProtoRules source)
      , it "runs shorthand and full CEL in the same order across descriptor, source, and TH" $ do
          descriptor <- either (fail . show) pure (messageRulesFromDescriptor shorthandDescriptor "Shorthand")
          sourceRules <- either (fail . show) pure (parseProtoRules shorthandProto)
          source <- maybe (fail "missing Shorthand") pure (lookup "Shorthand" sourceRules)
          let valid = msg [("code", VInt 2), ("labels", VList (V.singleton (VString "ab"))), ("scores", msg [("ok", VInt 1)]), ("ignored", VInt 1)]
              invalid = msg [("code", VInt 0), ("labels", VList (V.singleton (VString "a"))), ("scores", msg [("bad", VInt 0)])]
              expected =
                [ Violation "code" "this > 0" "\"this > 0\" returned false"
                , Violation "code" "this == 2 ? '' : 'code must equal 2'" "code must equal 2"
                , Violation "code" "code.explicit" "code is too small"
                , Violation "labels[0]" "this.size() > 1" "\"this.size() > 1\" returned false"
                , Violation "scores[bad]" "this != 'bad'" "\"this != 'bad'\" returned false"
                , Violation "scores[bad]" "this > 0" "\"this > 0\" returned false"
                , Violation "" "this.code != 0" "\"this.code != 0\" returned false"
                , Violation "" "this.code >= 2" "\"this.code >= 2\" returned false"
                , Violation "" "message.explicit" "\"this.code == 2\" returned false"
                ]
              check validator = do
                validator valid `shouldBe` []
                validator invalid `shouldBe` expected
          mapM_ check [(flip validate source), (flip validate descriptor), shorthandValidator]
      , it "agrees on defaults and explicit presence across descriptor, source, and TH" $ do
          descriptor <- either (fail . show) pure (messageRulesFromDescriptor presenceDescriptor "Presence")
          sourceRules <- either (fail . show) pure (parseProtoRules presenceProto)
          source <- maybe (fail "missing Presence") pure (lookup "Presence" sourceRules)
          let valid = msg
                [ ("implicit", VString "x"), ("explicit", VString "")
                , ("entries", VList (V.singleton (VString ""))), ("counts", msg [("x", VInt 0)])
                , ("ordinary", VString "x"), ("signed_zero", VDouble (-0.0))
                ]
              zeros = msg
                [ ("implicit", VString ""), ("explicit", VString "")
                , ("entries", VList V.empty), ("counts", msg []), ("selected", VString ""), ("signed_zero", VDouble 0)
                ]
              paths = map (\v -> (violationFieldPath v, violationConstraintId v))
              check validator = do
                validator valid `shouldBe` []
                paths (validator zeros) `shouldBe`
                  [ ("implicit", "required"), ("entries", "required"), ("ordinary", "string.min_len")
                  , ("selected", "string.min_len"), ("signed_zero", "required"), ("counts", "required")
                  ]
                paths (validator (msg [])) `shouldBe`
                  [ ("implicit", "required"), ("explicit", "required"), ("entries", "required")
                  , ("ordinary", "string.min_len"), ("signed_zero", "required"), ("counts", "required")
                  ]
                validator zeros `shouldBe` validate zeros source
                validator (msg []) `shouldBe` validate (msg []) source
          mapM_ check [(flip validate source), (flip validate descriptor), presenceValidator]
      , it "agrees on Editions file defaults and field overrides across descriptor, source, and TH" $ do
          descriptor <- either (fail . show) pure (messageRulesFromDescriptor editionPresenceDescriptor "EditionPresence")
          sourceRules <- either (fail . show) pure (parseProtoRules editionPresenceProto)
          source <- maybe (fail "missing EditionPresence") pure (lookup "EditionPresence" sourceRules)
          let valid = msg [("inherited", VInt 1), ("explicit", VInt 0), ("ordinary", VInt 1)]
              invalid = msg [("inherited", VInt 0), ("explicit", VInt 0)]
              paths = map (\v -> (violationFieldPath v, violationConstraintId v))
              check validator = do
                validator valid `shouldBe` []
                paths (validator invalid) `shouldBe` [("inherited", "required"), ("ordinary", "int32.gt")]
                paths (validator (msg [])) `shouldBe`
                  [("inherited", "required"), ("explicit", "required"), ("ordinary", "int32.gt")]
                validator invalid `shouldBe` validate invalid source
          mapM_ check [(flip validate source), (flip validate descriptor), editionPresenceValidator]
      , it "projects message defaults and presence through recursive fields and collections on all paths" $ do
          descriptor <- either (fail . show) pure (messageRulesFromDescriptor projectionDescriptor "fixture.v1.Projection")
          sourceRules <- either (fail . show) pure (parseProtoRules messageCelProto)
          source <- maybe (fail "missing Projection") pure (lookup "Projection" sourceRules)
          let empty = msg []
              zeros = msg [("code", VInt 0), ("children", VList V.empty), ("lookup", empty)]
              present = msg
                [ ("optional_code", VInt 0), ("chosen", VInt 0), ("expect_present", VBool True)
                , ("children", VList (V.singleton empty)), ("lookup", msg [("key", empty)]), ("child", empty)
                ]
              invalid = msg [("child", msg [("count", VInt 1)])]
              check validator = do
                mapM_ (\input -> validator input `shouldBe` []) [empty, zeros, present]
                validator invalid `shouldBe` validate invalid source
          map violationFieldPath (validate invalid source) `shouldBe` ["child", ""]
          mapM_ check [(flip validate source), (flip validate descriptor), projectionValidator]
      , it "uses escaped proto2 defaults and bytes rules with explicit presence on all paths" $ do
          descriptor <- either (fail . show) pure (messageRulesFromDescriptor explicitDefaultsDescriptor "ExplicitDefaults")
          sourceRules <- either (fail . show) pure (parseProtoRules explicitDefaultsProto)
          source <- maybe (fail "missing ExplicitDefaults") pure (lookup "ExplicitDefaults" sourceRules)
          parsed <- either (fail . show) pure (parseProtoFile "defaults.proto" explicitDefaultsProto)
          printedRules <- either (fail . show) pure (parseProtoRules (printProtoFile parsed))
          printed <- maybe (fail "missing ExplicitDefaults after printing") pure (lookup "ExplicitDefaults" printedRules)
          let explicit = msg
                [ ("count", VInt 7), ("state", VInt 2), ("expect_present", VBool True)
                , ("chosen", VInt 2)
                , ("data", VBytes (BS.pack [255, 195, 169]))
                ]
              invalid = msg [("data", VBytes "wrong")]
              check validator = do
                validator (msg []) `shouldBe` []
                validator explicit `shouldBe` []
                validator invalid `shouldBe` validate invalid source
                map violationFieldPath (validator invalid) `shouldBe` ["data", ""]
          mapM_ check [(flip validate source), (flip validate printed), (flip validate descriptor), explicitDefaultsValidator]
      , it "validates recursive messages through singular, oneof, list, and map fields on all paths" $ do
          sourceRules <- either (fail . show) pure (parseProtoRules recursiveProto)
          let bad = msg [("code", VInt (-1))]
              good = msg []
              ignored =
                [ ("ignored", bad)
                , ("ignored_items", VList (V.singleton bad))
                , ("ignored_values", msg [("bad", bad)])
                ]
              invalid = msg $
                [ ("next", msg [("next", bad)])
                , ("children", VList (V.fromList [good, bad]))
                , ("lookup", msg [("bad", bad)])
                , ("peer", msg [("node", bad)])
                , ("selected", bad)
                ] ++ ignored
              check name compiled valid badInput expectedPaths = do
                descriptor <- either (fail . show) pure (messageRulesFromDescriptor recursiveDescriptor ("fixture.recursive." <> name))
                source <- maybe (fail "missing recursive message") pure (lookup name sourceRules)
                let expected = validate badInput source
                map violationFieldPath expected `shouldBe` expectedPaths
                map violationConstraintId expected `shouldBe` map (const "int32.gte") expectedPaths
                mapM_ (\validator -> do
                  validator good `shouldBe` []
                  validator valid `shouldBe` []
                  validator badInput `shouldBe` expected)
                  [(flip validate source), (flip validate descriptor), compiled]
          check "Node" recursiveNodeValidator (msg ignored) invalid
            ["next.next.code", "children[1].code", "peer.node.code", "selected.code", "lookup[bad].code"]
          check "Peer" recursivePeerValidator (msg [("node", msg ignored)])
            (msg [("node", msg [("peer", msg [("node", bad)])])]) ["node.peer.node.code"]
      , it "enforces message oneofs and applies default ignore only to unset members on all paths" $ do
          let groups = [(["name", "labels", "flag", "entries"], True), (["status", "region"], False)]
          descriptor <- either (fail . show) pure (messageRulesFromDescriptor (messageOneofDescriptor groups) "Exclusive")
          sourceRules <- either (fail . show) pure (parseProtoRules messageOneofProto)
          source <- maybe (fail "missing Exclusive") pure (lookup "Exclusive" sourceRules)
          let status = ("status", VString "ok")
              flag = ("flag", VBool False)
              options = "name, labels, flag, entries"
              missing = Violation "" "message.oneof" ("one of " <> options <> " must be set")
              conflict = Violation "" "message.oneof" ("only one of " <> options <> " can be set")
              check validator = do
                validator (msg [status]) `shouldBe` [missing]
                validator (msg [status, ("name", VString ""), ("labels", VList V.empty), ("entries", msg [])]) `shouldBe` [missing]
                validator (msg [status, flag]) `shouldBe` []
                validator (msg [status, ("entries", msg [("zero", VInt 0)])]) `shouldBe` []
                validator (msg [status, ("name", VString "ok"), ("labels", VList (V.singleton (VString "x")))]) `shouldBe` [conflict]
                validator (msg [status, flag, ("region", VUInt 1)]) `shouldBe`
                  [Violation "" "message.oneof" "only one of status, region can be set"]
                ids (validator (msg [status, ("name", VString "x")])) `shouldBe` ["string.min_len"]
                map violationFieldPath (validator (msg [flag])) `shouldBe` ["status"]
          mapM_ check [(flip validate source), (flip validate descriptor), messageOneofValidator]
      , it "rejects empty, duplicate, and unknown message oneof declarations" $ do
          let check names option = do
                let descriptor = messageOneofDescriptor [(names, False)]
                    source = "syntax = \"proto3\"; message Exclusive { string name = 1; option (buf.validate.message).oneof = " <> option <> "; }"
                    expectLeft = either (const (pure ())) (const (expectationFailure "Accepted invalid message oneof"))
                expectLeft (parseProtoRules source)
                expectLeft (messageRulesFromDescriptor descriptor "Exclusive")
          check [] "{}"
          check ["name", "name"] "{ fields: [\"name\", \"name\"] }"
          check ["unknown"] "{ fields: \"unknown\" }"
          malformedMessageOneofRejected `shouldBe` True
      , it "omits absent generic fields and keeps explicitly present CEL nulls" $ do
          let rules = compileValidator (messageRules [("optionalValue", emptyFieldRules {frRequired = True})] [])
          ids (validateValue rules (OptionalInput Nothing)) `shouldBe` ["required"]
          validateValue rules (OptionalInput (Just VNull)) `shouldBe` []
      , it "converts a single repeated field occurrence to a list and preserves float negative zero" $ do
          input <- either (fail . show) pure (decodeDynamic (BS.pack [8, 1, 21, 0, 0, 0, 128]))
          let schema =
                [ FieldSchema 1 "entries" (RepeatedField (ScalarField KInt32))
                , FieldSchema 2 "signed_zero" (ScalarField KFloat)
                ]
              isSignedZero (VDouble n) = isNegativeZero n
              isSignedZero _ = False
          case dynamicMessageToCel schema input of
            VMap fields -> do
              celMapLookup (VString "entries") fields `shouldBe` Just (VList (V.singleton (VInt 1)))
              fmap isSignedZero (celMapLookup (VString "signed_zero") fields) `shouldBe` Just True
            _ -> expectationFailure "Expected a CEL map"
          isSignedZero (toCel (-0.0 :: Float)) `shouldBe` True
      , it "projects 32-bit protobuf wire values at their declared width" $ do
          let bits = 4294967296
              wire = BS.pack
                (varintField 1 (bits - 3) <> varintField 2 (bits + 7)
                  <> varintField 3 (bits + 5) <> fixed32Field 4 4294967293
                  <> varintField 5 (bits - 3) <> varintField 6 (bits - 3))
              schema =
                [ FieldSchema 1 "signed" (ScalarField KInt32)
                , FieldSchema 2 "unsigned" (ScalarField KUint32)
                , FieldSchema 3 "zigzag" (ScalarField KSint32)
                , FieldSchema 4 "fixed" (ScalarField KSfixed32)
                , FieldSchema 5 "enum" (ScalarField KEnum)
                , FieldSchema 6 "wide" (ScalarField KInt64)
                ]
          input <- either (fail . show) pure (decodeDynamic wire)
          let expected =
                msg [("signed", VInt (-3)), ("unsigned", VUInt 7), ("zigzag", VInt (-3)),
                     ("fixed", VInt (-3)), ("enum", VInt (-3)), ("wide", VInt 4294967293)]
          dynamicMessageToCel schema input `shouldBe` expected
          dynamicMessageToCel [FieldSchema 1 "group" (MessageField schema)]
            (setDynamicField 1 (DynGroup input) emptyDynamic)
            `shouldBe` msg [("group", expected)]
      , it "projects schemaless strings, split submessages and mixed packed records using the declared shape" $ do
          let childSchema =
                [ FieldSchema 1 "first" (ScalarField KInt32)
                , FieldSchema 2 "second" (ScalarField KInt32)
                ]
              schema =
                [ FieldSchema 1 "child" (MessageField childSchema)
                , FieldSchema 2 "label" (ScalarField KString)
                , FieldSchema 3 "scores" (RepeatedField (ScalarField KInt32))
                , FieldSchema 4 "revision" (ScalarField KInt32)
                , FieldSchema 5 "children" (RepeatedField (MessageField childSchema))
                , FieldSchema 6 "fixed" (RepeatedField (ScalarField KSfixed32))
                , FieldSchema 7 "ratios" (RepeatedField (ScalarField KDouble))
                ]
              first = varintField 1 7
              second = varintField 2 9
              wire = BS.pack
                (lenField 1 first <> varintField 1 0 <> lenField 1 second <> strField 2 "ok"
                  <> varintField 3 1 <> lenField 3 [2, 3] <> lenField 3 [] <> varintField 3 4
                  <> varintField 4 1 <> varintField 4 2 <> lenField 4 [0] <> lenField 5 first <> lenField 5 second
                  <> lenField 6 (fixed32 4294967293) <> fixed32Field 6 5
                  <> lenField 7 [0, 0, 0, 0, 0, 0, 240, 63])
              expected = msg
                [ ("child", msg [("first", VInt 7), ("second", VInt 9)])
                , ("label", VString "ok")
                , ("scores", VList (V.fromList (map VInt [1, 2, 3, 4])))
                , ("revision", VInt 2)
                , ("children", VList (V.fromList [msg [("first", VInt 7)], msg [("second", VInt 9)]]))
                , ("fixed", VList (V.fromList (map VInt [-3, 5])))
                , ("ratios", VList (V.singleton (VDouble 1)))
                ]
          fmap (dynamicMessageToCel schema) (decodeDynamic wire) `shouldBe` Right expected
      , it "retains malformed protobuf payloads in the best-effort projection" $ do
          let schema =
                [ FieldSchema 1 "child" (MessageField [FieldSchema 1 "number" (ScalarField KInt32)])
                , FieldSchema 2 "label" (ScalarField KString)
                , FieldSchema 3 "scores" (RepeatedField (ScalarField KInt32))
                ]
              wire = BS.pack (lenField 1 [8] <> lenField 1 [1] <> lenField 2 [255] <> lenField 3 [128])
              expected = msg
                [ ("child", VList (V.fromList [VBytes (BS.singleton 8), VBytes (BS.singleton 1)]))
                , ("label", VBytes (BS.singleton 255))
                , ("scores", VList (V.singleton (VBytes (BS.singleton 128))))
                ]
          fmap (dynamicMessageToCel schema) (decodeDynamic wire) `shouldBe` Right expected
      , it "ignores mismatched wire records when projecting declared fields" $ do
          let childSchema = [FieldSchema 1 "id" (ScalarField KInt32)]
              schema =
                [ FieldSchema 1 "revision" (ScalarField KInt32)
                , FieldSchema 2 "scores" (RepeatedField (ScalarField KInt32))
                , FieldSchema 3 "children" (RepeatedField (MessageField childSchema))
                , FieldSchema 4 "child" (MessageField childSchema)
                , FieldSchema 5 "labels" (RepeatedField (ScalarField KString))
                , FieldSchema 6 "empty" (RepeatedField (ScalarField KInt32))
                ]
              wire = BS.pack
                (strField 1 "a" <> varintField 2 7 <> fixed32Field 2 1 <> lenField 2 [8, 9]
                  <> lenField 3 (varintField 1 5) <> varintField 3 9 <> varintField 4 9
                  <> varintField 5 7 <> strField 5 "ok" <> lenField 6 [])
              expected = msg
                [ ("scores", VList (V.fromList (map VInt [7, 8, 9])))
                , ("children", VList (V.singleton (msg [("id", VInt 5)])))
                , ("labels", VList (V.singleton (VString "ok")))
                , ("empty", VList V.empty)
                ]
          fmap (dynamicMessageToCel schema) (decodeDynamic wire) `shouldBe` Right expected
      , it "projects split and repeated well-known temporal messages as CEL time values" $ do
          let schema =
                [ FieldSchema 1 "at" (ScalarField KTimestamp)
                , FieldSchema 2 "ttl" (ScalarField KDuration)
                , FieldSchema 3 "times" (RepeatedField (ScalarField KTimestamp))
                , FieldSchema 4 "malformed" (ScalarField KDuration)
                , FieldSchema 5 "invalid" (ScalarField KTimestamp)
                ]
              invalid = varintField 2 1000000000
              wire = BS.pack
                (lenField 1 (varintField 1 7) <> varintField 1 42 <> lenField 1 (varintField 2 9)
                  <> lenField 2 (varintField 1 maxBound <> varintField 2 (maxBound - 1))
                  <> lenField 3 [] <> lenField 3 (varintField 1 8) <> lenField 4 [8] <> lenField 5 invalid)
              expected = msg
                [ ("at", VTimestamp (Timestamp 7 9))
                , ("ttl", VDuration (Duration (-1) (-2)))
                , ("times", VList (V.fromList [VTimestamp (Timestamp 0 0), VTimestamp (Timestamp 8 0)]))
                , ("malformed", VBytes (BS.singleton 8))
                , ("invalid", VBytes (BS.pack invalid))
                ]
          fmap (dynamicMessageToCel schema) (decodeDynamic wire) `shouldBe` Right expected
      , it "projects declared groups from their retained schemaless wire records" $ do
          let childSchema =
                [ FieldSchema 1 "first" (ScalarField KInt32)
                , FieldSchema 2 "second" (ScalarField KInt32)
                ]
              schema =
                [ FieldSchema 1 "group" (GroupField childSchema)
                , FieldSchema 2 "groups" (RepeatedField (GroupField childSchema))
                , FieldSchema 3 "child" (MessageField childSchema)
                , FieldSchema 4 "absent" (GroupField childSchema)
                ]
              group number body = tag number 3 <> body <> tag number 4
              first = varintField 1 7
              second = varintField 2 9
              wire = BS.pack
                (group 1 first <> lenField 1 (varintField 1 42) <> group 1 second
                  <> group 2 first <> group 2 second
                  <> group 3 first <> lenField 3 second <> lenField 4 second)
              expected = msg
                [ ("group", msg [("first", VInt 7), ("second", VInt 9)])
                , ("groups", VList (V.fromList [msg [("first", VInt 7)], msg [("second", VInt 9)]]))
                , ("child", msg [("second", VInt 9)])
                ]
          fmap (dynamicMessageToCel schema) (decodeDynamic wire) `shouldBe` Right expected
          child <- either (fail . show) pure (decodeDynamic (BS.pack first))
          dynamicMessageToCel [FieldSchema 1 "group" (GroupField childSchema)]
            (setDynamicField 1 (DynGroup child) emptyDynamic)
            `shouldBe` msg [("group", msg [("first", VInt 7)])]
      , it "rejects invalid shorthand when loading rules" $ do
          let invalid = "syntax = \"proto3\"; message Bad { int32 code = 1 [(buf.validate.field).cel_expression = \"this +\"]; }"
              field bytes = defaultFieldDescriptorProto
                { fdpFieldName = "code"
                , fdpFieldNumber = 1
                , fdpFieldType = 5
                , fdpFieldOptions = Just defaultFieldOptions
                    {fldoUnknownFields = [UnknownLenDelim 1159 (BS.pack bytes)]}
                }
              descriptor bytes = defaultFileDescriptorProto
                {fdpMessageType = V.singleton defaultDescriptorProto {dpName = "Bad", dpField = V.singleton (field bytes)}}
              expectLeft = either (const (pure ())) (const (expectationFailure "Accepted malformed shorthand"))
          expectLeft (parseProtoRules invalid)
          expectLeft (messageRulesFromDescriptor (descriptor (strField 29 "this +")) "Bad")
          expectLeft (messageRulesFromDescriptor (descriptor (lenField 29 [255])) "Bad")
          malformedShorthandRejected `shouldBe` True
      , it "only IGNORE_ALWAYS bypasses required on an implicit zero" $ do
          let field name number mode hasPresence =
                defaultFieldDescriptorProto
                  { fdpFieldName = name
                  , fdpFieldNumber = number
                  , fdpFieldLabel = 1
                  , fdpFieldType = 9
                  , fdpFieldProto3Optional = hasPresence
                  , fdpFieldOptions = Just defaultFieldOptions
                      { fldoUnknownFields =
                          [UnknownLenDelim 1159 (BS.pack (stringFieldConstraints ++ varintField 27 mode))]
                      }
                  }
              descriptor =
                defaultFileDescriptorProto
                  { fdpSyntax = "proto3"
                  , fdpMessageType = V.singleton defaultDescriptorProto
                      { dpName = "Ignore"
                      , dpField = V.fromList
                          [field "implicit" 1 1 False, field "explicit" 2 1 True, field "always" 3 3 False]
                      }
                  }
              rules = either (error . show) id (messageRulesFromDescriptor descriptor "Ignore")
          ids (validate (msg [("implicit", VString ""), ("explicit", VString "xyz")]) rules) `shouldBe` ["required"]
          validate (msg [("implicit", VString "xyz"), ("explicit", VString "xyz"), ("always", VString "x")]) rules `shouldBe` []
          map violationFieldPath (validate (msg [("implicit", VString "xyz"), ("explicit", VString "")]) rules)
            `shouldBe` ["explicit", "explicit"]
      ]
