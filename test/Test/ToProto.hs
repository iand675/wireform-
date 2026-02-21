{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeApplications #-}
module Test.ToProto (toProtoTests) where

import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

import Proto.AST
import Proto.Parser (parseProtoFile)
import Proto.Print (printProtoFile)
import Proto.Schema
import Proto.ToProto


-- Test types with ProtoMessage / ProtoEnum / ProtoService instances

data TestPerson = TestPerson
  { tpName  :: !Text
  , tpId    :: !Int32
  , tpEmail :: !Text
  }

instance ProtoMessage TestPerson where
  protoMessageName _ = "test.TestPerson"
  protoPackageName _ = "test"
  protoFieldDescriptors _ = Map.fromList
    [ (1, SomeField (FieldDescriptor "name"  1 (ScalarType StringField) LabelOptional tpName  (\v m -> m { tpName = v })))
    , (2, SomeField (FieldDescriptor "id"    2 (ScalarType Int32Field)  LabelOptional tpId    (\v m -> m { tpId = v })))
    , (3, SomeField (FieldDescriptor "email" 3 (ScalarType StringField) LabelOptional tpEmail (\v m -> m { tpEmail = v })))
    ]
  protoDefaultValue = TestPerson "" 0 ""


data TestStatus
  = StatusUnknown
  | StatusActive
  | StatusInactive

instance ProtoEnum TestStatus where
  protoEnumName _ = "test.TestStatus"
  protoEnumValues _ =
    [ ("STATUS_UNKNOWN", 0)
    , ("STATUS_ACTIVE", 1)
    , ("STATUS_INACTIVE", 2)
    ]
  fromProtoEnumValue 0 = Just StatusUnknown
  fromProtoEnumValue 1 = Just StatusActive
  fromProtoEnumValue 2 = Just StatusInactive
  fromProtoEnumValue _ = Nothing
  toProtoEnumValue StatusUnknown  = 0
  toProtoEnumValue StatusActive   = 1
  toProtoEnumValue StatusInactive = 2


data TestAllFields = TestAllFields
  { tafDouble   :: !Double
  , tafFloat    :: !Float
  , tafInt64    :: !Int64
  , tafUint32   :: !Word
  , tafBool     :: !Bool
  , tafString   :: !Text
  , tafBytes    :: !ByteString
  , tafRepeated :: !(V.Vector Int32)
  }

instance ProtoMessage TestAllFields where
  protoMessageName _ = "test.TestAllFields"
  protoPackageName _ = "test"
  protoFieldDescriptors _ = Map.fromList
    [ (1,  SomeField (FieldDescriptor "double_val"   1  (ScalarType DoubleField)  LabelOptional tafDouble   (\v m -> m { tafDouble = v })))
    , (2,  SomeField (FieldDescriptor "float_val"    2  (ScalarType FloatField)   LabelOptional tafFloat    (\v m -> m { tafFloat = v })))
    , (3,  SomeField (FieldDescriptor "int64_val"    3  (ScalarType Int64Field)   LabelOptional tafInt64    (\v m -> m { tafInt64 = v })))
    , (4,  SomeField (FieldDescriptor "uint32_val"   4  (ScalarType UInt32Field)  LabelOptional tafUint32   (\v m -> m { tafUint32 = v })))
    , (5,  SomeField (FieldDescriptor "bool_val"     5  (ScalarType BoolField)    LabelOptional tafBool     (\v m -> m { tafBool = v })))
    , (6,  SomeField (FieldDescriptor "string_val"   6  (ScalarType StringField)  LabelOptional tafString   (\v m -> m { tafString = v })))
    , (7,  SomeField (FieldDescriptor "bytes_val"    7  (ScalarType BytesField)   LabelOptional tafBytes    (\v m -> m { tafBytes = v })))
    , (8,  SomeField (FieldDescriptor "repeated_val" 8  (ScalarType Int32Field)   LabelRepeated tafRepeated (\v m -> m { tafRepeated = v })))
    ]
  protoDefaultValue = TestAllFields 0 0 0 0 False "" "" V.empty


data TestWithSubMsg = TestWithSubMsg
  { twsName :: !Text
  , twsSub  :: !(Maybe TestPerson)
  }

instance ProtoMessage TestWithSubMsg where
  protoMessageName _ = "test.TestWithSubMsg"
  protoPackageName _ = "test"
  protoFieldDescriptors _ = Map.fromList
    [ (1, SomeField (FieldDescriptor "name" 1 (ScalarType StringField)         LabelOptional twsName (\v m -> m { twsName = v })))
    , (2, SomeField (FieldDescriptor "sub"  2 (MessageType "test.TestPerson")  LabelOptional twsSub  (\v m -> m { twsSub = v })))
    ]
  protoDefaultValue = TestWithSubMsg "" Nothing


data TestWithMap = TestWithMap
  { twmLabels :: !(Map.Map Text Text)
  , twmCounts :: !(Map.Map Text Int32)
  }

instance ProtoMessage TestWithMap where
  protoMessageName _ = "test.TestWithMap"
  protoPackageName _ = "test"
  protoFieldDescriptors _ = Map.fromList
    [ (1, SomeField (FieldDescriptor "labels" 1 (MapType StringField (ScalarType StringField)) LabelRepeated twmLabels (\v m -> m { twmLabels = v })))
    , (2, SomeField (FieldDescriptor "counts" 2 (MapType StringField (ScalarType Int32Field))  LabelRepeated twmCounts (\v m -> m { twmCounts = v })))
    ]
  protoDefaultValue = TestWithMap Map.empty Map.empty


data TestWithEnum = TestWithEnum
  { tweId     :: !Int32
  , tweStatus :: !Int32
  }

instance ProtoMessage TestWithEnum where
  protoMessageName _ = "test.TestWithEnum"
  protoPackageName _ = "test"
  protoFieldDescriptors _ = Map.fromList
    [ (1, SomeField (FieldDescriptor "id"     1 (ScalarType Int32Field)     LabelOptional tweId     (\v m -> m { tweId = v })))
    , (2, SomeField (FieldDescriptor "status" 2 (EnumType "test.TestStatus") LabelOptional tweStatus (\v m -> m { tweStatus = v })))
    ]
  protoDefaultValue = TestWithEnum 0 0


data TestGreeterService
instance ProtoService TestGreeterService where
  protoServiceName _ = "test.Greeter"
  protoServiceMethods _ =
    [ MethodDescriptor "SayHello"   "HelloRequest"  "HelloReply"  False False
    , MethodDescriptor "StreamChat" "ChatMessage"   "ChatMessage" True  True
    ]


toProtoTests :: TestTree
toProtoTests = testGroup "ToProto"
  [ testGroup "messageToAST"
      [ testCase "simple message has correct name" $ do
          let ast = messageToAST (Proxy @TestPerson)
          msgName ast @?= "TestPerson"

      , testCase "simple message has correct field count" $ do
          let ast = messageToAST (Proxy @TestPerson)
          length (msgElements ast) @?= 3

      , testCase "fields are ordered by field number" $ do
          let ast = messageToAST (Proxy @TestPerson)
              names = fmap extractFieldName (msgElements ast)
          names @?= ["name", "id", "email"]

      , testCase "all scalar types" $ do
          let ast = messageToAST (Proxy @TestAllFields)
          length (msgElements ast) @?= 8
          let names = fmap extractFieldName (msgElements ast)
          names @?= ["double_val", "float_val", "int64_val", "uint32_val", "bool_val", "string_val", "bytes_val", "repeated_val"]

      , testCase "repeated field gets Repeated label" $ do
          let ast = messageToAST (Proxy @TestAllFields)
          case last (msgElements ast) of
            MEField fd -> fieldLabel fd @?= Just Repeated
            other -> assertFailure ("Expected MEField, got " <> show other)

      , testCase "submessage field" $ do
          let ast = messageToAST (Proxy @TestWithSubMsg)
              elems = msgElements ast
          length elems @?= 2
          case elems !! 1 of
            MEField fd -> fieldType fd @?= FTNamed "test.TestPerson"
            other -> assertFailure ("Expected MEField, got " <> show other)

      , testCase "enum field reference" $ do
          let ast = messageToAST (Proxy @TestWithEnum)
          case msgElements ast !! 1 of
            MEField fd -> fieldType fd @?= FTNamed "test.TestStatus"
            other -> assertFailure ("Expected MEField, got " <> show other)

      , testCase "map fields become MEMapField" $ do
          let ast = messageToAST (Proxy @TestWithMap)
          case msgElements ast of
            [MEMapField mf1, MEMapField mf2] -> do
              mapFieldName mf1 @?= "labels"
              mapKeyType mf1 @?= SString
              mapValueType mf1 @?= FTScalar SString
              mapFieldName mf2 @?= "counts"
              mapKeyType mf2 @?= SString
              mapValueType mf2 @?= FTScalar SInt32
            other -> assertFailure ("Expected two MEMapField, got " <> show other)
      ]

  , testGroup "enumToAST"
      [ testCase "enum has correct name" $ do
          let ast = enumToAST (Proxy @TestStatus)
          enumName ast @?= "TestStatus"

      , testCase "enum has correct values" $ do
          let ast = enumToAST (Proxy @TestStatus)
          length (enumValues ast) @?= 3
          fmap evName (enumValues ast) @?= ["STATUS_UNKNOWN", "STATUS_ACTIVE", "STATUS_INACTIVE"]
          fmap evNumber (enumValues ast) @?= [0, 1, 2]
      ]

  , testGroup "serviceToAST"
      [ testCase "service has correct name" $ do
          let ast = serviceToAST (Proxy @TestGreeterService)
          svcName ast @?= "Greeter"

      , testCase "service has correct RPCs" $ do
          let ast = serviceToAST (Proxy @TestGreeterService)
          length (svcRpcs ast) @?= 2
          let rpc1 = head (svcRpcs ast)
          rpcName rpc1 @?= "SayHello"
          rpcInputStr rpc1 @?= NoStream
          rpcOutputStr rpc1 @?= NoStream
          let rpc2 = svcRpcs ast !! 1
          rpcName rpc2 @?= "StreamChat"
          rpcInputStr rpc2 @?= Streaming
          rpcOutputStr rpc2 @?= Streaming
      ]

  , testGroup "toProtoFile"
      [ testCase "builds complete file" $ do
          let pf = toProtoFile Proto3 (Just "test") [messageTopLevel (Proxy @TestPerson)]
          protoSyntax pf @?= Proto3
          protoPackage pf @?= Just "test"
          length (protoTopLevels pf) @?= 1
      ]

  , testGroup "text rendering"
      [ testCase "messageToProtoText produces valid proto" $ do
          let text = messageToProtoText (Proxy @TestPerson)
          assertBool "contains message keyword" (T.isInfixOf "message TestPerson" text)
          assertBool "contains name field" (T.isInfixOf "string name = 1;" text)
          assertBool "contains id field" (T.isInfixOf "int32 id = 2;" text)
          assertBool "contains email field" (T.isInfixOf "string email = 3;" text)

      , testCase "enumToProtoText produces valid proto" $ do
          let text = enumToProtoText (Proxy @TestStatus)
          assertBool "contains enum keyword" (T.isInfixOf "enum TestStatus" text)
          assertBool "contains UNKNOWN" (T.isInfixOf "STATUS_UNKNOWN = 0;" text)
          assertBool "contains ACTIVE" (T.isInfixOf "STATUS_ACTIVE = 1;" text)

      , testCase "serviceToProtoText produces valid proto" $ do
          let text = serviceToProtoText (Proxy @TestGreeterService)
          assertBool "contains service keyword" (T.isInfixOf "service Greeter" text)
          assertBool "contains SayHello rpc" (T.isInfixOf "rpc SayHello" text)
          assertBool "contains streaming rpc" (T.isInfixOf "stream ChatMessage" text)

      , testCase "map fields render correctly" $ do
          let text = messageToProtoText (Proxy @TestWithMap)
          assertBool "contains map<string, string>" (T.isInfixOf "map<string, string>" text)
          assertBool "contains map<string, int32>" (T.isInfixOf "map<string, int32>" text)

      , testCase "repeated field renders correctly" $ do
          let text = messageToProtoText (Proxy @TestAllFields)
          assertBool "contains repeated" (T.isInfixOf "repeated int32 repeated_val = 8;" text)
      ]

  , testGroup "roundtrip (AST -> text -> parse)"
      [ testCase "simple message roundtrips through parser" $ do
          let pf = toProtoFile Proto3 (Just "test")
                     [messageTopLevel (Proxy @TestPerson)]
              text = printProtoFile pf
          case parseProtoFile "<test>" text of
            Left e -> assertFailure ("Parse failed: " <> show e <> "\nText:\n" <> T.unpack text)
            Right parsed -> do
              protoSyntax parsed @?= Proto3
              protoPackage parsed @?= Just "test"
              let msgs = protoTopLevels parsed
              length msgs @?= 1
              case head msgs of
                TLMessage md -> do
                  msgName md @?= "TestPerson"
                  length (msgElements md) @?= 3
                other -> assertFailure ("Expected TLMessage, got " <> show other)

      , testCase "enum roundtrips through parser" $ do
          let pf = toProtoFile Proto3 (Just "test")
                     [enumTopLevel (Proxy @TestStatus)]
              text = printProtoFile pf
          case parseProtoFile "<test>" text of
            Left e -> assertFailure ("Parse failed: " <> show e <> "\nText:\n" <> T.unpack text)
            Right parsed -> do
              let tls = protoTopLevels parsed
              length tls @?= 1
              case head tls of
                TLEnum ed -> do
                  enumName ed @?= "TestStatus"
                  length (enumValues ed) @?= 3
                other -> assertFailure ("Expected TLEnum, got " <> show other)

      , testCase "service roundtrips through parser" $ do
          let pf = toProtoFile Proto3 Nothing
                     [serviceTopLevel (Proxy @TestGreeterService)]
              text = printProtoFile pf
          case parseProtoFile "<test>" text of
            Left e -> assertFailure ("Parse failed: " <> show e <> "\nText:\n" <> T.unpack text)
            Right parsed -> do
              let tls = protoTopLevels parsed
              length tls @?= 1
              case head tls of
                TLService sd -> do
                  svcName sd @?= "Greeter"
                  length (svcRpcs sd) @?= 2
                other -> assertFailure ("Expected TLService, got " <> show other)

      , testCase "map message roundtrips through parser" $ do
          let pf = toProtoFile Proto3 (Just "test")
                     [messageTopLevel (Proxy @TestWithMap)]
              text = printProtoFile pf
          case parseProtoFile "<test>" text of
            Left e -> assertFailure ("Parse failed: " <> show e <> "\nText:\n" <> T.unpack text)
            Right parsed -> do
              case head (protoTopLevels parsed) of
                TLMessage md -> do
                  msgName md @?= "TestWithMap"
                  let mapFields = filter isMapElem (msgElements md)
                  length mapFields @?= 2
                other -> assertFailure ("Expected TLMessage, got " <> show other)

      , testCase "all scalar types roundtrip through parser" $ do
          let pf = toProtoFile Proto3 (Just "test")
                     [messageTopLevel (Proxy @TestAllFields)]
              text = printProtoFile pf
          case parseProtoFile "<test>" text of
            Left e -> assertFailure ("Parse failed: " <> show e <> "\nText:\n" <> T.unpack text)
            Right parsed -> do
              case head (protoTopLevels parsed) of
                TLMessage md -> length (msgElements md) @?= 8
                other -> assertFailure ("Expected TLMessage, got " <> show other)

      , testCase "composite file with messages, enums, and services" $ do
          let pf = toProtoFile Proto3 (Just "test")
                     [ enumTopLevel (Proxy @TestStatus)
                     , messageTopLevel (Proxy @TestPerson)
                     , messageTopLevel (Proxy @TestWithEnum)
                     , serviceTopLevel (Proxy @TestGreeterService)
                     ]
              text = printProtoFile pf
          case parseProtoFile "<test>" text of
            Left e -> assertFailure ("Parse failed: " <> show e <> "\nText:\n" <> T.unpack text)
            Right parsed -> do
              protoSyntax parsed @?= Proto3
              protoPackage parsed @?= Just "test"
              length (protoTopLevels parsed) @?= 4
      ]

  , testGroup "messagePackage"
      [ testCase "extracts non-empty package" $ do
          messagePackage (Proxy @TestPerson) @?= Just "test"
      ]
  ]


-- Helpers

extractFieldName :: MessageElement -> Text
extractFieldName = \case
  MEField fd    -> fieldName fd
  MEMapField mf -> mapFieldName mf
  _             -> "<non-field>"

isMapElem :: MessageElement -> Bool
isMapElem (MEMapField _) = True
isMapElem _              = False
