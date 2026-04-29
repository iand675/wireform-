module Test.TDP (tdpTests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int32, Int64)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word32, Word64)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog

import Proto.Decode
import Proto.Encode
import Proto.Schema
import Proto.TDP
import Proto.Wire (WireType(..), Tag(..), fieldTag)
import qualified Proto.Wire.Decode as WD
import Proto.Wire.Decode (DecodeError(..), Decoder, DecodeResult(..), getVarint, getText,
  getFixed32, getFixed64, getLengthDelimited, runDecoder, runDecoder',
  withTagM, skipWireType, getTag)
import Proto.Wire.Encode
import Wireform.FFI (countPackedVarints, packedAllSingleByte, validateUtf8SWAR)
import qualified Data.Text.Encoding

tdpTests :: TestTree
tdpTests = testGroup "TDP (Table-Driven Parser)"
  [ tdpCoreTests
  , tdpCompileTests
  , tdpWireFFITests
  , tdpPackedTests
  , tdpUTF8Tests
  , tdpWithTagMTests
  ]

-- ============================================================
-- TDP core interpreter tests
-- ============================================================

tdpCoreTests :: TestTree
tdpCoreTests = testGroup "Core interpreter"
  [ testCase "empty message" $ do
      let result = runParseTable emptyTable BS.empty
      case result of
        Right msg -> IntMap.null (tdpFields msg) @?= True
        Left e -> assertFailure (show e)

  , testCase "single varint field" $ do
      let bs = buildToBS $ putTag 1 WireVarint <> putVarint 42
          result = runParseTable testSimpleTable bs
      case result of
        Right msg -> do
          case IntMap.lookup 1 (tdpFields msg) of
            Just (TVVarint 42) -> pure ()
            other -> assertFailure ("Expected TVVarint 42, got: " <> show other)
        Left e -> assertFailure (show e)

  , testCase "multiple fields in order" $ do
      let bs = buildToBS $
            putTag 1 WireVarint <> putVarint 100 <>
            putTag 2 WireLengthDelimited <> putText "hello" <>
            putTag 3 WireVarint <> putVarint 1
          result = runParseTable testMultiTable bs
      case result of
        Right msg -> do
          IntMap.lookup 1 (tdpFields msg) @?= Just (TVVarint 100)
          IntMap.lookup 2 (tdpFields msg) @?= Just (TVBytes (buildToBS (B.byteString "hello")))
          IntMap.lookup 3 (tdpFields msg) @?= Just (TVVarint 1)
        Left e -> assertFailure (show e)

  , testCase "fields out of order" $ do
      let bs = buildToBS $
            putTag 3 WireVarint <> putVarint 99 <>
            putTag 1 WireVarint <> putVarint 42
          result = runParseTable testMultiTable bs
      case result of
        Right msg -> do
          IntMap.lookup 1 (tdpFields msg) @?= Just (TVVarint 42)
          IntMap.lookup 3 (tdpFields msg) @?= Just (TVVarint 99)
        Left e -> assertFailure (show e)

  , testCase "unknown fields are skipped" $ do
      let bs = buildToBS $
            putTag 1 WireVarint <> putVarint 42 <>
            putTag 99 WireVarint <> putVarint 999 <>
            putTag 2 WireLengthDelimited <> putText "hi"
          result = runParseTable testMultiTable bs
      case result of
        Right msg -> do
          IntMap.lookup 1 (tdpFields msg) @?= Just (TVVarint 42)
          IntMap.member 99 (tdpFields msg) @?= False
        Left e -> assertFailure (show e)

  , testCase "last value wins for scalar fields" $ do
      let bs = buildToBS $
            putTag 1 WireVarint <> putVarint 10 <>
            putTag 1 WireVarint <> putVarint 20
          result = runParseTable testSimpleTable bs
      case result of
        Right msg ->
          IntMap.lookup 1 (tdpFields msg) @?= Just (TVVarint 20)
        Left e -> assertFailure (show e)

  , testCase "fixed32 field" $ do
      let bs = buildToBS $ putTag 1 Wire32Bit <> putFixed32 0xDEADBEEF
          table = makeSimpleTable 1 Wire32Bit (thunkFixed32Pub TVFixed32)
          result = runParseTable table bs
      case result of
        Right msg ->
          IntMap.lookup 1 (tdpFields msg) @?= Just (TVFixed32 0xDEADBEEF)
        Left e -> assertFailure (show e)

  , testCase "fixed64 field" $ do
      let bs = buildToBS $ putTag 1 Wire64Bit <> putFixed64 0xCAFEBABEDEADBEEF
          table = makeSimpleTable 1 Wire64Bit (thunkFixed64Pub TVFixed64)
          result = runParseTable table bs
      case result of
        Right msg ->
          IntMap.lookup 1 (tdpFields msg) @?= Just (TVFixed64 0xCAFEBABEDEADBEEF)
        Left e -> assertFailure (show e)

  , testProperty "varint field roundtrip through TDP" $ property $ do
      val <- forAll $ Gen.word64 (Range.linear 0 maxBound)
      let bs = buildToBS $ putTag 1 WireVarint <> putVarint val
          result = runParseTable testSimpleTable bs
      case result of
        Right msg ->
          IntMap.lookup 1 (tdpFields msg) === Just (TVVarint val)
        Left e -> do
          annotate (show e)
          failure

  , testProperty "multiple varint fields roundtrip" $ property $ do
      v1 <- forAll $ Gen.word64 (Range.linear 0 maxBound)
      v2 <- forAll $ Gen.word64 (Range.linear 0 maxBound)
      v3 <- forAll $ Gen.word64 (Range.linear 0 maxBound)
      let bs = buildToBS $
            putTag 1 WireVarint <> putVarint v1 <>
            putTag 2 WireVarint <> putVarint v2 <>
            putTag 3 WireVarint <> putVarint v3
          result = runParseTable testThreeVarintTable bs
      case result of
        Right msg -> do
          IntMap.lookup 1 (tdpFields msg) === Just (TVVarint v1)
          IntMap.lookup 2 (tdpFields msg) === Just (TVVarint v2)
          IntMap.lookup 3 (tdpFields msg) === Just (TVVarint v3)
        Left e -> do
          annotate (show e)
          failure
  ]

-- ============================================================
-- Compile from schema tests
-- ============================================================

tdpCompileTests :: TestTree
tdpCompileTests = testGroup "Compilation from schema"
  [ testCase "compileParseTable produces non-empty table" $ do
      let table = compileParseTable (Proxy :: Proxy TestSchemaMsg)
      V.length (ptFields table) @?= 3
      BS.length (ptTagLUT table) @?= 128

  , testCase "TagLUT has entries for small field numbers" $ do
      let table = compileParseTable (Proxy :: Proxy TestSchemaMsg)
          -- Field 1, varint: tag = 0x08
          lut08 = BS.index (ptTagLUT table) 0x08
          -- Field 2, length-delimited: tag = 0x12
          lut12 = BS.index (ptTagLUT table) 0x12
      lut08 /= 0xFF @?= True
      lut12 /= 0xFF @?= True

  , testCase "compiled table decodes matching wire data" $ do
      let table = compileParseTable (Proxy :: Proxy TestSchemaMsg)
          bs = buildToBS $
            putTag 1 WireVarint <> putVarint 42 <>
            putTag 2 WireLengthDelimited <> putText "test" <>
            putTag 3 WireVarint <> putVarint 1
          result = runParseTable table bs
      case result of
        Right msg -> do
          IntMap.lookup 1 (tdpFields msg) @?= Just (TVVarint 42)
          IntMap.lookup 3 (tdpFields msg) @?= Just (TVBool True)
        Left e -> assertFailure (show e)

  , testProperty "compiled table roundtrips with encodeMessage" $ property $ do
      val <- forAll $ Gen.word64 (Range.linear 0 1000)
      name <- forAll $ Gen.text (Range.linear 0 50) Gen.alphaNum
      active <- forAll Gen.bool
      let msg = TestSchemaMsg val name active
          encoded = encodeMessage msg
          table = compileParseTable (Proxy :: Proxy TestSchemaMsg)
          result = runParseTable table encoded
      case result of
        Right tdpMsg -> do
          case IntMap.lookup 1 (tdpFields tdpMsg) of
            Just (TVVarint v) -> v === val
            Nothing | val == 0 -> success
            other -> do
              annotate ("field 1: " <> show other)
              failure
        Left e -> do
          annotate (show e)
          failure
  ]

-- ============================================================
-- Wire FFI tests (SWAR routines)
-- ============================================================

tdpWireFFITests :: TestTree
tdpWireFFITests = testGroup "Wire FFI (SWAR)"
  [ testCase "countPackedVarints empty" $
      countPackedVarints BS.empty @?= 0

  , testCase "countPackedVarints single byte" $
      countPackedVarints (BS.pack [42]) @?= 1

  , testCase "countPackedVarints all single byte" $
      countPackedVarints (BS.pack [0, 1, 2, 3, 4, 5, 6, 7]) @?= 8

  , testCase "countPackedVarints two-byte varints" $
      countPackedVarints (BS.pack [0x80, 0x01, 0x80, 0x02]) @?= 2

  , testCase "countPackedVarints mixed" $
      countPackedVarints (BS.pack [42, 0x80, 0x01, 99]) @?= 3

  , testProperty "countPackedVarints matches manual count" $ property $ do
      vals <- forAll $ Gen.list (Range.linear 0 100) (Gen.word64 (Range.linear 0 0xFFFF))
      let encoded = BL.toStrict $ B.toLazyByteString $ foldMap putVarint vals
          expected = length vals
      countPackedVarints encoded === expected

  , testCase "packedAllSingleByte empty" $
      packedAllSingleByte BS.empty @?= True

  , testCase "packedAllSingleByte all small" $
      packedAllSingleByte (BS.pack [0, 1, 42, 127]) @?= True

  , testCase "packedAllSingleByte has continuation" $
      packedAllSingleByte (BS.pack [0, 0x80, 1]) @?= False

  , testProperty "packedAllSingleByte correct for small values" $ property $ do
      vals <- forAll $ Gen.list (Range.linear 0 200) (Gen.word8 (Range.linear 0 127))
      let bs = BS.pack vals
      packedAllSingleByte bs === True

  , testProperty "packedAllSingleByte false when large varint present" $ property $ do
      vals <- forAll $ Gen.list (Range.linear 1 50) (Gen.word64 (Range.linear 128 0xFFFF))
      let encoded = BL.toStrict $ B.toLazyByteString $ foldMap putVarint vals
      packedAllSingleByte encoded === False
  ]

-- ============================================================
-- Packed field decode tests (zero-copy, bulk memcpy paths)
-- ============================================================

tdpPackedTests :: TestTree
tdpPackedTests = testGroup "Packed field optimizations"
  [ testProperty "packed varint single-byte fast path" $ property $ do
      vals <- forAll $ Gen.list (Range.linear 0 200) (Gen.word64 (Range.linear 0 127))
      let vec = VU.fromList vals
          encoded = buildToBS (encodePackedWord64 1 vec)
      if VU.null vec
        then assert (BS.null encoded)
        else case runDecoder (getTag >> decodePackedVarint) encoded of
          Left e -> do
            annotate (show e)
            failure
          Right decoded -> VU.toList decoded === vals

  , testProperty "packed varint multi-byte values" $ property $ do
      vals <- forAll $ Gen.list (Range.linear 0 100) (Gen.word64 (Range.linear 128 0xFFFFFFFF))
      let vec = VU.fromList vals
          encoded = buildToBS (encodePackedWord64 1 vec)
      if VU.null vec
        then assert (BS.null encoded)
        else case runDecoder (getTag >> decodePackedVarint) encoded of
          Left e -> do
            annotate (show e)
            failure
          Right decoded -> VU.toList decoded === vals

  , testProperty "packed fixed32 bulk memcpy path" $ property $ do
      vals <- forAll $ Gen.list (Range.linear 0 200) (Gen.word32 Range.linearBounded)
      let vec = VU.fromList vals
          encoded = buildToBS (encodePackedFixed32 1 vec)
      if VU.null vec
        then assert (BS.null encoded)
        else case runDecoder (getTag >> decodePackedFixed32) encoded of
          Left e -> do
            annotate (show e)
            failure
          Right decoded -> VU.toList decoded === vals

  , testProperty "packed fixed64 bulk memcpy path" $ property $ do
      vals <- forAll $ Gen.list (Range.linear 0 200) (Gen.word64 Range.linearBounded)
      let vec = VU.fromList vals
          encoded = buildToBS (encodePackedFixed64 1 vec)
      if VU.null vec
        then assert (BS.null encoded)
        else case runDecoder (getTag >> decodePackedFixed64) encoded of
          Left e -> do
            annotate (show e)
            failure
          Right decoded -> VU.toList decoded === vals

  , testProperty "packed float bulk memcpy path" $ property $ do
      vals <- forAll $ Gen.list (Range.linear 0 200) (Gen.float (Range.linearFrac (-1e30) 1e30))
      let vec = VU.fromList vals
          encoded = buildToBS (encodePackedFloat 1 vec)
      if VU.null vec
        then assert (BS.null encoded)
        else case runDecoder (getTag >> decodePackedFloat) encoded of
          Left e -> do
            annotate (show e)
            failure
          Right decoded -> VU.toList decoded === vals

  , testProperty "packed double bulk memcpy path" $ property $ do
      vals <- forAll $ Gen.list (Range.linear 0 200) (Gen.double (Range.linearFrac (-1e300) 1e300))
      let vec = VU.fromList vals
          encoded = buildToBS (encodePackedDouble 1 vec)
      if VU.null vec
        then assert (BS.null encoded)
        else case runDecoder (getTag >> decodePackedDouble) encoded of
          Left e -> do
            annotate (show e)
            failure
          Right decoded -> VU.toList decoded === vals

  , testProperty "packed sint32 pre-allocated path" $ property $ do
      vals <- forAll $ Gen.list (Range.linear 0 100) (Gen.int32 Range.linearBounded)
      let vec = VU.fromList vals
          encoded = buildToBS (encodePackedSVarint32 1 vec)
      if VU.null vec
        then assert (BS.null encoded)
        else case runDecoder (getTag >> decodePackedSVarint32) encoded of
          Left e -> do
            annotate (show e)
            failure
          Right decoded -> VU.toList decoded === vals

  , testProperty "packed sint64 pre-allocated path" $ property $ do
      vals <- forAll $ Gen.list (Range.linear 0 100) (Gen.int64 Range.linearBounded)
      let vec = VU.fromList vals
          encoded = buildToBS (encodePackedSVarint64 1 vec)
      if VU.null vec
        then assert (BS.null encoded)
        else case runDecoder (getTag >> decodePackedSVarint64) encoded of
          Left e -> do
            annotate (show e)
            failure
          Right decoded -> VU.toList decoded === vals
  ]

-- ============================================================
-- SWAR UTF-8 validation tests
-- ============================================================

tdpUTF8Tests :: TestTree
tdpUTF8Tests = testGroup "SWAR UTF-8 validation"
  [ testCase "empty is valid" $
      validateUtf8SWAR BS.empty @?= True

  , testCase "ASCII is valid" $
      validateUtf8SWAR "hello world" @?= True

  , testCase "long ASCII string" $
      validateUtf8SWAR (BS.replicate 1000 0x41) @?= True

  , testCase "valid 2-byte UTF-8" $
      validateUtf8SWAR (BS.pack [0xC3, 0xA9]) @?= True  -- é

  , testCase "valid 3-byte UTF-8" $
      validateUtf8SWAR (BS.pack [0xE2, 0x80, 0x99]) @?= True  -- '

  , testCase "valid 4-byte UTF-8" $
      validateUtf8SWAR (BS.pack [0xF0, 0x9F, 0x98, 0x80]) @?= True  -- 😀

  , testCase "invalid: bare continuation byte" $
      validateUtf8SWAR (BS.pack [0x80]) @?= False

  , testCase "invalid: overlong 2-byte" $
      validateUtf8SWAR (BS.pack [0xC0, 0xAF]) @?= False

  , testCase "invalid: surrogate" $
      validateUtf8SWAR (BS.pack [0xED, 0xA0, 0x80]) @?= False

  , testCase "invalid: truncated 2-byte" $
      validateUtf8SWAR (BS.pack [0xC3]) @?= False

  , testCase "invalid: truncated 3-byte" $
      validateUtf8SWAR (BS.pack [0xE2, 0x80]) @?= False

  , testCase "invalid: byte 0xFF" $
      validateUtf8SWAR (BS.pack [0xFF]) @?= False

  , testCase "mixed ASCII and multibyte" $
      validateUtf8SWAR "hello \xC3\xA9 world \xF0\x9F\x98\x80" @?= True

  , testProperty "all generated unicode text is valid" $ property $ do
      t <- forAll $ Gen.text (Range.linear 0 500) Gen.unicode
      let bs = encodeUtf8 t
      validateUtf8SWAR bs === True
  ]
  where
    encodeUtf8 = Data.Text.Encoding.encodeUtf8

-- ============================================================
-- withTagM CPS tests
-- ============================================================

tdpWithTagMTests :: TestTree
tdpWithTagMTests = testGroup "withTagM CPS dispatch"
  [ testCase "withTagM at EOF returns kEOF" $ do
      let decoder = withTagM (pure True) (\_ _ -> pure False)
      runDecoder decoder BS.empty @?= Right True

  , testCase "withTagM on varint field" $ do
      let bs = buildToBS $ putTag 1 WireVarint <> putVarint 42
          decoder = withTagM
            (pure (0 :: Int, 0 :: Word64))
            (\fn _wt -> do
              val <- getVarint
              pure (fn, val))
      case runDecoder decoder bs of
        Right (fn, val) -> do
          fn @?= 1
          val @?= 42
        Left e -> assertFailure (show e)

  , testCase "withTagM dispatches correctly on wire type" $ do
      let bs = buildToBS $ putTag 5 Wire32Bit <> putFixed32 999
          decoder = withTagM
            (pure Nothing)
            (\fn wt -> do
              if wt == 5  -- Wire32Bit
                then do
                  v <- getFixed32
                  pure (Just (fn, v))
                else do
                  skipWireType wt
                  pure Nothing)
      case runDecoder decoder bs of
        Right (Just (5, 999)) -> pure ()
        other -> assertFailure ("Unexpected: " <> show other)

  , testCase "map entry decode via withTagM" $ do
      let keyEnc = putTag 1 WireVarint <> putVarint 42
          valEnc = putTag 2 WireLengthDelimited <> putText "value"
          encoded = buildToBS (keyEnc <> valEnc)
      case runDecoder (decodeMapEntry getVarint getText 0 "" ) encoded of
        Right (k, v) -> do
          k @?= 42
          v @?= "value"
        Left e -> assertFailure (show e)

  , testCase "map entry with reversed field order" $ do
      let valEnc = putTag 2 WireLengthDelimited <> putText "first"
          keyEnc = putTag 1 WireVarint <> putVarint 7
          encoded = buildToBS (valEnc <> keyEnc)
      case runDecoder (decodeMapEntry getVarint getText 0 "") encoded of
        Right (k, v) -> do
          k @?= 7
          v @?= "first"
        Left e -> assertFailure (show e)
  ]

-- ============================================================
-- Helpers
-- ============================================================

buildToBS :: B.Builder -> ByteString
buildToBS = BL.toStrict . B.toLazyByteString

-- Expose thunk builders for tests
thunkVarintPub :: (Word64 -> TDPValue) -> FieldThunk
thunkVarintPub f bs off =
  case runDecoder' getVarint bs off of
    DecodeOK v off' -> pure (f v, off')
    DecodeFail e    -> error (show e)

thunkFixed32Pub :: (Word32 -> TDPValue) -> FieldThunk
thunkFixed32Pub f bs off =
  case runDecoder' getFixed32 bs off of
    DecodeOK v off' -> pure (f v, off')
    DecodeFail e    -> error (show e)

thunkFixed64Pub :: (Word64 -> TDPValue) -> FieldThunk
thunkFixed64Pub f bs off =
  case runDecoder' getFixed64 bs off of
    DecodeOK v off' -> pure (f v, off')
    DecodeFail e    -> error (show e)

emptyTable :: ParseTable
emptyTable = ParseTable V.empty (BS.replicate 128 0xFF) IntMap.empty 0

testSimpleTable :: ParseTable
testSimpleTable = makeSimpleTable 1 WireVarint (thunkVarintPub TVVarint)

testMultiTable :: ParseTable
testMultiTable = makeMultiTable
  [ (1, WireVarint, thunkVarintPub TVVarint)
  , (2, WireLengthDelimited, thunkLenDelimPub)
  , (3, WireVarint, thunkVarintPub TVVarint)
  ]

testThreeVarintTable :: ParseTable
testThreeVarintTable = makeMultiTable
  [ (1, WireVarint, thunkVarintPub TVVarint)
  , (2, WireVarint, thunkVarintPub TVVarint)
  , (3, WireVarint, thunkVarintPub TVVarint)
  ]

thunkLenDelimPub :: FieldThunk
thunkLenDelimPub bs off =
  case runDecoder' getLengthDelimited bs off of
    DecodeOK v off' -> pure (TVBytes v, off')
    DecodeFail e    -> error (show e)

makeSimpleTable :: Int -> WireType -> FieldThunk -> ParseTable
makeSimpleTable fn wt thunk = makeMultiTable [(fn, wt, thunk)]

makeMultiTable :: [(Int, WireType, FieldThunk)] -> ParseTable
makeMultiTable entries =
  let n = length entries
      parsers = V.fromList
        [ FieldParser
          { fpTag = fieldTag fn wt
          , fpFieldNum = fn
          , fpNextOk = (i + 1) `mod` n
          , fpNextErr = (i + 1) `mod` n
          , fpParse = thunk
          , fpLabel = LabelOptional
          , fpSubmsg = Nothing
          }
        | (i, (fn, wt, thunk)) <- zip [0..] entries
        ]
      tagMap = IntMap.fromList
        [ (fromIntegral (fieldTag fn wt), i)
        | (i, (fn, wt, _)) <- zip [0..] entries
        ]
      lut = BS.pack
        [ case IntMap.lookup (fromIntegral b) tagMap of
            Just idx | idx < 256 -> fromIntegral idx
            _ -> 0xFF
        | b <- [0..127 :: Int]
        ]
  in ParseTable parsers lut tagMap (min 4 n)

-- A test message type with ProtoMessage instance for compile tests
data TestSchemaMsg = TestSchemaMsg
  { tsmValue  :: {-# UNPACK #-} !Word64
  , tsmName   :: !Text
  , tsmActive :: !Bool
  } deriving stock (Show, Eq)

instance MessageEncode TestSchemaMsg where
  buildMessage msg =
    (if tsmValue msg /= 0 then encodeFieldVarint 1 (tsmValue msg) else mempty) <>
    (if tsmName msg /= "" then encodeFieldString 2 (tsmName msg) else mempty) <>
    (if tsmActive msg then encodeFieldBool 3 True else mempty)

instance MessageSize TestSchemaMsg where
  messageSize msg =
    (if tsmValue msg /= 0 then fieldVarintSize 1 (tsmValue msg) else 0) +
    (if tsmName msg /= "" then fieldTextSize 2 (tsmName msg) else 0) +
    (if tsmActive msg then fieldBoolSize 3 else 0)

instance ProtoMessage TestSchemaMsg where
  protoMessageName _ = "test.TestSchemaMsg"
  protoPackageName _ = "test"
  protoDefaultValue = TestSchemaMsg 0 "" False
  protoFieldDescriptors _ = Map.fromList
    [ (1, SomeField FieldDescriptor
        { fdName = "value"
        , fdNumber = 1
        , fdTypeDesc = ScalarType UInt64Field
        , fdLabel = LabelOptional
        , fdGet = tsmValue
        , fdSet = \v m -> m { tsmValue = v }
        })
    , (2, SomeField FieldDescriptor
        { fdName = "name"
        , fdNumber = 2
        , fdTypeDesc = ScalarType StringField
        , fdLabel = LabelOptional
        , fdGet = tsmName
        , fdSet = \v m -> m { tsmName = v }
        })
    , (3, SomeField FieldDescriptor
        { fdName = "active"
        , fdNumber = 3
        , fdTypeDesc = ScalarType BoolField
        , fdLabel = LabelOptional
        , fdGet = tsmActive
        , fdSet = \v m -> m { tsmActive = v }
        })
    ]
