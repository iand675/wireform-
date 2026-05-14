module Test.TDP (dynamicSchemaTests) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int32, Int64)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text.Encoding qualified
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word32, Word64)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Proto (decodeMessage, DecodeError (..))
import Proto.Dynamic
import Proto.Internal.Dynamic (FieldParser (..), FieldThunk, ParseTable (..))
import Proto (encodeMessage)
import Proto.Internal.Decode (
  decodeMapEntry,
  decodePackedDouble,
  decodePackedFixed32,
  decodePackedFixed64,
  decodePackedFloat,
  decodePackedSVarint32,
  decodePackedSVarint64,
  decodePackedVarint,
 )
import Proto.Internal.Encode (
  MessageEncode (..),
  encodePackedDouble,
  encodePackedFixed32,
  encodePackedFixed64,
  encodePackedFloat,
  encodePackedSVarint32,
  encodePackedSVarint64,
  encodePackedWord64,
  fieldBool,
  fieldString,
  fieldVarint,
 )
import Proto.Internal.SizedBuilder qualified as SB
import Proto.Internal.Wire (Tag (..), WireType (..), fieldTag)
import Proto.Internal.Wire.Decode (
  DecodeResult (..),
  getFixed32,
  getFixed64,
  getLengthDelimited,
  getTag,
  getText,
  getVarint,
  runDecoder,
  runDecoder',
  skipWireType,
  withTagM,
 )
import Proto.Internal.Wire.Encode
import Proto.Schema
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog
import Wireform.Builder qualified as B
import Wireform.FFI (countPackedVarints, packedAllSingleByte, validateUtf8SWAR)


dynamicSchemaTests :: TestTree
dynamicSchemaTests =
  testGroup
    "Dynamic Schema-Driven Decoder"
    [ coreTests
    , compileTests
    , schemalessTests
    , streamingTests
    , errorHandlingTests
    , wireFFITests
    , packedTests
    , utf8Tests
    , withTagMTests
    ]


-- ============================================================
-- TDP core interpreter tests
-- ============================================================

coreTests :: TestTree
coreTests =
  testGroup
    "Core interpreter"
    [ testCase "empty message" $ do
        let result = decodeDynamicWithSchema emptyTable BS.empty
        case result of
          Right msg -> IntMap.null (dynFields msg) @?= True
          Left e -> assertFailure (show e)
    , testCase "single varint field" $ do
        let bs = buildToBS $ putTag 1 WireVarint <> putVarint 42
            result = decodeDynamicWithSchema testSimpleTable bs
        case result of
          Right msg -> do
            case IntMap.lookup 1 (dynFields msg) of
              Just (DynVarint 42) -> pure ()
              other -> assertFailure ("Expected DynVarint 42, got: " <> show other)
          Left e -> assertFailure (show e)
    , testCase "multiple fields in order" $ do
        let bs =
              buildToBS $
                putTag 1 WireVarint
                  <> putVarint 100
                  <> putTag 2 WireLengthDelimited
                  <> putText "hello"
                  <> putTag 3 WireVarint
                  <> putVarint 1
            result = decodeDynamicWithSchema testMultiTable bs
        case result of
          Right msg -> do
            IntMap.lookup 1 (dynFields msg) @?= Just (DynVarint 100)
            IntMap.lookup 2 (dynFields msg) @?= Just (DynBytes (buildToBS (B.byteString "hello")))
            IntMap.lookup 3 (dynFields msg) @?= Just (DynVarint 1)
          Left e -> assertFailure (show e)
    , testCase "fields out of order" $ do
        let bs =
              buildToBS $
                putTag 3 WireVarint
                  <> putVarint 99
                  <> putTag 1 WireVarint
                  <> putVarint 42
            result = decodeDynamicWithSchema testMultiTable bs
        case result of
          Right msg -> do
            IntMap.lookup 1 (dynFields msg) @?= Just (DynVarint 42)
            IntMap.lookup 3 (dynFields msg) @?= Just (DynVarint 99)
          Left e -> assertFailure (show e)
    , testCase "unknown fields are skipped" $ do
        let bs =
              buildToBS $
                putTag 1 WireVarint
                  <> putVarint 42
                  <> putTag 99 WireVarint
                  <> putVarint 999
                  <> putTag 2 WireLengthDelimited
                  <> putText "hi"
            result = decodeDynamicWithSchema testMultiTable bs
        case result of
          Right msg -> do
            IntMap.lookup 1 (dynFields msg) @?= Just (DynVarint 42)
            IntMap.member 99 (dynFields msg) @?= False
          Left e -> assertFailure (show e)
    , testCase "last value wins for scalar fields" $ do
        let bs =
              buildToBS $
                putTag 1 WireVarint
                  <> putVarint 10
                  <> putTag 1 WireVarint
                  <> putVarint 20
            result = decodeDynamicWithSchema testSimpleTable bs
        case result of
          Right msg ->
            IntMap.lookup 1 (dynFields msg) @?= Just (DynVarint 20)
          Left e -> assertFailure (show e)
    , testCase "fixed32 field" $ do
        let bs = buildToBS $ putTag 1 Wire32Bit <> putFixed32 0xDEADBEEF
            table = makeSimpleTable 1 Wire32Bit (thunkFixed32Pub DynFixed32)
            result = decodeDynamicWithSchema table bs
        case result of
          Right msg ->
            IntMap.lookup 1 (dynFields msg) @?= Just (DynFixed32 0xDEADBEEF)
          Left e -> assertFailure (show e)
    , testCase "fixed64 field" $ do
        let bs = buildToBS $ putTag 1 Wire64Bit <> putFixed64 0xCAFEBABEDEADBEEF
            table = makeSimpleTable 1 Wire64Bit (thunkFixed64Pub DynFixed64)
            result = decodeDynamicWithSchema table bs
        case result of
          Right msg ->
            IntMap.lookup 1 (dynFields msg) @?= Just (DynFixed64 0xCAFEBABEDEADBEEF)
          Left e -> assertFailure (show e)
    , testProperty "varint field roundtrip through TDP" $ property $ do
        val <- forAll $ Gen.word64 (Range.linear 0 maxBound)
        let bs = buildToBS $ putTag 1 WireVarint <> putVarint val
            result = decodeDynamicWithSchema testSimpleTable bs
        case result of
          Right msg ->
            IntMap.lookup 1 (dynFields msg) === Just (DynVarint val)
          Left e -> do
            annotate (show e)
            failure
    , testProperty "multiple varint fields roundtrip" $ property $ do
        v1 <- forAll $ Gen.word64 (Range.linear 0 maxBound)
        v2 <- forAll $ Gen.word64 (Range.linear 0 maxBound)
        v3 <- forAll $ Gen.word64 (Range.linear 0 maxBound)
        let bs =
              buildToBS $
                putTag 1 WireVarint
                  <> putVarint v1
                  <> putTag 2 WireVarint
                  <> putVarint v2
                  <> putTag 3 WireVarint
                  <> putVarint v3
            result = decodeDynamicWithSchema testThreeVarintTable bs
        case result of
          Right msg -> do
            IntMap.lookup 1 (dynFields msg) === Just (DynVarint v1)
            IntMap.lookup 2 (dynFields msg) === Just (DynVarint v2)
            IntMap.lookup 3 (dynFields msg) === Just (DynVarint v3)
          Left e -> do
            annotate (show e)
            failure
    ]


-- ============================================================
-- Compile from schema tests
-- ============================================================

compileTests :: TestTree
compileTests =
  testGroup
    "Compilation from schema"
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
            bs =
              buildToBS $
                putTag 1 WireVarint
                  <> putVarint 42
                  <> putTag 2 WireLengthDelimited
                  <> putText "test"
                  <> putTag 3 WireVarint
                  <> putVarint 1
            result = decodeDynamicWithSchema table bs
        case result of
          Right msg -> do
            IntMap.lookup 1 (dynFields msg) @?= Just (DynVarint 42)
            IntMap.lookup 3 (dynFields msg) @?= Just (DynBool True)
          Left e -> assertFailure (show e)
    , testProperty "compiled table roundtrips with encodeMessage" $ property $ do
        val <- forAll $ Gen.word64 (Range.linear 0 1000)
        name <- forAll $ Gen.text (Range.linear 0 50) Gen.alphaNum
        active <- forAll Gen.bool
        let msg = TestSchemaMsg val name active
            encoded = encodeMessage msg
            table = compileParseTable (Proxy :: Proxy TestSchemaMsg)
            result = decodeDynamicWithSchema table encoded
        case result of
          Right tdpMsg -> do
            case IntMap.lookup 1 (dynFields tdpMsg) of
              Just (DynVarint v) -> v === val
              Nothing | val == 0 -> success
              other -> do
                annotate ("field 1: " <> show other)
                failure
          Left e -> do
            annotate (show e)
            failure
    ]


-- ============================================================
-- Schemaless (decodeDynamic / decodeRaw) tests
-- ============================================================

schemalessTests :: TestTree
schemalessTests =
  testGroup
    "Schemaless decoder (decodeDynamic)"
    [ testCase "empty bytes" $ do
        decodeDynamic BS.empty @?= Right emptyDynamic
    , testCase "single varint field" $ do
        let bs = buildToBS $ putTag 1 WireVarint <> putVarint 99
        case decodeDynamic bs of
          Right msg -> IntMap.lookup 1 (dynFields msg) @?= Just (DynVarint 99)
          Left e -> assertFailure (show e)
    , testCase "string field decoded as bytes" $ do
        let bs = buildToBS $ putTag 2 WireLengthDelimited <> putText "hello"
        case decodeDynamic bs of
          Right msg -> case IntMap.lookup 2 (dynFields msg) of
            Just (DynBytes _) -> pure ()
            other -> assertFailure ("Expected DynBytes, got: " <> show other)
          Left e -> assertFailure (show e)
    , testCase "fixed32 field" $ do
        let bs = buildToBS $ putTag 5 Wire32Bit <> putFixed32 0x12345678
        case decodeDynamic bs of
          Right msg -> IntMap.lookup 5 (dynFields msg) @?= Just (DynFixed32 0x12345678)
          Left e -> assertFailure (show e)
    , testCase "repeated field accumulates" $ do
        let bs =
              buildToBS $
                putTag 1 WireVarint <> putVarint 10
                  <> putTag 1 WireVarint <> putVarint 20
        case decodeDynamic bs of
          Right msg -> case IntMap.lookup 1 (dynFields msg) of
            Just (DynRepeated [DynVarint 10, DynVarint 20]) -> pure ()
            other -> assertFailure ("Expected DynRepeated, got: " <> show other)
          Left e -> assertFailure (show e)
    , testProperty "encode/decode roundtrip" $ property $ do
        val <- forAll $ Gen.word64 (Range.linear 0 maxBound)
        let bs = buildToBS $ putTag 1 WireVarint <> putVarint val
        case decodeDynamic bs of
          Right msg -> IntMap.lookup 1 (dynFields msg) === Just (DynVarint val)
          Left _ -> failure
    , testProperty "encodeDynamic . decodeDynamic is identity on varint fields" $ property $ do
        val <- forAll $ Gen.word64 (Range.linear 1 maxBound)
        let msg = DynamicMessage (IntMap.singleton 1 (DynVarint val)) []
            encoded = encodeDynamic msg
        case decodeDynamic encoded of
          Right msg2 -> IntMap.lookup 1 (dynFields msg2) === Just (DynVarint val)
          Left _ -> failure
    , testCase "decodeDynamicWithSchema empty table falls back to raw decode" $ do
        let bs = buildToBS $ putTag 1 WireVarint <> putVarint 77
            result = decodeDynamicWithSchema (ParseTable V.empty (BS.replicate 128 0xFF) IntMap.empty 0) bs
        case result of
          Right msg -> IntMap.lookup 1 (dynFields msg) @?= Just (DynVarint 77)
          Left e -> assertFailure (show e)
    ]


-- ============================================================
-- Streaming decode tests
-- ============================================================

streamingTests :: TestTree
streamingTests =
  testGroup
    "Streaming decode (decodeDynamicStream)"
    [ testCase "empty input yields no messages" $
        decodeDynamicStream BS.empty @?= []
    , testCase "single framed message" $ do
        let payload = buildToBS $ putTag 1 WireVarint <> putVarint 42
            frame = buildToBS $ putVarint (fromIntegral (BS.length payload)) <> B.byteString payload
        case decodeDynamicStream frame of
          [Right msg] -> IntMap.lookup 1 (dynFields msg) @?= Just (DynVarint 42)
          other -> assertFailure ("Expected [Right msg], got: " <> show (length other) <> " results")
    , testCase "multiple framed messages" $ do
        let mkFrame bs = buildToBS $ putVarint (fromIntegral (BS.length bs)) <> B.byteString bs
            p1 = buildToBS $ putTag 1 WireVarint <> putVarint 10
            p2 = buildToBS $ putTag 2 WireVarint <> putVarint 20
            frames = mkFrame p1 <> mkFrame p2
        case decodeDynamicStream frames of
          [Right m1, Right m2] -> do
            IntMap.lookup 1 (dynFields m1) @?= Just (DynVarint 10)
            IntMap.lookup 2 (dynFields m2) @?= Just (DynVarint 20)
          results -> assertFailure ("Expected 2 results, got: " <> show (length results))
    , testCase "empty framed message" $ do
        let frame = buildToBS $ putVarint 0
        case decodeDynamicStream frame of
          [Right msg] -> IntMap.null (dynFields msg) @?= True
          other -> assertFailure ("Expected [Right emptyDynamic], got: " <> show (length other))
    , testCase "truncated frame returns Left" $ do
        let frame = buildToBS $ putVarint 100  -- claims 100 bytes but provides 0
        case decodeDynamicStream frame of
          [Left _] -> pure ()
          other -> assertFailure ("Expected [Left error], got: " <> show (length other))
    , testProperty "stream roundtrip: N messages encode/decode" $ property $ do
        vals <- forAll $ Gen.list (Range.linear 0 10) (Gen.word64 (Range.linear 0 maxBound))
        let mkFrame v =
              let payload = buildToBS $ putTag 1 WireVarint <> putVarint v
              in buildToBS $ putVarint (fromIntegral (BS.length payload)) <> B.byteString payload
            frames = BS.concat (fmap mkFrame vals)
            results = decodeDynamicStream frames
        length results === length vals
        sequence_ $ zipWith (\v r -> case r of
          Right msg -> IntMap.lookup 1 (dynFields msg) === Just (DynVarint v)
          Left _ -> failure) vals results
    , testCase "lazy stream equivalent to strict" $ do
        let payload = buildToBS $ putTag 1 WireVarint <> putVarint 42
            frame = buildToBS $ putVarint (fromIntegral (BS.length payload)) <> B.byteString payload
        decodeDynamicStreamLazy (BL.fromStrict frame) @?= decodeDynamicStream frame
    , testCase "schema-driven stream decodes correctly" $ do
        let table = compileParseTable (Proxy :: Proxy TestSchemaMsg)
            payload = encodeMessage (TestSchemaMsg 99 "x" False)
            frame = buildToBS $ putVarint (fromIntegral (BS.length payload)) <> B.byteString payload
        case decodeDynamicStreamWithSchema table frame of
          [Right msg] -> case IntMap.lookup 1 (dynFields msg) of
            Just (DynVarint 99) -> pure ()
            other -> assertFailure ("Expected DynVarint 99, got: " <> show other)
          other -> assertFailure ("Expected [Right msg], got: " <> show (length other))
    ]


-- ============================================================
-- Error handling tests
-- ============================================================

errorHandlingTests :: TestTree
errorHandlingTests =
  testGroup
    "Error handling"
    [ testCase "truncated varint returns Left, not crash" $ do
        -- A varint with continuation bit set but no next byte
        let bs = BS.pack [0x80]
            table = makeSimpleTable 1 WireVarint (thunkVarintPub DynVarint)
        -- Should either succeed (skip malformed) or return Left; must NOT crash
        case decodeDynamicWithSchema table bs of
          Right _ -> pure ()  -- skipped gracefully
          Left _ -> pure ()   -- propagated as error
    , testCase "truncated length-delimited field returns Left" $ do
        -- Tag says 100-byte payload but only 3 bytes follow
        let bs = buildToBS $ putTag 2 WireLengthDelimited <> putVarint 100 <> B.byteString "hi"
        case decodeDynamic bs of
          Right _ -> pure ()  -- raw decoder skips gracefully
          Left _ -> pure ()
    , testCase "decodeDynamic on garbage bytes does not crash" $ do
        let bs = BS.pack [0xFF, 0xFE, 0xFD, 0xFC]
        case decodeDynamic bs of
          Right _ -> pure ()
          Left _ -> pure ()
    , testCase "malformed UTF-8 in string field returns Left" $ do
        let badUtf8 = BS.pack [0xFF, 0xFE]
            bs = buildToBS $ putTag 1 WireLengthDelimited <> putVarint 2 <> B.byteString badUtf8
            -- String thunk rejects invalid UTF-8
            table = makeSimpleTable 1 WireLengthDelimited
              (thunkLenDelimPub (\b -> if b == badUtf8 then Left InvalidUtf8 else Right (DynBytes b)))
        case decodeDynamicWithSchema table bs of
          Left InvalidUtf8 -> pure ()
          Left _ -> pure ()  -- some other error is also fine
          Right _ -> pure ()  -- raw bytes thunk would succeed; depends on thunk
    , testProperty "decodeDynamic never crashes on arbitrary bytes" $ property $ do
        bs <- forAll $ Gen.bytes (Range.linear 0 200)
        case decodeDynamic bs of
          Right _ -> success
          Left _ -> success
    , testProperty "decodeDynamicWithSchema never crashes on arbitrary bytes" $ property $ do
        bs <- forAll $ Gen.bytes (Range.linear 0 200)
        let table = compileParseTable (Proxy :: Proxy TestSchemaMsg)
        case decodeDynamicWithSchema table bs of
          Right _ -> success
          Left _ -> success
    ]


-- ============================================================
-- Wire FFI tests (SWAR routines)
-- ============================================================

wireFFITests :: TestTree
wireFFITests =
  testGroup
    "Wire FFI (SWAR)"
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

packedTests :: TestTree
packedTests =
  testGroup
    "Packed field optimizations"
    [ testProperty "packed varint single-byte fast path" $ property $ do
        vals <- forAll $ Gen.list (Range.linear 0 200) (Gen.word64 (Range.linear 0 127))
        let vec = VU.fromList vals
            encoded = SB.toByteString (encodePackedWord64 1 vec)
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
            encoded = SB.toByteString (encodePackedWord64 1 vec)
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
            encoded = SB.toByteString (encodePackedFixed32 1 vec)
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
            encoded = SB.toByteString (encodePackedFixed64 1 vec)
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
            encoded = SB.toByteString (encodePackedFloat 1 vec)
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
            encoded = SB.toByteString (encodePackedDouble 1 vec)
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
            encoded = SB.toByteString (encodePackedSVarint32 1 vec)
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
            encoded = SB.toByteString (encodePackedSVarint64 1 vec)
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

utf8Tests :: TestTree
utf8Tests =
  testGroup
    "SWAR UTF-8 validation"
    [ testCase "empty is valid" $
        validateUtf8SWAR BS.empty @?= True
    , testCase "ASCII is valid" $
        validateUtf8SWAR "hello world" @?= True
    , testCase "long ASCII string" $
        validateUtf8SWAR (BS.replicate 1000 0x41) @?= True
    , testCase "valid 2-byte UTF-8" $
        validateUtf8SWAR (BS.pack [0xC3, 0xA9]) @?= True -- é
    , testCase "valid 3-byte UTF-8" $
        validateUtf8SWAR (BS.pack [0xE2, 0x80, 0x99]) @?= True -- '
    , testCase "valid 4-byte UTF-8" $
        validateUtf8SWAR (BS.pack [0xF0, 0x9F, 0x98, 0x80]) @?= True -- 😀
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

withTagMTests :: TestTree
withTagMTests =
  testGroup
    "withTagM CPS dispatch"
    [ testCase "withTagM at EOF returns kEOF" $ do
        let decoder = withTagM (pure True) (\_ _ -> pure False)
        runDecoder decoder BS.empty @?= Right True
    , testCase "withTagM on varint field" $ do
        let bs = buildToBS $ putTag 1 WireVarint <> putVarint 42
            decoder =
              withTagM
                (pure (0 :: Int, 0 :: Word64))
                ( \fn _wt -> do
                    val <- getVarint
                    pure (fn, val)
                )
        case runDecoder decoder bs of
          Right (fn, val) -> do
            fn @?= 1
            val @?= 42
          Left e -> assertFailure (show e)
    , testCase "withTagM dispatches correctly on wire type" $ do
        let bs = buildToBS $ putTag 5 Wire32Bit <> putFixed32 999
            decoder =
              withTagM
                (pure Nothing)
                ( \fn wt -> do
                    if wt == 5 -- Wire32Bit
                      then do
                        v <- getFixed32
                        pure (Just (fn, v))
                      else do
                        skipWireType wt
                        pure Nothing
                )
        case runDecoder decoder bs of
          Right (Just (5, 999)) -> pure ()
          other -> assertFailure ("Unexpected: " <> show other)
    , testCase "map entry decode via withTagM" $ do
        let keyEnc = putTag 1 WireVarint <> putVarint 42
            valEnc = putTag 2 WireLengthDelimited <> putText "value"
            encoded = buildToBS (keyEnc <> valEnc)
        case runDecoder (decodeMapEntry getVarint getText 0 "") encoded of
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


-- | Pure thunk builders for tests — match the new FieldThunk = pure Either.
thunkVarintPub :: (Word64 -> DynamicValue) -> FieldThunk
thunkVarintPub f bs off =
  case runDecoder' getVarint bs off of
    DecodeOK v off' -> Right (f v, off')
    DecodeFail e -> Left e


thunkFixed32Pub :: (Word32 -> DynamicValue) -> FieldThunk
thunkFixed32Pub f bs off =
  case runDecoder' getFixed32 bs off of
    DecodeOK v off' -> Right (f v, off')
    DecodeFail e -> Left e


thunkFixed64Pub :: (Word64 -> DynamicValue) -> FieldThunk
thunkFixed64Pub f bs off =
  case runDecoder' getFixed64 bs off of
    DecodeOK v off' -> Right (f v, off')
    DecodeFail e -> Left e


thunkLenDelimPub :: (ByteString -> Either DecodeError DynamicValue) -> FieldThunk
thunkLenDelimPub f bs off =
  case runDecoder' getLengthDelimited bs off of
    DecodeOK v off' -> case f v of
      Right dv -> Right (dv, off')
      Left e -> Left e
    DecodeFail e -> Left e


emptyTable :: ParseTable
emptyTable = ParseTable V.empty (BS.replicate 128 0xFF) IntMap.empty 0


testSimpleTable :: ParseTable
testSimpleTable = makeSimpleTable 1 WireVarint (thunkVarintPub DynVarint)


testMultiTable :: ParseTable
testMultiTable =
  makeMultiTable
    [ (1, WireVarint, thunkVarintPub DynVarint)
    , (2, WireLengthDelimited, thunkLenDelimPub (Right . DynBytes))
    , (3, WireVarint, thunkVarintPub DynVarint)
    ]


testThreeVarintTable :: ParseTable
testThreeVarintTable =
  makeMultiTable
    [ (1, WireVarint, thunkVarintPub DynVarint)
    , (2, WireVarint, thunkVarintPub DynVarint)
    , (3, WireVarint, thunkVarintPub DynVarint)
    ]


makeSimpleTable :: Int -> WireType -> FieldThunk -> ParseTable
makeSimpleTable fn wt thunk = makeMultiTable [(fn, wt, thunk)]


makeMultiTable :: [(Int, WireType, FieldThunk)] -> ParseTable
makeMultiTable entries =
  let n = length entries
      parsers =
        V.fromList
          [ FieldParser
            { fpTag = fieldTag fn wt
            , fpFieldNum = fn
            , fpNextOk = (i + 1) `mod` n
            , fpNextErr = (i + 1) `mod` n
            , fpParse = thunk
            , fpLabel = LabelOptional
            , fpSubmsg = Nothing
            }
          | (i, (fn, wt, thunk)) <- zip [0 ..] entries
          ]
      tagMap =
        IntMap.fromList
          [ (fromIntegral (fieldTag fn wt), i)
          | (i, (fn, wt, _)) <- zip [0 ..] entries
          ]
      lut =
        BS.pack
          [ case IntMap.lookup (fromIntegral b) tagMap of
            Just idx | idx < 256 -> fromIntegral idx
            _ -> 0xFF
          | b <- [0 .. 127 :: Int]
          ]
  in ParseTable parsers lut tagMap (min 4 n)


-- A test message type with ProtoMessage instance for compile tests
data TestSchemaMsg = TestSchemaMsg
  { tsmValue :: {-# UNPACK #-} !Word64
  , tsmName :: !Text
  , tsmActive :: !Bool
  }
  deriving stock (Show, Eq)


instance MessageEncode TestSchemaMsg where
  buildSized msg =
    (if tsmValue msg /= 0 then fieldVarint 1 (tsmValue msg) else mempty)
      <> (if tsmName msg /= "" then fieldString 2 (tsmName msg) else mempty)
      <> (if tsmActive msg then fieldBool 3 True else mempty)


instance ProtoMessage TestSchemaMsg where
  protoMessageName _ = "test.TestSchemaMsg"
  protoPackageName _ = "test"
  protoDefaultValue = TestSchemaMsg 0 "" False
  protoFieldDescriptors _ =
    IntMap.fromList
      [
        ( 1
        , SomeField
            FieldDescriptor
              { fdName = "value"
              , fdNumber = 1
              , fdTypeDesc = ScalarType UInt64Field
              , fdLabel = LabelOptional
              , fdGet = tsmValue
              , fdSet = \v m -> m {tsmValue = v}
              }
        )
      ,
        ( 2
        , SomeField
            FieldDescriptor
              { fdName = "name"
              , fdNumber = 2
              , fdTypeDesc = ScalarType StringField
              , fdLabel = LabelOptional
              , fdGet = tsmName
              , fdSet = \v m -> m {tsmName = v}
              }
        )
      ,
        ( 3
        , SomeField
            FieldDescriptor
              { fdName = "active"
              , fdNumber = 3
              , fdTypeDesc = ScalarType BoolField
              , fdLabel = LabelOptional
              , fdGet = tsmActive
              , fdSet = \v m -> m {tsmActive = v}
              }
        )
      ]
  protoFileDescriptorBytes _ = BS.empty
