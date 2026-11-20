module Test.TDP (dynamicSchemaTests) where

import Control.Exception (IOException, evaluate, try)
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
import Proto.Decode
import Proto.Dynamic
import Proto.Encode
import Proto.Internal.Wire (Tag (..), WireType (..), fieldTag)
import Proto.Internal.Wire.Decode (
  DecodeError (..),
  DecodeResult (..),
  Decoder,
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
import Proto.Internal.Wire.Decode qualified as WD
import Proto.Internal.Wire.Encode
import Proto.Schema
import System.IO.Error (ioeGetErrorString)
import Test.Syd
import Test.Syd.Hedgehog ()
import Wireform.Builder qualified as B
import Wireform.FFI (countPackedVarints, packedAllSingleByte, validateUtf8SWAR)


dynamicSchemaTests :: Spec
dynamicSchemaTests =
  describe
    "Dynamic Schema-Driven Decoder"
    $ sequence_
      [ coreTests
      , compileTests
      , wireFFITests
      , packedTests
      , utf8Tests
      , withTagMTests
      ]


-- ============================================================
-- TDP core interpreter tests
-- ============================================================

coreTests :: Spec
coreTests =
  describe
    "Core interpreter"
    $ sequence_
      [ it "empty message" $ do
          let result = decodeDynamicWithSchema emptyTable BS.empty
          case result of
            Right msg -> Map.null (dynFields msg) `shouldBe` True
            Left e -> expectationFailure (show e)
      , it "single varint field" $ do
          let bs = buildToBS $ putTag 1 WireVarint <> putVarint 42
              result = decodeDynamicWithSchema testSimpleTable bs
          case result of
            Right msg -> do
              case Map.lookup 1 (dynFields msg) of
                Just (DynVarint 42) -> pure ()
                other -> expectationFailure ("Expected DynVarint 42, got: " <> show other)
            Left e -> expectationFailure (show e)
      , it "multiple fields in order" $ do
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
              Map.lookup 1 (dynFields msg) `shouldBe` Just (DynVarint 100)
              Map.lookup 2 (dynFields msg) `shouldBe` Just (DynBytes (buildToBS (B.byteString "hello")))
              Map.lookup 3 (dynFields msg) `shouldBe` Just (DynVarint 1)
            Left e -> expectationFailure (show e)
      , it "fields out of order" $ do
          let bs =
                buildToBS $
                  putTag 3 WireVarint
                    <> putVarint 99
                    <> putTag 1 WireVarint
                    <> putVarint 42
              result = decodeDynamicWithSchema testMultiTable bs
          case result of
            Right msg -> do
              Map.lookup 1 (dynFields msg) `shouldBe` Just (DynVarint 42)
              Map.lookup 3 (dynFields msg) `shouldBe` Just (DynVarint 99)
            Left e -> expectationFailure (show e)
      , it "unknown fields are skipped" $ do
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
              Map.lookup 1 (dynFields msg) `shouldBe` Just (DynVarint 42)
              Map.member 99 (dynFields msg) `shouldBe` False
            Left e -> expectationFailure (show e)
      , it "last value wins for scalar fields" $ do
          let bs =
                buildToBS $
                  putTag 1 WireVarint
                    <> putVarint 10
                    <> putTag 1 WireVarint
                    <> putVarint 20
              result = decodeDynamicWithSchema testSimpleTable bs
          case result of
            Right msg ->
              Map.lookup 1 (dynFields msg) `shouldBe` Just (DynVarint 20)
            Left e -> expectationFailure (show e)
      , it "the empty-table fallback preserves every occurrence in order" $ do
          let wire = BS.pack [8, 10, 8, 20]
          fmap (dynamicField 1) (decodeDynamicWithSchema emptyTable wire)
            `shouldBe` Right (Just (DynRepeated [DynVarint 10, DynVarint 20]))
      , it "uses the supplied child table with raw or schemaless field thunks" $ do
          let child = makeSimpleTable 1 WireVarint (thunkVarintPub (DynBool . (/= 0)))
              withChild (ParseTable fields lut tags misses) =
                ParseTable (V.map (\fp -> fp {fpSubmsg = Just child}) fields) lut tags misses
              raw = withChild (makeSimpleTable 1 WireLengthDelimited thunkLenDelimPub)
              schemaless bs off = do
                (value, end) <- thunkLenDelimPub bs off
                case value of
                  DynBytes body -> either (fail . show) (\msg -> pure (DynMessage msg, end)) (decodeDynamic body)
                  _ -> fail "expected length-delimited payload"
              wrapped = withChild (makeSimpleTable 1 WireLengthDelimited schemaless)
              wire = BS.pack [10, 2, 8, 0, 10, 2, 8, 2]
              expected = Just (DynMessage (setDynamicField 1 (DynBool True) emptyDynamic))
          fmap (dynamicField 1) (decodeDynamicWithSchema raw wire) `shouldBe` Right expected
          fmap (dynamicField 1) (decodeDynamicWithSchema wrapped wire) `shouldBe` Right expected
      , it "looks up a low tag whose parser index is too large for the byte table" $ do
          let entry number = (number, WireVarint, thunkVarintPub DynVarint)
              table = makeMultiTable (map entry ([16 .. 270] <> [1]))
              wire = BS.pack [8, 42]
          fmap (dynamicField 1) (decodeDynamicWithSchema table wire)
            `shouldBe` Right (Just (DynVarint 42))
      , it "exceptions from custom field thunks reach the caller" $ do
          let table = makeSimpleTable 1 WireVarint (\_ _ -> ioError (userError "custom field thunk"))
          result <- try @IOException (evaluate (decodeDynamicWithSchema table (BS.pack [8, 10])))
          case result of
            Left e -> ioeGetErrorString e `shouldBe` "custom field thunk"
            Right value -> expectationFailure ("Expected custom thunk exception, got " <> show value)
      , it "fixed32 field" $ do
          let bs = buildToBS $ putTag 1 Wire32Bit <> putFixed32 0xDEADBEEF
              table = makeSimpleTable 1 Wire32Bit (thunkFixed32Pub DynFixed32)
              result = decodeDynamicWithSchema table bs
          case result of
            Right msg ->
              Map.lookup 1 (dynFields msg) `shouldBe` Just (DynFixed32 0xDEADBEEF)
            Left e -> expectationFailure (show e)
      , it "fixed64 field" $ do
          let bs = buildToBS $ putTag 1 Wire64Bit <> putFixed64 0xCAFEBABEDEADBEEF
              table = makeSimpleTable 1 Wire64Bit (thunkFixed64Pub DynFixed64)
              result = decodeDynamicWithSchema table bs
          case result of
            Right msg ->
              Map.lookup 1 (dynFields msg) `shouldBe` Just (DynFixed64 0xCAFEBABEDEADBEEF)
            Left e -> expectationFailure (show e)
      , it "varint field roundtrip through TDP" $ property $ do
          val <- forAll $ Gen.word64 (Range.linear 0 maxBound)
          let bs = buildToBS $ putTag 1 WireVarint <> putVarint val
              result = decodeDynamicWithSchema testSimpleTable bs
          case result of
            Right msg ->
              Map.lookup 1 (dynFields msg) === Just (DynVarint val)
            Left e -> do
              annotate (show e)
              failure
      , it "multiple varint fields roundtrip" $ property $ do
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
              Map.lookup 1 (dynFields msg) === Just (DynVarint v1)
              Map.lookup 2 (dynFields msg) === Just (DynVarint v2)
              Map.lookup 3 (dynFields msg) === Just (DynVarint v3)
            Left e -> do
              annotate (show e)
              failure
      ]


-- ============================================================
-- Compile from schema tests
-- ============================================================

compileTests :: Spec
compileTests =
  describe
    "Compilation from schema"
    $ sequence_
      [ it "compileParseTable produces non-empty table" $ do
          let table = compileParseTable (Proxy :: Proxy TestSchemaMsg)
          V.length (ptFields table) `shouldBe` 3
          BS.length (ptTagLUT table) `shouldBe` 128
      , it "TagLUT has entries for small field numbers" $ do
          let table = compileParseTable (Proxy :: Proxy TestSchemaMsg)
              -- Field 1, varint: tag = 0x08
              lut08 = BS.index (ptTagLUT table) 0x08
              -- Field 2, length-delimited: tag = 0x12
              lut12 = BS.index (ptTagLUT table) 0x12
          lut08 /= 0xFF `shouldBe` True
          lut12 /= 0xFF `shouldBe` True
      , it "compiled table decodes matching wire data" $ do
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
              Map.lookup 1 (dynFields msg) `shouldBe` Just (DynVarint 42)
              Map.lookup 3 (dynFields msg) `shouldBe` Just (DynBool True)
            Left e -> expectationFailure (show e)
      , it "compiled table roundtrips with encodeMessage" $ property $ do
          val <- forAll $ Gen.word64 (Range.linear 0 1000)
          name <- forAll $ Gen.text (Range.linear 0 50) Gen.alphaNum
          active <- forAll Gen.bool
          let msg = TestSchemaMsg val name active
              encoded = encodeMessage msg
              table = compileParseTable (Proxy :: Proxy TestSchemaMsg)
              result = decodeDynamicWithSchema table encoded
          case result of
            Right tdpMsg -> do
              case Map.lookup 1 (dynFields tdpMsg) of
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
-- Wire FFI tests (SWAR routines)
-- ============================================================

wireFFITests :: Spec
wireFFITests =
  describe
    "Wire FFI (SWAR)"
    $ sequence_
      [ it "countPackedVarints empty" $
          countPackedVarints BS.empty `shouldBe` 0
      , it "countPackedVarints single byte" $
          countPackedVarints (BS.pack [42]) `shouldBe` 1
      , it "countPackedVarints all single byte" $
          countPackedVarints (BS.pack [0, 1, 2, 3, 4, 5, 6, 7]) `shouldBe` 8
      , it "countPackedVarints two-byte varints" $
          countPackedVarints (BS.pack [0x80, 0x01, 0x80, 0x02]) `shouldBe` 2
      , it "countPackedVarints mixed" $
          countPackedVarints (BS.pack [42, 0x80, 0x01, 99]) `shouldBe` 3
      , it "countPackedVarints matches manual count" $ property $ do
          vals <- forAll $ Gen.list (Range.linear 0 100) (Gen.word64 (Range.linear 0 0xFFFF))
          let encoded = BL.toStrict $ B.toLazyByteString $ foldMap putVarint vals
              expected = length vals
          countPackedVarints encoded === expected
      , it "packedAllSingleByte empty" $
          packedAllSingleByte BS.empty `shouldBe` True
      , it "packedAllSingleByte all small" $
          packedAllSingleByte (BS.pack [0, 1, 42, 127]) `shouldBe` True
      , it "packedAllSingleByte has continuation" $
          packedAllSingleByte (BS.pack [0, 0x80, 1]) `shouldBe` False
      , it "packedAllSingleByte correct for small values" $ property $ do
          vals <- forAll $ Gen.list (Range.linear 0 200) (Gen.word8 (Range.linear 0 127))
          let bs = BS.pack vals
          packedAllSingleByte bs === True
      , it "packedAllSingleByte false when large varint present" $ property $ do
          vals <- forAll $ Gen.list (Range.linear 1 50) (Gen.word64 (Range.linear 128 0xFFFF))
          let encoded = BL.toStrict $ B.toLazyByteString $ foldMap putVarint vals
          packedAllSingleByte encoded === False
      ]


-- ============================================================
-- Packed field decode tests (zero-copy, bulk memcpy paths)
-- ============================================================

packedTests :: Spec
packedTests =
  describe
    "Packed field optimizations"
    $ sequence_
      [ it "packed varint single-byte fast path" $ property $ do
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
      , it "packed varint multi-byte values" $ property $ do
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
      , it "packed fixed32 bulk memcpy path" $ property $ do
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
      , it "packed fixed64 bulk memcpy path" $ property $ do
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
      , it "packed float bulk memcpy path" $ property $ do
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
      , it "packed double bulk memcpy path" $ property $ do
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
      , it "packed sint32 pre-allocated path" $ property $ do
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
      , it "packed sint64 pre-allocated path" $ property $ do
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

utf8Tests :: Spec
utf8Tests =
  describe
    "SWAR UTF-8 validation"
    $ sequence_
      [ it "empty is valid" $
          validateUtf8SWAR BS.empty `shouldBe` True
      , it "ASCII is valid" $
          validateUtf8SWAR "hello world" `shouldBe` True
      , it "long ASCII string" $
          validateUtf8SWAR (BS.replicate 1000 0x41) `shouldBe` True
      , it "valid 2-byte UTF-8" $
          validateUtf8SWAR (BS.pack [0xC3, 0xA9]) `shouldBe` True -- é
      , it "valid 3-byte UTF-8" $
          validateUtf8SWAR (BS.pack [0xE2, 0x80, 0x99]) `shouldBe` True -- '
      , it "valid 4-byte UTF-8" $
          validateUtf8SWAR (BS.pack [0xF0, 0x9F, 0x98, 0x80]) `shouldBe` True -- 😀
      , it "invalid: bare continuation byte" $
          validateUtf8SWAR (BS.pack [0x80]) `shouldBe` False
      , it "invalid: overlong 2-byte" $
          validateUtf8SWAR (BS.pack [0xC0, 0xAF]) `shouldBe` False
      , it "invalid: surrogate" $
          validateUtf8SWAR (BS.pack [0xED, 0xA0, 0x80]) `shouldBe` False
      , it "invalid: truncated 2-byte" $
          validateUtf8SWAR (BS.pack [0xC3]) `shouldBe` False
      , it "invalid: truncated 3-byte" $
          validateUtf8SWAR (BS.pack [0xE2, 0x80]) `shouldBe` False
      , it "invalid: byte 0xFF" $
          validateUtf8SWAR (BS.pack [0xFF]) `shouldBe` False
      , it "mixed ASCII and multibyte" $
          validateUtf8SWAR "hello \xC3\xA9 world \xF0\x9F\x98\x80" `shouldBe` True
      , it "all generated unicode text is valid" $ property $ do
          t <- forAll $ Gen.text (Range.linear 0 500) Gen.unicode
          let bs = encodeUtf8 t
          validateUtf8SWAR bs === True
      ]
  where
    encodeUtf8 = Data.Text.Encoding.encodeUtf8


-- ============================================================
-- withTagM CPS tests
-- ============================================================

withTagMTests :: Spec
withTagMTests =
  describe
    "withTagM CPS dispatch"
    $ sequence_
      [ it "withTagM at EOF returns kEOF" $ do
          let decoder = withTagM (pure True) (\_ _ -> pure False)
          runDecoder decoder BS.empty `shouldBe` Right True
      , it "withTagM on varint field" $ do
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
              fn `shouldBe` 1
              val `shouldBe` 42
            Left e -> expectationFailure (show e)
      , it "withTagM dispatches correctly on wire type" $ do
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
            other -> expectationFailure ("Unexpected: " <> show other)
      , it "map entry decode via withTagM" $ do
          let keyEnc = putTag 1 WireVarint <> putVarint 42
              valEnc = putTag 2 WireLengthDelimited <> putText "value"
              encoded = buildToBS (keyEnc <> valEnc)
          case runDecoder (decodeMapEntry getVarint getText 0 "") encoded of
            Right (k, v) -> do
              k `shouldBe` 42
              v `shouldBe` "value"
            Left e -> expectationFailure (show e)
      , it "map entry with reversed field order" $ do
          let valEnc = putTag 2 WireLengthDelimited <> putText "first"
              keyEnc = putTag 1 WireVarint <> putVarint 7
              encoded = buildToBS (valEnc <> keyEnc)
          case runDecoder (decodeMapEntry getVarint getText 0 "") encoded of
            Right (k, v) -> do
              k `shouldBe` 7
              v `shouldBe` "first"
            Left e -> expectationFailure (show e)
      ]


-- ============================================================
-- Helpers
-- ============================================================

buildToBS :: B.Builder -> ByteString
buildToBS = BL.toStrict . B.toLazyByteString


-- Expose thunk builders for tests
thunkVarintPub :: (Word64 -> DynamicValue) -> FieldThunk
thunkVarintPub f bs off =
  case runDecoder' getVarint bs off of
    DecodeOK v off' -> pure (f v, off')
    DecodeFail e -> error (show e)


thunkFixed32Pub :: (Word32 -> DynamicValue) -> FieldThunk
thunkFixed32Pub f bs off =
  case runDecoder' getFixed32 bs off of
    DecodeOK v off' -> pure (f v, off')
    DecodeFail e -> error (show e)


thunkFixed64Pub :: (Word64 -> DynamicValue) -> FieldThunk
thunkFixed64Pub f bs off =
  case runDecoder' getFixed64 bs off of
    DecodeOK v off' -> pure (f v, off')
    DecodeFail e -> error (show e)


emptyTable :: ParseTable
emptyTable = ParseTable V.empty (BS.replicate 128 0xFF) IntMap.empty 0


testSimpleTable :: ParseTable
testSimpleTable = makeSimpleTable 1 WireVarint (thunkVarintPub DynVarint)


testMultiTable :: ParseTable
testMultiTable =
  makeMultiTable
    [ (1, WireVarint, thunkVarintPub DynVarint)
    , (2, WireLengthDelimited, thunkLenDelimPub)
    , (3, WireVarint, thunkVarintPub DynVarint)
    ]


testThreeVarintTable :: ParseTable
testThreeVarintTable =
  makeMultiTable
    [ (1, WireVarint, thunkVarintPub DynVarint)
    , (2, WireVarint, thunkVarintPub DynVarint)
    , (3, WireVarint, thunkVarintPub DynVarint)
    ]


thunkLenDelimPub :: FieldThunk
thunkLenDelimPub bs off =
  case runDecoder' getLengthDelimited bs off of
    DecodeOK v off' -> pure (DynBytes v, off')
    DecodeFail e -> error (show e)


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
  buildMessage msg =
    (if tsmValue msg /= 0 then encodeFieldVarint 1 (tsmValue msg) else mempty)
      <> (if tsmName msg /= "" then encodeFieldString 2 (tsmName msg) else mempty)
      <> (if tsmActive msg then encodeFieldBool 3 True else mempty)


instance MessageSize TestSchemaMsg where
  messageSize msg =
    (if tsmValue msg /= 0 then fieldVarintSize 1 (tsmValue msg) else 0)
      + (if tsmName msg /= "" then fieldTextSize 2 (tsmName msg) else 0)
      + (if tsmActive msg then fieldBoolSize 3 else 0)


instance ProtoMessage TestSchemaMsg where
  protoMessageName _ = "test.TestSchemaMsg"
  protoPackageName _ = "test"
  protoDefaultValue = TestSchemaMsg 0 "" False
  protoFieldDescriptors _ =
    Map.fromList
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
