{-# LANGUAGE OverloadedStrings #-}

module Test.Thrift.CompactBool (tests) where

import Control.Exception (evaluate)
import Control.Monad (forM_, when)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Lazy qualified as BL
import Data.Either (isLeft)
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Data.Vector qualified as V
import Data.Vector.Generic.Base qualified as VG
import Data.Word (Word32)
import Foreign.ForeignPtr (mallocForeignPtrBytes)
import GHC.Conc (getAllocationCounter)
import GHC.Float (castWord64ToDouble)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Numeric (readHex)
import Test.Syd
import Test.Syd.Hedgehog ()
import Thrift.Decode (decodeBinary, decodeCompact, decodeCompactBinaryStrings, decodeCompactFrom)
import Thrift.Encode (binaryEncodedSizeBounded, compactEncodedSizeBounded, encodeBinaryBounded, encodeCompact, encodeCompactBounded)
import Thrift.Value qualified as Value
import Thrift.Wire qualified as Wire
import Wireform.Builder qualified as Builder


tests :: Spec
tests = describe "Compact container booleans" $ do
  encodeBoundsTests
  protocolGoldenTests
  let list = Value.Struct (V.singleton (1, Value.List Wire.TT_BOOL (V.fromList [Value.Bool False, Value.Bool True, Value.Bool False])))
      listBytes = BS.pack [0x19, 0x31, 2, 1, 2, 0]
      set = Value.Struct (V.singleton (1, Value.Set Wire.TT_BOOL (V.fromList [Value.Bool False, Value.Bool True])))
      setBytes = BS.pack [0x1a, 0x21, 2, 1, 0]
      mapValue =
        Value.Struct
          ( V.singleton
              ( 1
              , Value.Map
                  Wire.TT_BOOL
                  Wire.TT_BOOL
                  (V.fromList [(Value.Bool False, Value.Bool True), (Value.Bool True, Value.Bool False)])
              )
          )
      mapBytes = BS.pack [0x1b, 2, 0x11, 2, 1, 1, 2, 0]
  it "writes the published list<bool> byte sequence" $ encodeCompact list `shouldBe` listBytes
  it "reads standard false/true/false list elements" $ decodeCompact listBytes `shouldBe` Right list
  it "writes standard boolean set elements" $ encodeCompact set `shouldBe` setBytes
  it "reads standard boolean set elements" $ decodeCompact setBytes `shouldBe` Right set
  it "writes standard boolean map keys and values" $ encodeCompact mapValue `shouldBe` mapBytes
  it "reads standard boolean map keys and values" $ decodeCompact mapBytes `shouldBe` Right mapValue
  it "uses marker2 in the builder API as well" $
    BL.toStrict (Builder.toLazyByteString (Wire.tCompEncodeBool False)) `shouldBe` BS.singleton 2
  it "retains binary protocol's separate false marker" $
    BL.toStrict (Builder.toLazyByteString (Wire.tBinEncodeBool False)) `shouldBe` BS.singleton 0
  it "reads old wireform zero markers but rewrites standard markers" $ do
    decodeCompact (BS.pack [0x19, 0x31, 0, 1, 0, 0]) `shouldBe` Right list
    encodeCompact list `shouldBe` listBytes
  it "rejects invalid markers and offsets" $ do
    Wire.tCompDecodeBool (BS.singleton 3) 0 `shouldBe` Nothing
    Wire.tCompDecodeBool (BS.singleton 1) (-1) `shouldBe` Nothing
    Wire.tCompDecodeBool (BS.singleton 1) 1 `shouldBe` Nothing
  it "preserves arbitrary mixed boolean lists" $ property $ do
    values <- forAll (Gen.list (Range.linear 0 100) Gen.bool)
    let value = Value.Struct (V.singleton (1, Value.List Wire.TT_BOOL (V.fromList (Value.Bool <$> values))))
    decodeCompact (encodeCompact value) === Right value
  describe "opaque Compact strings" $ do
    let nested payloads =
          let binaries = Value.Binary <$> V.fromList payloads
              bools = Value.List Wire.TT_BOOL (V.fromList [Value.Bool False, Value.Bool True])
              -- Compact omits type tags for an empty map.
              opaqueMap key value entries
                | V.null entries = Value.Map Wire.TT_STOP Wire.TT_STOP entries
                | otherwise = Value.Map key value entries
          in Value.Struct
               ( V.fromList
                   [ (1, Value.List Wire.TT_STRING binaries)
                   , (2, Value.Set Wire.TT_STRING binaries)
                   , (3, opaqueMap Wire.TT_STRING Wire.TT_LIST (V.map (\b -> (b, bools)) binaries))
                   , (4, Value.Struct (V.singleton (1, Value.List Wire.TT_STRING binaries)))
                   , (5, opaqueMap Wire.TT_LIST Wire.TT_STRING (V.map (\b -> (bools, b)) binaries))
                   ]
               )
    it "preserves UTF-8, NULs, empty and non-UTF-8 bytes in every container" $ do
      let value = nested [BS.empty, "plain", Text.encodeUtf8 "é雪", BS.pack [0, 1, 0, 127], BS.pack [255, 192, 128]]
      decodeCompactBinaryStrings (encodeCompact value) `shouldBe` Right value
    it "keeps the default decoder's text interpretation" $ do
      let value = Value.Struct (V.singleton (1, Value.String "plain-é"))
          binary = Value.Struct (V.singleton (1, Value.Binary (Text.encodeUtf8 "plain-é")))
          bytes = encodeCompact value
      decodeCompact bytes `shouldBe` Right value
      decodeCompactBinaryStrings bytes `shouldBe` Right binary
    it "preserves arbitrary sliced binary payloads in nested containers" $ property $ do
      payloads <- forAll (Gen.list (Range.linear 0 40) (Gen.bytes (Range.linear 0 256)))
      wirePrefix <- forAll (Gen.bytes (Range.linear 1 16))
      let value = nested payloads
          wire = BS.drop (BS.length wirePrefix) (wirePrefix <> encodeCompact value)
      decodeCompactBinaryStrings wire === Right value
  it "rejects impossible compact container counts before allocating" $
    mapM_
      (\bytes -> isLeft (decodeCompact (BS.pack bytes)) `shouldBe` True)
      [ [0x19, 0xf1, 0xff, 0xff, 0xff, 0xff, 0x07, 0]
      , [0x1a, 0xf1, 0xff, 0xff, 0xff, 0xff, 0x07, 0]
      , [0x1b, 0xff, 0xff, 0xff, 0xff, 0x07, 0x11, 0]
      , [0x19, 0xf1, 0x80, 0x80, 0x80, 0x80, 0x08, 0]
      , [0x1a, 0xf1, 0x80, 0x80, 0x80, 0x80, 0x08, 0]
      , [0x1b, 0x80, 0x80, 0x80, 0x80, 0x08, 0x11, 0]
      , [0x19, 0xf1, 0x80, 0x80, 0x80, 0x80, 0x10, 0]
      , [0x1a, 0xf1, 0x80, 0x80, 0x80, 0x80, 0x10, 0]
      , [0x1b, 0x80, 0x80, 0x80, 0x80, 0x10, 0]
      , [0x19, 0xf1, 0x81, 0x80, 0x80, 0x80, 0x10, 1, 0]
      , [0x1a, 0xf1, 0x81, 0x80, 0x80, 0x80, 0x10, 1, 0]
      , [0x1b, 0x81, 0x80, 0x80, 0x80, 0x10, 0x11, 1, 1, 0]
      ]
  it "rejects overflowing varints, wrapped string lengths and negative root offsets" $ do
    Wire.tCompDecodeVarint (BS.pack (replicate 9 0x80 ++ [2])) 0 `shouldBe` Nothing
    Wire.tCompDecodeVarint (BS.singleton 0) (-1) `shouldBe` Nothing
    Wire.tCompDecodeString (BS.pack [0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 1]) 0 `shouldBe` Nothing
    Wire.tCompDecodeListBegin (BS.singleton 0) (-1) `shouldBe` Nothing
    isLeft (decodeCompactFrom (BS.singleton 0) (-1)) `shouldBe` True
  it "rejects negative and impossible binary container counts" $
    mapM_
      (\bytes -> isLeft (decodeBinary (BS.pack bytes)) `shouldBe` True)
      [ [15, 0, 1, 2, 0xff, 0xff, 0xff, 0xff, 0]
      , [14, 0, 1, 2, 0xff, 0xff, 0xff, 0xff, 0]
      , [13, 0, 1, 2, 2, 0xff, 0xff, 0xff, 0xff, 0]
      , [15, 0, 1, 2, 0x7f, 0xff, 0xff, 0xff, 0]
      , [14, 0, 1, 2, 0x7f, 0xff, 0xff, 0xff, 0]
      , [13, 0, 1, 2, 2, 0x7f, 0xff, 0xff, 0xff, 0]
      ]


data EncoderCase = EncoderCase
  { encoderLabel :: !String
  , boundedEncode :: !(Int -> Value.Value -> Either String BS.ByteString)
  , boundedSize :: !(Int -> Value.Value -> Either String Int)
  }


encoderCases :: [EncoderCase]
encoderCases = [EncoderCase "Binary" encodeBinaryBounded binaryEncodedSizeBounded, EncoderCase "Compact" encodeCompactBounded compactEncodedSizeBounded]


encodeBoundsTests :: Spec
encodeBoundsTests = describe "Bounded Binary/Compact encoders" $ do
  forM_ encoderCases $ \encoder -> describe (encoderLabel encoder) $ do
    it "validates UUID width before writing a fixed-size buffer" $
      forM_ [0, 1, 15, 17, 32] $ \n ->
        boundedEncode encoder maxBound (Value.UUID (BS.replicate n 0))
          `shouldBe` Left "Thrift.Encode: UUID must contain exactly 16 bytes"
    it "writes exactly sixteen sliced UUID bytes" $ do
      let bytes = BS.pack [0 .. 15]
          sliced = BS.take 16 (BS.drop 3 ("pad" <> bytes <> "tail"))
      boundedEncode encoder 16 (Value.UUID sliced) `shouldBe` Right bytes
      isLeft (boundedEncode encoder 15 (Value.UUID sliced)) `shouldBe` True
    it "rejects negative budgets and mismatched container tags" $ do
      isLeft (boundedEncode encoder (-1) (Value.Bool True)) `shouldBe` True
      forM_
        [ Value.List Wire.TT_I32 (V.singleton (Value.Bool True))
        , Value.Set Wire.TT_STRING (V.singleton (Value.I64 1))
        , Value.Map Wire.TT_BOOL Wire.TT_I32 (V.singleton (Value.I32 1, Value.I32 2))
        , Value.Map Wire.TT_BOOL Wire.TT_I32 (V.singleton (Value.Bool True, Value.String "x"))
        ]
        (\value -> isLeft (boundedEncode encoder maxBound value) `shouldBe` True)
    it "measures UTF8 bytes without allocating the encoded text" $ do
      let text = T.replicate (1024 * 1024) "é"
      _ <- evaluate (T.length text)
      allocationBefore <- getAllocationCounter
      rejected <- evaluate (isLeft (boundedEncode encoder 32 (Value.String text)))
      allocationAfter <- getAllocationCounter
      rejected `shouldBe` True
      (allocationBefore - allocationAfter < 65536) `shouldBe` True
    it "matches actual encoded lengths and exact byte budgets for nested values" $ property $ do
      value <- forAll (genValue 3)
      bytes <- evalEither (boundedEncode encoder maxBound value)
      let size = BS.length bytes
      boundedSize encoder maxBound value === Right size
      boundedEncode encoder size value === Right bytes
      assert (isLeft (boundedEncode encoder (size - 1) value))
    it "checks impossible container views before traversing backing storage" $ do
      let elements = VG.basicUnsafeSlice 0 maxBound (V.singleton (Value.Bool True))
          entries = VG.basicUnsafeSlice 0 maxBound (V.singleton (Value.Bool True, Value.Bool False))
          fields = VG.basicUnsafeSlice 0 maxBound (V.singleton (1, Value.Bool True))
      forM_ [Value.List Wire.TT_BOOL elements, Value.Set Wire.TT_BOOL elements, Value.Map Wire.TT_BOOL Wire.TT_BOOL entries, Value.Struct fields] $ \value ->
        isLeft (boundedEncode encoder 16 value) `shouldBe` True
  it "preserves compact double bits at an unaligned byte offset" $ do
    let number = castWord64ToDouble 0x7ff8000000000042
        value = Value.Struct (V.fromList [(1, Value.Byte 0), (2, Value.Double number)])
    bytes <- either fail pure (encodeCompactBounded maxBound value)
    BS.take 8 (BS.drop 3 bytes) `shouldBe` BS.pack [66, 0, 0, 0, 0, 0, 248, 127]
  it "uses full field headers across the signed16 field-id range" $ do
    let value = Value.Struct (V.fromList [(maxBound, Value.I32 1), (minBound, Value.I32 2), (0, Value.Bool False)])
    bytes <- either fail pure (encodeCompactBounded maxBound value)
    compactEncodedSizeBounded maxBound value `shouldBe` Right (BS.length bytes)
    decodeCompact bytes `shouldBe` Right (Value.Struct (V.fromList [(minBound, Value.I32 2), (0, Value.Bool False), (maxBound, Value.I32 1)]))
  it "checks signed Binary and unsigned Compact length fields before copying" $
    when (toInteger (maxBound :: Int) > toInteger (maxBound :: Word32)) $ do
      storage <- mallocForeignPtrBytes 1
      let bytes n = Value.Binary (BSI.fromForeignPtr storage 0 n)
          signed = 2147483647
          unsigned = 4294967295
      binaryEncodedSizeBounded maxBound (bytes signed) `shouldBe` Right (signed + 4)
      isLeft (encodeBinaryBounded maxBound (bytes (signed + 1))) `shouldBe` True
      compactEncodedSizeBounded maxBound (bytes unsigned) `shouldBe` Right (unsigned + 5)
      isLeft (encodeCompactBounded maxBound (bytes (unsigned + 1))) `shouldBe` True
      isLeft (encodeCompactBounded 1024 (bytes unsigned)) `shouldBe` True
  it "rejects cumulative sizes without materializing repeated large payloads" $ do
    let bytes = BS.replicate (1024 * 1024) 0
        value = Value.List Wire.TT_STRING (V.replicate 4096 (Value.Binary bytes))
    _ <- evaluate (BS.length bytes)
    forM_ encoderCases $ \encoder -> do
      allocationBefore <- getAllocationCounter
      rejected <- evaluate (isLeft (boundedEncode encoder 0xffffffff value))
      allocationAfter <- getAllocationCounter
      rejected `shouldBe` True
      (allocationBefore - allocationAfter < 2 * 1024 * 1024) `shouldBe` True


genValue :: Int -> Gen Value.Value
genValue depth = do
  typ <- Gen.element (scalarTypes ++ if depth <= 0 then [] else [Wire.TT_STRUCT, Wire.TT_LIST, Wire.TT_SET, Wire.TT_MAP])
  genTyped depth typ
  where
    scalarTypes = [Wire.TT_BOOL, Wire.TT_BYTE, Wire.TT_I16, Wire.TT_I32, Wire.TT_I64, Wire.TT_DOUBLE, Wire.TT_STRING, Wire.TT_UUID]


genTyped :: Int -> Wire.ThriftType -> Gen Value.Value
genTyped depth typ = case typ of
  Wire.TT_BOOL -> Value.Bool <$> Gen.bool
  Wire.TT_BYTE -> Value.Byte <$> Gen.int8 Range.linearBounded
  Wire.TT_I16 -> Value.I16 <$> Gen.int16 Range.linearBounded
  Wire.TT_I32 -> Value.I32 <$> Gen.int32 Range.linearBounded
  Wire.TT_I64 -> Value.I64 <$> Gen.int64 Range.linearBounded
  Wire.TT_DOUBLE -> Value.Double <$> Gen.double (Range.linearFrac (-10000) 10000)
  Wire.TT_STRING -> Gen.choice [Value.Binary <$> Gen.bytes (Range.linear 0 64), Value.String <$> Gen.text (Range.linear 0 32) Gen.unicode]
  Wire.TT_UUID -> Value.UUID <$> Gen.bytes (Range.singleton 16)
  Wire.TT_STRUCT -> do
    fields <- Gen.list (Range.linear 0 6) $ do
      fid <- Gen.int16 Range.linearBounded
      value <- genValue (depth - 1)
      pure (fid, value)
    pure (Value.Struct (V.fromList fields))
  Wire.TT_LIST -> sequenceOf Value.List
  Wire.TT_SET -> sequenceOf Value.Set
  Wire.TT_MAP -> do
    key <- genValue (depth - 1)
    value <- genValue (depth - 1)
    let kt = Value.thriftTypeOf key
        vt = Value.thriftTypeOf value
    entries <- Gen.list (Range.linear 0 6) ((,) <$> genTyped (depth - 1) kt <*> genTyped (depth - 1) vt)
    pure (Value.Map kt vt (V.fromList entries))
  Wire.TT_STOP -> pure (Value.Struct V.empty)
  where
    sequenceOf constructor = do
      sample <- genValue (depth - 1)
      let elementType = Value.thriftTypeOf sample
      elements <- V.fromList <$> Gen.list (Range.linear 0 8) (genTyped (depth - 1) elementType)
      pure (constructor elementType elements)


-- Independent expected bytes are generated by Apache's Python implementation.
-- Keep these values separate from the Python fixture definitions so that a
-- writer regression cannot update its own expected wire representation.
data ProtocolFixture = ProtocolFixture
  { protocolFixtureName :: !String
  , protocolFixtureValue :: !Value.Value
  }


protocolGoldenTests :: Spec
protocolGoldenTests = describe "Apache Thrift protocol goldens" $
  it "matches both reference protocols for scalars, field ids and nested containers" $ do
    contents <- readFile "test/fixtures/protocol-encodings.tsv"
    let dataRow [] = False
        dataRow ('#' : _) = False
        dataRow _ = True
        rows = filter dataRow (lines contents)
    length rows `shouldBe` length protocolFixtures
    forM_ rows $ \line -> case words line of
      [name, binaryHex, compactHex] -> case filter ((== name) . protocolFixtureName) protocolFixtures of
        [fixture] -> do
          binary <- parseHex binaryHex
          compact <- parseHex compactHex
          encodeBinaryBounded maxBound (protocolFixtureValue fixture) `shouldBe` Right binary
          encodeCompactBounded maxBound (protocolFixtureValue fixture) `shouldBe` Right compact
        _ -> fail ("unknown or duplicate protocol fixture: " ++ name)
      _ -> fail "malformed protocol fixture row"
  where
    parseHex [] = pure BS.empty
    parseHex (a : b : rest) = case readHex [a, b] of
      [(byte, "")] -> BS.cons byte <$> parseHex rest
      _ -> fail "invalid protocol fixture hex"
    parseHex _ = fail "odd protocol fixture hex length"


protocolFixtures :: [ProtocolFixture]
protocolFixtures =
  [ ProtocolFixture "scalars" $
      struct
        [ (1, Value.Bool False)
        , (2, Value.Bool True)
        , (3, Value.Byte minBound)
        , (4, Value.I16 minBound)
        , (5, Value.I32 minBound)
        , (6, Value.I64 minBound)
        , (7, Value.Double (-123.5))
        , (8, Value.String "é雪\0")
        , (9, Value.Binary (BS.pack [0, 255, 128, 1]))
        , (10, Value.UUID (BS.pack [0 .. 15]))
        ]
  , ProtocolFixture "field-ids" $
      struct [(maxBound, Value.I32 1), (minBound, Value.I32 2), (0, Value.Bool False)]
  , ProtocolFixture "container-booleans" $
      struct
        [ (1, Value.List Wire.TT_BOOL (V.fromList [Value.Bool False, Value.Bool True, Value.Bool False]))
        , (2, Value.Set Wire.TT_BOOL (V.fromList [Value.Bool False, Value.Bool True]))
        , (3, Value.Map Wire.TT_BOOL Wire.TT_BOOL (V.fromList [(Value.Bool False, Value.Bool True), (Value.Bool True, Value.Bool False)]))
        ]
  ]
    ++ map
      (\count -> ProtocolFixture ("list-" ++ show count) (struct [(1, Value.List Wire.TT_I32 (V.fromList (map (Value.I32 . fromIntegral) [-64 .. count - 65])))]))
      ([0, 14, 15, 128] :: [Int])
    ++ [ ProtocolFixture "nested" $
           struct
             [ (1, Value.List Wire.TT_STRUCT (V.fromList [struct [(1, Value.String "first"), (2, Value.Bool False)], struct [(1, Value.String "雪"), (2, Value.Bool True)]]))
             ,
               ( 2
               , Value.Map Wire.TT_STRING Wire.TT_LIST $
                   V.fromList
                     [ (Value.String "a", Value.List Wire.TT_BOOL (V.fromList [Value.Bool False, Value.Bool True]))
                     , (Value.String "é", Value.List Wire.TT_BOOL V.empty)
                     ]
               )
             , (3, Value.Map Wire.TT_STOP Wire.TT_STOP V.empty)
             ]
       ]
  where
    struct = Value.Struct . V.fromList
