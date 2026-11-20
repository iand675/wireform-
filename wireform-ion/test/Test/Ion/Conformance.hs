{-# LANGUAGE OverloadedStrings #-}

-- Independent binary fixtures follow the Ion 1.0 binary and symbol specs:
-- https://amazon-ion.github.io/ion-docs/docs/binary.html
-- https://amazon-ion.github.io/ion-docs/docs/symbols.html
-- Helpers below only frame literal bytes; they never call Ion.Encode.
module Test.Ion.Conformance (tests) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.Builder.Int qualified as TBI
import Data.Vector qualified as V
import Data.Word (Word8)
import GHC.Float (castDoubleToWord64)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Ion.Decode (decode)
import Ion.Encode (encode)
import Ion.Value qualified as I
import Test.Syd
import Test.Syd.Hedgehog ()


bvm :: ByteString
bvm = BS.pack [0xe0, 1, 0, 0xea]


bytes :: [Word8] -> ByteString
bytes = BS.pack


frame :: Word8 -> ByteString -> ByteString
frame code payload
  | BS.length payload < 14 = BS.cons (code * 16 + fromIntegral (BS.length payload)) payload
  | BS.length payload < 128 = bytes [code * 16 + 14, 128 + fromIntegral (BS.length payload)] <> payload
  | otherwise = error "fixture helper only frames payloads below 128 bytes"


str :: T.Text -> ByteString
str = frame 8 . TE.encodeUtf8


lst :: [ByteString] -> ByteString
lst = table . (bytes [0x87] <>) . frame 11 . BS.concat


table :: ByteString -> ByteString
table fields = frame 14 (bytes [0x81, 0x83] <> frame 13 fields)


expect :: ByteString -> I.Value -> IO ()
expect raw value = decode (bvm <> raw) `shouldBe` Right value


reject :: ByteString -> IO ()
reject raw = decode (bvm <> raw) `shouldSatisfy` isLeft


tests :: Spec
tests = describe "Ion 1.0 binary conformance" $ do
  it "writes container lengths across VarUInt byte boundaries" $
    forM_ [(127, [0xff]), (128, [0x01, 0x80]), (16383, [0x7f, 0xff]), (16384, [0x01, 0x00, 0x80])] $ \(count, lengthBytes) -> do
      let value = I.String (T.replicate count "a")
          expected = bvm <> bytes (0x8e : lengthBytes) <> BS.replicate count 0x61
      encode value `shouldBe` expected
      decode expected `shouldBe` Right value
  it "copies sliced string and local-symbol UTF-8 at short-copy boundaries" $
    forM_ ["a", "a雪é"] $ \alphabet ->
      forM_ [0 .. 16] $ \offset -> forM_ [0 .. 40] $ \count -> do
        let text = T.take count (T.drop offset (T.replicate 64 alphabet))
            value = I.List (V.fromList [I.String text, I.Symbol text, I.Annotation text (I.String text)])
        decode (encode value) `shouldBe` Right value
  it "decodes system SID 4 rather than UTF-8 payload" $
    expect (bytes [0x71, 4]) (I.Symbol "name")
  it "encodes a system symbol without a redundant local table" $
    encode (I.Symbol "name") `shouldBe` bvm <> bytes [0x71, 4]
  it "writes a local symbol table and UInt SID, with independent golden bytes" $ do
    let localFoo = bytes [0xe9, 0x81, 0x83, 0xd6, 0x87, 0xb4, 0x83, 0x66, 0x6f, 0x6f]
    encode (I.Symbol "foo") `shouldBe` bvm <> localFoo <> bytes [0x71, 10]
    expect (localFoo <> bytes [0x71, 10]) (I.Symbol "foo")
  it "writes struct field names as VarUInt SIDs" $
    encode (I.Struct (V.singleton ("foo", I.Int 42)))
      `shouldBe` bvm <> lst [str "foo"] <> bytes [0xd3, 0x8a, 0x21, 42]
  it "decodes struct field system SIDs" $
    expect (bytes [0xd3, 0x84, 0x21, 42]) (I.Struct (V.singleton ("name", I.Int 42)))
  it "does not collapse duplicate local table slots" $
    expect (lst [str "foo", str "foo", str "bar"] <> bytes [0x71, 12]) (I.Symbol "bar")
  it "resolves either duplicate slot to the same text" $
    expect
      (lst [str "foo", str "foo"] <> bytes [0xb4, 0x71, 10, 0x71, 11])
      (I.List (V.fromList [I.Symbol "foo", I.Symbol "foo"]))
  it "preserves non-string gaps when assigning subsequent SIDs" $
    expect (lst [str "foo", bytes [0x0f], bytes [0x20], str "bar"] <> bytes [0x71, 13]) (I.Symbol "bar")
  it "does not invent text for an unknown local slot" $
    reject (lst [bytes [0x0f]] <> bytes [0x71, 10])
  it "rejects out-of-range SIDs" $
    reject (lst [str "foo"] <> bytes [0x71, 11])
  it "a replacement LST discards earlier locals" $
    expect (lst [str "a", str "b"] <> lst [str "c"] <> bytes [0x71, 10]) (I.Symbol "c")
  it "a replacement LST does not retain stale SIDs" $
    reject (lst [str "a", str "b"] <> lst [str "c"] <> bytes [0x71, 11])
  it "imports:$ion_symbol_table appends without renumbering" $
    expect
      ( lst [str "a"]
          <> table (bytes [0x86, 0x71, 3, 0x87] <> frame 11 (str "b"))
          <> bytes [0xb4, 0x71, 10, 0x71, 11]
      )
      (I.List (V.fromList [I.Symbol "a", I.Symbol "b"]))
  it "a subsequent BVM resets the local table" $
    reject (lst [str "a"] <> bvm <> bytes [0x71, 10])
  it "a top-level SID 2 NOP does not reset the local table" $
    expect (lst [str "a"] <> bytes [0x71, 2, 0x71, 10]) (I.Symbol "a")
  it "a local alias of $ion_1_0 is also a NOP" $
    expect (lst [str "$ion_1_0", str "a"] <> bytes [0x71, 10, 0x71, 11]) (I.Symbol "a")
  it "null.struct LST resets the context" $
    reject (lst [str "a"] <> bytes [0xe3, 0x81, 0x83, 0xdf, 0x71, 10])
  it "ignores unknown LST metadata, including unsupported application types" $
    expect (table (bytes [0x84, 0x50, 0x87] <> frame 11 (str "a")) <> bytes [0x71, 10]) (I.Symbol "a")
  it "retains the types of annotated metadata lists and symbol strings" $ do
    let annotated payload = frame 14 (bytes [0x81, 0x84] <> payload)
    expect (table (bytes [0x87] <> annotated (frame 11 (annotated (str "a")))) <> bytes [0x71, 10]) (I.Symbol "a")
  it "non-list symbols is an empty replacement table" $
    reject (lst [str "a"] <> table (bytes [0x87, 0x20]) <> bytes [0x71, 10])
  it "rejects duplicate symbols fields instead of silently choosing one" $
    reject (table (bytes [0x87, 0xb0, 0x87, 0xb0]) <> bytes [0x20])
  it "reserves unavailable shared imports before local symbols" $ do
    let imported = frame 13 (bytes [0x84] <> str "catalog" <> bytes [0x88, 0x21, 5])
    expect
      ( table (bytes [0x86] <> frame 11 imported <> bytes [0x87] <> frame 11 (str "a"))
          <> bytes [0x71, 15]
      )
      (I.Symbol "a")
  it "rejects a shared import without max_id when no catalog is available" $ do
    let imported = frame 13 (bytes [0x84] <> str "catalog")
    reject (table (bytes [0x86] <> frame 11 imported) <> bytes [0x20])
  it "unknown imported symbol text raises an error" $ do
    let imported = frame 13 (bytes [0x84] <> str "catalog" <> bytes [0x88, 0x21, 5])
    reject (table (bytes [0x86] <> frame 11 imported) <> bytes [0x71, 10])
  it "decodes multiple annotations from one wrapper in order" $
    expect
      (bytes [0xe5, 0x82, 0x84, 0x85, 0x21, 42])
      (I.Annotation "name" (I.Annotation "version" (I.Int 42)))
  it "encodes nested annotation constructors as a single legal wrapper" $
    encode (I.Annotation "name" (I.Annotation "version" (I.Int 42)))
      `shouldBe` bvm <> bytes [0xe5, 0x82, 0x84, 0x85, 0x21, 42]
  it "a non-first $ion_symbol_table annotation has no table effect" $
    expect
      (bytes [0xe4, 0x82, 0x84, 0x83, 0xd0])
      (I.Annotation "name" (I.Annotation "$ion_symbol_table" (I.Struct V.empty)))
  it "accepts a multiply annotated LST when its first annotation is system SID 3" $
    expect
      ( frame 14 (bytes [0x82, 0x83, 0x84] <> frame 13 (bytes [0x87] <> frame 11 (str "a")))
          <> bytes [0x71, 10]
      )
      (I.Symbol "a")
  it "skips top-level and list NOP padding" $ do
    expect (bytes [0x01, 0xac, 0x20, 0x00]) (I.Int 0)
    expect (bytes [0xb4, 0x01, 0xac, 0x21, 42]) (I.List (V.singleton (I.Int 42)))
  it "ignores field SID for struct padding even when SID is unknown" $
    expect (bytes [0xd5, 0x8f, 0x00, 0x84, 0x21, 42]) (I.Struct (V.singleton ("name", I.Int 42)))
  it "decodes an ordered struct with an explicit length" $
    expect (bytes [0xd1, 0x83, 0x84, 0x21, 42]) (I.Struct (V.singleton ("name", I.Int 42)))
  it "preserves IEEE negative zero" $ do
    let value = I.Float (-0.0)
    case decode (encode value) of
      Right (I.Float d) -> castDoubleToWord64 d `shouldBe` 0x8000000000000000
      other -> expectationFailure (show other)
  it "reads a 32-bit float" $
    expect (bytes [0x44, 0x3f, 0x80, 0, 0]) (I.Float 1)
  it "preserves the complete Int64 range" $ property $ do
    n <- forAll (Gen.int64 Range.linearBounded)
    decode (encode (I.Int n)) === Right (I.Int n)
  it "interns Unicode fields, symbols and annotations across nested values" $ property $ do
    field <- forAll (Gen.text (Range.linear 0 60) Gen.unicode)
    sym <- forAll (Gen.text (Range.linear 0 60) Gen.unicode)
    ann <- forAll (Gen.text (Range.linear 0 60) Gen.unicode)
    -- Keep reserved top-level system values inside an ordinary list.
    let value =
          I.List
            ( V.singleton
                ( I.Annotation
                    ann
                    ( I.Struct
                        ( V.fromList
                            [(field, I.Symbol sym), (field, I.List (V.singleton (I.Symbol sym)))]
                        )
                    )
                )
            )
    decode (encode value) === Right value
  it "round-trips generated nested values with exact buffer sizing" $ property $ do
    generated <- forAll genValue
    let wrapped = I.List (V.singleton generated)
    decode (encode wrapped) === Right wrapped
  it "uses value equality for symbol hints on separately sliced but equal names" $ do
    let original = "a symbol longer than a literal"
        copied = T.drop 3 ("雪éx" <> original)
        first = I.Annotation original (I.Struct (V.singleton (original, I.Symbol original)))
        later = I.Annotation copied (I.Struct (V.singleton (copied, I.Symbol copied)))
        value = I.List (V.fromList [first, later, first])
    decode (encode value) `shouldBe` Right value
  it "checks symbol hints when later record names, annotations and shapes differ" $ do
    let records =
          I.List
            ( V.fromList
                [ I.Annotation "event" (I.Struct (V.fromList [("a", I.Symbol "first"), ("b", I.Int 1)]))
                , I.Annotation "other" (I.Struct (V.fromList [("b", I.Symbol "second"), ("a", I.List (V.singleton (I.Symbol "first")))]))
                , I.Struct (V.singleton ("added", I.Symbol "third"))
                , I.List (V.fromList [I.Symbol "first", I.Symbol "second"])
                , I.Int 42
                , I.Annotation "event" (I.Struct (V.fromList [("a", I.Symbol "first"), ("b", I.Int 2)]))
                ]
            )
    decode (encode records) `shouldBe` Right records
  it "keeps SID assignment correct beyond the one-byte ID range" $ do
    let symbols = V.generate 300 (I.Symbol . TL.toStrict . TB.toLazyText . TBI.decimal)
        value = I.List (symbols V.++ V.reverse symbols)
    decode (encode value) `shouldBe` Right value
  it "rejects each strict prefix of a symbol-bearing document" $ do
    let encoded = encode (I.Struct (V.singleton ("field", I.Symbol "value")))
    mapM_ (\n -> decode (BS.take n encoded) `shouldSatisfy` isLeft) [0 .. BS.length encoded - 1]
  it "rejects malformed descriptors, lengths, wrappers and integer overflow" $
    mapM_
      (reject . bytes)
      [ [0x12]
      , [0x30]
      , [0x31, 0]
      , [0xff]
      , [0xb1, 0x21, 1] -- child extends beyond its declared container
      , [0xbe, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0xff]
      , [0xe3, 0x81, 0x84, 0x00] -- annotation wraps a NOP
      , [0xe5, 0x81, 0x84, 0x21, 1, 0x0f] -- extra wrapped value
      , [0xe6, 0x81, 0x84, 0xe3, 0x81, 0x85, 0x0f] -- nested wrapper
      , [0xe3, 0x80, 0x21, 1] -- empty annotation sequence
      , [0x28, 0x80, 0, 0, 0, 0, 0, 0, 0] -- positive 2^63
      , [0x38, 0x80, 0, 0, 0, 0, 0, 0, 1] -- negative magnitude >2^63
      , [0x81, 0xff] -- malformed UTF-8
      , [0xd1, 0x80] -- empty ordered struct
      ]


-- Recursion exercises variable-length container headers, annotation chains,
-- field-name interning and string byte lengths together, rather than only
-- testing each branch in isolation. A list wrapper keeps reserved Ion
-- system names in ordinary value context.
genValue :: Gen I.Value
genValue =
  Gen.recursive
    Gen.choice
    [ pure I.Null
    , I.Bool <$> Gen.bool
    , I.Int <$> Gen.int64 Range.linearBounded
    , I.Float <$> Gen.double (Range.linearFrac (-1e100) 1e100)
    , I.String <$> text
    , I.Symbol <$> text
    , I.Blob <$> Gen.bytes (Range.linear 0 80)
    , I.Clob <$> Gen.bytes (Range.linear 0 80)
    ]
    [ I.List . V.fromList <$> Gen.list (Range.linear 0 8) genValue
    , I.Struct . V.fromList <$> Gen.list (Range.linear 0 8) ((,) <$> text <*> genValue)
    , I.Annotation <$> text <*> genValue
    ]
  where
    text = Gen.text (Range.linear 0 80) Gen.unicode
