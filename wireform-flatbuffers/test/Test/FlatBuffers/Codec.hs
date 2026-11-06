module Test.FlatBuffers.Codec (tests) where

import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.Text (Text)
import Data.Text.IO qualified as T
import Data.Vector qualified as V
import FlatBuffers.Codec
import FlatBuffers.Decode
import FlatBuffers.Encode
import FlatBuffers.Parser
import FlatBuffers.Reader qualified as R
import FlatBuffers.Schema
import FlatBuffers.Value qualified as F
import GHC.Float (castDoubleToWord64, castFloatToWord32, castWord32ToFloat, castWord64ToDouble)
import Paths_wireform_flatbuffers (getDataFileName)
import Test.Syd


schema :: Text -> FlatBuffersSchema
schema = either error id . parseFlatBuffers


fixture :: IO FlatBuffersSchema
fixture = schema <$> (getDataFileName "test/fixtures/codec.fbs" >>= T.readFile)


table :: [Maybe F.Value] -> F.Value
table = F.VTable . V.fromList


vector :: [F.Value] -> F.Value
vector = F.VVector . V.fromList


vec3 :: Float -> F.Value
vec3 x = F.VStruct (V.fromList [F.VFloat x, F.VFloat (x + 1), F.VFloat (x + 2)])


child :: Text -> F.Value
child label = table [Just (F.VString label), Just (F.VWord32 7)]


fixtureValue :: F.Value
fixtureValue =
  table
    [ Just (F.VString "wireform")
    , Just (F.VInt8 (-7))
    , Just (F.VInt64 maxBound)
    , Just (vec3 1)
    , Just (vector [vec3 4, vec3 7])
    , Just (vector [F.VInt64 minBound, F.VInt64 maxBound])
    , Just (child "nested")
    , Just (vector [table [Just (F.VString "one"), Just (F.VWord32 1)], table [Just (F.VString "two"), Just (F.VWord32 2)]])
    , Just (vector [F.VString "", F.VString "hello", F.VString "😀"])
    , Just (F.VWord8 3)
    , Just (F.VUnion 1 (child "union"))
    , Just (F.VInt16 0)
    , Nothing
    , Just (F.VStruct (V.fromList [F.VInt8 9, F.VInt64 1234567890123, vec3 1]))
    , Just (vector [F.VStruct (V.fromList [F.VInt8 (-9), F.VInt64 (-1234567890123), vec3 4])])
    , Just (vector [F.VWord8 0, F.VWord8 127, F.VWord8 255])
    , Just (vector [F.VBool True, F.VBool False, F.VBool True])
    , Just (vector [F.VInt16 minBound, F.VInt16 0, F.VInt16 maxBound])
    , Just (vector [F.VStruct (V.fromList [F.VInt16 (-1), F.VInt16 2, F.VInt16 3])])
    ]


tests :: Spec
tests = describe "schema-directed dynamic codec" $ do
  it "decodes the official flatc fixture, including field IDs, defaults, unions and padded structs" $ do
    fbs <- fixture
    bytes <- getDataFileName "test/fixtures/codec.bin" >>= BS.readFile
    decode fbs bytes `shouldBe` Right fixtureValue
  it "round-trips the fixture through a reused compiled schema" $ do
    fbs <- fixture
    case compileSchema fbs of
      Left err -> expectationFailure err
      Right compiled -> (encodeWith compiled fixtureValue >>= decodeWith compiled) `shouldBe` Right fixtureValue
  it "preserves scalar types, integer extrema, optional presence and empty vectors" $ do
    let fbs = schema "table R { b:bool; a:byte; c:ubyte; d:short; e:ushort; f:int; g:uint; h:long; i:ulong; j:float; k:double; s:string; v:[ulong]; o:int=null; } root_type R;"
        value = table (map Just [F.VBool True, F.VInt8 minBound, F.VWord8 maxBound, F.VInt16 minBound, F.VWord16 maxBound, F.VInt32 minBound, F.VWord32 maxBound, F.VInt64 minBound, F.VWord64 maxBound, F.VFloat 1.5, F.VDouble (-2.5), F.VString "", vector []] <> [Nothing])
    (encode fbs value >>= decode fbs) `shouldBe` Right value
  it "uses schema defaults without confusing absent optional scalars with explicit zero" $ do
    let fbs = schema "table R { x:int=23; o:int=null; } root_type R;"
    (encode fbs (table []) >>= decode fbs) `shouldBe` Right (table [Just (F.VInt32 23), Nothing])
    (encode fbs (table [Nothing, Just (F.VInt32 0)]) >>= decode fbs) `shouldBe` Right (table [Just (F.VInt32 23), Just (F.VInt32 0)])
  it "rejects missing required fields, wrong types, overflow and heterogeneous vectors" $ do
    let fbs = schema "table R { s:string(required); b:byte; v:[long]; } root_type R;"
    mapM_
      (\value -> isLeft (encode fbs value) `shouldBe` True)
      [ table []
      , table [Just (F.VInt32 5)]
      , table [Just (F.VString "x"), Just (F.VInt32 128)]
      , table [Just (F.VString "x"), Nothing, Just (vector [F.VInt64 1, F.VString "wrong"])]
      ]
  it "rejects invalid schema IDs, defaults, includes and recursive structs" $ do
    mapM_
      (\fbs -> isLeft (compileSchema (schema fbs)) `shouldBe` True)
      [ "table R { x:int(id:1); } root_type R;"
      , "table R { x:int(id:0); y:int; } root_type R;"
      , "table R { x:byte=128; } root_type R;"
      , "table R { x:int(required); } root_type R;"
      , "struct S { self:S; } table R { x:S; } root_type R;"
      , "include \"other.fbs\"; table R {} root_type R;"
      ]
  it "rejects truncation, bad identifiers, and forged lengths/offsets" $ do
    fbs <- fixture
    case encode fbs fixtureValue of
      Left err -> expectationFailure err
      Right bytes -> do
        isLeft (decode fbs (BS.take 3 bytes)) `shouldBe` True
        isLeft (decode fbs (BS.take 4 bytes <> "NOPE" <> BS.drop 8 bytes)) `shouldBe` True
        isLeft (decode fbs (BS.replicate 4 255 <> BS.drop 4 bytes)) `shouldBe` True
    let strings = schema "table R { s:string; } root_type R;"
    case encode strings (table [Just (F.VString "hi")]) of
      Left err -> expectationFailure err
      Right bytes -> do
        case do
          root <- R.followUOffset bytes 0
          slots <- R.resolveTable bytes root
          at <- maybe (Left "missing string") Right (slots 0)
          R.followUOffset bytes at of
          Left err -> expectationFailure err
          Right target -> do
            let terminator = target + 6
                mutate at byte = BS.take at bytes <> BS.singleton byte <> BS.drop (at + 1) bytes
            isLeft (decode strings (BS.take terminator bytes)) `shouldBe` True
            isLeft (decode strings (mutate terminator 255)) `shouldBe` True
            isLeft (decode strings (mutate (target + 4) 255)) `shouldBe` True

  it "handles empty tables and absent reference fields" $ do
    let fbs = schema "table R { s:string; } root_type R;"
    (encode fbs (table []) >>= decode fbs) `shouldBe` Right (table [Nothing])

  it "enforces required inline structs" $ do
    let fbs = schema "struct S { n:long; } table R { s:S(required); } root_type R;"
        value = table [Just (F.VStruct (V.singleton (F.VInt64 9)))]
    (encode fbs value >>= decode fbs) `shouldBe` Right value
    isLeft (encode fbs (table [])) `shouldBe` True
  it "rejects oversized materialization before allocating a vector" $ do
    let fbs = schema "table R { v:[ubyte]; } root_type R;"
    case encode fbs (table [Just (vector [])]) of
      Left err -> expectationFailure err
      Right bytes -> case do
        root <- R.followUOffset bytes 0
        slots <- R.resolveTable bytes root
        at <- maybe (Left "missing vector") Right (slots 0)
        R.followUOffset bytes at of
        Left err -> expectationFailure err
        Right target -> do
          let count = 1000001
              lengthBytes = BS.pack [0x41, 0x42, 0x0f, 0]
              huge = BS.take target bytes <> lengthBytes <> BS.replicate count 0
          decode fbs huge `shouldBe` Left "FlatBuffers: vector exceeds materialization limit"
  it "rejects missing required fields on the wire and invalid union pairs" $ do
    fbs <- fixture
    bytes <- getDataFileName "test/fixtures/codec.bin" >>= BS.readFile
    case do
      root <- R.followUOffset bytes 0
      displacement <- R.peekI32 bytes root
      slots <- R.resolveTable bytes root
      tag <- maybe (Left "missing discriminator") Right (slots 10)
      Right (root - fromIntegral displacement, tag) of
      Left err -> expectationFailure err
      Right (vtable, tag) -> do
        let absentName = BS.take (vtable + 4) bytes <> BS.pack [0, 0] <> BS.drop (vtable + 6) bytes
            absentTag = BS.take tag bytes <> BS.singleton 0 <> BS.drop (tag + 1) bytes
        isLeft (decode fbs absentName) `shouldBe` True
        isLeft (decode fbs absentTag) `shouldBe` True

  it "preserves floating bit patterns including signed zero, subnormals, infinities and NaNs" $ do
    let fbs = schema "table R { f:float; d:double; } root_type R;"
        patterns = [(0x80000000, 0x8000000000000000), (1, 1), (0x7f800000, 0x7ff0000000000000), (0x7fc01234, 0x7ff8000000001234)]
    mapM_
      ( \(f, d) -> do
          let input = table [Just (F.VFloat (castWord32ToFloat f)), Just (F.VDouble (castWord64ToDouble d))]
          case encode fbs input >>= decode fbs of
            Right (F.VTable values) -> case V.toList values of
              [Just (F.VFloat f'), Just (F.VDouble d')] -> do
                castFloatToWord32 f' `shouldBe` f
                castDoubleToWord64 d' `shouldBe` d
              _ -> expectationFailure "wrong floating field types"
            other -> expectationFailure (show other)
      )
      patterns
