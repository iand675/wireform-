module Test.FlatBuffers (flatBuffersTests) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word (Word32)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog

import qualified FlatBuffers.Value as F
import FlatBuffers.Encode (encode, encodeWithFileIdentifier)
import FlatBuffers.Decode (decode, fileIdentifier, hasFileIdentifier)
import qualified FlatBuffers.Parser as P
import qualified FlatBuffers.Schema as FS
import qualified FlatBuffers.Typed as FT

flatBuffersTests :: TestTree
flatBuffersTests = testGroup "FlatBuffers"
  [ unitTests
  , wireFormatTests
  , edgeCases
  , propertyTests
  , fileIdentifierTests
  , vtableDedupTests
  , nestedTableTests
  , schemaAwareTests
  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "VBool true encodes" $ do
      let bs = encode (F.VBool True)
      BS.length bs @?= 5
      BS.index bs 4 @?= 0x01

  , testCase "VBool false encodes" $ do
      let bs = encode (F.VBool False)
      BS.length bs @?= 5
      BS.index bs 4 @?= 0x00

  , testCase "VInt8 encodes" $ do
      let bs = encode (F.VInt8 42)
      BS.length bs @?= 5
      BS.index bs 4 @?= 42

  , testCase "VInt16 encodes" $ do
      let bs = encode (F.VInt16 256)
      BS.length bs @?= 6

  , testCase "VInt32 encodes" $ do
      let bs = encode (F.VInt32 100000)
      BS.length bs @?= 8

  , testCase "VInt64 encodes" $ do
      let bs = encode (F.VInt64 maxBound)
      BS.length bs @?= 12

  , testCase "VWord8 encodes" $ do
      let bs = encode (F.VWord8 255)
      BS.length bs @?= 5

  , testCase "VWord16 encodes" $ do
      let bs = encode (F.VWord16 65535)
      BS.length bs @?= 6

  , testCase "VWord32 encodes" $ do
      let bs = encode (F.VWord32 maxBound)
      BS.length bs @?= 8

  , testCase "VWord64 encodes" $ do
      let bs = encode (F.VWord64 maxBound)
      BS.length bs @?= 12

  , testCase "VFloat encodes" $ do
      let bs = encode (F.VFloat 3.14)
      BS.length bs @?= 8

  , testCase "VDouble encodes" $ do
      let bs = encode (F.VDouble 2.718)
      BS.length bs @?= 12

  , testCase "VString encodes" $ do
      let bs = encode (F.VString (T.pack "hello"))
      BS.length bs > 4 @?= True

  , testCase "VVector empty encodes" $ do
      let bs = encode (F.VVector V.empty)
      BS.length bs @?= 8
  ]

wireFormatTests :: TestTree
wireFormatTests = testGroup "Wire format"
  [ testCase "Root offset is first 4 bytes LE" $ do
      let bs = encode (F.VInt32 42)
          off = readLE32 bs 0
      off @?= 4

  , testCase "VString length prefix" $ do
      let bs = encode (F.VString (T.pack "ABC"))
          strLen = readLE32 bs 4
      strLen @?= 3

  , testCase "VString NUL terminated" $ do
      let bs = encode (F.VString (T.pack "AB"))
          nulPos = 4 + 4 + 2
      BS.index bs nulPos @?= 0x00

  , testCase "VVector length prefix" $ do
      let bs = encode (F.VVector (V.fromList [F.VWord8 1, F.VWord8 2]))
          cnt = readLE32 bs 4
      cnt @?= 2

  , testCase "VInt32 LE bytes" $ do
      let bs = encode (F.VInt32 0x04030201)
      BS.index bs 4 @?= 0x01
      BS.index bs 5 @?= 0x02
      BS.index bs 6 @?= 0x03
      BS.index bs 7 @?= 0x04
  ]

edgeCases :: TestTree
edgeCases = testGroup "Edge cases"
  [ testCase "Empty string" $ do
      let bs = encode (F.VString T.empty)
      BS.length bs > 4 @?= True
      readLE32 bs 4 @?= 0

  , testCase "Empty vector" $ do
      let bs = encode (F.VVector V.empty)
      readLE32 bs 4 @?= 0

  , testCase "VStruct encodes" $ do
      let val = F.VStruct (V.fromList [F.VInt32 1, F.VInt32 2])
          bs = encode val
      BS.length bs @?= 12

  , testCase "Decode too short" $
      case decode (BS.pack [0, 0]) of
        Left _ -> pure ()
        Right _ -> assertFailure "expected error on short input"

  , testCase "Nested VVector" $ do
      let val = F.VVector (V.fromList [F.VVector (V.fromList [F.VWord8 1])])
          bs = encode val
      BS.length bs > 4 @?= True
  ]

propertyTests :: TestTree
propertyTests = testGroup "Property tests"
  [ testProperty "VBool encode size" $ property $ do
      b <- forAll Gen.bool
      let bs = encode (F.VBool b)
      BS.length bs === 5

  , testProperty "VInt32 encode size" $ property $ do
      n <- forAll $ Gen.int32 Range.linearBounded
      let bs = encode (F.VInt32 n)
      BS.length bs === 8

  , testProperty "VInt64 encode size" $ property $ do
      n <- forAll $ Gen.int64 Range.linearBounded
      let bs = encode (F.VInt64 n)
      BS.length bs === 12

  , testProperty "VWord32 encode size" $ property $ do
      w <- forAll $ Gen.word32 Range.linearBounded
      let bs = encode (F.VWord32 w)
      BS.length bs === 8

  , testProperty "VWord64 encode size" $ property $ do
      w <- forAll $ Gen.word64 Range.linearBounded
      let bs = encode (F.VWord64 w)
      BS.length bs === 12

  , testProperty "VFloat encode size" $ property $ do
      f <- forAll $ Gen.float (Range.linearFrac (-1e6) 1e6)
      let bs = encode (F.VFloat f)
      BS.length bs === 8

  , testProperty "VDouble encode size" $ property $ do
      d <- forAll $ Gen.double (Range.linearFrac (-1e6) 1e6)
      let bs = encode (F.VDouble d)
      BS.length bs === 12

  , testProperty "VString encode has correct structure" $ property $ do
      t <- forAll $ Gen.text (Range.linear 0 64) Gen.alphaNum
      let bs = encode (F.VString t)
      assert (BS.length bs > 4)
      let strLen = readLE32 bs 4
      strLen === fromIntegral (T.length t)

  , testProperty "VVector of bytes encode" $ property $ do
      ws <- forAll $ Gen.list (Range.linear 0 20) (Gen.word8 Range.linearBounded)
      let val = F.VVector (V.fromList (map F.VWord8 ws))
          bs = encode val
      assert (BS.length bs >= 8)
  ]

fileIdentifierTests :: TestTree
fileIdentifierTests = testGroup "File identifier"
  [ testCase "encodeWithFileIdentifier embeds 4 bytes after root offset" $ do
      let table = F.VTable (V.fromList [Just (F.VInt32 1)])
          bs = encodeWithFileIdentifier "ABCD" table
      BS.index bs 4 @?= 0x41
      BS.index bs 5 @?= 0x42
      BS.index bs 6 @?= 0x43
      BS.index bs 7 @?= 0x44

  , testCase "fileIdentifier reads back the magic" $ do
      let table = F.VTable (V.fromList [Just (F.VInt32 1)])
          bs = encodeWithFileIdentifier "ABCD" table
      fileIdentifier bs @?= Just "ABCD"

  , testCase "hasFileIdentifier checks magic" $ do
      let table = F.VTable (V.fromList [Just (F.VInt32 1)])
          bs = encodeWithFileIdentifier "WFRM" table
      hasFileIdentifier "WFRM" bs @?= True
      hasFileIdentifier "WHAT" bs @?= False

  , testCase "shorter identifier zero-padded" $ do
      let table = F.VTable (V.fromList [Just (F.VInt32 1)])
          bs = encodeWithFileIdentifier "AB" table
      BS.index bs 4 @?= 0x41
      BS.index bs 5 @?= 0x42
      BS.index bs 6 @?= 0x00
      BS.index bs 7 @?= 0x00

  , testCase "longer identifier truncated to 4 bytes" $ do
      let table = F.VTable (V.fromList [Just (F.VInt32 1)])
          bs = encodeWithFileIdentifier "ABCDEFG" table
      fileIdentifier bs @?= Just "ABCD"

  , testCase "no file identifier on plain encode" $ do
      let table = F.VTable (V.fromList [Just (F.VInt32 1)])
          bs = encode table
      hasFileIdentifier "ABCD" bs @?= False
  ]

vtableDedupTests :: TestTree
vtableDedupTests = testGroup "Vtable deduplication"
  [ testCase "two identical sub-tables share a vtable" $ do
      let inner = F.VTable (V.fromList [Just (F.VInt32 7), Just (F.VInt32 8)])
          outer = F.VTable (V.fromList
            [ Just inner
            , Just inner
            , Just (F.VInt32 99)
            ])
          bs = encode outer
      -- Sanity: encoding should be smaller than naive encoding because
      -- the inner vtable is shared. We assert it succeeds and decodes.
      BS.length bs > 0 @?= True
      case decode bs of
        Right _ -> pure ()
        Left e  -> assertFailure $ "decode failed: " ++ e

  , testCase "different field sets get different vtables" $ do
      let a = F.VTable (V.fromList [Just (F.VInt32 1)])
          b = F.VTable (V.fromList [Just (F.VInt32 1), Just (F.VInt32 2)])
          outer = F.VTable (V.fromList [Just a, Just b])
          bs = encode outer
      BS.length bs > 0 @?= True
  ]

nestedTableTests :: TestTree
nestedTableTests = testGroup "Nested tables (decode)"
  [ testCase "single nested table roundtrips" $ do
      let inner = F.VTable (V.fromList [Just (F.VInt32 99)])
          outer = F.VTable (V.fromList [Just inner])
          bs = encode outer
      case decode bs of
        Right (F.VTable fields) -> case V.toList fields of
          [Just (F.VTable inner')] -> case V.toList inner' of
            [Just _] -> pure ()
            _ -> assertFailure "inner table missing field"
          _ -> assertFailure $ "expected single nested table, got: " ++ show fields
        Left e  -> assertFailure $ "decode failed: " ++ e
        Right v -> assertFailure $ "expected VTable, got " ++ show v

  , testCase "two-deep nesting roundtrips" $ do
      let leaf = F.VTable (V.fromList [Just (F.VInt32 1)])
          mid  = F.VTable (V.fromList [Just leaf])
          root = F.VTable (V.fromList [Just mid])
          bs   = encode root
      case decode bs of
        Right (F.VTable rootFs) -> case V.toList rootFs of
          [Just (F.VTable midFs)] -> case V.toList midFs of
            [Just (F.VTable leafFs)] -> case V.toList leafFs of
              [Just _] -> pure ()
              other -> assertFailure $ "leaf shape: " ++ show other
            other -> assertFailure $ "mid shape: " ++ show other
          other -> assertFailure $ "root shape: " ++ show other
        Right v -> assertFailure $ "expected VTable, got " ++ show v
        Left e  -> assertFailure $ "decode failed: " ++ e

  , testCase "absent fields preserved on decode" $ do
      let table = F.VTable (V.fromList
            [ Just (F.VInt32 7)
            , Nothing
            , Just (F.VInt32 9)
            ])
          bs = encode table
      case decode bs of
        Right (F.VTable fields) -> do
          V.length fields @?= 3
          case fields V.! 1 of
            Nothing -> pure ()
            Just _  -> assertFailure "middle field should be absent"
        Right v -> assertFailure $ "expected VTable, got " ++ show v
        Left e  -> assertFailure $ "decode failed: " ++ e

  , testCase "string field roundtrips through nested table" $ do
      let inner = F.VTable (V.fromList [Just (F.VString (T.pack "hi"))])
          outer = F.VTable (V.fromList [Just inner])
          bs = encode outer
      case decode bs of
        Right (F.VTable outerFs) -> case V.toList outerFs of
          [Just (F.VTable innerFs)] -> case V.toList innerFs of
            [Just (F.VString t)] -> t @?= T.pack "hi"
            other -> assertFailure $ "inner shape: " ++ show other
          other -> assertFailure $ "outer shape: " ++ show other
        Right v -> assertFailure $ "expected VTable, got " ++ show v
        Left e  -> assertFailure $ "decode failed: " ++ e

  , testCase "shared vtable: two inner tables decode independently" $ do
      let inner = F.VTable (V.fromList [Just (F.VInt32 1), Just (F.VInt32 2)])
          outer = F.VTable (V.fromList [Just inner, Just inner])
          bs = encode outer
      case decode bs of
        Right (F.VTable outerFs) -> case V.toList outerFs of
          [Just (F.VTable a), Just (F.VTable b)] -> do
            V.length a @?= 2
            V.length b @?= 2
          other -> assertFailure $ "outer shape: " ++ show other
        Right v -> assertFailure $ "expected VTable, got " ++ show v
        Left e  -> assertFailure $ "decode failed: " ++ e

  , testCase "table with file identifier still decodes" $ do
      let t = F.VTable (V.fromList [Just (F.VInt32 7)])
          bs = encodeWithFileIdentifier "WFRM" t
      case decode bs of
        Right (F.VTable fields) -> V.length fields @?= 1
        Right v -> assertFailure $ "expected VTable, got " ++ show v
        Left e  -> assertFailure $ "decode failed: " ++ e
  ]

schemaAwareTests :: TestTree
schemaAwareTests = testGroup "Schema-aware encode/decode"
  [ testCase "round-trip of a Monster table with defaults" $ do
      let schema = parseOrFail monsterSchema
          v = F.VTable (V.fromList
                [ Just (F.VString "Goblin")
                , Just (F.VInt16 50)
                , Nothing
                , Nothing
                , Nothing
                ])
      bs <- expectRight "encode" (FT.encodeWithSchema schema v)
      BS.take 4 (BS.drop 4 bs) @?= "MNST"
      out <- expectRight "decode" (FT.decodeWithSchema schema bs)
      case out of
        F.VTable fs -> do
          V.length fs @?= 5
          fs V.! 0 @?= Just (F.VString "Goblin")
          fs V.! 1 @?= Just (F.VInt16 50)
          fs V.! 2 @?= Just (F.VInt32 150)
          fs V.! 3 @?= Nothing
          fs V.! 4 @?= Nothing
        _ -> assertFailure $ "expected VTable, got " ++ show out

  , testCase "vector<ubyte> round-trips through schema" $ do
      let schema = parseOrFail monsterSchema
          inv = F.VVector (V.fromList [F.VWord8 1, F.VWord8 2, F.VWord8 3])
          v = F.VTable (V.fromList
                [ Just (F.VString "Skeleton")
                , Nothing, Nothing
                , Just inv
                , Nothing
                ])
      bs <- expectRight "encode" (FT.encodeWithSchema schema v)
      out <- expectRight "decode" (FT.decodeWithSchema schema bs)
      case out of
        F.VTable fs -> do
          fs V.! 1 @?= Just (F.VInt16 100)
          fs V.! 2 @?= Just (F.VInt32 150)
          fs V.! 3 @?= Just inv
        _ -> assertFailure "expected VTable"

  , testCase "self-referential nested table (friend)" $ do
      let schema = parseOrFail monsterSchema
          inner = F.VTable (V.fromList
                [ Just (F.VString "Imp")
                , Nothing, Nothing, Nothing, Nothing ])
          v = F.VTable (V.fromList
                [ Just (F.VString "Demon")
                , Nothing, Nothing, Nothing
                , Just inner ])
      bs <- expectRight "encode" (FT.encodeWithSchema schema v)
      out <- expectRight "decode" (FT.decodeWithSchema schema bs)
      case out of
        F.VTable fs -> case fs V.! 4 of
          Just (F.VTable inner') -> inner' V.! 0 @?= Just (F.VString "Imp")
          other -> assertFailure $ "expected nested table, got " ++ show other
        _ -> assertFailure "expected VTable"

  , testCase "file_identifier mismatch rejected on decode" $ do
      let schema = parseOrFail monsterSchema
          v = F.VTable (V.fromList
                [ Just (F.VString "X"), Nothing, Nothing, Nothing, Nothing ])
      bs <- expectRight "encode" (FT.encodeWithSchema schema v)
      let badBs = BS.take 4 bs <> "XXXX" <> BS.drop 8 bs
      case FT.decodeWithSchema schema badBs of
        Left _  -> pure ()
        Right _ -> assertFailure "expected file_identifier mismatch error"

  , testCase "struct (Vec3) and enum default round-trip" $ do
      let schema = parseOrFail extSchema
          pos = F.VStruct (V.fromList [F.VFloat 1.5, F.VFloat 2.5, F.VFloat 3.5])
          v = F.VTable (V.fromList
                [ Just pos
                , Nothing  -- tint defaults to Green = 2
                ])
      bs <- expectRight "encode" (FT.encodeWithSchema schema v)
      out <- expectRight "decode" (FT.decodeWithSchema schema bs)
      case out of
        F.VTable fs -> do
          fs V.! 0 @?= Just pos
          fs V.! 1 @?= Just (F.VWord8 2)
        _ -> assertFailure "expected VTable"
  ]
  where
    monsterSchema = T.unlines
      [ "table Monster {"
      , "  name:string;"
      , "  hp:short = 100;"
      , "  mana:int = 150;"
      , "  inventory:[ubyte];"
      , "  friend:Monster;"
      , "}"
      , "root_type Monster;"
      , "file_identifier \"MNST\";"
      ]
    extSchema = T.unlines
      [ "enum Color : ubyte { Red = 1, Green = 2, Blue = 4 }"
      , "struct Vec3 { x:float; y:float; z:float; }"
      , "table Item { pos:Vec3; tint:Color = Green; }"
      , "root_type Item;"
      ]

parseOrFail :: T.Text -> FS.FlatBuffersSchema
parseOrFail t = case P.parseFlatBuffers t of
  Right s -> s
  Left e  -> error ("schema parse failed: " ++ e)

expectRight :: String -> Either String a -> IO a
expectRight ctx (Left e)  = assertFailure (ctx ++ ": " ++ e) >> error "unreachable"
expectRight _   (Right v) = pure v

readLE32 :: BS.ByteString -> Int -> Word32
readLE32 bs off =
  fromIntegral (BS.index bs off)
  + fromIntegral (BS.index bs (off+1)) * 256
  + fromIntegral (BS.index bs (off+2)) * 65536
  + fromIntegral (BS.index bs (off+3)) * 16777216
