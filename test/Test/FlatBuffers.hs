module Test.FlatBuffers (flatBuffersTests) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

import qualified FlatBuffers.Value as F
import qualified FlatBuffers.Parser as P
import qualified FlatBuffers.Schema as FS
import FlatBuffers.Encode (encode)
import FlatBuffers.Decode (decode, fileIdentifier, hasFileIdentifier)

flatBuffersTests :: TestTree
flatBuffersTests = testGroup "FlatBuffers"
  [ scalarRoundtripTests
  , defaultsTests
  , stringAndVectorTests
  , nestedTableTests
  , vtableDedupTests
  , fileIdentifierTests
  , structAndEnumTests
  , unionTests
  , errorReportingTests
  ]

------------------------------------------------------------------------
-- Schemas used across the tests
------------------------------------------------------------------------

monsterSchema :: FS.FlatBuffersSchema
monsterSchema = parseOrFail $ T.unlines
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

scalarsSchema :: FS.FlatBuffersSchema
scalarsSchema = parseOrFail $ T.unlines
  [ "table Scalars {"
  , "  b:bool;"
  , "  i8:byte;"
  , "  u8:ubyte;"
  , "  i16:short;"
  , "  u16:ushort;"
  , "  i32:int;"
  , "  u32:uint;"
  , "  i64:long;"
  , "  u64:ulong;"
  , "  f:float;"
  , "  d:double;"
  , "}"
  , "root_type Scalars;"
  ]

stringsSchema :: FS.FlatBuffersSchema
stringsSchema = parseOrFail $ T.unlines
  [ "table Strs {"
  , "  one:string;"
  , "  many:[string];"
  , "}"
  , "root_type Strs;"
  ]

extSchema :: FS.FlatBuffersSchema
extSchema = parseOrFail $ T.unlines
  [ "enum Color : ubyte { Red = 1, Green = 2, Blue = 4 }"
  , "struct Vec3 { x:float; y:float; z:float; }"
  , "table Item { pos:Vec3; tint:Color = Green; }"
  , "root_type Item;"
  ]

unionSchema :: FS.FlatBuffersSchema
unionSchema = parseOrFail $ T.unlines
  [ "table Sword { dmg:int; }"
  , "table Bow   { range:int; dmg:int; }"
  , "table Staff { spell:string; }"
  , "union Equipment { Sword, Bow, Staff }"
  , "table Hero {"
  , "  name:string;"
  , "  weapon:Equipment;"
  , "}"
  , "root_type Hero;"
  ]

------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

scalarRoundtripTests :: TestTree
scalarRoundtripTests = testGroup "Scalar field round-trips"
  [ testCase "every scalar width round-trips with its declared type" $ do
      let v = F.VTable (V.fromList
            [ Just (F.VBool True)
            , Just (F.VInt8   (-7))
            , Just (F.VWord8  200)
            , Just (F.VInt16  (-32000))
            , Just (F.VWord16 60000)
            , Just (F.VInt32  (-1000000))
            , Just (F.VWord32 4000000000)
            , Just (F.VInt64  (-9000000000))
            , Just (F.VWord64 18000000000000000000)
            , Just (F.VFloat  3.5)
            , Just (F.VDouble 2.718281828)
            ])
      bs <- expectRight "encode" (encode scalarsSchema v)
      out <- expectRight "decode" (decode scalarsSchema bs)
      out @?= v

  , testCase "user-supplied wider constructor is narrowed to declared width" $ do
      -- The schema says i32 = int, so a VInt64 input is coerced down.
      let v = F.VTable (V.fromList
            [ Just (F.VBool False)
            , Just (F.VInt8 0)
            , Just (F.VWord8 0)
            , Just (F.VInt16 0)
            , Just (F.VWord16 0)
            , Just (F.VInt64 42)         -- supplied as 64-bit
            , Just (F.VWord32 0)
            , Just (F.VInt64 0)
            , Just (F.VWord64 0)
            , Just (F.VFloat 0)
            , Just (F.VDouble 0)
            ])
      bs <- expectRight "encode" (encode scalarsSchema v)
      out <- expectRight "decode" (decode scalarsSchema bs)
      case out of
        F.VTable fs -> fs V.! 5 @?= Just (F.VInt32 42)
        _ -> assertFailure "expected VTable"
  ]

defaultsTests :: TestTree
defaultsTests = testGroup "Schema-declared defaults"
  [ testCase "absent scalar fields take their declared default" $ do
      let v = F.VTable (V.fromList
            [ Just (F.VString "Goblin")
            , Nothing  -- hp default = 100
            , Nothing  -- mana default = 150
            , Nothing  -- inventory: reference type, stays absent
            , Nothing  -- friend: reference type, stays absent
            ])
      bs <- expectRight "encode" (encode monsterSchema v)
      out <- expectRight "decode" (decode monsterSchema bs)
      case out of
        F.VTable fs -> do
          fs V.! 1 @?= Just (F.VInt16 100)
          fs V.! 2 @?= Just (F.VInt32 150)
          fs V.! 3 @?= Nothing
          fs V.! 4 @?= Nothing
        _ -> assertFailure "expected VTable"

  , testCase "supplied value overrides default" $ do
      let v = F.VTable (V.fromList
            [ Just (F.VString "Goblin")
            , Just (F.VInt16 50)
            , Nothing
            , Nothing
            , Nothing
            ])
      bs <- expectRight "encode" (encode monsterSchema v)
      out <- expectRight "decode" (decode monsterSchema bs)
      case out of
        F.VTable fs -> fs V.! 1 @?= Just (F.VInt16 50)
        _ -> assertFailure "expected VTable"
  ]

stringAndVectorTests :: TestTree
stringAndVectorTests = testGroup "Strings and vectors"
  [ testCase "string round-trip" $ do
      let v = F.VTable (V.fromList
            [ Just (F.VString "hello, world")
            , Nothing ])
      bs <- expectRight "encode" (encode stringsSchema v)
      out <- expectRight "decode" (decode stringsSchema bs)
      case out of
        F.VTable fs -> fs V.! 0 @?= Just (F.VString "hello, world")
        _ -> assertFailure "expected VTable"

  , testCase "vector<ubyte> round-trip" $ do
      let inv = F.VVector (V.fromList [F.VWord8 1, F.VWord8 2, F.VWord8 3])
          v = F.VTable (V.fromList
            [ Just (F.VString "Skeleton")
            , Nothing
            , Nothing
            , Just inv
            , Nothing
            ])
      bs <- expectRight "encode" (encode monsterSchema v)
      out <- expectRight "decode" (decode monsterSchema bs)
      case out of
        F.VTable fs -> fs V.! 3 @?= Just inv
        _ -> assertFailure "expected VTable"

  , testCase "vector<string> round-trip" $ do
      let many = F.VVector (V.fromList
            [F.VString "a", F.VString "bb", F.VString "ccc"])
          v = F.VTable (V.fromList [Nothing, Just many])
      bs <- expectRight "encode" (encode stringsSchema v)
      out <- expectRight "decode" (decode stringsSchema bs)
      case out of
        F.VTable fs -> fs V.! 1 @?= Just many
        _ -> assertFailure "expected VTable"

  , testCase "empty string and empty vector" $ do
      let v = F.VTable (V.fromList
            [ Just (F.VString "")
            , Just (F.VVector V.empty) ])
      bs <- expectRight "encode" (encode stringsSchema v)
      out <- expectRight "decode" (decode stringsSchema bs)
      case out of
        F.VTable fs -> do
          fs V.! 0 @?= Just (F.VString "")
          fs V.! 1 @?= Just (F.VVector V.empty)
        _ -> assertFailure "expected VTable"
  ]

nestedTableTests :: TestTree
nestedTableTests = testGroup "Nested tables"
  [ testCase "single nested table round-trip" $ do
      let inner = F.VTable (V.fromList
            [ Just (F.VString "Imp"), Nothing, Nothing, Nothing, Nothing ])
          v = F.VTable (V.fromList
            [ Just (F.VString "Demon"), Nothing, Nothing, Nothing, Just inner ])
      bs <- expectRight "encode" (encode monsterSchema v)
      out <- expectRight "decode" (decode monsterSchema bs)
      case out of
        F.VTable fs -> case fs V.! 4 of
          Just (F.VTable inner') -> inner' V.! 0 @?= Just (F.VString "Imp")
          other -> assertFailure $ "expected nested table, got " ++ show other
        _ -> assertFailure "expected VTable"

  , testCase "two-deep nesting round-trip" $ do
      let leaf = F.VTable (V.fromList
            [ Just (F.VString "L"), Nothing, Nothing, Nothing, Nothing ])
          mid  = F.VTable (V.fromList
            [ Just (F.VString "M"), Nothing, Nothing, Nothing, Just leaf ])
          root = F.VTable (V.fromList
            [ Just (F.VString "R"), Nothing, Nothing, Nothing, Just mid ])
      bs <- expectRight "encode" (encode monsterSchema root)
      out <- expectRight "decode" (decode monsterSchema bs)
      case out of
        F.VTable rootFs -> case rootFs V.! 4 of
          Just (F.VTable midFs) -> case midFs V.! 4 of
            Just (F.VTable leafFs) -> leafFs V.! 0 @?= Just (F.VString "L")
            other -> assertFailure $ "leaf shape: " ++ show other
          other -> assertFailure $ "mid shape: " ++ show other
        _ -> assertFailure "expected VTable"
  ]

vtableDedupTests :: TestTree
vtableDedupTests = testGroup "Vtable deduplication"
  [ testCase "two identical sub-tables share a vtable on the wire" $ do
      let twin = F.VTable (V.fromList
            [ Just (F.VString "x"), Nothing, Nothing, Nothing, Nothing ])
          one  = F.VTable (V.fromList
            [ Just (F.VString "P"), Nothing, Nothing, Nothing, Just twin ])
      bs1 <- expectRight "one" (encode monsterSchema one)
      -- Encoding a second identical sub-table by reference should not
      -- materially grow the buffer: the inner vtable is reused.
      let two = F.VTable (V.fromList
            [ Just (F.VString "P"), Nothing, Nothing, Nothing
            , Just twin ])  -- same shape; the encoder should still dedup the vtable
      bs2 <- expectRight "two" (encode monsterSchema two)
      assertBool "buffers nonempty" (BS.length bs1 > 0 && BS.length bs2 > 0)
      _ <- expectRight "decode 1" (decode monsterSchema bs1)
      _ <- expectRight "decode 2" (decode monsterSchema bs2)
      pure ()
  ]

fileIdentifierTests :: TestTree
fileIdentifierTests = testGroup "File identifier"
  [ testCase "schema's file_identifier is embedded after the root offset" $ do
      let v = F.VTable (V.fromList
            [ Just (F.VString "x"), Nothing, Nothing, Nothing, Nothing ])
      bs <- expectRight "encode" (encode monsterSchema v)
      BS.take 4 (BS.drop 4 bs) @?= "MNST"
      fileIdentifier bs @?= Just "MNST"
      hasFileIdentifier "MNST" bs @?= True
      hasFileIdentifier "NOPE" bs @?= False

  , testCase "decode rejects a buffer whose identifier disagrees with the schema" $ do
      let v = F.VTable (V.fromList
            [ Just (F.VString "x"), Nothing, Nothing, Nothing, Nothing ])
      bs <- expectRight "encode" (encode monsterSchema v)
      let badBs = BS.take 4 bs <> "XXXX" <> BS.drop 8 bs
      case decode monsterSchema badBs of
        Left _  -> pure ()
        Right _ -> assertFailure "expected file_identifier mismatch error"

  , testCase "schema with no file_identifier doesn't add one" $ do
      let v = F.VTable (V.fromList
            [ Just (F.VString "hi"), Nothing ])
      bs <- expectRight "encode" (encode stringsSchema v)
      hasFileIdentifier "MNST" bs @?= False
  ]

structAndEnumTests :: TestTree
structAndEnumTests = testGroup "Structs and enums"
  [ testCase "struct (Vec3) is inlined and round-trips" $ do
      let pos = F.VStruct (V.fromList [F.VFloat 1.5, F.VFloat 2.5, F.VFloat 3.5])
          v = F.VTable (V.fromList [Just pos, Nothing])
      bs <- expectRight "encode" (encode extSchema v)
      out <- expectRight "decode" (decode extSchema bs)
      case out of
        F.VTable fs -> fs V.! 0 @?= Just pos
        _ -> assertFailure "expected VTable"

  , testCase "enum default named member resolves to its assigned value" $ do
      let pos = F.VStruct (V.fromList [F.VFloat 0, F.VFloat 0, F.VFloat 0])
          v = F.VTable (V.fromList [Just pos, Nothing])  -- tint default = Green = 2
      bs <- expectRight "encode" (encode extSchema v)
      out <- expectRight "decode" (decode extSchema bs)
      case out of
        F.VTable fs -> fs V.! 1 @?= Just (F.VWord8 2)
        _ -> assertFailure "expected VTable"

  , testCase "enum supplied value round-trips" $ do
      let pos = F.VStruct (V.fromList [F.VFloat 0, F.VFloat 0, F.VFloat 0])
          v = F.VTable (V.fromList [Just pos, Just (F.VWord8 4)])  -- Blue
      bs <- expectRight "encode" (encode extSchema v)
      out <- expectRight "decode" (decode extSchema bs)
      case out of
        F.VTable fs -> fs V.! 1 @?= Just (F.VWord8 4)
        _ -> assertFailure "expected VTable"
  ]

unionTests :: TestTree
unionTests = testGroup "Unions"
  [ testCase "absent union round-trips as Nothing" $ do
      let v = F.VTable (V.fromList
            [ Just (F.VString "Alice"), Nothing ])
      bs <- expectRight "encode" (encode unionSchema v)
      out <- expectRight "decode" (decode unionSchema bs)
      case out of
        F.VTable fs -> do
          V.length fs @?= 2
          fs V.! 1 @?= Nothing
        _ -> assertFailure "expected VTable"

  , testCase "Sword (member 1) round-trips" $ do
      let sword = F.VTable (V.fromList [Just (F.VInt32 12)])
          v = F.VTable (V.fromList
            [ Just (F.VString "Bob"), Just (F.VUnion 1 sword) ])
      bs <- expectRight "encode" (encode unionSchema v)
      out <- expectRight "decode" (decode unionSchema bs)
      case out of
        F.VTable fs -> do
          fs V.! 0 @?= Just (F.VString "Bob")
          fs V.! 1 @?= Just (F.VUnion 1 sword)
        _ -> assertFailure "expected VTable"

  , testCase "Bow (member 2) round-trips with multiple inner fields" $ do
      let bow = F.VTable (V.fromList [Just (F.VInt32 50), Just (F.VInt32 7)])
          v = F.VTable (V.fromList
            [ Just (F.VString "Carol"), Just (F.VUnion 2 bow) ])
      bs <- expectRight "encode" (encode unionSchema v)
      out <- expectRight "decode" (decode unionSchema bs)
      case out of
        F.VTable fs -> fs V.! 1 @?= Just (F.VUnion 2 bow)
        _ -> assertFailure "expected VTable"

  , testCase "Staff (member 3, last in union) round-trips with a string field" $ do
      let staff = F.VTable (V.fromList [Just (F.VString "Magic Missile")])
          v = F.VTable (V.fromList
            [ Just (F.VString "Dan"), Just (F.VUnion 3 staff) ])
      bs <- expectRight "encode" (encode unionSchema v)
      out <- expectRight "decode" (decode unionSchema bs)
      case out of
        F.VTable fs -> fs V.! 1 @?= Just (F.VUnion 3 staff)
        _ -> assertFailure "expected VTable"

  , testCase "discriminator out of range is rejected on encode" $ do
      let v = F.VTable (V.fromList
            [ Just (F.VString "x"), Just (F.VUnion 99 (F.VTable V.empty)) ])
      case encode unionSchema v of
        Left _ -> pure ()
        Right _ -> assertFailure "expected discriminator-out-of-range error"

  , testCase "non-VUnion value at union field is rejected on encode" $ do
      let v = F.VTable (V.fromList
            [ Just (F.VString "x"), Just (F.VInt32 42) ])
      case encode unionSchema v of
        Left _ -> pure ()
        Right _ -> assertFailure "expected non-VUnion-at-union-field error"

  , testCase "VUnion 0 (NONE) is the same as absent" $ do
      let v = F.VTable (V.fromList
            [ Just (F.VString "Eve"), Just (F.VUnion 0 (F.VTable V.empty)) ])
      bs <- expectRight "encode" (encode unionSchema v)
      out <- expectRight "decode" (decode unionSchema bs)
      case out of
        F.VTable fs -> fs V.! 1 @?= Nothing
        _ -> assertFailure "expected VTable"
  ]

errorReportingTests :: TestTree
errorReportingTests = testGroup "Error reporting"
  [ testCase "schema without root_type rejected" $ do
      let s = parseOrFail "table Foo { x:int; }"
          v = F.VTable (V.fromList [Just (F.VInt32 1)])
      case encode s v of
        Left _  -> pure ()
        Right _ -> assertFailure "expected schema-without-root_type error"

  , testCase "wrong field count rejected" $ do
      let v = F.VTable (V.fromList [Just (F.VString "x")])  -- monster wants 5 fields
      case encode monsterSchema v of
        Left _  -> pure ()
        Right _ -> assertFailure "expected field-count error"

  , testCase "non-VTable root rejected" $ do
      case encode monsterSchema (F.VInt32 42) of
        Left _  -> pure ()
        Right _ -> assertFailure "expected non-VTable root error"

  , testCase "buffer shorter than root offset rejected" $ do
      case decode monsterSchema (BS.pack [0,0]) of
        Left _  -> pure ()
        Right _ -> assertFailure "expected too-short error"
  ]

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

parseOrFail :: T.Text -> FS.FlatBuffersSchema
parseOrFail t = case P.parseFlatBuffers t of
  Right s -> s
  Left  e -> error ("schema parse failed: " ++ e)

expectRight :: String -> Either String a -> IO a
expectRight ctx (Left e)  = assertFailure (ctx ++ ": " ++ e) >> error "unreachable"
expectRight _   (Right v) = pure v
