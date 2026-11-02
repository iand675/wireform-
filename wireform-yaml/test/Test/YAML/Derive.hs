{-# LANGUAGE OverloadedStrings #-}

module Test.YAML.Derive (tests) where

import Data.Either (isLeft)
import Data.Vector qualified as V
import Test.Syd
import Test.YAML.Derive.Instances ()
import Test.YAML.Derive.Types
import YAML.Class qualified as Y
import YAML.Value qualified as YV


tests :: Spec
tests =
  describe "YAML.Derive" $
    sequence_
      [ describe "defaults and coercion" $ do
          it "derives coercion constraints for polymorphic record fields" $
            Y.decodeYAML (Y.encodeYAML (CoercedBox (Wrapped 7))) `shouldBe` Right (CoercedBox (Wrapped 7))
          it "derives coercion constraints for optional polymorphic record fields" $
            mapM_ (\value -> Y.decodeYAML (Y.encodeYAML value) `shouldBe` Right value) [OptionalCoercedBox (Just (Wrapped 9)), OptionalCoercedBox Nothing]
          it "uses defaults for missing ordinary fields" $
            Y.decodeYAML "{}" `shouldBe` Right (DefaultRecord 3)
          it "decodes provided fields instead of their default" $
            Y.decodeYAML "retries: 9" `shouldBe` Right (DefaultRecord 9)
          it "combines optional defaults and coercion" $ do
            Y.decodeYAML "{}" `shouldBe` Right (OptionalCoerced (Just (Wrapped 5)))
            Y.decodeYAML "optional-wrapped: null" `shouldBe` Right (OptionalCoerced Nothing)
          it "round-trips present and absent coerced optional fields" $
            mapM_ (\value -> Y.decodeYAML (Y.encodeYAML value) `shouldBe` Right value) [OptionalCoerced Nothing, OptionalCoerced (Just (Wrapped 9))]
          it "round-trips through the annotated coercion target" $
            Y.decodeYAML (Y.encodeYAML (CoercedRecord (Wrapped 7))) `shouldBe` Right (CoercedRecord (Wrapped 7))
      , describe "optional fields" $ do
          it "round-trips null and present values" $
            mapM_ (\v -> Y.decodeYAML (Y.encodeYAML v) `shouldBe` Right v) [OptionalRecord Nothing, OptionalRecord (Just 3)]
          it "accepts an absent optional field" $
            Y.decodeYAML "{}" `shouldBe` Right (OptionalRecord Nothing)
      , describe "record constructors in sums" $ do
          it "encodes record contents as a mapping with field modifiers" $ do
            let value = NamedRecord "test" 7 "secret"
            case Y.toYAML value of
              YV.YMap entries -> case YV.lookupKey "contents" (YV.YMap entries) of
                Just contents -> do
                  YV.lookupKey "name" contents `shouldBe` Just (YV.YString "test")
                  YV.lookupKey "record-private" contents `shouldBe` Nothing
                Nothing -> expectationFailure "missing contents"
              _ -> expectationFailure "expected mapping"
          it "applies defaults and skipped-field defaults to record sum contents" $
            Y.decodeYAML "tag: named-record\ncontents: {name: test}\n" `shouldBe` Right (NamedRecord "test" 3 "<redacted>")
          it "round-trips record and nullary constructors" $
            mapM_ (\value -> Y.decodeYAML (Y.encodeYAML value) `shouldBe` Right value) [NoRecord, NamedRecord "test" 7 "<redacted>"]
      , describe "native field names" $ do
          it "rejects ambiguous tagged field names in TH records" $
            (Y.decodeYAML "!a boxed: 1\n!b boxed: 2\n" :: Either String (Box Int)) `shouldSatisfy` isLeft
          it "rejects ambiguous tagged field names in generic records" $
            (Y.decodeYAML "!a genericCount: 1\n!b genericCount: 2\n" :: Either String GenericRecord) `shouldSatisfy` isLeft
          it "decodes ordinary generic records" $
            Y.decodeYAML "genericCount: 3" `shouldBe` Right (GenericRecord 3)
      , recordTests
      , newtypeTests
      , enumTests
      , sumTests
      , describe "polymorphic types" $ do
          it "derives constraints for record fields" $
            Y.decodeYAML (Y.encodeYAML (Box (3 :: Int))) `shouldBe` Right (Box (3 :: Int))
          it "constrains applied higher-kinded parameters" $
            Y.decodeYAML (Y.encodeYAML (WrappedF (Just (3 :: Int)))) `shouldBe` Right (WrappedF (Just (3 :: Int)))
          it "supports recursive polymorphic sums" $ do
            let tree = Branch (Leaf (3 :: Int)) (Just (Leaf 4))
            Y.decodeYAML (Y.encodeYAML tree) `shouldBe` Right tree
          it "leaves phantom parameters unconstrained" $
            Y.toYAML (Phantom :: Phantom (Int -> Int)) `shouldBe` YV.YString "phantom"
      ]


recordTests :: Spec
recordTests =
  describe "record" $
    sequence_
      [ it "encode applies rename + renameStyle, drops skipped" $ do
          let p = Profile "Alice" 30 "a@x" "secret"
          case Y.toYAML p of
            YV.YMap kvs -> do
              (V.elem (YV.YString "name", YV.YString "Alice") kvs) `shouldBe` True
              (V.any (keyIs "profile_age") kvs) `shouldBe` True
              (V.any (keyIs "email") kvs) `shouldBe` True
              (not (V.any (keyIs "profilePrivate") kvs)) `shouldBe` True
            v -> expectationFailure ("expected YMap, got " ++ show v)
      , it "round-trip fills skipped from defaults" $ do
          let p = Profile "Alice" 30 "a@x" "secret"
          case Y.fromYAML (Y.toYAML p) of
            Right p' -> do
              profileName p' `shouldBe` profileName p
              profileAge p' `shouldBe` profileAge p
              profileEmail p' `shouldBe` profileEmail p
              profilePrivate p' `shouldBe` defaultPrivate
            Left e -> expectationFailure e
      ]
  where
    keyIs t (YV.YString k, _) = k == t
    keyIs _ _ = False


newtypeTests :: Spec
newtypeTests =
  describe "newtype" $
    sequence_
      [ it "pass-through" $
          Y.toYAML (Tag 42) `shouldBe` YV.YInt 42
      , it "round-trip" $
          Y.fromYAML (Y.toYAML (Tag 7)) `shouldBe` Right (Tag 7)
      ]


enumTests :: Spec
enumTests =
  describe "enum" $
    sequence_
      [ it "Red" $ Y.toYAML Red `shouldBe` YV.YString "red"
      , it "DarkBlue" $ Y.toYAML DarkBlue `shouldBe` YV.YString "dark-blue"
      , it "round-trip" $
          mapM_ rt [Red, Green, DarkBlue]
      , it "unknown fails" $
          case Y.fromYAML (YV.YString "purple") :: Either String Color of
            Left _ -> pure ()
            Right c -> expectationFailure ("unexpected " ++ show c)
      ]
  where
    rt :: Color -> IO ()
    rt c = Y.fromYAML (Y.toYAML c) `shouldBe` Right c


sumTests :: Spec
sumTests =
  describe "sum" $
    sequence_
      [ it "Origin (nullary) -> tag only, no contents" $
          Y.toYAML Origin
            `shouldBe` YV.YMap
              ( V.fromList
                  [ (YV.YString "tag", YV.YString "origin")
                  ]
              )
      , it "Circle (unary)   -> contents = inner value" $
          Y.toYAML (Circle 1.5)
            `shouldBe` YV.YMap
              ( V.fromList
                  [ (YV.YString "tag", YV.YString "circle")
                  , (YV.YString "contents", YV.YFloat 1.5)
                  ]
              )
      , it "Rect   (n-ary)   -> contents = YSeq" $
          Y.toYAML (Rect 2 3)
            `shouldBe` YV.YMap
              ( V.fromList
                  [ (YV.YString "tag", YV.YString "rect")
                  ,
                    ( YV.YString "contents"
                    , YV.YSeq (V.fromList [YV.YFloat 2, YV.YFloat 3])
                    )
                  ]
              )
      , it "round-trip Origin" $ rt Origin
      , it "round-trip Circle" $ rt (Circle 2.5)
      , it "round-trip Rect" $ rt (Rect 4 5)
      , it "unknown tag fails" $ do
          let bad =
                YV.YMap
                  ( V.fromList
                      [ (YV.YString "tag", YV.YString "ellipse")
                      ]
                  )
          case Y.fromYAML bad :: Either String Shape of
            Left _ -> pure ()
            Right s -> expectationFailure ("unexpected " ++ show s)
      ]
  where
    rt :: Shape -> IO ()
    rt s = Y.fromYAML (Y.toYAML s) `shouldBe` Right s
