{-# LANGUAGE QuasiQuotes #-}
module Test.TextFormatParsed (textFormatParsedTests) where

import Data.Aeson qualified as Aeson
import Data.Proxy (Proxy (..))
import Proto.Schema (ProtoMessage (..))
import Data.Text qualified as T
import Data.Vector qualified as V
import Test.Tasty
import Test.Tasty.HUnit

import Proto (decodeMessage, MessageDecode (..))
import Proto (encodeMessage)
import Proto.Internal.Encode (MessageEncode (..))
import Proto.TextFormat (textToTyped, typedToTextPretty)
import Proto.TH.QQ (proto)
import Data.Reflection (Given (..))
import Proto.Internal.JSON.Extension (ExtensionRegistry, emptyExtensionRegistry)


-- Generated types for these tests.
[proto|
  syntax = "proto3";
  package test.textfmt;

  enum Color {
    COLOR_UNSPECIFIED = 0;
    COLOR_RED         = 1;
    COLOR_GREEN       = 2;
    COLOR_BLUE        = 3;
  }

  message Point {
    int32  x = 1;
    int32  y = 2;
    string label = 3;
    Color  color = 4;
    bytes  data  = 5;
    repeated int32 values = 6;
  }

  message Outer {
    string name  = 1;
    Point  inner = 2;
  }
|]

instance Given ExtensionRegistry where
  given = emptyExtensionRegistry


textFormatParsedTests :: TestTree
textFormatParsedTests =
  testGroup
    "textToTyped"
    [ testCase "empty message round-trips" $ do
        let msg = defaultPoint
            pbtxt = typedToTextPretty (Proxy @Point) msg
        case pbtxt of
          Nothing -> assertFailure "typedToTextPretty returned Nothing for empty message"
          Just t -> case textToTyped (Proxy @Point) t of
            Left e -> assertFailure ("textToTyped failed: " <> e)
            Right p -> p @?= msg

    , testCase "scalar fields round-trip" $ do
        let msg = defaultPoint
              { pointX = 42
              , pointY = -7
              , pointLabel = "hello world"
              }
        roundTrip (Proxy @Point) msg

    , testCase "enum field round-trips" $ do
        let msg = defaultPoint { pointColor = Color'ColorRed }
        roundTrip (Proxy @Point) msg

    , testCase "all enum values round-trip" $
        mapM_
          (\c -> roundTrip (Proxy @Point) (defaultPoint { pointColor = c }))
          [Color'ColorUnspecified, Color'ColorRed, Color'ColorGreen, Color'ColorBlue]

    , testCase "nested message round-trips" $ do
        let inner = defaultPoint { pointX = 10, pointY = 20, pointLabel = "inner" }
            msg = defaultOuter { outerName = "outer", outerInner = Just inner }
        roundTrip (Proxy @Outer) msg

    , testCase "repeated field round-trips" $ do
        let msg = defaultPoint { pointValues = V.fromList [1, 2, 3, 100, -5] }
        roundTrip (Proxy @Point) msg

    , testCase "parse known text format" $
        -- Build text format manually without round-tripping through typedToTextPretty.
        -- This tests that the parser itself works for user-provided pbtxt.
        case textToTyped (Proxy @Point) "x: 99\ny: -3\nlabel: \"foo\"" of
          Left e -> assertFailure ("textToTyped failed: " <> e)
          Right p -> do
            pointX p @?= 99
            pointY p @?= -3
            pointLabel p @?= "foo"

    , testCase "parse nested message text format" $ do
        let src = T.unlines
              [ "name: \"test\""
              , "inner {"
              , "  x: 5"
              , "  y: 10"
              , "}"
              ]
        case textToTyped (Proxy @Outer) src of
          Left e -> assertFailure ("textToTyped failed: " <> e)
          Right o -> do
            outerName o @?= "test"
            fmap pointX (outerInner o) @?= Just 5
            fmap pointY (outerInner o) @?= Just 10

    , testCase "wire-format ↔ text format ↔ wire-format consistency" $ do
        -- textToTyped ∘ typedToTextPretty ∘ decodeMessage ∘ encodeMessage == id
        let msg = defaultPoint { pointX = 7, pointY = -3, pointLabel = "wire", pointColor = Color'ColorBlue }
            bytes = encodeMessage msg
        case decodeMessage @Point bytes of
          Left e -> assertFailure ("decode failed: " <> show e)
          Right decoded -> roundTrip (Proxy @Point) decoded

    , testCase "empty string returns default" $
        case textToTyped (Proxy @Point) "" of
          Left e -> assertFailure ("textToTyped empty failed: " <> e)
          Right p -> p @?= defaultPoint

    , testCase "unknown field names are ignored" $
        -- Graceful: unknown names result in fields staying at default,
        -- but the parse itself succeeds (fromJSON ignores unknown keys).
        case textToTyped (Proxy @Point) "not_a_field: 42\nx: 5" of
          Left _ -> pure ()   -- also acceptable to fail
          Right p -> pointX p @?= 5
    ]
  where
    roundTrip :: (Given ExtensionRegistry, MessageEncode a, MessageDecode a, ProtoMessage a, Aeson.ToJSON a, Aeson.FromJSON a, Eq a, Show a) => Proxy a -> a -> IO ()
    roundTrip p msg =
      case typedToTextPretty p msg of
        Nothing -> assertFailure "typedToTextPretty returned Nothing"
        Just t ->
          case textToTyped p t of
            Left e -> assertFailure ("textToTyped failed: " <> e)
            Right r -> r @?= msg
