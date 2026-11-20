{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Tests for the compile-time validator generator: the @.proto@'s rules are
read at compile time and every predicate is compiled to Haskell.
-}
module Test.Protovalidate.TH (tests) where

import CEL (Duration (..), Timestamp (..), Value (..), celMapFromList)
import Data.ByteString qualified as BS
import Data.List (sort)
import Data.Text (Text)
import Data.Vector qualified as V
import Protovalidate
import Protovalidate.TH (compileMessageValidator, messageValidatorE)
import Test.Protovalidate.UserProto (eventProto, ignoreProto, parityProto, predefinedRules, userProto)
import Test.Syd


-- Generated at compile time; each rule's CEL is compiled to Haskell.
userValidator :: Value -> [Violation]
userValidator = $(compileMessageValidator userProto "User")


-- Exercises compiled timestamp/duration literal bounds and enum defined_only.
eventValidator :: Value -> [Violation]
eventValidator = $(compileMessageValidator eventProto "Event")


ignoreValidator :: Value -> [Violation]
ignoreValidator = $(compileMessageValidator ignoreProto "Ignore")


parityValidator :: Value -> [Violation]
parityValidator = $(compileMessageValidator parityProto "Parent")


predefinedValidator :: Value -> [Violation]
predefinedValidator = $(messageValidatorE predefinedRules)


msg :: [(Text, Value)] -> Value
msg fs = VMap (celMapFromList [(VString k, v) | (k, v) <- fs])


ids :: [Violation] -> [Text]
ids = sort . map violationConstraintId


tests :: Spec
tests =
  describe
    "compile-time validator (Protovalidate.TH)"
    $ sequence_
      [ it "valid message: no violations" $
          userValidator
            ( msg
                [ ("id", VString "abc")
                , ("age", VUInt 30)
                , ("email", VString "alice@example.com")
                ]
            )
            `shouldBe` []
      , it "interpreted and compiled validators distinguish the ignore modes" $ do
          rules <- case parseProtoRules ignoreProto of
            Right [("Ignore", parsed)] -> pure parsed
            other -> fail ("Expected rules for Ignore, got " <> show other)
          mapM_ (\check -> do
            check (msg [("implicit", VString ""), ("always", VString "x")]) `shouldBe` []
            check (msg []) `shouldBe` []
            map violationFieldPath (check (msg [("implicit", VString "x"), ("explicit", VString "")]))
              `shouldBe` ["implicit", "explicit"])
            [(`validate` rules), ignoreValidator]
      , it "invalid message: all compiled predicates fire" $
          ids
            ( userValidator
                ( msg
                    [ ("id", VString "")
                    , ("age", VUInt 200)
                    , ("email", VString "bad")
                    ]
                )
            )
            `shouldBe` sort ["string.min_len", "uint32.lte", "string.email", "id_required_with_age"]
      , it "compiled time bounds + enum defined_only: valid" $
          eventValidator
            ( msg
                [ ("at", VTimestamp (Timestamp 2000 0))
                , ("ttl", VDuration (Duration 30 0))
                , ("kind", VInt 1)
                ]
            )
            `shouldBe` []
      , it "compiled time bounds + enum defined_only: violations" $
          ids
            ( eventValidator
                ( msg
                    [ ("at", VTimestamp (Timestamp 500 0))
                    , ("ttl", VDuration (Duration 90 0))
                    , ("kind", VInt 9)
                    ]
                )
            )
            `shouldBe` sort ["timestamp.gt", "duration.lte", "enum.defined_only"]
      , it "preserves nanoseconds and matches the runtime validator around a strict timestamp bound" $ do
          rules <- lookupRules "Event" eventProto
          let check nanos expected = do
                let input = msg [("at", VTimestamp (Timestamp 1000 nanos))]
                    runtime = validate input rules
                ids runtime `shouldBe` expected
                eventValidator input `shouldBe` runtime
          check 499 ["timestamp.gt"]
          check 500 ["timestamp.gt"]
          check 501 []
      , it "applies the same required, nested, repeated, map, and contextual rules" $ do
          rules <- lookupRules "Parent" parityProto
          let valid =
                msg
                  [ ("flag", VBool False)
                  , ("child", msg [("code", VString "ok")])
                  , ("labels", VList (V.singleton (VString "ab")))
                  , ("scores", msg [("ab", VInt 2)])
                  , ("name", VString "ab")
                  , ("uuid", VString "")
                  ]
              invalid =
                msg
                  [ ("flag", VBool True)
                  , ("child", msg [])
                  , ("labels", VList (V.fromList [VString "ok", VString "x"]))
                  , ("scores", msg [("a", VInt 0)])
                  , ("name", VString "x")
                  ]
          validate valid rules `shouldBe` []
          parityValidator valid `shouldBe` []
          let runtime = validate invalid rules
          map (\v -> (violationFieldPath v, violationConstraintId v)) runtime
            `shouldBe` [ ("flag", "bool.const")
                       , ("child.code", "required")
                       , ("labels[1]", "string.min_len")
                       , ("name", "string.min_len")
                       , ("scores[a]", "string.min_len")
                       , ("scores[a]", "int32.gt")
                       ]
          parityValidator invalid `shouldBe` runtime
          runValidator (compileValidator rules) invalid `shouldBe` runtime
      , it "preserves predefined rule values and uses the parsed AST" $ do
          let valid = msg [("data", VBytes (BS.pack [0, 255]))]
              invalid = msg [("data", VBytes BS.empty)]
          validate valid predefinedRules `shouldBe` []
          predefinedValidator valid `shouldBe` []
          let runtime = validate invalid predefinedRules
          ids runtime `shouldBe` ["value_matches"]
          predefinedValidator invalid `shouldBe` runtime
      ]


lookupRules :: Text -> Text -> IO MessageRules
lookupRules name source =
  case parseProtoRules source of
    Right rules | Just parsed <- lookup name rules -> pure parsed
    other -> fail ("Expected rules for " <> show name <> ", got " <> show other)
