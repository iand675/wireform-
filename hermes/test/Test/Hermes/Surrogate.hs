{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.Surrogate (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import qualified Network.HTTP.Headers.SurrogateCapability as SCap
import qualified Network.HTTP.Headers.SurrogateControl as SCon
import Test.Syd
import Test.Syd.Hedgehog ()


dropOws :: ByteString -> ByteString
dropOws = BS.dropWhile (\w -> w == 0x20 || w == 0x09)


parseCap :: ByteString -> Either String SCap.SurrogateCapability
parseCap bs = case runParser SCap.surrogateCapabilityParser bs of
  OK v leftover
    | BS.null (dropOws leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


renderCap :: SCap.SurrogateCapability -> ByteString
renderCap = M.toStrictByteString . SCap.renderSurrogateCapability


parseCon :: ByteString -> Either String SCon.SurrogateControl
parseCon bs = case runParser SCon.surrogateControlParser bs of
  OK v leftover
    | BS.null (dropOws leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


renderCon :: SCon.SurrogateControl -> ByteString
renderCon = M.toStrictByteString . SCon.renderSurrogateControl


-- Surrogate-Capability ------------------------------------------------------

unit_cap_parse :: Spec
unit_cap_parse = it "parses a device token with a quoted capability set" $
  case parseCap "abc=\"Surrogate/1.0 ESI/1.0\"" of
    Right (SCap.SurrogateCapability (e :| [])) -> do
      SCap.surrogateCapabilityDevice e `shouldBe` ST.fromString "abc"
      SCap.surrogateCapabilitySet e `shouldBe` ST.fromString "Surrogate/1.0 ESI/1.0"
    other -> error (show other)


unit_cap_render :: Spec
unit_cap_render =
  it "renders a capability entry quoted" $
    let v =
          SCap.SurrogateCapability
            ( SCap.SurrogateCapabilityEntry
                (ST.fromString "abc")
                (ST.fromString "Surrogate/1.0")
                :| []
            )
    in renderCap v `shouldBe` "abc=\"Surrogate/1.0\""


-- Surrogate-Control ---------------------------------------------------------

unit_con_parse :: Spec
unit_con_parse = it "parses a comma list of control directives" $
  case parseCon "max-age=300, no-store, content=\"ESI/1.0\"" of
    Right (SCon.SurrogateControl (a :| [b, c])) -> do
      SCon.surrogateControlName a `shouldBe` ST.fromString "max-age"
      SCon.surrogateControlValue a `shouldBe` Just (ST.fromString "300")
      SCon.surrogateControlName b `shouldBe` ST.fromString "no-store"
      SCon.surrogateControlValue b `shouldBe` Nothing
      SCon.surrogateControlName c `shouldBe` ST.fromString "content"
      SCon.surrogateControlValue c `shouldBe` Just (ST.fromString "ESI/1.0")
    other -> error (show other)


unit_con_render :: Spec
unit_con_render =
  it "renders directives, quoting only non-token values" $
    let v =
          SCon.SurrogateControl
            ( SCon.SurrogateControlDirective (ST.fromString "max-age") (Just (ST.fromString "300"))
                :| [ SCon.SurrogateControlDirective (ST.fromString "no-store") Nothing
                   , SCon.SurrogateControlDirective (ST.fromString "content") (Just (ST.fromString "ESI/1.0"))
                   ]
            )
    in renderCon v `shouldBe` "max-age=300, no-store, content=\"ESI/1.0\""


-- Generators ----------------------------------------------------------------

genNonEmpty :: Gen a -> Gen (NonEmpty a)
genNonEmpty g = do
  x <- g
  xs <- Gen.list (Range.linear 0 3) g
  pure (x :| xs)


genToken :: Gen ST.ShortText
genToken =
  ST.fromString
    <$> Gen.string (Range.linear 1 8) (Gen.element (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "-._"))


-- | Capability sets are always quoted; allow spaces and @/@.
genCapSet :: Gen ST.ShortText
genCapSet =
  ST.fromString
    <$> Gen.string (Range.linear 1 16) (Gen.element (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "/. -"))


-- | Control values exercise both the bare-token and quoted branches.
genConValue :: Gen ST.ShortText
genConValue =
  ST.fromString
    <$> Gen.string (Range.linear 1 12) (Gen.element (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "/.-"))


genCap :: Gen SCap.SurrogateCapability
genCap = SCap.SurrogateCapability <$> genNonEmpty (SCap.SurrogateCapabilityEntry <$> genToken <*> genCapSet)


genCon :: Gen SCon.SurrogateControl
genCon = SCon.SurrogateControl <$> genNonEmpty (SCon.SurrogateControlDirective <$> genToken <*> Gen.maybe genConValue)


prop_cap_roundtrip :: Property
prop_cap_roundtrip = property $ do
  v <- forAll genCap
  let bs = renderCap v
  case parseCap bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


prop_con_roundtrip :: Property
prop_con_roundtrip = property $ do
  v <- forAll genCon
  let bs = renderCon v
  case parseCon bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


tests :: Spec
tests =
  describe "Surrogate" $
    sequence_
      [ unit_cap_parse
      , unit_cap_render
      , unit_con_parse
      , unit_con_render
      , it "Surrogate-Capability round-trip" prop_cap_roundtrip
      , it "Surrogate-Control round-trip" prop_con_roundtrip
      ]
