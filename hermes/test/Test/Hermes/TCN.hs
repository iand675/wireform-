{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.TCN (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.AcceptAdditions as AA
import qualified Network.HTTP.Headers.AcceptFeatures as AF
import qualified Network.HTTP.Headers.Alternates as Alt
import qualified Network.HTTP.Headers.Mason as M
import qualified Network.HTTP.Headers.Negotiate as Neg
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import qualified Network.HTTP.Headers.TCN as TCN
import qualified Network.HTTP.Headers.VariantVary as VV
import Test.Syd
import Test.Syd.Hedgehog ()


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

parseOk :: (ByteString -> Result String a) -> ByteString -> Either String a
parseOk run bs = case run bs of
  OK v leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


tcharChars :: [Char]
tcharChars = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "-.!#$%&'*+^_`|~"


-- | A letter-led token (never equal to the bare @*@ wildcard).
genName :: Gen ST.ShortText
genName = do
  c <- Gen.element (['a' .. 'z'] <> ['A' .. 'Z'])
  cs <- Gen.list (Range.linear 0 7) (Gen.element tcharChars)
  pure (ST.fromString (c : cs))


-- | A non-empty token used as a directive\/parameter value.
genVal :: Gen ST.ShortText
genVal = ST.fromString <$> Gen.list (Range.linear 1 8) (Gen.element tcharChars)


-- ---------------------------------------------------------------------------
-- TCN
-- ---------------------------------------------------------------------------

unit_tcn_parse :: Spec
unit_tcn_parse =
  it "parses a TCN directive list" $
    parseOk (runParser TCN.tcnParser) "list, keep"
      `shouldBe` Right (TCN.TCN [TCN.TCNDirective "list" Nothing, TCN.TCNDirective "keep" Nothing])


unit_tcn_render :: Spec
unit_tcn_render =
  it "renders a TCN directive list" $
    M.toStrictByteString
      (TCN.renderTCN (TCN.TCN [TCN.TCNDirective "choice" Nothing, TCN.TCNDirective "foo" (Just "bar")]))
      `shouldBe` "choice, foo=bar"


genTCN :: Gen TCN.TCN
genTCN = TCN.TCN <$> Gen.list (Range.linear 0 4) genDirective
  where
    genDirective = TCN.TCNDirective <$> genName <*> Gen.maybe genVal


prop_tcn_roundtrip :: Property
prop_tcn_roundtrip = property $ do
  v <- forAll genTCN
  let bs = M.toStrictByteString (TCN.renderTCN v)
  case parseOk (runParser TCN.tcnParser) bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- ---------------------------------------------------------------------------
-- Negotiate
-- ---------------------------------------------------------------------------

unit_negotiate_parse :: Spec
unit_negotiate_parse =
  it "parses an rvsa-version list" $
    parseOk (runParser Neg.negotiateParser) "1.0, 2.5"
      `shouldBe` Right (Neg.Negotiate (Neg.NegotiateDirective "1.0" Nothing :| [Neg.NegotiateDirective "2.5" Nothing]))


unit_negotiate_render :: Spec
unit_negotiate_render =
  it "renders negotiate directives" $
    M.toStrictByteString
      (Neg.renderNegotiate (Neg.Negotiate (Neg.NegotiateDirective "trans" Nothing :| [Neg.NegotiateDirective "vlist" Nothing])))
      `shouldBe` "trans, vlist"


genNegotiate :: Gen Neg.Negotiate
genNegotiate = Neg.Negotiate <$> Gen.nonEmpty (Range.linear 1 4) genDirective
  where
    genDirective = Neg.NegotiateDirective <$> genName <*> Gen.maybe genVal


prop_negotiate_roundtrip :: Property
prop_negotiate_roundtrip = property $ do
  v <- forAll genNegotiate
  let bs = M.toStrictByteString (Neg.renderNegotiate v)
  case parseOk (runParser Neg.negotiateParser) bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- ---------------------------------------------------------------------------
-- Variant-Vary
-- ---------------------------------------------------------------------------

unit_variantvary_parse_all :: Spec
unit_variantvary_parse_all =
  it "parses the * wildcard" $
    parseOk (runParser VV.variantVaryParser) "*" `shouldBe` Right VV.VariantVaryAll


unit_variantvary_parse_fields :: Spec
unit_variantvary_parse_fields =
  it "parses a field-name list" $
    parseOk (runParser VV.variantVaryParser) "Accept, Accept-Language"
      `shouldBe` Right (VV.VariantVaryFields ("Accept" :| ["Accept-Language"]))


unit_variantvary_render :: Spec
unit_variantvary_render =
  it "renders a field-name list" $
    M.toStrictByteString (VV.renderVariantVary (VV.VariantVaryFields ("Accept" :| ["Accept-Language"])))
      `shouldBe` "Accept, Accept-Language"


genVariantVary :: Gen VV.VariantVary
genVariantVary =
  Gen.choice
    [ pure VV.VariantVaryAll
    , VV.VariantVaryFields <$> Gen.nonEmpty (Range.linear 1 4) genName
    ]


prop_variantvary_roundtrip :: Property
prop_variantvary_roundtrip = property $ do
  v <- forAll genVariantVary
  let bs = M.toStrictByteString (VV.renderVariantVary v)
  case parseOk (runParser VV.variantVaryParser) bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- ---------------------------------------------------------------------------
-- Accept-Additions
-- ---------------------------------------------------------------------------

unit_acceptadditions_parse :: Spec
unit_acceptadditions_parse =
  it "parses additions with parameters" $
    parseOk (runParser AA.acceptAdditionsParser) "Cream, Whisky;q=0.7"
      `shouldBe` Right
        ( AA.AcceptAdditions
            [ AA.Addition "Cream" []
            , AA.Addition "Whisky" [AA.AdditionParam "q" (Just "0.7")]
            ]
        )


unit_acceptadditions_render :: Spec
unit_acceptadditions_render =
  it "renders additions with parameters" $
    M.toStrictByteString
      ( AA.renderAcceptAdditions
          ( AA.AcceptAdditions
              [ AA.Addition "Cream" []
              , AA.Addition "Whisky" [AA.AdditionParam "q" (Just "0.7")]
              ]
          )
      )
      `shouldBe` "Cream, Whisky;q=0.7"


genAcceptAdditions :: Gen AA.AcceptAdditions
genAcceptAdditions = AA.AcceptAdditions <$> Gen.list (Range.linear 0 4) genAddition
  where
    genAddition = AA.Addition <$> genName <*> Gen.list (Range.linear 0 3) genParam
    genParam = AA.AdditionParam <$> genName <*> Gen.maybe genVal


prop_acceptadditions_roundtrip :: Property
prop_acceptadditions_roundtrip = property $ do
  v <- forAll genAcceptAdditions
  let bs = M.toStrictByteString (AA.renderAcceptAdditions v)
  case parseOk (runParser AA.acceptAdditionsParser) bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- ---------------------------------------------------------------------------
-- Accept-Features (raw-preserving)
-- ---------------------------------------------------------------------------

unit_acceptfeatures_parse :: Spec
unit_acceptfeatures_parse =
  it "preserves the raw Accept-Features value" $
    parseOk (runParser AF.acceptFeaturesParser) "blex, !blebber, colordepth={5}, *"
      `shouldBe` Right (AF.AcceptFeatures "blex, !blebber, colordepth={5}, *")


unit_acceptfeatures_render :: Spec
unit_acceptfeatures_render =
  it "renders the raw Accept-Features value" $
    M.toStrictByteString (AF.renderAcceptFeatures (AF.AcceptFeatures "blex, !blebber, *"))
      `shouldBe` "blex, !blebber, *"


-- ---------------------------------------------------------------------------
-- Alternates (raw-preserving)
-- ---------------------------------------------------------------------------

unit_alternates_parse :: Spec
unit_alternates_parse =
  it "preserves the raw Alternates value" $
    parseOk (runParser Alt.alternatesParser) "{\"paper.1\" 0.9 {type text/html}}, proxy-rvsa=\"1.0\""
      `shouldBe` Right (Alt.Alternates "{\"paper.1\" 0.9 {type text/html}}, proxy-rvsa=\"1.0\"")


unit_alternates_render :: Spec
unit_alternates_render =
  it "renders the raw Alternates value" $
    M.toStrictByteString (Alt.renderAlternates (Alt.Alternates "{\"a.html\" 1.0 {language en}}"))
      `shouldBe` "{\"a.html\" 1.0 {language en}}"


-- ---------------------------------------------------------------------------
-- Suite
-- ---------------------------------------------------------------------------

tests :: Spec
tests =
  describe "TCN" $
    sequence_
      [ unit_tcn_parse
      , unit_tcn_render
      , it "TCN round-trip" prop_tcn_roundtrip
      , unit_negotiate_parse
      , unit_negotiate_render
      , it "Negotiate round-trip" prop_negotiate_roundtrip
      , unit_variantvary_parse_all
      , unit_variantvary_parse_fields
      , unit_variantvary_render
      , it "Variant-Vary round-trip" prop_variantvary_roundtrip
      , unit_acceptadditions_parse
      , unit_acceptadditions_render
      , it "Accept-Additions round-trip" prop_acceptadditions_roundtrip
      , unit_acceptfeatures_parse
      , unit_acceptfeatures_render
      , unit_alternates_parse
      , unit_alternates_render
      ]
