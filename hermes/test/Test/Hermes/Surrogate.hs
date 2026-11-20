{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.Surrogate (tests) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Headers (headerMapFromList, lookupHeader, lookupRawHeader, setHeader)
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
genCapSet = do
  features <- genNonEmpty $ do
    name <- genToken
    version <- Gen.maybe genToken
    pure (name <> maybe "" ("/" <>) version)
  pure (NE.head features <> foldMap (" " <>) (NE.tail features))


-- | Control values exercise both the bare-token and quoted branches.
genConValue :: Gen ST.ShortText
genConValue =
  ST.fromString
    <$> Gen.string (Range.linear 1 12) (Gen.element (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "/.-"))


genCap :: Gen SCap.SurrogateCapability
genCap = do
  entries <- genNonEmpty (SCap.SurrogateCapabilityEntry <$> genToken <*> genCapSet)
  let withUniqueDevice prefix entry = entry {SCap.surrogateCapabilityDevice = ST.pack [prefix] <> SCap.surrogateCapabilityDevice entry}
  pure (SCap.SurrogateCapability (NE.zipWith withUniqueDevice ('a' :| ['b' .. 'd']) entries))


genCon :: Gen SCon.SurrogateControl
genCon = SCon.SurrogateControl <$> genNonEmpty directive
  where
    directive = do
      name <- ("ext-" <>) <$> genToken
      value <- Gen.maybe genConValue
      targets <- Gen.maybe (genNonEmpty genToken)
      pure (maybe (SCon.SurrogateControlDirective name value) (SCon.TargetedSurrogateControlDirective name value) targets)


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


typedCapabilities :: Spec
typedCapabilities = describe "typed surrogate capabilities" $ do
  it "preserves devices and capabilities in the advertised order across field lines" $ do
    let a = SCap.SurrogateCapabilityEntry "abc" "Surrogate/1.0"
        b = SCap.SurrogateCapabilityEntry "def" "ESI/1.0 ESI-Inline/2 notify"
        value = SCap.SurrogateCapability (a :| [b])
        fields = headerMapFromList [("Surrogate-Capability", ", abc=\"Surrogate/1.0\", "), ("Surrogate-Capability", "def = \"ESI/1.0 ESI-Inline/2 notify\"")]
    lookupHeader @SCap.SurrogateCapability fields `shouldBe` Right (Just value)
    SCap.surrogateCapabilityFeatures b
      `shouldBe` Right (SCap.SurrogateFeature "ESI" (Just "1.0") :| [SCap.SurrogateFeature "ESI-Inline" (Just "2"), SCap.SurrogateFeature "notify" Nothing])
    SCap.surrogateCapabilityFromFeatures "abc" (SCap.SurrogateFeature "Surrogate" (Just "1.0") :| [])
      `shouldBe` a
    lookupRawHeader "Surrogate-Capability" (setHeader value (headerMapFromList []))
      `shouldBe` Just ("abc=\"Surrogate/1.0\", def=\"ESI/1.0 ESI-Inline/2 notify\"" :| [])
  it "rejects repeated device identifiers and malformed quoted capability products" $ do
    forM_ ["", ",", "dev=token", "dev=\"\"", "dev=\"a/\"", "dev=\"a/b/c\"", "dev=\"a\\b\"", "dev=\"a,b\"", "dev=\"a b \"", "dev=\"a  b\""] $ \wire ->
      lookupHeader @SCap.SurrogateCapability (headerMapFromList [("Surrogate-Capability", wire)]) `shouldSatisfy` isLeft
    lookupHeader @SCap.SurrogateCapability (headerMapFromList [("Surrogate-Capability", "abc=\"ESI\""), ("Surrogate-Capability", "abc=\"ESI/1.0\"")])
      `shouldSatisfy` isLeft


typedControl :: Spec
typedControl = describe "typed surrogate control" $ do
  it "preserves multiple targets and separates supported directive arguments" $ do
    let first = SCon.TargetedSurrogateControlDirective "MAX-AGE" (Just "60+300") ("abc" :| ["def"])
        second = SCon.SurrogateControlDirective "no-store-remote" Nothing
        third = SCon.TargetedSurrogateControlDirective "content" (Just "ESI") ("def" :| [])
        fourth = SCon.SurrogateControlDirective "stale-while-revalidate" (Just "30")
        value = SCon.SurrogateControl (first :| [second, third, fourth])
        fields = headerMapFromList [("Surrogate-Control", "MAX-AGE = 60+300 ; abc; def,, no-store-remote"), ("Surrogate-Control", "content=\"ESI\";def, stale-while-revalidate=30")]
    lookupHeader @SCon.SurrogateControl fields `shouldBe` Right (Just value)
    map SCon.surrogateControlTargets [first, second, third] `shouldBe` [["abc", "def"], [], ["def"]]
    map SCon.surrogateControlKind [first, second, third, fourth]
      `shouldBe` map
        Right
        [ SCon.SurrogateMaxAge (SCon.SurrogateFreshness 60 300)
        , SCon.SurrogateNoStoreRemote
        , SCon.SurrogateContent (SCap.SurrogateFeature "ESI" Nothing :| [])
        , SCon.SurrogateStaleWhileRevalidate 30
        ]
    lookupRawHeader "Surrogate-Control" (setHeader value (headerMapFromList []))
      `shouldBe` Just ("MAX-AGE=60+300;abc;def, no-store-remote, content=\"ESI\";def, stale-while-revalidate=30" :| [])
    SCon.surrogateControlFromKind (SCon.SurrogateMaxAge (SCon.SurrogateFreshness 60 300)) ["abc", "def"]
      `shouldBe` SCon.TargetedSurrogateControlDirective "max-age" (Just "60+300") ("abc" :| ["def"])
    SCon.surrogateControlKind (SCon.surrogateControlFromKind (SCon.SurrogateStaleIfError 90) [])
      `shouldBe` Right (SCon.SurrogateStaleIfError 90)
  it "retains extension octets and quoted delimiters" $ do
    let wire = "vendor=\"x,;\\\"\128\255\\\\\";edge"
        value = SCon.SurrogateControl (SCon.TargetedSurrogateControlDirective "vendor" (Just "x,;\"\128\255\\") ("edge" :| []) :| [])
    lookupHeader @SCon.SurrogateControl (headerMapFromList [("Surrogate-Control", wire)]) `shouldBe` Right (Just value)
    renderCon value `shouldBe` wire
  it "validates known arguments and target syntax across every field line" $ do
    forM_ ["", "max-age", "max-age=3+", "max-age=-3", "max-age=3+4+5", "no-store=1", "content=ESI", "content=\"ESI/\"", "stale-if-error=NaN", "no-store;", "no-store;\"edge\"", "ext=\"bad\rvalue\""] $ \wire ->
      lookupHeader @SCon.SurrogateControl (headerMapFromList [("Surrogate-Control", wire)]) `shouldSatisfy` isLeft
    lookupHeader @SCon.SurrogateControl (headerMapFromList [("Surrogate-Control", "no-store"), ("Surrogate-Control", "max-age=nope")])
      `shouldSatisfy` isLeft
    let overflow = SCon.SurrogateControlDirective "max-age" (Just "999999999999999999999999999999999999")
    SCon.surrogateControlKind overflow `shouldBe` Right (SCon.SurrogateMaxAge (SCon.SurrogateFreshness maxBound 0))


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
      , typedCapabilities
      , typedControl
      ]
