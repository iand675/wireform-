{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.Caching (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (RFC8941String (..), Result (..), runParser)
import qualified Network.HTTP.Headers.Pragma as P
import qualified Network.HTTP.Headers.Warning as W
import Test.Syd
import Test.Syd.Hedgehog ()


-- Helpers -------------------------------------------------------------------

dropOws :: ByteString -> ByteString
dropOws = BS.dropWhile (\w -> w == 0x20 || w == 0x09)


parsePragma :: ByteString -> Either String P.Pragma
parsePragma bs = case runParser P.pragmaParser bs of
  OK v leftover
    | BS.null (dropOws leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


parseWarning :: ByteString -> Either String W.Warning
parseWarning bs = case runParser W.warningParser bs of
  OK v leftover
    | BS.null (dropOws leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


renderPragma :: P.Pragma -> ByteString
renderPragma = M.toStrictByteString . P.renderPragma


renderWarning :: W.Warning -> ByteString
renderWarning = M.toStrictByteString . W.renderWarning


mkTime :: Integer -> Int -> Int -> Integer -> UTCTime
mkTime year month day secs =
  UTCTime (fromGregorian year month day) (secondsToDiffTime secs)


-- Pragma --------------------------------------------------------------------

unit_pragma_parse :: Spec
unit_pragma_parse = it "parses no-cache and an extension directive" $
  case parsePragma "no-cache, custom=val" of
    Right (P.Pragma (a :| [b])) -> do
      P.pragmaName a `shouldBe` ST.fromString "no-cache"
      P.pragmaValue a `shouldBe` Nothing
      P.pragmaName b `shouldBe` ST.fromString "custom"
      P.pragmaValue b `shouldBe` Just (P.PragmaToken (ST.fromString "val"))
    other -> error (show other)


unit_pragma_quoted :: Spec
unit_pragma_quoted = it "parses a quoted extension value" $
  case parsePragma "foo=\"a b\"" of
    Right (P.Pragma (a :| [])) -> do
      P.pragmaName a `shouldBe` ST.fromString "foo"
      P.pragmaValue a `shouldBe` Just (P.PragmaQuoted (RFC8941String (ST.fromString "a b")))
    other -> error (show other)


unit_pragma_render :: Spec
unit_pragma_render =
  it "renders directives joined by commas" $
    let v =
          P.Pragma
            ( P.PragmaDirective (ST.fromString "no-cache") Nothing
                :| [P.PragmaDirective (ST.fromString "x") (Just (P.PragmaToken (ST.fromString "y")))]
            )
    in renderPragma v `shouldBe` "no-cache, x=y"


prop_pragma_roundtrip :: Property
prop_pragma_roundtrip = property $ do
  v <- forAll pragmaGen
  let bs = renderPragma v
  case parsePragma bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- Warning -------------------------------------------------------------------

unit_warning_parse :: Spec
unit_warning_parse = it "parses a warning-value with agent, text, and date" $
  case parseWarning "112 - \"network down\" \"Sat, 25 Aug 2012 23:34:45 GMT\"" of
    Right (W.Warning (a :| [])) -> do
      W.warnCode a `shouldBe` 112
      W.warnAgent a `shouldBe` ST.fromString "-"
      W.warnText a `shouldBe` RFC8941String (ST.fromString "network down")
      W.warnDate a `shouldBe` Just (mkTime 2012 8 25 84885)
    other -> error (show other)


unit_warning_parse_nodate :: Spec
unit_warning_parse_nodate = it "parses a warning-value without a date" $
  case parseWarning "199 example.net:80 \"misc\"" of
    Right (W.Warning (a :| [])) -> do
      W.warnCode a `shouldBe` 199
      W.warnAgent a `shouldBe` ST.fromString "example.net:80"
      W.warnText a `shouldBe` RFC8941String (ST.fromString "misc")
      W.warnDate a `shouldBe` Nothing
    other -> error (show other)


unit_warning_render :: Spec
unit_warning_render =
  it "renders warn-code zero-padded to three digits" $
    let v = W.Warning (W.WarningValue 12 (ST.fromString "proxy") (RFC8941String (ST.fromString "stale")) Nothing :| [])
    in renderWarning v `shouldBe` "012 proxy \"stale\""


unit_warning_render_date :: Spec
unit_warning_render_date =
  it "renders a warn-date in quotes" $
    let v =
          W.Warning
            ( W.WarningValue 113 (ST.fromString "-") (RFC8941String (ST.fromString "heuristic")) (Just (mkTime 1994 11 6 31777))
                :| []
            )
    in renderWarning v `shouldBe` "113 - \"heuristic\" \"Sun, 06 Nov 1994 08:49:37 GMT\""


prop_warning_roundtrip :: Property
prop_warning_roundtrip = property $ do
  v <- forAll warningGen
  let bs = renderWarning v
  case parseWarning bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- Generators ----------------------------------------------------------------

tokenGen :: Gen ST.ShortText
tokenGen = ST.fromString <$> Gen.list (Range.linear 1 8) (Gen.element tchars)
  where
    tchars = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "-"


-- Quoted-string body restricted to qdtext that never needs escaping, so the
-- round-trip equality does not depend on the escape path (covered elsewhere).
quotedBodyGen :: Gen ST.ShortText
quotedBodyGen = ST.fromString <$> Gen.list (Range.linear 0 10) (Gen.element qchars)
  where
    qchars = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> " .-_/:"


pragmaValueGen :: Gen P.PragmaValue
pragmaValueGen =
  Gen.choice
    [ P.PragmaToken <$> tokenGen
    , P.PragmaQuoted . RFC8941String <$> quotedBodyGen
    ]


pragmaGen :: Gen P.Pragma
pragmaGen = P.Pragma <$> Gen.nonEmpty (Range.linear 1 4) directiveGen
  where
    directiveGen = P.PragmaDirective <$> tokenGen <*> Gen.maybe pragmaValueGen


warnCodeGen :: Gen Word
warnCodeGen = Gen.word (Range.linear 0 999)


agentGen :: Gen ST.ShortText
agentGen = ST.fromString <$> Gen.list (Range.linear 1 10) (Gen.element achars)
  where
    achars = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> ".-:"


utcTimeGen :: Gen UTCTime
utcTimeGen = do
  year <- Gen.integral (Range.linear 1000 9999) :: Gen Integer
  month <- Gen.integral (Range.linear 1 12) :: Gen Int
  day <- Gen.integral (Range.linear 1 28) :: Gen Int
  secs <- Gen.integral (Range.linear 0 86399) :: Gen Integer
  pure (mkTime year month day secs)


warningGen :: Gen W.Warning
warningGen = W.Warning <$> Gen.nonEmpty (Range.linear 1 4) valueGen
  where
    valueGen =
      W.WarningValue
        <$> warnCodeGen
        <*> agentGen
        <*> (RFC8941String <$> quotedBodyGen)
        <*> Gen.maybe utcTimeGen


-- Suite ---------------------------------------------------------------------

tests :: Spec
tests =
  describe "Caching" $
    sequence_
      [ unit_pragma_parse
      , unit_pragma_quoted
      , unit_pragma_render
      , it "Pragma render/parse round-trip" prop_pragma_roundtrip
      , unit_warning_parse
      , unit_warning_parse_nodate
      , unit_warning_render
      , unit_warning_render_date
      , it "Warning render/parse round-trip" prop_warning_roundtrip
      ]
